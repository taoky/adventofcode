use std::{
    fs::File,
    io::{Read, Write},
    os::unix::prelude::FromRawFd,
    process::Command,
    time::Duration,
};

use super::{Measurement, Resource};
use anyhow::{Context, Result};
use nix::{
    libc,
    sys::{
        resource::{getrusage, UsageWho},
        wait::waitpid,
    },
    unistd::{fork, pipe, ForkResult},
};

#[derive(Default)]
pub struct GetRusage {}

impl Resource for GetRusage {
    fn init(&mut self) -> Result<()> {
        Ok(())
    }

    fn cleanup(&self) -> Result<()> {
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, _day: u8, _part: u8) -> Result<Measurement> {
        let pipes = pipe()?; // (read, write)
        let mut write_file = unsafe { File::from_raw_fd(pipes.1) };
        let mut read_file = unsafe { File::from_raw_fd(pipes.0) };
        match unsafe { fork()? } {
            ForkResult::Parent { child, .. } => {
                waitpid(child, None)?;
            }
            ForkResult::Child => {
                // Please note that getrusage() is not in the async signal safe list.
                // thus it's impossible to get the resource usage of the child process safely after fork()
                // in multithreaded programs.
                // This program is single-thread thus it's not a problem.
                std::mem::drop(read_file);
                fn go(cmd: &mut Command) -> Result<Measurement> {
                    let mut process = cmd.spawn()?;
                    process.wait()?;
                    let rusage = getrusage(UsageWho::RUSAGE_CHILDREN)?;
                    // println!("{:?}", getrusage(UsageWho::RUSAGE_CHILDREN)?);

                    let timeval = rusage.user_time() + rusage.system_time();
                    let time = Duration::from_secs(timeval.tv_sec().try_into()?)
                        + Duration::from_micros(timeval.tv_usec().try_into()?);

                    let memory = rusage.max_rss() * 1024;
                    // println!("{:?} {}", time, memory);

                    Ok(Measurement {
                        time,
                        memory: Some(memory.try_into()?),
                    })
                }

                let output = match go(cmd) {
                    Ok(measurement) => {
                        // println!("{:?}", measurement);
                        format!(
                            "({},{})",
                            measurement.time.as_nanos(),
                            measurement.memory.unwrap()
                        )
                    }
                    Err(e) => {
                        format!("Error: {}", e)
                    }
                };
                let output = output.as_bytes();
                write_file.write_all(output).unwrap();

                unsafe { libc::_exit(0) };
            }
        }
        std::mem::drop(write_file);

        let mut buffer = String::new();
        read_file.read_to_string(&mut buffer)?;

        if buffer.starts_with("Error:") {
            Err(anyhow::anyhow!("{}", buffer))
        } else {
            let split: Vec<_> = buffer
                .strip_prefix('(')
                .context("buffer not prefixed with (")?
                .strip_suffix(')')
                .context("buffer not suffixed with )")?
                .split(',')
                .collect();
            // println!("{} {:?}", buffer, split);
            let time = Duration::from_nanos(split[0].parse()?);
            let memory = split[1].parse()?;
            Ok(Measurement {
                time,
                memory: Some(memory),
            })
        }
    }
}
