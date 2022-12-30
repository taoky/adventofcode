use std::{
    ffi::CString,
    fs::File,
    io::{Read, Seek, SeekFrom, Write},
    os::unix::prelude::FromRawFd,
    process::Command,
    time::Duration,
};

use super::{Measurement, Resource};
use anyhow::{Context, Result};
use nix::{
    libc,
    sys::{
        memfd::{memfd_create, MemFdCreateFlag},
        resource::{getrusage, UsageWho},
        wait::waitpid,
    },
    unistd::{fork, ForkResult},
};

#[derive(Default)]
pub struct GetRusage {
    memfd: Option<File>,
}

impl Resource for GetRusage {
    fn init(&mut self) -> Result<()> {
        // create shared memory
        let shared_memfile =
            memfd_create(&CString::new("shared")?, MemFdCreateFlag::MFD_ALLOW_SEALING)?;
        self.memfd = Some(unsafe { File::from_raw_fd(shared_memfile) });
        Ok(())
    }

    fn cleanup(&self) -> Result<()> {
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, _day: u8, _part: u8) -> Result<Measurement> {
        match unsafe { fork()? } {
            ForkResult::Parent { child, .. } => {
                waitpid(child, None)?;
            }
            ForkResult::Child => {
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
                self.memfd
                    .as_mut()
                    .unwrap()
                    .set_len(output.len().try_into().unwrap())
                    .unwrap();
                self.memfd
                    .as_mut()
                    .unwrap()
                    .seek(SeekFrom::Start(0))
                    .unwrap();
                self.memfd.as_mut().unwrap().write_all(output).unwrap();
                self.memfd
                    .as_mut()
                    .unwrap()
                    .seek(SeekFrom::Start(0))
                    .unwrap();
                // self.memfd.as_ref().unwrap().sync_all().unwrap();

                unsafe { libc::_exit(0) };
            }
        }

        let mut buffer = String::new();
        self.memfd.as_ref().unwrap().read_to_string(&mut buffer)?;

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
