use std::{process::Command, time::Duration};

use super::{Measurement, Resource};
use anyhow::Result;
use nix::{
    sys::{
        resource::{getrusage, UsageWho},
        wait::{waitid, Id, WaitPidFlag, WaitStatus},
    },
    unistd::Pid,
};

pub struct GetRusage {
    last_time: Duration,
    last_memory: usize,
}

impl Default for GetRusage {
    fn default() -> Self {
        println!("{:?}", getrusage(UsageWho::RUSAGE_CHILDREN));
        Self {
            last_time: Duration::from_secs(0),
            last_memory: 0,
        }
    }
}

impl Resource for GetRusage {
    fn init(&mut self) -> Result<()> {
        Ok(())
    }

    fn cleanup(&self) -> Result<()> {
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, _day: u8, _part: u8) -> Result<Measurement> {
        let mut process = cmd.spawn()?;

        // fork and NOWAIT waiting (not reaping)
        loop {
            let wait_status = waitid(
                Id::Pid(Pid::from_raw(process.id().try_into()?)),
                WaitPidFlag::WEXITED | WaitPidFlag::WNOWAIT,
            )?;

            match wait_status {
                WaitStatus::Exited(_, status) => {
                    if status != 0 {
                        return Err(anyhow::anyhow!(
                            "Command exited with non-zero status code: {}",
                            status
                        ));
                    }
                    break;
                }
                WaitStatus::Signaled(_, _, _) => {
                    return Err(anyhow::anyhow!("Command exited with signal"));
                }
                WaitStatus::Stopped(_, _) => {}
                WaitStatus::PtraceEvent(_, _, _) => {}
                WaitStatus::PtraceSyscall(_) => {}
                WaitStatus::Continued(_) => {}
                _ => unreachable!(),
            }
        }

        // getrusage
        let rusage = getrusage(UsageWho::RUSAGE_CHILDREN)?;
        process.wait()?;

        let timeval = rusage.user_time() + rusage.system_time();
        let time = Duration::from_secs(timeval.tv_sec().try_into()?)
            + Duration::from_micros(timeval.tv_usec().try_into()?)
            - self.last_time;

        let memory = usize::try_from(rusage.max_rss() * 1024)? - self.last_memory;

        self.last_time = time;
        self.last_memory = memory;

        Ok(Measurement {
            time,
            memory: Some(memory),
        })
    }
}
