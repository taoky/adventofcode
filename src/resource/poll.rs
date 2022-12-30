use std::process::{Command, ExitStatus};

use super::{Measurement, Resource};
use anyhow::{Context, Result};
use nix::unistd::{sysconf, SysconfVar};

#[derive(Default)]
pub struct Poll {}

impl Resource for Poll {
    fn init(&mut self) -> Result<()> {
        Ok(())
    }

    fn cleanup(&self) -> Result<()> {
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, _day: u8, _part: u8) -> Result<Measurement> {
        let start_time = std::time::Instant::now();
        let mut process = cmd.spawn()?;
        let pid = process.id();
        // polling /proc/<PID>/stat
        let mut max_mem: usize = 0;
        let pagesize = sysconf(SysconfVar::PAGE_SIZE)?.context("Cannot get page size")? as usize;
        let res: ExitStatus;
        loop {
            let stat = std::fs::read_to_string(format!("/proc/{}/statm", pid))?;
            let stat: Vec<_> = stat.split(' ').collect();

            let rss = stat[1].parse::<usize>()?;
            let shared = stat[2].parse::<usize>()?;
            max_mem = max_mem.max((rss + shared) * pagesize);

            match process.try_wait()? {
                None => {}
                Some(status) => {
                    res = status;
                    break;
                }
            }
        }
        if !res.success() {
            match res.code() {
                Some(code) => {
                    return Err(anyhow::anyhow!(
                        "Command exited with non-zero status code: {}",
                        code
                    ))
                }
                None => return Err(anyhow::anyhow!("Command terminated by a signal")),
            }
        }
        let elapsed = start_time.elapsed();

        Ok(Measurement {
            time: elapsed,
            memory: Some(max_mem),
        })
    }
}
