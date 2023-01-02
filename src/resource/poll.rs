use std::{
    path::Path,
    process::{Command, ExitStatus},
};

use super::{Measurement, Resource};
use anyhow::{Context, Result};

pub struct Poll {
    use_rollup: bool,
}

impl Default for Poll {
    fn default() -> Self {
        Self { use_rollup: true }
    }
}

impl Resource for Poll {
    fn init(&mut self) -> Result<()> {
        if !Path::new("/proc/self/smaps_rollup").try_exists()? {
            self.use_rollup = false;
        }
        Path::new("/proc/self/smaps")
            .try_exists()
            .context("Cannot find /proc/self/smaps")?;
        Ok(())
    }

    fn cleanup(&self) -> Result<()> {
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, _day: u8, _part: u8) -> Result<Measurement> {
        let start_time = std::time::Instant::now();
        let mut process = cmd.spawn()?;
        let pid = process.id();
        // polling /proc/<PID>/smaps_rollup, which contains both RSS and Swap memory
        // RSS does not include pages which have ..., or which are swapped out
        let mut max_mem: usize = 0;
        let res: ExitStatus;
        loop {
            let stats = std::fs::read_to_string(if self.use_rollup {
                format!("/proc/{}/smaps_rollup", pid)
            } else {
                format!("/proc/{}/smaps", pid)
            });
            // smaps_rollup is only available when the process hasn't finished yet
            if let Ok(stats) = stats {
                let stats: Vec<_> = stats.split('\n').collect();
                let mut rss = 0;
                let mut swap = 0;
                for stat in stats {
                    if stat.starts_with("Rss:") {
                        let stat = stat
                            .strip_prefix("Rss:")
                            .unwrap()
                            .strip_suffix(" kB")
                            .unwrap()
                            .trim();
                        rss += stat.parse::<usize>()?;
                    } else if stat.starts_with("Swap:") {
                        let stat = stat
                            .strip_prefix("Swap:")
                            .unwrap()
                            .strip_suffix(" kB")
                            .unwrap()
                            .trim();
                        swap += stat.parse::<usize>()?;
                    }
                }

                max_mem = max_mem.max((rss + swap) * 1024);
            }

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
