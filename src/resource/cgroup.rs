use crate::utils::day_part_iterator;

use super::{Measurement, Resource};
use std::{
    io::{Read, Write},
    os::unix::process::CommandExt,
    path::Path,
    process::Command,
};

use anyhow::Result;
use nix::unistd::{Pid, Uid};

const CGROUP_DIR: &str = "/sys/fs/cgroup/adventofcode-2022";

fn sudo(script: &str, explanation: &str) -> Result<()> {
    eprint!("{}", explanation);
    eprintln!(" Thus the following script will be run with bash by sudo:");
    eprintln!("{}", script);
    eprint!("Continue? [y/N] ");

    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    if input.trim() == "y" {
        let mut cmd = Command::new("sudo");
        cmd.args(["bash", "-c", script]);
        let res = cmd.spawn()?.wait()?;
        if !res.success() {
            Err(anyhow::anyhow!("Failed to run sudo script."))
        } else {
            Ok(())
        }
    } else {
        Err(anyhow::anyhow!("User cancelled."))
    }
}

fn rust_initialize_cgroup() -> Result<()> {
    let path = Path::new(CGROUP_DIR);

    std::fs::create_dir(path)?;
    // write subtree-control
    {
        let path = path.join("cgroup.subtree_control");
        let mut file = std::fs::OpenOptions::new().write(true).open(path)?;
        file.write_all(b"+memory")?;
    }
    // create subcgroups
    for (day, part) in day_part_iterator() {
        let path = path.join(format!("day{}-{}", day, part));
        std::fs::create_dir(path)?;
    }

    Ok(())
}

fn rust_cleanup_cgroup() -> Result<()> {
    let path = Path::new(CGROUP_DIR);
    if path.try_exists()? {
        for (day, part) in day_part_iterator() {
            let path = path.join(format!("day{}-{}", day, part));
            if path.try_exists()? {
                std::fs::remove_dir(path)?;
            }
        }
        let leaf = path.join("day0");
        if leaf.try_exists()? {
            std::fs::remove_dir(leaf)?;
        }
        std::fs::remove_dir(path)?;
    }
    Ok(())
}

fn sudo_initialize_cgroup() -> Result<()> {
    let uid = Uid::current();
    let mut script = format!(
        r#"
set -ex
rmdir /sys/fs/cgroup/adventofcode-2022/day* || true
rmdir /sys/fs/cgroup/adventofcode-2022 || true
mkdir /sys/fs/cgroup/adventofcode-2022
chown {} /sys/fs/cgroup/adventofcode-2022
chown {} /sys/fs/cgroup/adventofcode-2022/cgroup.procs
mkdir /sys/fs/cgroup/adventofcode-2022/day0 && chown {} /sys/fs/cgroup/adventofcode-2022/day0 && chown {} /sys/fs/cgroup/adventofcode-2022/day0/cgroup.procs
echo "+memory" > /sys/fs/cgroup/adventofcode-2022/cgroup.subtree_control
echo {} > /sys/fs/cgroup/adventofcode-2022/day0/cgroup.procs
"#,
        uid,
        uid,
        uid,
        uid,
        Pid::this()
    );
    for (day, part) in day_part_iterator() {
        script += format!(
            "mkdir /sys/fs/cgroup/adventofcode-2022/day{}-{} && chown {} /sys/fs/cgroup/adventofcode-2022/day{}-{} && chown {} /sys/fs/cgroup/adventofcode-2022/day{}-{}/cgroup.procs\n",
            day, part, uid, day, part, uid, day, part
        )
        .as_str();
    }

    sudo(script.as_str(), "This program needs to initialize cgroupv2 for memory usage analysis. This requires root permission.")
}

fn sudo_cleanup_cgroup() -> Result<()> {
    let script = r#"
set -ex
# move current process to root cgroup tree node
PIDS=$(</sys/fs/cgroup/adventofcode-2022/day0/cgroup.procs)
for i in $PIDS; do 
    echo $i > /sys/fs/cgroup/cgroup.procs || true; 
done
rmdir /sys/fs/cgroup/adventofcode-2022/day*
rmdir /sys/fs/cgroup/adventofcode-2022
"#;
    sudo(script, "This program needs to cleanup cgroupv2 (created at the beginning). This requires root permission.")
}

pub struct Cgroup {
    enabled: bool,
}

impl Cgroup {
    pub fn new(enable_cgroup: bool) -> Self {
        Self {
            enabled: enable_cgroup,
        }
    }
}

impl Resource for Cgroup {
    fn init(&mut self) -> Result<()> {
        // initialize cgroupv2 with root if possible
        if self.enabled {
            if let Err(e) = {
                if Uid::current().is_root() {
                    rust_initialize_cgroup()
                } else {
                    sudo_initialize_cgroup()
                }
            } {
                self.enabled = false;
                return Err(e);
            }
        }
        Ok(())
    }

    fn cleanup(&self) -> Result<()> {
        if self.enabled {
            if !Uid::current().is_root() {
                sudo_cleanup_cgroup()?;
            } else {
                rust_cleanup_cgroup()?;
            }
        }
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, day: u8, part: u8) -> Result<Measurement> {
        if self.enabled {
            unsafe {
                cmd.pre_exec(move || {
                    std::fs::OpenOptions::new()
                        .write(true)
                        .open(
                            Path::new(CGROUP_DIR)
                                .join(format!("day{}-{}", day, part))
                                .join("cgroup.procs"),
                        )
                        .unwrap()
                        .write_all(format!("{}", std::process::id()).as_bytes())
                        .unwrap();
                    Ok(())
                });
            }
        }

        let start_time = std::time::Instant::now();
        let res = cmd.spawn()?.wait()?;
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

        let peak = if self.enabled {
            let mut peak = String::new();
            std::fs::File::open(
                Path::new(CGROUP_DIR)
                    .join(format!("day{}-{}", day, part))
                    .join("memory.peak"),
            )?
            .read_to_string(&mut peak)?;
            Some(peak.trim().parse::<usize>()?)
        } else {
            None
        };

        Ok(Measurement {
            time: elapsed,
            memory: peak,
        })
    }
}
