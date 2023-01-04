use crate::utils::{
    day_part_iterator,
    u32_to_bytes,
};

#[cfg(feature = "systemd")]
use crate::utils::systemd::{ManagerProxyBlocking, ScopeProxyBlocking};

use super::{Measurement, Resource};
use std::{
    io::{Read, Write},
    os::unix::process::CommandExt,
    path::Path,
    process::Command,
};

use anyhow::Result;
use nix::{
    fcntl::{open, OFlag},
    sys::stat::Mode,
    unistd::{close, getpid, write, Uid},
};

#[cfg(feature = "systemd")]
use zbus::blocking::Connection;

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

fn rust_initialize_cgroup(root: &str) -> Result<()> {
    let path = Path::new(root);

    std::fs::create_dir_all(path)?;
    // write subtree-control
    {
        let path = path.join("cgroup.subtree_control");
        let mut file = std::fs::OpenOptions::new().write(true).open(path)?;
        file.write_all(b"+memory")?;
    }
    // create subcgroups
    for (day, part) in day_part_iterator() {
        let folder = path.join(format!("day{}-{}", day, part));
        std::fs::create_dir(folder)?;
        // disable swap
        let path = path.join("memory.swap.max");
        let mut file = std::fs::OpenOptions::new().write(true).open(path)?;
        file.write_all(b"0")?;
    }

    Ok(())
}

fn rust_cleanup_cgroup(root: &str, remove_root: bool) -> Result<()> {
    let path = Path::new(root);
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
        if remove_root {
            std::fs::remove_dir(path)?;
        }
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
        getpid().as_raw()
    );
    for (day, part) in day_part_iterator() {
        script += format!(
            "mkdir /sys/fs/cgroup/adventofcode-2022/day{}-{} && chown {} /sys/fs/cgroup/adventofcode-2022/day{}-{} && chown {} /sys/fs/cgroup/adventofcode-2022/day{}-{}/cgroup.procs && echo 0 > /sys/fs/cgroup/adventofcode-2022/day{}-{}/memory.swap.max\n",
            day, part, uid, day, part, uid, day, part, day, part
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
    #[cfg(feature = "systemd")]
    systemd: bool,
    cgroup_root: Option<String>,
}

impl Cgroup {
    pub fn new(enable_cgroup: bool, use_systemd: bool) -> Self {
        let root = if !use_systemd {
            Some("/sys/fs/cgroup/adventofcode-2022".to_owned())
        } else {
            None
        };
        Self {
            enabled: enable_cgroup,
            #[cfg(feature = "systemd")]
            systemd: use_systemd,
            cgroup_root: root,
        }
    }

    fn get_cgroup_root(&self) -> &str {
        self.cgroup_root.as_ref().unwrap()
    }
}

impl Resource for Cgroup {
    fn init(&mut self) -> Result<()> {
        // initialize cgroupv2 with root if possible
        if self.enabled {
            #[cfg(feature = "systemd")]
            if self.systemd {
                // use dbus to ask systemd give a delegated cgroup
                let connection = Connection::session()?;
                let systemd_manager = ManagerProxyBlocking::new(&connection)?;
                // pids MUST BE an u32 array, otherwise System.Error.ENXIO will be returned by systemd
                // Error occurred at https://github.com/systemd/systemd/blob/de712a85ffb7ea129536b4e14a1f5cb48f7116f7/src/core/dbus-scope.c#LL90C21-L90C51
                // where "u" means unsigned 32-bit integer
                let pids = [getpid().as_raw() as u32];
                // remove failed scope
                let _err = systemd_manager.reset_failed_unit("adventofcode-transient.scope");

                let removed_jobs = systemd_manager.receive_job_removed()?;

                let job = systemd_manager.start_transient_unit(
                    "adventofcode-transient.scope",
                    "replace",
                    &[
                        ("PIDs", pids[..].into()),
                        ("Description", "Transient cgroup for benchmarking".into()),
                    ],
                    &[],
                )?;

                for signal in removed_jobs {
                    let args = signal.args()?;

                    if args.job == job.as_ref() {
                        break;
                    }
                }

                let scope_dbus_path = systemd_manager.get_unit("adventofcode-transient.scope")?;
                let systemd_scope = ScopeProxyBlocking::builder(&connection)
                    .path(scope_dbus_path)?
                    .build()?;
                let cgroup_path = systemd_scope.control_group()?;
                let cgroup_root = format!("/sys/fs/cgroup{}", cgroup_path);
                self.cgroup_root = Some(cgroup_root.to_owned());

                // write current pid to day0/cgroup.procs
                let cgroup_root = Path::new(cgroup_root.as_str());
                std::fs::create_dir_all(cgroup_root.join("day0"))?;
                std::fs::write(
                    cgroup_root.join("day0/cgroup.procs"),
                    getpid().as_raw().to_string(),
                )?;
            }
            if let Err(e) = {
                if Uid::current().is_root() {
                    rust_initialize_cgroup(self.get_cgroup_root())
                } else {
                    #[cfg(feature = "systemd")]
                    if self.systemd {
                        rust_initialize_cgroup(self.get_cgroup_root())
                    } else {
                        sudo_initialize_cgroup()
                    }
                    #[cfg(not(feature = "systemd"))]
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
            #[cfg(feature = "systemd")]
            if self.systemd {
                let cgroup_root = Path::new(self.cgroup_root.as_ref().unwrap());
                // disable memory subtree control
                std::fs::write(cgroup_root.join("cgroup.subtree_control"), "-memory")?;
                // move current process to root cgroup tree node
                std::fs::write(
                    cgroup_root.join("cgroup.procs"),
                    getpid().as_raw().to_string(),
                )?;
            }

            if !Uid::current().is_root() {
                #[cfg(feature = "systemd")]
                if self.systemd {
                    rust_cleanup_cgroup(self.get_cgroup_root(), false)?;
                    return Ok(())
                }
                sudo_cleanup_cgroup()?;
            } else {
                #[cfg(feature = "systemd")]
                rust_cleanup_cgroup(self.get_cgroup_root(), !self.systemd)?;
                #[cfg(not(feature = "systemd"))]
                rust_cleanup_cgroup(self.get_cgroup_root(), true)?;
            }
        }
        Ok(())
    }

    fn spawn(&mut self, cmd: &mut Command, day: u8, part: u8) -> Result<Measurement> {
        let mut proc_file_fd = -1;
        if self.enabled {
            // be sophisticated: no heap allocation inside pre_exec
            proc_file_fd = open(
                Path::new(self.get_cgroup_root())
                    .join(format!("day{}-{}", day, part))
                    .join("cgroup.procs")
                    .as_os_str(),
                OFlag::O_WRONLY,
                Mode::empty(),
            )?;
            unsafe {
                cmd.pre_exec(move || {
                    // no heap memory allocation inside
                    let mut buf = [0u8; 32];
                    let id = std::process::id();
                    let len = u32_to_bytes(id, &mut buf);
                    write(proc_file_fd, &buf.as_slice()[..len])?;
                    close(proc_file_fd)?;
                    Ok(())
                });
            }
        }

        let start_time = std::time::Instant::now();
        let res = cmd.spawn()?.wait()?;
        if self.enabled {
            close(proc_file_fd)?;
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

        let peak = if self.enabled {
            let mut peak = String::new();
            std::fs::File::open(
                Path::new(self.get_cgroup_root())
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
