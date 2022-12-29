// Generate performance analysis

use std::{
    fs::File,
    io::{Read, Write},
    os::unix::process::CommandExt,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use clap::{Parser, ValueEnum};
use nix::unistd::{Pid, Uid};

use anyhow::Result;

#[derive(Parser)]
struct Args {
    #[arg(long)]
    /// Path to rust project root. Defaults to current directory
    dir: Option<PathBuf>,

    /// Solution binaries to test
    #[arg(value_enum, default_value_t = Mode::Release)]
    mode: Mode,

    #[arg(long, default_value_t = true)]
    memory: bool,
}

#[derive(ValueEnum, Clone, Copy, Debug)]
enum Mode {
    /// Test debug build
    Debug,
    /// Test release build
    Release,
}

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

macro_rules! mem_failed {
    ($memory: expr) => {
        eprintln!("Failed to configure cgroup: above command execution failed.");
        eprintln!("Will not calculate memory usage.");
        $memory = false;
    };
    ($memory: expr, $err: expr) => {
        eprintln!("Failed to configure cgroup: {}", $err);
        eprintln!("{}", $err.backtrace());
        eprintln!("Will not calculate memory usage.");
        $memory = false;
    };
}

fn day_part_iterator() -> impl Iterator<Item = (u8, u8)> {
    (1..=24)
        .flat_map(|day| (1..=2).map(move |part| (day, part)))
        .chain((25..26).map(|day| (day, 1)))
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
    let script = format!(
        r#"
set -ex
# move current process to root cgroup tree node
PIDS=$(</sys/fs/cgroup/adventofcode-2022/day0/cgroup.procs)
for i in $PIDS; do 
    echo $i > /sys/fs/cgroup/cgroup.procs || true; 
done
rmdir /sys/fs/cgroup/adventofcode-2022/day*
rmdir /sys/fs/cgroup/adventofcode-2022
"#
    );
    sudo(script.as_str(), "This program needs to cleanup cgroupv2 (created at the beginning). This requires root permission.")
}

fn humanize(size: usize) -> String {
    let mut size = size as f64;
    let suffix = ["B", "KiB", "MiB", "GiB"];
    for unit in suffix {
        if size < 1024.0 {
            return format!("{:.3}{}", size, unit);
        }
        size /= 1024.0;
    }
    format!("{:.1}{}", size, "TiB")
}

fn main() {
    let mut cli = Args::parse();

    let project_dir = cli.dir.unwrap_or_else(|| std::env::current_dir().unwrap());
    let input_dir = project_dir.join("input/");
    let binaries_dir = project_dir.join("target/").join(match cli.mode {
        Mode::Debug => "debug/",
        Mode::Release => "release/",
    });
    let uid = Uid::current();

    if cli.memory {
        // initialize cgroupv2 with root if possible
        if uid.is_root() {
            if let Err(e) = rust_initialize_cgroup() {
                mem_failed!(cli.memory, e);
            }
        } else if let Err(e) = sudo_initialize_cgroup() {
            mem_failed!(cli.memory, e);
        }
    }

    for (day, part) in day_part_iterator() {
        if day == 25 && part == 2 {
            continue;
        }

        let start_time = std::time::Instant::now();
        let mut cmd = Command::new(binaries_dir.join(format!("day{}-{}", day, part)));
        if day == 15 {
            cmd.arg(match part {
                1 => "2000000",
                2 => "4000000",
                _ => unreachable!(),
            });
        }

        let cmd = cmd
            .stdin(File::open(input_dir.join(format!("day{}", day)).join("input.txt")).unwrap())
            .stdout(Stdio::null());

        if cli.memory {
            // get process into cgroup
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

        let res = cmd.spawn().unwrap().wait().unwrap();

        if !res.success() {
            panic!("Day {}-{} failed", day, part);
        }
        let elapsed = start_time.elapsed();
        let elapsed = if elapsed.as_millis() > 0 {
            format!("{}ms", elapsed.as_millis())
        } else {
            format!("{}μs", elapsed.as_micros())
        };
        let mut result = format!("Day {}-{}: {}", day, part, elapsed);
        if cli.memory {
            let mut peak = String::new();
            std::fs::File::open(
                Path::new(CGROUP_DIR)
                    .join(format!("day{}-{}", day, part))
                    .join("memory.peak"),
            )
            .unwrap()
            .read_to_string(&mut peak)
            .unwrap();
            result += format!(", {}", humanize(peak.trim().parse().unwrap())).as_str();
        }
        println!("{}", result);
    }

    if cli.memory {
        if let Err(e) = {
            if !uid.is_root() {
                sudo_cleanup_cgroup()
            } else {
                rust_cleanup_cgroup()
            }
        } {
            eprintln!("Failed to cleanup cgroupv2: {}", e);
        }
    }
}
