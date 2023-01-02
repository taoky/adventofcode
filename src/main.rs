// Generate performance analysis

use std::{
    fs::File,
    path::PathBuf,
    process::{Command, Stdio},
};

use adventofcode_2022::{
    resource::{cgroup::Cgroup, getrusage::GetRusage, poll::Poll, Resource},
    utils::day_part_iterator,
};
use clap::{Parser, ValueEnum};

#[derive(Parser)]
struct Args {
    #[arg(long)]
    /// Path to rust project root. Defaults to current directory
    dir: Option<PathBuf>,

    /// Solution binaries to test
    #[arg(long, value_enum, default_value_t = Mode::Release)]
    mode: Mode,

    /// Test memory usage with cgroupv2 or not
    #[arg(long, value_enum, default_value_t = Resourcer::Getrusage)]
    resourcer: Resourcer,
}

#[derive(ValueEnum, Clone, Copy, Debug)]
enum Mode {
    /// Test debug build
    Debug,
    /// Test release build
    Release,
}

#[derive(ValueEnum, Clone, Copy, Debug, PartialEq, Eq)]
enum Resourcer {
    /// Use cgroupv2 to measure memory usage (memory.peak)
    Cgroup,
    /// Use fork(), pipe() and getrusage() to measure memory usage (RSS) and time used (user + kernel)
    Getrusage,
    /// Polling /proc/<pid>/smaps_rollup or /proc/<pid>/smaps to get memory usage (RSS + swap)
    Poll,
    /// Just measure time used
    Pure,
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
    let cli = Args::parse();

    let project_dir = cli.dir.unwrap_or_else(|| std::env::current_dir().unwrap());
    let input_dir = project_dir.join("input/");
    let binaries_dir = project_dir.join("target/").join(match cli.mode {
        Mode::Debug => "debug/",
        Mode::Release => "release/",
    });
    let mut resourcer: Box<dyn Resource> = match cli.resourcer {
        Resourcer::Cgroup => Box::new(Cgroup::new(true)),
        Resourcer::Getrusage => Box::new(GetRusage::default()),
        Resourcer::Poll => Box::new(Poll::default()),
        Resourcer::Pure => Box::new(Cgroup::new(false)),
    };

    if let Err(e) = resourcer.init() {
        if cli.resourcer == Resourcer::Cgroup {
            eprintln!("Failed to initialize cgroup: {}", e);
            eprintln!("{}", e.backtrace());
            eprintln!("Will not calculate memory usage.");
        } else {
            panic!("Failed to initialize resourcer: {}", e);
        }
    }

    for (day, part) in day_part_iterator() {
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

        let measurement = resourcer.spawn(cmd, day, part).unwrap_or_else(|e| {
            panic!(
                "Day {}-{} failed by \"{}\" with backtrace:\n{}",
                day,
                part,
                e,
                e.backtrace()
            )
        });
        let elapsed = measurement.time;
        let elapsed = if elapsed.as_millis() > 0 {
            format!("{}ms", elapsed.as_millis())
        } else {
            format!("{}μs", elapsed.as_micros())
        };
        let mut result = format!("Day {}-{}: {}", day, part, elapsed);
        if let Some(peak) = measurement.memory {
            result += format!(", {}", humanize(peak)).as_str();
        }
        println!("{}", result);
    }

    if let Err(e) = { resourcer.cleanup() } {
        eprintln!("Failed to cleanup cgroupv2: {}", e);
    }
}
