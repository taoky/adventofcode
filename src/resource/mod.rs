use std::{process::Command, time::Duration};

use anyhow::Result;

pub struct Measurement {
    pub time: Duration,
    pub memory: Option<usize>,
}

pub trait Resource {
    fn init(&mut self) -> Result<()>;
    fn cleanup(&self) -> Result<()>;
    fn spawn(&mut self, cmd: &mut Command, day: u8, part: u8) -> Result<Measurement>;
}

pub mod cgroup;
pub mod getrusage;
