pub mod day13;
pub mod day23;
pub mod day24;
pub mod day7;

// for measurement program
pub fn day_part_iterator() -> impl Iterator<Item = (u8, u8)> {
    (1..=24)
        .flat_map(|day| (1..=2).map(move |part| (day, part)))
        .chain((25..26).map(|day| (day, 1)))
}
