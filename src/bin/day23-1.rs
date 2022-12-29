use adventofcode_2022::utils::day23::{find_border, pretty_print, simulate, Elf, Map};

fn main() {
    let mut elves = Vec::new();
    let mut map = Map::new();

    let mut x = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }

        for (y, c) in input.trim().chars().enumerate() {
            if c == '#' {
                elves.push(Elf::new(x, y as isize));
                map.insert((x, y as isize));
            }
        }

        x += 1;
    }

    for _ in 0..10 {
        if simulate(&mut elves, &mut map, true).is_none() {
            break;
        }
    }
    pretty_print(&map);

    let (min_x, max_x, min_y, max_y) = find_border(&map);
    println!(
        "{}",
        (max_x - min_x + 1) * (max_y - min_y + 1) - elves.len() as isize
    );
}
