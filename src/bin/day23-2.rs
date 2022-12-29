use adventofcode_2022::utils::day23::{simulate, Elf, Map};

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

    let mut cnt = 0;
    loop {
        cnt += 1;
        if simulate(&mut elves, &mut map, false).is_none() {
            break;
        }
    }
    // pretty_print(&map);

    println!("{}", cnt);
}
