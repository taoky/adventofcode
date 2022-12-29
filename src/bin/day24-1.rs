use adventofcode_2022::utils::day24::{bfs, Blizzard, Direction};

fn main() {
    // let mut map = Map::new();
    let mut blizzards = Vec::new();

    let mut height = 0;
    let mut width = None;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        width = match width {
            Some(w) => {
                assert_eq!(w, input.trim().len() as isize);
                Some(w)
            }
            None => Some(input.trim().len() as isize),
        };
        for (y, c) in input.trim().chars().enumerate() {
            let blizzard = Blizzard {
                coord: (height, y as isize),
                direction: match c {
                    '>' => Direction::Right,
                    '<' => Direction::Left,
                    '^' => Direction::Up,
                    'v' => Direction::Down,
                    _ => continue,
                },
            };
            blizzards.push(blizzard);
            // let idx = blizzards.len() - 1;
            // map.entry((height, y))
            //     .and_modify(|f| f.push(idx))
            //     .or_insert_with(|| vec![idx]);
        }

        height += 1;
    }
    let width = width.unwrap();

    let start = (0, 1);
    let end = (height - 1, width - 2);

    println!("{}", bfs(start, end, height, width, &blizzards, 0));
}
