use std::collections::HashSet;

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

macro_rules! touching {
    ($x1: expr, $y1: expr, $x2: expr, $y2: expr) => {
        i32::abs($x1 - $x2) <= 1 && i32::abs($y1 - $y2) <= 1
    };
}

fn main() {
    let mut hashset: HashSet<(i32, i32)> = std::collections::HashSet::new();
    hashset.insert((0, 0));
    let mut knots = vec![(0, 0); 10];
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let splitted: Vec<_> = input.trim().split(' ').collect();
        let dir_str = splitted[0];
        let steps = splitted[1].parse::<usize>().unwrap();

        let dir = match dir_str {
            "R" => Dir::Right,
            "U" => Dir::Up,
            "L" => Dir::Left,
            "D" => Dir::Down,
            _ => panic!("Invalid dir"),
        };

        for _ in 0..steps {
            match dir {
                Dir::Right => {
                    knots[0].0 += 1;
                }
                Dir::Up => {
                    knots[0].1 += 1;
                }
                Dir::Left => {
                    knots[0].0 -= 1;
                }
                Dir::Down => {
                    knots[0].1 -= 1;
                }
            }
            // knots
            for i in 1..knots.len() {
                let last_x = knots[i - 1].0;
                let last_y = knots[i - 1].1;
                let this_x = knots[i].0;
                let this_y = knots[i].1;

                let mut success = false;
                if touching!(last_x, last_y, this_x, this_y) {
                    // knots touching
                    break;
                } else if last_x == this_x || last_y == this_y {
                    for dir in [Dir::Up, Dir::Right, Dir::Left, Dir::Down] {
                        let new_pt = match dir {
                            Dir::Right => (this_x + 1, this_y),
                            Dir::Up => (this_x, this_y + 1),
                            Dir::Left => (this_x - 1, this_y),
                            Dir::Down => (this_x, this_y - 1),
                        };
                        if touching!(new_pt.0, new_pt.1, last_x, last_y) {
                            success = true;
                            knots[i] = new_pt;
                            break;
                        }
                    }
                    assert!(success);
                } else {
                    for dir in [Dir::Up, Dir::Right, Dir::Left, Dir::Down] {
                        let new_pt = match dir {
                            Dir::Right => (this_x + 1, this_y + 1),
                            Dir::Up => (this_x - 1, this_y + 1),
                            Dir::Left => (this_x - 1, this_y - 1),
                            Dir::Down => (this_x + 1, this_y - 1),
                        };
                        if touching!(new_pt.0, new_pt.1, last_x, last_y) {
                            success = true;
                            knots[i] = new_pt;
                            break;
                        }
                    }
                    assert!(success);
                }
            }
            // println!("{} {:?}", input, knots);
            hashset.insert(knots[knots.len() - 1]);
        }
    }
    println!("{}", hashset.len());
}
