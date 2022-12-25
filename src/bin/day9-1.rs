use std::collections::HashSet;

fn main() {
    let mut hashset: HashSet<(i32, i32)> = std::collections::HashSet::new();
    hashset.insert((0, 0));
    let mut hx = 0;
    let mut hy = 0;
    let mut tx = 0;
    let mut ty = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let splitted: Vec<_> = input.trim().split(' ').collect();
        let dir = splitted[0];
        let steps = splitted[1].parse::<usize>().unwrap();

        for _ in 0..steps {
            match dir {
                "R" => {
                    hx += 1;
                }
                "U" => {
                    hy += 1;
                }
                "L" => {
                    hx -= 1;
                }
                "D" => {
                    hy -= 1;
                }
                _ => panic!("Invalid dir"),
            }
            if i32::abs(hx - tx) <= 1 && i32::abs(hy - ty) <= 1 {
                // head and tail touching
            } else if hx == tx || hy == ty {
                // head and tail in same row/column
                match dir {
                    "R" => {
                        tx += 1;
                    }
                    "U" => {
                        ty += 1;
                    }
                    "L" => {
                        tx -= 1;
                    }
                    "D" => {
                        ty -= 1;
                    }
                    _ => panic!("Invalid dir"),
                }
            } else {
                // move one step diagonally
                match dir {
                    "R" => {
                        tx = hx - 1;
                        ty = hy;
                    }
                    "U" => {
                        tx = hx;
                        ty = hy - 1;
                    }
                    "L" => {
                        tx = hx + 1;
                        ty = hy;
                    }
                    "D" => {
                        tx = hx;
                        ty = hy + 1;
                    }
                    _ => panic!("Invalid dir"),
                }
            }
            hashset.insert((tx, ty));
        }
    }
    println!("{}", hashset.len());
}
