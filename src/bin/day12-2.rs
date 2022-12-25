use std::collections::VecDeque;

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

fn main() {
    let mut map: Vec<Vec<i32>> = Vec::new();
    let mut start: Option<(i32, i32)> = None;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        map.push(
            input
                .trim()
                .chars()
                .enumerate()
                .map(|(idx, mut x)| {
                    if x == 'S' {
                        x = 'a';
                    } else if x == 'E' {
                        start = Some((map.len() as i32, idx as i32));
                        x = 'z';
                    }
                    (x as i32) - ('a' as i32)
                })
                .collect(),
        );
    }

    let mut shortest_record = vec![vec![None; map[0].len()]; map.len()];

    // BFS
    let start = start.unwrap();
    let mut queue = VecDeque::new();
    queue.push_back((start, 0));

    let mut minimal = i32::MAX;
    while !queue.is_empty() {
        let (pos, dist) = queue.pop_front().unwrap();
        let pos_height = map[pos.0 as usize][pos.1 as usize];
        for dir in [Dir::Down, Dir::Left, Dir::Right, Dir::Up] {
            let target = match dir {
                Dir::Down => (pos.0 + 1, pos.1),
                Dir::Left => (pos.0, pos.1 - 1),
                Dir::Right => (pos.0, pos.1 + 1),
                Dir::Up => (pos.0 - 1, pos.1),
            };
            // check validity
            if target.0 < 0
                || target.0 >= map.len() as i32
                || target.1 < 0
                || target.1 >= map[0].len() as i32
            {
                continue;
            }
            // check if visited
            if let Some(d) = shortest_record[target.0 as usize][target.1 as usize] {
                if dist + 1 >= d {
                    continue;
                }
            }
            // check if rev-climbable
            let target_height = map[target.0 as usize][target.1 as usize];
            if pos_height - 1 <= target_height {
                queue.push_back((target, dist + 1));
                shortest_record[target.0 as usize][target.1 as usize] = Some(dist + 1);
                if map[target.0 as usize][target.1 as usize] == 0 && dist + 1 < minimal {
                    minimal = dist + 1;
                }
            }
        }
    }
    println!("{}", minimal);
}
