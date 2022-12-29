use std::collections::{HashSet, VecDeque};

// type Map = HashMap<(usize, usize), Vec<usize>>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
pub struct Blizzard {
    pub coord: (isize, isize),
    pub direction: Direction,
}

impl Blizzard {
    pub fn go_rounds(&self, rounds: isize, height: isize, width: isize) -> (isize, isize) {
        // assuming no up/down blizzard on (*, 1) and (*, width - 2)
        match self.direction {
            Direction::Up => (
                (self.coord.0.wrapping_sub(1).wrapping_sub(rounds)).rem_euclid(height - 2) + 1,
                self.coord.1,
            ),
            Direction::Down => (
                (self.coord.0.wrapping_sub(1) + rounds).rem_euclid(height - 2) + 1,
                self.coord.1,
            ),
            Direction::Left => (
                self.coord.0,
                (self.coord.1.wrapping_sub(1).wrapping_sub(rounds)).rem_euclid(width - 2) + 1,
            ),
            Direction::Right => (
                self.coord.0,
                (self.coord.1.wrapping_sub(1) + rounds).rem_euclid(width - 2) + 1,
            ),
        }
    }
}

pub fn bfs(
    start_coord: (isize, isize),
    end_coord: (isize, isize),
    height: isize,
    width: isize,
    blizzards: &[Blizzard],
    start_rounds: isize,
) -> isize {
    let mut queue = VecDeque::new();
    let mut dedup = HashSet::new();
    queue.push_back((start_coord, start_rounds));
    dedup.insert((start_coord, start_rounds));

    // speed up bfs
    let mut cache: Vec<Vec<Vec<bool>>> = Vec::new();

    while !queue.is_empty() {
        let head = queue.pop_front().unwrap();
        if head.0 == end_coord {
            return head.1;
        }

        for choice in [
            Some(Direction::Up),
            Some(Direction::Down),
            Some(Direction::Left),
            Some(Direction::Right),
            None,
        ] {
            let next_coord = match choice {
                Some(dir) => {
                    let next = match dir {
                        Direction::Up => {
                            if head.0 .0 == 0 {
                                continue;
                            }
                            (head.0 .0 - 1, head.0 .1)
                        }
                        Direction::Down => (head.0 .0 + 1, head.0 .1),
                        Direction::Left => {
                            if head.0 .1 == 0 {
                                continue;
                            }
                            (head.0 .0, head.0 .1 - 1)
                        }
                        Direction::Right => (head.0 .0, head.0 .1 + 1),
                    };
                    if next != end_coord
                        && (next.0 == 0
                            || next.0 >= height - 1
                            || next.1 == 0
                            || next.1 >= width - 1)
                    {
                        continue;
                    }
                    next
                }
                None => head.0,
            };

            for _ in cache.len()..(head.1 as usize + 2 - start_rounds as usize) {
                let mut item = vec![vec![false; width as usize]; height as usize];
                for blizzard in blizzards.iter() {
                    let next = blizzard.go_rounds(head.1 + 1, height, width);
                    item[next.0 as usize][next.1 as usize] = true;
                }
                cache.push(item);
            }
            assert!(cache.len() == (head.1 as usize + 2 - start_rounds as usize));
            // println!("cache len: {}", cache.len());
            // println!("next coord: {:?}", (next_coord.0, next_coord.1));
            if cache[(head.1 + 1 - start_rounds) as usize][next_coord.0 as usize]
                [next_coord.1 as usize]
            {
                continue;
            }

            // let mut success = true;
            // for blizzard in blizzards.iter() {
            // movable?
            // let conflict_coord = blizzard.go_rounds(head.1, height, width);
            // match choice {
            //     None => {}
            //     Some(Direction::Up) => {
            //         if blizzard.direction == Direction::Down
            //             && conflict_coord.0 == next_coord.0 - 1
            //             && conflict_coord.1 == next_coord.1
            //         {
            //             success = false;
            //             break;
            //         }
            //     }
            //     Some(Direction::Down) => {
            //         if blizzard.direction == Direction::Up
            //             && conflict_coord.0 == next_coord.0 + 1
            //             && conflict_coord.1 == next_coord.1
            //         {
            //             success = false;
            //             break;
            //         }
            //     }
            //     Some(Direction::Left) => {
            //         if blizzard.direction == Direction::Right
            //             && conflict_coord.0 == next_coord.0
            //             && conflict_coord.1 == next_coord.1 - 1
            //         {
            //             success = false;
            //             break;
            //         }
            //     }
            //     Some(Direction::Right) => {
            //         if blizzard.direction == Direction::Left
            //             && conflict_coord.0 == next_coord.0
            //             && conflict_coord.1 == next_coord.1 + 1
            //         {
            //             success = false;
            //             break;
            //         }
            //     }
            // };

            // let next = blizzard.go_rounds(head.1 + 1, height, width);
            // if next == next_coord {
            //     success = false;
            //     break;
            // }
            // }
            // if !success {
            //     continue;
            // }

            let value = (next_coord, head.1 + 1);
            if dedup.contains(&value) {
                continue;
            }
            // println!("{:?} -> {:?}", head, value);
            queue.push_back(value);
            dedup.insert(value);
        }
    }

    0
}
