use std::fmt::Debug;

#[derive(Debug)]
enum Instruction {
    Steps(i32),
    Clockwise,
    CounterClockwise,
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum Block {
    Open,
    Wall,
    Void,
}

impl Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Block::Open => write!(f, "."),
            Block::Wall => write!(f, "#"),
            Block::Void => write!(f, " "),
        }
    }
}

#[derive(Default, Debug)]
struct Map {
    map: Vec<Vec<Block>>,
    width: usize,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    #[default]
    Right,
}

#[derive(Debug, Default)]
struct State {
    coord: (isize, isize),
    direction: Direction,
}

impl Map {
    fn fetch(&self, x: isize, y: isize) -> Block {
        self.map[x as usize][y as usize]
    }

    #[allow(clippy::manual_range_contains)]
    fn void_transfer(&self, x: isize, y: isize, dir: Direction) -> ((isize, isize), Direction) {
        let height = self.map.len() as isize;
        let width = self.width as isize;
        assert!(x < 0 || x >= height || y < 0 || y >= width || self.fetch(x, y) == Block::Void);

        let (new_coord, new_dir) = match width {
            16 => {
                // example
                if x == -1 {
                    assert!(8 <= y && y <= 11 && dir == Direction::Up);
                    ((4, 11 - y), Direction::Down)
                } else if y == 7 && 0 <= x && x <= 3 {
                    if dir == Direction::Left {
                        ((4, x + 4), Direction::Down)
                    } else if dir == Direction::Up {
                        assert!(x == 3);
                        ((3, 8), Direction::Right)
                    } else {
                        unreachable!()
                    }
                } else if x == 3 && 0 <= y && y <= 3 {
                    assert!(dir == Direction::Up);
                    ((0, 11 - y), Direction::Down)
                } else if x == 3 && 4 <= y && y < 7 {
                    assert!(dir == Direction::Up);
                    ((y - 4, 8), Direction::Right)
                } else if y == 12 && 0 <= x && x <= 3 {
                    assert!(dir == Direction::Right);
                    ((11 - x, 15), Direction::Left)
                } else if y == -1 && 4 <= x && x <= 7 {
                    assert!(dir == Direction::Left);
                    ((11, 19 - x), Direction::Up)
                } else if y == 12 && 4 <= x && x <= 7 {
                    assert!(dir == Direction::Right);
                    if dir == Direction::Right {
                        ((8, 19 - x), Direction::Down)
                    } else if dir == Direction::Up {
                        assert!(x == 7);
                        ((7, 11), Direction::Left)
                    } else {
                        unreachable!()
                    }
                } else if x == 7 && 12 < y && y <= 15 {
                    assert!(dir == Direction::Up);
                    ((19 - y, 11), Direction::Left)
                } else if x == 8 && 0 <= y && y <= 3 {
                    assert!(dir == Direction::Down);
                    ((11, 11 - y), Direction::Up)
                } else if x == 8 && 4 <= y && y <= 7 {
                    if dir == Direction::Down {
                        ((15 - y, 8), Direction::Right)
                    } else if dir == Direction::Left {
                        assert!(y == 7);
                        ((7, 7), Direction::Up)
                    } else {
                        unreachable!()
                    }
                } else if y == 7 && 8 < x && x <= 11 {
                    assert!(dir == Direction::Left);
                    ((7, 15 - x), Direction::Up)
                } else if y == 16 && 8 <= x && x <= 11 {
                    assert!(dir == Direction::Right);
                    ((11 - x, 11), Direction::Left)
                } else if x == 12 && 8 <= y && y <= 11 {
                    assert!(dir == Direction::Down);
                    ((7, 11 - y), Direction::Up)
                } else if x == 12 && 12 <= y && y <= 15 {
                    assert!(dir == Direction::Down);
                    ((19 - y, 0), Direction::Right)
                } else {
                    unreachable!()
                }
            }
            150 => {
                // input
                if x == -1 && 50 <= y && y <= 99 {
                    assert!(dir == Direction::Up);
                    ((100 + y, 0), Direction::Right)
                } else if x == -1 && 100 <= y && y <= 149 {
                    assert!(dir == Direction::Up);
                    ((199, y - 100), Direction::Up)
                } else if y == 49 && 0 <= x && x <= 49 {
                    assert!(dir == Direction::Left);
                    ((149 - x, 0), Direction::Right)
                } else if y == 49 && 50 <= x && x <= 99 {
                    if dir == Direction::Left {
                        ((100, x - 50), Direction::Down)
                    } else if dir == Direction::Up {
                        assert!(x == 99);
                        ((99, 50), Direction::Right)
                    } else {
                        unreachable!()
                    }
                } else if y == 150 && 0 <= x && x <= 49 {
                    assert!(dir == Direction::Right);
                    ((149 - x, 99), Direction::Left)
                } else if x == 50 && 100 <= y && y <= 149 {
                    if dir == Direction::Down {
                        ((y - 50, 99), Direction::Left)
                    } else if dir == Direction::Right {
                        assert!(y == 149);
                        ((49, 100), Direction::Up)
                    } else {
                        unreachable!()
                    }
                } else if y == 100 && 50 < x && x <= 99 {
                    assert!(dir == Direction::Right);
                    ((49, x + 50), Direction::Up)
                } else if x == 99 && 0 <= y && y <= 49 {
                    assert!(dir == Direction::Up);
                    ((y + 50, 50), Direction::Right)
                } else if y == -1 && 100 <= x && x <= 149 {
                    assert!(dir == Direction::Left);
                    ((149 - x, 50), Direction::Right)
                } else if y == 100 && 100 <= x && x <= 149 {
                    assert!(dir == Direction::Right);
                    ((149 - x, 149), Direction::Left)
                } else if x == 150 && 50 <= y && y <= 99 {
                    if dir == Direction::Down {
                        ((100 + y, 49), Direction::Left)
                    } else if dir == Direction::Right {
                        assert!(y == 50);
                        ((149, 50), Direction::Up)
                    } else {
                        unreachable!()
                    }
                } else if y == -1 && 150 <= x && x <= 199 {
                    assert!(dir == Direction::Left);
                    ((0, x - 100), Direction::Down)
                } else if y == 50 && 150 < x && x <= 199 {
                    assert!(dir == Direction::Right);
                    ((149, x - 100), Direction::Up)
                } else if x == 200 && 0 <= y && y <= 49 {
                    assert!(dir == Direction::Down);
                    ((0, y + 100), Direction::Down)
                } else {
                    println!("{} {}", x, y);
                    unreachable!()
                }
            }
            _ => unimplemented!(),
        };
        println!("{:?} {:?}", new_coord, new_dir);
        assert!(
            new_coord.0 >= 0 && new_coord.0 < height && new_coord.1 >= 0 && new_coord.1 < width
        );
        assert!(self.fetch(new_coord.0, new_coord.1) != Block::Void);
        (new_coord, new_dir)
    }
}

fn main() {
    let mut strings = Vec::new();
    let mut map = Map::default();
    let mut instructions = Vec::new();
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim().is_empty() {
            std::io::stdin().read_line(&mut input).unwrap();
            let mut num = 0;
            for i in input.trim().chars() {
                match i {
                    'L' => {
                        instructions.push(Instruction::Steps(num));
                        instructions.push(Instruction::CounterClockwise);
                        num = 0;
                    }
                    'R' => {
                        instructions.push(Instruction::Steps(num));
                        instructions.push(Instruction::Clockwise);
                        num = 0;
                    }
                    _ => {
                        num = num * 10 + i.to_digit(10).unwrap() as i32;
                    }
                }
            }
            if num > 0 {
                instructions.push(Instruction::Steps(num));
            }
            break;
        }
        strings.push(input.trim_end().to_string());
    }
    let width = strings
        .iter()
        .max_by(|x, y| (x.len()).cmp(&y.len()))
        .unwrap()
        .len();
    for i in strings {
        let mut row = Vec::new();
        for j in i.chars() {
            match j {
                '.' => row.push(Block::Open),
                '#' => row.push(Block::Wall),
                _ => row.push(Block::Void),
            }
        }
        while row.len() < width {
            row.push(Block::Void);
        }
        map.map.push(row);
    }
    map.width = width;

    // println!("{:?}", instructions);
    // println!("{:?}", map);

    let mut start_point = (0, 0);
    for i in 0..map.map.len() {
        let mut got = false;
        for j in 0..map.map[i].len() {
            if map.map[i][j] == Block::Open {
                start_point = (i as isize, j as isize);
                got = true;
                break;
            }
        }
        if got {
            break;
        }
    }
    // println!("{:?}", start_point);

    let mut state = State {
        coord: start_point,
        ..Default::default()
    };

    for inst in instructions {
        // println!("{:?}", inst);
        match inst {
            Instruction::Steps(num) => {
                for _ in 0..num {
                    let new_coord = match state.direction {
                        Direction::Up => (state.coord.0 - 1, state.coord.1),
                        Direction::Down => (state.coord.0 + 1, state.coord.1),
                        Direction::Left => (state.coord.0, state.coord.1 - 1),
                        Direction::Right => (state.coord.0, state.coord.1 + 1),
                    };
                    let (new_coord, new_dir) = if new_coord.0 < 0
                        || new_coord.1 < 0
                        || new_coord.0 >= map.map.len() as isize
                        || new_coord.1 >= map.map[0].len() as isize
                    {
                        map.void_transfer(new_coord.0, new_coord.1, state.direction)
                    } else {
                        (new_coord, state.direction)
                    };
                    match map.fetch(new_coord.0, new_coord.1) {
                        Block::Open => {
                            state.coord = new_coord;
                            state.direction = new_dir;
                        }
                        Block::Wall => {
                            break;
                        }
                        Block::Void => {
                            let (new_coord, new_dir) =
                                map.void_transfer(new_coord.0, new_coord.1, new_dir);

                            match map.fetch(new_coord.0, new_coord.1) {
                                Block::Open => {
                                    state.coord = new_coord;
                                    state.direction = new_dir;
                                }
                                Block::Wall => {
                                    break;
                                }
                                Block::Void => {
                                    panic!("void transfer failed");
                                }
                            }
                        }
                    }
                    // println!("{:?}", state);
                }
            }
            Instruction::Clockwise => match state.direction {
                Direction::Up => {
                    state.direction = Direction::Right;
                }
                Direction::Down => {
                    state.direction = Direction::Left;
                }
                Direction::Left => {
                    state.direction = Direction::Up;
                }
                Direction::Right => {
                    state.direction = Direction::Down;
                }
            },
            Instruction::CounterClockwise => match state.direction {
                Direction::Up => {
                    state.direction = Direction::Left;
                }
                Direction::Down => {
                    state.direction = Direction::Right;
                }
                Direction::Left => {
                    state.direction = Direction::Down;
                }
                Direction::Right => {
                    state.direction = Direction::Up;
                }
            },
        }
    }

    println!(
        "{:?}",
        1000 * (state.coord.0 + 1)
            + 4 * (state.coord.1 + 1)
            + match state.direction {
                Direction::Up => 3,
                Direction::Down => 1,
                Direction::Left => 2,
                Direction::Right => 0,
            }
    );
}
