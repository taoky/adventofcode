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
}

#[derive(Debug, Default)]
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
    println!("{:?}", instructions);
    println!("{:?}", map);

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
    println!("{:?}", start_point);

    let mut state = State {
        coord: start_point,
        ..Default::default()
    };

    for inst in instructions {
        match inst {
            Instruction::Steps(num) => {
                for _ in 0..num {
                    let new_coord = match state.direction {
                        Direction::Up => (state.coord.0 - 1, state.coord.1),
                        Direction::Down => (state.coord.0 + 1, state.coord.1),
                        Direction::Left => (state.coord.0, state.coord.1 - 1),
                        Direction::Right => (state.coord.0, state.coord.1 + 1),
                    };
                    if new_coord.0 < 0
                        || new_coord.1 < 0
                        || new_coord.0 >= map.map.len() as isize
                        || new_coord.1 >= map.map[0].len() as isize
                    {
                        continue;
                    }
                    match map.fetch(new_coord.0, new_coord.1) {
                        Block::Open => {
                            state.coord = new_coord;
                        }
                        Block::Wall => {
                            break;
                        }
                        Block::Void => {
                            unimplemented!();
                        }
                    }
                    println!("{:?}", state);
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
}
