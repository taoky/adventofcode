// A copy of Haskell version's algorithm
use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn get_result(
    map: &Vec<Vec<char>>,
    start: (usize, usize),
    direction: Direction,
    mut hs: &mut HashSet<((usize, usize), Direction)>,
) {
    if hs.contains(&(start, direction)) {
        return;
    }
    hs.insert((start, direction));
    let current = map[start.0][start.1];
    let next_dirs = match current {
        '.' => vec![direction],
        '/' => match direction {
            Direction::Up => vec![Direction::Right],
            Direction::Down => vec![Direction::Left],
            Direction::Left => vec![Direction::Down],
            Direction::Right => vec![Direction::Up],
        },
        '\\' => match direction {
            Direction::Up => vec![Direction::Left],
            Direction::Down => vec![Direction::Right],
            Direction::Left => vec![Direction::Up],
            Direction::Right => vec![Direction::Down],
        },
        '-' => match direction {
            Direction::Up => vec![Direction::Left, Direction::Right],
            Direction::Down => vec![Direction::Left, Direction::Right],
            Direction::Left => vec![Direction::Left],
            Direction::Right => vec![Direction::Right],
        },
        '|' => match direction {
            Direction::Up => vec![Direction::Up],
            Direction::Down => vec![Direction::Down],
            Direction::Left => vec![Direction::Up, Direction::Down],
            Direction::Right => vec![Direction::Up, Direction::Down],
        },
        _ => unreachable!(),
    };
    for next_dir in next_dirs {
        let next = match next_dir {
            Direction::Up => (start.0 - 1, start.1),
            Direction::Down => (start.0 + 1, start.1),
            Direction::Left => (start.0, start.1 - 1),
            Direction::Right => (start.0, start.1 + 1),
        };
        // if next.0 < 0 || next.0 >= map.len() || next.1 < 0 || next.1 >= map[0].len() {
        if next.0 >= map.len() || next.1 >= map[0].len() {
            continue;
        }
        get_result(map, next, next_dir, &mut hs);
    }
}

fn main() {
    let file = File::open("../input/day16").unwrap();
    let reader = BufReader::new(file).lines();
    let mut map = vec![];
    for line in reader {
        let line = line.unwrap();
        let items: Vec<char> = line.chars().collect();
        map.push(items);
    }

    let height = map.len();
    let width = map[0].len();
    let mut choices = vec![];
    choices.extend((0..width).map(|x| ((0, x), Direction::Down)));
    choices.extend((0..width).map(|x| ((height - 1, x), Direction::Up)));
    choices.extend((0..height).map(|x| ((x, 0), Direction::Right)));
    choices.extend((0..height).map(|x| ((x, width - 1), Direction::Left)));

    let mut res = 0;
    for (start, direction) in choices {
        let mut hs = HashSet::new();
        get_result(&map, start, direction, &mut hs);
        let coords = hs.iter().map(|(x, _)| x).collect::<Vec<_>>();
        let mut hs = HashSet::new();
        for coord in coords {
            hs.insert(coord);
        }
        // println!("{}", hs.len());
        res = res.max(hs.len());
    }
    println!("{}", res)
}
