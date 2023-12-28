use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs::File,
    io::{BufRead, BufReader},
};

#[derive(Debug, PartialEq, Clone, Copy)]
enum Item {
    Path,
    Forest,
    UpSlope,
    DownSlope,
    LeftSlope,
    RightSlope,
}

#[derive(Debug)]
struct Matrix {
    data: Vec<Item>,
    width: i64,
    height: i64,
}

impl Matrix {
    fn get(&self, x: i64, y: i64) -> &Item {
        self.data.get((x * self.width + y) as usize).unwrap()
    }

    fn feasible(&self, x: i64, y: i64) -> bool {
        x >= 0 && y >= 0 && x < self.height && y < self.width && self.get(x, y) != &Item::Forest
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn towards(direction: &Direction, x: i64, y: i64) -> (i64, i64) {
    match direction {
        Direction::Up => (x - 1, y),
        Direction::Down => (x + 1, y),
        Direction::Left => (x, y - 1),
        Direction::Right => (x, y + 1),
    }
}

fn get_nearby(matrix: &Matrix, x: i64, y: i64) -> Vec<(i64, i64)> {
    let mut result = Vec::new();
    for direction in vec![
        Direction::Up,
        Direction::Down,
        Direction::Left,
        Direction::Right,
    ] {
        let (x, y) = towards(&direction, x, y);
        if matrix.feasible(x, y) {
            result.push((x, y));
        }
    }
    result
}

fn is_junction(matrix: &Matrix, x: i64, y: i64) -> bool {
    let count = get_nearby(matrix, x, y).len();
    count > 2 || x == 0 || x == matrix.height - 1
}

fn towards_until_junction(
    matrix: &Matrix,
    x: i64,
    y: i64,
    direction: &Direction,
    acc: i64,
) -> Option<((i64, i64), Direction, i64)> {
    let mut dirs = vec![direction.clone()];
    match direction {
        Direction::Up => dirs.extend(vec![Direction::Left, Direction::Right]),
        Direction::Down => dirs.extend(vec![Direction::Left, Direction::Right]),
        Direction::Left => dirs.extend(vec![Direction::Up, Direction::Down]),
        Direction::Right => dirs.extend(vec![Direction::Up, Direction::Down]),
    }
    for dir in dirs {
        let (xnew, ynew) = towards(&dir, x, y);
        if matrix.feasible(xnew, ynew) {
            if is_junction(matrix, xnew, ynew) {
                return Some(((xnew, ynew), dir, acc + 1));
            } else {
                return towards_until_junction(matrix, xnew, ynew, &dir, acc + 1);
            }
        }
    }
    None
}

type Graph = HashMap<(i64, i64), Vec<((i64, i64), i64)>>;

fn on2_dedup(v: &Vec<((i64, i64), i64)>) -> Vec<((i64, i64), i64)> {
    let mut result = Vec::new();
    for (xy, weight) in v {
        if !result.iter().any(|(xy_, _)| xy_ == xy) {
            result.push((*xy, *weight));
        }
    }
    result
}

fn make_junction_graph(matrix: &Matrix, x: i64, y: i64) -> Graph {
    fn next_dirs(d: &Direction) -> Vec<Direction> {
        match d {
            Direction::Up => vec![Direction::Left, Direction::Right, Direction::Up],
            Direction::Down => vec![Direction::Left, Direction::Right, Direction::Down],
            Direction::Left => vec![Direction::Up, Direction::Down, Direction::Left],
            Direction::Right => vec![Direction::Up, Direction::Down, Direction::Right],
        }
    }
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    let mut graph: Graph = HashMap::new();
    queue.push_back(((x, y), Direction::Down));
    while !queue.is_empty() {
        let ((x, y), direction) = queue.pop_front().unwrap();
        visited.insert((x, y));
        let res = towards_until_junction(matrix, x, y, &direction, 0);
        let ((x_, y_), dir_, weight) = match res {
            None => continue,
            Some(res) => res,
        };
        if visited.contains(&(x_, y_)) {
            continue;
        }
        graph
            .entry((x, y))
            .or_insert(Vec::new())
            .push(((x_, y_), weight));
        graph
            .entry((x_, y_))
            .or_insert(Vec::new())
            .push(((x, y), weight));
        // dedup
        let gxy = graph.get(&(x, y)).unwrap();
        // gxy.dedup();
        graph.insert((x, y), on2_dedup(gxy));
        let gxy_ = graph.get(&(x_, y_)).unwrap();
        // gxy_.dedup();
        graph.insert((x_, y_), on2_dedup(gxy_));
        for d in next_dirs(&dir_) {
            queue.push_back(((x_, y_), d));
        }
    }
    graph
}

fn dfs<F>(
    graph: &Graph,
    visited: &mut HashSet<(i64, i64)>,
    x: i64,
    y: i64,
    acc: i64,
    is_end: &F,
) -> i64
where
    F: Fn(i64) -> bool,
{
    if is_end(x) {
        return acc;
    }
    let nearby = match graph.get(&(x, y)) {
        None => return -1,
        Some(nearby) => nearby,
    };
    let nearby: Vec<_> = nearby
        .iter()
        .filter(|(xy, _)| !visited.contains(xy))
        .collect();
    if nearby.is_empty() {
        return -1;
    }
    let mut max = -1;
    for (xy, weight) in nearby {
        visited.insert(*xy);
        let res = dfs(graph, visited, xy.0, xy.1, acc + weight, is_end);
        if res != -1 && res > max {
            max = res;
        }
        visited.remove(xy);
    }
    max
}

fn main() {
    let file = File::open("../input/day23").unwrap();
    let reader = BufReader::new(file).lines();
    let mut matrix = Matrix {
        data: Vec::new(),
        width: 0,
        height: 0,
    };
    for line in reader {
        let line = line.unwrap();
        assert!(matrix.width == 0 || matrix.width as usize == line.len());
        matrix.width = line.len() as i64;
        matrix.height += 1;
        for c in line.chars() {
            matrix.data.push(match c {
                '.' => Item::Path,
                '#' => Item::Forest,
                '^' => Item::UpSlope,
                'v' => Item::DownSlope,
                '<' => Item::LeftSlope,
                '>' => Item::RightSlope,
                _ => panic!("Unknown character"),
            });
        }
    }
    let mut start: (i64, i64) = (i64::MAX, i64::MAX);
    for y in 0..matrix.width {
        if matrix.get(0, y) != &Item::Forest {
            start = (0, y);
            break;
        }
    }
    assert!(start != (i64::MAX, i64::MAX));
    let junction_graph = make_junction_graph(&matrix, start.0, start.1);
    println!("{:?} {:?}", start, junction_graph);
    let mut visited = HashSet::new();
    visited.insert(start);
    let res = dfs(&junction_graph, &mut visited, start.0, start.1, 0, &|x| {
        x == matrix.height - 1
    });
    println!("Part 2: {}", res);
}
