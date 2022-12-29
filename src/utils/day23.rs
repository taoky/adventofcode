use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug)]
pub enum Direction {
    North, // Up
    South, // Down
    West,  // Left
    East,  // Right
}

pub type Map = HashSet<(isize, isize)>;

impl Direction {
    pub fn go(&self, x: isize, y: isize) -> (isize, isize) {
        match self {
            Direction::North => (x - 1, y),
            Direction::South => (x + 1, y),
            Direction::West => (x, y - 1),
            Direction::East => (x, y + 1),
        }
    }

    pub fn is_movable(&self, map: &Map, x: isize, y: isize) -> bool {
        !match self {
            Direction::North => {
                map.contains(&(x - 1, y))
                    || map.contains(&(x - 1, y - 1))
                    || map.contains(&(x - 1, y + 1))
            }
            Direction::South => {
                map.contains(&(x + 1, y))
                    || map.contains(&(x + 1, y - 1))
                    || map.contains(&(x + 1, y + 1))
            }
            Direction::West => {
                map.contains(&(x, y - 1))
                    || map.contains(&(x - 1, y - 1))
                    || map.contains(&(x + 1, y - 1))
            }
            Direction::East => {
                map.contains(&(x, y + 1))
                    || map.contains(&(x - 1, y + 1))
                    || map.contains(&(x + 1, y + 1))
            }
        }
    }
}

#[derive(Debug)]
pub struct Elf {
    pub coord: (isize, isize),
    propose_list: VecDeque<Direction>,
}

impl Elf {
    pub fn new(x: isize, y: isize) -> Self {
        Self {
            coord: (x, y),
            propose_list: [
                Direction::North,
                Direction::South,
                Direction::West,
                Direction::East,
            ]
            .into_iter()
            .collect(),
        }
    }

    pub fn is_empty(&self, map: &Map) -> bool {
        assert!(self.propose_list.len() == 4);
        self.propose_list
            .iter()
            .all(|dir| dir.is_movable(map, self.coord.0, self.coord.1))
    }

    pub fn propose(&self, map: &Map) -> Option<(isize, isize)> {
        let mut new_coord = None;
        for dir in self.propose_list.iter() {
            if dir.is_movable(map, self.coord.0, self.coord.1) {
                new_coord = Some(dir.go(self.coord.0, self.coord.1));
                break;
            }
        }

        new_coord
    }

    pub fn rotate(&mut self) {
        let first = self.propose_list.pop_front().unwrap();
        self.propose_list.push_back(first);
    }
}

pub fn find_border(map: &Map) -> (isize, isize, isize, isize) {
    let mut min_x = std::isize::MAX;
    let mut max_x = std::isize::MIN;
    let mut min_y = std::isize::MAX;
    let mut max_y = std::isize::MIN;

    for (x, y) in map.iter() {
        min_x = min_x.min(*x);
        max_x = max_x.max(*x);
        min_y = min_y.min(*y);
        max_y = max_y.max(*y);
    }

    (min_x, max_x, min_y, max_y)
}

pub fn pretty_print(map: &Map) {
    let (min_x, max_x, min_y, max_y) = find_border(map);

    for x in min_x..=max_x {
        for y in min_y..=max_y {
            if map.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}

pub fn simulate(elves: &mut Vec<Elf>, map: &mut Map, print: bool) -> Option<()> {
    assert!(elves.len() == map.len());
    if print {
        pretty_print(map);
    }
    let mut collision: HashMap<(isize, isize), usize> = HashMap::new();
    if elves.iter().all(|elf| elf.is_empty(map)) {
        return None;
    }

    let proposes: Vec<_> = elves
        .iter_mut()
        .map(|elf| {
            if elf.is_empty(map) {
                return None;
            }
            let proposal = elf.propose(map);
            if let Some(proposal) = proposal {
                collision
                    .entry(proposal)
                    .and_modify(|f| *f += 1)
                    .or_insert(1);
            };
            proposal
        })
        .collect();

    elves.iter_mut().for_each(|x| x.rotate());

    for (i, proposal) in proposes.iter().enumerate() {
        if let Some(proposal) = proposal {
            if collision[proposal] == 1 {
                assert!(map.remove(&elves[i].coord));

                elves[i].coord = *proposal;
                assert!(map.insert(*proposal));
            }
        }
    }

    Some(())
}
