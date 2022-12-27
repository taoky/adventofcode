// Continue abusing std::collections::HashSet!

use std::collections::{HashSet, VecDeque};

macro_rules! coord_neighbors {
    ($coord: expr) => {
        [
            ($coord.0 - 1, $coord.1, $coord.2),
            ($coord.0 + 1, $coord.1, $coord.2),
            ($coord.0, $coord.1 - 1, $coord.2),
            ($coord.0, $coord.1 + 1, $coord.2),
            ($coord.0, $coord.1, $coord.2 - 1),
            ($coord.0, $coord.1, $coord.2 + 1),
        ]
    };
}

fn get_offset(hs: &HashSet<(i32, i32, i32)>, coord: (i32, i32, i32)) -> i32 {
    let mut offset = 6;
    for x in coord_neighbors!(coord) {
        if hs.contains(&x) {
            offset -= 2;
        }
    }
    offset
}

fn floodfill(
    cube_hs: &HashSet<(i32, i32, i32)>,
    trap_pot: &HashSet<(i32, i32, i32)>,
    coord: (i32, i32, i32),
    max_border: (i32, i32, i32),
    min_border: (i32, i32, i32),
) -> Option<Vec<(i32, i32, i32)>> {
    let mut res = HashSet::new();
    let mut queue = VecDeque::new();
    assert!(trap_pot.contains(&coord));
    queue.push_back(coord);
    res.insert(coord);

    while !queue.is_empty() {
        let coord = queue.pop_front().unwrap();

        for x in coord_neighbors!(coord) {
            if res.contains(&x) {
                continue;
            }
            if trap_pot.contains(&x) {
                queue.push_back(x);
                res.insert(x);
            } else if cube_hs.contains(&x) {
                continue;
            } else {
                if x.0 > max_border.0 || x.1 > max_border.1 || x.2 > max_border.2 {
                    return None;
                }
                if x.0 < min_border.0 || x.1 < min_border.1 || x.2 < min_border.2 {
                    return None;
                }
                // assuming that it is inside air pocket
                // as it's possible that a block not in trap_pot is inside air pocket
                // or it is outside, but if so finally the floodfilling will touch max/min border and fail
                queue.push_back(x);
                res.insert(x);
            }
        }
    }

    Some(res.into_iter().collect())
}

fn main() {
    let mut res = 0;
    let mut hs = HashSet::new();
    let mut trapped_potential = HashSet::new();

    let mut max_border = (0, 0, 0);
    let mut min_border = (i32::MAX, i32::MAX, i32::MAX);
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let coord = input
            .trim()
            .split(',')
            .map(|x| x.parse::<i32>().unwrap())
            .collect::<Vec<_>>();
        assert!(coord.len() == 3);
        let coord = (coord[0], coord[1], coord[2]);

        max_border.0 = max_border.0.max(coord.0);
        max_border.1 = max_border.1.max(coord.1);
        max_border.2 = max_border.2.max(coord.2);
        min_border.0 = min_border.0.min(coord.0);
        min_border.1 = min_border.1.min(coord.1);
        min_border.2 = min_border.2.min(coord.2);

        let offset = get_offset(&hs, coord);
        hs.insert(coord);
        res += offset;

        for x in coord_neighbors!(coord) {
            trapped_potential.insert(x);
        }
    }
    // println!("{:?} {:?}", max_border, min_border);
    let mut trapped_potential: HashSet<_> = trapped_potential.difference(&hs).cloned().collect();

    while !trapped_potential.is_empty() {
        let coord = *trapped_potential.iter().next().unwrap();
        // println!("head: {:?}", coord);
        assert!(!hs.contains(&coord));
        if let Some(cube) = floodfill(&hs, &trapped_potential, coord, max_border, min_border) {
            let mut offset = 0;
            let mut tmp_hs = HashSet::new();
            for x in cube.iter() {
                offset += get_offset(&tmp_hs, *x);
                tmp_hs.insert(*x);
            }
            // println!("{}", offset);
            // println!("{:?}", cube);
            res -= offset;
            for x in cube {
                // assert!(trapped_potential.remove(&x));
                trapped_potential.remove(&x);
            }
            assert!(!trapped_potential.contains(&coord));
        } else {
            assert!(trapped_potential.remove(&coord));
        }
    }

    println!("{}", res);
}
