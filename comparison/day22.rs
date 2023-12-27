use std::{
    fs::File,
    io::{BufRead, BufReader},
};

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
struct Item {
    start: (i32, i32, i32),
    end: (i32, i32, i32),
}

fn item_sort(x: &Item, y: &Item) -> std::cmp::Ordering {
    let z0 = x.start.2;
    let z1 = y.start.2;
    z0.cmp(&z1)
}

fn is_xy_covering(a: &Item, b: &Item) -> bool {
    let x1 = a.start.0;
    let y1 = a.start.1;
    let x2 = a.end.0;
    let y2 = a.end.1;
    let x1_ = b.start.0;
    let y1_ = b.start.1;
    let x2_ = b.end.0;
    let y2_ = b.end.1;
    (x1 <= x2_ && x1_ <= x2) && (y1 <= y2_ && y1_ <= y2)
}

fn get_supporting(item: Item, under_blocks: &[Item]) -> Vec<Item> {
    let mut supporting = vec![];
    let item_min_z = item.start.2;
    for block in under_blocks {
        if block.end.2 + 1 == item_min_z && is_xy_covering(&item, block) {
            supporting.push(block.clone());
        }
    }
    supporting
}

fn is_supporting(item: Item, under_blocks: &[Item]) -> bool {
    !get_supporting(item, under_blocks).is_empty()
}

fn lower(item: &Item) -> Option<Item> {
    let z1 = item.start.2;
    if z1 > 1 {
        Some(Item {
            start: (item.start.0, item.start.1, z1 - 1),
            end: (item.end.0, item.end.1, item.end.2 - 1),
        })
    } else {
        None
    }
}

fn gravity(blocks: &Vec<Item>) -> Vec<Item> {
    let mut new_blocks = vec![];
    for block in blocks {
        let mut block_ = block.clone();
        while !is_supporting(block_, &new_blocks) {
            let lblock = lower(&block_);
            if lblock.is_some() {
                block_ = lblock.unwrap();
            } else {
                break;
            }
        }
        new_blocks.push(block_.clone());
    }
    new_blocks
}

fn gravity_with_counter(blocks1: &[Item], blocks2: &[Item]) -> (i32, Vec<Item>) {
    let mut counter = 0;
    let mut new_blocks = blocks1.to_vec();
    for block in blocks2 {
        let z = block.start.2;
        if is_supporting(*block, &new_blocks) || z <= 1 {
            new_blocks.push(*block);
        } else {
            counter += 1 as i32;
        }
    }
    // println!("{:?} {:?} {}", blocks1, blocks2, counter);
    (counter, new_blocks.to_vec())
}

fn main() {
    let file = File::open("../input/day22").unwrap();
    let reader = BufReader::new(file).lines();
    let mut blocks = vec![];
    for line in reader {
        let line = line.unwrap();
        let items: Vec<_> = line.split('~').collect();
        assert!(items.len() == 2);
        let start: Vec<_> = items[0].split(',').map(|x| x.trim()).collect();
        assert!(start.len() == 3);
        let end: Vec<_> = items[1].split(',').map(|x| x.trim()).collect();
        assert!(end.len() == 3);
        let item = Item {
            start: (
                start[0].parse().unwrap(),
                start[1].parse().unwrap(),
                start[2].parse().unwrap(),
            ),
            end: (
                end[0].parse().unwrap(),
                end[1].parse().unwrap(),
                end[2].parse().unwrap(),
            ),
        };
        blocks.push(item);
    }
    blocks.sort_by(item_sort);
    let mut fallen_blocks = gravity(&blocks);
    fallen_blocks.sort_by(item_sort);
    assert!(fallen_blocks.len() == blocks.len());
    let rev_blocks = fallen_blocks.iter().rev().cloned().collect::<Vec<_>>();

    let mut hs1 = std::collections::HashSet::new();
    for (i, block) in rev_blocks.iter().enumerate() {
        let supporting = get_supporting(*block, &rev_blocks[i + 1..]);
        if supporting.len() == 1 {
            hs1.insert(supporting[0]);
        }
    }
    // println!("{:?}", fallen_blocks);
    println!("{}", blocks.len() - hs1.len());

    let mut res2 = vec![];
    for i in 0..fallen_blocks.len() {
        let b1 = &fallen_blocks[..i];
        let b2 = &fallen_blocks[(i + 1)..];
        res2.push(gravity_with_counter(b1, b2).0);
    }
    println!("{}", res2.iter().sum::<i32>());
}
