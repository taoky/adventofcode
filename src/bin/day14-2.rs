use std::collections::HashSet;

fn main() {
    let mut hs = HashSet::new();
    let mut abyss = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let points: Vec<_> = input
            .trim()
            .split(" -> ")
            .map(|x| {
                let list = x
                    .split(',')
                    .map(|x| x.parse::<i32>().unwrap())
                    .collect::<Vec<i32>>();
                assert!(list.len() == 2);
                (list[0], list[1])
            })
            .collect();
        for idx in 1..points.len() {
            let (x1, y1) = points[idx - 1];
            let (x2, y2) = points[idx];
            if x1 == x2 {
                let iter = if y1 < y2 { y1..=y2 } else { y2..=y1 };
                let mut iter = iter.peekable();
                let lowest = iter.peek().unwrap();
                if *lowest > abyss {
                    abyss = *lowest;
                }
                for i in iter {
                    hs.insert((x1, i));
                }
            } else if y1 == y2 {
                let iter = if x1 < x2 { x1..=x2 } else { x2..=x1 };
                if y1 > abyss {
                    abyss = y1;
                }
                for i in iter {
                    hs.insert((i, y1));
                }
            } else {
                panic!("Invalid input")
            }
        }
    }

    // pouring sand
    let mut cnt = 0;
    loop {
        let mut sand_cord = (500, 0);
        let mut end = false;
        loop {
            let next_down = (sand_cord.0, sand_cord.1 + 1);
            let next_left = (sand_cord.0 - 1, sand_cord.1 + 1);
            let next_right = (sand_cord.0 + 1, sand_cord.1 + 1);
            if sand_cord.1 + 1 >= abyss + 2 {
                break;
            } else if !hs.contains(&next_down) {
                sand_cord = next_down;
            } else if !hs.contains(&next_left) {
                sand_cord = next_left;
            } else if !hs.contains(&next_right) {
                sand_cord = next_right;
            } else {
                if sand_cord == (500, 0) {
                    end = true;
                }
                break;
            }
        }
        hs.insert(sand_cord);
        cnt += 1;
        if end {
            println!("{}", cnt);
            break;
        }
    }
}
