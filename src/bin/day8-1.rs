enum Direction {
    Updown,
    Leftright,
    Rightleft,
    Downup,
}

struct Tree {
    height: i32,
    marked: bool,
}

fn main() {
    let mut trees: Vec<Vec<Tree>> = Vec::new();
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let tree = input
            .trim()
            .chars()
            .map(|x| Tree {
                height: x.to_digit(10).unwrap() as i32,
                marked: false,
            })
            .collect();
        trees.push(tree);
    }

    assert!(trees.len() == trees[0].len());

    for dir in [
        Direction::Updown,
        Direction::Leftright,
        Direction::Rightleft,
        Direction::Downup,
    ] {
        for i in 0..trees.len() {
            let mut maximum = -1;
            for j in 0..trees.len() {
                let x = match dir {
                    Direction::Updown => j,
                    Direction::Leftright => i,
                    Direction::Rightleft => i,
                    Direction::Downup => trees.len() - 1 - j,
                };
                let y = match dir {
                    Direction::Updown => i,
                    Direction::Leftright => j,
                    Direction::Rightleft => trees.len() - 1 - j,
                    Direction::Downup => i,
                };
                if trees[x][y].height > maximum {
                    maximum = trees[x][y].height;
                    trees[x][y].marked = true;
                }
            }
        }
    }

    let mut cnt = 0;
    for i in trees {
        for j in i {
            if j.marked {
                cnt += 1;
            }
        }
    }
    println!("{}", cnt);
}
