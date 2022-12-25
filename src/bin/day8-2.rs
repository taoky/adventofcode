enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn main() {
    let mut trees: Vec<Vec<i32>> = Vec::new();
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let tree = input
            .trim()
            .chars()
            .map(|x| x.to_digit(10).unwrap() as i32)
            .collect();
        trees.push(tree);
    }

    assert!(trees.len() == trees[0].len());

    let mut maximum = -1;

    for i in 0..trees.len() {
        for j in 0..trees.len() {
            let mut scores = Vec::new();
            for dir in [
                Direction::Up,
                Direction::Down,
                Direction::Left,
                Direction::Right,
            ] {
                let mut cnt = 0;
                let mut x = i;
                let mut y = j;
                loop {
                    match dir {
                        Direction::Up => {
                            if x == 0 {
                                break;
                            }
                            x -= 1;
                        }
                        Direction::Down => {
                            if x == trees.len() - 1 {
                                break;
                            }
                            x += 1;
                        }
                        Direction::Left => {
                            if y == 0 {
                                break;
                            }
                            y -= 1;
                        }
                        Direction::Right => {
                            if y == trees.len() - 1 {
                                break;
                            }
                            y += 1;
                        }
                    }
                    cnt += 1;
                    if trees[x][y] >= trees[i][j] {
                        break;
                    }
                }
                scores.push(cnt);
            }

            let score = scores[0] * scores[1] * scores[2] * scores[3];
            if score > maximum {
                maximum = score;
            }
        }
    }
    println!("{}", maximum);
}
