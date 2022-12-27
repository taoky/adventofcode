use std::collections::HashSet;

fn main() {
    let mut res = 0;
    let mut hs = HashSet::new();
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

        let mut offset = 6;
        if hs.contains(&(coord.0 - 1, coord.1, coord.2)) {
            offset -= 2;
        }
        if hs.contains(&(coord.0 + 1, coord.1, coord.2)) {
            offset -= 2;
        }
        if hs.contains(&(coord.0, coord.1 - 1, coord.2)) {
            offset -= 2;
        }
        if hs.contains(&(coord.0, coord.1 + 1, coord.2)) {
            offset -= 2;
        }
        if hs.contains(&(coord.0, coord.1, coord.2 - 1)) {
            offset -= 2;
        }
        if hs.contains(&(coord.0, coord.1, coord.2 + 1)) {
            offset -= 2;
        }
        hs.insert(coord);
        res += offset;
    }
    println!("{}", res);
}
