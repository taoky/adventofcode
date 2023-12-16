// A copy of Haskell version's algorithm
use std::{
    fs::File,
    io::{BufRead, BufReader}, collections::HashMap,
};

type Num = u64;

fn fast_count(x: &[char], y: &[Num], mut hs: &mut HashMap<(Vec<char>, Vec<Num>), Num>) -> Num {
    if hs.contains_key(&(x.to_vec(), y.to_vec())) {
        return *hs.get(&(x.to_vec(), y.to_vec())).unwrap();
    }
    let res = if x.len() == 0 {
        if y.len() == 0 {
            1
        } else {
            0
        }
    } else {
        if y.len() == 0 {
            if x.contains(&'#') {
                0
            } else {
                1
            }
        } else {
            if x.len() < y.iter().sum::<Num>() as usize + y.len() - 1 {
                0
            } else {
                let first_char = x[0];
                match first_char {
                    '.' => fast_count(&x[1..], y, &mut hs),
                    '#' => {
                        let first_group = y[0] as usize;
                        if !&x[..first_group].contains(&'.')
                            && (x.len() == first_group || x[first_group] != '#')
                        {
                            let new_x = if x.len() == first_group {
                                &[]
                            } else {
                                &x[first_group + 1..]
                            };
                            fast_count(new_x, &y[1..], &mut hs)
                        } else {
                            0
                        }
                    }
                    _ => {
                        let mut new_x = vec![];
                        new_x.push('#');
                        new_x.extend_from_slice(&x[1..]);
                        let mut new_x2 = vec![];
                        new_x2.push('.');
                        new_x2.extend_from_slice(&x[1..]);
                        fast_count(&new_x, y, &mut hs) + fast_count(&new_x2, y, &mut hs)
                    }
                }
            }
        }
    };
    hs.insert((x.to_vec(), y.to_vec()), res);
    res
}

fn enlarge(x: &[char], y: &[Num]) -> (Vec<char>, Vec<Num>) {
    let mut new_x = vec![];
    let mut new_y = vec![];
    for i in 0..5 {
        new_x.extend_from_slice(x);
        new_y.extend_from_slice(y);
        if i != 4 {
            new_x.push('?');
        }
    }
    (new_x, new_y)
}

fn main() {
    let file = File::open("../input/day12").unwrap();
    let reader = BufReader::new(&file).lines();
    let mut result = vec![];
    let mut hs = HashMap::new();
    for line in reader {
        let line = line.unwrap();
        let items: Vec<_> = line.split(" ").collect();
        assert!(items.len() == 2);
        let chars = items[0].chars().collect::<Vec<_>>();
        let nums: Vec<Num> = items[1].split(",").map(|x| x.parse().unwrap()).collect();
        let (chars, nums) = enlarge(&chars, &nums);
        result.push(fast_count(&chars, &nums, &mut hs));
    }
    println!("{}", result.iter().sum::<Num>());
}
