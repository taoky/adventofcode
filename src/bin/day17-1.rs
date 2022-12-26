use std::{cmp::max, collections::HashSet};

macro_rules! insert_or_panic {
    ($hs: ident, $value: expr) => {
        if $value.1 < 0 || $value.1 > 6 {
            panic!("Invalid coord");
        }
        $hs.insert($value)
            .then(|| ())
            .unwrap_or_else(|| panic!("Duplicate value: {:?}", $value));
    };
}

macro_rules! check {
    ($hs: ident, $value: expr) => {
        if $hs.contains(&$value) {
            return false;
        }
    };
}

fn check(hs: &HashSet<(i32, i32)>, anchor: &(i32, i32), rock_id: usize) -> bool {
    match rock_id % 5 {
        0 => {
            check!(hs, anchor);
            check!(hs, (anchor.0, anchor.1 + 1));
            check!(hs, (anchor.0, anchor.1 + 2));
            check!(hs, (anchor.0, anchor.1 + 3));
        }
        1 => {
            check!(hs, (anchor.0, anchor.1 + 1));
            check!(hs, (anchor.0 + 1, anchor.1));
            check!(hs, (anchor.0 + 1, anchor.1 + 1));
            check!(hs, (anchor.0 + 1, anchor.1 + 2));
            check!(hs, (anchor.0 + 2, anchor.1 + 1));
        }
        2 => {
            check!(hs, anchor);
            check!(hs, (anchor.0, anchor.1 + 1));
            check!(hs, (anchor.0, anchor.1 + 2));
            check!(hs, (anchor.0 + 1, anchor.1 + 2));
            check!(hs, (anchor.0 + 2, anchor.1 + 2));
        }
        3 => {
            check!(hs, anchor);
            check!(hs, (anchor.0 + 1, anchor.1));
            check!(hs, (anchor.0 + 2, anchor.1));
            check!(hs, (anchor.0 + 3, anchor.1));
        }
        4 => {
            check!(hs, anchor);
            check!(hs, (anchor.0, anchor.1 + 1));
            check!(hs, (anchor.0 + 1, anchor.1));
            check!(hs, (anchor.0 + 1, anchor.1 + 1));
        }
        _ => unreachable!(),
    }
    true
}

fn main() {
    let rock_right: [i32; 5] = [3, 2, 2, 0, 1];
    // let rock_down: [Vec<i32>; 5] = [vec![0, 1, 2, 3], vec![1], vec![0, 1, 2], vec![0], vec![0, 1]];

    let mut seq = String::new();
    std::io::stdin().read_line(&mut seq).unwrap();
    let seq: Vec<char> = seq.trim().chars().collect();

    let mut chamber = HashSet::new();
    let mut height = 0;
    let mut seq_ptr = 0;
    for rock_id in 0..2022 {
        let mut anchor = (height + 3, 2);
        // println!("anchor: {:?}", anchor);
        loop {
            let dir = seq[seq_ptr % seq.len()];
            seq_ptr += 1;

            // test pushing
            let moved_y = match dir {
                '>' => {
                    let moved_y = anchor.1 + 1;
                    if moved_y + rock_right[rock_id % 5] > 6
                        || !check(&chamber, &(anchor.0, moved_y), rock_id)
                    {
                        anchor.1
                    } else {
                        anchor.1 + 1
                    }
                }
                '<' => {
                    let moved_y = anchor.1 - 1;
                    if moved_y >= 0 && check(&chamber, &(anchor.0, moved_y), rock_id) {
                        anchor.1 - 1
                    } else {
                        anchor.1
                    }
                }
                _ => panic!("Invalid input"),
            };
            anchor = (anchor.0, moved_y);

            // test falling
            if anchor.0 - 1 < 0 {
                break;
            }
            let ok = check(&chamber, &(anchor.0 - 1, anchor.1), rock_id);
            if !ok {
                break;
            }
            anchor = (anchor.0 - 1, anchor.1);
        }

        match rock_id % 5 {
            0 => {
                insert_or_panic!(chamber, anchor);
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 1));
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 2));
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 3));
                height = max(height, anchor.0 + 1);
            }
            1 => {
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 1));
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1));
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1 + 1));
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1 + 2));
                insert_or_panic!(chamber, (anchor.0 + 2, anchor.1 + 1));
                height = max(height, anchor.0 + 3);
            }
            2 => {
                insert_or_panic!(chamber, anchor);
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 1));
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 2));
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1 + 2));
                insert_or_panic!(chamber, (anchor.0 + 2, anchor.1 + 2));
                height = max(height, anchor.0 + 3);
            }
            3 => {
                insert_or_panic!(chamber, anchor);
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1));
                insert_or_panic!(chamber, (anchor.0 + 2, anchor.1));
                insert_or_panic!(chamber, (anchor.0 + 3, anchor.1));
                height = max(height, anchor.0 + 4);
            }
            4 => {
                insert_or_panic!(chamber, anchor);
                insert_or_panic!(chamber, (anchor.0, anchor.1 + 1));
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1));
                insert_or_panic!(chamber, (anchor.0 + 1, anchor.1 + 1));
                height = max(height, anchor.0 + 2);
            }
            _ => unreachable!(),
        }
        // println!("{:?}", chamber);
    }
    println!("{}", height);
}
