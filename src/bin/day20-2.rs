#[derive(Debug, Default)]
struct List {
    arena: Vec<Node>,
    // head: Option<usize>,
    // tail: Option<usize>
}

#[derive(Debug)]
struct Node {
    val: i64,
    next: usize,
    prev: usize,
}

impl Node {
    fn new(val: i64) -> Self {
        Self {
            val,
            next: 0,
            prev: 0,
        }
    }
}

fn integrity_check(seq: &List) {
    let len = seq.arena.len();
    let mut idx = 0;
    let mut next = seq.arena[idx].next;
    let mut cnt = 0;
    while next != 0 {
        cnt += 1;
        let prev = idx;
        idx = next;
        next = seq.arena[idx].next;
        assert_eq!(seq.arena[idx].prev, prev);
    }
    assert_eq!(cnt, len - 1);

    idx = 0;
    cnt = 0;
    let mut prev = seq.arena[idx].prev;
    while prev != 0 {
        cnt += 1;
        let next = idx;
        idx = prev;
        prev = seq.arena[idx].prev;
        assert_eq!(seq.arena[idx].next, next);
    }
    assert_eq!(cnt, len - 1);
}

// fn print_list(seq: &List) {
//     let mut idx = 0;
//     let mut next = seq.arena[idx].next;
//     print!("{} ", seq.arena[idx].val);
//     while next != 0 {
//         idx = next;
//         next = seq.arena[idx].next;
//         print!("{} ", seq.arena[idx].val);
//     }
//     println!();
// }

fn main() {
    let mut seq: List = List::default();
    let mut zero_idx = None;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let val: i64 = input.trim().parse::<i64>().unwrap() * 811589153;
        seq.arena.push(Node::new(val));
        if val == 0 {
            zero_idx = Some(seq.arena.len() - 1);
        }
    }
    let zero_idx = zero_idx.unwrap();
    for i in 0..seq.arena.len() {
        if i == 0 {
            // seq.head = Some(i);
            seq.arena[i].prev = seq.arena.len() - 1;
        } else {
            seq.arena[i - 1].next = i;
            seq.arena[i].prev = i - 1;
        }
        if i == seq.arena.len() - 1 {
            seq.arena[i].next = 0;
        }
    }
    // println!("{:?}", seq);
    // integrity_check(&seq);
    for _ in 0..10 {
        for i in 0..seq.arena.len() {
            let val = seq.arena[i].val;
            let pre_idx = seq.arena[i].prev;
            let next_idx = seq.arena[i].next;

            // print!("({} ", val);
            if val == 0 {
                continue;
            }

            let mut target_idx = i;
            if val > 0 {
                let steps = val % (seq.arena.len() - 1) as i64;
                for _ in 0..steps {
                    target_idx = seq.arena[target_idx].next;
                    if target_idx == i {
                        // trap: skip myself
                        target_idx = seq.arena[target_idx].next;
                    }
                }
            } else {
                let steps = -val % (seq.arena.len() - 1) as i64;
                for _ in 0..steps {
                    target_idx = seq.arena[target_idx].prev;
                    if target_idx == i {
                        // trap: skip myself
                        target_idx = seq.arena[target_idx].prev;
                    }
                }
                target_idx = seq.arena[target_idx].prev;
            }
            // let pre_target_idx = seq.arena[target_idx].prev;
            let next_target_idx = seq.arena[target_idx].next;
            if next_target_idx == i || target_idx == i {
                continue;
            }

            // insert after target_idx
            seq.arena[pre_idx].next = next_idx;
            seq.arena[next_idx].prev = pre_idx;

            seq.arena[i].prev = target_idx;
            seq.arena[i].next = next_target_idx;

            seq.arena[target_idx].next = i;
            seq.arena[next_target_idx].prev = i;

            // print_list(&seq);
            integrity_check(&seq);
        }
    }

    // print_list(&seq);

    let mut sum = 0;
    let mut idx = zero_idx;

    for i in 1..=3000 {
        idx = seq.arena[idx].next;
        if i % 1000 == 0 {
            println!("{}: {}", i, seq.arena[idx].val);
            sum += seq.arena[idx].val;
        }
    }
    println!("{}", sum);
}
