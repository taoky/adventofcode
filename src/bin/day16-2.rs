use std::collections::{HashMap, HashSet, VecDeque};

struct Valve {
    // name: String,
    rate: i32,
    to_valves: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct State {
    pos: usize,
    remaining_time: i32,
    score: i32,
    opened_valves: Vec<usize>,
}

impl State {
    fn new(start: usize) -> Self {
        Self {
            pos: start,
            remaining_time: 26,
            score: 0,
            opened_valves: Vec::new(),
        }
    }
}

fn main() {
    let mut valves: Vec<Valve> = Vec::new();
    let mut namemap: HashMap<String, usize> = HashMap::new();

    let mut openable_valve_cnt = 0;
    let mut start_idx = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let splitted: Vec<_> = input.trim().split(' ').collect();
        let valve_name = splitted[1].to_owned();
        let rate: i32 = splitted[4]
            .strip_prefix("rate=")
            .unwrap()
            .strip_suffix(';')
            .unwrap()
            .parse()
            .unwrap();
        let to_valves: Vec<_> = splitted[9..]
            .to_vec()
            .iter()
            .map(|x| x.trim_end_matches(',').to_owned())
            .collect();
        valves.push(Valve { rate, to_valves });
        namemap.insert(valve_name.clone(), valves.len() - 1);
        if rate != 0 {
            openable_valve_cnt += 1;
        }
        if valve_name == "AA" {
            start_idx = valves.len() - 1;
        }
    }

    // Floyd algo
    let mut dist: Vec<Vec<u32>> = vec![Vec::new(); valves.len()];
    for (i, item) in dist.iter_mut().enumerate().take(valves.len()) {
        *item = vec![std::u32::MAX; valves.len()];
        item[i] = 0;
    }
    for i in 0..valves.len() {
        for to in &valves[i].to_valves {
            let to_idx = namemap[to];
            dist[i][to_idx] = 1;
        }
    }
    for k in 0..valves.len() {
        for i in 0..valves.len() {
            for j in 0..valves.len() {
                if dist[i][k] == std::u32::MAX || dist[k][j] == std::u32::MAX {
                    continue;
                }
                if dist[i][j] > dist[i][k] + dist[k][j] {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }

    let mut active_valves: Vec<usize> = Vec::new();
    for (idx, i) in valves.iter().enumerate() {
        if i.rate != 0 || idx == start_idx {
            active_valves.push(idx);
        }
    }

    // BFS
    let mut queue: VecDeque<State> = VecDeque::new();
    queue.push_back(State::new(start_idx));
    let mut hs = HashSet::new();
    // (score, remaining time)
    // let mut max_scores = vec![(0, 0, Vec::new()); valves.len()];
    let mut results: HashMap<Vec<usize>, i32> = HashMap::new();

    while !queue.is_empty() {
        let head = queue.pop_front().unwrap();
        if head.remaining_time == 0 || head.opened_valves.len() == openable_valve_cnt {
            results
                .entry({
                    let mut x = head.opened_valves.clone();
                    x.sort();
                    x
                })
                .and_modify(|x| {
                    if *x < head.score {
                        *x = head.score;
                    }
                })
                .or_insert(head.score);
            continue;
        }
        if !(valves[head.pos].rate == 0 || head.opened_valves.contains(&head.pos)) {
            // this valve can be opened
            let mut new_state = head.clone();
            new_state.remaining_time -= 1;
            new_state.score += valves[head.pos].rate * new_state.remaining_time;

            new_state.opened_valves.push(new_state.pos);
            // println!("Opening: {:?}", new_state);
            queue.push_back(new_state);
        }
        for to_idx in active_valves.iter() {
            if head.opened_valves.contains(to_idx) || *to_idx == start_idx {
                continue;
            }
            let mut new_state = head.clone();
            new_state.pos = *to_idx;
            new_state.remaining_time -= dist[head.pos][*to_idx] as i32;
            if new_state.remaining_time < 0 {
                continue;
            }

            if hs.contains(&new_state) {
                continue;
            } else {
                hs.insert(new_state.clone());
            }
            // println!("Changing valve: {:?}", new_state);
            queue.push_back(new_state);
        }
    }

    let mut results: Vec<_> = results.iter().collect();
    results.sort_by(|a, b| b.1.cmp(a.1));

    let mut maximum = 0;
    for i in 0..results.len() {
        let x = results[i].1;
        let x_valves = results[i].0;
        for item in results.iter().skip(i + 1) {
            let y_valves = item.0;
            let mut success = true;
            for valve in y_valves {
                if x_valves.contains(valve) {
                    success = false;
                    break;
                }
            }
            if !success {
                continue;
            }
            let y = item.1;
            if x + y > maximum {
                maximum = x + y;
            }
            break;
        }
    }
    println!("{}", maximum);
}
