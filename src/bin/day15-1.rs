use std::collections::HashSet;

macro_rules! commit {
    ($ans: expr, $start: expr, $end: expr, $exclusion: expr) => {
        // println!("{}..{}", $start, $end);
        $ans += $end - $start + 1;
        for i in $exclusion.iter() {
            // println!("exclusion {}", i);
            if i >= &$start && i <= &$end {
                $ans -= 1;
            }
        }
    };
}

fn main() {
    // get target y from argv
    let target_y = std::env::args()
        .nth(1)
        .expect("target_y should be in argv[1]")
        .parse::<i32>()
        .unwrap();
    let mut ranges = Vec::new();
    let mut exclusion = HashSet::new();
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let sp: Vec<_> = input.trim().split(": ").collect();
        assert!(sp.len() == 2);

        let sensor = sp[0]
            .strip_prefix("Sensor at ")
            .unwrap()
            .split(", ")
            .map(|x| x[2..].parse::<i32>().unwrap())
            .collect::<Vec<i32>>();
        assert!(sensor.len() == 2);
        let sensor = (sensor[0], sensor[1]);

        let beacon = sp[1]
            .strip_prefix("closest beacon is at ")
            .unwrap()
            .split(", ")
            .map(|x| x[2..].parse::<i32>().unwrap())
            .collect::<Vec<i32>>();
        assert!(beacon.len() == 2);
        let beacon = (beacon[0], beacon[1]);

        if beacon.1 == target_y {
            exclusion.insert(beacon.0);
        }

        let manhattan_dist = (sensor.0 - beacon.0).abs() + (sensor.1 - beacon.1).abs();

        // |? - sensor.0| + |target_y - sensor.1| = manhattan_dist
        let lval = manhattan_dist - (target_y - sensor.1).abs();
        if lval < 0 {
            // trap: target_y may be too far away
            continue;
        }
        let lval_v1 = lval + sensor.0;
        let lval_v2 = sensor.0 - lval;

        let range = if lval_v1 < lval_v2 {
            (lval_v1, lval_v2)
        } else {
            (lval_v2, lval_v1)
        };
        // println!("{:?}", range);
        ranges.push(range);
    }
    ranges.sort();
    let mut ans = 0;
    let mut start = ranges[0].0;
    let mut end = start;
    // println!("{:?}", ranges);
    for (idx, range) in ranges.iter().enumerate() {
        if range.0 != start && range.0 > end {
            // commit
            commit!(ans, start, end, &exclusion);
            start = range.0;
            // end = start;
        }
        end = std::cmp::max(end, range.1);
        if idx == ranges.len() - 1 {
            commit!(ans, start, end, &exclusion);
        }
    }
    println!("{}", ans);
}
