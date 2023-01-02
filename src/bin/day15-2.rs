fn main() {
    // get target y from argv
    let max_coor = std::env::args()
        .nth(1)
        .expect("max_coor should be in argv[1]")
        .parse::<i32>()
        .unwrap();
    let mut map: Vec<Vec<(i32, i32)>> = vec![Vec::new(); (max_coor + 1) as usize];
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

        // if beacon.1 == target_y {
        //     exclusion.push(beacon.0);
        // }

        let manhattan_dist = (sensor.0 - beacon.0).abs() + (sensor.1 - beacon.1).abs();

        // |? - sensor.0| + |target_y - sensor.1| = manhattan_dist
        let get_range_at_y = |y: i32| -> Option<(i32, i32)> {
            let lval = manhattan_dist - (y - sensor.1).abs();
            if lval < 0 {
                // trap: target_y may be too far away
                return None;
            }
            let lval_v1 = lval + sensor.0;
            let lval_v2 = sensor.0 - lval;

            Some(if lval_v1 < lval_v2 {
                (lval_v1, lval_v2)
            } else {
                (lval_v2, lval_v1)
            })
        };

        for y in sensor.1 - manhattan_dist..=sensor.1 + manhattan_dist {
            if y < 0 || y > max_coor {
                continue;
            }
            let range = get_range_at_y(y).unwrap();
            map[y as usize].push(range);
        }

        // println!("{:?}", map);
    }

    let mut not_covered = Vec::new();
    for (y, xranges) in map.iter_mut().enumerate() {
        xranges.sort();
        let lp = xranges[0].0;
        let mut rp = xranges[0].1;
        if lp > 0 {
            // println!("y {} not covered: 0..{}", y, lp);
            for i in 0..lp {
                not_covered.push((i, y));
            }
        }
        for xrange in xranges {
            if xrange.0 > rp {
                // println!("y {} not covered: {}..{}", y, rp + 1, xrange.0)
                for i in rp + 1..xrange.0 {
                    not_covered.push((i, y));
                }
            }
            if xrange.1 > rp {
                rp = xrange.1
            }
        }
        if rp < max_coor {
            // println!("y {} not covered: {}..={}", y, rp + 1, max_coor)
            for i in rp + 1..=max_coor {
                not_covered.push((i, y));
            }
        }
    }
    println!("{:?}", not_covered);

    assert!(not_covered.len() == 1);
    let coord = not_covered[0];
    // trap: overflow
    println!("{}", coord.0 as u64 * 4000000 + coord.1 as u64);
}
