use std::collections::HashSet;

fn manhattan_dist(sensor: (i32, i32), dist: (i32, i32)) -> i32 {
    (sensor.0 - dist.0).abs() + (sensor.1 - dist.1).abs()
}

#[derive(Debug)]
struct Segment {
    // ax + b = y, which x in [l, r]
    a: i32, // a = 1 or -1
    b: i32,
    l: i32,
    r: i32,
}

impl Segment {
    fn new(coord: (i32, i32), a: i32, l: i32, r: i32) -> Self {
        let b = match a {
            1 => coord.1 - coord.0,
            -1 => coord.1 + coord.0,
            _ => unreachable!(),
        };
        Segment { a, b, l, r }
    }

    fn intersection(&self, other: &Segment) -> Option<(i32, i32)> {
        if self.a == other.a {
            return None;
        }
        let x = (other.b - self.b) / (self.a - other.a);
        let y = self.a * x + self.b;
        if x >= self.l && x <= self.r && x >= other.l && x <= other.r {
            Some((x, y))
        } else {
            None
        }
    }

    // fn is_on(&self, coord: (i32, i32)) -> bool {
    //     coord.1 == self.a * coord.0 + self.b && coord.0 >= self.l && coord.0 <= self.r
    // }
}

fn main() {
    // get target y from argv
    let max_coor = std::env::args()
        .nth(1)
        .expect("max_coor should be in argv[1]")
        .parse::<i32>()
        .unwrap();
    let mut sensors = Vec::new();
    let mut segments = Vec::new();
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

        let dist = manhattan_dist(sensor, beacon);
        sensors.push((sensor, dist));

        let coord1 = (sensor.0, sensor.1 - dist - 1);
        let coord2 = (sensor.0, sensor.1 + dist + 1);

        segments.push(Segment::new(coord1, -1, sensor.0 - dist - 1, sensor.0));
        segments.push(Segment::new(coord1, 1, sensor.0, sensor.0 + dist + 1));
        segments.push(Segment::new(coord2, -1, sensor.0, sensor.0 + dist + 1));
        segments.push(Segment::new(coord2, 1, sensor.0 - dist - 1, sensor.0));
    }

    fn is_not_covered(sensors: &[((i32, i32), i32)], coord: (i32, i32)) -> bool {
        for sensor in sensors.iter() {
            if manhattan_dist(sensor.0, coord) <= sensor.1 {
                return false;
            }
        }
        true
    }

    let mut answers = HashSet::new();
    for i in 0..segments.len() {
        for j in i + 1..segments.len() {
            // println!("{:?} {:?}", segments[i], segments[j]);
            if let Some(coord) = segments[i].intersection(&segments[j]) {
                if coord.0 < 0 || coord.0 >= max_coor || coord.1 < 0 || coord.1 >= max_coor {
                    continue;
                }
                if !answers.insert(coord) {
                    continue;
                }
                if is_not_covered(&sensors, coord) {
                    println!("{:?}", coord);
                    println!("{}", coord.0 as u64 * 4000000 + coord.1 as u64);
                }
            }
        }
    }
    // corner points
    for coord in [
        (1, 1),
        (1, max_coor - 1),
        (max_coor - 1, 1),
        (max_coor - 1, max_coor - 1),
    ] {
        if !answers.insert(coord) && is_not_covered(&sensors, coord) {
            println!("{:?}", coord);
            println!("{}", coord.0 as u64 * 4000000 + coord.1 as u64);
        }
    }
}
