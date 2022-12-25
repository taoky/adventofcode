fn main() {
    let mut cnt = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let pairs: Vec<_> = input.trim().split(',').collect();
        assert!(pairs.len() == 2);
        let p1: Vec<_> = pairs[0].split('-').collect();
        let p2: Vec<_> = pairs[1].split('-').collect();

        let p1_start: i32 = p1[0].parse().unwrap();
        let p1_end: i32 = p1[1].parse().unwrap();
        let p2_start: i32 = p2[0].parse().unwrap();
        let p2_end: i32 = p2[1].parse().unwrap();

        if (p1_start <= p2_start && p1_end >= p2_end) || (p2_start <= p1_start && p2_end >= p1_end)
        {
            cnt += 1;
        }
    }
    println!("{}", cnt);
}
