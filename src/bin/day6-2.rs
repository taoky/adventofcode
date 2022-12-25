fn main() {
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let mut idx = 0;
        loop {
            let slice = &input[idx..idx + 14];
            let hset = slice.chars().collect::<std::collections::HashSet<_>>();
            if hset.len() == 14 {
                break;
            }
            idx += 1;
        }
        println!("{}", idx + 14);
    }
}
