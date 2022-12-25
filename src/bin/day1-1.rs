fn main() {
    let mut maximum = 0;
    let mut current = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input.trim().is_empty() {
            if current > maximum {
                maximum = current;
            }
            current = 0;
            if input_size == 0 {
                break;
            } else {
                continue;
            }
        }
        let input = input.trim().parse::<i32>().unwrap();
        current += input;
    }
    println!("{}", maximum);
}