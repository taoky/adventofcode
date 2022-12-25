fn main() {
    let mut array = Vec::new();
    let mut current = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input.trim().is_empty() {
            array.push(current);
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
    array.sort_by(|a, b| b.cmp(a));
    // println!("{:?}", array);
    println!("{}", array[0] + array[1] + array[2]);
}
