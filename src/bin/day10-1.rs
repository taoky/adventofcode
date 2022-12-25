fn main() {
    let mut cycle = 1;
    let mut x = 1;

    let mut res = 0;
    let cmp_list = [20, 60, 100, 140, 180, 220];
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let splitted: Vec<_> = input.trim().split(' ').collect();
        let inst = splitted[0];
        match inst {
            "noop" => {
                // nop
                if cmp_list.contains(&cycle) {
                    res += cycle * x;
                }
                cycle += 1;
            }
            "addx" => {
                let operand = splitted[1];
                if cmp_list.contains(&cycle) {
                    res += cycle * x;
                }
                cycle += 1;
                if cmp_list.contains(&cycle) {
                    res += cycle * x;
                }
                cycle += 1;
                x += operand.parse::<i32>().unwrap();
            }
            _ => panic!("Invalid inst"),
        }
    }

    println!("{}", res);
}
