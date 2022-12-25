fn draw(x: i32, cycle: i32) {
    if x <= cycle % 40 && cycle % 40 < x + 3 {
        print!("#");
    } else {
        print!(".");
    }
    if cycle % 40 == 0 {
        println!()
    }
}

fn main() {
    let mut cycle = 1;
    let mut x = 1;

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
                draw(x, cycle);
                cycle += 1;
            }
            "addx" => {
                let operand = splitted[1];
                draw(x, cycle);
                cycle += 1;
                draw(x, cycle);
                cycle += 1;
                x += operand.parse::<i32>().unwrap();
            }
            _ => panic!("Invalid inst"),
        }
    }
}
