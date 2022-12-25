fn main() {
    let mut stack_strings = Vec::new();
    let stack_cnt: usize;
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if !input.contains('[') {
            let stacks_num: Vec<_> = input.trim().split(' ').collect();
            stack_cnt = stacks_num[stacks_num.len() - 1].parse().unwrap();
            break;
        }
        stack_strings.push(input);
    }
    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); stack_cnt];
    for i in stack_strings.iter().rev() {
        for j in 0..stack_cnt {
            let slice = &i[j * 4..j * 4 + 3];
            if slice.starts_with('[') {
                stacks[j].push(slice.chars().nth(1).unwrap());
            }
        }
    }

    // simulation
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        if input.trim().is_empty() {
            continue;
        }
        let splitted: Vec<_> = input.trim().split(' ').collect();
        let move_cnt = splitted[1].parse().unwrap();
        let move_from: usize = splitted[3].parse().unwrap();
        let move_to: usize = splitted[5].parse().unwrap();

        for _ in 0..move_cnt {
            let item = stacks[move_from - 1].pop().unwrap();
            stacks[move_to - 1].push(item);
        }
    }

    for i in stacks {
        print!("{}", i[i.len() - 1]);
    }
    println!()
}
