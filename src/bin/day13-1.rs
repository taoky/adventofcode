use adventofcode_2022::utils::day13::{cmp, parse, State};

fn main() {
    let mut cnt = 0;
    let mut idx = 0;
    loop {
        let mut str1 = String::new();
        let mut str2 = String::new();

        std::io::stdin().read_line(&mut str1).unwrap();
        std::io::stdin().read_line(&mut str2).unwrap();

        idx += 1;

        let obj1 = parse(str1.trim());
        let obj2 = parse(str2.trim());

        let res = cmp(&obj1, &obj2);

        assert!(res != State::Continue);

        if res == State::Right {
            cnt += idx;
        }

        let mut empty = String::new();
        let input_size = std::io::stdin().read_line(&mut empty).unwrap();
        if input_size == 0 {
            break;
        }
    }
    println!("{}", cnt);
}
