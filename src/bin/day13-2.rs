use adventofcode_2022::utils::day13::{cmp, parse, Object, State};

fn main() {
    let mut array = Vec::new();
    array.push(parse("[[2]]"));
    array.push(parse("[[6]]"));
    loop {
        let mut input = String::new();

        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input.trim().is_empty() {
            if input_size == 0 {
                break;
            }
            continue;
        }

        array.push(parse(input.trim()));
    }
    array.sort_by(|a, b| match cmp(a, b) {
        State::Right => std::cmp::Ordering::Less,
        State::Notright => std::cmp::Ordering::Greater,
        State::Continue => panic!("Invalid res"),
    });

    let mut decode = 1;
    for (idx, i) in array.iter().enumerate() {
        if *i == Object::List(["[2]".to_owned()].to_vec())
            || *i == Object::List(["[6]".to_owned()].to_vec())
        {
            decode *= idx + 1;
        }
    }
    println!("{}", decode);
}
