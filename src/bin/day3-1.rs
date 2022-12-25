fn score(ch: char) -> i32 {
    let ascii = ch as i32;
    if ch.is_lowercase() {
        ascii - 'a' as i32 + 1
    } else {
        ascii - 'A' as i32 + 27
    }
}

fn main() {
    let mut sum = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let first_part = &input[..input_size / 2];
        let second_part = &input[input_size / 2..input_size - 1];

        let first_hashset = first_part.chars().collect::<std::collections::HashSet<_>>();
        let second_hashset = second_part
            .chars()
            .collect::<std::collections::HashSet<_>>();
        let combined: Vec<_> = first_hashset.intersection(&second_hashset).collect();

        assert!(combined.len() == 1);
        let combined = combined[0];

        sum += score(*combined);
    }
    println!("{}", sum);
}
