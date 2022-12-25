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
    let mut hashsets = Vec::new();
    let mut idx = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }

        let hashset = input
            .trim()
            .chars()
            .collect::<std::collections::HashSet<_>>();
        hashsets.push(hashset);

        if idx % 3 == 2 {
            let intersection = hashsets
                .iter()
                .skip(1)
                .fold(hashsets[0].clone(), |acc, hs| {
                    acc.intersection(hs).cloned().collect()
                });
            hashsets.clear();

            assert!(intersection.len() == 1);
            let combined = intersection.iter().next().unwrap();
            sum += score(*combined);
        }
        idx += 1;
    }
    println!("{}", sum);
}
