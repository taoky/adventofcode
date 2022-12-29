fn parse_snafu(s: &str) -> i64 {
    let mut res = 0;
    let mut base = 1;
    for c in s.chars().rev() {
        res += match c {
            '=' => -2,
            '-' => -1,
            '0' => 0,
            '1' => 1,
            '2' => 2,
            _ => unreachable!(),
        } * base;
        base *= 5;
    }
    res
}

fn to_snafu(x: i64) -> String {
    let mut res = Vec::new();
    let mut x = x;
    while x != 0 {
        res.push(match x % 5 {
            3 => {
                x += 2;
                '='
            }
            4 => {
                x += 1;
                '-'
            }
            0 => '0',
            1 => '1',
            2 => '2',
            _ => unreachable!(),
        });
        x /= 5;
    }
    res.reverse();
    res.iter().collect()
}

fn main() {
    let mut sum = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        sum += parse_snafu(input.trim());
    }
    println!("{}", to_snafu(sum));
}
