use std::collections::HashMap;

#[derive(Debug)]
enum Expression {
    Add(String, String),
    Mul(String, String),
    Minus(String, String),
    Div(String, String),
    Num(i64),
}

fn calc(map: &HashMap<String, Expression>, key: &str) -> i64 {
    match map.get(key).unwrap() {
        Expression::Add(left, right) => calc(map, left) + calc(map, right),
        Expression::Mul(left, right) => calc(map, left) * calc(map, right),
        Expression::Minus(left, right) => calc(map, left) - calc(map, right),
        Expression::Div(left, right) => calc(map, left) / calc(map, right),
        Expression::Num(num) => *num,
    }
}

fn main() {
    let mut map: HashMap<String, Expression> = HashMap::new();
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let splitted: Vec<_> = input.trim().split(": ").collect();
        assert!(splitted.len() == 2);
        let key = splitted[0].to_string();
        let value = splitted[1].to_string();

        let mut got = false;
        for i in [" + ", " - ", " * ", " / "] {
            if value.contains(i) {
                got = true;
                let splitted: Vec<_> = value.split(i).collect();
                assert!(splitted.len() == 2);
                let left = splitted[0].to_string();
                let right = splitted[1].to_string();
                let expr = match i {
                    " + " => Expression::Add(left, right),
                    " - " => Expression::Minus(left, right),
                    " * " => Expression::Mul(left, right),
                    " / " => Expression::Div(left, right),
                    _ => unreachable!(),
                };
                map.insert(key.clone(), expr);
                break;
            }
        }
        if !got {
            map.insert(key.clone(), Expression::Num(value.parse().unwrap()));
        }
    }

    println!("{}", calc(&map, "root"));
}
