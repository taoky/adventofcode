use std::collections::HashMap;

#[derive(Debug)]
enum Expression {
    Add(String, String),
    Mul(String, String),
    Minus(String, String),
    Div(String, String),
    Num(i64),
    Unknown,
}

fn calc(map: &HashMap<String, Expression>, key: &str) -> Option<i64> {
    match map.get(key).unwrap() {
        Expression::Add(left, right) => Some(calc(map, left)? + calc(map, right)?),
        Expression::Mul(left, right) => Some(calc(map, left)? * calc(map, right)?),
        Expression::Minus(left, right) => Some(calc(map, left)? - calc(map, right)?),
        Expression::Div(left, right) => Some(calc(map, left)? / calc(map, right)?),
        Expression::Num(num) => Some(*num),
        Expression::Unknown => None,
    }
}

// can be optimized
fn rev_calc(map: &HashMap<String, Expression>, key: &str, value: i64) -> Option<i64> {
    match map.get(key).unwrap() {
        Expression::Add(left, right) => {
            let lval = calc(map, left);
            match lval {
                Some(lval) => {
                    let rval = value - lval;
                    rev_calc(map, right, rval)
                }
                None => {
                    let rval = calc(map, right).unwrap();
                    let lval = value - rval;
                    rev_calc(map, left, lval)
                }
            }
        }
        Expression::Minus(left, right) => {
            let lval = calc(map, left);
            match lval {
                Some(lval) => {
                    let rval = lval - value;
                    rev_calc(map, right, rval)
                }
                None => {
                    let rval = calc(map, right).unwrap();
                    let lval = rval + value;
                    rev_calc(map, left, lval)
                }
            }
        }
        Expression::Mul(left, right) => {
            let lval = calc(map, left);
            match lval {
                Some(lval) => {
                    let rval = value / lval;
                    rev_calc(map, right, rval)
                }
                None => {
                    let rval = calc(map, right).unwrap();
                    let lval = value / rval;
                    rev_calc(map, left, lval)
                }
            }
        }
        Expression::Div(left, right) => {
            let lval = calc(map, left);
            match lval {
                Some(lval) => {
                    let rval = lval / value;
                    rev_calc(map, right, rval)
                }
                None => {
                    let rval = calc(map, right).unwrap();
                    let lval = rval * value;
                    rev_calc(map, left, lval)
                }
            }
        }
        Expression::Num(num) => {
            assert_eq!(*num, value);
            Some(*num)
        }
        Expression::Unknown => {
            println!("{}", value);
            None
        }
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

        if key == "humn" {
            map.insert(key.clone(), Expression::Unknown);
            continue;
        }

        let mut got = false;
        for i in [" + ", " - ", " * ", " / "] {
            if value.contains(i) {
                got = true;
                let splitted: Vec<_> = value.split(i).collect();
                assert!(splitted.len() == 2);
                let left = splitted[0].to_string();
                let right = splitted[1].to_string();
                let expr = if key == "root" {
                    Expression::Add(left, right)
                } else {
                    match i {
                        " + " => Expression::Add(left, right),
                        " - " => Expression::Minus(left, right),
                        " * " => Expression::Mul(left, right),
                        " / " => Expression::Div(left, right),
                        _ => unreachable!(),
                    }
                };
                map.insert(key.clone(), expr);
                break;
            }
        }
        if !got {
            map.insert(key.clone(), Expression::Num(value.parse().unwrap()));
        }
    }

    let eq = match &map["root"] {
        Expression::Add(left, right) => {
            let lval = calc(&map, &left);
            match lval {
                Some(lval) => (lval, right),
                None => (calc(&map, right).unwrap(), left),
            }
        }
        _ => unreachable!(),
    };
    rev_calc(&map, eq.1, eq.0);
}
