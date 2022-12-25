#[derive(Default, Debug, Clone)]
struct Monkey {
    items: Vec<i32>,
    operation: Operation,
    test: Test,
}

#[derive(Default, Debug, Clone)]
struct Operation {
    operator: Operator,
    operand: Option<i32>,
}

#[derive(Default, Debug, Clone)]
enum Operator {
    #[default]
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Default, Debug, Clone)]
struct Test {
    divisible: i32,
    true_throw: usize,
    false_throw: usize,
}

fn main() {
    let mut monkeys = Vec::new();
    let mut monkey: Monkey = Monkey::default();
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input.trim().is_empty() {
            monkeys.push(monkey.clone());
            if input_size == 0 {
                break;
            }
            continue;
        }

        if input.contains("Starting") {
            let input = input.trim().strip_prefix("Starting items: ").unwrap();
            monkey.items = input.split(", ").map(|x| x.parse().unwrap()).collect();
        } else if input.contains("Operation") {
            let input = input.trim().strip_prefix("Operation: new = old ").unwrap();
            let splitted: Vec<_> = input.split(' ').collect();
            let operator = match splitted[0] {
                "+" => Operator::Plus,
                "-" => Operator::Minus,
                "*" => Operator::Multiply,
                "/" => Operator::Divide,
                _ => panic!("Invalid operator"),
            };
            let operand = if splitted[1] == "old" {
                None
            } else {
                Some(splitted[1].parse().unwrap())
            };
            monkey.operation = Operation { operator, operand };
        } else if input.contains("Test") {
            let input = input
                .trim()
                .strip_prefix("Test: divisible by ")
                .unwrap()
                .parse()
                .unwrap();
            monkey.test = Test {
                divisible: input,
                true_throw: usize::MAX,
                false_throw: usize::MAX,
            };
        } else if input.contains("If true") {
            let input = input
                .trim()
                .strip_prefix("If true: throw to monkey ")
                .unwrap()
                .parse()
                .unwrap();
            monkey.test.true_throw = input;
        } else if input.contains("If false") {
            let input = input
                .trim()
                .strip_prefix("If false: throw to monkey ")
                .unwrap()
                .parse()
                .unwrap();
            monkey.test.false_throw = input;
        } else if input.contains("Monkey") {
            // ignore
        } else {
            panic!("Invalid input")
        }
    }

    let mut inspection_cnts = vec![0; monkeys.len()];

    for _ in 0..20 {
        for monkey_idx in 0..monkeys.len() {
            let monkey = &monkeys[monkey_idx].clone();
            for item in monkey.items.iter() {
                let new_item = match monkey.operation.operator {
                    Operator::Plus => item + monkey.operation.operand.unwrap_or(*item),
                    Operator::Minus => item - monkey.operation.operand.unwrap_or(*item),
                    Operator::Multiply => item * monkey.operation.operand.unwrap_or(*item),
                    Operator::Divide => item / monkey.operation.operand.unwrap_or(*item),
                } / 3;
                inspection_cnts[monkey_idx] += 1;
                if new_item % monkey.test.divisible == 0 {
                    monkeys[monkey.test.true_throw].items.push(new_item);
                } else {
                    monkeys[monkey.test.false_throw].items.push(new_item);
                }
            }
            monkeys[monkey_idx].items.clear();
        }
    }

    inspection_cnts.sort_by(|x, y| y.cmp(x));
    println!("{}", inspection_cnts[0] * inspection_cnts[1]);
}
