#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Number(i32),
    List(Vec<String>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum State {
    Right,
    Notright,
    Continue,
}

pub fn parse(s: &str) -> Object {
    if !s.starts_with('[') {
        Object::Number(s.parse().unwrap())
    } else {
        let s = s.strip_prefix('[').unwrap().strip_suffix(']').unwrap();
        let mut list = Vec::new();
        let mut buffer = Vec::new();
        let mut in_string = 0;
        for i in s.chars() {
            match i {
                '[' => {
                    buffer.push(i);
                    in_string += 1;
                }
                ']' => {
                    buffer.push(i);
                    in_string -= 1;
                }
                ',' => {
                    if in_string == 0 {
                        list.push(buffer.iter().collect());
                        buffer.clear();
                    } else {
                        buffer.push(i);
                    }
                }
                _ => buffer.push(i),
            };
        }
        if !buffer.is_empty() {
            list.push(buffer.iter().collect());
        }
        Object::List(list)
    }
}

pub fn cmp(obj1: &Object, obj2: &Object) -> State {
    match (obj1, obj2) {
        (Object::Number(n1), Object::Number(n2)) => match n1.cmp(n2) {
            std::cmp::Ordering::Less => State::Right,
            std::cmp::Ordering::Greater => State::Notright,
            std::cmp::Ordering::Equal => State::Continue,
        },
        (Object::List(l1), Object::List(l2)) => {
            let mut i = 0;
            while i < l1.len() && i < l2.len() {
                let res = cmp(&parse(&l1[i]), &parse(&l2[i]));
                match res {
                    State::Right => return State::Right,
                    State::Notright => return State::Notright,
                    State::Continue => i += 1,
                }
            }
            match l1.len().cmp(&l2.len()) {
                std::cmp::Ordering::Less => State::Right,
                std::cmp::Ordering::Greater => State::Notright,
                std::cmp::Ordering::Equal => State::Continue,
            }
        }
        (Object::Number(n1), Object::List(_)) => {
            cmp(&Object::List([n1.to_string()].to_vec()), obj2)
        }
        (Object::List(_), Object::Number(n2)) => {
            cmp(obj1, &Object::List([n2.to_string()].to_vec()))
        }
    }
}
