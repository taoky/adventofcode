enum Choice {
    Rock,
    Paper,
    Scissors,
}

fn outcome_score(x: Choice, y: Choice) -> i32 {
    // score of x
    match (x, y) {
        (Choice::Rock, Choice::Rock) => 3,
        (Choice::Rock, Choice::Paper) => 0,
        (Choice::Rock, Choice::Scissors) => 6,
        (Choice::Paper, Choice::Rock) => 6,
        (Choice::Paper, Choice::Paper) => 3,
        (Choice::Paper, Choice::Scissors) => 0,
        (Choice::Scissors, Choice::Rock) => 0,
        (Choice::Scissors, Choice::Paper) => 6,
        (Choice::Scissors, Choice::Scissors) => 3,
    }
}

fn main() {
    let mut score = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let input: Vec<char> = input.chars().collect();
        let opponent = input[0];
        let my = input[2];
        let opponent = match opponent {
            'A' => Choice::Rock,
            'B' => Choice::Paper,
            'C' => Choice::Scissors,
            _ => panic!("Invalid input"),
        };
        let my = match my {
            'X' => Choice::Rock,
            'Y' => Choice::Paper,
            'Z' => Choice::Scissors,
            _ => panic!("Invalid input"),
        };

        score += match my {
            Choice::Rock => 1,
            Choice::Paper => 2,
            Choice::Scissors => 3,
        };
        score += outcome_score(my, opponent);
    }
    println!("{}", score);
}