enum Choice {
    Rock,
    Paper,
    Scissors,
}

enum Outcome {
    Win,
    Draw,
    Lose,
}

fn choice_score(x: Outcome, y: Choice) -> Choice {
    // score of x
    match (x, y) {
        (Outcome::Win, Choice::Rock) => Choice::Paper,
        (Outcome::Win, Choice::Paper) => Choice::Scissors,
        (Outcome::Win, Choice::Scissors) => Choice::Rock,
        (Outcome::Draw, Choice::Rock) => Choice::Rock,
        (Outcome::Draw, Choice::Paper) => Choice::Paper,
        (Outcome::Draw, Choice::Scissors) => Choice::Scissors,
        (Outcome::Lose, Choice::Rock) => Choice::Scissors,
        (Outcome::Lose, Choice::Paper) => Choice::Rock,
        (Outcome::Lose, Choice::Scissors) => Choice::Paper,
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
            'X' => Outcome::Lose,
            'Y' => Outcome::Draw,
            'Z' => Outcome::Win,
            _ => panic!("Invalid input"),
        };

        score += match my {
            Outcome::Lose => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        };
        score += match choice_score(my, opponent) {
            Choice::Rock => 1,
            Choice::Paper => 2,
            Choice::Scissors => 3,
        }
    }
    println!("{}", score);
}