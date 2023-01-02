use adventofcode_2022::utils::day19::{dfs, DFSExternal, State};

fn main() {
    let mut idx = 1;
    let mut sum = 0;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        let splitted: Vec<_> = input.trim().split(' ').collect();

        let ore_robot_ore_cost = splitted[6].parse::<i32>().unwrap();
        let clay_robot_ore_cost = splitted[12].parse::<i32>().unwrap();
        let obsidian_robot_ore_cost = splitted[18].parse::<i32>().unwrap();
        let obsidian_robot_clay_cost = splitted[21].parse::<i32>().unwrap();
        let geode_robot_ore_cost = splitted[27].parse::<i32>().unwrap();
        let geode_robot_obsidian_cost = splitted[30].parse::<i32>().unwrap();

        let dfs_info = DFSExternal {
            ore_robot_ore_cost,
            clay_robot_ore_cost,
            obsidian_robot_ore_cost,
            obsidian_robot_clay_cost,
            geode_robot_ore_cost,
            geode_robot_obsidian_cost,
        };

        let (max_geode, max_state) = dfs(&State::new(24), &dfs_info);
        println!("Case #{}: {} {:?}", idx, max_geode, max_state);
        sum += max_geode * idx;

        idx += 1;
    }
    println!("{}", sum);
}
