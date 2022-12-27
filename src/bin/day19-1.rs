use std::{
    cmp::max,
    collections::{HashSet, VecDeque},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct State {
    ore_robot: i32,
    clay_robot: i32,
    obsidian_robot: i32,
    geode_robot: i32,
    ore: i32,
    clay: i32,
    obsidian: i32,
    geode: i32,
    remaining_time: i32,
}

impl Default for State {
    fn default() -> Self {
        State {
            ore_robot: 1,
            clay_robot: 0,
            obsidian_robot: 0,
            geode_robot: 0,
            ore: 0,
            clay: 0,
            obsidian: 0,
            geode: 0,
            remaining_time: 24,
        }
    }
}

// calculate the time of gathering resource
fn cal_minute(resource: i32, resource_robot: i32, resource_requirements: i32) -> i32 {
    if resource >= resource_requirements {
        0
    } else {
        if resource_robot == 0 {
            return i16::MAX as i32;
        }
        (resource_requirements - resource + resource_robot - 1) / resource_robot
    }
}

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

        let mut queue: VecDeque<State> = VecDeque::new();
        let mut hs = HashSet::new();
        queue.push_back(State::default());

        let mut max_geode = 0;
        let mut max_state = State::default();
        while !queue.is_empty() {
            let state = queue.pop_front().unwrap();
            let mut continued = false;

            // println!("{:?}", state);

            for i in 0..4 {
                match i {
                    0 => {
                        // geode
                        let minutes = max(
                            cal_minute(state.ore, state.ore_robot, geode_robot_ore_cost),
                            cal_minute(
                                state.obsidian,
                                state.obsidian_robot,
                                geode_robot_obsidian_cost,
                            ),
                        ) + 1;
                        if minutes >= state.remaining_time {
                            continue;
                        }
                        let mut new_state = state;
                        new_state.ore =
                            new_state.ore + minutes * new_state.ore_robot - geode_robot_ore_cost;
                        new_state.obsidian = new_state.obsidian
                            + minutes * new_state.obsidian_robot
                            - geode_robot_obsidian_cost;

                        new_state.clay += minutes * new_state.clay_robot;
                        new_state.geode += minutes * new_state.geode_robot;
                        new_state.geode_robot += 1;
                        new_state.remaining_time -= minutes;
                        if hs.contains(&new_state) {
                            continue;
                        }
                        queue.push_back(new_state);
                        hs.insert(new_state);
                        continued = true;
                    }
                    1 => {
                        // obsidian
                        if state.obsidian_robot >= geode_robot_obsidian_cost {
                            continue;
                        }
                        let minutes = max(
                            cal_minute(state.ore, state.ore_robot, obsidian_robot_ore_cost),
                            cal_minute(state.clay, state.clay_robot, obsidian_robot_clay_cost),
                        ) + 1;
                        if minutes >= state.remaining_time {
                            continue;
                        }
                        let mut new_state = state;

                        new_state.ore =
                            new_state.ore + minutes * new_state.ore_robot - obsidian_robot_ore_cost;
                        new_state.clay = new_state.clay + minutes * new_state.clay_robot
                            - obsidian_robot_clay_cost;
                        new_state.obsidian += minutes * new_state.obsidian_robot;
                        new_state.geode += minutes * new_state.geode_robot;
                        new_state.obsidian_robot += 1;
                        new_state.remaining_time -= minutes;
                        if hs.contains(&new_state) {
                            continue;
                        }
                        queue.push_back(new_state);
                        hs.insert(new_state);
                        continued = true;
                    }
                    2 => {
                        // clay
                        // don't build clay bots if we got enough clay
                        if state.clay_robot >= obsidian_robot_clay_cost {
                            continue;
                        }
                        let minutes =
                            cal_minute(state.ore, state.ore_robot, clay_robot_ore_cost) + 1;
                        if minutes >= state.remaining_time {
                            continue;
                        }
                        let mut new_state = state;
                        new_state.ore =
                            new_state.ore + minutes * new_state.ore_robot - clay_robot_ore_cost;
                        new_state.clay += minutes * new_state.clay_robot;
                        new_state.obsidian += minutes * new_state.obsidian_robot;
                        new_state.geode += minutes * new_state.geode_robot;
                        new_state.clay_robot += 1;
                        new_state.remaining_time -= minutes;
                        if hs.contains(&new_state) {
                            continue;
                        }
                        queue.push_back(new_state);
                        hs.insert(new_state);
                        continued = true;
                    }
                    3 => {
                        // ore
                        if state.ore_robot
                            >= max(
                                clay_robot_ore_cost,
                                max(obsidian_robot_ore_cost, geode_robot_ore_cost),
                            )
                        {
                            continue;
                        }
                        let minutes =
                            cal_minute(state.ore, state.ore_robot, ore_robot_ore_cost) + 1;
                        if minutes >= state.remaining_time {
                            continue;
                        }
                        let mut new_state = state;
                        new_state.ore =
                            new_state.ore + minutes * new_state.ore_robot - ore_robot_ore_cost;
                        new_state.clay += minutes * new_state.clay_robot;
                        new_state.obsidian += minutes * new_state.obsidian_robot;
                        new_state.geode += minutes * new_state.geode_robot;
                        new_state.ore_robot += 1;
                        new_state.remaining_time -= minutes;
                        if hs.contains(&new_state) {
                            continue;
                        }
                        queue.push_back(new_state);
                        hs.insert(new_state);
                        continued = true;
                    }
                    _ => unreachable!(),
                }
            }

            if !continued {
                let geode = state.geode + state.geode_robot * state.remaining_time;
                if geode > max_geode {
                    max_geode = geode;
                    max_state = state;
                }
            }
        }
        println!("Case #{}: {} {:?}", idx, max_geode, max_state);
        sum += max_geode * idx;

        idx += 1;
    }
    println!("{}", sum);
}
