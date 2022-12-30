# adventofcode

My solutions for [Advent Of Code 2022](https://adventofcode.com/2022/) challenges.

- Rust
- No third-patry crates for challenge solutions
- All solutions takes reasonable time and memory usage (WIP) [performance record](./performance.txt)
- A program ([src/main.rs](src/main.rs)) printing the time and peak memory usage (with [cgroups](src/resource/cgroup.rs)(7) or [getrusage](src/resource/getrusage.rs)(2), or [polling /proc/pid/statm](src/resource/poll.rs)) of every compiled solution program on Linux.

## Notes

<details><summary>Spoilers of solutions</summary>

- Day 1 - 10 are very easy and can be completed even without knowledge of algorithm.
  - I would like to recommend arena-based allocation for implementing data structures like trees or lists in Rust, as you don't need to worry about lifetime, etc. if doing so.
- Day 11 Part II: LCM (Least Common Multiple)
- Day 12: BFS
- Day 13: Interesting as you need to write a simple parser and comparer.
- Day 15: Be careful of result overflowing. And when calculating range on a fixed y with Manhattan distance, don't count when y is too far away.
- Day 16: Eliminate intermediate state by using Floyd-Warshall algorithm to get shortest path distances between start point and all active valves.
- Day 17 Part II: It's impossible to calculate directly even when O(n) (as n = 1000000000000). You need to find a repeating pattern when simulating. A `std::collection::HashSet` is a good choice for storing the state.
- Day 18: Floodfilling.
- Day 19: It may help to eliminate intermediate state by searching "what robot should be made next" instead of "what should I do next minute". And the most important optimization is: Don't make a X-cracking robot when there are enough X-cracking robots to make other robots that need X (You need one minute to make a robot out).
- Day 20: A linked list may help. Consider the case when rounds >> array length (not covered by example).
- Day 21 Part II: The best solution may be using a symbolic solver (Z3? SymPy? or even Mathematica?). However, it seems that one side of the expression does not have "input", and "input" is only involved once on the other side.
- Day 22 Part II: The hardest part of AoC 2022. I cannot figure out a universal solution and can only hard-coded specifically for the example and my input. Drawing a diagram like [assets/day22.svg](assets/day22.svg) may help (I use Inkscape).
- Day 23: Even when an elf does nothing, their "proposal direction list" still changes at the end of every round.
- Day 24: It's a good idea to "cache" whether a coordinate is under blizzard for every round required.
- Day 25: Very easy, and don't need to worry about overflowing.

</details>