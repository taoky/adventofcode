# Performance comparison

This folder contains some Rust implementation with same algorithm as the Haskell ones, focusing on slow (> 1s) solutions (Why are they slow? Is it the fault of algorithm or Haskell code or something else?)

Use `cargo-play` to run them.

## Day12

```console
> rustc -O day12.rs
> time ./day12
4964259839627

________________________________________________________
Executed in  433.64 millis    fish           external
   usr time  397.84 millis    0.00 micros  397.84 millis
   sys time   34.08 millis  704.00 micros   33.37 millis
```

Haskell version takes 10s, due to slow `Data.MemoCombinators`.
