# Advent of code 2022 solutions

## Running the code

There are two executables - `cli` and `tests`. Run the following command to trigger the test suit.

```bash
$ cabal run tests
```

Use the cli executable to run the solution by specifying the day using `-d` and the solution
number using `-s`. For example, the following command will run the first solution for the 2nd day.

```bash
$ cabal run cli -- -d 2 -s 1
```

It automatically selects the `input2.txt` file in the `inputs` folder. In case of a need to
run against a different input file, use the `-f`. The following command will run the 2nd
solution for the first day using an example input stored under `inputs/example1.txt`.

```bash
$ cabal run cli -- -d 1 -s 2 -f example1.txt
```

## Development

I use `ghc 8.10.7` with `cabal 3.6.2.0` because this combination works on the macOS and supports
the haskell language server (I use `hls 1.8.0.0`).

Current 3rd party dependencies are:

- [split](https://hackage.haskell.org/package/split) - for making (big surprise) splitting lists
- [containers](https://hackage.haskell.org/package/containers) - containes stuff like sets, trees, maps
- [lens](https://hackage.haskell.org/package/lens) - for manipulating data structures
