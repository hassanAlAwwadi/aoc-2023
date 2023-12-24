# Advent of Code for Haskell
My code for AOC-2023. Template copied from {go back and find link}

## CLI Usage

Here's how to use the CLI for common tasks:

- Create a new solution for a specific day:

  ```bash
  ./aoc-hs new -d <day>
  ```

- Run a solution for a specific day and part:
  
  ```bash
  ./aoc-hs run -d <day> -p <part> --input
  ```

Below there is the complete description of the CLI (you can run `./aoc-hs --help` to get it in you local machine):

```bash
Usage: aoc-hs [new -d <day> [--no-curl] | run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]]

Description:
  This tool simplifies Advent of Code solutions in Haskell by creating templates and handling input files. No need to learn Cabal!

Subcommand: new

Create a new Advent of Code solution for the specified day. It creates a main module, modifies the .cabal file, and downloads the input data.

Usage: aoc-hs new -d <day>
Example: aoc-hs new -d 3
         aoc-hs new -d 3 --no-curl
Options:
  -d <day>       Specify the day for the Advent of Code puzzle (1-25).
  --no-curl      It wont download your personal AoC input file. You don't have you provide a cookie with this option

Subcommand: run

Run an Advent of Code solution for the specified day and part. The input data is read from a file which can be supplied via -f or you can 
use shortcuts --example and --input. Default --input

Usage: aoc-hs run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]
Example: aoc-hs run -d 3 -p 2 --example
         aoc-hs run -d 3 -p 3 -e
         aoc-hs run -d 3 -p 2 --input
         aoc-hs run -d 3 -p 2 -i
         aoc-hs run -d 3 -p 2 -f my-input-file.txt
Options:
  -d <day>       Specify the day for the Advent of Code puzzle (1-25).
  -p <part>      Specify the part of the puzzle (1 or 2).
  -f <file-name> Specify a custom input file to use.
  --example, -e  Use the example input file (./inputs/day-<day>.example) as input.
  --input, -i    (Default) Use the puzzle input file (./inputs/day-<day>.input) as input.
```
