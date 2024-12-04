# Advent of Code 2024 :santa: :christmas_tree:

## Quickstart

If you just want to run a day against the example or real input, save the input to a `.txt` file and pipe it to the main program.

```shell
$ janet d1/main.janet < path/to/input.txt
```

With a LISP-y language like Janet, the recommended approach is to connect your editor a REPL and interactively run the code. Here's how to start the REPL.

```shell
$ jpm -l janet -e "(import spork/netrepl) (netrepl/server)"
```

## Progress (4/25)

|     | Janet  | Solution Comment                                                               |
| --- | ------ | ------------------------------------------------------------------------------ |
| 1   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h3vp6n/comment/lzv4qlv) |
| 2   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h4ncyr/comment/m041ne0) |
| 3   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h5frsp/comment/m06empm) |
| 4   | :bell:  |                                                                                |
| 5   | :zzz:  |                                                                                |
| 6   | :zzz:  |                                                                                |
| 7   | :zzz:  |                                                                                |
| 8   | :zzz:  |                                                                                |
| 9   | :zzz:  |                                                                                |
| 10  | :zzz:  |                                                                                |
| 11  | :zzz:  |                                                                                |
| 12  | :zzz:  |                                                                                |
| 13  | :zzz:  |                                                                                |
| 14  | :zzz:  |                                                                                |
| 15  | :zzz:  |                                                                                |
| 16  | :zzz:  |                                                                                |
| 17  | :zzz:  |                                                                                |
| 18  | :zzz:  |                                                                                |
| 19  | :zzz:  |                                                                                |
| 20  | :zzz:  |                                                                                |
| 21  | :zzz:  |                                                                                |
| 22  | :zzz:  |                                                                                |
| 23  | :zzz:  |                                                                                |
| 24  | :zzz:  |                                                                                |
| 25  | :zzz:  |                                                                                |

## Make Reddit Code Snippet

For longer code snippets, use https://topaz.github.io/paste/. If it's short enough, do this:

```
$ cat code | sed 's/^/    /' | xsel -b
$ cat code | sed 's/^/    /' | pbcopy
```

## Reddit Comment Template

```text
[LANGUAGE: Janet]

26 lines with `wc -l`.

- [GitHub Repository](https://github.com/cideM/aoc2024-janet)
- [Topaz Paste]()
```

## Disable Copilot

Add `set exrc` to your Neovim configuration, then `echo 'let g:copilot_enabled=v:false' > .nvimrc`, open the file and `:trust` it.
