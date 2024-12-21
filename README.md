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

## Progress (19/25)

|     | Janet  | Solution Comment                                                               |
| --- | ------ | ------------------------------------------------------------------------------ |
| 1   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h3vp6n/comment/lzv4qlv) |
| 2   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h4ncyr/comment/m041ne0) |
| 3   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h5frsp/comment/m06empm) |
| 4   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h689qf/comment/m0ctka8) |
| 5   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h71eyz/comment/m0p2ml5) |
| 6   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h7tovg/comment/m0rpud7) |
| 7   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h8l3z5/comment/m0uswaj) |
| 8   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1h9bdmp/comment/m135mk9) |
| 9   | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1ha27bo/comment/m19d3xc) |
| 10  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hau6hl/comment/m1cgexv) |
| 11  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hbm0al/comment/m1i5n8s) |
| 12  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hcdnk0/comment/m1xbgkd) |
| 13  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hd4wda/comment/m1xvl16) |
| 14  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hdvhvu/comment/m22rs18) |
| 15  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hele8m/comment/m2nky3u) |
| 16  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hfboft/comment/m2sx157) |
| 17  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hg38ah/comment/m37jllp) |
| 18  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hguacy/comment/m2w99rc) |
| 19  | :bell: | [Link](https://www.reddit.com/r/adventofcode/comments/1hhlb8g/comment/m37j5me) |
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
