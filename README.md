`vgrep` -- A pager for `grep`
=============================

![Screenshot](./vgrep.png)

## Usage

* As a pager:
```bash
grep -rn data /some/path | vgrep  # -n for line numbers
```

* As a drop-in replacement for `grep`:
```bash
vgrep data /some/path                  # recursive by default
vgrep data /some/path | vgrep default  # works with pipes, too
```

## Installation
Installation via [`stack`][1] is recommended:
```bash
git clone https://github.com/fmthoma/vgrep.git
cd vgrep
stack setup
stack install
```
This will install `vgrep` to your `~/.local/bin` directory.

## Known limitations
* **No streaming**: Currently, `vgrep` will wait for input until it either has
collected one page of grep output to display, or `stdin` is exhausted.
For grepping large files or directory structures with few results,
`vgrep` may block for a while until displaying results at all.
(See [#18](https://github.com/fmthoma/vgrep/issues/18))

[1]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
