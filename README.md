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

### Binaries

`.deb` files are available for the [latest release](https://github.com/fmthoma/vgrep/releases/latest).

### From [Hackage](https://hackage.haskell.org/packages/vgrep)

Installation from Hackage via [`stack`][1] is recommended:
```bash
stack update
stack install vgrep
```
This will install `vgrep` to your `~/.local/bin` directory.

### From source

```bash
git clone https://github.com/fmthoma/vgrep.git
cd vgrep
stack setup
stack install
```

[1]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
