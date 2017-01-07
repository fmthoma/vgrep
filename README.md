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

* With a `git` alias defined in your `~/.gitconfig`:

    ```bash
    git config --global alias.vgrep '!__git_vgrep () { git grep --color=always "$@" | vgrep; }; __git_vgrep'
    git vgrep data
    ```
* Using [`ack`][ack]/[`ag`][ag] instead of `grep`? No problem:

    ```bash
    ack data | vgrep           # Output of `ack` is compatible
    ack --color data | vgrep   # Even coloring works
    ag --color data | vgrep    # Same for `ag`
    ```
[ack]: http://beyondgrep.com/
[ag]:  https://github.com/ggreer/the_silver_searcher

## Installation

### Binaries

Debian/Ubuntu: `.deb` files are available for the [latest release][1].

```bash
wget https://github.com/fmthoma/vgrep/releases/download/v0.1.4.1/vgrep_0.1.4.1-1_amd64.deb
sudo dpkg -i vgrep_0.1.4.1-1_amd64.deb
```

### From [Hackage][2]

Installation from Hackage via [`stack`][3] is recommended:
```bash
stack update
stack install vgrep
```
This will install `vgrep` to your `~/.local/bin` directory.

### From [source][4]

```bash
git clone https://github.com/fmthoma/vgrep.git
cd vgrep
stack setup
stack install
```

### With [nix][5]

Generate the necessary files with
```bash
#!/usr/bin/env nix-shell
#! nix-shell -p cabal2nix -i bash
cabal2nix .         > default.nix
cabal2nix --shell . > shell.nix
```
and run or install vgrep via the know nix tools, e.g. `nix-shell` or `nix-build`.

[1]: https://github.com/fmthoma/vgrep/releases/latest
[2]: https://hackage.haskell.org/packages/vgrep
[3]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
[4]: https://github.com/fmthoma/vgrep
[5]: https://nixos.org/nix/
