`vgrep` -- A pager for `grep`
=============================

![Screenshot](./screenshot.gif)

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

Keybindings:

* Use `hjkl` or the arrow keys to navigate
* `Enter` opens a pager with the selected file
* `e` opens the selected file in `$EDITOR`
* `Tab` switches between results list and pager
* `q` closes the pager and then the entire application.

## Installation

### Binaries

Debian/Ubuntu: `.deb` files are available for the [latest release][1].

```bash
wget https://github.com/fmthoma/vgrep/releases/download/v0.2/vgrep_0.2.0.0-1_amd64.deb
sudo dpkg -i vgrep_0.2.0.0-1_amd64.deb
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

[1]: https://github.com/fmthoma/vgrep/releases/latest
[2]: https://hackage.haskell.org/package/vgrep
[3]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
[4]: https://github.com/fmthoma/vgrep
