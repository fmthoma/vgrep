Changelog
=========

## v0.2 (unreleased)

* Added support for a config file:
  A YAML file located at `~/.vgrep/config.yaml` is recognized as configuration
  file for colors and other settings. The default config file can be produced
  using `vgrep --dump-default-config > ~/.vgrep/config.yaml`.


## v0.1.4.1

* Switch to strict `Text`
* Less dependent on `template-haskell`


## v0.1.4

* User events (like key events) now have priority over other events, the UI does
  not block any more.
* `--help` and `--version` now produce sensible output.


## v0.1.3

* Fix pageUp in Results view


## v0.1.2

* Performance improvements
* Tests for Pager and Results widget
* Haddock documentation


## v0.1.1

* Fixed `j`/`k` keys in pager view
* Additional `h`/`l`/`←`/`→` keybindings for horizontal scrolling in pager
* Matching lines are now highlighted in pager view


## v0.1
