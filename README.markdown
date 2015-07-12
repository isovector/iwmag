# IWMAG: I Wanna Make a Game

## Dedication

Currently trapped in Lithuania without a lot on my plate. As a wise man said:

> "Clubbing hard, f***in' women; ain't much to do." -Drake



## Building

The project is in Haskell, using [stack][dlstack] for dependency management.

[dlstack]: https://www.stackage.org/install

This might run it for you. Maybe.

```bash
sudo apt-get install cabal-install
git clone https://github.com/isovector/iwmag.git
cabal sandbox init
stack build
cabal run
```
