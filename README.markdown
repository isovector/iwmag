# IWMAG: I Wanna Make a Game

## Dedication

I'm currently trapped in Lithuania without a lot on my plate. As a wise man once
said:

> "Clubbing hard, f***in' women; ain't much to do." - Drake



## Building

The project is in Haskell, using [Stack][dlstack] for dependency management.

[dlstack]: https://www.stackage.org/install

After you've installed Stack, this might run it for you. Maybe.

```bash
sudo apt-get install cabal-install
git clone https://github.com/isovector/iwmag.git
cd iwmag
cabal sandbox init
stack build
cabal run
```

