# haskell-language-conf
[![Hackage](https://img.shields.io/hackage/v/language-conf.svg?maxAge=2592000)](https://hackage.haskell.org/package/language-conf)
- - -
**`language-conf`** contains `.conf` (e.g. nginx configuration) parsers and
pretty-printers for the Haskell programming language.

- `Data.Conf` exports the `.conf` parser
- `Data.Conf.PrettyPrint` exports the `.conf` pretty-printer
- `ConfFmt` is a `.conf` file formatter that serves as an example; it's built as
  `conffmt` by the cabal configuration

For `hcl` parsers, see
[**haskell-language-hcl**](https://github.com/beijaflor-io/haskell-language-hcl)

## Build & Test
```
make build
```
```
make test
```
```
make all
```

## Build `conffmt`
```
make conffmt
```

## License
Published under the MIT license
