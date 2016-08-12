# haskell-language-conf
**`language-conf`** `.conf` (e.g. nginx configuration) parsers and
pretty-printers for the Haskell programming language.

- `Data.Conf` exports the `.conf` parser
- `Data.Conf.PrettyPrint` exports the `.conf` pretty-printer
- `ConfFmt` is a `.conf` file formatter that serves as an example; it's built as
  `conffmt` by the cabal configuration

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
