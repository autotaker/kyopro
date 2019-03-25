# kyopro: My kyopro framework

# Requirements
Stack or cabal
# install

## with stack
```console
$ stack install 
```

## with cabal
```console
$ cabal v2-install exe:kyopro
```

Build may take more than 5 minutes. Please bee patient.

# How to use it

```console
$ cd ~/your/workspace/abc120
$ kyopro --generate # generates default render template `render.yaml`
$ kyopro abc120
$ editor A/main.cpp
```

# How to customize template

Template language is [Ginger](https://ginger.tobiasdammers.nl/guide/),
which is a variant of Jinja2 template.
See `render.yaml` for details.


