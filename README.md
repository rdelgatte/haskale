# Hask-Ale :beer:

## Pre-requisites

### Stack

Install `Haskell Tool Stack`, see [Haskell Tool Stack Installation](https://docs.haskellstack.org/en/stable/README/)

### LZMA Library

You may need to install manually the `lzma` library used by the `Swagger` library:

- Fedora: `sudo dnf install ghc-lzma-devel`
- macOs: `brew install xz`

### [macOs] Libz

For macOs users, you have to re-install `libz` properly following these steps: 

```
rm -f /usr/local/lib/libz*
brew install lzlib
```

### Clone this repo
```bash
git clone git@github.com:dktunited/hask-ale.git
```

## Startup

### Run the tests

```
stack test
```

You can add the `--file-watch` option to reload automatically the tests after each modification.

### Run the application

```
stack run
```

You should see the following message:

```
listening on port 3000
```

You're all set for the workshop!

Don't forget to let us know by email!
