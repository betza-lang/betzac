# betzac

## Requirements

- stack
- GHC
- dot or xdot (optional)

## Build

To build and install (i.e. copy the binaries to `~/.local/bin`):

```bash
stack install
```

If the installation was successful, you should be able to run this command:

```bash
betzac --version
```

## Usage

To run on `FILE`:
```bash
betzac FILE
```

For help:
```bash
betzac --help
```

## Visualize AST
You can use `dot` from `graphviz` to visualize the AST.

```bash
betzac FILE --dot - | dot -Txlib
# or
betzac FILE --dot - | xdot -
```
