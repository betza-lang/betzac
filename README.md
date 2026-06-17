# betzac

## Requirements

- stack
- GHC
- dot (optional)

## Build

To build and install (i.e. copy the binaries to `~/.local/bin`):

```bash
stack install
```

## Usage

To run on `FILE`:
```bash
betzac FILE
```

For help:
```bash
```

## Visualize AST
You can use `dot` from `graphviz` to visualize the AST.

```bash
betzac FILE --dot - | dot -Txlib &
```
