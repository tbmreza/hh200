# fmt

This folder will be the future home for a Ropes data structure implementation in Lean.
This folder will be the future home for a pretty-printer generated using BNFC.

> [!NOTE]
> Contributions are better invested in the main compiler than in here.

## hhs-fmt

This directory also contains `hhs-fmt`, a C++26 implementation of the Rope data structure.

### Building

To build the project, ensure you have a C++26 compliant compiler (like GCC 13+ or Clang 18+) and CMake installed.

```bash
mkdir build && cd build
cmake ..
make
```

### Testing

Run the unit tests using:

```bash
ctest
```

### Debian Packaging

To create a Debian package using CPack:

```bash
mkdir build && cd build
cmake ..
cpack -G DEB
```
