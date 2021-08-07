# probabilisp

Probabilistic reasoning is used in many domains, including financial stock prediction, intrusion detection for cybersecurity, and image recognition. Probabilistic programming is a paradigm in which a user specifies parameters for their desired probability models and inference is performed automatically. `probabilisp` is a simple Lisp-like domain specific probabilistic programming language.

## Getting Started

Navigate to the `project` directory.

```sh
# initialize project using package.yaml
make init  # or stack init

# build the probabilisp interpreter
make build  # or stack build

# build and run the probabilisp interpreter
make run  # or stack run

# run tests
make test  # or stack test

# start Haskell repl with Prob module loaded
stack repl ./src/Prob.hs
```
