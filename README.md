# Liquid-QSA
This is a small "project" done for my master thesis to simulate and better understand quantum search algorithms using LIQUID.

### Requirements:
- LIQUID library
- F#
- Change the path of the libraries in the files

### Implemented search algorithms:
* Grover search  https://arxiv.org/abs/quant-ph/9605043
* Ozhigov search (only for two functions) http://www.complex-systems.com/abstracts/v11_i06_a03.html

Some examples of gates which can be printed can be seen in the example folder. If you want to use parts of it as library or execute code, have a look at the	SearchAlgorithmsSamples.fsx file, to see how it is done.

Execution on Linux with the fsharpi library is done by
```
fsharpi SearchAlgorithmsSamples.fsx
```
or for interactive mode
```
fsharpi --use:SearchAlgorithmsSamples.fsx
```

### Circuit diagram of Ozhigov's search
![](https://github.com/agoscinski/Liquid-QSA/blob/master/docs/PG_2-parallel_thesis.svg "PG2 circuit diagram")
