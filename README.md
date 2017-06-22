# Liquid-QSA
This is a small "project" done for my master thesis to simulate and better understand quantum search algorithms using LIQUID.

Requirement:
- LIQUID library
- F#
- Change the path of the libraries in the files

Implemented are:
* Grover Search  https://arxiv.org/abs/quant-ph/9605043
* Yuri Ozhigov (only for two functions) http://www.complex-systems.com/abstracts/v11_i06_a03.html

Some examples of gates which can be printed can be seen in the example folder. If you want to use parts of it as library or execute code, have a look at the	SearchAlgorithmsSamples.fsx file, to see how it is done.

Execution on Linux with the fsharpi library is done by
```
fsharpi SearchAlgorithmsSamples.fsx
```
or for interactive mode
```
fsharpi --use:SearchAlgorithmsSamples.fsx
```

There is no intention to create a fully functional library, because I do not think there is a wide interest for such a library. I uploaded the code for people who maybe want to see some code implemented in LIQUID and want to understand quantum computing better using LIQUID, since there is not so much code online. If there is any interest in usage or explanation of the code needed, please let me know.
