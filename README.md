# Bayes

This is the Bayes benchmark, an adapted version of the Bayes benchmark of the STAMP benchmark suite [Minh2008] ported to Clojure and extended with the use of transactional futures [Swalens2016].

## How to run

You can run the program using [Leiningen](https://leiningen.org/).

Run the benchmark as follows (all parameters are optional):

    $ lein run -- -n 5 -r 512 -v 48 -x alternatives-parallel

To run the original version of the benchmark, which does not use transactional actors and is similar to the benchmark of the STAMP suite, execute:

    $ lein run -- -n 5 -r 512 -v 48

Parameters from original benchmark:
* `-t`: number of threads that process transactions.
* `-v`: number of variables.
* `-r`: number of records.
* `-e`: maximum number of edges learned per variable (-1 for no limit).
* `-i`: edge insert penalty.
* `-n`: maximum number of parents.
* `-p`: percent chance of parent.
* `-q`: operation quality factor.
* `-s`: seed for random number generator.

Additional parameters:
* `-m`: enable profiling.
* `-x`: comma-separated list of "variations" on the original program to enable. (Described below)

(Run `lein run -- -h` to get this description and more.)

Running the program prints the given options and the total execution time to the screen.

## Variations

Using command-line options, you can change the behavior of the benchmark. Without any option, it is similar to the original STAMP benchmark.

There are two variations that can be enabled:
* `alternatives-parallel`: in learner/find-best-insert-task, calculate the alternatives in parallel. This uses transactional futures.
* `strict-adtree`: in adtree/make-vary, generate the adtree strictly instead of lazily.

In our benchmarks, we compare the `alternatives-parallel` variation with the original, to measure the benefit of using transactional futures.

## License
Licensed under the MIT license, included in the file `LICENSE`.

## References

[Swalens2016]
J. Swalens, J. De Koster, and W. De Meuter. 2016. "Transactional Tasks: Parallelism in Software Transactions". In _Proceedings of the 30th European Conference on Object-Oriented Programming (ECOOP'16)_.

[Minh2008]
C. C. Minh, J. Chung, C. Kozyrakis, and K. Olukotun. 2008. "STAMP: Stanford Transactional Applications for Multi-Processing". In _Proceedings of the IEEE International Symposium on Workload Characterization (IISWC'08)_.
