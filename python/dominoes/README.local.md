# Local (i.e. personal) notes

Additional files for testing were added, as follows. This assumes we have these
packages:

* pytest
* pytest-benchmark
* hypothesis

## `dominoes_prop_test.py`

Playing with hypothesis to do property testing. Settings can be changed via the
`@settings` decorator (see
https://hypothesis.readthedocs.io/en/latest/settings.html#available-settings).

## `dominoes_benchmark_test.py`

Depending on the hardware, might want to tweak settings in `pytest.ini`'s
benchmark-related extra options.

By default benchmarking is skipped, to run it:

    pytest --benchmark-only
    
To save the results (for comparison):

    pytest --benchmark-only --benchmark-save=NAME
    
which will save stats to `.benchmarks/<architecture>/<counter>_<NAME>.json`

To compare, run e.g.:
   
    pytest --benchmark-only --benchmark-compare=0001
    
the count must be precise string, seems it doesn't work with the name... a bit silly.

See https://pytest-benchmark.readthedocs.io/en/latest/usage.html for more.
