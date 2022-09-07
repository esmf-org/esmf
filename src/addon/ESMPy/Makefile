.PHONY: clean install test test_int test_all uninstall

clean:
		rm -rf build || :
		rm -rf dist || :
		find . -name "*.egg-info" -exec rm -rf {} \; || :
		find . -name "*.pyc" -exec rm -f {} \; || :
		find . -name "*ESMF_LogFile*" -exec rm -f {} \; || :
		find . -name "*.log" -exec rm -f {} \; || :
		find . -name "*.vtk" -exec rm -f {} \; || :
		find . -name "*.pytest_cache" -exec rm -rf {} \; || :
		find . -name "*.python-version" -exec rm -f {} \; || :
		find . -name "*__pycache__" -exec rm -rf {} \; || :
		find . -name ".report.json" -exec rm -rf {} \; || :
		find . -name ".test" -exec rm -rf {} \; || :
		rm -rf src/esmpy/test/regrid_from_file/data || :
		rm -rf examples/data || :

install:
		python3 -m pip install .

dryrun_examples:
		python3 -m pytest -vs examples/exampletestdryrun.py
	
dryrun_regrid_from_file:
		python3 -m pytest -vs src/esmpy/test/regrid_from_file/run_regrid_from_file_dryrun.py

dryrun: dryrun_examples dryrun_regrid_from_file

test:
		bash test_all.bash

test_unit:
		python3 -m pytest -vs

test_unit_parallel:
		mpiexec -n 4 python3 -vs

test_examples:
		python3 -m pytest -vs examples/exampletest.py

test_examples_parallel:
		mpiexec -n 4 python3 -m pytest -vs examples/exampletest.py

test_regrid_from_file:
		python3 -m pytest -vs src/esmpy/test/regrid_from_file/run_regrid_from_file.py

test_regrid_from_file_parallel:
		mpiexec -n 4 python3 -m pytest -vs src/esmpy/test/regrid_from_file/run_regrid_from_file.py

test_serial: test_unit test_examples test_regrid_from_file

test_parallel: test_unit_parallel test_examples_parallel test_regrid_from_file_parallel

test_all: test_unit test_unit_parallel test_examples test_examples_parallel test_regrid_from_file test_regrid_from_file_parallel

uninstall:
		python3 -m pip uninstall ESMPy
