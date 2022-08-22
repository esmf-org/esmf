.PHONY: clean install test test_int test_all uninstall

clean:
		rm -rf build
		rm -rf dist
		rm -rf *.egg-info
    # find . -name "*.pyc" -exec rm -f {} \;')
    # find . -name "*ESMF_LogFile*" -exec rm -f {} \;')
    # find . -name "*.log" -exec rm -f {} \;')
    # find . -name "*.vtk" -exec rm -f {} \;')
    # rm src/ESMF/interface/esmfmkfile.py')
    # rm -rf build')
    # rm -rf dist')
    # rm -rf src/ESMF/test/regrid_from_file/data')
    # rm -rf examples/data')


install:
		pip install .

test_unit:
		pytest -m="not parallel"

test_unit_parallel:
		mpirun -n 4 python -m pytest -m="not serial"

test_examples_dryrun:
		pytest -vs examples/exampletestdryrun.py
	
test_examples:
		pytest -m="not parallel" examples/exampletest.py

test_examples_parallel:
		mpirun -n 4 python -m pytest -m="not serial" examples/exampletest.py

test_regrid_from_file_dryrun:
		pytest -vs src/ESMF/test/regrid_from_file/rfftestdryrun.py

test_regrid_from_file:
		pytest -vs src/ESMF/test/regrid_from_file/rfftest.py

test_regrid_from_file_parallel:
		pytest -vs src/ESMF/test/regrid_from_file/rfftestdryrun.py
		mpirun -n 4 python -m pytest -vs src/ESMF/test/regrid_from_file/rfftest.py

test_all: test_unit test_parallel test_examples_dryrun test_examples test_examples_parallel

test: test_unit test_unit_parallel test_examples test_examples_parallel

test_uni: test_unit test_examples

uninstall:
		pip uninstall ESMPy
