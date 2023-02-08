.PHONY: clean install test test_int test_all uninstall

-include $(ESMFMKFILE)

ifndef ESMF_INTERNAL_MPIRUN
		ESMF_INTERNAL_MPIRUN=mpiexec
endif


clean:
		rm -rf build || :
		find . -name "*.egg-info" -exec rm -rf {} \; || :
		find . -name "*.pyc" -exec rm -f {} \; || :
		find . -name "*.log" -exec rm -f {} \; || :
		find . -name "*.vtk" -exec rm -f {} \; || :
		find . -name "*.pytest_cache" -exec rm -rf {} \; || :
		find . -name "*.python-version" -exec rm -f {} \; || :
		find . -name "*__pycache__" -exec rm -rf {} \; || :
		find . -name "*ESMF_LogFile*" -exec rm -f {} \; || :
		find . -name "*.report.json" -exec rm -rf {} \; || :
		find . -name "*.test" -exec rm -rf {} \; || :
dust:
		find . -name "*ESMF_LogFile*" -exec rm -f {} \; || :
		find . -name "*.report.json" -exec rm -rf {} \; || :
		find . -name "*.test" -exec rm -rf {} \; || :

install:
		python3 -m pip install .

download_examples:
		python3 examples/test_examples_download.py
	
download_unittest:
		python3 src/esmpy/test/test_api/test_unit_download.py
	
download_regrid_from_file:
		python3 src/esmpy/test/regrid_from_file/test_regrid_from_file_download.py

download: download_unittest download_examples download_regrid_from_file

test: dust
		bash src/esmpy/test/test_all.bash ${ESMF_INTERNAL_MPIRUN}

test_unit:
		python3 -m pytest -vs

test_unit_parallel:
		$(ESMF_INTERNAL_MPIRUN) -np 4 python3 -vs

test_examples:
		python3 -m pytest -vs examples/test_examples.py

test_examples_parallel:
		$(ESMF_INTERNAL_MPIRUN) -np 4 python3 -m pytest -vs examples/test_examples.py

test_regrid_from_file:
		python3 -m pytest -vs src/esmpy/test/regrid_from_file/test_regrid_from_file.py

test_regrid_from_file_parallel:
		$(ESMF_INTERNAL_MPIRUN) -np 4 python3 -m pytest -vs src/esmpy/test/regrid_from_file/test_regrid_from_file.py

test_serial: test_unit test_examples test_regrid_from_file

test_parallel: test_unit_parallel test_examples_parallel test_regrid_from_file_parallel

test_all: test_unit test_unit_parallel test_examples test_examples_parallel test_regrid_from_file test_regrid_from_file_parallel

uninstall:
		python3 -m pip uninstall esmpy
