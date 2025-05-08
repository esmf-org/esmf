#!/bin/bash

set -e

MPIEXEC=${1}

VERSION=$(python3 -c "import esmpy; print (esmpy.__version__)")

echo "Testing ESMPy ${VERSION}"

# If we're using mpiuni, we only run with NP=1; otherwise we run with NP=1,4,6
#
# The logic here relies on the fact that, when using mpiuni, MPIEXEC will look like
# /path/to/mpiuni/mpirun - so we check if the final part of the path is "mpiuni/mpirun"
#
# Note that, if MPIEXEC is simply "mpirun", MPIEXEC_DIR will be "."; this will be wrong,
# but is okay for the sake of the logic here.
MPIEXEC_DIR=$(dirname "$MPIEXEC")
MPIEXEC_END_OF_PATH=$(basename "$MPIEXEC_DIR")/$(basename "$MPIEXEC")
if [ "$MPIEXEC_END_OF_PATH" = "mpiuni/mpirun" ]
then
    PE_COUNTS=(1)
else
    PE_COUNTS=(1 4 6)
fi

for NP in ${PE_COUNTS[@]}
do
    REPORT="esmpy${VERSION}-petx${NP}.test"
    COMMAND="${MPIEXEC} -np ${NP} python3 -m pytest -vs --json-report --json-report-summary > $REPORT 2>&1"
    echo ${COMMAND}
    eval "${COMMAND}"
    find . -name "*.ESMF_LogFile" -exec cat {} >> ${REPORT} \;
    cat .report.json >> ${REPORT}
done

echo "Report is in ${REPORT}"
