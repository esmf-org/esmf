#!/bin/bash

set -e

MPIEXEC=${1}

VERSION=$(python3 -c "import esmpy; print (esmpy.__version__)")

echo "Testing ESMPy ${VERSION}"

for NP in 1 4 6
do
    REPORT="esmpy${VERSION}-petx${NP}.test"
    COMMAND="${MPIEXEC} -np ${NP} python3 -m pytest -vs --json-report --json-report-summary > $REPORT 2>&1"
    echo ${COMMAND}
    eval "${COMMAND}"
    find . -name "*.ESMF_LogFile" -exec cat {} >> ${REPORT} \;
    cat .report.json >> ${REPORT}
done

echo "Report is in ${REPORT}"
