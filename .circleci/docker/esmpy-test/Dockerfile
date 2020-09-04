ARG ESMF_BRANCH=""

FROM bekozi/esmf:${ESMF_BRANCH}

RUN echo "ESMF_BRANCH=${ESMF_BRANCH}"

ENV ESMF_ARTIFACTS=/artifacts
RUN mkdir -p ${ESMF_ARTIFACTS}

WORKDIR ${HOME}/sandbox/esmf
RUN git clone --branch ${ESMF_BRANCH} --depth 1 https://github.com/esmf-org/esmf.git src-git

ENV ESMF_DIR=${HOME}/sandbox/esmf/src-git
ENV ESMPY_DIR=${ESMF_DIR}/src/addon/ESMPy

RUN python setup.py test

# tdk: Store artifacts
RUN cp nosetests.xml ${ESMF_ARTIFACTS}
