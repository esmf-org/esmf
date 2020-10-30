FROM bekozi/esmpy-doc-base

ARG ESMF_BRANCH=""
RUN echo "ESMF_BRANCH=$ESMF_BRANCH"

ENV ARTIFACTS=/artifacts/doc-esmpy
ENV ESMF_DIR=/esmf

RUN rm -rf ${ESMF_DIR}
RUN mkdir -p ${ARTIFACTS}

WORKDIR /
RUN git clone --branch ${ESMF_BRANCH} --depth 1 https://github.com/esmf-org/esmf.git esmf

WORKDIR ${ESMF_DIR}/src/addon/ESMPy
RUN python setup.py build
WORKDIR doc
RUN make html latexpdf 2>&1 | tee /artifacts/doc-esmpy/esmpy-make-doc.out

RUN cp -r esmpy_doc ${ARTIFACTS} && \
    cd /artifacts && \
    zip -r doc-esmpy.zip doc-esmpy
