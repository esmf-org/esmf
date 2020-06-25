FROM bekozi/esmf-doc-anvil

ARG ESMF_BRANCH=""
RUN echo "ESMF_BRANCH=$ESMF_BRANCH"

ENV ESMF_ARTIFACTS=/artifacts
RUN mkdir -p ${ESMF_ARTIFACTS}

WORKDIR ${HOME}/sandbox/esmf
RUN git clone --branch ${ESMF_BRANCH} --depth 1 https://github.com/esmf-org/esmf.git src-git

ENV ESMF_DIR=${HOME}/sandbox/esmf/src-git

WORKDIR ${ESMF_DIR}/src/addon/NUOPC/doc
RUN make localdoc 2>&1 | tee ${ESMF_ARTIFACTS}/nuopc-make-doc.out

WORKDIR ${ESMF_DIR}
RUN make doc 2>&1 | tee ${ESMF_ARTIFACTS}/esmf-make-doc.out

RUN zip -r ${ESMF_ARTIFACTS}/doc.zip doc src/addon/NUOPC/doc/*.pdf src/addon/NUOPC/doc/NUOPC_refdoc src/addon/NUOPC/doc/NUOPC_howtodoc
RUN zip ${ESMF_ARTIFACTS}/doc.out.zip ${ESMF_ARTIFACTS}/*.out
