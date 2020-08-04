FROM esmpy-doc-base

ARG ESMF_BRANCH=""
RUN echo "ESMF_BRANCH=$ESMF_BRANCH"

ENV ESMF_ARTIFACTS=/artifacts
RUN mkdir -p ${ESMF_ARTIFACTS}

ENV ESMF_DIR=/esmf

RUN rm -rf ${ESMF_DIR}

WORKDIR /
RUN git clone --branch ${ESMF_BRANCH} --depth 1 https://github.com/esmf-org/esmf.git esmf

WORKDIR ${ESMF_DIR}
RUN python setup.py build
WORKDIR doc
RUN make html latexpdf 2>&1 | tee ${ESMF_ARTIFACTS}/esmpy-make-doc.out

#RUN cp -rv doc ${ESMF_ARTIFACTS}/doc-esmf
#RUN mkdir ${ESMF_ARTIFACTS}/doc-nuopc
#RUN cp -rv src/addon/NUOPC/doc/*.pdf src/addon/NUOPC/doc/NUOPC_refdoc src/addon/NUOPC/doc/NUOPC_howtodoc ${ESMF_ARTIFACTS}/doc-nuopc

#RUN zip -r ${ESMF_ARTIFACTS}/doc-artifacts.zip ${ESMF_ARTIFACTS}
