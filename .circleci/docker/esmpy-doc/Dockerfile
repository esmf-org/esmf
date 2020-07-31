FROM bekozi/esmf

ARG ESMF_BRANCH=""
RUN echo "ESMF_BRANCH=$ESMF_BRANCH"

ENV ESMF_ARTIFACTS=/artifacts
RUN mkdir -p ${ESMF_ARTIFACTS}

ENV ESMF_DIR=${HOME}/sandbox/esmf/src-git

RUN rm -rf ${ESMF_DIR}

WORKDIR ${HOME}/sandbox/esmf
RUN git clone --branch ${ESMF_BRANCH} --depth 1 https://github.com/esmf-org/esmf.git src-git

RUN pip3 install sphinx sphinxcontrib-packages sphinxcontrib-bibtex
RUN apt-get --yes update && apt-get --yes install latexmk texlive-fonts-recommended texlive-latex-recommended texlive-latex-extra

WORKDIR ${ESMF_DIR}/src/addon/ESMPy
RUN python setup.py build
WORKDIR doc
RUN make html latexpdf 2>&1 | tee ${ESMF_ARTIFACTS}/esmpy-make-doc.out

cd
git clone git@github.com:esmf-org/esmf-org.github.io.git
cp -rf ${ESMF_DIR}/src/addon/ESMPy/doc/esmpy_doc/*  ~/esmf-org.github.io/dev_docs/esmpy/
cd ~/esmf-org.github.io/
git pull
git add .
git commit -m " pushing esmpy doc build by CircleCI on `date` "
git push origin master

#RUN cp -rv doc ${ESMF_ARTIFACTS}/doc-esmf
#RUN mkdir ${ESMF_ARTIFACTS}/doc-nuopc
#RUN cp -rv src/addon/NUOPC/doc/*.pdf src/addon/NUOPC/doc/NUOPC_refdoc src/addon/NUOPC/doc/NUOPC_howtodoc ${ESMF_ARTIFACTS}/doc-nuopc

#RUN zip -r ${ESMF_ARTIFACTS}/doc-artifacts.zip ${ESMF_ARTIFACTS}
