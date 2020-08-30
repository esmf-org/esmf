ARG DOCKER_NAMESPACE="esmf"
ARG ESMF_BRANCH="develop"

FROM ${DOCKER_NAMESPACE}/esmf:${ESMF_BRANCH}

RUN git clone --depth 1 https://github.com/esmf-org/nuopc-app-prototypes.git
WORKDIR nuopc-app-prototypes
RUN git checkout ${ESMF_BRANCH} || echo "WARNING: ESMF_BRANCH does not exist. Checking out develop" && git checkout develop
RUN ln -s /usr/bin/make /usr/bin/gmake || echo "INFO: gmake symlink already exists"

ENV ESMF_ARTIFACTS=/opt/artifacts
RUN mkdir -p ${ESMF_ARTIFACTS}
RUN bash testProtos.sh 2>&1 | tee ${ESMF_ARTIFACTS}/testProtos.out

RUN pip3 install --user unittest-xml-reporting
ADD ./meta_test.py /opt/meta_test.py
RUN python /opt/meta_test.py -v 2>&1 | tee ${ESMF_ARTIFACTS}/meta_test.out
RUN cd ${ESMF_ARTIFACTS}/.. && zip -rv artifacts.zip artifacts
