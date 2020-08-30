ARG DOCKER_NAMESPACE="esmf"
ARG ESMF_BRANCH="develop"

FROM ${DOCKER_NAMESPACE}/esmf:${ESMF_BRANCH}

RUN git clone --depth 1 https://github.com/esmf-org/nuopc-app-prototypes.git
WORKDIR nuopc-app-prototypes
RUN git checkout ${ESMF_BRANCH} || echo "WARNING: ESMF_BRANCH does not exist. Checking out develop" && git checkout develop
RUN ln -s /usr/bin/make /usr/bin/gmake || echo "INFO: gmake symlink already exists"

ENV ESMF_ARTIFACTS=/artifacts/nuopc-app-prototypes
RUN mkdir -p ${ESMF_ARTIFACTS}
RUN bash testProtos.sh 2>&1 | tee ${ESMF_ARTIFACTS}/testProtos.out

#COPY ./docker-entrypoint.sh /
#ENTRYPOINT ["/docker-entrypoint.sh"]
