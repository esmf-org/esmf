FROM bekozi/esmf-ubuntu

ARG ESMF_BRANCH=""
RUN echo "ESMF_BRANCH=$ESMF_BRANCH"

ENV _DEBIAN_FRONTEND=$DEBIAN_FRONTEND
ENV DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/Denver /etc/localtime
RUN apt-get -y update && apt-get -y install texlive latex2html perl csh
ENV DEBIAN_FRONTEND=$_DEBIAN_FRONTEND
ENV _DEBIAN_FRONTEND=""

WORKDIR ${HOME}/sandbox/esmf
RUN git clone --branch ${ESMF_BRANCH} --depth 1 https://github.com/esmf-org/esmf.git src-git
WORKDIR src-git

ENV ESMF_DIR=${HOME}/sandbox/esmf/src-git

RUN make doc 2>&1 | tee esmf-make-doc.out
