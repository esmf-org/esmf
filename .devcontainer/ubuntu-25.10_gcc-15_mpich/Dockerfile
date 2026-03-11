FROM ubuntu:25.10

# Define arguments for image metadata
ARG VERSION="1.0.0" \
    VENDOR="ESMF Core Team" \
    TITLE="esmfdev_ubuntu-25.10_gcc-15_mpich" \
    DESCRIPTION="ESMF development container with gcc-15 and mpich" \
    AUTHORS="esmf_support@ucar.edu"

# Set system environment variables
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

# Install system packages via apt
RUN <<INSTALL_PACKAGES
    # Force rebuild when VERSION arg changes
    echo "IMAGE_VERSION=${VERSION}" >/dev/null
    apt-get update
    apt-get install -yqq \
        build-essential \
        ca-certificates \
        cmake \
        curl \
        file \
        g++-15 \
        gcc-15 \
        gfortran-15 \
        git \
        gpg \
        gzip \
        libc6-dev \
        libcurl4-openssl-dev \
        libhdf5-dev \
        libmpich-dev \
        libnetcdf-dev \
        libnetcdff-dev \
        libssl-dev \
        locales \
        m4 \
        make \
        mpich \
        netcdf-bin \
        openssh-client \
        pkg-config \
        python3 \
        python3-dev \
        python3-pip \
        python3-yaml \
        sudo \
        tar \
        unzip \
        vim \
        wget \
        zlib1g-dev
    locale-gen en_US.UTF-8
    update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
    ln -sf gcc-15 /usr/bin/gcc
    ln -sf g++-15 /usr/bin/g++
    ln -sf gfortran-15 /usr/bin/gfortran
    arch=$(dpkg-architecture -qDEB_HOST_MULTIARCH)
    ln -sf "${arch}/libnetcdf.so" /usr/lib/libnetcdf.so
    ln -sf "${arch}/libnetcdff.so" /usr/lib/libnetcdff.so
    apt-get clean
INSTALL_PACKAGES

# Create non-root user with sudo privileges
ARG USER=esmfdev
ENV HOME=/home/${USER}
RUN <<SETUP_USER
    groupadd ${USER}
    useradd -m -d ${HOME} -s /bin/bash -g ${USER} ${USER}
    adduser ${USER} sudo
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
SETUP_USER
USER ${USER}
WORKDIR ${HOME}

# Install spack via git clone
ENV SPACK_ROOT=${HOME}/spack
RUN <<INSTALL_SPACK
    mkdir -p ${SPACK_ROOT}
    git clone --depth=2 --branch=releases/v1.1 \
      https://github.com/spack/spack.git ${SPACK_ROOT}
INSTALL_SPACK
ENV PATH="${SPACK_ROOT}/bin:${PATH}"

# Configure external spack packages
RUN <<SPACK_PKG_CONFIG
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    spack compiler find
    echo "Discovering external packages, this may take a few minutes..."
    spack external find --all
    # Discover existing package configuration in spack
    SPACK_PKGS="$(
      spack find --show-configured-externals --format '{name}@{version}'
    )"
    # Extract versions for apt packages
    gcc_version="$(gcc -dumpfullversion -dumpversion)"
    cmake_version="$(cmake --version | head -n1 | awk '{ print $3 }')"
    mpich_version="$(
      mpichversion | awk -F': *' \
        '/^[[:space:]]*MPICH Version:/ { print $2; exit }'
    )"
    hdf5_version="$(
      h5cc -showconfig | awk -F': *' \
        '/^[[:space:]]*HDF5 Version:/ { print $2; exit }'
    )"
    netcdf_c_version="$(nc-config --version | awk '{ print $2 }')"
    netcdf_fortran_version="$(nf-config --version | awk '{ print $2 }')"
    python_version="$(python3 --version | awk '{ print $2 }')"
    pip_version="$(pip3 --version | awk '{ print $2 }')"
    pyyaml_version="$(python3 -c 'import yaml; print(yaml.__version__)')"
    # Add apt package configuration to spack if missing
    check_package() {
      local pkg_ver="$1" # package@version
      if ! printf '%s\n' "$SPACK_PKGS" | \
          grep -Fqx "${pkg_ver}"; then
        return 1
      fi
      return 0
    }
    add_ext_package() {
      local pkg="$1" # package
      local spc="$2" # spec
      local pfx="$3" # prefix
      spack config add \
        "packages:${pkg}:externals:[{'spec': '${spc}', 'prefix': '${pfx}'}]"
    }
    if ! check_package "cmake@${cmake_version}"; then
      add_ext_package cmake \
        "cmake@${cmake_version}%gcc@${gcc_version}" \
        "/usr"
    fi
    if ! check_package "mpich@${mpich_version}"; then
      add_ext_package mpich \
        "mpich@${mpich_version}%gcc@${gcc_version}" \
        "/usr"
    fi
    if ! check_package "hdf5@${hdf5_version}"; then
      add_ext_package hdf5 \
        "hdf5@${hdf5_version}%gcc@${gcc_version}" \
        "/usr"
    fi
    if ! check_package "netcdf-c@${netcdf_c_version}"; then
      add_ext_package netcdf-c \
        "netcdf-c@${netcdf_c_version}%gcc@${gcc_version}" \
        "/usr"
    fi
    if ! check_package "netcdf-fortran@${netcdf_fortran_version}"; then
      add_ext_package netcdf-fortran \
        "netcdf-fortran@${netcdf_fortran_version}%gcc@${gcc_version}" \
        "/usr"
    fi
    if ! check_package "python@${python_version}"; then
      add_ext_package python \
        "python@${python_version}" \
        "/usr"
    fi
    if ! check_package "py-pip@${pip_version}"; then
      add_ext_package py-pip \
        "py-pip@${pip_version} ^python@${python_version}" \
        "/usr"
    fi
    if ! check_package "py-pyyaml@${pyyaml_version}"; then
      add_ext_package py-pyyaml \
        "py-pyyaml@${pyyaml_version}+libyaml ^python@${python_version}" \
        "/usr"
    fi
    spack clean --all
SPACK_PKG_CONFIG

# Install spack packages
RUN <<SPACK_PKG_INSTALL
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    spack install --fail-fast \
        cmake \
        mpich \
        hdf5 \
        netcdf-c \
        netcdf-fortran \
        python \
        py-pip \
        py-pyyaml
    spack clean --all
SPACK_PKG_INSTALL

# Copy user environment setup script
COPY --chown=$USER:$USER --chmod=0755 envsetup.sh ${HOME}/.envsetup.sh

# Source the user environment setup script
RUN <<SETUPENV
    tmp=$(mktemp)
    printf '%s\n' \
        '# Source the user environment setup script' \
        'if [ -f "${HOME}/.envsetup.sh" ]; then' \
        '    . "${HOME}/.envsetup.sh"' \
        'fi' '' > "$tmp"
    [ -f ${HOME}/.bashrc ] && cat ${HOME}/.bashrc >> "$tmp"
    mv "$tmp" ${HOME}/.bashrc
SETUPENV

# Copy README file
COPY --chown=$USER:$USER --chmod=0644 README.md ${HOME}/README.md

# Default command
CMD ["/bin/bash", "-l"]

# Set environment variable for devcontainer title
ENV ESMF_DEVCONTAINER=${TITLE}

# Add OCI labels for image metadata
# https://specs.opencontainers.org/image-spec/annotations/
LABEL org.opencontainers.image.version="${VERSION}" \
      org.opencontainers.image.vendor="${VENDOR}" \
      org.opencontainers.image.title="${TITLE}" \
      org.opencontainers.image.description="${DESCRIPTION}" \
      org.opencontainers.image.authors="${AUTHORS}"
