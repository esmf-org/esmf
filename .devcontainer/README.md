# Earth System Modeling Framework Dev Containers

This directory contains the Earth System Modeling Framework (ESMF) development container definitions used for local Docker workflows and VS Code Dev Containers.

## Purpose

Dev containers provide a reproducible environment with the compilers, MPI libraries, and scientific libraries required to build the Earth System Modeling Framework. Each container variant lives in its own directory and includes:

- `devcontainer.json` for VS Code Dev Containers
- `Dockerfile` and supporting scripts
- `README.md` with variant-specific usage notes

## Available Dev Containers

Container directories are named as `<os>_<compiler>_<mpi>` so additional variants can be added without changing the layout of this directory. Configuration information for each variant (title, description, registries, platforms, etc.) is stored in [`variants.json`](variants.json).

| Variant | Summary |
|---------|---------|
| [ubuntu-25.10_gcc-15_mpich](ubuntu-25.10_gcc-15_mpich/) | GCC-15 and MPICH development environment with Spack-managed ESMF dependencies |

## VS Code Usage

1. Install [Docker](https://www.docker.com/) (or another OCI-compatible container runtime).
2. Install [Visual Studio Code](https://code.visualstudio.com/) and the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers).
3. Clone this repository and open it in VS Code.
4. Run **Dev Containers: Reopen in Container** from the Command Palette and select the desired container.

## GitHub Codespaces

1. Open the repository on GitHub.
2. Select **Code** → **Codespaces** → **Codespace repository configuration (...)** → **New with options...**
3. Select Options → **Create codespace**

## Pulling and Running a Dev Container Locally

Pull and run a pre-built image directly with Docker:

```bash
docker pull ghcr.io/esmf-org/esmfdev_ubuntu-25.10_gcc-15_mpich:latest
docker run -it --rm ghcr.io/esmf-org/esmfdev_ubuntu-25.10_gcc-15_mpich:latest
```

To work on local source code inside the container, mount your checkout:

```bash
docker run -it --rm \
  -v "$(pwd)":/home/esmf-dev/esmf \
  -w /home/esmf-dev/esmf \
  ghcr.io/esmf-org/esmfdev_ubuntu-25.10_gcc-15_mpich:latest
```

## Notes

For background on Docker itself, see the [Docker documentation](https://docs.docker.com/).
