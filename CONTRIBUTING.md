The [Earth System Modeling Framework](https://earthsystemmodeling.org/) (ESMF)
is high-performance, flexible software infrastructure for building and coupling
weather, climate, and related Earth science applications. Please review the
following guidelines for contributing to the Earth System Modeling Framework.

[fork]: https://github.com/esmf-org/esmf/fork
[pr]: https://github.com/esmf-org/esmf/compare
[actions]: https://github.com/esmf-org/esmf/actions

## Resources

* ESMF User Guide - [html](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc), [pdf](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc.pdf)
* Fortran Reference Manual - [html](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc), [pdf](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc.pdf)
* C Reference Manual - [html](https://earthsystemmodeling.org/docs/nightly/develop/ESMC_crefdoc), [pdf](https://earthsystemmodeling.org/docs/nightly/develop/ESMC_crefdoc.pdf)
* NUOPC Reference Manual - [html](https://earthsystemmodeling.org/docs/nightly/develop/NUOPC_refdoc), [pdf](https://earthsystemmodeling.org/docs/nightly/develop/NUOPC_refdoc.pdf)
* ESMPy Documentation - [html](https://earthsystemmodeling.org/esmpy_doc/nightly/develop/html), [pdf](https://earthsystemmodeling.org/esmpy_doc/nightly/develop/ESMPy.pdf)
* Developer’s Guide - [html](https://earthsystemmodeling.org/docs/nightly/develop/dev_guide)

## Submitting a pull request

1. [Fork][fork] and clone the repository
2. Create a new branch: `git checkout -b my-branch-name`
3. Make and [test](#testing) your changes
4. Update [documentation](#documentation)
5. Push to your fork
6. Execute `ESMF CI` and `ESMF Docs` from your fork on your branch using [actions][actions]
7. Compare across forks and [submit a pull request][pr]

## Testing

Pull requests must pass the [ESMF CI][actions] action before they are accepted
into the `develop` branch. The [ESMF CI][actions] action is configured to test
ESMF on a variety of configurations. These include multiple operating systems,
MPI libraries, compilers, and build options. Prior to executing the
[ESMF CI][actions] action, changes should pass `make all_tests` on a local
machine or HPC. 

## Documentation

ESMF uses LaTeX and LaTeX extensions to build inline documentation.
Documentation can be built using the [ESMF Docs][actions] action, which provides
all the software needed to build documentation. Documentation can also be built
locally using `texlive-full` and `latex2html`. ESMPY also requires the following
Python packages: `sphinx`, `sphinxcontrib-packages` and `sphinxcontrib-bibtex`.

* ESMF Documentation `cd ${ESMF_DIR} && make doc`
* ESMF Developers Guide: `cd ${ESMF_DIR}/src/doc/dev_guide && make`
* NUOPC Documentation `cd ${ESMF_DIR}/src/addon/NUOPC/doc && make localdoc`
* ESMPY Documentation `cd ${ESMF_DIR}/src/addon/esmpy && . ${PYTHON_VENV_ACTIVATE} && python3 -m pip install . && cd doc && . ${PYTHON_VENV_ACTIVATE} && make html latexpdf`

## Contact Us

[ESMF Discussions](https://github.com/orgs/esmf-org/discussions) is a GitHub
Discussions forum for publicly discussing ESMF concerns and requests. For more
information please see the
[ESMF Discussions How-To Guide](https://github.com/orgs/esmf-org/discussions/246).

If you do not have a GitHub Account, if you have privacy concerns posting to
ESMF Discussions, or if you prefer communication via email then please contact
esmf_support@ucar.edu.

---
Thank You,<br> 
The ESMF Core Team
