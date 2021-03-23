Generate Fortran and C template code.

Templates use the Python `jinja2` framework: https://jinja.palletsprojects.com/en/2.10.x/

The Python unittest framework is used for file generation. To generate
files, use the unittest CLI: https://docs.python.org/3/library/unittest.html#command-line-interface

For example, to generate the `ESMF_Attribute.F90` code:
```sh
cd ${ESMF_DIR}/src/scripts/jinja2_templating
python -m unittest generate_templates.Runner.test_ESMF_Attribute
```

The `test` prefix is left on method names to hook the template generation into the
testing framework. For example, running all "tests" will generate all the templates.