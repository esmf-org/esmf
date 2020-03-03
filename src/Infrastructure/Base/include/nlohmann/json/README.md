# JSON for Modern C++

* URL: https://github.com/nlohmann/json
* Release: v3.7.3
* Git Clone Command: `git clone -b <Release> --depth 1 <URL>`

## Updating to New Version

1. Run the `git clone` command above in an external directory to get the appropriate release.
2. Copy `./single_include/nlohmann/json.hpp` and `./LICENSE.MIT` into the ESMF directory `./src/Infrastructure/Base/include/nlohmann/json`.
3. Update the `Release` above.
4. Update the `Change Log` below.

## Change Log

* 2018-10-03, Ben Koziol
    * Added initial code (v3.2.0) to ESMF Attribute directory
    * Added `README.md` describing how to bring in additional versions
* 2020-03-02, Ben Koziol
    * Updated `nlohmann/json` to tag `v3.7.3`.