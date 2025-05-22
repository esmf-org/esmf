# ESMX Data Component

The ESMX Data component is a lightweight testing component that derives from `NUOPC_Model`. Each ESMX Data component provides a custom list of export fields with prescribed values and a custom list of import fields with value ranges for testing validity. Multiple ESMX Data components can be used in the same application for testing complex sets of connected components.

## ESMX Data Build Configuration

The ESMX Data component is included in all ESMX applications unless it is disabled in the ESMX_BUILD_FILE, see the following example.

```
application:

  disable_comps: ESMX_Data
```

The ESMX Data component may be replaced in the ESMX_BUILD_FILE, see the following example.

```
components:

  ESMX_Data:
    source_dir: MyDataComponent
```

## ESMX Data Run Configuration

Each ESMX Data component is configured using [YAML](https://yaml.org/) format, providing settings such as output, geom, import fields, and export fields.


### Output

Output options are configured under the `output` key. If no key/value pair is provided then the default will be used.

| Option key          | Description / Value options                            | Default   |
| ------------------- | ------------------------------------------------------ | --------- |
| `write_final`       | write import and export state to NetCDF during Finaize | true      |

For an example, see the following configuration snippet.

```
ATM:
  output:
    write_final: true
```

### Geom

Geometry options are configured under the `geom` key. If no key/value pair is provided then the default will be used.

| Option key | Description / Value options                                            | Default                 |
| ---------- | ---------------------------------------------------------------------- | ----------------------- |
| `nx`       | number of cells along the x-axis, typically longitude                  | 64                      |
| `ny`       | number of cells along the y-axis, typically latitude                   | 32                      |
| `nz`       | number of cells along the z-axis, ungridded dimension for layers       | 4                       |
| `coordSys` | `ESMF_COORDSYS_CART`, `ESMF_COORDSYS_SPH_DEG`, `ESMF_COORDSYS_SPH_RAD` | `ESMF_COORDSYS_SPH_DEG` | 
| `minx`     | minimum coordinate for the x-axis, typically longitude                 | -126.0                  |
| `miny`     | minimum coordinate for the y-axis, typically latitude                  | 22.0                    |
| `maxx`     | maximum coordinate for the x-axis, typically longitude                 | -64.0                   |
| `maxy`     | maximum coordinate for the y-axis, typically latitude                  | 50.0                    |

For an example, see the following configuration snippet.

```
ATM:
  geom:
    nx: 64
    ny: 32
    minx: -126.0
    miny: 22.0
    maxx: -64.0
    maxy: 50.0
```

### Import Fields

Import field list is configured under the `importFields` key. See the following table for valid field configuration options.

| Option key | Description / Value options         | Default    |
| ---------- | ----------------------------------- | ---------- |
| `dim`      | number of dimensions                | 2          |
| `min`      | minimum valid field value           | 0          |
| `max`      | maximum valid field value           | 0          |

For an example, see the following configuration snippet.

```
ATM:
  importFields:
    sea_surface_temperature: {dim: 2, min: 260, max: 280}
```

### Export Fields

Export field list is configured under the `exportFields` key. See the following table for valid field configuration options.

| Option key | Description / Value options                                | Default    |
| ---------- | ---------------------------------------------------------- | ---------- |
| `dim`      | number of dimensions, 2 or 3, third dimension is ungridded | 2          |
| `val`      | prescribed value used to fill field                        | 0          |

For an example, see the following configuration snippet.

```
ATM:
  exportFields:
    sea_surface_temperature: {dim: 2, val: 273}
```

