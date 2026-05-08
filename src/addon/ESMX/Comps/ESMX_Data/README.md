# ESMX Data Component

ESMX Data is a lightweight data component designed for use in basic technical testing. Each instance of ESMX Data is run-time configured with a custom list of import and export fields. Each field references a specific geometry, typekind, and optionally init, min, and max values. Multiple geometries can be defined per `ESMX_Data` instance.

## ESMX Data Build Configuration

The ESMX Data component is built into ESMX applications by default, unless it is explicitly disabled in the ESMX_BUILD_FILE via the `disable_comps` option.

```
application:

  disable_comps: ESMX_Data
```

The default ESMX Data implementation that comes with ESMF can be overridden with a custom version under the `components` section of the ESMX_BUILD_FILE. For example, for a custom version that is located under the `MyCustomDataComponent` source directory:
```
components:

  ESMX_Data:
    source_dir: MyCustomDataComponent
```

## ESMX Data Run Configuration

Each ESMX Data instance is configured under its component label section in `esmxRun.yaml` using [YAML](https://yaml.org/) format. The available configuration keys are listed below.


### `timeKeeping`

The `timeKeeping` key is ***required***. It must either be set to `Model` or `Mediator`:
- Use the `Model` setting to timestamp the export fields according to the time at the *end* of the Advance step of the `ESMX_Data` component instance.
- Use the `Mediator` setting to timestamp the export fields according to the time at the *beginning* of the Advance step of the `ESMX_Data` component instance.

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  timeKeeping:        Mediator
```

### `geometries`

The `geometries` key must be associated with a map of key/value pairs. Each key specifies the name by which the geometry can be referenced from a field in `importFields` or `exportFields` (defined further down). The value once again is a map with key/values as per the following table.

| Option key      | Description / Value options                                      | Default           |
| --------------- | ---------------------------------------------------------------- | ----------------- |
| `geom`          | ESMF geometry shorthand: `grid1PeriDim`, `gridNoPeriDim`.        | ***required***    |
| `minIndex`      | The lower corner of the global index space.                      | [1,1] or [1,1,1] depending on rank |
| `maxIndex`      | The upper corner of the global index space.                      | ***required***    |
| `minCornerCoord`| The coordinate of the lower corner.                              | ***required***    |
| `maxCornerCoord`| The coordinate of the upper corner.                              | ***required***    |
| `coordSys`      | ESMF coordSys shorthand: `CART`, `SPH_DEG`, `SPH_RAD`.           | `SPH_DEG`         |
| `staggerLoc`    | ESMF staggerLoc shorthand: `CENTER`, `CORNER`, `EDGE1`, `EDGE2`. | `CENTER`          |

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  geometries:
    global:
      geom:           grid1PeriDim
      minCornerCoord: [-180, -89]
      maxCornerCoord: [+180, +89]
      maxIndex:       [ 200, 100]
      staggerLoc:     center
```

This defines a geometry called `global`, which is instantiated as 2D `ESMF_Grid` object where the first dimension is periodic. There are 200 elements along the first dimension, and 100 elements along the second dimension. Default spherical degrees are used for the coordinates. The longitudes (first dimension) run from `-180` to `+180` degrees. The latitudes (second dimension) run from `-89` to `+89` degrees. The values of any field defined on `global` are located on the `center` stagger location.

### `importFields`

The `importFields` key must be associated with a map of key/value pairs. Each key specifies the standard name of a field in the import state of the ESMX Data instance. The value once again is a map with key/values as per the following table.

| Option key       | Description / Value options                                                          | Default           |
| ---------------- | ------------------------------------------------------------------------------------ | ----------------- |
| `geometry`       | The name of a geometry defined under `geometries`.                                   | ***required***    |
| `typekind`       | One of the valid type kinds: `i4`, `i8`, `r4`, `r8`.                                 | ***required***    |
| `gridToFieldMap` | The mapping of grid to field dimension. For details see ESMF documentation.          | `[1,2]` or `[1,2,3]` depending on rank |
| `ungriddedLBound`| The lower bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `ungriddedUBound`| The upper bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `dataInit`       | The numerical value used to fill field data during initialization.                   | *none* |
| `dataMask`       | The numerical value ignored during field statistics and validation check.            | *none* |
| `dataMin`        | The minimum numerical value allowed in the field data to pass validation check.      | *none* |
| `dataMax`        | The maximum numerical value allowed in the field data to pass validation check.      | *none* |
| `dataDiagnose`   | Enable/disable output of field data diagnostics: `yes` or `no`.                      | `no` |
| `dataValidate`   | The level of field data validation against the provided `dataMin` and `dataMax`: `no` - no validation, `warn` - issue warning if data found outside value range, `err` - return with error if data found outside value range.      | `no` |

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  importFields:
    sea_surface_temperature:  {geometry: global, typekind: r8, dataDiagnose: yes}
    density:
      geometry:         global
      typekind:         r4
      ungriddedLBound:  [1]
      ungriddedUBound:  [104]
      dataMin:          1e-05
      dataDiagnose:     yes
      dataValidate:     err
```

This configuration defines two fields within the `DAT` import state. The first, standard-named `sea_surface_temperature`, is defined on the `global` geometry using double-precision (`r8`) data. As there are no ungridded dimensions, `sea_surface_temperature` functions as a 2D surface field. Because no `*Value` keys are specified, the field is neither locally initialized nor restricted by global min/max data bounds. While `dataDiagnose: yes` enables global diagnostic output to `stdout`, data validation remains inactive.

The second field, standard-named `density`, is defined on the `global` geometry using single-precision (`r4`) data. It features a single ungridded dimension spanning indices `1` to `104`, representing 104 levels. A `dataMin` of `1e-05` is established to monitor the field during each Advance step. With data diagnostics enabled, the system will output field status to `stdout`; furthermore, the `dataValidate: err` setting ensures an error is triggered if any `density` value falls below the defined minimum.

### `exportFields`

The `exportFields` key must be associated with a map of key/value pairs. Each key specifies the standard name of a field in the export state of the ESMX Data instance. The value once again is a map with key/values as per the following table.

| Option key       | Description / Value options                                                          | Default           |
| ---------------- | ------------------------------------------------------------------------------------ | ----------------- |
| `geometry`       | The name of a geometry defined under `geometries`.                                   | ***required***    |
| `typekind`       | One of the valid type kinds: `i4`, `i8`, `r4`, `r8`.                                 | ***required***    |
| `gridToFieldMap` | The mapping of grid to field dimension. For details see ESMF documentation.          | `[1,2]` or `[1,2,3]` depending on rank |
| `ungriddedLBound`| The lower bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `ungriddedUBound`| The upper bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `dataInit`       | The numerical value used to fill field data during initialization.                   | *none* |
| `dataMask`       | The numerical value ignored during field statistics and validation check.            | *none* |
| `dataMin`        | The minimum numerical value allowed in the field data to pass validation check.      | *none* |
| `dataMax`        | The maximum numerical value allowed in the field data to pass validation check.      | *none* |
| `dataDiagnose`   | Enable/disable output of field data diagnostics: `yes` or `no`.                      | `no` |
| `dataValidate`   | The level of field data validation against the provided `dataMin` and `dataMax`: `no` - no validation, `warn` - issue warning if data found outside value range, `err` - return with error if data found outside value range.      | `no` |
| `dataAdvance`    | Simple arithmetic expression that supports numbers and standard names of import fields as operands, and +, -, *, / as operators. Parentheses are supported. The expression is used to update the data of export fields during the Advance step.  | *none* |

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  exportFields:
    sea_surface_temperature:
      geometry:     global
      typekind:     r8
      dataAdvance:  1.1 * sea_surface_temperature
```

This configuration defines a single field, `sea_surface_temperature`, in the `DAT` export state. It is defined on the `global` geometry using double-precision (`r8`) values. With no ungridded dimensions, the field is treated as a 2D surface. The omission of `*Value` keys indicates that the field is not locally initialized and incoming values are not validated against global extrema. During each `Advance` step, the `dataAdvance` expression exports the field at 110% of its current imported value; if `sea_surface_temperature` is missing from the `importState`, an error is triggered.
