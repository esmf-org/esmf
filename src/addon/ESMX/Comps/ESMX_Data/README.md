# ESMX Data Component

`ESMX_Data` is a lightweight data component designed for basic technical testing of NUOPC compliant components and applications. Each instance of ESMX Data is run-time configured with a custom list of import and export fields. Each field references a specific geometry, typekind, and implements optional data validation. Multiple geometries can be defined per ESMX Data instance.

The component functions as a programmable transformation layer that can be used as a "synthetic data generator", "data feedback component", and/or "diagnostic processor". During the execution phase, it ingests fields from its import state and applies user-defined mathematical expressions element-wise across the spatial grid. This allows for the dynamic derivation of new data or the modification of existing fields. To provide physical context, the system can inject simulation metadata into these calculations, such as spatial coordinates, the current time step index, or physical constants like Pi. Standard mathematical functions, such as sin(), cos(), etc., are supported.

Once the transformation is complete, the data undergoes an optional validation stage where it is checked against user-defined guards to ensure numerical stability and prevent the propagation of invalid values. The final processed fields are then timestamped according to the component's time keeping configuration and made available to connected components through its export state.

## ESMX Data Build Configuration

The ESMX Data component is built into ESMX applications by default. It can be disabled by explicitly setting the `disable_comps` option in the ESMX_BUILD_FILE.

```
application:

  disable_comps: ESMX_Data
```

Furthermore, the default ESMX Data implementation that comes with ESMF can be overridden with a custom version under the `components` section of the ESMX_BUILD_FILE. For example, the following uses a custom ESMX Data version that is located under the `MyCustomDataComponent` source directory:
```
components:

  ESMX_Data:
    source_dir: MyCustomDataComponent
```

## ESMX Data Run Configuration

Each ESMX Data instance is configured under its component label section in `esmxRun.yaml`. All the standard [ESMX Component Label Options](../../README.md#component-label-options) are supported, with `model` set to `ESMX_Data`. The following example defines an `ESMX_Data` component called `DAT`, where the `Verbosity` attribute is set to `high` for increased level of NUOPC level information written to the ESMF Log file during execution.

```
DAT:
  model:        ESMX_Data
  attributes:
    Verbosity:  high
```

In addition, `ESMX_Data` implements the following custom configuration keys.


### `timeKeeping`

The `timeKeeping` key is ***required***. It must either be set to `Model` or `Mediator`:
- Use the `Model` setting to timestamp the export fields according to the time at the *end* of the Advance step of the `ESMX_Data` component instance.
- Use the `Mediator` setting to timestamp the export fields according to the time at the *beginning* of the Advance step of the `ESMX_Data` component instance.

The following example sets `Mediator` style time keeping in the `DAT` instance:

```
DAT:
  ...
  timeKeeping:        Mediator
```

### `geometries`

The `geometries` key must be associated with a map of key/value pairs. Each key specifies the name by which the geometry can be referenced from a field in `importFields` or `exportFields` (defined further down). The value once again is a map with key/values as per the following table.

| Option key      | Description / Value options                                      | Default           |
| --------------- | ---------------------------------------------------------------- | ----------------- |
| `geom`          | ESMF geometry shorthand: `grid1PeriDimUfrm`, `gridNoPeriDimUfrm`.| ***required***    |
| `minIndex`      | The lower corner of the global index space.                      | [1,1] or [1,1,1] depending on rank |
| `maxIndex`      | The upper corner of the global index space.                      | ***required***    |
| `minCornerCoord`| The coordinate of the lower corner.                              | ***required***    |
| `maxCornerCoord`| The coordinate of the upper corner.                              | ***required***    |
| `coordSys`      | ESMF coordSys shorthand: `CART`, `SPH_DEG`, `SPH_RAD`.           | `SPH_DEG`         |
| `staggerLocList`| List of stagger locations provided by this geom object. ESMF staggerLoc shorthand options:<br> 2D: `CENTER`, `CORNER`, `EDGE1`, `EDGE2`.<br> 3D: `CENTER_VCENTER`, `CORNER_VCENTER`, `EDGE1_VCENTER`, `EDGE2_VCENTER`, `CORNER_VFACE`, `EDGE1_VFACE`, `EDGE2_VFACE`, `CENTER_VFACE`.| `CENTER` or `CENTER_VCENTER` depending on rank |

The geometric rank is uniquely determined by the size of `minIndex` (if present), `maxIndex`, `minCornerCoord`, and `maxCornerCoord`. The sizes of these arrays must be identical; a mismatch in their lengths will result in a configuration validation error.

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  ...
  geometries:
    global:
      geom:           grid1PeriDimUfrm
      minCornerCoord: [-180, -89]
      maxCornerCoord: [+180, +89]
      maxIndex:       [ 200, 100]
      staggerLocList: center
```

This defines a geometry called `global`, which is instantiated as 2D `ESMF_Grid` object where the first dimension is periodic. There are 200 elements along the first dimension, and 100 elements along the second dimension. Default spherical degrees are used for the coordinates. The longitudes (first dimension) run from `-180` to `+180` degrees. The latitudes (second dimension) run from `-89` to `+89` degrees. The values of any field defined on `global` are located on the `center` stagger location.

### `importFields`

The `importFields` key must be associated with a map of key/value pairs. Each key specifies the standard name of a field in the import state of the ESMX Data instance. The value once again is a map with key/value pairs as per the following table.

| Option key       | Description / Value options                                                          | Default           |
| ---------------- | ------------------------------------------------------------------------------------ | ----------------- |
| `geometry`       | The name of a geometry defined under `geometries`. Optionally followed by `@staggerLoc`, where `staggerLoc` is any valid staggerLoc option used under `geometries`.      | ***required***    |
| `typekind`       | One of the valid type kinds: `i4`, `i8`, `r4`, `r8`.                                 | ***required***    |
| `gridToFieldMap` | The mapping of grid to field dimension. For details see ESMF documentation.          | `[1,2]` or `[1,2,3]` depending on rank |
| `ungriddedLBound`| The lower bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `ungriddedUBound`| The upper bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `dataValidate`   | [Data validation](#data-validation) applied to the import fields *before* the Advance step.                          | *none* |
| `outputList`     | List of output names defined under the `outputs` key.                                | *none* |

ESMF_Data uses the standard NUOPC data-dependencies during initialize protocol to initialize the data in all of the import fields. As per standard NUOPC rules, any import fields that are not connected will trigger an error, causing the application to abort.

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  ...
  importFields:
    sea_surface_temperature:  {geometry: global, typekind: r8, dataValidate: {print: yes} }
    density:
      geometry:         global
      typekind:         r4
      ungriddedLBound:  [1]
      ungriddedUBound:  [104]
      dataValidate:     {min: 1e-05, print: yes, action: error}
      outputList:       [import]
```

This configuration defines two fields within the `DAT` import state. The first, standard-named `sea_surface_temperature`, is defined on the `global` geometry using double-precision (`r8`) data. As there are no ungridded dimensions, `sea_surface_temperature` functions as a 2D surface field. The field is not restricted by global min/max data bounds. However `dataValidate: {print: yes}` enables global diagnostic output to `stdout`.

The second field, standard-named `density`, is defined on the `global` geometry using single-precision (`r4`) data. It features a single ungridded dimension spanning indices `1` to `104`, representing 104 levels. Data validation is established with `min` of `1e-05` to monitor the field during each Advance step. Diagnostic output to `stdout` is enabled; furthermore, the `action: error` setting ensures an error is triggered if any `density` value falls below the specified minimum. The field is added to the `import` output.

### `exportFields`

The `exportFields` key must be associated with a map of key/value pairs. Each key specifies the standard name of a field in the export state of the ESMX Data instance. The value once again is a map with key/values as per the following table.

| Option key       | Description / Value options                                                          | Default           |
| ---------------- | ------------------------------------------------------------------------------------ | ----------------- |
| `geometry`       | The name of a geometry defined under `geometries`. Optionally followed by `@staggerLoc`, where `staggerLoc` is any valid staggerLoc option used under `geometries`.      | ***required***    |
| `typekind`       | One of the valid type kinds: `i4`, `i8`, `r4`, `r8`.                                 | ***required***    |
| `gridToFieldMap` | The mapping of grid to field dimension. For details see ESMF documentation.          | `[1,2]` or `[1,2,3]` depending on rank |
| `ungriddedLBound`| The lower bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `ungriddedUBound`| The upper bound of the ungridded dimension(s). For details see ESMF documentation.   | *none* |
| `dataInit`       | [Dynamic arithmetic expression](#dynamic-arithmetic-expressions) used to initialze field data during DataInitialize. | *none* |
| `dataAdvance`    | [Dynamic arithmetic expression](#dynamic-arithmetic-expressions) used to update field data during Advance.           | *none* |
| `dataValidate`   | [Data validation](#data-validation) applied to the export fields *after* the Advance step.                           | *none* |
| `outputList`     | List of output names defined under the `outputs` key.                                | *none* |

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  ...
  exportFields:
    sea_surface_temperature:
      geometry:     global
      typekind:     r8
      dataInit:     sea_surface_temperature
      dataAdvance:  1.1 * sea_surface_temperature
      outputList:   [export]

```

This configuration defines a single field, `sea_surface_temperature`, in the `DAT` export state. It is defined on the `global` geometry using double-precision (`r8`) values. With no ungridded dimensions, the field is treated as a 2D surface. The field data is initialized via `dataInit` to match that of the imported `sea_surface_temperature` field. During each `Advance` step, the `dataAdvance` expression exports the field at 110% of its current imported value; if `sea_surface_temperature` is missing from the `importState`, an error is triggered. The field is added to the `export` output.

### `outputs`

The `outputs` key must be associated with a map of key/value pairs. Each key specifies the name by which the output can be referenced from a field in `importFields` or `exportFields`. The value once again is a map with key/values as per the following table.

| Option key      | Description / Value options                                      | Default           |
| --------------- | ---------------------------------------------------------------- | ----------------- |
| `onDataInit`    | Logical to enable/disable data output at the end of the DataInitialize phase. | `false` |
| `onImport`      | Logical to enable/disable data output at the beginning of the Advance phase.  | `false` |
| `onExport`      | Logical to enable/disable data output at the end of the Advance phase.        | `false` |
| `separateFieldFiles` | Logical to control whether fields are written to individual files (`true`), or bundled together into a single file (`false`).              | `false` |
| `separateTimeFiles`  | Logical to control whether each time step is written to a separate file (`true`), or appended as timeslices within the same file (`false`).| `false` |

Each field that is associated with a particular output will be written to file when the output triggers. The format of these data files is NetCDF. The naming pattern of the generated files is:

```
data_<component-name>_<output-name>[_<time-slice>][_<standard-name>].nc
```

For an example, see the following configuration snippet for `ESMX_Data` instance named `DAT`.

```
DAT:
  ...
  outputs:
    import:
      onImport:   true
    export:
      onDataInit: true
      onExport:   true
```

This configuration defines two outputs. The output called `import` triggers at the beginning of the Advance phase, while output called `export` triggers at the end of the DataInitialize _and_ Advance phases.

---

### Data validation

The `dataValidate` option, if specified, must be associated with a map of key/value pairs. All pairs are optional with default values as per the following table.

| Option key       | Description / Value options                                                          | Default           |
| ---------------- | ------------------------------------------------------------------------------------ | ----------------- |
| `min`            | The minimum numerical value allowed in the field data to pass validation.            | *no minimum*  |
| `max`            | The maximum numerical value allowed in the field data to pass validation.            | *no maximum*  |
| `mask`           | The numerical value ignored during field validation.                                 | *no mask*     |
| `print`          | Logical to enable/disable field data diagnostic output to stdout.                    | `false`       |
| `action`         | Action to be taken when field data validation fails: `ignore`, `warning`, `error`    | `error`       |

### Dynamic arithmetic expressions

The `dataInit` and `dataAdvance` options support dynamic arithmetic expressions that allow users to define mathematical transformations for field data using standard **infix notation**. These expressions are evaluated element-wise across the entire spatial domain of the involved fields.

#### 1. Supported Operators
Expressions support standard arithmetic operators following traditional mathematical precedence.

| Operator | Description | Precedence | Example |
| :--- | :--- | :--- | :--- |
| `*` | Multiplication | high | `field_a * 10.0` |
| `/` | Division | high | `field_b / 2.0` |
| `+` | Addition (including Unary Plus) | low | `field_a + field_b` |
| `-` | Subtraction (including Unary Minus) | low | `-field_c + 5.0` |

> **Note**: Parentheses `()` can be used to override default precedence and group operations.

---

#### 2. Mathematical Functions
Functions are case-insensitive and apply a transformation to each point in the data field. Most of these functions are implemented directly using their standard Fortran intrinsic equivalents.

##### Trigonometric & Hyperbolic
* **Trigonometry**: `SIN`, `COS`, `TAN`, `ASIN`, `ACOS`, `ATAN`
* **Hyperbolic**: `SINH`, `COSH`, `TANH`, `ASINH`, `ACOSH`, `ATANH`
* **Angular Conversion**: `DEG2RAD` (Degrees to Radians), `RAD2DEG` (Radians to Degrees)

##### Basic & Advanced Math
* **Logarithmic/Power**: `EXP`, `LOG` (Natural), `LOG10`, `SQRT`
* **Rounding/Truncation**: `ABS`, `AINT`, `ANINT`, `CEILING`, `FLOOR`
* **Special Functions**: `ERF`, `ERFC`, `ERFC_SCALED`, `GAMMA`, `LOG_GAMMA`

---

#### 3. Operands and Variables
The system recognizes three types of values within an expression:

##### Input Fields
Any alphanumeric name (e.g., `sea_surface_temperature`) is treated as a field name. The system will attempt to retrieve this field from the model's **Import State**. Supported data types include:
* 4-byte and 8-byte Integers (`I4`, `I8`)
* 4-byte and 8-byte Real numbers (`R4`, `R8`)

##### Special Context Variables
Variables prefixed with an underscore provide metadata about the current simulation state:
* `_PI`: The mathematical constant PI.
* `_STEP`: The current model time step index.
* `_COORDx`: The spatial coordinate for dimension `x` (e.g., `_COORD1` typically represents Longitude/X).

##### Numeric Constants
Standard numerical values (e.g., `2.5`, `100`, `1.0E-4`) are interpreted as double-precision floating-point numbers.

---

#### 4. Usage Examples

* **Synthetic data generation (Temperature field with 10 Kelvin variablity around 270 Kelvin mean)**:<br>
    `dataAdvance: 10 * (sin(_coord1) * cos(_coord2)) + 270`

* **Data feedback (Return a temperature field that is 10% hotter than the incoming field)**:<br>
    `dataAdvance: 1.1 * temperature`

* **Unit Conversion (Kelvin to Celsius)**:<br>
    `dataAdvance: temperature - 273.15`

* **Applying a spatial mask**:<br>
    `dataAdvance: field_a * sin(_coord1)`

