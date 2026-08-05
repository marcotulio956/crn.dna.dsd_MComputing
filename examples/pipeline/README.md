# Pipeline CRN Examples

This folder contains prototype phased CRN pipeline examples based on `ideas.txt`.

## Files

- `01_primitives_test.R`: validates the absence-indicator primitive.
- `02_singlecall_pipeline_test.R`: assembles a full pipelined circuit and runs it with a single `react()` call.
- `03_absence_wave_test.R`: runs an oscillating wave generator with absence readout.

```text
while (i > a) {
  b = b - i
  i = i - 1
}
```

## Run

From repository root:

```bash
Rscript examples/pipeline/01_primitives_test.R
Rscript examples/pipeline/02_singlecall_pipeline_test.R
Rscript examples/pipeline/03_absence_wave_test.R
```

## Notes

This is a first reusable library focused on phase separation and completion-by-absence.
Rate constants are intentionally separated into `slow` and `fast` classes to emulate clocked phases.
