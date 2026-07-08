# Example model

`mxl_panel_example.R` estimates a minimal Panel Mixed Logit on the dataset:
an MNL kernel with iid error components in all three alternatives sharing a
single scale parameter, constant across the choice tasks
of each respondent (panel effect). Utilities include all SP attributes; the
cost variable is the fare actually displayed to each respondent (standard or
reduced, according to `reduced_fare`).

## Run

In a folder containing the dataset .csv:

```bash
Rscript mxl_panel_example.R
```

Requires R with the `apollo` package (`install.packages("apollo")`).
Estimation with 1500 MLHS inter-individual draws takes a few minutes on a
standard laptop.

## Output

Apollo writes the estimation results to `example/output/`:

- `mxl_panel_example_output.txt` — full estimation report (estimates,
  classical and robust standard errors, fit statistics).
- `mxl_panel_example_estimates.csv` — parameter estimates in CSV form.
- Additional Apollo artifacts (`_iterations.csv`, `_model.rds`, etc.).

Reference output from the run documented in the data dictionary is included
in this folder.
