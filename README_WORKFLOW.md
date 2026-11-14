# Mortgage-Risk project workflow

## Scope
- Main dataset: Freddie Mac Single-Family Loan-Level Dataset.
- External validation dataset: Fannie Mae Single-Family Loan Performance Data.
- Time split: train 2011-2014, validation 2015, test 2016.
- Main models: Logistic Regression, GPBoost Spatial, GPBoost Spatio-temporal, SVC, Random Forest and LightGBM.
- External validation: LightGBM and GPBoost Spatial only.

## Data policy
Do not commit raw or processed loan-level data, model checkpoints or other large artifacts.
Place raw source files under `1_data_set/data/raw/` and generated datasets under `1_data_set/data/processed/`.

## R environment
Open `Mortgage-Risk.Rproj` in RStudio. Initialize renv after agreeing on project packages:

```r
install.packages("renv")
renv::init()
```

After installing the required packages, record exact versions with `renv::snapshot()`.
Other team members can restore the environment using `renv::restore()`.
Commit the generated `renv.lock`; do not create a hand-written placeholder lockfile.

## Reproducibility
Record data vintage, source schema, preprocessing decisions, random seeds, model configuration, package versions and evaluation outputs.
