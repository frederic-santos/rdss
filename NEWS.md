# rdss 1.0.0 (Release date: 2020-12-XX)

This is the first release of `rdss` which is ready for public use. This version takes into account the suggestions made by anonymous reviewers of the *International Journal of Osteoarchaeology*, who must be acknowledged for their work.

## Substantial changes
- New option for the "Import data" step: individuals with all missing values can now be automatically removed.
- The whole data file can now be viewed in a pop-up just after the importation step.
- The UI widgets (filtering criteria) now reset to their default values when chosing a new target individual.
- New UI layout for tab "3. Check or customize the reference sample".
- The imputed reference dataset can now be downloaded through the user interface.
- More confidence levels (i.e., classification thresholds) are now allowed in tab "4. Perform sex estimation".
- Classification accuracy in LOOCV is now also given for females and males separately, in search for potential bias in sex estimation.

## Minor changes
- Fixed typos in UI.
- Small code improvements and optimization.

# rdss 0.9.7 (Release date: 2020-10-06)

## Minor changes
- Additional tips and help have been added in the last tab of the user interface.
- A loading screen has been added for the computer-intensive method of missing data imputation.
- Some very simple unit tests have been added for the most elementary functions.

# rdss 0.9.6 (Release date: 2020-09-30)

## Bug fix
- Results of sensitivity analysis are now correctly displayed.

## Minor changes
- Improved behavior of some UI widgets.

# rdss 0.9.5 (Release date: 2020-09-29)

## Bug fix
- Fixed issue when submitting filtered datasets to Robust LDA or Penalized regression.

# rdss 0.9.4 (Release date: 2020-09-03)

## Minor changes
- Fixed issue for displaying the results of robust LDA.
- An example file has been added.

# rdss 0.9.3 (Release date: 2020-09-01)

## New features
- Robust linear discriminant analysis is now available for sex estimation.

## Minor changes
- Small UI adjustments.

# rdss 0.9.2 (Release date: 2020-08-31)

## New features
- Penalized logistic regression (ridge or LASSO) is now available for sex estimation.

# rdss 0.9.1 (Release date: 2020-08-28)

## New features
- Improved auto-tuning for random forests.
- More parameters for random forests have been added in the UI.

## Minor changes
- UI: large tables (if many variables are used for sex estimation) are now better displayed in the UI.
- UI: variable names can now be rotated on the `md.pattern()` plot.

# rdss 0.9.0 (Release date: 2020-08-11)

First (experimental) release on GitLab.
