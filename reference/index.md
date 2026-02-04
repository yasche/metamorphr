# Package index

## All functions

- [`atoms`](https://yasche.github.io/metamorphr/reference/atoms.md) : A
  tibble containing the NIST standard atomic weights

- [`calc_km()`](https://yasche.github.io/metamorphr/reference/calc_km.md)
  : Calculate the Kendrick mass

- [`calc_kmd()`](https://yasche.github.io/metamorphr/reference/calc_kmd.md)
  : Calculate the Kendrick mass defect (KMD)

- [`calc_neutral_loss()`](https://yasche.github.io/metamorphr/reference/calc_neutral_loss.md)
  : Calculate neutral losses from precursor ion mass and fragment ion
  masses

- [`calc_nominal_km()`](https://yasche.github.io/metamorphr/reference/calc_nominal_km.md)
  : Calculate the nominal Kendrick mass

- [`collapse_max()`](https://yasche.github.io/metamorphr/reference/collapse_max.md)
  : Collapse intensities of technical replicates by calculating their
  maximum

- [`collapse_mean()`](https://yasche.github.io/metamorphr/reference/collapse_mean.md)
  : Collapse intensities of technical replicates by calculating their
  mean

- [`collapse_median()`](https://yasche.github.io/metamorphr/reference/collapse_median.md)
  : Collapse intensities of technical replicates by calculating their
  median

- [`collapse_min()`](https://yasche.github.io/metamorphr/reference/collapse_min.md)
  : Collapse intensities of technical replicates by calculating their
  minimum

- [`create_metadata_skeleton()`](https://yasche.github.io/metamorphr/reference/create_metadata_skeleton.md)
  : Create a blank metadata skeleton

- [`filter_blank()`](https://yasche.github.io/metamorphr/reference/filter_blank.md)
  : Filter Features based on their occurrence in blank samples

- [`filter_cv()`](https://yasche.github.io/metamorphr/reference/filter_cv.md)
  : Filter Features based on their coefficient of variation

- [`filter_global_mv()`](https://yasche.github.io/metamorphr/reference/filter_global_mv.md)
  : Filter Features based on the absolute number or fraction of samples
  it was found in

- [`filter_grouped_mv()`](https://yasche.github.io/metamorphr/reference/filter_grouped_mv.md)
  : Group-based feature filtering

- [`filter_msn()`](https://yasche.github.io/metamorphr/reference/filter_msn.md)
  : Filter Features based on occurrence of fragment ions

- [`filter_mz()`](https://yasche.github.io/metamorphr/reference/filter_mz.md)
  : Filter Features based on their mass-to-charge ratios

- [`filter_neutral_loss()`](https://yasche.github.io/metamorphr/reference/filter_neutral_loss.md)
  : Filter Features based on occurrence of neutral losses

- [`formula_to_mass()`](https://yasche.github.io/metamorphr/reference/formula_to_mass.md)
  : Calculate the monoisotopic mass from a given formula

- [`impute_bpca()`](https://yasche.github.io/metamorphr/reference/impute_bpca.md)
  : Impute missing values using Bayesian PCA

- [`impute_global_lowest()`](https://yasche.github.io/metamorphr/reference/impute_global_lowest.md)
  : Impute missing values by replacing them with the lowest observed
  intensity (global)

- [`impute_knn()`](https://yasche.github.io/metamorphr/reference/impute_knn.md)
  : Impute missing values using nearest neighbor averaging

- [`impute_lls()`](https://yasche.github.io/metamorphr/reference/impute_lls.md)
  : Impute missing values using Local Least Squares (LLS)

- [`impute_lod()`](https://yasche.github.io/metamorphr/reference/impute_lod.md)
  : Impute missing values by replacing them with the Feature 'Limit of
  Detection'

- [`impute_mean()`](https://yasche.github.io/metamorphr/reference/impute_mean.md)
  : Impute missing values by replacing them with the Feature mean

- [`impute_median()`](https://yasche.github.io/metamorphr/reference/impute_median.md)
  : Impute missing values by replacing them with the Feature median

- [`impute_min()`](https://yasche.github.io/metamorphr/reference/impute_min.md)
  : Impute missing values by replacing them with the Feature minimum

- [`impute_nipals()`](https://yasche.github.io/metamorphr/reference/impute_nipals.md)
  : Impute missing values using NIPALS PCA

- [`impute_ppca()`](https://yasche.github.io/metamorphr/reference/impute_ppca.md)
  : Impute missing values using Probabilistic PCA

- [`impute_rf()`](https://yasche.github.io/metamorphr/reference/impute_rf.md)
  : Impute missing values using random forest

- [`impute_svd()`](https://yasche.github.io/metamorphr/reference/impute_svd.md)
  : Impute missing values using Singular Value Decomposition (SVD)

- [`impute_user_value()`](https://yasche.github.io/metamorphr/reference/impute_user_value.md)
  : Impute missing values by replacing them with a user-provided value

- [`join_metadata()`](https://yasche.github.io/metamorphr/reference/join_metadata.md)
  : Join a featuretable and sample metadata

- [`normalize_cyclic_loess()`](https://yasche.github.io/metamorphr/reference/normalize_cyclic_loess.md)
  : Normalize intensities across samples using cyclic LOESS
  normalization

- [`normalize_factor()`](https://yasche.github.io/metamorphr/reference/normalize_factor.md)
  : Normalize intensities across samples using a normalization factor

- [`normalize_median()`](https://yasche.github.io/metamorphr/reference/normalize_median.md)
  : Normalize intensities across samples by dividing by the sample
  median

- [`normalize_pqn()`](https://yasche.github.io/metamorphr/reference/normalize_pqn.md)
  : Normalize intensities across samples using a Probabilistic Quotient
  Normalization (PQN)

- [`normalize_quantile_all()`](https://yasche.github.io/metamorphr/reference/normalize_quantile_all.md)
  : Normalize intensities across samples using standard Quantile
  Normalization

- [`normalize_quantile_batch()`](https://yasche.github.io/metamorphr/reference/normalize_quantile_batch.md)
  : Normalize intensities across samples using grouped Quantile
  Normalization with multiple batches

- [`normalize_quantile_group()`](https://yasche.github.io/metamorphr/reference/normalize_quantile_group.md)
  : Normalize intensities across samples using grouped Quantile
  Normalization

- [`normalize_quantile_smooth()`](https://yasche.github.io/metamorphr/reference/normalize_quantile_smooth.md)
  : Normalize intensities across samples using smooth Quantile
  Normalization (qsmooth)

- [`normalize_ref()`](https://yasche.github.io/metamorphr/reference/normalize_ref.md)
  : Normalize intensities across samples using a reference feature

- [`normalize_sum()`](https://yasche.github.io/metamorphr/reference/normalize_sum.md)
  : Normalize intensities across samples by dividing by the sample sum

- [`plot_pca()`](https://yasche.github.io/metamorphr/reference/plot_pca.md)
  : Draws a scores or loadings plot or performs calculations necessary
  to draw them manually

- [`plot_volcano()`](https://yasche.github.io/metamorphr/reference/plot_volcano.md)
  : Draws a Volcano Plot or performs calculations necessary to draw one
  manually

- [`read_featuretable()`](https://yasche.github.io/metamorphr/reference/read_featuretable.md)
  : Read a feature table into a tidy tibble

- [`read_mgf()`](https://yasche.github.io/metamorphr/reference/read_mgf.md)
  : Read a MGF file into a tidy tibble

- [`scale_auto()`](https://yasche.github.io/metamorphr/reference/scale_auto.md)
  : Scale intensities of features using autoscale

- [`scale_center()`](https://yasche.github.io/metamorphr/reference/scale_center.md)
  : Center intensities of features around zero

- [`scale_level()`](https://yasche.github.io/metamorphr/reference/scale_level.md)
  : Scale intensities of features using level scaling

- [`scale_msn()`](https://yasche.github.io/metamorphr/reference/scale_msn.md)
  : Scale intensities in MSn spectra to the highest value within each
  spectrum

- [`scale_pareto()`](https://yasche.github.io/metamorphr/reference/scale_pareto.md)
  : Scale intensities of features using Pareto scaling

- [`scale_range()`](https://yasche.github.io/metamorphr/reference/scale_range.md)
  : Scale intensities of features using range scaling

- [`scale_vast()`](https://yasche.github.io/metamorphr/reference/scale_vast.md)
  : Scale intensities of features using vast scaling

- [`scale_vast_grouped()`](https://yasche.github.io/metamorphr/reference/scale_vast_grouped.md)
  : Scale intensities of features using grouped vast scaling

- [`summary_featuretable()`](https://yasche.github.io/metamorphr/reference/summary_featuretable.md)
  : General information about a feature table and sample-wise summary

- [`toy_metaboscape`](https://yasche.github.io/metamorphr/reference/toy_metaboscape.md)
  : A small toy data set created from a feature table in MetaboScape
  style

- [`toy_metaboscape_metadata`](https://yasche.github.io/metamorphr/reference/toy_metaboscape_metadata.md)
  :

  Sample metadata for the fictional dataset `toy_metaboscape`

- [`toy_mgf`](https://yasche.github.io/metamorphr/reference/toy_mgf.md)
  : A small toy data set containing MSn spectra

- [`transform_log()`](https://yasche.github.io/metamorphr/reference/transform_log.md)
  : Transforms the intensities by calculating their log

- [`transform_power()`](https://yasche.github.io/metamorphr/reference/transform_power.md)
  :

  Transforms the intensities by calculating their *n*th root
