# Package index

## Descriptives & Reliability

Summary statistics, frequencies and reliability

- [`cdf()`](https://sedzinfo.github.io/rwf/reference/cdf.md) : Check
  dataframe
- [`cdff()`](https://sedzinfo.github.io/rwf/reference/cdff.md) : Check
  dataframe (optimised)
- [`compute_descriptives()`](https://sedzinfo.github.io/rwf/reference/compute_descriptives.md)
  : Descriptive statistics
- [`compute_frequencies()`](https://sedzinfo.github.io/rwf/reference/compute_frequencies.md)
  : Frequency table for categorical variables
- [`compute_skewness()`](https://sedzinfo.github.io/rwf/reference/compute_skewness.md)
  : Compute skewness of a numeric vector
- [`compute_kurtosis()`](https://sedzinfo.github.io/rwf/reference/compute_kurtosis.md)
  : Compute kurtosis of a numeric vector
- [`compute_standard()`](https://sedzinfo.github.io/rwf/reference/compute_standard.md)
  : Compute standard scores from a numeric vector
- [`compute_standard_error()`](https://sedzinfo.github.io/rwf/reference/compute_standard_error.md)
  : Compute the standard error of the mean
- [`compute_confidence_inteval()`](https://sedzinfo.github.io/rwf/reference/compute_confidence_inteval.md)
  : Compute confidence interval
- [`compute_scores()`](https://sedzinfo.github.io/rwf/reference/compute_scores.md)
  : Compute subject ability for thurstonian models
- [`compute_aggregate()`](https://sedzinfo.github.io/rwf/reference/compute_aggregate.md)
  : Aggregate descriptive statistics by group
- [`raw_alpha()`](https://sedzinfo.github.io/rwf/reference/raw_alpha.md)
  : Cronbach's alpha (raw)
- [`alpha_diagnostics()`](https://sedzinfo.github.io/rwf/reference/alpha_diagnostics.md)
  : Item-total correlations and alpha-if-item-removed diagnostics
- [`mean_sd_alpha()`](https://sedzinfo.github.io/rwf/reference/mean_sd_alpha.md)
  : Mean and SD of scale scores
- [`report_alpha()`](https://sedzinfo.github.io/rwf/reference/report_alpha.md)
  : Cronbach's alpha reliability report for multiple scales

## Correlation & Covariance

Correlation matrices, power analysis and related utilities

- [`generate_correlation_matrix()`](https://sedzinfo.github.io/rwf/reference/generate_correlation_matrix.md)
  : Generate a data frame with a predetermined correlation structure
- [`simulate_correlation_from_sample()`](https://sedzinfo.github.io/rwf/reference/simulate_correlation_from_sample.md)
  : Simulate data preserving the correlation structure of an input data
  frame
- [`compute_power_r()`](https://sedzinfo.github.io/rwf/reference/compute_power_r.md)
  : Compute r power curve
- [`compute_power_r_matrix()`](https://sedzinfo.github.io/rwf/reference/compute_power_r_matrix.md)
  : Compute correlation matrix
- [`display_upper_lower_triangle()`](https://sedzinfo.github.io/rwf/reference/display_upper_lower_triangle.md)
  : Return upper diagonal from one matrix and lower diagonal from
  another matrix
- [`matrix_triangle()`](https://sedzinfo.github.io/rwf/reference/matrix_triangle.md)
  : Extract the upper or lower triangle of a matrix
- [`symmetric_matrix()`](https://sedzinfo.github.io/rwf/reference/symmetric_matrix.md)
  : Make a symmetric matrix by duplicating one triangle
- [`off_diagonal_index()`](https://sedzinfo.github.io/rwf/reference/off_diagonal_index.md)
  : Get off-diagonal indices for a square matrix
- [`report_correlation()`](https://sedzinfo.github.io/rwf/reference/report_correlation.md)
  : Report correlation matrix
- [`report_choric_serial()`](https://sedzinfo.github.io/rwf/reference/report_choric_serial.md)
  : Report polychoric tetrachoric polyserial biserial correlation
- [`plot_corrplot()`](https://sedzinfo.github.io/rwf/reference/plot_corrplot.md)
  : Correlation matrix plots
- [`plot_mtmm()`](https://sedzinfo.github.io/rwf/reference/plot_mtmm.md)
  : Multitrait-multimethod (MTMM) matrix plot

## Exploratory Factor Analysis

- [`compute_map()`](https://sedzinfo.github.io/rwf/reference/compute_map.md)
  : Simulate prior distribution
- [`report_efa()`](https://sedzinfo.github.io/rwf/reference/report_efa.md)
  : Output EFA model
- [`plot_scree()`](https://sedzinfo.github.io/rwf/reference/plot_scree.md)
  : Scree plot displaying the Kaiser and Jolife criteria for factor
  extraction
- [`plot_loadings()`](https://sedzinfo.github.io/rwf/reference/plot_loadings.md)
  : Plot loadings

## Confirmatory Factor Analysis & SEM

- [`key_to_cfa_model()`](https://sedzinfo.github.io/rwf/reference/key_to_cfa_model.md)
  : Convert a key list to a lavaan CFA model string
- [`model_loadings()`](https://sedzinfo.github.io/rwf/reference/model_loadings.md)
  : Pattern and structure matrix
- [`cfa_icc_index()`](https://sedzinfo.github.io/rwf/reference/cfa_icc_index.md)
  : index of items to convert from lavaan to thurstonian order for
  analysis
- [`icc_cfa()`](https://sedzinfo.github.io/rwf/reference/icc_cfa.md) :
  Select responses for each dimension
- [`check_heywood()`](https://sedzinfo.github.io/rwf/reference/check_heywood.md)
  : Check for Heywood Cases and Related SEM Estimation Problems
- [`extract_components()`](https://sedzinfo.github.io/rwf/reference/extract_components.md)
  : Extract and plot variance components from a mixed model
- [`simulate_cfa_fit()`](https://sedzinfo.github.io/rwf/reference/simulate_cfa_fit.md)
  : Simulate CFA model fit across sample sizes
- [`report_cfa()`](https://sedzinfo.github.io/rwf/reference/report_cfa.md)
  : Report
- [`plot_cfa()`](https://sedzinfo.github.io/rwf/reference/plot_cfa.md) :
  Batch-plot CFA across layouts and display modes
- [`plot_cfa_gg()`](https://sedzinfo.github.io/rwf/reference/plot_cfa_gg.md)
  : Plot CFA model (semPlot-free)

## Item Response Theory

- [`compute_ability()`](https://sedzinfo.github.io/rwf/reference/compute_ability.md)
  : Compute subject ability for thurstonian models
- [`compute_unidimensional_ability()`](https://sedzinfo.github.io/rwf/reference/compute_unidimensional_ability.md)
  : Compute theta for unidimensional models
- [`compute_unidimensional_theta()`](https://sedzinfo.github.io/rwf/reference/compute_unidimensional_theta.md)
  : Compute theta for unidimensional models
- [`compute_se_theta()`](https://sedzinfo.github.io/rwf/reference/compute_se_theta.md)
  : Compute the SE of theta
- [`compute_info_1pl()`](https://sedzinfo.github.io/rwf/reference/compute_info_1pl.md)
  : Compute item information for 1PL model
- [`compute_info_2pl()`](https://sedzinfo.github.io/rwf/reference/compute_info_2pl.md)
  : Compute item information for 2PL model
- [`compute_info_3pl()`](https://sedzinfo.github.io/rwf/reference/compute_info_3pl.md)
  : Compute item information for 3PL model
- [`compute_icc_thurstonian()`](https://sedzinfo.github.io/rwf/reference/compute_icc_thurstonian.md)
  : Compute item characteristic curves for thurstonian models
- [`compute_dissatenuation()`](https://sedzinfo.github.io/rwf/reference/compute_dissatenuation.md)
  : Compute the disattenuation correction for measurement error
- [`report_irt()`](https://sedzinfo.github.io/rwf/reference/report_irt.md)
  : Output for irt model
- [`plot_irt_onefactor()`](https://sedzinfo.github.io/rwf/reference/plot_irt_onefactor.md)
  : Return data for irt plots
- [`plot_icc_thurstonian()`](https://sedzinfo.github.io/rwf/reference/plot_icc_thurstonian.md)
  : Plot thurstonian icc

## Thurstonian IRT / Forced Choice

- [`score_tirt()`](https://sedzinfo.github.io/rwf/reference/score_tirt.md)
  : Score Multiple Thurstonian IRT Response Patterns (MAP / EBM)
- [`score_tirt_pattern()`](https://sedzinfo.github.io/rwf/reference/score_tirt_pattern.md)
  : Score a Single Thurstonian IRT Response Pattern (MAP / EBM)
- [`extract_tirt_params()`](https://sedzinfo.github.io/rwf/reference/extract_tirt_params.md)
  : Extract Thurstonian IRT Parameters from a lavaan-Fitted Model
- [`rank3_to_triplets()`](https://sedzinfo.github.io/rwf/reference/rank3_to_triplets.md)
  : Convert thurstonian binary triplets to scale
- [`rank_df_to_binary()`](https://sedzinfo.github.io/rwf/reference/rank_df_to_binary.md)
  : Convert scale to thurstonian binary with n items per block and n
  blocks
- [`rank_to_binary()`](https://sedzinfo.github.io/rwf/reference/rank_to_binary.md)
  : Convert scale to thurstonian binary with n items per ranking block
- [`generate_comparisons_matrix()`](https://sedzinfo.github.io/rwf/reference/generate_comparisons_matrix.md)
  : Generate comparisons matrix
- [`generate_unique_comparisons_index()`](https://sedzinfo.github.io/rwf/reference/generate_unique_comparisons_index.md)
  : Generate index for unique comparisons
- [`generate_matrix_A()`](https://sedzinfo.github.io/rwf/reference/generate_matrix_A.md)
  : Generate Matrix A
- [`generate_matrix_lambda_hat()`](https://sedzinfo.github.io/rwf/reference/generate_matrix_lambda_hat.md)
  : Generate matrix lambda for spesified number of comparisons
- [`name_triplet_pairs()`](https://sedzinfo.github.io/rwf/reference/name_triplet_pairs.md)
  : Create Pair Labels from Consecutive Triplets of Items
- [`comparison_combinations()`](https://sedzinfo.github.io/rwf/reference/comparison_combinations.md)
  : All pairwise column name combinations

## Regression & Logistic

- [`report_regression()`](https://sedzinfo.github.io/rwf/reference/report_regression.md)
  : Regression
- [`report_hlr()`](https://sedzinfo.github.io/rwf/reference/report_hlr.md)
  : Report HLR
- [`report_logistic()`](https://sedzinfo.github.io/rwf/reference/report_logistic.md)
  : Report logistic regression
- [`output_compare_model_logistic()`](https://sedzinfo.github.io/rwf/reference/output_compare_model_logistic.md)
  : Compare logistic regression models models
- [`compute_y_logistic()`](https://sedzinfo.github.io/rwf/reference/compute_y_logistic.md)
  : Compute y for logistic function
- [`compute_dummy_comparisons()`](https://sedzinfo.github.io/rwf/reference/compute_dummy_comparisons.md)
  : Compute number of dummy comparisons
- [`dummy_arrange()`](https://sedzinfo.github.io/rwf/reference/dummy_arrange.md)
  : Dummy-code a multiple response vector into a binary data frame
- [`plot_logistic_model()`](https://sedzinfo.github.io/rwf/reference/plot_logistic_model.md)
  : Logistic model plot
- [`proportion_accurate()`](https://sedzinfo.github.io/rwf/reference/proportion_accurate.md)
  : Proportion overall accuracy of a confusion matrix
- [`plot_roc()`](https://sedzinfo.github.io/rwf/reference/plot_roc.md) :
  Plot Receiver Operating Characteristic (ROC) curve

## ANOVA & Group Comparisons

- [`compute_one_way_test()`](https://sedzinfo.github.io/rwf/reference/compute_one_way_test.md)
  : one way test
- [`compute_aov_es()`](https://sedzinfo.github.io/rwf/reference/compute_aov_es.md)
  : Compute eta and omega
- [`compute_posthoc()`](https://sedzinfo.github.io/rwf/reference/compute_posthoc.md)
  : Games Howell Tukey post hoc tests
- [`compute_kruskal_wallis_test()`](https://sedzinfo.github.io/rwf/reference/compute_kruskal_wallis_test.md)
  : Kruskal-Wallis Test with Effect Sizes
- [`report_oneway()`](https://sedzinfo.github.io/rwf/reference/report_oneway.md)
  : One way
- [`report_factorial_anova()`](https://sedzinfo.github.io/rwf/reference/report_factorial_anova.md)
  : Plot means with standard error for every level in a dataframe
- [`report_manova()`](https://sedzinfo.github.io/rwf/reference/report_manova.md)
  : Manova result
- [`report_ttests()`](https://sedzinfo.github.io/rwf/reference/report_ttests.md)
  : Run Pairwise t-tests and Return a Reporting Table
- [`report_wtests()`](https://sedzinfo.github.io/rwf/reference/report_wtests.md)
  : Run Pairwise Wilcoxon Tests and Return a Reporting Table
- [`plot_oneway()`](https://sedzinfo.github.io/rwf/reference/plot_oneway.md)
  : Plot group means with error bars for all IV-DV combinations
- [`plot_oneway_diagnostics()`](https://sedzinfo.github.io/rwf/reference/plot_oneway_diagnostics.md)
  : Diagnostic plots for one-way ANOVA models

## Classification & Confusion

- [`confusion()`](https://sedzinfo.github.io/rwf/reference/confusion.md)
  : Create a confusion matrix from observed and predicted vectors
- [`confusion_matrix_percent()`](https://sedzinfo.github.io/rwf/reference/confusion_matrix_percent.md)
  : Confusion matrix with row and column percent
- [`result_confusion_performance()`](https://sedzinfo.github.io/rwf/reference/result_confusion_performance.md)
  : Plot performance of confusion matrix for different cut off points
- [`plot_confusion()`](https://sedzinfo.github.io/rwf/reference/plot_confusion.md)
  : Plot confusion matrix
- [`excel_confusion_matrix()`](https://sedzinfo.github.io/rwf/reference/excel_confusion_matrix.md)
  : Write matrix or dataframe to excel sheet
- [`report_lda()`](https://sedzinfo.github.io/rwf/reference/report_lda.md)
  : Report for MASS::lda
- [`report_xgboost()`](https://sedzinfo.github.io/rwf/reference/report_xgboost.md)
  : Report for xgboost::xgb.train
- [`plot_trees_xgboost()`](https://sedzinfo.github.io/rwf/reference/plot_trees_xgboost.md)
  : Plot trees for xgboost::xgb.train

## Plotting & Visualisation

- [`plot_boxplot()`](https://sedzinfo.github.io/rwf/reference/plot_boxplot.md)
  : Side-by-side boxplots for all numeric columns
- [`plot_histogram()`](https://sedzinfo.github.io/rwf/reference/plot_histogram.md)
  : Histograms per numeric column
- [`plot_scatterplot()`](https://sedzinfo.github.io/rwf/reference/plot_scatterplot.md)
  : Scatter plots for all variable pairs in a data frame
- [`plot_interaction()`](https://sedzinfo.github.io/rwf/reference/plot_interaction.md)
  : Plot two-way interaction graphs for all IV pair and DV combinations
- [`compute_crosstable()`](https://sedzinfo.github.io/rwf/reference/compute_crosstable.md)
  : Pairwise cross-tabulation of categorical variables
- [`compute_tversky_index()`](https://sedzinfo.github.io/rwf/reference/compute_tversky_index.md)
  : Compute the Tversky index
- [`plot_crosstable()`](https://sedzinfo.github.io/rwf/reference/plot_crosstable.md)
  : Bubble plots for pairwise cross-tabulations
- [`plot_mosaic()`](https://sedzinfo.github.io/rwf/reference/plot_mosaic.md)
  : Mosaic plots for pairwise categorical variables
- [`plot_multiplot()`](https://sedzinfo.github.io/rwf/reference/plot_multiplot.md)
  : Arrange multiple ggplot objects in a grid layout
- [`plot_normality_diagnostics()`](https://sedzinfo.github.io/rwf/reference/plot_normality_diagnostics.md)
  : Normality diagnostic plots (histogram, density, boxplot, Q-Q)
- [`plot_response_frequencies()`](https://sedzinfo.github.io/rwf/reference/plot_response_frequencies.md)
  : Horizontal bar charts of response frequencies
- [`plot_separability()`](https://sedzinfo.github.io/rwf/reference/plot_separability.md)
  : Plot separability
- [`plot_outlier()`](https://sedzinfo.github.io/rwf/reference/plot_outlier.md)
  : Dot plot of outliers by detection method
- [`plot_qq()`](https://sedzinfo.github.io/rwf/reference/plot_qq.md) :
  Q-Q plots against the normal distribution
- [`plot_acf()`](https://sedzinfo.github.io/rwf/reference/plot_acf.md) :
  Autocorrelation, autocovariance, and partial autocorrelation plot
- [`plot_ts()`](https://sedzinfo.github.io/rwf/reference/plot_ts.md) :
  Line plot for a time series
- [`duplicate_y_axis()`](https://sedzinfo.github.io/rwf/reference/duplicate_y_axis.md)
  : Duplicate the y axis on the right side of a ggplot
- [`hinvert_title_grob()`](https://sedzinfo.github.io/rwf/reference/hinvert_title_grob.md)
  : Invert a title grob horizontally

## Reporting & Export

- [`report_dataframe()`](https://sedzinfo.github.io/rwf/reference/report_dataframe.md)
  : Write matrix or dataframe to excel sheet
- [`report_normality_tests()`](https://sedzinfo.github.io/rwf/reference/report_normality_tests.md)
  : Battery of normality tests
- [`report_pdf()`](https://sedzinfo.github.io/rwf/reference/report_pdf.md)
  : Save or display a list of plots as a multi-page PDF
- [`output_separator()`](https://sedzinfo.github.io/rwf/reference/output_separator.md)
  : Print a formatted console output block with separators
- [`excel_generic_format()`](https://sedzinfo.github.io/rwf/reference/excel_generic_format.md)
  : Format an Excel worksheet with styles, comments, and frozen panes
- [`excel_critical_value()`](https://sedzinfo.github.io/rwf/reference/excel_critical_value.md)
  : Write a data frame to Excel with per-column conditional formatting
  thresholds
- [`excel_matrix()`](https://sedzinfo.github.io/rwf/reference/excel_matrix.md)
  : Write a matrix or data frame to an Excel worksheet with optional
  conditional formatting
- [`write_txt()`](https://sedzinfo.github.io/rwf/reference/write_txt.md)
  : Print an object and optionally save output to a log file

## Data Manipulation

- [`c_bind()`](https://sedzinfo.github.io/rwf/reference/c_bind.md) :
  Column-bind data frames or vectors of unequal lengths
- [`rbind_all()`](https://sedzinfo.github.io/rwf/reference/rbind_all.md)
  : Row-bind two data frames with different column sets
- [`change_data_type()`](https://sedzinfo.github.io/rwf/reference/change_data_type.md)
  : Convert column data types in a data frame
- [`recode_scale_dummy()`](https://sedzinfo.github.io/rwf/reference/recode_scale_dummy.md)
  : Scale and dummy code
- [`drop_levels()`](https://sedzinfo.github.io/rwf/reference/drop_levels.md)
  : Drop unused factor levels and collapse rare levels into "Other"
- [`trim_df()`](https://sedzinfo.github.io/rwf/reference/trim_df.md) :
  Trim whitespace from all character cells in a data frame
- [`round_dataframe()`](https://sedzinfo.github.io/rwf/reference/round_dataframe.md)
  : Round numeric columns in a data frame
- [`padNA()`](https://sedzinfo.github.io/rwf/reference/padNA.md) : Pad a
  data frame to a target number of rows with NAs
- [`replace_na_with_previous()`](https://sedzinfo.github.io/rwf/reference/replace_na_with_previous.md)
  : Last observation carried forward (LOCF) imputation
- [`remove_outliers()`](https://sedzinfo.github.io/rwf/reference/remove_outliers.md)
  : Replace outliers with NA using IQR fences
- [`outlier_summary()`](https://sedzinfo.github.io/rwf/reference/outlier_summary.md)
  : Percentage of outliers at three z-score thresholds
- [`k_fold()`](https://sedzinfo.github.io/rwf/reference/k_fold.md) :
  K-Fold train test sampling
- [`k_sample()`](https://sedzinfo.github.io/rwf/reference/k_sample.md) :
  Train test sampling
- [`generate_missing()`](https://sedzinfo.github.io/rwf/reference/generate_missing.md)
  : Introduce missing values into a vector or data frame
- [`generate_data()`](https://sedzinfo.github.io/rwf/reference/generate_data.md)
  : Generate a data frame of random numbers
- [`generate_factor()`](https://sedzinfo.github.io/rwf/reference/generate_factor.md)
  : Generate a data frame of random factor vectors
- [`generate_string()`](https://sedzinfo.github.io/rwf/reference/generate_string.md)
  : Generate random strings
- [`generate_multiple_responce_vector()`](https://sedzinfo.github.io/rwf/reference/generate_multiple_responce_vector.md)
  : Generate a multiple response vector
- [`data_frame_index()`](https://sedzinfo.github.io/rwf/reference/data_frame_index.md)
  : dataframe index
- [`increase_index()`](https://sedzinfo.github.io/rwf/reference/increase_index.md)
  : index dataframe picks
- [`min_max_index()`](https://sedzinfo.github.io/rwf/reference/min_max_index.md)
  : Indices of the minimum and maximum values in a vector
- [`flatten_list()`](https://sedzinfo.github.io/rwf/reference/flatten_list.md)
  : Flatten a two-dimensional list into a data frame
- [`swap()`](https://sedzinfo.github.io/rwf/reference/swap.md) :
  Reverse-score a numeric vector

## String Utilities

- [`str_count()`](https://sedzinfo.github.io/rwf/reference/str_count.md)
  : Count the number of pattern matches in a string
- [`str_pad()`](https://sedzinfo.github.io/rwf/reference/str_pad.md) :
  Pad a string to a minimum width
- [`str_replace()`](https://sedzinfo.github.io/rwf/reference/str_replace.md)
  : Replace the first pattern match in a string
- [`str_replace_all()`](https://sedzinfo.github.io/rwf/reference/str_replace_all.md)
  : Replace all pattern matches in a string
- [`str_split_fixed()`](https://sedzinfo.github.io/rwf/reference/str_split_fixed.md)
  : Split strings into a fixed-width matrix of pieces
- [`str_squish()`](https://sedzinfo.github.io/rwf/reference/str_squish.md)
  : Remove leading, trailing, and internal extra whitespace
- [`str_wrap()`](https://sedzinfo.github.io/rwf/reference/str_wrap.md) :
  Wrap long strings to a specified line width
- [`sub_str()`](https://sedzinfo.github.io/rwf/reference/sub_str.md) :
  Extract n characters from the left or right of a string
- [`split_str()`](https://sedzinfo.github.io/rwf/reference/split_str.md)
  : Split a string vector into a data frame of parts
- [`split_str_df()`](https://sedzinfo.github.io/rwf/reference/split_str_df.md)
  : Split a string column or row names in a data frame into separate
  columns
- [`mgsub()`](https://sedzinfo.github.io/rwf/reference/mgsub.md) : Apply
  gsub for multiple patterns with a single replacement
- [`stat_word_char()`](https://sedzinfo.github.io/rwf/reference/stat_word_char.md)
  : Text similarity measures
- [`text_similarity()`](https://sedzinfo.github.io/rwf/reference/text_similarity.md)
  : Text similarity measures
- [`clear_text()`](https://sedzinfo.github.io/rwf/reference/clear_text.md)
  : Clear text
- [`clear_stopwords()`](https://sedzinfo.github.io/rwf/reference/clear_stopwords.md)
  : Remove stopwods
- [`proper()`](https://sedzinfo.github.io/rwf/reference/proper.md) :
  Convert a string to proper case
- [`remove_nc()`](https://sedzinfo.github.io/rwf/reference/remove_nc.md)
  : Replace and remove non-computable values
- [`string_aes()`](https://sedzinfo.github.io/rwf/reference/string_aes.md)
  : Clean and format string aesthetics
- [`tag_pos()`](https://sedzinfo.github.io/rwf/reference/tag_pos.md) :
  Part of speech tagging
- [`wrapper()`](https://sedzinfo.github.io/rwf/reference/wrapper.md) :
  Wrap a string to a specified width

## Utilities & Environment

- [`install_load()`](https://sedzinfo.github.io/rwf/reference/install_load.md)
  : Install and load multiple packages
- [`install_all_packages()`](https://sedzinfo.github.io/rwf/reference/install_all_packages.md)
  : Install all missing CRAN packages
- [`detach_package()`](https://sedzinfo.github.io/rwf/reference/detach_package.md)
  : Detach and unload a package
- [`remove_user_packages()`](https://sedzinfo.github.io/rwf/reference/remove_user_packages.md)
  : Remove all user-installed packages
- [`environment_options()`](https://sedzinfo.github.io/rwf/reference/environment_options.md)
  : Load environment options
- [`get_script_directory()`](https://sedzinfo.github.io/rwf/reference/get_script_directory.md)
  : Get script directory
- [`getfwp()`](https://sedzinfo.github.io/rwf/reference/getfwp.md) : Get
  the file path of the currently running script
- [`call_to_string()`](https://sedzinfo.github.io/rwf/reference/call_to_string.md)
  : Convert a model call to a compact string
- [`dotnames()`](https://sedzinfo.github.io/rwf/reference/dotnames.md) :
  Pad a data frame to a target number of rows with NAs
- [`fixed()`](https://sedzinfo.github.io/rwf/reference/fixed.md) : Mark
  a pattern as a fixed string
- [`deg2rad()`](https://sedzinfo.github.io/rwf/reference/deg2rad.md) :
  Convert degrees to radians
- [`rad2deg()`](https://sedzinfo.github.io/rwf/reference/rad2deg.md) :
  Convert radians to degrees
- [`shrout()`](https://sedzinfo.github.io/rwf/reference/shrout.md) :
  Shrout-Fleiss reliability coefficients
- [`compute_solve()`](https://sedzinfo.github.io/rwf/reference/compute_solve.md)
  : Solve Linear Systems or Invert a Matrix (Gauss-Jordan)
- [`compute_adjustment()`](https://sedzinfo.github.io/rwf/reference/compute_adjustment.md)
  : Compute multiple comparison alpha adjustments
- [`compute_moving_average()`](https://sedzinfo.github.io/rwf/reference/compute_moving_average.md)
  : Centered moving average
- [`compute_residual_stats()`](https://sedzinfo.github.io/rwf/reference/compute_residual_stats.md)
  : Residuals for matrices
- [`ts_smoothing()`](https://sedzinfo.github.io/rwf/reference/ts_smoothing.md)
  : Time series smoothing with multiple bandwidth levels
- [`decompose_datetime()`](https://sedzinfo.github.io/rwf/reference/decompose_datetime.md)
  : Decompose datetime objects to dataframe collumns
- [`convert_excel_unix_timestamp()`](https://sedzinfo.github.io/rwf/reference/convert_excel_unix_timestamp.md)
  : Convert UNIX EXCEL timestamp
- [`questions_by_keys()`](https://sedzinfo.github.io/rwf/reference/questions_by_keys.md)
  : Convert a key vector to a list of question indices by dimension
- [`questions_dimensions_dataframe()`](https://sedzinfo.github.io/rwf/reference/questions_dimensions_dataframe.md)
  : Build a question-to-dimension mapping table
- [`response_dimension()`](https://sedzinfo.github.io/rwf/reference/response_dimension.md)
  : index parameter and items relative to their dimensions
- [`response_frequency()`](https://sedzinfo.github.io/rwf/reference/response_frequency.md)
  : Response frequency table for ordinal or Likert-scale variables

## Datasets

Example datasets bundled with the package

- [`df_admission`](https://sedzinfo.github.io/rwf/reference/df_admission.md)
  : Admission Data
- [`df_automotive_data`](https://sedzinfo.github.io/rwf/reference/df_automotive_data.md)
  : Automotive Data
- [`df_blood_pressure`](https://sedzinfo.github.io/rwf/reference/df_blood_pressure.md)
  : Blood Pressure Data
- [`df_co2`](https://sedzinfo.github.io/rwf/reference/df_co2.md) :
  Carbon Dioxide Uptake in Grass Plants
- [`df_crop_yield`](https://sedzinfo.github.io/rwf/reference/df_crop_yield.md)
  : Crop Yield Data
- [`df_difficile`](https://sedzinfo.github.io/rwf/reference/df_difficile.md)
  : Difficile Data
- [`df_insurance`](https://sedzinfo.github.io/rwf/reference/df_insurance.md)
  : Insurance Data
- [`df_ocean`](https://sedzinfo.github.io/rwf/reference/df_ocean.md) :
  Big Five Personality Test Dataset
- [`df_personality`](https://sedzinfo.github.io/rwf/reference/df_personality.md)
  : Big Five Inventory (BFI-44) Personality Dataset
- [`df_responses`](https://sedzinfo.github.io/rwf/reference/df_responses.md)
  : Young People Survey Responses
- [`df_responses_state`](https://sedzinfo.github.io/rwf/reference/df_responses_state.md)
  : Responses State Data
- [`df_sexual_comp`](https://sedzinfo.github.io/rwf/reference/df_sexual_comp.md)
  : Sexual Compatibility Data
- [`df_titanic`](https://sedzinfo.github.io/rwf/reference/df_titanic.md)
  : Titanic Dataset
