% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_null_distribution.R
\name{create_null_distribution}
\alias{create_null_distribution}
\title{Creates and plots a null distribution for given test statistic}
\usage{
create_null_distribution(
  sample_data,
  extreme,
  rand_matrix,
  permutation_null_function,
  test_stat,
  variable,
  iterations
)
}
\arguments{
\item{sample_data}{Sample data set. Data should have first column indicating
exposure (1) or non exposure (0) for each group (row) that is measured. Each
measured outcome (variable) should be represented by an additional column.
Data must be ordered so that paired units are in subsequent rows.}

\item{extreme}{Direction which is defined as more extreme for test statistic
in distribution (0 for less or 1 for greater)}

\item{rand_matrix}{Matrix with unique randomizations for exposure
assignment}

\item{permutation_null_function}{Function used to create permutation
vector (Vector of tests statistics created from sampling that are used as
datapoints for null distribution. Test statistic much match observed
test statistic.)}

\item{test_stat}{Observed test statistic.}

\item{variable}{Variable measured for test statistic
(Format: sample_data$column)}

\item{iterations}{Number of unique arrangements of exposure assignments
used to generate distribution (At most the number of rows in rand_matrix)}
}
\value{
Vector with all generated test statistics in null distribution
}
\description{
Resamples data to create and plot null distribution. Calculates and prints
observed test statistic and p-value. Returns vector with null distribution
data points (test statistics created from resampling). Observed test
statistic is indicated in null distribution using dashed black line.
}
\examples{
create_null_distribution(sample_district_1DS,0,rand_matrix_1DS,
permutation_null_diff_means,
find_test_stat_diff_means(sample_district_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre),
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128)

create_null_distribution(sample_district_1DS,0,rand_matrix_1DS,
permutation_null_cohens_d,
find_test_stat_cohens_d(sample_district_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre),
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128)

create_null_distribution(sample_district_1DS,0,rand_matrix_1DS,
permutation_null_t,
find_test_stat_t(sample_district_1DS,
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre),
sample_district_1DS$charge_prosecuted_1000_rate_post -
sample_district_1DS$charge_prosecuted_1000_rate_pre,128)

create_null_distribution(sample_district_1D,0,rand_matrix_1D,
permutation_null_diff_means,
find_test_stat_diff_means(sample_district_1D,
sample_district_1D$charge_prosecuted_1000_rate_post -
sample_district_1D$charge_prosecuted_1000_rate_pre),
sample_district_1D$charge_prosecuted_1000_rate_post -
sample_district_1D$charge_prosecuted_1000_rate_pre,10000)
}
