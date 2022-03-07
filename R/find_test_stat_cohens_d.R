#' Finds Cohen's D (observed test statistic) for data set
#'
#' Finds Cohen's D test statistic for experimental
#' (exposed) and control (non exposed) group for measured outcome in dataset
#'
#' @param sample_data Sample data set. Data should have first column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' @param variable Variable measured
#' Format: sample_data$column
#' @examples
#' find_test_stat_cohens_d(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre)
#'
#' find_test_stat_cohens_d(sample_district_1D,
#' sample_district_1D$charge_prosecuted_1000_rate_post -
#' sample_district_1D$charge_prosecuted_1000_rate_pre)
#' @return Observed Test Statistic (Numeric)
#' @export
# calculate test statistic (Cohen's D)
find_test_stat_cohens_d<-function(sample_data,variable){
  cohen_d <- cohen.d((variable)[sample_data[,1]=="1"],
                     (variable)[sample_data[,1]=="0"])
  return(invisible(cohen_d$estimate))
}

