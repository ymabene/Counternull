#' Finds T statistic (observed test statistic) for data set
#'
#' Finds T statistic between experimental
#' (exposed) and control (non exposed) group for measured outcome in dataset
#'
#' @param sample_data Sample data set. Data should have first column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' For paired randomization, data must be ordered so that paired units are in
#' subsequent rows.
#' @param variable Variable measured
#' (Format: sample_data$column)
#' @examples
#' find_test_stat_t(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre)
#'
#' find_test_stat_t(sample_district_1D,
#' sample_district_1D$charge_prosecuted_1000_rate_post -
#' sample_district_1D$charge_prosecuted_1000_rate_pre)
#' @return Observed Test Statistic (Numeric)
#' @export
# calculate test statistic (t test)
find_test_stat_t<-function(sample_data,variable){
  test_stat <- t.test((variable)[sample_data[,1]=="1"],
                        y = (variable)[sample_data[,1]=="0"],
                        alternative = c("less"),
                        mu = 0,
                        conf.level = .95)

  return(invisible(test_stat$statistic))
}
