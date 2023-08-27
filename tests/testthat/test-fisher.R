

test_that("assess fisher ",{
  skip_on_cran()
  y = sample_data$turn_angle
  w = sample_data$w
  n_r = create_null_rand(y,w, sample_matrix, test_stat = c("diffmeans"))
  expected = create_fisher_interval(n_r)
  expect_s3_class(expected, "fisher_interval")
  expect_named(expected, c("lower_bound", "upper_bound", "alpha",
                           "pvalue_lower", "pvalue_upper", "range",
                           "null_r"))

  fun = function (x,y){
    return(invisible(6))
  }

  fun_two = function (x,y){
    d = mean(x) - mean(y)
    if(d < 3){
      return(invisible())
    }
  }
  t_r = create_null_rand(y,w, sample_matrix, fun = fun)
  expect_warning(create_fisher_interval(t_r))
  expect_error(create_fisher_interval(n_r, -1))

  expect_error(create_fisher_interval(20))

  t_r = n_r
  t_r$alternative = "less"

  c = find_counternull_values(t_r, c(25,25))
  expect_equal(c$low, expected$lower_bound)

  c_r = find_counternull_values(t_r, c(975,975))
  expect_equal(c_r$high, expected$upper_bound)

})
