


test_that("assess adjust pvalues", {
  skip_on_cran()
  y = sample_data$turn_angle
  w = sample_data$w
  n_one = create_null_rand(y, w, sample_matrix, test_stat = c("t"))

  fun = function(x,y){
    return(invisible(ks.test(x,y)$statistic))

  }

  n_two = create_null_rand(y, w, sample_matrix, fun = fun,
                           alternative = c("less"))

  n_four = create_null_rand(y, w, sample_matrix, test_stat = c("t"),
                            alternative = c("greater"))


  n_three = n_two
  n_three$alternative = "nothing"

  ls = list(n_one,n_two)
  expected = adjust_pvalues(ls)

  ls_two = list(n_one, n_two, n_three)
  ls_three = list(n_one, n_two, n_four)


  expect_equal(length(expected), length(ls))
  expect_true(is.vector(expected))
  expect_error(adjust_pvalues(list(n_one)))
  expect_error(adjust_pvalues(n_one, n_two))
  expect_error(adjust_pvalues(ls_two))
  expect_equal(sort(adjust_pvalues(ls)),
               sort(adjust_pvalues(list(n_two, n_one))))
  expect_equal(sort(adjust_pvalues(ls_three)),
               sort(adjust_pvalues(list(n_two, n_one, n_four))))

})
