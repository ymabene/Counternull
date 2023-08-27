
test_that("assess counternull values", {
  skip_on_cran()

  y = sample_data$turn_angle
  w = sample_data$w
  n_r = create_null_rand(y, w, sample_matrix,
  test_stat = c("diffmeans"))
  expected = find_counternull_values(n_r)

  expect_s3_class(expected, "counternull")
  expect_named(expected, c("counternull_perm", "low", "high",
                           "counternull_perm_two",
                           "low_two", "high_two", "null_r", "bw"))

  expect_error(find_counternull_values(y))

  t_r = n_r
  t_r$rand_matrix = n_r$rand_matrix[1:7,]
  expect_error(find_counternull_values(t_r))

  expect_error(find_counternull_values(y, 2))

  expect_error(find_counternull_values(n_r, bw= -10))
  expect_error(find_counternull_values(n_r, counts = 10))
  expect_error(find_counternull_values(n_r, counts = c(20,10)))
  expect_error(find_counternull_values(n_r, counts = c("a", "b")))

  expect_error(find_counternull_values(t_r, counts = c(1,10000)))
  expect_error(find_counternull_values(t_r, counts = c(-10000,1000)))

  expect_error(find_counternull_values(y, c(3,2)))

  expected_two = find_counternull_values(n_r, c(56,56))

  expect_equal(expected, expected_two)

  n_r = create_null_rand(y,w, sample_matrix[,1:100],
                         test_stat = c("diffmeans"), alternative = c("less"))
  expect_warning(find_counternull_values(n_r , c(1,15)))

  func = function(x,y){
    return(6)
  }

  n_r = create_null_rand(y, w, sample_matrix,
                         fun = func)
  t_l = create_null_rand(y, w, sample_matrix,
                         fun = func, alternative = "less")
  t_r = create_null_rand(y, w, sample_matrix,
                         fun = func, alternative = "greater")
  expected_four = find_counternull_values(n_r , c(0,0))
  expected_five = find_counternull_values(n_r , c(0,0))
  expected_six = find_counternull_values(n_r , c(0,0))
  expect_message(summary(expected_four), "No counternull values found.")
  expect_message(plot(expected_four), "No counternull values found.")
  expect_message(summary(expected_five), "No counternull values found.")
  expect_message(plot(expected_five), "No counternull values found.")
  expect_message(summary(expected_six), "No counternull values found.")
  expect_message(plot(expected_six), "No counternull values found.")


})

