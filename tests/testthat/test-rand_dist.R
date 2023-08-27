
test_that("assess randomization matrix", {
  skip_on_cran()
  expected = create_randomization_matrix(14,128, rep(1:7, each = 2))
  expect_true(is.matrix(expected))
  expect_equal(nrow(expected), 14)
  expect_error(create_randomization_matrix(14,128, rep(1:7, each = 3)))
  expect_error(create_randomization_matrix(6,8, c('a','b','a','b','a','b')))
  expect_error(create_randomization_matrix("a", 100,rep(1:7, each = 2)))
  expect_error(create_randomization_matrix(6, "a",rep(1:3, each = 2)))
})



test_that("assess find_test_stat",{
  skip_on_cran()
  expected = find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0,0),
                            test_stat = "diffmeans")

  expect_equal(expected, 2)

  expect_true(is.numeric(expected))
  expect_error(find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0,0)))
  expect_error(find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0)))
  expect_error(find_test_stat(c("a", "b"), c(1,1,1,0,0,0)))
  expect_error(find_test_stat(c(2,4,6,1,2,3), c("a")))
  expect_error(find_test_stat(c(2,4,6,1,2,3), c(1,1,1,-1,-1,-1),
                              test_stat = "paired-t"))
  expected = find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0,0),
                            test_stat = "paired-t")
  expect_true(length(expected) == 1)

  func_one = function(x,y){
    return(TRUE)
  }

  func_two = function(x){
    return(3)
  }

  expect_error(find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0,0),
                              fun = func_one))

  expect_error(find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0,0),
                              fun = func_two))

  expect_error(find_test_stat(c(2,4,6,1,2,3), c(1,1,1,0,0,0),
                              test_stat = "ks-test"))

})

test_that("check null distribution",{
  skip_on_cran()

  y = sample_data$turn_angle
  w = sample_data$w
  expected = create_null_rand(y, w, sample_matrix, test_stat = c("t"),
  alternative = c("greater"))
  expect_s3_class(expected, "null_rand")
  expected = create_null_rand(y, w, sample_matrix, test_stat = c("cohens-d"),
                              alternative = c("greater"))
  expect_named(expected, c("null_dist","t_obs","counts","pvalue","alternative",
                           "rand_matrix","bin_width", "y", "w",
                           "test_stat", "fun"))

  expect_error(create_null_rand(y, c(1,1,1,0,0,0), sample_matrix,
                                test_stat = c("t"),alternative = c("less")))

  expect_error(create_null_rand(y, w, c(1,0), test_stat = c("t"),
               alternative = c("two-sided")))

  expect_error(create_null_rand(y, w, sample_matrix, test_stat = c("t"),
                                alternative = "bigger"))

  expect_error(create_null_rand(y,w, sample_matrix))

  expect_error(create_null_rand(y, w, sample_matrix[1:12,], test_stat = c("t"),
                                alternative = c("less")))

  expect_error(create_null_rand(c("a","b","c"), w, sample_matrix, test_stat = c("t"),
                   alternative = c("greater")))

  expect_error(create_null_rand(y, c("a","b","c"), sample_matrix, test_stat = c("t"),
                   alternative = c("greater")))

  expect_error(create_null_rand(y, w, sample_matrix, test_stat = c("t"),
                   alternative = c("greater"), bw= -100))

  y = c(2,4,6,1,2,3)
  w = c(1,1,1,0,0,0)
  r = create_randomization_matrix(6,20)
  func_three = function(x,y){
    if(mean(x) - mean(y) == 2){
      return(2)
    }

    return(runif(1))

  }


  expected = create_null_rand(y,w,r,fun = func_three,
                              alternative = c("less"))

  expect_equal(expected$counts, 20)

  func_four = function(x,y){
    return(invisible(3))
  }

  expected = create_null_rand(y,w,r,fun = func_four,
                              alternative = c("less"))
  expect_equal(expected$pvalue, 1)

})
