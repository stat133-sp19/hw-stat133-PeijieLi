context("Check binomial arguments")

test_that("check_prob with ok arguments", {
  expect_true(check_prob(1))
  expect_true(check_prob(0.4))
  expect_true(check_prob(0.112))
})

test_that("check_prob with bad arguments",{
  expect_error(check_prob(-0.3))
  expect_error(check_prob(1.34))
  expect_error(check_prob("notAnumber"))
  expect_error(check_prob(c(0.4, 0.5)))
})

test_that("check_trials with ok arguments", {
  expect_true(check_trials(5))
  expect_true(check_trials(0))
  expect_true(check_trials(10))
})

test_that("check_trials with bad arguments", {
  expect_error(check_trials(-2))
  expect_error(check_trials(5.3))
  expect_error(check_trials("what"))
})

test_that("check_success with ok arguments", {
  expect_true(check_success(1, 5))
  expect_true(check_success(c(1,2,3), 5))
  expect_true(check_success(0, 1))
  expect_true(check_success(1, 1))
})

test_that("check_success with bad arguments", {
  expect_error(check_success(-1 , 5))
  expect_error(check_success(1, c(5, 6)))
  expect_error(check_success(3, 1))
})

# ------------------------------------ Summary Measures ---------
test_that("aux_mean with ok arguments", {
  expect_true(aux_mean(10, 0.3) == 3)
  expect_true(aux_mean(20, 0.9) == 20*0.9)
  expect_true(aux_mean(0, 0.1) == 0)
})

test_that("aux_variance with ok arguments", {
  expect_true(aux_variance(10, 0.3) == 10*0.3*0.7)
  expect_true(aux_variance(10, 0) == 0)
  expect_true(aux_variance(0, 0.1) == 0)
})

test_that("aux_mode with ok arguments", {
  expect_true(aux_mode(10, 0.3) == 3)
  expect_true(aux_mode(20, 0.9) == 18)
  expect_true(aux_mode(0, 0.1) == 0)
})

test_that("aux_skewness with ok arguments", {
  expect_true((aux_skewness(10, 0.3) * 100) %/% 1 == 27)
  expect_true((aux_skewness(20, 0.9) * 100) %/% 1 == -60)
  expect_true((aux_skewness(1, 0.1) * 100) %/% 1 == 266)
})

test_that("aux_kurtosis with ok arguments", {
  expect_true((aux_kurtosis(10, 0.3) * 100) %/% 1 == -13)
  expect_true((aux_kurtosis(20, 0.9) * 100) %/% 1 == 25)
  expect_true((aux_kurtosis(1, 0.1) * 100) %/% 1 == 511)
})
# ------------------------------------ Binomial ---------
test_that("bin_choose with ok arguments", {
  expect_true(bin_choose(n=5, k = 2) == 10)
  expect_true(bin_choose(5, 0) == 1)
  expect_true(length(bin_choose(5, 1:3)) == 3)
})

test_that("bin_choose with bad arguments", {
  expect_error(bin_choose(3, 5))
  expect_error(bin_choose(5:10, 3))
})

test_that("bin_probability with ok arguments", {
  expect_true(bin_probability(success = 2, trials = 5, prob = 0.5) == 0.3125)
  expect_true(bin_probability(success = 0, trials = 5, prob = 0.5) == 0.03125)
  expect_true(bin_probability(success = 1, trials = 5, prob = 0.5) == 0.15625)
  expect_true((10000*bin_probability(success = 55, trials = 100, prob = 0.45))%/%1 == 107)
})

test_that("bin_probability with bad arguments", {
  expect_error(bin_probability(2, 5, -0.1))
  expect_error(bin_probability(2, 5, 1.2))
  expect_error(bin_probability(2, 5, c(0.4, 0.5)))
  expect_error(bin_probability(5, 2, 0.2))
})

test_that("bin_distribution with ok arguments", {
  dis1 <- bin_distribution(trials = 5, prob = 0.5)
  expect_true(nrow(dis1) == 6)
  probs <- dis1$probability
  expect_true(probs[1] == 0.03125)
  expect_true(probs[2] == 0.15625)
  expect_true(probs[3] == 0.31250)
  expect_true(probs[4] == 0.31250)
  expect_true(probs[5] == 0.15625)
  expect_true(probs[6] == 0.03125)
})

test_that("bin_distribution with bad arguments", {
  expect_error(bin_distribution(trials = 3, prob = 1.1))
  expect_error(bin_distribution(trials = -1, prob = 0.1))
  expect_error(bin_distribution(trials = 3, prob = -0.5))
})
test_that("bin_cumulative with ok arguments", {
  dis1 <- bin_cumulative(trials = 5, prob = 0.5)
  expect_true(nrow(dis1) == 6)
  probs <- dis1$cumulative
  expect_true(probs[1] == 0.03125)
  expect_true(probs[2] == 0.18750)
  expect_true(probs[3] == 0.50000)
  expect_true(probs[4] == 0.81250)
  expect_true(probs[5] == 0.96875)
  expect_true(probs[6] == 1.00000)
})

test_that("bin_cumulative with bad arguments", {
  expect_error(bin_cumulative(trials = 5, prob = 1.5))
  expect_error(bin_cumulative(trials = 5, prob = -0.5))
  expect_error(bin_cumulative(trials = -5, prob = 0.5))
})


