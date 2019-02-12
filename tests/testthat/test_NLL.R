context("Tests of reproductibility")

copula_param <- 1.2
marginal_params <- list(list(meanlog = 1.5, sdlog = 1),
  list(shape = 3, scale = 5))
marginal_distribs <- c("lnorm", "weibull")
fleet <- generate_random_fleet(copula_param, marginal_params,
  marginal_distribs, 10, 10)

test_that("Random fleet is reproductible", {
  expect_equal(generate_random_fleet(copula_param, marginal_params,
      marginal_distribs, 10, 10),
    generate_random_fleet(copula_param, marginal_params,
      marginal_distribs, 10, 10))
  })

test_that("Computation of negloglikelihood is reproductible", {
  expect_equal(
    compute_negloglikelihood(fleet, 1.2, marginal_params, marginal_distribs),
    compute_negloglikelihood(fleet, 1.2, marginal_params, marginal_distribs))
})

test_that("Auxiliary function is working", {
  expect_equal(
    compute_negloglikelihood(fleet, 1.2, marginal_params, marginal_distribs),
    aux_fun_to_optimize(c(1.2, 1.5, 1, 3, 5), marginal_distribs, fleet)
    )
})
