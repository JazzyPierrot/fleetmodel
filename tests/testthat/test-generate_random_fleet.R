context("Tests of reproductibility")

copula_param <- 1.2
marginal_params <- list(list(meanlog = 1.5, sdlog = 1),
  list(shape = 3, scale = 5))
marginal_distribs <- c("lnorm", "weibull")

test_that("Random fleet works as expected", {
  fleet <- generate_random_fleet(
    copula_param,
    marginal_params,
    marginal_distribs,
    n_agents = 1,
    n_obs = 2
  )
  expect_equal(
    fleet,
    data.frame(ind = as.character(rep(1, 2)), ddd = rep(6, 2), stringsAsFactors = FALSE)
  )
})

test_that("Random fleet is reproductible", {
  expect_equal(
    generate_random_fleet(
      copula_param,
      marginal_params,
      marginal_distribs,
      n_agents = 10,
      n_obs = 10
    ),
    generate_random_fleet(
      copula_param,
      marginal_params,
      marginal_distribs,
      n_agents = 10,
      n_obs = 10
    ))
  })

