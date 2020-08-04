#' Generate a random fleet
#'
#' Generates a random fleet of vehicles after the model's statistical
#' distribution
#'
#' @param params Named vector of parameters
#' @param n_agents Number of agents
#' @param n_obs Number of observations per agent
#'
#' @return Returns a list of vectors. Each vector represents one individual,
#'   each element of the vector represents a DVKT.
#'
#' @export
#'
#' @examples
#' copula_param <- 1.2
#' marginal_params <- list(list(meanlog = 0, sdlog = 1), list(shape = 3, scale = 5))
#' marginal_distribs <- c("lnorm", "weibull")
#' generate_random_fleet(copula_param, marginal_params, marginal_distribs, 100)
#'
generate_random_fleet <- function(
  copula_param,
  marginal_params,
  marginal_distribs,
  n_agents,
  n_obs,
  seed = 123
) {

  set.seed(seed)

  check_params_and_distribs(marginal_params, marginal_distribs)

  p <- generate_rdm_distribution_p(
    copula_param,
    marginal_params,
    marginal_distribs,
    n_agents
  )

  assertthat::assert_that(all(p[, 1] > 0))
  lambda <- p[, 2] / gamma(1 + 1 / p[, 1]) #scale
  k <- p[, 1] #shape

  fleet <- purrr::map2_dfr(
    .x = k,
    .y = lambda,
    .f = ~ data.frame(ddd = round(rweibull(shape = .x, scale = .y, n = n_obs))),
    .id = "ind"
  )

  return(fleet)
}
