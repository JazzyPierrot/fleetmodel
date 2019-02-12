#' Check the validity of a vector of parameters
#'
#' A vector of parameters has exactly five parameters.
#'
#' @param params vector of parameters to check.
#'
#' @return NULL
#' @export
#'
#' @examples
check_params_and_distribs <- function(params, distribs){

  assertthat::assert_that(length(distribs) == 2)
  assertthat::assert_that(
    all(distribs %in% c("weibull", "gamma", "lnorm")),
    msg= 'Wrong distribution name. Only "weibull", "gamma" and "lnorm" are allowed')


  assertthat::assert_that(
    params %>% inherits("list"),
    msg= "Wrong parameters format. Parameters must be given as a list of lists"
  )

  assertthat::assert_that(length(params) == 2)
  assertthat::assert_that(
    params[[1]] %>% inherits("list") && params[[2]] %>% inherits("list"),
    msg = "Wrong parameters format. Parameters must be given as a list of lists"
  )

  assertthat::assert_that(length(params[[1]] == 2) && length(params[[2]] == 2))

  aux_check_specific_distribution <- function(distribution, params){
    all(names(params) %in% get_parameter_names(distribution))
  }
}


generate_rdm_distribution_p <- function(
  copula_param,
  marginal_params,
  marginal_distribs,
  n){

  set.seed(12345)
  clayton_cop <- copula::claytonCopula(param = copula_param, dim = 2)

  multivariate_dist <- copula::mvdc(
    clayton_cop,
    margins = c(marginal_distribs[1], marginal_distribs[2]),
    paramMargins = marginal_params
  )

  p <- copula::rMvdc(n, multivariate_dist)

  return(p)

}

#' Get the parameter names of a given distribution
#'
#' @param distribution
#'
#' @return Vector of parameter names
#' @export
#'
#' @examples
get_parameter_names <- function(distribution){
    switch(distribution,
      "weibull" = {
        c("shape", "scale")
      },
      "gamma" = {
        c("shape", "rate")
      },
      "lnorm" = {
        c("meanlog", "sdlog")
      })
}
