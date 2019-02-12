#' Negloglikelihood with unique parameter
#'
#' Auxiliary function to be used with optim
#'
#' @param x vector of 5 parameters, first a clayton copula, then the marginal
#' distribution parameters (see description for order)
#' @param marginal_distribs List of marginal distributions
#'
#' @section Order of parameters:
#' Parameters are given in the same order than expected in the {stats}
#' functions:
#' * Weibull: first shape, then scale
#' * Gamma: first shape, then rate
#' * lnorm: first meanlog, then sdlog
#'
#' @return
#' @export
#'
#' @examples
aux_fun_to_optimize <- function(x, marginal_distribs, fleet){
  copula_param <- x[1]

  marginal_params <- list(as.list(x[2:3]), as.list(x[4:5]))
  names(marginal_params[[1]]) <- get_parameter_names(marginal_distribs[[1]])
  names(marginal_params[[2]]) <- get_parameter_names(marginal_distribs[[2]])

  return(compute_negloglikelihood(
      fleet, copula_param, marginal_params, marginal_distribs)
    )
}


#' Optimize the neg-loglikelihood of the model for a given fleet
#'
#' @param marginal_distribs Marginal distribution of the model to optimize.
#'   Vector of length 2.
#' @param fleet Fleet to optimize on (List of vectors)
#'
#' @return
#' @export
#'
#' @examples
optimize_NLL <- function(marginal_distribs, fleet){

  res  <- optim(
    par = c(1, 1, 1, 3, 4.5),
    fn = aux_fun_to_optimize,
    gr = NULL,
    marginal_distribs = marginal_distribs,
    fleet = fleet,
    method = "L-BFGS-B",
    lower = c(0.5, 0.5, 0.5, 2.5, 4),
    upper = c(2, 2, 2, 4.5, 6),
    control = list(
      trace = 3, # Verbosity
      parscale = c(1.2, 1.5, 1, 3, 5), # Scale parameters
      maxit = 1000, # Maximum number of iterations
      reltol = 1e-8
      )
    )
  return(res)
}
