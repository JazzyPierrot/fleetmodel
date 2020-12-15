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

#' Compute the negative log-likelihood
#'
#' @param fleet List of vectors. Each agent is represented by a vector of DVKTs.
#' @param copula_param Clayton copula parameter
#' @param marginal_params List of list of Parameters of marginal distributions of p
#' @param marginal_distribs List of Marginal distributions (lnorm, weibull or gamma)
#' @param n_montecarlo number of monte-carlo simulations
#'
#' @return
#' @export
#'
#' @examples
compute_negloglikelihood <- function(
  fleet,
  copula_param,
  marginal_params,
  marginal_distribs,
  n_montecarlo = 1000,
  use_mpfr = TRUE) {

  check_params_and_distribs(marginal_params, marginal_distribs)

  # Monte-Carlo sample

  p <- generate_rdm_distribution_p(
    copula_param,
    marginal_params,
    marginal_distribs,
    n_montecarlo
  )

  assertthat::assert_that(all(p[, 1] > 0))
  scale_vec <- p[, 2] / gamma(1 + 1 / p[, 1]) # scale lambda
  shape_vec <- p[, 1] # shape k

  # Under the double integral
  # Product of very small numbers
  # Use of Rmpfr package

  #for each agent i,
  # for each shape and scale index j
  #  compute: product over k of dweibull(DVKT_k, shape_j, scale_j)

  # Matrix :
  # Each line represents one agent,
  # Each column represents one Monte-Carlo simulation
  # M_i,j represents the above product

  #mpfr_dweibull <- function(x, shape, scale){
  #  if (x < 0) return(0)
  #  return(
  #    shape / scale *
  #      exp(
  #        (shape - 1)*log(x / scale) -
  #          (x / scale)^shape
  #      )
  #  )
  #}

  # Below auxiliary function computes one simulation for one agent
  aux_one_agent <- function(DVKT_vec, shape_n_scale) {
    vec <- sapply(X = DVKT_vec,  dweibull, shape = shape_n_scale[1], scale = shape_n_scale[2])
    if (use_mpfr) {
      vec <- Rmpfr::mpfr(vec, precBits = 128)
    }
    return(prod(vec))
  }
  fleet <- purrr::map(1:max(fleet$ind), ~ fleet[fleet$ind == ., "dvkt"])
  # Converts matrix of function parameters to lists
  shape_n_scale_list <- mapply(c, shape_vec, scale_vec, SIMPLIFY = FALSE)
  fun_params <- expand.grid(fleet, shape_n_scale_list)

  ILL <- mapply( # Individual likelihoods
    aux_one_agent,
    fun_params[[1]], # list of DVKT_vec
    fun_params[[2]]) # list of shape_n_scale


  # Reconstitute matrix
  if (use_mpfr) {
    ILL <- Rmpfr::mpfr2array(
      ILL,
      dim = c( length(fleet), n_montecarlo)
    )
  } else {
    ILL <- matrix(ILL,
                  nrow = length(fleet),
                  ncol = n_montecarlo,
                  byrow = FALSE)
  }
  # Monte-Carlo estimation of double integral
  double_int <- Matrix::rowSums(ILL) / n_montecarlo

  # NLL
  NLL <- -sum(log(double_int))

  return(as.numeric(NLL))

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
