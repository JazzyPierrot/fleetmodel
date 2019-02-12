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
  n_montecarlo = 1000){

  check_params_and_distribs(marginal_params, marginal_distribs)

  # Monte-Carlo sample

  p <- generate_rdm_distribution_p(
    copula_param,
    marginal_params,
    marginal_distribs,
    n_montecarlo
  )

  assertthat::assert_that(all(p[,1] > 0))
  scale_vec <- p[,2] / gamma(1 + 1/p[,1]) # scale lambda
  shape_vec <- p[,1] # shape k

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

  # Below auxiliary function computes one simulation for one agent
  aux_one_agent <- function(DVKT_vec, shape_n_scale){
    vec <- sapply(X = DVKT_vec,  dweibull, shape = shape_n_scale[1], scale = shape_n_scale[2])
    # Here transfom to Rmpfr
    return(prod(vec))
  }

  # Converts matrix of function parameters to lists
  shape_n_scale_list <- mapply(c, shape_vec, scale_vec, SIMPLIFY = FALSE)
  fun_params <- expand.grid(fleet, shape_n_scale_list)

  ILL <- mapply( # Individual likelihoods
    aux_one_agent,
    fun_params[[1]], # list of DVKT_vec
    fun_params[[2]]) # list of shape_n_scale

  # Reconstitute matrix
  ILL <- matrix(ILL,
                nrow = length(fleet),
                ncol = n_montecarlo,
                byrow = FALSE)

  # Monte-Carlo estimation of double integral
  double_int <- rowSums(ILL) / n_montecarlo

  # NLL

  NLL <- - sum(log(double_int))

  return(NLL)

}
