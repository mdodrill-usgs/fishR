#' @title Calculate mean, 95\% Lower and Upper CRI, from JAGS and Stan
#'
#' @description Takes an fitted Bayesian model object from either JAGS or Stan and returns summary stats.
#'
#' @param fit Model object from JAGS or Stan
#' @param par.name Parameter of intrest to return summary stats
#' @param percent Percentage for calculating credible intervals. Defaults to 95\% CRI.
#' @param transform Transformation to be applied to the data before summarizing.
#'   This is a character vector. For example, transform = "exp", will
#'   exponentiate the parameter.
#'
#' @examples Add these in...
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "group_by"
#' @importFrom dplyr "summarize"
#' @export

bayes_summary = function(fit, par.name, percent = 0.95, transform = NULL){

  dat = organize(fit, par.name, mcmc.out = FALSE)

  l = (1 - percent) / 2

  if(!is.null(transform)){
    dat$value = eval(call(transform, dat$value))
  }

  dat.2 = group_by(dat, Parameter) %>%
    summarize(my.mean = mean(value),
              upper = quantile(value, 1 - l),
              lower = quantile(value, l))

  return(dat.2)
}
