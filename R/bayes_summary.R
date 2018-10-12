#' @title Calculate mean, 95% Lower and Upper CRI, from JAGS and Stan
#'
#' @description Takes an fitted Bayesian model object from either JAGS or Stan and returns summary stats.
#'
#' @param fit Model object from JAGS or Stan
#' @param par.name Parameter of intrest to return summary stats
#'
#' @examples Add these in...
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "group_by"
#' @importFrom dplyr "summarize"
#' @export

bayes_summary = function(fit, par.name){

  dat = organize(fit, par.name, mcmc.out = FALSE)

  dat.2 = group_by(dat, Parameter) %>%
    summarize(my.mean = mean(value),
              upper = quantile(value, .95),
              lower = quantile(value, .05))

  return(dat.2)
}
