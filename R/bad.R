#' @title  Returns parameters from a Stan object which are not converged
#'
#' @description Returns the parameters from a Stan object which are not
#'   converged, based on Rhat.
#'
#' @param fit Model object from Stan
#' @param r.hat.level Argument controling the level for Rhat. Defaults to 1.2
#' @param plot Display trace plots of these parameters. (not working)
#'
#' @examples
#' \dontrun{
#' library(rstan)
#' scode <- "
#' parameters {
#' real y[2];
#' }
#' model {
#' y[1] ~ normal(0, 1);
#' y[2] ~ double_exponential(0, 2);
#' }
#' "
#' fit <- stan(model_code = scode, iter = 10, verbose = FALSE)
#' bad(fit)
#' }
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#' @export

bad = function(fit, r.hat.level = 1.2, plot = FALSE){

  if(any(class(fit) == "stanfit")){

    Rhat = rstan::summary(fit)$summary[,"Rhat"]
    bad = Rhat[which(Rhat > r.hat.level)]
    b1 = length(bad)
    bad.per = round(b1 / length(Rhat), 3)

    message(c(print(b1)," parms < ", r.hat.level, " Rhat, ", bad.per, " % Overall"))

    # if(plot == TURE){
    #   stan_trace(fit = fit, par.name = )
    # }
    # this is still returning something - not sure why
    if(length(bad) == 0){
      return(invisible())
    } else {
      return(bad)
    }

  } else {
    message("This only works for Stan objects")
  }
}
