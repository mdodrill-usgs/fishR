#' @title  Returns parameters from a Stan object which are not converged
#'
#' @description Returns the parameters from a Stan object which are not
#'   converged, based on Rhat.
#'
#' @param fit Model object from Stan
#' @param r.hat.level Argument controling the level for Rhat. Defaults to 1.2
#' @param plot Display trace plots of these parameters. (not working)
#'
#' @examples Need to add these
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

    return(bad)

  } else {
    message("This only works for Stan objects")
  }
}
