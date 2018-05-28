#' @title
#'
#' @description
#'
#' @param fit Model object from JAGS or Stan
#' @param par.name Parameter of intrest to organize into a dataframe
#' @param mcmc.out Logical, whether the full dataframe with all values should be
#'   returned. Defaults to FALSE.
#'
#' @examples Add these in...
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#' @export

organize = function(fit, par.name, mcmc.out = FALSE){
  # require(ggmcmc)

  # JAGS
  if(class(fit)[1] == "rjags"){
    if(mcmc.out == FALSE){
      tmp = coda::mcmc.list(lapply(1:fit$model$nchain(), function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))

      f1 = ggs(tmp, family = par.name)

    } else {

      f1 = coda::mcmc.list(lapply(1:fit$model$nchain(), function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))
    }
  }

  # JAGS Parallel
  if(class(fit)[1] == "rjags.parallel"){
    if(mcmc.out == FALSE){
      tmp = coda::mcmc.list(lapply(1:3, function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))

      f1 = ggs(tmp, family = par.name)

    } else {

      f1 = coda::mcmc.list(lapply(1:3, function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))
    }
  }

  # for a stan object
  if(any(class(fit) == "stanfit")){
    if(mcmc.out == FALSE){
      # looks like gss works directly with stan objects now...
      f1 <- ggs(fit, family = par.name)
      # used to do this... but with the new 'family' argument
      # tmp <- coda::mcmc.list(lapply(1:ncol(fit), function(x) coda::mcmc(as.array(fit)[,x,])))
      # f1 <- ggs(tmp, family = par.name)

    } else {

      f1 = coda::mcmc.list(lapply(1:ncol(fit), function(x) coda::mcmc(as.array(fit)[,x,])))

    }
  }

  return(f1)
}


# organize(JD.out, par.name = "p")
# organize(SM.c, par.name = "p")
