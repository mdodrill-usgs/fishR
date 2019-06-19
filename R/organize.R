#' @title Organizes fitted model objects from JAGS and Stan
#'
#' @description Takes an fitted Bayesian model object from either JAGS or Stan and returns a dataframe.
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
    # if(fit$model)
    tryCatch({fit$model$nchain()},
             error = function(e){
               fit = R2jags::recompile(fit)
               return(fit)
             })
  }

  if(class(fit)[1] == "rjags"){
    if(mcmc.out == FALSE){
      # "^" is regex for start - so only match for patterns at start
      key = grep(paste0("^", par.name), dimnames(fit$BUGSoutput$sims.array)[[3]])

      tmp = coda::mcmc.list(lapply(1:fit$model$nchain(), function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,key])))

      # subset for par.name above, with the key (hopfully this speeds things up)
      # f1 = ggmcmc::ggs(tmp, family = par.name)
      f1 = ggmcmc::ggs(tmp)

    } else {

      f1 = coda::mcmc.list(lapply(1:fit$model$nchain(), function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))
    }
  }

  # JAGS Parallel
  if(class(fit)[1] == "rjags.parallel"){
    if(mcmc.out == FALSE){
      # "^" is regex for start - so only match for patterns at start
      key = grep(paste0("^", par.name), dimnames(fit$BUGSoutput$sims.array)[[3]])

      tmp = coda::mcmc.list(lapply(1:3, function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,key])))

      f1 = ggmcmc::ggs(tmp)

    } else {

      f1 = coda::mcmc.list(lapply(1:3, function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))
    }
  }

  # for a stan object
  if(any(class(fit) == "stanfit")){
    if(mcmc.out == FALSE){
      # looks like gss works directly with stan objects now...
      # f1 <- ggmcmc::ggs(fit, family = par.name)  # not working 6/16/2019, switched back to below...
      # used to do this... but with the new 'family' argument
      tmp <- coda::mcmc.list(lapply(1:ncol(fit), function(x) coda::mcmc(as.array(fit)[,x,])))
      f1 <- ggmcmc::ggs(tmp, family = par.name)

    } else {

      f1 = coda::mcmc.list(lapply(1:ncol(fit), function(x) coda::mcmc(as.array(fit)[,x,])))

    }
  }

  return(f1)
}


# organize(JD.out, par.name = "p")
# organize(SM.c, par.name = "p")
