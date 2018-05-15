#' @title Trace plots for Stan model objects
#'
#' @description Creates trace and density plots for Stan model objects, with
#'   R-hat and Neff displayed.
#' @param fit Stan model object.
#' @param par.name Name of the parameter of interest in quotes.
#' @param number A numeric vector giving the index for what values of the
#'   parameter to display.  This can be also be a list for specifying paramaters
#'   in a matrix. See Details.
#' @param same.scale Display the density plots on the same x-axis scale, across parameters. Defaults to false.
#' @examples Need to add examples.
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#' @export

# to do:
# 1). add in mean bars for right hand side plot
# 2). for large model objects, figure out a better way to format the data!

# little helper function
# make.name = function(par.name, number){
#   out = vector()
#   for(i in 1:length(number)){
#     out[i] = c(paste(par.name,"[",number[i],"]", sep = ""))
#   }
#   return(out)
# }

# number = c(1:7)
# number = list(c(1:7), c(1:7))
# par.name = "mu_sz"
# par.name = "s_sp_eff"

stan_trace = function(fit, par.name, number, same.scale = FALSE){
  # require(ggmcmc)
  # require(gridExtra)
  # require(ggthemes)

  make.name = function(par.name, number){
    out = vector()
    if(is.list(number)){
      for(i in 1:length(number[[1]])){
        out[i] = c(paste(par.name, "[", number[[1]][i], ",", number[[2]][i], "]", sep = ""))
      }
    } else {
      for(i in 1:length(number)){
        out[i] = c(paste(par.name,"[",number[i],"]", sep = ""))
      }
    }
    return(out)
  }

  s <- coda::mcmc.list(lapply(1:ncol(fit), function(x) coda::mcmc(as.array(fit)[,x,])))
  # this often thows a warning, don't think this is needed/meaningful, so I turned it off ;)
  s <- suppressWarnings(ggmcmc::ggs(s))

  names = make.name(par.name, number)

  ltl.s = s[which(s$Parameter %in% names),]

  attr(ltl.s, "nParameters") <- length(names)
  attr(ltl.s, "nChains") <- 3
  attr(ltl.s, "nThin") <- 1
  attr(ltl.s, "nBurnin") <- 0

  # get the Rhat values
  r.tmp = rstan::summary(fit)$summary[,"Rhat"]
  r.hat = r.tmp[which(names(r.tmp) %in% names)]

  # get the n.eff
  n.tmp = rstan::summary(fit)$summary[,"n_eff"]
  n.eff = n.tmp[which(names(n.tmp) %in% names)]


  my.y = group_by(ltl.s, Parameter) %>%
    summarize(y.val = quantile(value, .9)) # y.val is the position to place the label (below)

  my.y2 = group_by(ltl.s, Parameter) %>%
    summarize(y.val = quantile(value, .2)) # y.val is the position to place the label (below)

  r.hat.d = data.frame(Parameter = names(r.hat),
                       Rhat = round(r.hat, 2),
                       Chain = rep(1,length(names)),
                       my.y = my.y$y.val)

  n.eff.d = data.frame(Parameter = names(n.eff),
                       Rhat = round(n.eff),
                       Chain = rep(1,length(names)),
                       my.y = my.y2$y.val)

  set.1 = ggmcmc::ggs_traceplot(ltl.s) +
    # geom_text(data = r.hat.d, x = -3, aes(y = my.y, label = Rhat), color = "#cb4b16") + # add Rhat values
    geom_label(data = r.hat.d, x = 0, aes(y = my.y, label = Rhat), color = "#cb4b16", fill = "#002b36", size = 3) + # add Rhat values
    geom_label(data = n.eff.d, x = 0, aes(y = my.y, label = Rhat), color = "#cb4b16", fill = "#002b36", size = 3) + # add Rhat values


    ggthemes::theme_solarized(light = FALSE) +
    theme(strip.background = element_rect(fill = "#002b36", color = "#002b36"),
          strip.text = element_text(color = "#cb4b16"),
          legend.position = "none")

  if(same.scale == TRUE){
    set.2 <- ggplot(ltl.s, aes(x = value, colour = as.factor(Chain), fill = as.factor(Chain))) +
      geom_density(alpha = 0.3) + scale_fill_discrete(name = "Chain") +
      scale_colour_discrete(name = "Chain") +
      facet_wrap(~Parameter, ncol = 1, scales = "free_y") +
      ggthemes::theme_solarized(light = FALSE) +
      theme(strip.background = element_rect(fill = "#002b36", color = "#002b36"),
            strip.text = element_text(color = "#cb4b16"),
            legend.key = element_blank())
  } else {
    set.2 <- ggplot(ltl.s, aes(x = value, colour = as.factor(Chain), fill = as.factor(Chain))) +
      geom_density(alpha = 0.3) + scale_fill_discrete(name = "Chain") +
      scale_colour_discrete(name = "Chain") +
      facet_wrap(~Parameter, ncol = 1, scales = "free") +
      ggthemes::theme_solarized(light = FALSE) +
      theme(strip.background = element_rect(fill = "#002b36", color = "#002b36"),
            strip.text = element_text(color = "#cb4b16"),
            legend.key = element_blank())

  }

  gridExtra::grid.arrange(set.1, set.2, ncol = 2)
}

# my_plots(fit, "mu_lprod", 6:10)
# my_plots(fit, "mu_sz", 1:7)
# my_plots(fit, "s_sp_eff", list(rep(1,7),c(1:7)))

# other_theme = theme(strip.background = element_rect(fill = "white"),
#       strip.background = element_blank(),

# how to find the colors used in the theme
# ggthemes_data$solarized$accents
# ggthemes_data$solarized$base

#-----------------------------------------------------------------------------#
