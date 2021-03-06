#' @title Trace plots for Stan or JAGS model objects
#'
#' @description Creates trace and density plots for Stan or JAGS model objects, with
#'   R-hat and Neff displayed.
#' @param fit Stan model object.
#' @param par.name Name of the parameter of interest in quotes.
#' @param number A numeric vector giving the index for what values of the
#'   parameter to display.  This can be also be a list for specifying paramaters
#'   in a matrix. See Details.
#' @param same.scale Display the density plots on the same x-axis scale, across parameters. Defaults to false.
#' @param n.per.page Number of parameters to display per page. Defaults to 6.
#' @examples Need to add examples.
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "geom_label"
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "geom_density"
#' @importFrom ggplot2 "scale_colour_discrete"
#' @importFrom ggplot2 "scale_fill_discrete"
#' @importFrom ggplot2 "facet_wrap"
#' @importFrom ggplot2 "element_rect"
#' @importFrom ggplot2 "element_blank"
#' @importFrom ggplot2 "element_text"
#' @importFrom ggplot2 "labs"
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

stan_trace = function(fit, par.name, number, same.scale = FALSE, n.per.page = 6){

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
  #------------------------------------
  # s <- coda::mcmc.list(lapply(1:ncol(fit), function(x) coda::mcmc(as.array(fit)[,x,])))
  # this often thows a warning, don't think this is needed/meaningful, so I turned it off ;)
  # s <- suppressWarnings(ggmcmc::ggs(s))
  s.mcmc <- organize(fit = fit, mcmc.out = TRUE)
  s <- suppressWarnings(ggmcmc::ggs(s.mcmc))

  names = make.name(par.name, number)

  ltl.s = s[which(s$Parameter %in% names),]

  # fixes name where there is only one element to the parameter (ex, "beta", not "beta[1]")
  if(length(names) == 1 & nrow(ltl.s) == 0){
    names = substr(names, 1, (nchar(names) - 3))
    ltl.s = s[which(s$Parameter %in% names),]
  }
  #------------------------------------
  n.page = ceiling(length(names) / n.per.page)

  for(i in 1:n.page){
    idx = ((i-1)*n.per.page + 1):(n.per.page*i)
    idx = idx[which(idx <= length(names))]

    sub.ltl.s = ltl.s[which(as.character(ltl.s$Parameter) %in% names[idx]),]

    sub.ltl.s = droplevels(sub.ltl.s)

    attr(sub.ltl.s, "nParameters") <- length(names[idx])
    attr(sub.ltl.s, "nChains") <- 3
    attr(sub.ltl.s, "nThin") <- 1
    attr(sub.ltl.s, "nBurnin") <- 0

    if(any(class(fit) == "stanfit")){
      # get the Rhat values
      r.tmp = rstan::summary(fit)$summary[,"Rhat"]
      r.hat = r.tmp[which(names(r.tmp) %in% names[idx])]

      # get the n.eff
      n.tmp = rstan::summary(fit)$summary[,"n_eff"]
      n.eff = n.tmp[which(names(n.tmp) %in% names[idx])]
    }

    if(any(class(fit) == "rjags")){
      r.tmp = fit$BUGSoutput$summary[,8]
      r.hat = r.tmp[which(names(r.tmp) %in% names[idx])]

      # n.tmp = coda::effectiveSize(fit)
      n.tmp = fit$BUGSoutput$summary[,9]
      n.eff = n.tmp[which(names(n.tmp) %in% names[idx])]
    }

    my.y = dplyr::group_by(sub.ltl.s, Parameter) %>%
      dplyr::summarize(y.val = quantile(value, .9)) # y.val is the position to place the label (below)

    my.y2 = dplyr::group_by(sub.ltl.s, Parameter) %>%
      dplyr::summarize(y.val = quantile(value, .2)) # y.val is the position to place the label (below)

    r.hat.d = data.frame(Parameter = names(r.hat),
                         Rhat = round(r.hat, 2),
                         Chain = rep(1,length(idx)),
                         my.y = my.y$y.val)

    n.eff.d = data.frame(Parameter = names(n.eff),
                         Rhat = round(n.eff),
                         Chain = rep(1,length(idx)),
                         my.y = my.y2$y.val)

    set.1 = ggmcmc::ggs_traceplot(sub.ltl.s) +
      geom_label(data = r.hat.d, x = 0, aes(y = my.y, label = Rhat), color = "#cb4b16", fill = "#002b36", size = 3) + # add Rhat values
      geom_label(data = n.eff.d, x = 0, aes(y = my.y, label = Rhat), color = "#cb4b16", fill = "#002b36", size = 3) + # add Rhat values
      ggthemes::theme_solarized_2(light = FALSE) +
      theme(strip.background = element_rect(fill = "#002b36", color = "#002b36"),
            strip.text = element_text(color = "#cb4b16"),
            legend.position = "none")

    if(same.scale == TRUE){
      set.2 <- ggplot(sub.ltl.s, aes(x = value, colour = as.factor(Chain), fill = as.factor(Chain))) +
        geom_density(alpha = 0.3) + scale_fill_discrete(name = "Chain") +
        scale_colour_discrete(name = "Chain") +
        labs(y = "") +
        facet_wrap(~Parameter, ncol = 1, scales = "free_y") +
        ggthemes::theme_solarized_2(light = FALSE) +
        theme(strip.background = element_rect(fill = "#002b36", color = "#002b36"),
              strip.text = element_text(color = "#cb4b16"),
              legend.position = "none")
    } else {
      set.2 <- ggplot(sub.ltl.s, aes(x = value, colour = as.factor(Chain), fill = as.factor(Chain))) +
        geom_density(alpha = 0.3) + scale_fill_discrete(name = "Chain") +
        scale_colour_discrete(name = "Chain") +
        labs(y = "") +
        facet_wrap(~Parameter, ncol = 1, scales = "free") +
        ggthemes::theme_solarized_2(light = FALSE) +
        theme(strip.background = element_rect(fill = "#002b36", color = "#002b36"),
              strip.text = element_text(color = "#cb4b16"),
              legend.position = "none")
    }

    gridExtra::grid.arrange(set.1, set.2, ncol = 2)
  }
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
