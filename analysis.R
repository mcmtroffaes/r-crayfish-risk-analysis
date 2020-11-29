###########################################################################
# get extreme points of utility weights
###########################################################################

library(rcdd)

get_hrep = function(csvfile) {
  expinfo = read.csv(csvfile)
  pair1 = as.numeric(expinfo[expinfo$input == "pair1",-1])  # good
  pair2 = as.numeric(expinfo[expinfo$input == "pair2",-1])  # bad
  worst = match(1, expinfo[expinfo$input == "worst",-1])
  other = which(expinfo[expinfo$input == "worst",-1] == 0)
  slider1 = as.numeric(expinfo[expinfo$input == "slider1",-1]) / 100.
  slider2 = as.numeric(expinfo[expinfo$input == "slider2",-1]) / 100.
  n_attrs = length(pair1)
  u = t(replicate(n_attrs + 1, pair1))
  for (i in 1:n_attrs)
      u[i, i] = pair2[i]
  best = n_attrs + 1
  a0 = -diag(n_attrs)
  b0 = rep(0, n_attrs)
  a1 = NULL
  for (j in other) {
      a1 = rbind(a1, -u[j,] + (1-slider1[j])*u[worst,] + slider1[j]*u[best,])
      a1 = rbind(a1,  u[j,] - (1-slider2[j])*u[worst,] - slider2[j]*u[best,])
  }
  b1 = rep(0, 2 * length(other))
  a = rbind(a0, a1)
  b = c(b0, b1)
  a_eq = rep(1, n_attrs)
  b_eq = 1
  # a * x <= b, a_eq * x == b_eq
  makeH(a, b, a_eq, b_eq)
}

get_util_weights = function(csvfile) {
  vrep = scdd(get_hrep(csvfile))$output
  stopifnot(all(vrep[,1] == 0))
  stopifnot(all(vrep[,2] == 1))
  vrep[,c(-1,-2)]
}

###########################################################################
# run the model with jags
###########################################################################

library(rjags)

# model without management decision (posterior probability of presence)
get_h = function(t, alpha) {
  ms = "
model {
  theta ~ dbeta(s*t, s*(1-t)) T(0.001,0.999) # truncate to avoid infinite density
  H ~ dbinom(theta,1)
  E ~ dbinom(alpha,H)
}"
  data_to_model = list(s=2, t=t, alpha=alpha, E=0)
  m = jags.model(textConnection(ms), data=data_to_model, inits=list(H=1), n.adapt=10^6, n.chains=1)
  summary(coda.samples(m, 'H', n.iter=10^4, thin=1))
}

# model with management decision (range on posterior probability of
# presence after management decision, and range on posterior expected
# utility)
get_hprime_util_ranges = function(csvfile, t, alpha) {
  ms = "
model {
  theta ~ dbeta(s*t, s*(1-t)) T(0.001,0.999) # truncate to avoid infinite density
  H ~ dbinom(theta,1)
  E ~ dbinom(alpha,H)
  for(j in 1:n_decisions) {
    for(i in 1:n_beta_points) {
      Hprime[i,j] ~ dbinom(1-beta[i,j],H)
      for (k in 1:n_util_points) {
        U[j,i,k] =
            Hprime[i,j]       * inprod(util_hprime_one [,j], util_weights[k,])
          + (1 - Hprime[i,j]) * inprod(util_hprime_zero[,j], util_weights[k,])
      }
    }
  }
}"
  
  n_decisions = 6
  n_beta_points = 2
  beta_bounds = matrix(c(0.0,0.05,0.3,0.4,1.0,0.7,
                         0.0,0.25,0.5,0.7,1.0,0.8), nrow=2, byrow=TRUE)
  util_hprime_zero = matrix(c(4,4,3,3,2,2,
                              4,4,3,3,2,1,
                              4,4,3,2,1,2,
                              4,4,3,1,2,3), ncol=6, byrow=TRUE)
  util_hprime_one  = matrix(c(1,1,1,1,1,1,
                              1,1,1,1,1,1,
                              4,4,3,2,1,2,
                              4,4,3,1,2,3), ncol=6, byrow=TRUE)
  util_weights = get_util_weights(csvfile)
  n_util_points = dim(util_weights)[1]
  rownames(beta_bounds) = c('lower','upper')
  
  # to get the results from the ISIPTA paper, set
  # s=10000, t=0.999, alpha=0.999, E=1
  
  data_to_model = list(s=2, t=t, alpha=alpha, E=0,
                       beta=beta_bounds, n_beta_points=n_beta_points,
                       n_decisions=n_decisions,
                       n_util_points=n_util_points, util_weights=util_weights,
                       util_hprime_zero=util_hprime_zero,
                       util_hprime_one=util_hprime_one)
  
  m = jags.model(textConnection(ms), data=data_to_model, inits=list(H=1), n.adapt=10^6, n.chains=1)
  smry = summary(coda.samples(m, c('Hprime', 'U'), n.iter=10^4, thin=1))
  s = smry$statistics[,'Mean']
  hprime = array(
    s[1:(n_beta_points * n_decisions)], c(n_beta_points, n_decisions))
  util = array(
    s[-(1:(n_beta_points * n_decisions))], c(n_decisions, n_beta_points, n_util_points))
  list(
    hprime_range=apply(hprime, 2, range),
    util_range=apply(util, 1, range))
}

###########################################################################
# plot results
###########################################################################

library(ggplot2)

# update range so every column has minimum width
make_range_min_width = function(range, min_width) {
  mid = 0.5 * (range[1,] + range[2,])
  mid_range = rbind(mid - 0.5 * min_width, mid + 0.5 * min_width)
  rbind(pmin(range[1,], mid_range[1,]), pmax(range[2,], mid_range[2,]))
}

plotit = function(csvfile, t, alpha) {
  pdf(paste0('fig-t', t * 10, '-a', alpha * 10, '.pdf'), width=4.5, height=2.25)
  ranges = get_hprime_util_ranges(csvfile=csvfile, t=t, alpha=alpha)
  barnames = c('I','II','III','IV','V','VI')
  groupnames = factor(c('probability of presence','expected utility'),levels=c('probability of presence','expected utility'))

  # update range data scale and ensure ranges are visible
  ranges$hprime_range_plot = make_range_min_width(
      100 * ranges$hprime_range, 0.02 * 100)
  ranges$util_range_plot = make_range_min_width(ranges$util_range, 0.02 * 3)

  # line range data
  data_range = rbind(
    data.frame(
      lower=ranges$hprime_range_plot[1,], upper=ranges$hprime_range_plot[2,],
      d=barnames, param=groupnames[1]),
    data.frame(
      lower=ranges$util_range_plot[1,], upper=ranges$util_range_plot[2,],
      d=barnames, param=groupnames[2]))
  # hline data
  data_hline = data.frame(
    val=c(t*100, max(ranges$util_range_plot[1,])), param=groupnames)
  # blank data to ensure common limits on all plots
  data_blank = data.frame(
    y=c(0,100,1,4), param=rep(groupnames, each=2))
  
  p = ggplot() + 
    scale_color_discrete() +
    geom_linerange(data=data_range,
                   aes(ymin=lower, ymax=upper, x=d, col=d), size=6) +
    geom_hline(data=data_hline, aes(yintercept=val), linetype=c(1, 2)) +
    geom_blank(data=data_blank, aes(y=y, param=param)) +
    facet_wrap(~param, scales='free') +
    coord_flip() +
    theme(legend.position='none') + 
    labs(x='decision', y ='')
  print(p)

  dev.off()
}

###########################################################################
# some extra helper functions
###########################################################################

print_util_weights = function(csvfile) {
  library(xtable)
  print(xtable(get_util_weights(csvfile)), floating=FALSE, tabular.environment="tabular", hline.after=NULL, include.rownames=TRUE, include.colnames=TRUE)
}

###########################################################################
# run everything
###########################################################################

main = function(csvfile) {
  plotit(csvfile, t=0.1, alpha=0.1)
  plotit(csvfile, t=0.5, alpha=0.1)
  plotit(csvfile, t=0.9, alpha=0.1)
  plotit(csvfile, t=0.1, alpha=0.5)
  plotit(csvfile, t=0.5, alpha=0.5)
  plotit(csvfile, t=0.9, alpha=0.5)
  plotit(csvfile, t=0.8, alpha=0.1)

  # alpha=0.9 is not interesting since then we know
  # there is no crayfish with high probability

  print(get_h(t=0.8, alpha=0.1))
  print_util_weights(csvfile)
}

main("expert-data.csv")

##########################################################################
# run the combined analysis
##########################################################################

t_vals = c(0.1,0.1,0.9,0.9)
alpha_vals = c(0.1,0.5,0.1,0.5)
plotit_2 = function(csvfile, t_vals, alpha_vals) {
  csvfile = "expert-data.csv"
  pdf(paste0('fig-all.pdf'), width=4.5, height=2.25)
  ranges_list = lapply(1:4,function(ix){
    get_hprime_util_ranges(csvfile=csvfile, t=t_vals[ix], alpha=alpha_vals[ix])
  })
  ranges <- ranges_list[[1]]
  ranges_h <- lapply(ranges_list,function(x){x$hprime_range})
  ranges_h_min <- pmin(ranges_h[[1]],ranges_h[[2]],ranges_h[[3]],ranges_h[[4]])
  ranges_h_max <- pmax(ranges_h[[1]],ranges_h[[2]],ranges_h[[3]],ranges_h[[4]])
  ranges$hprime_range <- rbind(ranges_h_min[1,], ranges_h_max[2,])
  ranges_u <- lapply(ranges_list,function(x){x$util_range})
  ranges_u_min <- pmin(ranges_u[[1]],ranges_u[[2]],ranges_u[[3]],ranges_u[[4]])
  ranges_u_max <- pmax(ranges_u[[1]],ranges_u[[2]],ranges_u[[3]],ranges_u[[4]])
  ranges$util_range <- rbind(ranges_u_min[1,], ranges_u_max[2,])
  
  barnames = c('I','II','III','IV','V','VI')
  groupnames = factor(c('probability of presence','expected utility'),levels=c('probability of presence','expected utility'))
  
  # update range data scale and ensure ranges are visible
  ranges$hprime_range_plot = make_range_min_width(
    100 * ranges$hprime_range, 0.02 * 100)
  ranges$util_range_plot = make_range_min_width(ranges$util_range, 0.02 * 3)
  
  # line range data
  data_range = rbind(
    data.frame(
      lower=ranges$hprime_range_plot[1,], upper=ranges$hprime_range_plot[2,],
      d=barnames, param=groupnames[1]),
    data.frame(
      lower=ranges$util_range_plot[1,], upper=ranges$util_range_plot[2,],
      d=barnames, param=groupnames[2]))
  # hline data
  data_hline = data.frame(
    val=c(t*100, max(ranges$util_range_plot[1,])), param=groupnames)
  # blank data to ensure common limits on all plots
  data_blank = data.frame(
    y=c(0,100,1,4), param=rep(groupnames, each=2))
  
  p = ggplot() + 
    scale_color_discrete() +
    geom_linerange(data=data_range,
                   aes(ymin=lower, ymax=upper, x=d, col=d), size=6) +
    geom_hline(data=data_hline, aes(yintercept=val), linetype=c(1, 2)) +
    geom_blank(data=data_blank, aes(y=y, param=param)) +
    facet_wrap(~param, scales='free') +
    coord_flip() +
    theme(legend.position='none') + 
    labs(x='decision', y ='')
  print(p)
  
  dev.off()
}
