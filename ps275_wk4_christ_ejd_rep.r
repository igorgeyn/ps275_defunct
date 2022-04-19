
### Some replication code for PS275, week four
### Christensen and Ejdemir replication
### In particular I was curious to see what would come of running the analysis on different quantiles of the outcome var.
### IG note: It almost certainly wasn't right to run this inclusive of post-tx outcomes.
### IG note: DT then mentioned a slightly different application of the initial idea, which is to do a kind-of quantile reg
### targeting quantities other than the mean.

## some additional stuff (incl. add'l rob checks)

# these plots suggest the intial motivation
ggplot(data = nyc_2008_ig) + geom_density(aes(x = log_y))
ggplot(data = nyc_2008_ig %>% filter(y < quantile(nyc_2008_ig$y,0.75))) + 
  geom_density(aes(x = log_y))

# setup DFs to use later; note this isn't the main output table
coefficients = tibble()
std_errs = tibble()
p_vals = tibble()

# define the func
est_quantile = function(inp_bound, ...) {
  low = -inp_bound
  hi = inp_bound
  iters = tibble()
  ## NB: the first 3 quantiles (i.e. 0.1 and 0.2 and 0.3) are zero;
  ## hence starting at 0.4.
  for (quant_inp in seq(0.4 ,1 ,0.10)) { 
    quant_inp = quant_inp
    out = feols(data = nyc_2008_ig %>% 
                filter(between(wks_till, low, hi) & y <= quantile(y,quant_inp)),
              log_y ~ T_post | district + opened + type)
    iter = cbind(quant_inp, out$coefficients, out$se[1], pvalue(out))
    iters = rbind(iters, iter)
  }
  
  iters = iters %>% mutate(inp_bound = inp_bound)
  table_of_iters <<- rbind(table_of_iters, iters)
}

# this is the main output table
table_of_iters = tibble()
# run the function on some set of weeks above/below the policy date (i.e. Oct 23, 2008)
for (inp_bound in c(2, 3, 4, 12)) {
  est_quantile(inp_bound = inp_bound)
}

# there is a better way to do this but i'm lazy
colnames(table_of_iters) = c('quant_inp', 'coeff', 'std_err', 'p_val', 'inp_bound')

# and finally make a plot!
# should probably add confidence bands
table_of_iters %>% 
  ggplot(aes(x = quant_inp, y = coeff,
             group = inp_bound,
             color = as.factor(inp_bound)
             )) + 
  geom_point() +
  geom_line() +
  labs(title = 'Tx effect by response time quantile', 
       x = 'Resp. Time Quantile',
       y = 'ATT of Term Extension') +
  theme_bw()