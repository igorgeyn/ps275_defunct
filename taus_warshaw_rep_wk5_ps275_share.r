
library(lme4); library(plm); library(ebal); library(gridExtra); library(tidyverse)

##### Tausanovitch and Warshaw replication
##### week 5

#### Notes / KIM:
#### - I don't do a version of the one-way FE inclusive of ebal (or other weights), but it could be worth trying.

#### Replication

## load things up

load(file="cities_140430.RData")
glimpse(data)
taus_warsh_data = data
n_distinct(taus_warsh_data$city_id)

## FE regression (!! remember to run tests !!)
## running this for the 'saturated' regressions (i.e. the ones
## with the covariates, i.e. i.e. the even-numbered ones)

# run FE models

# full_covars_set_fe = 'mrp_ideology*fog2 + mrp_ideology*initiative2 + 
#          mrp_ideology*partisan_elections2 + mrp_ideology*term_limits2 + 
#          mrp_ideology*pal_binary+median_income + city_pop + percent_black + house_value + 
#        as.factor(abb)'
full_covars_set_fe = c(
  'mrp_ideology*fog2',
  'mrp_ideology*initiative2',
  'mrp_ideology*partisan_elections2', 'mrp_ideology*term_limits2', 
  'mrp_ideology*pal_binary', 'median_income', 'city_pop',
  'percent_black', 'house_value', 'as.factor(abb)')
# lm(data = taus_warsh_data, 
#    as.formula(sprintf('policy ~ %s', paste(full_covars_set_fe, collapse = "+")))
#    )

# would be nice to figure this out for a case with a bunch of vars
# reg_form_fe = function(inp_data, dep_var, ...) {
#   # lm(data = inp_data, formula = dep_var ~ paste(full_covars_set_fe, collapse = "+"))
#   lm(data = inp_data,
#      formula = as.formula(sprintf(!!sym(dep_var), ' ~ %s', paste(full_covars_set_fe, collapse = "+")))
     # )
# }

reg_col2_fe =  ## all good here
  lm(data=taus_warsh_data,
       formula = 
       as.formula(sprintf('policy ~ %s', paste(full_covars_set_fe, collapse = "+")))
  )

reg_col4_fe =  ## all good here
  lm(data=taus_warsh_data,
     formula = 
       as.formula(sprintf('expenditures_capita ~ %s', paste(full_covars_set_fe, collapse = "+")))
  )

reg_col6_fe =  ## all good here
  lm(data=taus_warsh_data,
     formula = 
       as.formula(sprintf('taxes_capita ~ %s', paste(full_covars_set_fe, collapse = "+")))
  )

reg_col8_fe =  ## all good here
  lm(data=taus_warsh_data,
     formula = 
       as.formula(sprintf('salestax_share ~ %s', paste(full_covars_set_fe, collapse = "+")))
  )

# output RE with FE for comparison

stargazer(
  reg_col2_rep, reg_col2_fe,
  reg_col4_rep, reg_col4_fe, 
  reg_col6_rep, reg_col6_fe, 
  reg_col8_rep, reg_col8_fe,
  # column.labels = c('policy', 'expenditures_capita', 'taxes_capita', 'salestax_share'),
  dep.var.labels.include = F,
  keep.stat = 'n',
  omit.table.layout = 'n',
  keep = 'mrp_ideology',
  type = 'text' ## be sure to spit out a latex version, too (done)
)

## and now some basic testing

# phtest(reg_col2_rep, reg_col2_fe) ## PH test doesn't work on lmer objs :(((

# AIC !! put these in the robustness output table !!
AIC(logLik(reg_col2_rep))
AIC(logLik(reg_col2_fe))
AIC(logLik(reg_col4_rep))
AIC(logLik(reg_col4_fe))
AIC(logLik(reg_col6_rep))
AIC(logLik(reg_col6_fe))
AIC(logLik(reg_col8_rep))
AIC(logLik(reg_col8_fe))
# sprintf('AIC', & 428 & 428 & 1,461 & 1,461 & 1,433 & 1,433 & 907 & 907 \\ )

cat(sprintf("AIC & %4.0f & %4.0f & %4.0f & %4.0f & %4.0f & %4.0f & %4.0f & %4.0f \\\\", 
        AIC(logLik(reg_col2_rep)),
        AIC(logLik(reg_col2_fe)),
        AIC(logLik(reg_col4_rep)),
        AIC(logLik(reg_col4_fe)),
        AIC(logLik(reg_col6_rep)),
        AIC(logLik(reg_col6_fe)),
        AIC(logLik(reg_col8_rep)),
        AIC(logLik(reg_col8_fe))
))

### produce figure 5 (type of gov't / FOG)

### ================ notes:
## - ebal on  median income, median home values, population, city conservatism 
## - (cont.) and the use of partisan elections, ballot initiatives,
## - (cont.) term limits, and at-large districts
### ================ notes:

# check missing (for weights)
for (var in c('median_income', 'house_value',
              'city_pop', 'mrp_ideology',
              'partisan_elections2', 'initiative2',
              'term_limits2', 'pal_binary')) {
  print(taus_warsh_data %>% filter(is.na(!!sym(var))) %>% nrow())
  
}

taus_warsh_data_ebal = ## this isn't enough (maybe)
  taus_warsh_data %>% 
  filter(!is.na(fog2) & !is.na(median_income) &
           !is.na(house_value) & !is.na(city_pop) & !is.na(mrp_ideology) &
           !is.na(partisan_elections2) & !is.na(initiative2) & 
           !is.na(term_limits2) & !is.na(pal_binary))

# check missing
for (var in c('median_income', 'house_value',
              'city_pop', 'mrp_ideology',
              'partisan_elections2', 'initiative2',
              'term_limits2', 'pal_binary')) {
  print(taus_warsh_data_ebal %>% filter(is.na(!!sym(var))) %>% nrow())
  
}

# produce weights fig 5
weights_loess = 
  ebalance(
  Treatment = taus_warsh_data_ebal$fog2,
         X = taus_warsh_data_ebal %>% 
    select(c(median_income, house_value,
             city_pop, mrp_ideology,
             partisan_elections2, initiative2,
             term_limits2, pal_binary)),
         # taus_warsh_data
         
         )$w

# make plot data
taus_warsh_data_plot = 
  cbind(taus_warsh_data_ebal %>% filter(fog2==0), 
        weights_loess) %>% 
  rbind(taus_warsh_data_ebal %>% 
          filter(fog2==1) %>% 
          mutate(weights_loess=1))

plot_taus_fig5 = 
  function(y_var, ...) {
    ggplot(taus_warsh_data_plot, aes(x = mrp_ideology, y = !!sym(y_var),
                                     weight = weights_loess,
                                     # group = as.factor(as.factor(!!sym(group_var))),
                                     # color = as.factor(as.factor(!!sym(group_var)))
                                   group = as.factor(fog2), color = as.factor(fog2))
                                   ) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = loess, legend = F) +
  theme_bw()
  }

taus_warsh_fig5_a = plot_taus_fig5(y_var = 'policy')

taus_warsh_fig5 = 
  grid.arrange(
    plot_taus_fig5(y_var = 'policy'
                    # , group_var = 'fog2'
                    ), 
    plot_taus_fig5(y_var = 'expenditures_capita'
                    # , group_var = 'fog2'
                    ),
    plot_taus_fig5(y_var = 'taxes_capita'
                    # , group_var = 'fog2'
                    ), 
    plot_taus_fig5(y_var = 'salestax_share'
                    # , group_var = 'fog2'
                    ),
  ncol = 2, nrow = 2
)

### produce figure 9 (at-large elections)

### ================ notes:
## - ebal on  median income, median home values, population, city conservatism 
## - (cont.) and the use of direct mayoral elections (fog), partisan election, 
## - (cont.) ballot initiatives, and term limits.
### ================ notes:


# produce weights fig 9
weights_loess_fig9 = 
  ebalance(
    Treatment = taus_warsh_data_ebal$pal_binary,
    X = taus_warsh_data_ebal %>% 
      select(c(median_income, house_value,
               city_pop, mrp_ideology,
               partisan_elections2, initiative2,
               term_limits2, fog2)),
    # taus_warsh_data
    
  )$w

# make plot data
taus_warsh_data_plot_fig9 = 
  cbind(taus_warsh_data_ebal %>% filter(pal_binary==0), 
        weights_loess_fig9) %>% 
  rbind(taus_warsh_data_ebal %>% 
          filter(pal_binary==1) %>% 
          mutate(weights_loess_fig9=1))

plot_taus_fig9 = 
  function(y_var, ...) {
    ggplot(taus_warsh_data_plot_fig9, aes(x = mrp_ideology, y = !!sym(y_var),
                                     weight = weights_loess_fig9,
                                     # group = as.factor(as.factor(!!sym(group_var))),
                                     # color = as.factor(as.factor(!!sym(group_var)))
                                     group = as.factor(pal_binary), 
                                     color = as.factor(pal_binary))
    ) +
      geom_point() + 
      geom_smooth(method = loess, legend = F) +
      theme_bw()
  }

taus_warsh_fig9 = 
  grid.arrange(
    plot_taus_fig9(y_var = 'policy'
                   # , group_var = 'fog2'
    ), 
    plot_taus_fig9(y_var = 'expenditures_capita'
                   # , group_var = 'fog2'
    ),
    plot_taus_fig9(y_var = 'taxes_capita'
                   # , group_var = 'fog2'
    ), 
    plot_taus_fig9(y_var = 'salestax_share'
                   # , group_var = 'fog2'
    ),
    ncol = 2, nrow = 2
  )

### take a look at congruence in Taus and Warsh data (rather than responsiveness)
### So:
### - code ideology as binary, code each outcome as binary, gen var that is defined by match
### - then do some other stuff (see below).

## first just do the dumb of thing running lmer on ideology + pres_2008 vote:
reg_col2_rep_w_pres_2008 = 
  lmer(data=taus_warsh_data,
       policy ~ mrp_ideology*fog2 + pres_2008 + mrp_ideology*initiative2 + 
         mrp_ideology*partisan_elections2 + mrp_ideology*term_limits2 + 
         mrp_ideology*pal_binary+median_income + city_pop + percent_black + house_value + 
         (1|abb) ## this is the RE part i guess
  )


reg_col4_rep_w_pres_2008 = 
  lmer(data=taus_warsh_data,
       expenditures_capita ~ mrp_ideology*fog2 + pres_2008 + mrp_ideology*initiative2 + 
         mrp_ideology*partisan_elections2 + mrp_ideology*term_limits2 + 
         mrp_ideology*pal_binary+median_income + city_pop + percent_black + house_value + 
         (1|abb) ## this is the RE part i guess
  )

reg_col6_rep_w_pres_2008 = 
  lmer(data=taus_warsh_data,
       taxes_capita ~ mrp_ideology*fog2 + pres_2008 + mrp_ideology*initiative2 + 
         mrp_ideology*partisan_elections2 + mrp_ideology*term_limits2 + 
         mrp_ideology*pal_binary+median_income + city_pop + percent_black + house_value + 
         (1|abb) ## this is the RE part i guess
  )

reg_col8_rep_w_pres_2008 = 
  lmer(data=taus_warsh_data,
       salestax_share ~ mrp_ideology*fog2 + pres_2008 + mrp_ideology*initiative2 + 
         mrp_ideology*partisan_elections2 + mrp_ideology*term_limits2 + 
         mrp_ideology*pal_binary+median_income + city_pop + percent_black + house_value + 
         (1|abb) ## this is the RE part i guess
  )

stargazer(
  reg_col2_rep_w_pres_2008, reg_col4_rep_w_pres_2008,
  reg_col6_rep_w_pres_2008, reg_col8_rep_w_pres_2008,
  type = 'text'
)

# not great, but it'll work
cor(taus_warsh_data %>% filter(!is.na(pres_2008) & !is.na(mrp_ideology)) %>% mutate(pres_2008_rev = -pres_2008) %>% select(pres_2008_rev), 
    taus_warsh_data %>% filter(!is.na(pres_2008) & !is.na(mrp_ideology)) %>% select(mrp_ideology)
    )


## starting without `ebal`
taus_warsh_data_cong = 
  taus_warsh_data %>% 
  mutate(pres_2008_rev = -pres_2008) %>% 
  mutate(lib = if_else(pres_2008 > 0.50,1,0)) ## dep var for cong

## the regressions
taus_warsh_twfe_policy = feols(data = taus_warsh_data_cong, policy ~ lib * mrp_ideology | as.factor(abb))
taus_warsh_twfe_exps = feols(data = taus_warsh_data_cong, expenditures_capita ~ lib * mrp_ideology | as.factor(abb))
taus_warsh_twfe_taxes = feols(data = taus_warsh_data_cong, taxes_capita ~ lib * mrp_ideology | as.factor(abb))
taus_warsh_twfe_salestax = feols(data = taus_warsh_data_cong, salestax_share ~ lib * mrp_ideology | as.factor(abb))

## stargazer doesn't work for feols objects (do not run)
# stargazer(
#   taus_warsh_twfe_policy,taus_warsh_twfe_exps,
#   taus_warsh_twfe_taxes, taus_warsh_twfe_salestax,
#   type = 'text'
# )  

# spit out a simple twfe plot (preview)
etable(
  taus_warsh_twfe_policy, taus_warsh_twfe_exps,
  taus_warsh_twfe_taxes, taus_warsh_twfe_salestax
)

etable(
  #taus_warsh_twfe_policy, 
  taus_warsh_twfe_policy, taus_warsh_twfe_exps,
  taus_warsh_twfe_taxes, taus_warsh_twfe_salestax,
  tex = T, 
  file = paste0(path, "/output/tausanovitch_warshaw_2014/taus_warsh_twfe_all_outcomes.tex"))

# spit out a simple twfe plot (latex)
etable(taus_warsh_twfe_policy, tex = T, 
       file = paste0(path, "/output/tausanovitch_warshaw_2014/taus_warsh_twfe.tex"))


















