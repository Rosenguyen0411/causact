########## Richard McElreath’s blog the Missing Oxen model #################

## data
# Actual Values That Models Will Try To Recover
actual_p_u = 0.5
actual_p_s = 1
actual_sigma = 0.75
set.seed(1)

N_children <- 51   ## 51 children-ox pairs
s <- rbinom( N_children , size=1 , prob=actual_sigma )
s_obs <- s   # copy to add censoring

## make 21 censored obs (i.e. NA), leave 30 uncensored
s_obs[ sample( 1:N_children , size=21 ) ] <- NA  
tea <- rbinom( N_children , size=1 , prob= s*actual_p_s + (1-s)*actual_p_u )

## create data frame of observations
## NA is used to indicate an enclosed barn 
# (hence, data regarding ox is censored or unobserved)
dataDF = tibble(s_obs,tea)

s_obs_only = s_obs[!is.na(s_obs)]
s_missing = s_obs[is.na(s_obs)]
tea_obs = tea[!is.na(s_obs)]
tea_missing = tea[is.na(s_obs)]

graph = dag_create() %>%
  dag_node(descr = "Drinking Tea - observe case",label = "tea_obs",
           rhs = binomial(1, pi_obs),
           data = tea_obs) %>%
  dag_node(descr = "Drinking Tea - missing case", label = "tea_missing",
           rhs = binomial(1, pi_missing),
           data = tea_missing) %>%
  dag_node(descr = "Probability of drinking tea-observed",label = "pi_obs",
           rhs = s_obs * p_s + (1 - s_obs) * p_u,
           child = "tea_obs") %>%
  dag_node(descr = "Probability of drinking tea- ubobserved",label = "pi_missing",
           rhs = s_missing * p_s + (1 - s_missing) * p_u,
           child = "tea_missing") %>%
  dag_node(descr = "Prob of Drinking if Stable",label = "p_s",
           child = c("pi_obs", "pi_missing"),
           rhs = beta(2,2)) %>%
  dag_node(descr = "Prob of Drinking if not Stable",label = "p_u",
           child = c("pi_obs", "pi_missing"),
           rhs = beta(2,2)) %>%
  dag_node(descr = "Stable Indicator - observed ",label = "s_obs",
           child = "pi_obs",
           rhs = binomial(1, sigma),
           data = s_obs_only) %>%
  dag_node(descr = "Stable Indicator - missing ",label = "s_missing",
           child = "pi_missing",
           rhs = binomial(1, sigma),
           data = s_missing) %>%
  dag_node(descr = "Prob of Stable",label = "sigma",
           child = c("s_obs", "s_missing"),
           rhs = beta(2,2))



graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) ## did not converge => use NUTS 

summary(draws_df)


graph$nodes_df