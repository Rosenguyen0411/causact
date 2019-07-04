remove.packages("causact")
remotes::install_github("Rosenguyen0411/causact")

########### SET UP
# if the JuliaCall package has not been installed, run the following line
#install.packages("JuliaCall")

library(JuliaCall)

# if there is a StatsModels error => in Julia type: ] add StatsModels@0.5

## initial setup, need to provide JULIA_HOME
julia <- julia_setup(JULIA_HOME="/Applications/Julia-1.1.app/Contents/Resources/julia/bin/")

## include Julia package, need to install these packages in Julia first
julia_library(pkg_name = "Turing")
julia_library(pkg_name = "MCMCChains")
julia_library(pkg_name = "Distributions")
julia_library(pkg_name = "DataFrames")
julia_library(pkg_name = "StatsFuns")
julia_library(pkg_name = "Statistics")


library(causact)
library(tidyverse)
library(greta)
library(rethinking)

################ Simple coin flip example
###### RUN IN 18 SECONDS FOR NUTS ###########

data = rbern(1000) # 1000 flips

graph = dag_create() %>%
  dag_node(descr = "data", label = "d",
           rhs = bernoulli(theta),
           data = data) %>%
  dag_node(descr = "Card Probability", label = "theta",
           rhs = beta(1,1),
           child = "d") 
graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS = TRUE)
graph %>% dag_julia(HMC = TRUE)

summary(draws_df)


############ CAR EXAMPLE
#########  RUN IN 35 SECONDS FOR NUTS ###########

graph = dag_create() %>%
  dag_node(descr = "Get Card", label = "y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability", label = "theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_plate(descr = "Car Model", label = "x",  
            data = carModelDF$carModel,  
            nodeLabels = "theta",  
            addDataNode = TRUE)
graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS = TRUE)
graph %>% dag_julia(HMC = TRUE)

summary(draws_df)


############## Statistical Rethinking - Milk model (m5.7), linear regression of Kcal on neocortex and mass

########## RUN IN 2.27 SECONDS !!!!!!!!!!!!!

## Get the data ready

data(milk)
milk = milk
milk<- milk[ complete.cases(milk$neocortex.perc) , ]
K <- scale( milk$kcal.per.g )
N <- scale( milk$neocortex.perc )
M <- scale( log(milk$mass))


graph = dag_create() %>%
  dag_node(descr = "Kcal", label = "K",
           rhs = normal(mu, sigma),
           data = K) %>%
  dag_node(descr = "Variation", label = "sigma",
           rhs = exponential(1),
           child = "K") %>%
  dag_node(descr = "Mean", label = "mu",
           rhs = a + bN * N + bM * M,
           child = "K")  %>%
  dag_node(descr = "Intercept", label = "a",
          rhs = normal(0, 0.2),
          child = "mu") %>%
  dag_node(descr = "Slope for Neocortex", label = "bN",
          rhs = normal(0, 0.5),
          child = "mu") %>%
  dag_node(descr = "Slope for Mass", label = "bM",
          rhs = normal(0, 0.5),
          child = "mu") %>%
  dag_node(descr = "Neo", label = "N",
           data = N,
           child = "mu") %>%
  dag_node(descr = "Mass", label = "M",
           data = M,
           child = "mu")
graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE)

summary(draws_df)

############# Statistical Rethinking - Milk model (m5.10), with categorical variables: clade and house

######## RUN IN 8.3 SECONDS ##########

data(milk)
d <- milk
unique(d$clade)
K <- milk$kcal.per.g
clade_id <-  d$clade
set.seed(63)
house <- sample( rep(1:4,each=8) , size=nrow(d) )


graph = dag_create() %>%
  dag_node(descr = "Kcal", label = "K",
           rhs = normal(mu, sigma),
           data = K) %>%
  dag_node(descr = "Variation", label = "sigma",
           rhs = exponential(1),
           child = "K") %>%
  dag_node(descr = "Mean", label = "mu",
           rhs = a + h,
           child = "K")  %>%
  dag_node(descr = "clade", label = "a",
           rhs = normal(0, 0.5),
           child = "mu") %>%
  dag_node(descr = "house", label = "h",
           rhs = normal(0, 0.5),
           child = "mu") %>%
  dag_plate(descr = "cladeID", label = "clade",
            data = clade_id,
            nodeLabels = "a",
            addDataNode = TRUE) %>%
  dag_plate(descr = "houseID", label = "house",
            data = house,
            nodeLabels = "h",
            addDataNode = TRUE)

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE)

summary(draws_df)

############# Statistical Rethinking - Rugged model (m8.5), linear regression of log_gdp on rugged, 
############## for african and non-african countries

############ RUN IN 35 SECONDS ##########

library(rethinking)
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
rugged_std <- dd$rugged / max(dd$rugged)
# make variable to index Africa (1) or not (2)
cid <- ifelse( dd$cont_africa==1 , 1 , 2 )


graph = dag_create() %>%
  dag_node(descr = "log gdp", label = "log_gdp",
           rhs = normal(mu, sigma),
           data = log_gdp_std) %>%
  dag_node(descr = "Variation", label = "sigma",
           rhs = exponential(1),
           child = "log_gdp") %>%
  dag_node(descr = "Mean", label = "mu",
           rhs = a + b * (rugged - 0.215),
           child = "log_gdp")  %>%
  dag_node(descr = "ruggedness", label = "rugged",
           data = rugged_std,
           child = "mu") %>%
  dag_node(descr = "intercept", label = "a",
           rhs = normal(1, 0.1),
           child = "mu") %>%
  dag_node(descr = "slope", label = "b",
           rhs = normal(0, 0.3),
           child = "mu") %>%
  dag_plate(descr = "African index", label = "cid",
            data = cid,
            nodeLabels = c("b", "a"),
            addDataNode = TRUE)

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE)

summary(draws_df)


############# Statistical Rethinking - Tulips model (m8.7), linear regression of bloom on water, shade and water*shade 

########### RUN IN 3 SECONDS ##################

library(rethinking)
data(tulips)
d <- tulips
blooms_std <- d$blooms / max(d$blooms)
water_cent <- d$water - mean(d$water)
shade_cent <- d$shade - mean(d$shade)


graph = dag_create() %>%
  dag_node(descr = "Bloom std", label = "bloom",
           rhs = normal(mu, sigma),
           data = blooms_std) %>%
  dag_node(descr = "Variation", label = "sigma",
           rhs = exponential(1),
           child = "bloom") %>%
  dag_node(descr = "Mean", label = "mu",
           rhs = a + bw * water_cent + bs * shade_cent + bws * water_cent * shade_cent,
           child = "bloom")  %>%
  dag_node(descr = "Water", label = "water_cent",
           data = water_cent,
           child = "mu") %>%
  dag_node(descr = "Shade", label = "shade_cent",
           data = shade_cent,
           child = "mu") %>%
  dag_node(descr = "intercept", label = "a",
           rhs = normal(0.5, 0.25),
           child = "mu") %>%
  dag_node(descr = "water slope", label = "bw",
           rhs = normal(0, 0.25),
           child = "mu") %>%
  dag_node(descr = "shade slope", label = "bs",
           rhs = normal(0, 0.25),
           child = "mu") %>%
  dag_node(descr = "water and shade slope", label = "bws",
           rhs = normal(0, 0.25),
           child = "mu")

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) ## reject alot of proposal, big std => NUTS is better

summary(draws_df)


############# Statistical Rethinking - Chapter 11- Chimpanzees model (m11.3)
######## RUN IN 36 SECONDS for NUTS and 21 SECONDS for HMC ############

library(rethinking)
data(chimpanzees)
d <- chimpanzees
pulled_left <- as.integer(d$pulled_left)
condition <- d$condition
prosoc_left <- d$prosoc_left


graph = dag_create() %>%
  dag_node(descr = "Pull left", label = "pull",
           rhs = binomial(1L, p),
           data = pulled_left) %>%
  dag_node(descr = "Pull Probability", label = "p",
           rhs = ilogit(logit_p),
           child = "pull") %>%
  dag_node(descr = "Logit Probability", label = "logit_p",
           rhs = a + bp * prosoc_left + bpC * condition * prosoc_left,
           child = "p")  %>%
  dag_node(descr = "condition", label = "condition",
           data = condition,
           child = "logit_p") %>%
  dag_node(descr = "prosoc_left", label = "prosoc_left",
           data = prosoc_left,
           child = "logit_p") %>%
  dag_node(descr = "intercept", label = "a",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_node(descr = "prosoc slope", label = "bp",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_node(descr = "prosoc and condition slope", label = "bpC",
           rhs = normal(0, 10),
           child = "logit_p")

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) ## very big std => use NUTS

summary(draws_df)


############# Statistical Rethinking - Chapter 11 - Chimpanzees model (m11.4), each intercept for each actor (7 actors)

########### RUN FOR 90 SECONDS FOR NUTS AND 26 SECONDS FOR HMC ###########

## data
library(rethinking)
data(chimpanzees)
d <- chimpanzees
pulled_left <- as.integer(d$pulled_left)
condition <- d$condition
prosoc_left <- d$prosoc_left
actor <- d$actor


graph = dag_create() %>%
  dag_node(descr = "Pull left", label = "pull",
           rhs = binomial(1L, p),
           data = pulled_left) %>%
  dag_node(descr = "Pull Probability", label = "p",
           rhs = ilogit(logit_p),
           child = "pull") %>%
  dag_node(descr = "Logit Probability", label = "logit_p",
           rhs = a + bp * prosoc_left + bpC * condition * prosoc_left,
           child = "p")  %>%
  dag_node(descr = "condition", label = "condition",
           data = condition,
           child = "logit_p") %>%
  dag_node(descr = "prosoc_left", label = "prosoc_left",
           data = prosoc_left,
           child = "logit_p") %>%
  dag_node(descr = "intercept", label = "a",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_node(descr = "prosoc slope", label = "bp",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_node(descr = "prosoc and condition slope", label = "bpC",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_plate(descr = "actor indicator", label = "actor",
            nodeLabels = "a",
            data = actor,
            addDataNode = TRUE)

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) 

summary(draws_df)


############# Statistical Rethinking - Chapter 11 - UCBadmit model (m11.9), each intercept for each department
################# RUN IN 5.3 SECONDS FOR NUTS ###################
## data
library(rethinking)
data(UCBadmit)
d <- UCBadmit
male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
dept_id <- coerce_index( d$dept )
admit <- d$admit
applications <- d$applications

graph = dag_create() %>%
  dag_node(descr = "Admission Decision", label = "admit",
           rhs = binomial(applications, p),
           data = admit) %>%
  dag_node(descr = "Admit Probability", label = "p",
           rhs = ilogit(logit_p),
           child = "admit") %>%
  dag_node(descr = "Logit Probability", label = "logit_p",
           rhs = a + bm * male,
           child = "p")  %>%
  dag_node(descr = "Number of Applications", label = "applications",
           data = applications,
           child = "admit") %>%
  dag_node(descr = "Male Indicator", label = "male",
           data = male,
           child = "logit_p") %>%
  dag_node(descr = "Intercept", label = "a",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_node(descr = "Male Slope", label = "bm",
           rhs = normal(0, 10),
           child = "logit_p") %>%
  dag_plate(descr = "Department Indicator", label = "dept_id",
            nodeLabels = "a",
            data = dept_id,
            addDataNode = TRUE)

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) ## very big std => use NUTS 

summary(draws_df)



############# Statistical Rethinking - Chapter 11 - Kline model (m11.10),
#### RUN IN 10.2 SECONDS FOR NUTS #########

# data
library(rethinking)
data(Kline)
d <- Kline

log_pop <- log(d$population)
contact_high <- ifelse( d$contact=="high" , 1 , 0 )
total_tools <- d$total_tools


graph = dag_create() %>%
  dag_node(descr = "Total Number of Tools", label = "total_tools",
           rhs = poisson(lambda),
           data = total_tools) %>%
  dag_node(descr = "Tool Rate", label = "lambda",
           rhs = exp(log_lambda),
           child = "total_tools") %>%
  dag_node(descr = "Log Tool Rate", label = "log_lambda",
           rhs = a + bp * log_pop + bc * contact_high + bpc * contact_high * log_pop,
           child = "lambda")  %>%
  dag_node(descr = "Log of Population", label = "log_pop",
           data = log_pop,
           child = "log_lambda") %>%
  dag_node(descr = "Contact Rate", label = "contact_high",
           data = contact_high,
           child = "log_lambda") %>%
  dag_node(descr = "Intercept", label = "a",
           rhs = normal(0, 100),
           child = "log_lambda") %>%
  dag_node(descr = "Log Population Slope", label = "bp",
           rhs = normal(0, 1),
           child = "log_lambda") %>%
  dag_node(descr = "Contact Rate Slope", label = "bc",
           rhs = normal(0, 1),
           child = "log_lambda") %>%
  dag_node(descr = "Log Population and Contact Rate Slope", label = "bpc",
           rhs = normal(0, 1),
           child = "log_lambda") 

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) ## did not converge => use NUTS 

summary(draws_df)


############# Statistical Rethinking - Chapter 13 - Reedfrogs model (m13.2), 50 parameters
################## RUN from 60 to 150 SECONDS FOR NUTS ################

library(rethinking)
data(reedfrogs)
d <- reedfrogs
tank <- 1:nrow(d)
surv <- d$surv
density <- d$density


graph = dag_create() %>%
  dag_node(descr = "Number of Frog Survive", label = "surv",
           rhs = binomial(density, p),
           data = surv) %>%
  dag_node(descr = "Survive Rate", label = "p",
           rhs = ilogit(logit_p),
           child = "surv") %>%
  dag_node(descr = "Logit Survive Rate", label = "logit_p",
           rhs = a_tank,
           child = "p")  %>%
  dag_node(descr = "Pond Population", label = "density",
           data = density,
           child = "surv") %>%
  dag_node(descr = "Intercept", label = "a_tank",
           rhs = normal(a, sigma),
           child = "logit_p") %>%
  dag_node(descr = "Mean Survival Rate", label = "a",
           rhs = normal(0, 1),
           child = "a_tank") %>%
  dag_node(descr = "Variation", label = "sigma",
           rhs = exponential(1),
           child = "a_tank") %>%
  dag_plate(descr = "Tank Indicator", label = "tank",
            nodeLabels = "a_tank",
            data = tank,
            addDataNode = TRUE)

graph %>% dag_render()
#graph %>% dag_greta()
graph %>% dag_julia(NUTS= TRUE)
graph %>% dag_julia(HMC= TRUE) ## did not converge => use NUTS 

summary(draws_df)



