remove.packages("causact")
remotes::install_github("Rosenguyen0411/causact")

########### SET UP
library(JuliaCall)

## initial setup
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


############ CAR EXAMPLE
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



############## Statistical Rethinking - Milk model (m5.7), linear regression of Kcal on neocortex and mass

########## SIGMA NOT DEFINED ERROR!!!!!!!!!!!!!

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


############# Statistical Rethinking - Milk model (m5.10), with categorical variables: clade and house

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


############# Statistical Rethinking - Rugged model (m8.5), linear regression of log_gdp on rugged, 
############## for african and non-african countries
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


