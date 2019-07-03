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



############# Statistical Rethinking - Tulips model (m8.7), linear regression of bloom on water, shade and water*shade 
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
graph %>% dag_julia(HMC= TRUE)


############# Statistical Rethinking - Chimpanzees model (m11.3)
######### HAVE TO STRIP OFF ARGUMENT NAME IN ILOGIT

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
graph %>% dag_julia(HMC= TRUE)


## The specified DAG corresponds to the following Julia code: 
prosoc_left <- prosoc_left   #DATA
condition <- condition       #DATA
pull <- pulled_left          #DATA
julia_command("@model julia_model(prosoc_left,condition,pull) = begin    #MODEL
bpC    = Array{Float64}(undef,1)
bpC    ~ Normal(0, 10) #PRIOR
bp     = Array{Float64}(undef,1)
bp     ~ Normal(0, 10) #PRIOR
a      = Array{Float64}(undef,1)
a      ~ Normal(0, 10) #PRIOR
logit_p = a .+ bp .* prosoc_left .+ bpC .* condition .* prosoc_left   #OPERATION
p       = logistic.(logit_p)                                      #OPERATION
for i in 1:length(pull  ) 
 pull[i] ~Binomial(1, p[i])
 end 
 end;")   #LIKELIHOOD
model  =  julia_call("julia_model", prosoc_left,condition,pull)   #CALL MODEL
#Choose one of these 2 following engine:
engine  =  julia_call("NUTS", 4000L,0.65)   #CALL NUTS SAMPLER
#engine  =  julia_call("HMC", 4000L,0.1,10L)   #CALL HMC SAMPLER
draws  =  julia_call("sample", model, engine)
draws_df  =  julia_call("DataFrame", draws)    #SAMPLING
