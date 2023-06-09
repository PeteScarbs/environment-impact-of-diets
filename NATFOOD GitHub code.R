### EPIC Oxford Environmental score uncertainty analyses ###

# Last updated: 9th JUne 2023

# This R script supports the analyses for the following paper:
# Scarborough P, Clark M, Cobiac LJ, Papier K, Knuppel A, Lynch J, Harrington RA, Key T, Springmann M. Vegans, vegetarians, fish-eaters and meat-eaters in the UK show discrepant environmental impacts. Nature Food, 2023.

### Description of primary dataset:
## Name: Results_21Mar2022
## Variables:
# mc_run_id {1-1000 indicating which iteration the result is taken from}
# grouping {combined grouping of diet_group and age_group}
# mean_ghgs {Mean for diet + age + sex group for GHG emissions}
# mean_land {Mean for diet + age + sex group for land use}
# mean_watscar {Mean for diet + age + sex group for water scarcity measure}
# mean_eut {Mean for diet + age + sex group for eutrophication}
# mean_ghgs_ch4 {Mean for diet + age + sex group for methane}
# mean_ghgs_n2o {Mean for diet + age + sex group for nitrous oxide}
# mean_bio {Mean for diet + age + sex group for biodiversity measure}
# mean_watuse {Mean for diet + age + sex group for water use}
# mean_acid {Mean for diet + age + sex group for acidification (land-based version of eutrophication)}
# sd_ghgs {SD for diet + age + sex group for GHG emissions}
# sd_land {SD for diet + age + sex group for land use}
# sd_watscar {SD for diet + age + sex group for water scarcity measure}
# sd_eut {SD for diet + age + sex group for eutrophication}
# sd_ghgs_ch4 {SD for diet + age + sex group for methane}
# sd_ghgs_n2o {SD for diet + age + sex group for nitrous oxide}
# sd_bio {SD for diet + age + sex group for biodiversity measure}
# sd_watuse {SD for diet + age + sex group for water use}
# sd_acid {SD for diet + age + sex group for acidification}
# n_participants {number of individuals in each diet + age + sex group}
# sex {female; male}
# diet_group {fish; meat; meat50; meat100; vegan; veggie} NB: meat50 = low meat eaters; meat = medium meat eaters; meat100 = high meat eaters
# age_group {20-29; 30-39; 40-49; 50-59; 60-69; 70-79}

### Description of secondary dataset:
## Name: Results_21Mar2022_nokcaladjust. NB: the date refers to the fact that this uses exactly the same data and runs as
#                                            the primary dataset. The dataset was actually produced in April 2023.
# Variables:
# Identical to the primary dataset, except all results are no longer standardised to 2000kcal


# Install packages
#install.packages("openxlsx")
library("openxlsx")
library("Rcpp")

# Open datasets
dirname <- "ADD HERE"
setwd(dirname)

# agesexdietdata <- "Results_21Mar2022.csv" # For primary analysis
agesexdietdata <- "Results_21Mar2022_nokcaladjust.csv" # For secondary analysis
itdb <- read.csv(agesexdietdata)


#################################################################################################
#                                                                                               #
# DISAGGREGATING CO2                                                                            #
#                                                                                               #
#################################################################################################

# Mike C's analyses provide 72k iterations with GHG estimates for CO2e, CH4 and N2O. To estimate CO2, we need to back
# transform the CO2e estimates, using the conversion factors that Mike has used. This is done line by line, so when
# compiled (below) the results will be comparable to how the other gases have been compiled.

# Function to generate CO2 estimates
# NB: In Mike's dataset, the CH4 and N2O data have already been converted to CO2e, so this is simply removing them
co2gen <- function(co2e, ch4, n2o) {
  co2e - ch4 - n2o
}

# Apply to the dataset
itdb$mean_ghgs_co2 <- mapply(co2gen, 
                             co2e = itdb$mean_ghgs,
                             ch4 = itdb$mean_ghgs_ch4,
                             n2o = itdb$mean_ghgs_n2o)


#################################################################################################
#                                                                                               #
# GENERATING NEW AGGREGATE GHG MEASURES                                                         #
#                                                                                               #
#################################################################################################

# First need to reset the CH4 and N2O data, by removing the conversion factors that Mike C applied (that transform them to CO2e)

convert_ch4_MC <- 34
convert_n2o_MC <- 298

# CH4 and N2O untransformed
itdb$mean_ghgs_ch4_nont <- itdb$mean_ghgs_ch4/convert_ch4_MC
itdb$mean_ghgs_n2o_nont <- itdb$mean_ghgs_n2o/convert_n2o_MC

# Generate three new GHG aggregate terms for the paper

co2efun <- function(co2, ch4, n2o, ch4convert, n2oconvert){
  co2 + ch4*ch4convert + n2o*n2oconvert
}

itdb$gwp100 <- mapply(co2efun, # This uses IPCC AR6 conversion factors
                      co2 = itdb$mean_ghgs_co2,
                      ch4 = itdb$mean_ghgs_ch4_nont,
                      n2o = itdb$mean_ghgs_n2o_nont,
                      ch4convert = 27,
                      n2oconvert = 273)

itdb$gtp100 <- mapply(co2efun, 
                      co2 = itdb$mean_ghgs_co2,
                      ch4 = itdb$mean_ghgs_ch4_nont,
                      n2o = itdb$mean_ghgs_n2o_nont,
                      ch4convert = 11,
                      n2oconvert = 297)

itdb$gwp20  <- mapply(co2efun, 
                      co2 = itdb$mean_ghgs_co2,
                      ch4 = itdb$mean_ghgs_ch4_nont,
                      n2o = itdb$mean_ghgs_n2o_nont,
                      ch4convert = 86,
                      n2oconvert = 268)

#################################################################################################
#                                                                                               #
# UNCERTAINTY ANALYSIS FOR BOTH SEXES COMBINED                                                  #
#                                                                                               #
#################################################################################################

# The secondary results reported in the paper come from regression analyses, which are shown in the log files shared by 
# Keren Papier on 24/01/2022. The log files are available in K:\CPNP\Pete\ENVIRONMENTAL SUSTAINABILITY\OTHER PROJECTS\LEAP\PAPERS\EPIC OXFORD 2
# For each environmental outcome, a regression analysis is conducted adjusted for age (and sex, when results are for
# adults). The 'margins' command in Stata is then used to produce a marginal analysis by diet group. See an explanation
# for marginal analyses in Stata here: https://www.stata.com/features/overview/marginal-analysis/
# Fundamentally, it uses the regression output to calculate a result for each diet group assuming average levels of
# all covariates. 

# This is equivalent to producing results for the Monte Carlo analysis that are standardised to the EPIC Oxford 
# sample. Since all of the food parameter distributions that are combined in the Monte Carlo analysis are 
# lognormal and heavily right-skewed, the results of the uncertainty analysis right shifts the estimates by diet group
# (see Combining lognormal distributions.R for details). Therefore, the results of the uncertainty analysis and the 
# regressions are not equal, but are conducted using equivalent methods to allow for comparisons between them.

# First, enter the number of participants by age and sex in the full sample (i.e. not disaggregated by diet group)
# Note that in the names below, 2 = 20-29; 3 = 30-39 and so on.
# These are provided in email from Keren Papier 25/01/2022
n.male.2   <- 1421
n.male.3   <- 2818 
n.male.4   <- 3285
n.male.5   <- 2374
n.male.6   <- 1981
n.male.7   <- 787
n.female.2 <- 7394
n.female.3 <- 9645
n.female.4 <- 11342
n.female.5 <- 8388
n.female.6 <- 4455
n.female.7 <- 1614

# Check this sums to appropriate number (n = 55,504)
sum(n.male.2,
    n.male.3,
    n.male.4,
    n.male.5,
    n.male.6,
    n.male.7,
    n.female.2,
    n.female.3,
    n.female.4,
    n.female.5,
    n.female.6,
    n.female.7)

## Collapse onto diet groups
# Here, the mean (weighted by number of participants) for each diet group across all iterations is calculated

# Variable for display of results
dietindex <- c("vegan",
               "veggie",
               "fish",
               "meat50",
               "meat",
               "meat100")

# Function for standardising results to the EPIC Oxford population
all.adult.standard <- function(male.2,
                               male.3,
                               male.4,
                               male.5,
                               male.6,
                               male.7,
                               female.2,
                               female.3,
                               female.4,
                               female.5,
                               female.6,
                               female.7){
  sum(male.2*n.male.2,  # This produces a weighted mean, that is weighted by the proportion of participants in each group
      male.3*n.male.3,
      male.4*n.male.4,
      male.5*n.male.5,
      male.6*n.male.6,
      male.7*n.male.7,
      female.2*n.female.2,
      female.3*n.female.3,
      female.4*n.female.4,
      female.5*n.female.5,
      female.6*n.female.6,
      female.7*n.female.7) / sum(n.male.2,
                                 n.male.3,
                                 n.male.4,
                                 n.male.5,
                                 n.male.6,
                                 n.male.7,
                                 n.female.2,
                                 n.female.3,
                                 n.female.4,
                                 n.female.5,
                                 n.female.6,
                                 n.female.7)
}

# Check whether standardising results from the regression works:
all.adult.standard(male.2 = 10.44407,
                   male.3 = 10.44407 + 0.034442,
                   male.4 = 10.44407 + 0.2183768,
                   male.5 = 10.44407 + 0.3909661,
                   male.6 = 10.44407 + 0.2853161,
                   male.7 = 10.44407 + 0.1492136,
                   female.2 = 10.44407 + 0.4991238,
                   female.3 = 10.44407 + 0.034442 + 0.4991238,
                   female.4 = 10.44407 + 0.2183768 + 0.4991238,
                   female.5 = 10.44407 + 0.3909661 + 0.4991238,
                   female.6 = 10.44407 + 0.2853161 + 0.4991238,
                   female.7 = 10.44407 + 0.1492136 + 0.4991238) # That recreates the margins value for high meat eaters perfectly.


### LAND USE RESULTS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("land.",i),temp)
}

## Calculate 95% uncertainty intervals and median
land.UI <- data.frame(diet.group = character(),
                      median = numeric(),
                      lowUI = numeric(),
                      highUI = numeric())

for (i in 1:6){
  land.UI[i,] <- c(dietindex[i],
                   eval(parse(text=paste0("as.numeric(quantile(land.",dietindex[i],"[,2], 0.5))"))),
                   eval(parse(text=paste0("as.numeric(quantile(land.",dietindex[i],"[,2], 0.025))"))),
                   eval(parse(text=paste0("as.numeric(quantile(land.",dietindex[i],"[,2], 0.975))"))))
}

### GHG EMISSION RESULTS
## GWP100

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$gwp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("gwp100.",i),temp)
}

## Calculate 95% uncertainty intervals and median
gwp100.UI  <- data.frame(diet.group = character(),
                         median = numeric(),
                         lowUI = numeric(),
                         highUI = numeric())

for (i in 1:6){
  gwp100.UI[i,]  <- c(dietindex[i],
                      eval(parse(text=paste0("as.numeric(quantile(gwp100.",dietindex[i],"[,2], 0.5))"))),
                      eval(parse(text=paste0("as.numeric(quantile(gwp100.",dietindex[i],"[,2], 0.025))"))),
                      eval(parse(text=paste0("as.numeric(quantile(gwp100.",dietindex[i],"[,2], 0.975))"))))
}

## GWP20

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$gwp20[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("gwp20.",i),temp)
}

## Calculate 95% uncertainty intervals and median
gwp20.UI  <- data.frame(diet.group = character(),
                         median = numeric(),
                         lowUI = numeric(),
                         highUI = numeric())

for (i in 1:6){
  gwp20.UI[i,]  <- c(dietindex[i],
                      eval(parse(text=paste0("as.numeric(quantile(gwp20.",dietindex[i],"[,2], 0.5))"))),
                      eval(parse(text=paste0("as.numeric(quantile(gwp20.",dietindex[i],"[,2], 0.025))"))),
                      eval(parse(text=paste0("as.numeric(quantile(gwp20.",dietindex[i],"[,2], 0.975))"))))
}

## GTP100

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$gtp100[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("gtp100.",i),temp)
}

## Calculate 95% uncertainty intervals and median
gtp100.UI  <- data.frame(diet.group = character(),
                         median = numeric(),
                         lowUI = numeric(),
                         highUI = numeric())

for (i in 1:6){
  gtp100.UI[i,]  <- c(dietindex[i],
                      eval(parse(text=paste0("as.numeric(quantile(gtp100.",dietindex[i],"[,2], 0.5))"))),
                      eval(parse(text=paste0("as.numeric(quantile(gtp100.",dietindex[i],"[,2], 0.025))"))),
                      eval(parse(text=paste0("as.numeric(quantile(gtp100.",dietindex[i],"[,2], 0.975))"))))
}



### WATER SCARCITY RESULTS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("watscar.",i),temp)
}

## Calculate 95% uncertainty intervals and median
watscar.UI  <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

for (i in 1:6){
  watscar.UI[i,]  <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(watscar.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(watscar.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(watscar.",dietindex[i],"[,2], 0.975))"))))
}

### WATER USE RESULTS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("watuse.",i),temp)
}

## Calculate 95% uncertainty intervals and median
watuse.UI  <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

for (i in 1:6){
  watuse.UI[i,]  <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(watuse.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(watuse.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(watuse.",dietindex[i],"[,2], 0.975))"))))
}

### EUTROPHICATION RESULTS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("eut.",i),temp)
}

## Calculate 95% uncertainty intervals and median
eut.UI  <- data.frame(diet.group = character(),
                      median = numeric(),
                      lowUI = numeric(),
                      highUI = numeric())

for (i in 1:6){
  eut.UI[i,]  <- c(dietindex[i],
                   eval(parse(text=paste0("as.numeric(quantile(eut.",dietindex[i],"[,2], 0.5))"))),
                   eval(parse(text=paste0("as.numeric(quantile(eut.",dietindex[i],"[,2], 0.025))"))),
                   eval(parse(text=paste0("as.numeric(quantile(eut.",dietindex[i],"[,2], 0.975))"))))
}

### CARBON DIOXIDE RESULTS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$mean_ghgs_co2[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("co2.",i),temp)
}

## Calculate 95% uncertainty intervals and median
co2.UI  <- data.frame(diet.group = character(),
                      median = numeric(),
                      lowUI = numeric(),
                      highUI = numeric())

for (i in 1:6){
  co2.UI[i,]  <- c(dietindex[i],
                   eval(parse(text=paste0("as.numeric(quantile(co2.",dietindex[i],"[,2], 0.5))"))),
                   eval(parse(text=paste0("as.numeric(quantile(co2.",dietindex[i],"[,2], 0.025))"))),
                   eval(parse(text=paste0("as.numeric(quantile(co2.",dietindex[i],"[,2], 0.975))"))))
}

### METHANE RESULTS
# NB: ALL RESULTS MULTIPLIED BY 1000 TO CONVERT TO G/D

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = 1000*itdb$mean_ghgs_ch4_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("ch4.",i),temp)
}

## Calculate 95% uncertainty intervals and median
ch4.UI  <- data.frame(diet.group = character(),
                      median = numeric(),
                      lowUI = numeric(),
                      highUI = numeric())

for (i in 1:6){
  ch4.UI[i,]  <- c(dietindex[i],
                   eval(parse(text=paste0("as.numeric(quantile(ch4.",dietindex[i],"[,2], 0.5))"))),
                   eval(parse(text=paste0("as.numeric(quantile(ch4.",dietindex[i],"[,2], 0.025))"))),
                   eval(parse(text=paste0("as.numeric(quantile(ch4.",dietindex[i],"[,2], 0.975))"))))
}

### NITROUS OXIDE RESULTS
# NB: ALL RESULTS MULTIPLIED BY 1000 TO CONVERT TO G/D

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = 1000*itdb$mean_ghgs_n2o_nont[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("n2o.",i),temp)
}

## Calculate 95% uncertainty intervals and median
n2o.UI  <- data.frame(diet.group = character(),
                      median = numeric(),
                      lowUI = numeric(),
                      highUI = numeric())

for (i in 1:6){
  n2o.UI[i,]  <- c(dietindex[i],
                   eval(parse(text=paste0("as.numeric(quantile(n2o.",dietindex[i],"[,2], 0.5))"))),
                   eval(parse(text=paste0("as.numeric(quantile(n2o.",dietindex[i],"[,2], 0.025))"))),
                   eval(parse(text=paste0("as.numeric(quantile(n2o.",dietindex[i],"[,2], 0.975))"))))
}

### BIODIVERSITY RESULTS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,all.adult.standard(male.2 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                     male.3 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                     male.4 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                     male.5 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                     male.6 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                     male.7 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"],
                                     female.2 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                     female.3 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                     female.4 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                     female.5 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                     female.6 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                     female.7 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("bio.",i),temp)
}

## Calculate 95% uncertainty intervals and median
bio.UI  <- data.frame(diet.group = character(),
                      median = numeric(),
                      lowUI = numeric(),
                      highUI = numeric())

for (i in 1:6){
  bio.UI[i,]  <- c(dietindex[i],
                   eval(parse(text=paste0("as.numeric(quantile(bio.",dietindex[i],"[,2], 0.5))"))),
                   eval(parse(text=paste0("as.numeric(quantile(bio.",dietindex[i],"[,2], 0.025))"))),
                   eval(parse(text=paste0("as.numeric(quantile(bio.",dietindex[i],"[,2], 0.975))"))))
}


# Extract results for all adults
extractlist <- list("GWP100" = gwp100.UI,
                    "GWP20" = gwp20.UI,
                    "GTP100" = gtp100.UI,
                    "land" = land.UI,
                    "watscar" = watscar.UI,
                    "watuse" = watuse.UI,
                    "eutro" = eut.UI,
                    "bio" = bio.UI,
                    "carbon dioxide" = co2.UI,
                    "methane" = ch4.UI,
                    "nitrous oxide" = n2o.UI)

# write.xlsx(extractlist, file = "ADD HERE")


#################################################################################################
#                                                                                               #
# UNCERTAINTY ANALYSIS DISAGGREGATED BY SEX                                                     #
#                                                                                               #
#################################################################################################

# For the results disaggregated by sex, the results are only age-standardised to the male or female sample,
# and then results are compiled for men and women separately.

# Functions for standardising results to the EPIC Oxford population
male.standard <- function(male.2,
                          male.3,
                          male.4,
                          male.5,
                          male.6,
                          male.7){
  sum(male.2*n.male.2,  # This produces a weighted mean, that is weighted by the proportion of participants in each group
      male.3*n.male.3,
      male.4*n.male.4,
      male.5*n.male.5,
      male.6*n.male.6,
      male.7*n.male.7) / sum(n.male.2,
                             n.male.3,
                             n.male.4,
                             n.male.5,
                             n.male.6,
                             n.male.7)
}

female.standard <- function(female.2,
                            female.3,
                            female.4,
                            female.5,
                            female.6,
                            female.7){
  sum(female.2*n.female.2,
      female.3*n.female.3,
      female.4*n.female.4,
      female.5*n.female.5,
      female.6*n.female.6,
      female.7*n.female.7) / sum(n.female.2,
                                 n.female.3,
                                 n.female.4,
                                 n.female.5,
                                 n.female.6,
                                 n.female.7)
}

# Check whether standardising results from the regression works:
male.standard(male.2 = 10.26184,
              male.3 = 10.26184 - 0.008085,
              male.4 = 10.26184 + 0.1217943,
              male.5 = 10.26184 + 0.2743658,
              male.6 = 10.26184 + 0.2567864,
              male.7 = 10.26184 + 0.2623666) # That recreates the margins value for high meat eaters perfectly.

female.standard(female.2 = 11.03044,
                female.3 = 11.03044 + 0.0327147,
                female.4 = 11.03044 + 0.2304997,
                female.5 = 11.03044 + 0.409631,
                female.6 = 11.03044 + 0.2864531,
                female.7 = 11.03044 + 0.0751358) # That recreates the margins value for high meat eaters perfectly.


## Collapse onto diet and sex groups

### LAND USE

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("land.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_land[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("land.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
land.male.UI <- data.frame(diet.group = character(),
                           median = numeric(),
                           lowUI = numeric(),
                           highUI = numeric())

land.female.UI <- data.frame(diet.group = character(),
                             median = numeric(),
                             lowUI = numeric(),
                             highUI = numeric())

for (i in 1:6){
  land.male.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(land.male.",dietindex[i],"[,2], 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(land.male.",dietindex[i],"[,2], 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(land.male.",dietindex[i],"[,2], 0.975))"))))
  land.female.UI[i,] <- c(dietindex[i],
                          eval(parse(text=paste0("as.numeric(quantile(land.female.",dietindex[i],"[,2], 0.5))"))),
                          eval(parse(text=paste0("as.numeric(quantile(land.female.",dietindex[i],"[,2], 0.025))"))),
                          eval(parse(text=paste0("as.numeric(quantile(land.female.",dietindex[i],"[,2], 0.975))"))))
}

### GREENHOUSE GAS EMISSIONS

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("ghg.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_ghgs[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("ghg.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
ghg.male.UI <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

ghg.female.UI <- data.frame(diet.group = character(),
                            median = numeric(),
                            lowUI = numeric(),
                            highUI = numeric())

for (i in 1:6){
  ghg.male.UI[i,] <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(ghg.male.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(ghg.male.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(ghg.male.",dietindex[i],"[,2], 0.975))"))))
  ghg.female.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(ghg.female.",dietindex[i],"[,2], 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(ghg.female.",dietindex[i],"[,2], 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(ghg.female.",dietindex[i],"[,2], 0.975))"))))
}

### WATER SCARCITY

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("watscar.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_watscar[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("watscar.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
watscar.male.UI <- data.frame(diet.group = character(),
                              median = numeric(),
                              lowUI = numeric(),
                              highUI = numeric())

watscar.female.UI <- data.frame(diet.group = character(),
                                median = numeric(),
                                lowUI = numeric(),
                                highUI = numeric())

for (i in 1:6){
  watscar.male.UI[i,] <- c(dietindex[i],
                           eval(parse(text=paste0("as.numeric(quantile(watscar.male.",dietindex[i],"[,2], 0.5))"))),
                           eval(parse(text=paste0("as.numeric(quantile(watscar.male.",dietindex[i],"[,2], 0.025))"))),
                           eval(parse(text=paste0("as.numeric(quantile(watscar.male.",dietindex[i],"[,2], 0.975))"))))
  watscar.female.UI[i,] <- c(dietindex[i],
                             eval(parse(text=paste0("as.numeric(quantile(watscar.female.",dietindex[i],"[,2], 0.5))"))),
                             eval(parse(text=paste0("as.numeric(quantile(watscar.female.",dietindex[i],"[,2], 0.025))"))),
                             eval(parse(text=paste0("as.numeric(quantile(watscar.female.",dietindex[i],"[,2], 0.975))"))))
}

### WATER USE

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("watuse.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_watuse[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("watuse.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
watuse.male.UI <- data.frame(diet.group = character(),
                              median = numeric(),
                              lowUI = numeric(),
                              highUI = numeric())

watuse.female.UI <- data.frame(diet.group = character(),
                                median = numeric(),
                                lowUI = numeric(),
                                highUI = numeric())

for (i in 1:6){
  watuse.male.UI[i,] <- c(dietindex[i],
                           eval(parse(text=paste0("as.numeric(quantile(watuse.male.",dietindex[i],"[,2], 0.5))"))),
                           eval(parse(text=paste0("as.numeric(quantile(watuse.male.",dietindex[i],"[,2], 0.025))"))),
                           eval(parse(text=paste0("as.numeric(quantile(watuse.male.",dietindex[i],"[,2], 0.975))"))))
  watuse.female.UI[i,] <- c(dietindex[i],
                             eval(parse(text=paste0("as.numeric(quantile(watuse.female.",dietindex[i],"[,2], 0.5))"))),
                             eval(parse(text=paste0("as.numeric(quantile(watuse.female.",dietindex[i],"[,2], 0.025))"))),
                             eval(parse(text=paste0("as.numeric(quantile(watuse.female.",dietindex[i],"[,2], 0.975))"))))
}


### EUTROPHICATION

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("eut.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_eut[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("eut.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
eut.male.UI <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

eut.female.UI <- data.frame(diet.group = character(),
                            median = numeric(),
                            lowUI = numeric(),
                            highUI = numeric())

for (i in 1:6){
  eut.male.UI[i,] <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(eut.male.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(eut.male.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(eut.male.",dietindex[i],"[,2], 0.975))"))))
  eut.female.UI[i,] <- c(dietindex[i],
                         eval(parse(text=paste0("as.numeric(quantile(eut.female.",dietindex[i],"[,2], 0.5))"))),
                         eval(parse(text=paste0("as.numeric(quantile(eut.female.",dietindex[i],"[,2], 0.025))"))),
                         eval(parse(text=paste0("as.numeric(quantile(eut.female.",dietindex[i],"[,2], 0.975))"))))
}

### METHANE

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("ch4.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_ghgs_ch4[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("ch4.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
ch4.male.UI <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

ch4.female.UI <- data.frame(diet.group = character(),
                            median = numeric(),
                            lowUI = numeric(),
                            highUI = numeric())

for (i in 1:6){
  ch4.male.UI[i,] <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(ch4.male.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(ch4.male.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(ch4.male.",dietindex[i],"[,2], 0.975))"))))
  ch4.female.UI[i,] <- c(dietindex[i],
                         eval(parse(text=paste0("as.numeric(quantile(ch4.female.",dietindex[i],"[,2], 0.5))"))),
                         eval(parse(text=paste0("as.numeric(quantile(ch4.female.",dietindex[i],"[,2], 0.025))"))),
                         eval(parse(text=paste0("as.numeric(quantile(ch4.female.",dietindex[i],"[,2], 0.975))"))))
}

### NITROUS OXIDE

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("n2o.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_ghgs_n2o[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("n2o.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
n2o.male.UI <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

n2o.female.UI <- data.frame(diet.group = character(),
                            median = numeric(),
                            lowUI = numeric(),
                            highUI = numeric())

for (i in 1:6){
  n2o.male.UI[i,] <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(n2o.male.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(n2o.male.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(n2o.male.",dietindex[i],"[,2], 0.975))"))))
  n2o.female.UI[i,] <- c(dietindex[i],
                         eval(parse(text=paste0("as.numeric(quantile(n2o.female.",dietindex[i],"[,2], 0.5))"))),
                         eval(parse(text=paste0("as.numeric(quantile(n2o.female.",dietindex[i],"[,2], 0.025))"))),
                         eval(parse(text=paste0("as.numeric(quantile(n2o.female.",dietindex[i],"[,2], 0.975))"))))
}

### BIODIVERSITY

# Compile results by diet group
for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,male.standard(male.2 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="20-29"],
                                male.3 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="30-39"],
                                male.4 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="40-49"],
                                male.5 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="50-59"],
                                male.6 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="60-69"],
                                male.7 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="male"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("bio.male.",i),temp)
}

for (i in dietindex) {
  temp <- NULL
  for (j in 1:1000) {
    output <- c(j,female.standard(female.2 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="20-29"],
                                  female.3 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="30-39"],
                                  female.4 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="40-49"],
                                  female.5 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="50-59"],
                                  female.6 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="60-69"],
                                  female.7 = itdb$mean_bio[itdb$mc_run_id==j&itdb$diet_group==i&itdb$sex=="female"&itdb$age_group=="70-79"]))
    temp <- rbind(temp,output)
  }
  assign(paste0("bio.female.",i),temp)
}


## Calculate 95% uncertainty intervals and median
bio.male.UI <- data.frame(diet.group = character(),
                          median = numeric(),
                          lowUI = numeric(),
                          highUI = numeric())

bio.female.UI <- data.frame(diet.group = character(),
                            median = numeric(),
                            lowUI = numeric(),
                            highUI = numeric())

for (i in 1:6){
  bio.male.UI[i,] <- c(dietindex[i],
                       eval(parse(text=paste0("as.numeric(quantile(bio.male.",dietindex[i],"[,2], 0.5))"))),
                       eval(parse(text=paste0("as.numeric(quantile(bio.male.",dietindex[i],"[,2], 0.025))"))),
                       eval(parse(text=paste0("as.numeric(quantile(bio.male.",dietindex[i],"[,2], 0.975))"))))
  bio.female.UI[i,] <- c(dietindex[i],
                         eval(parse(text=paste0("as.numeric(quantile(bio.female.",dietindex[i],"[,2], 0.5))"))),
                         eval(parse(text=paste0("as.numeric(quantile(bio.female.",dietindex[i],"[,2], 0.025))"))),
                         eval(parse(text=paste0("as.numeric(quantile(bio.female.",dietindex[i],"[,2], 0.975))"))))
}


# Extract results disaggregated by sex
extractlist <- list("GHGs m" = ghg.male.UI,
                    "GHGs f" = ghg.female.UI,
                    "land m" = land.male.UI,
                    "land f" = land.female.UI,
                    "watscar m" = watscar.male.UI,
                    "watscar f" = watscar.female.UI,
                    "watuse m" = watuse.male.UI,
                    "watuse f" = watuse.female.UI,
                    "eutro m" = eut.male.UI,
                    "eutro f" = eut.female.UI,
                    "bio m" = bio.male.UI,
                    "bio f" = bio.female.UI,
                    "methane m" = ch4.male.UI,
                    "methane f" = ch4.female.UI,
                    "nitrous oxide m" = n2o.male.UI,
                    "nitrous oxide f" = n2o.female.UI)

#write.xlsx(extractlist, file = "ADD HERE")


#################################################################################################
#                                                                                               #
# CALCULATE RATIOS (AND UIs) FOR ENVIRONMENTAL IMPACT BY DIET GROUP                             #
#                                                                                               #
#################################################################################################

# This section calculates the ratio of environmental impact for each outcome for each of the diet groups, by
# calculating ratios for each iteration and then extracting the median, 2.5th and 97.5th percentile. Note that 
# all of the ratio results are derived from the 'all adults' results

### LAND USE
for (i in dietindex) {
  eval(parse(text=(paste0("land.",i,".ratio <- land.",i,"[,2]/land.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
land.ratio.UI <- data.frame(diet.group = character(),
                            median = numeric(),
                            lowUI = numeric(),
                            highUI = numeric())

for (i in 1:6){
  land.ratio.UI[i,] <- c(dietindex[i],
                         eval(parse(text=paste0("as.numeric(quantile(land.",dietindex[i],".ratio, 0.5))"))),
                         eval(parse(text=paste0("as.numeric(quantile(land.",dietindex[i],".ratio, 0.025))"))),
                         eval(parse(text=paste0("as.numeric(quantile(land.",dietindex[i],".ratio, 0.975))"))))
}

### GREENHOUSE GAS EMISSIONS
## GWP100
for (i in dietindex) {
  eval(parse(text=(paste0("gwp100.",i,".ratio <- gwp100.",i,"[,2]/gwp100.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
gwp100.ratio.UI <- data.frame(diet.group = character(),
                              median = numeric(),
                              lowUI = numeric(),
                              highUI = numeric())

for (i in 1:6){
  gwp100.ratio.UI[i,] <- c(dietindex[i],
                           eval(parse(text=paste0("as.numeric(quantile(gwp100.",dietindex[i],".ratio, 0.5))"))),
                           eval(parse(text=paste0("as.numeric(quantile(gwp100.",dietindex[i],".ratio, 0.025))"))),
                           eval(parse(text=paste0("as.numeric(quantile(gwp100.",dietindex[i],".ratio, 0.975))"))))
}

## GTP100
for (i in dietindex) {
  eval(parse(text=(paste0("gtp100.",i,".ratio <- gtp100.",i,"[,2]/gtp100.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
gtp100.ratio.UI <- data.frame(diet.group = character(),
                              median = numeric(),
                              lowUI = numeric(),
                              highUI = numeric())

for (i in 1:6){
  gtp100.ratio.UI[i,] <- c(dietindex[i],
                           eval(parse(text=paste0("as.numeric(quantile(gtp100.",dietindex[i],".ratio, 0.5))"))),
                           eval(parse(text=paste0("as.numeric(quantile(gtp100.",dietindex[i],".ratio, 0.025))"))),
                           eval(parse(text=paste0("as.numeric(quantile(gtp100.",dietindex[i],".ratio, 0.975))"))))
}

## GWP20
for (i in dietindex) {
  eval(parse(text=(paste0("gwp20.",i,".ratio <- gwp20.",i,"[,2]/gwp20.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
gwp20.ratio.UI <- data.frame(diet.group = character(),
                              median = numeric(),
                              lowUI = numeric(),
                              highUI = numeric())

for (i in 1:6){
  gwp20.ratio.UI[i,] <- c(dietindex[i],
                           eval(parse(text=paste0("as.numeric(quantile(gwp20.",dietindex[i],".ratio, 0.5))"))),
                           eval(parse(text=paste0("as.numeric(quantile(gwp20.",dietindex[i],".ratio, 0.025))"))),
                           eval(parse(text=paste0("as.numeric(quantile(gwp20.",dietindex[i],".ratio, 0.975))"))))
}

### WATER SCARCITY
for (i in dietindex) {
  eval(parse(text=(paste0("watscar.",i,".ratio <- watscar.",i,"[,2]/watscar.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
watscar.ratio.UI <- data.frame(diet.group = character(),
                               median = numeric(),
                               lowUI = numeric(),
                               highUI = numeric())

for (i in 1:6){
  watscar.ratio.UI[i,] <- c(dietindex[i],
                            eval(parse(text=paste0("as.numeric(quantile(watscar.",dietindex[i],".ratio, 0.5))"))),
                            eval(parse(text=paste0("as.numeric(quantile(watscar.",dietindex[i],".ratio, 0.025))"))),
                            eval(parse(text=paste0("as.numeric(quantile(watscar.",dietindex[i],".ratio, 0.975))"))))
}

### WATER USE
for (i in dietindex) {
  eval(parse(text=(paste0("watuse.",i,".ratio <- watuse.",i,"[,2]/watuse.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
watuse.ratio.UI <- data.frame(diet.group = character(),
                               median = numeric(),
                               lowUI = numeric(),
                               highUI = numeric())

for (i in 1:6){
  watuse.ratio.UI[i,] <- c(dietindex[i],
                            eval(parse(text=paste0("as.numeric(quantile(watuse.",dietindex[i],".ratio, 0.5))"))),
                            eval(parse(text=paste0("as.numeric(quantile(watuse.",dietindex[i],".ratio, 0.025))"))),
                            eval(parse(text=paste0("as.numeric(quantile(watuse.",dietindex[i],".ratio, 0.975))"))))
}


### EUTROPHICATION
for (i in dietindex) {
  eval(parse(text=(paste0("eut.",i,".ratio <- eut.",i,"[,2]/eut.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
eut.ratio.UI <- data.frame(diet.group = character(),
                           median = numeric(),
                           lowUI = numeric(),
                           highUI = numeric())

for (i in 1:6){
  eut.ratio.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(eut.",dietindex[i],".ratio, 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(eut.",dietindex[i],".ratio, 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(eut.",dietindex[i],".ratio, 0.975))"))))
}

### CARBON DIOXIDE
for (i in dietindex) {
  eval(parse(text=(paste0("co2.",i,".ratio <- co2.",i,"[,2]/co2.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
co2.ratio.UI <- data.frame(diet.group = character(),
                           median = numeric(),
                           lowUI = numeric(),
                           highUI = numeric())

for (i in 1:6){
  co2.ratio.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(co2.",dietindex[i],".ratio, 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(co2.",dietindex[i],".ratio, 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(co2.",dietindex[i],".ratio, 0.975))"))))
}

### METHANE
for (i in dietindex) {
  eval(parse(text=(paste0("ch4.",i,".ratio <- ch4.",i,"[,2]/ch4.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
ch4.ratio.UI <- data.frame(diet.group = character(),
                           median = numeric(),
                           lowUI = numeric(),
                           highUI = numeric())

for (i in 1:6){
  ch4.ratio.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(ch4.",dietindex[i],".ratio, 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(ch4.",dietindex[i],".ratio, 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(ch4.",dietindex[i],".ratio, 0.975))"))))
}

### NITROUS OXIDE
for (i in dietindex) {
  eval(parse(text=(paste0("n2o.",i,".ratio <- n2o.",i,"[,2]/n2o.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
n2o.ratio.UI <- data.frame(diet.group = character(),
                           median = numeric(),
                           lowUI = numeric(),
                           highUI = numeric())

for (i in 1:6){
  n2o.ratio.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(n2o.",dietindex[i],".ratio, 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(n2o.",dietindex[i],".ratio, 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(n2o.",dietindex[i],".ratio, 0.975))"))))
}

### BIODIVERSITY
for (i in dietindex) {
  eval(parse(text=(paste0("bio.",i,".ratio <- bio.",i,"[,2]/bio.meat100[,2]"))))
}

## Calculate 95% uncertainty intervals and median
bio.ratio.UI <- data.frame(diet.group = character(),
                           median = numeric(),
                           lowUI = numeric(),
                           highUI = numeric())

for (i in 1:6){
  bio.ratio.UI[i,] <- c(dietindex[i],
                        eval(parse(text=paste0("as.numeric(quantile(bio.",dietindex[i],".ratio, 0.5))"))),
                        eval(parse(text=paste0("as.numeric(quantile(bio.",dietindex[i],".ratio, 0.025))"))),
                        eval(parse(text=paste0("as.numeric(quantile(bio.",dietindex[i],".ratio, 0.975))"))))
}


## Extract results
extractlist <- list("GWP100" = gwp100.ratio.UI,
                    "GTP100" = gtp100.ratio.UI,
                    "GWP20" = gwp20.ratio.UI,
                    "land" = land.ratio.UI,
                    "watscar" = watscar.ratio.UI,
                    "watuse" = watuse.ratio.UI,
                    "eutro" = eut.ratio.UI,
                    "bio" = bio.ratio.UI,
                    "carbon dioxide" = co2.ratio.UI,
                    "methane" = ch4.ratio.UI,
                    "nitrous oxide" = n2o.ratio.UI)

# write.xlsx(extractlist, file = "ADD HERE")


