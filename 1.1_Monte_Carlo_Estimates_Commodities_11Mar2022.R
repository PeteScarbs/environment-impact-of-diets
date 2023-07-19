#!/usr/bin/env Rscript

###
# libraries
library(plyr)
library(dplyr)

# Setting working directory
setwd("/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts")
# functions
source('/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts/Scripts/0.0_Functions_Estimating_Impacts_18Feb2022.R')

# food code data
food.codes <- read.csv(paste0(getwd(),"/Food_Code_Data/commodities to food codes.csv"), stringsAsFactors = FALSE)

# getting lca data
setwd("/data/pubh-glob2loc/pubh0329/foodDB_EnvEstimates")
lca.dat <- 
  read.csv(paste0(getwd(),"/Data Inputs/jp_lca_dat.csv"),
           stringsAsFactors = FALSE) %>%
  mutate(Weight = as.numeric(gsub("%","",Weight)))

# Adding translation for subcategories
lca.subcats <- read.csv(paste0(getwd(),'/Data Inputs/Search words, second round, 22Jan2022.csv'))

# And updating lca categories
lca.dat <-
  left_join(lca.dat, # Merging
            lca.subcats %>% dplyr::select(Data.S2.Name = LCA_Category, Product_details, LCA_Category_sub = LCA_sub_category, LCA_Category_sub_sub = LCA_sub_sub_category, 
                                          Average_of_original_category, Average_of_sub_category) %>% 
              filter(LCA_Category_sub != '') %>% unique(.)) %>%
  unique(.) #%>%
# mutate(LCA_Category_sub_sub = ifelse(Average_of_sub_category %in% 'Yes',NA,LCA_Category_sub_sub)) %>%
# mutate(LCA_Category_sub = ifelse(Average_of_original_category %in% 'Yes',NA,LCA_Category_sub))

# Merging in CO2 + CH4 + N2O info
co2.dat <- read.csv("/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts/Data Inputs/jp_lca_dat_disaggregated_ghgs.csv")
# co2.dat <- read.csv("/Volumes/Citadel/Oxford/Research Projects/EPIC Oxford Diet Impacts/Data Inputs/jp_lca_dat_disaggregated_ghgs.csv")

# columns that are n2o
# columns that are ch4
# columns that are co2
which.ch4 <- c(grep('CH4', names(co2.dat), ignore.case = TRUE), grep('LUC.Burn', names(co2.dat), ignore.case=TRUE))
which.n2o <- c(grep('n2o', names(co2.dat), ignore.case = TRUE), grep('LUC.Burn', names(co2.dat), ignore.case=TRUE))
which.co2 <-
  c(grep('co2', names(co2.dat), ignore.case = TRUE), grep('LUC.Burn', names(co2.dat), ignore.case=TRUE), 7,8,9,10) %>% .[. != 6]

# Weighting
ch4.weights <- c()
for(i in which.ch4) {
  counter = 1
  if(i %in% which.n2o) {
    counter = counter + 1
  }
  if(i %in% which.co2) {
    counter = counter + 1
  }
  ch4.weights <- c(ch4.weights,counter)
}


n2o.weights <- c()
for(i in which.n2o) {
  counter = 1
  if(i %in% which.ch4) {
    counter = counter + 1
  }
  if(i %in% which.co2) {
    counter = counter + 1
  }
  n2o.weights <- c(n2o.weights,counter)
}

co2.weights <- c()
for(i in which.co2) {
  counter = 1
  if(i %in% which.ch4) {
    counter = counter + 1
  }
  if(i %in% which.n2o) {
    counter = counter + 1
  }
  co2.weights <- c(co2.weights,counter)
}

# Getting rid of non numeric values
for(i in unique(c(which.ch4,which.n2o,which.co2, grep('Acid.Pot.*so2|Eutr.Pot.*po4|Freshwtr.*Withdr|Wtr.Sc.*L', names(co2.dat), ignore.case=TRUE)))) {
  co2.dat[,i] <- as.numeric(co2.dat[,i])
}
# And summing by gasses, assuming an equal proportion of e.g. LUC emissions goes to ch4, n2o and co2
co2.dat$ch4_in_kgco2eq <- base::rowSums(co2.dat[,which.ch4] * 1/ch4.weights, dims = 1, na.rm = TRUE)
co2.dat$n2o_in_kgco2eq <- base::rowSums(co2.dat[,which.n2o] * 1/n2o.weights, dims = 1, na.rm = TRUE)
co2.dat$co2_in_kgco2eq <- base::rowSums(co2.dat[,which.co2] * 1/co2.weights, dims = 1, na.rm = TRUE)


# Merging in
lca.dat <-
  left_join(lca.dat,
            co2.dat %>%
              dplyr::select(Product_details = Product, Country, GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks. = GHG.Emis...kg.CO2.eq.,
                            # Acidification..g.SO2eq. = Acid.Pot..kg.SO2.eq.,
                            Eutrophication..g.PO43.eq. = Eutr.Pot..kg.PO43..eq.,
                            Water.Use..L. = Freshwtr..Withdr...L.,
                            Scarcity.Weighted.Water.Use..L.eq. = Wtr.Sc...L.eq.,
                            ch4_in_kgco2eq, n2o_in_kgco2eq, co2_in_kgco2eq) %>%
              # mutate(col_check = 'check') %>%
              mutate(GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks. = round(GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks., digits = 2)) %>%
              mutate(Eutrophication..g.PO43.eq. = round(Eutrophication..g.PO43.eq. * 1000, digits = 2)) %>%
              mutate(Water.Use..L. = round(Water.Use..L., digits = 2)) %>%
              mutate(Scarcity.Weighted.Water.Use..L.eq. = round(Scarcity.Weighted.Water.Use..L.eq., digits = 2)))


# Adding conversion estimates
# lca.dat <-
#   rbind(lca.dat,
#         conversion.function(indicators = c('^Land.Use','GHG','Eutrophication','Scarcity','Acidification','^Water','Biodiversity')) %>% dplyr::select(-food.group)) %>%
#   .[,c('Data.S2.Name','LCA_Category_sub','LCA_Category_sub_sub','Weight','Land.Use..m2.year.','GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks.',
#        'Eutrophication..g.PO43.eq.','Scarcity.Weighted.Water.Use..L.eq.','Acidification..g.SO2eq.','Water.Use..L.','Biodiversity..sp.yr.10.14.',
#        'Average_of_original_category','Average_of_sub_category')]

# Changing directory
setwd('/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts')
# Getting conversion from eg soy to soy milk
lca.dat <-
  rbind(conversion.function(indicators = c('^Land.Use','GHG','Eutrophication','Scarcity','Acidification','^Water','Biodiversity','ch4_in','n2o_in','co2_in')) %>% dplyr::select(-food.group),
        lca.dat %>% filter(!grepl('Cheese',Data.S2.Name))) %>% # Conversion function goes from cheese to other types of cheese
  .[,c('Data.S2.Name','LCA_Category_sub','LCA_Category_sub_sub','Weight','Land.Use..m2.year.','GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks.',
       'Eutrophication..g.PO43.eq.','Scarcity.Weighted.Water.Use..L.eq.','Acidification..g.SO2eq.','Water.Use..L.','Biodiversity..sp.yr.10.14.',
       'ch4_in_kgco2eq','n2o_in_kgco2eq','co2_in_kgco2eq',
       'Average_of_original_category','Average_of_sub_category','Sys')] 
# And adding in other cheese category
lca.dat <-
  rbind(lca.dat,
        lca.dat %>% filter(grepl('Medium Cheese',LCA_Category_sub)) %>% mutate(LCA_Category_sub = 'Other Cheese')) # And adding in the other cheese category

# Updating categories for almond milk vs other milk
lca.dat <-
  lca.dat %>%
  mutate(Data.S2.Name = ifelse(Data.S2.Name %in% 'Almond milk' & !(LCA_Category_sub %in% 'Almonds'),'Other nut milk', Data.S2.Name)) %>%
  mutate(LCA_Category_sub = ifelse(Data.S2.Name %in% c('Almond milk','Other nut milk','Oat milk','Soymilk','Rice milk'),NA, LCA_Category_sub)) %>%
  mutate(LCA_Category_sub_sub = ifelse(Data.S2.Name %in% c('Almond milk','Other nut milk','Oat milk','Soymilk','Rice milk'),NA, LCA_Category_sub_sub))



# Adding butter, misc oils, and pig meat
# These weightings recommended by Joseph Poore, folliwng methods in Poore and Nemecek 2018 Science
lca.dat <-
  rbind(lca.dat,
        lca.dat %>% filter(Data.S2.Name %in% 'Milk') %>% mutate(Data.S2.Name = 'Butter, Cream & Ghee'),
        lca.dat %>% filter(Data.S2.Name %in% 'Rapeseed Oil') %>% mutate(Data.S2.Name = 'Oils Misc.'),
        lca.dat %>% filter(Data.S2.Name %in% 'Pig Meat') %>% mutate(Data.S2.Name = 'Animal Fats'))

# rbinding tea coffee and chocolate info
tea.dat <- 
  read.csv(paste0("/data/pubh-glob2loc/pubh0329/foodDB_EnvEstimates/Data Inputs/lcadat 17october2019.csv")) %>%
  # read.csv("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Runs 19April2021/Data Inputs/lcadat 17october2019.csv") %>%
  mutate(LCA_Category_sub = '', LCA_Category_sub_sub = '',
         ch4_in_kgco2eq = 0,
         n2o_in_kgco2eq = NA,
         co2_in_kgco2eq = NA,
         Average_of_original_category = NA,
         Average_of_sub_category = NA,
         Sys = 'C',
         Weight = 1) %>%
  filter(Data.S2.Name %in% 'Tea')
# reordering
tea.dat <- tea.dat[,names(lca.dat)]

# and rbinding
lca.dat <-
  rbind(lca.dat,
        tea.dat)

# Adding data on tea

# and updating names to merge with rest of script
names(lca.dat)[names(lca.dat) %in% 'Data.S2.Name'] <- 'Food_Category'
names(lca.dat)[names(lca.dat) %in% 'Land.Use..m2.year.'] <- 'Land'
names(lca.dat)[names(lca.dat) %in% 'GHG.Emissions..kg.CO2eq..IPCC2013.incl.CC.feedbacks.'] <- 'GHG'
names(lca.dat)[names(lca.dat) %in% 'Eutrophication..g.PO43.eq.'] <- 'Eut'
names(lca.dat)[names(lca.dat) %in% 'Scarcity.Weighted.Water.Use..L.eq.'] <- 'WatScar'
names(lca.dat)[names(lca.dat) %in% 'Biodiversity..sp.yr.10.14.'] <- 'Biodiversity'
names(lca.dat)[names(lca.dat) %in% 'Acidification..g.SO2eq.'] <- 'Acidification'
names(lca.dat)[names(lca.dat) %in% 'Water.Use..L.'] <- 'WaterUse'
names(lca.dat)[grepl('ch4_in', names(lca.dat))] <- 'GHGs_CH4'
names(lca.dat)[grepl('n2o_in', names(lca.dat))] <- 'GHGs_N2O'
names(lca.dat)[grepl('co2_in', names(lca.dat))] <- 'GHGs_CO2'


# and limiting lca dat to only necessary columns
cat('Adding Salt and Water')
salt.dat <- data.frame(Food_Category = 'Salt',LCA_Category_sub = NA, LCA_Category_sub_sub = NA, Weight = 1, Land = 0, GHG = 0, Eut = 0, WatScar = 0, Biodiversity = 0, Acidification = 0, WaterUse = 0,Average_of_original_category=NA,Average_of_sub_category=NA, Sys = 'C', GHGs_CH4 = 0, GHGs_N2O = 0, GHGs_CO2 = NA)
water.dat <- data.frame(Food_Category = 'Water',LCA_Category_sub = NA, LCA_Category_sub_sub = NA, Weight = 1, Land = 0, GHG = 0, Eut = 0, WatScar = 0, Biodiversity = 0, Acidification = 0, WaterUse = 0,Average_of_original_category=NA,Average_of_sub_category=NA, Sys = 'C', GHGs_CH4 = 0, GHGs_N2O = 0, GHGs_CO2 = NA)
lca.dat <-
  lca.dat[,c('Food_Category','LCA_Category_sub','LCA_Category_sub_sub','Weight','Land','GHG','Eut','WatScar','Biodiversity','Acidification','WaterUse','GHGs_CH4','GHGs_N2O','GHGs_CO2','Average_of_original_category','Average_of_sub_category','Sys')] %>%
  mutate(Food_Category = ifelse(Food_Category %in% c('Fish (farmed)','Fish (wild caught)','Crustaceans (farmed)','Crustaceans (wild caught)'),
                                gsub(" \\(farmed\\)| \\(wild caught\\)","",Food_Category),
                                Food_Category)) %>%
  rbind(., salt.dat[,names(.)]) %>%
  rbind(., water.dat[,names(.)])
# Renaming column - doing this for merging with food data later
lca.dat <-
  lca.dat %>%
  dplyr::rename(Food_Category_sub = LCA_Category_sub,
                Food_Category_sub_sub = LCA_Category_sub_sub)

# Changing working directory back
setwd("/data/pubh-glob2loc/pubh0329/foodDB_EnvEstimates")
# Adding in fisheries data
lca.dat <-
  rbind(lca.dat,
        fish.env.function('yay')[,names(lca.dat)])

# Updating food category for cheese - this has been updated on the env estimates, so also need to do this with the food codes
food.codes <-
	food.codes %>%
	mutate(food.group = ifelse(food.code %in% c(12131,12147,12160),'Soft Cheese', food.group), # brie, cotage, and fromage frais
	       food.group = ifelse(food.code %in% c(12134),'Hard Cheese', food.group),# Cheddar cheese
	       food.group = ifelse(food.code %in% c(12154),'Semi-Hard Cheese', food.group)) # Edam Cheese

# Adding info for coffee mate -- 
coffee.mate <- 
	food.codes[1:2,] %>%
	mutate(food.code = 12027) %>%
	mutate(food.group = c('Oils Misc.','Maize (Meal)'))

# merging
food.out <-
  left_join(food.codes %>% dplyr::select(food.code, Food_Category = food.group),
            lca.dat) %>%
  unique()

# and sampling
food.sampled <-
  food.out %>%
  mutate(Weight = ifelse(is.na(Weight), 0, Weight)) %>%
  mutate(Weight = Weight + .01) %>%
  dplyr::group_by(food.code, Food_Category) %>%
  sample_n(size = 1000, weight = Weight, replace = TRUE)

# And sampling for coffeemate
coffee.mate <-
	left_join(coffee.mate %>% dplyr::select(food.code, Food_Category = food.group),
		  lca.dat) %>%
	unique()

# Summing by mc index
coffee.mate <-
  coffee.mate  %>%
  mutate(Weight = ifelse(is.na(Weight), 0, Weight)) %>%
  mutate(Weight = Weight + .01) %>%
  dplyr::group_by(food.code, Food_Category) %>%
  sample_n(size = 1000, weight = Weight, replace = TRUE)
coffee.mate$sample_index <- rep(1:1000,2)
# Need to do this to multiply...  
coffee.mate <- as.data.frame(coffee.mate)
coffee.mate$Acidification <- as.numeric(coffee.mate$Acidification)
# multiplying
coffee.mate[coffee.mate$Food_Category %in% 'Oils Misc.', which(names(coffee.mate)%in%'GHG'):which(names(coffee.mate)%in%'GHGs_CO2')] <-
	coffee.mate[coffee.mate$Food_Category %in% 'Oils Misc.', which(names(coffee.mate)%in%'GHG'):which(names(coffee.mate)%in%'GHGs_CO2')] * .345
coffee.mate[coffee.mate$Food_Category %in% 'Oils Misc.', which(names(coffee.mate)%in%'GHG'):which(names(coffee.mate)%in%'GHGs_CO2')] <-
        coffee.mate[coffee.mate$Food_Category %in% 'Oils Misc.', which(names(coffee.mate)%in%'GHG'):which(names(coffee.mate)%in%'GHGs_CO2')] * .573*(12.12/27.7)

# And summarising by sample index
coffee.mate <-
	coffee.mate %>%
	dplyr::group_by(sample_index) %>%
	dplyr::summarise(Land = sum(Land),
			 GHG = sum(GHG),
			 Eut = sum(Eut),
			 WatScar = sum(WatScar),
			 Acidification = sum(Acidification),
			 WaterUse = sum(WaterUse),
			 GHGs_CH4 = sum(GHGs_CH4),
			 GHGs_N2O = sum(GHGs_N2O),
			 GHGs_CO2 = sum(GHGs_CO2),
			 Biodiversity = sum(Biodiversity))

# Adding columns to rbind
coffee.mate <-
	coffee.mate %>%
	mutate(Average_of_original_category = NA,
	       Average_of_sub_category = NA,
	       Sys = NA,
	       food.code = 12027,
	       Food_Category = 'Coffeemate',
	       Food_Category_sub = NA,
	       Food_Category_sub_sub = NA,
	       Weight = 0.1)
# Changing names to rbind
coffee.mate <- coffee.mate[,names(food.sampled)]
# And rbiding
food.sampled <- rbind(food.sampled %>% mutate(Acidification = as.numeric(Acidification)), coffee.mate %>% mutate(Acidification = as.numeric(Acidification)))

# And setting WD again
setwd("/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts")
# saving file
write.csv(food.sampled,
          paste0(getwd(),'/Food_Code_Outputs/MC_Commodity_Impacts.csv'),row.names=FALSE)

