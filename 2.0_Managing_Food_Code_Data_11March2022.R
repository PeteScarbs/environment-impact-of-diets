#!/usr/bin/env Rscript

# Managing food code outputs - taking multiple csvs, and converting into one

###
# libraries
library(plyr)
library(dplyr)
library(parallel)
library(stringr)

# Setting number of cores
mc_cores = 10

# Setting working directory
setwd("/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts")

# list of files
file.list <- list.files(paste0(getwd(),'/Food_Code_Outputs'),pattern='Impact',full.names=TRUE) %>% .[!grepl('Commodity',.)]

# function for the list of files
# this takes each file, and takes summary by product name then by food code
# across each monte carlo iteration
summary.function <- 
	function(i) {
	# Importing file
	tmp.file <- read.csv(i,stringsAsFactors=FALSE)

	# Taking summary by product name
	tmp.sum <-
		tmp.file %>%
		dplyr::group_by(sample_index, id) %>%
		dplyr::summarise(GHG = mean(tmp.impact_GHG),
				 Land = mean(tmp.impact_Land),
				 Eut = mean(tmp.impact_Eut),
				 WatScar = mean(tmp.impact_WatScar),
				 WatUse = mean(tmp.impact_WaterUse),
				 Acid = mean(tmp.impact_Acidification),
				 Bio = mean(tmp.impact_Biodiversity),
				 GHGs_N2O = mean(tmp.impact_GHGs_N2O),
				 GHGs_CH4 = mean(tmp.impact_GHGs_CH4))
	# Getting food code
	food.code <- str_extract(i,'_[0-9].*.csv') %>% str_extract(.,'[0-9]{1,}')

	# Taking summary by mc id
	tmp.sum <-
		tmp.sum %>%
		dplyr::group_by(sample_index) %>%
		dplyr::summarise(GHG = mean(GHG),
                                 Land = mean(Land),
                                 Eut = mean(Eut),
                                 WatScar = mean(WatScar),
                                 WatUse = mean(WatUse),
                                 Acid = mean(Acid),
                                 Bio = mean(Bio),
                                 GHGs_N2O = mean(GHGs_N2O),
                                 GHGs_CH4 = mean(GHGs_CH4)) %>%
		mutate(food_code = as.numeric(food.code))

	# And rbinding
	return(tmp.sum)
	}

# Now running this in parallel
out.food.codes <-
	do.call(rbind,mclapply(file.list,summary.function,mc.cores=mc_cores))


# And adding on the commodity codes
cat('Getting Food Codes')
comm.codes <- read.csv(list.files(paste0(getwd(),'/Food_Code_Outputs'),pattern='MC_Commodity',full.names=TRUE))

comm.codes <-
	comm.codes %>%
	dplyr::rename(WatUse = WaterUse,
		      Acid = Acidification,
		      Bio = Biodiversity,
		      food_code = food.code) %>%
	mutate(sample_index = rep(1:1000, nrow(.)/1000))

# Dividing commodity codes by 10 - env impacts in Poore 2018 reported in impacts per kg, but foodDB data reported in impacts per 100g
comm.codes <- 
	comm.codes %>%
	mutate(GHG = GHG/10,
	       Land = Land/10,
	       Eut = Eut/10,
	       WatScar = WatScar/10,
	       WatUse = WatUse/10,
	       Acid = Acid/10,
	       Bio = Bio/10,
	       GHGs_N2O = GHGs_N2O/10,
	       GHGs_CH4 = GHGs_CH4/10)

# rbinding
out.food.codes <- rbind(comm.codes[,names(out.food.codes)], out.food.codes)

#impact.dat <- read.csv(list.files(paste0(getwd(),'/Food_Code_Outputs'),pattern = 'Env_',full.names = TRUE),stringsAsFactors = FALSE)

# Correcting for units and conversions - see Scarborough et al 2014
# Coffee
out.food.codes[out.food.codes$food_code %in% 17152,which(names(out.food.codes)%in%'GHG'):which(names(out.food.codes)%in%'GHGs_CH4')] <-
        out.food.codes[out.food.codes$food_code %in% 17152,which(names(out.food.codes)%in%'GHG'):which(names(out.food.codes)%in%'GHGs_CH4')] * 15/1015
# Tea
out.food.codes[out.food.codes$food_code %in% 17165,which(names(out.food.codes)%in%'GHG'):which(names(out.food.codes)%in%'GHGs_CH4')] <-
        out.food.codes[out.food.codes$food_code %in% 17165,which(names(out.food.codes)%in%'GHG'):which(names(out.food.codes)%in%'GHGs_CH4')] * 2/192
# Porridge
out.food.codes[out.food.codes$food_code %in% 11143,which(names(out.food.codes)%in%'GHG'):which(names(out.food.codes)%in%'GHGs_CH4')] <-
        out.food.codes[out.food.codes$food_code %in% 11143,which(names(out.food.codes)%in%'GHG'):which(names(out.food.codes)%in%'GHGs_CH4')] * 60/567

# Hazelnut oil -- equivalent to hazelnuts
hazelnut.oil <-
        out.food.codes %>%
        filter(food_code %in% 14821) %>%
        mutate(food_code = 17037)

# Rbinding hazelnut oil
out.food.codes <- rbind(out.food.codes %>% filter(food_code != 17037), hazelnut.oil)


# And saving
write.csv(out.food.codes,paste0(getwd(),'/Food_Code_Outputs/Env_by_food_code.csv'),row.names=FALSE)

