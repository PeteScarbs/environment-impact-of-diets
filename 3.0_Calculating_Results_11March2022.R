#!/usr/bin/env Rscript

# Calculating results by person

###
# libraries
library(plyr)
library(dplyr)
library(readr)
library(parallel)
# Number of cores to use
mc_cores = 10

# Setting working directory
setwd("/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts")

# Importing consumption data
con.dat <- read_csv(list.files(paste0(getwd(),'/Data Inputs'),full.names=TRUE,pattern = 'epicoxbyperson')) %>%
	dplyr::select(epicnum, qty, foodcode) %>% # limiting to needed columns
	unique() # unique obesrvations only

# Importing food code impact data
impact.dat <- read.csv(list.files(paste0(getwd(),'/Food_Code_Outputs'),pattern = 'Env_',full.names = TRUE),stringsAsFactors = FALSE)

# Importing grouping information
group.dat <- read.csv(list.files(paste0(getwd(),'/Data Inputs'),full.names = TRUE, pattern = 'age\\+sex')) %>%
	mutate(age_group = floor(age / 10)) %>% # adding age group information
	mutate(age_group = paste0(age_group,'0-', age_group,'9')) # and categories

# Merging
# First with age group categories
epic.dat <-
	left_join(con.dat, group.dat %>% dplyr::select(-age)) %>%
	mutate(diet_sex_age = paste0(dietmeat,'_',sex,'_',age_group))

# List to loop through
loop.list <- sort(unique(epic.dat$diet_sex_age))

# Function to split participants into a loop to parallelise
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

# Function to loop across - this is used to parallelise outputs
loop.function <- function(k) {
	# filtering epic data to participants
	tmp.dat <- tmp.dat %>% filter(epicnum %in% k)

	# Merging in impacts
	tmp.dat <- left_join(tmp.dat %>% dplyr::select(epicnum,qty,foodcode,diet_sex_age),impact.dat %>% dplyr::rename(foodcode = food_code))
	# Multiplying impacts
	  # Multiplying impacts by consumption
        out.dat <- as.data.frame(tmp.dat)

	# Converting to numbers - acidification is not my friend
	for(z in which(names(out.dat)%in%'GHG'):which(names(out.dat)%in%'GHGs_CH4')) {
		out.dat[,z] <- as.numeric(out.dat[,z])
	}

	# Multiplying impacts by consumption
        out.dat[,which(names(out.dat)%in%'GHG'):which(names(out.dat)%in%'GHGs_CH4')] <-
                out.dat[,which(names(out.dat)%in%'GHG'):which(names(out.dat)%in%'GHGs_CH4')] * out.dat$qty

	# Summarising by participant
	out.dat <-
		out.dat %>%
		dplyr::group_by(epicnum, sample_index) %>%
		dplyr::summarise(GHG = sum(GHG,na.rm=TRUE),
				 Land = sum(Land,na.rm=TRUE),
				 Eut = sum(Eut),na.rm=TRUE,
				 WatScar = sum(WatScar,na.rm=TRUE),
				 WatUse = sum(WatUse,na.rm=TRUE), 
				 Acid = sum(Acid,na.rm=TRUE),
				 Bio = sum(Bio,na.rm=TRUE),
				 GHGs_N2O = sum(GHGs_N2O,na.rm=TRUE),
				 GHGs_CH4 = sum(GHGs_CH4,na.rm=TRUE)) %>%
		mutate(GHG = GHG / 1000,
		       Land = Land / 1000,
		       Eut = Eut / 1000,
		       WatScar = WatScar / 1000,
		       WatUse = WatUse / 1000,
		       Acid = Acid / 1000,
		       Bio = Bio / 1000,
		       GHGs_N2O = GHGs_N2O / 1000,
		       GHGs_CH4 = GHGs_CH4 / 1000)

	# And returning data frame
	return(out.dat)
}


# And looping through these
for(i in loop.list) {

	# Data frame that only includes the diet sex age participants
	tmp.dat <- epic.dat %>% filter(diet_sex_age %in% i)
	# And another loop to go through participants
	# Merging on the full diet group results in memory issues...
	# So need to do this - splitting into pools of 100 participants
	n_splits <- ceiling(length(unique(tmp.dat$epicnum))/100)# number of splits

	cat('Getting splits in data')	
	if(n_splits < mc_cores)  {n_splits = mc_cores}
	# Splitting the list
	participant.loop <- chunk2(unique(tmp.dat$epicnum),n_splits)


	#test.df <- loop.function(participant.loop[[1]])
	
	# Now running the function
	cat('Rbinding the data')
	write.df <- do.call(rbind,mclapply(participant.loop, loop.function, mc.cores = mc_cores))

	cat('Finished rbinding')
	# Getting rid of NA sample indices
	write.df <- 
		write.df %>%
		filter(!is.na(sample_index)) %>%
		mutate(diet_age_sex = i)

	cat('Summarising by MC ID')
	# Summarising by MC id
	write.df.sum <-
		write.df %>%
		dplyr::group_by(sample_index) %>%
		dplyr::summarise(GHG = mean(GHG,na.rm=TRUE),
                                 Land = mean(Land,na.rm=TRUE),
                                 Eut = mean(Eut,na.rm=TRUE),
                                 WatScar = mean(WatScar,na.rm=TRUE),
                                 WatUse = mean(WatUse,na.rm=TRUE),
                                 Acid = mean(Acid,na.rm=TRUE),
                                 Bio = mean(Bio,na.rm=TRUE),
                                 GHGs_N2O = mean(GHGs_N2O,na.rm=TRUE),
                                 GHGs_CH4 = mean(GHGs_CH4,na.rm=TRUE)) %>%
		mutate(diet_age_sex = i)

	cat('Writing file')
	# And filtering NAs
	write.df.sum <-
		write.df.sum %>%
		filter(!is.na(write.df.sum))

	# And saving data frames
	write.csv(write.df,
		  paste0(getwd(),'/Outputs_by_group/Individual_Results_',i,'.csv'),
		  row.names = FALSE)

	write.csv(write.df.sum,
                  paste0(getwd(),'/Outputs_by_group/Group_Results_',i,'.csv'),
                  row.names = FALSE)
}

# And need to do the consumption data by diet sex and age group
# Now with consumption dat
epic.dat <-
	left_join(epic.dat, impact.dat %>% dplyr::rename(foodcode = food_code)) %>%
	unique()

# Multiplying impacts by consumption

# Summarising demographic group

# Saving file
