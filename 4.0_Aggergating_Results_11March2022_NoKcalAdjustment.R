#!/usr/bin/env Rscript

# Libraries
library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(parallel)

# Working directory
setwd('/data/pubh-glob2loc/pubh0329/EPIC_Diet_Impacts/')

# Files to loop
file.list <- list.files(paste0(getwd(),'/Outputs_by_group'),
                        pattern = 'Individual_Results', full.names = TRUE)

# Rbinding files
out.df <- do.call(rbind, mclapply(file.list, read.csv, mc.cores = 1))

# Merging in kcal consumption by person
# Above estimates are standardised to 2000kcal
# And we need to do the analysis on non-standardised estimates
out_df_kcal <-
	left_join(out.df,
		  read.csv(paste0(getwd(),'/EPIC/Intakespp.csv')) %>%
			  dplyr::select(epicnum,energy))


# Not standardising to 2000 kcal
out_df_kcal[,c('GHG','Land','Eut','WatScar','Acid','Bio','GHGs_N2O','GHGs_CH4','WatUse')] <-
	out_df_kcal[,c('GHG','Land','Eut','WatScar','Acid','Bio','GHGs_N2O','GHGs_CH4','WatUse')] *
	(out_df_kcal$energy / 8368)

# Mean and sd by monte carlo id and diet_age_sex
out.df.sum <-
  out_df_kcal %>% # multiplying by 10
  mutate(GHG = GHG * 10,
         Land = Land * 10,
         WatScar = WatScar * 10,
         Eut = Eut * 10,
         GHGs_CH4 = GHGs_CH4 * 10,
         GHGs_N2O = GHGs_N2O * 10,
         Bio = Bio * 10,
         WatUse = WatUse * 10,
         Acid = Acid * 10) %>%
  dplyr::group_by(sample_index, diet_age_sex) %>%
  dplyr::summarise(mean_ghgs = mean(GHG, na.rm = TRUE),
                   mean_land = mean(Land, na.rm = TRUE),
                   mean_watscar = mean(WatScar, na.rm = TRUE),
                   mean_eut = mean(Eut, na.rm = TRUE),
                   mean_ghgs_ch4 = mean(GHGs_CH4, na.rm = TRUE),
                   mean_ghgs_n2o = mean(GHGs_N2O, na.rm = TRUE),
                   mean_bio = mean(Bio, na.rm = TRUE),
                   mean_watuse = mean(WatUse, na.rm = TRUE),
                   mean_acid = mean(Acid, na.rm = TRUE),
                   sd_ghgs = sd(GHG, na.rm = TRUE),
                   sd_land = sd(Land, na.rm = TRUE),
                   sd_watscar = sd(WatScar, na.rm = TRUE),
                   sd_eut = sd(Eut, na.rm = TRUE),
                   sd_ghgs_ch4 = sd(GHGs_CH4, na.rm = TRUE),
                   sd_ghgs_n2o = sd(GHGs_N2O, na.rm = TRUE),
                   sd_bio = sd(Bio, na.rm = TRUE),
                   sd_watuse = sd(WatUse, na.rm = TRUE),
                   sd_acid = sd(Acid, na.rm = TRUE),
                   n_participants = n())

# Updating column names
out.df.sum.write <-
  out.df.sum %>%
  mutate(sex = str_extract(diet_age_sex,'(fe)?male')) %>%
  mutate(diet_group = gsub("_.*","",diet_age_sex)) %>%
  mutate(age = str_extract(diet_age_sex,"[0-9]{2,2}-[0-9]{2,2}\\b")) %>%
  filter(!is.na(diet_group))

# Updating diet categories
out.df.sum.write <-
  out.df.sum.write %>%
  mutate(diet_group = ifelse(grepl('meat \\<',diet_group), 'meat50', diet_group)) %>%
  mutate(diet_group = ifelse(grepl('meat.*100',diet_group), 'meat100', diet_group)) %>%
  mutate(diet_group = ifelse(grepl('meat.*99',diet_group), 'meat', diet_group)) %>%
  dplyr::rename(mc_run_id = sample_index, grouping = diet_age_sex)

# Dropping category
out.df.sum.write <-
  out.df.sum.write %>%
  filter(grouping != 'NA_NA_NA')

# And saving file
write.csv(out.df.sum.write,
          paste0(getwd(),'/Final_Results/EPIC_Results_21March2022_nokcal_adjust.csv'), row.names = FALSE)

