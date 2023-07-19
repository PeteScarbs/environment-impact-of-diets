# Functions for estimating env impacts and nutriscore for each product

###
# Getting nutritional units, and adjusting all values to g/100g
# With the exception of energy, which will be kj/100g

nutrition.adjust.function <-
  function(dat, nutrient.list) {
    
    # Creating new columns to store data
    dat[,paste0(nutrient.list,"_value")] <- NA
    dat[,paste0(nutrient.list,"_unit")] <- NA
    
    dat <- as.data.frame(dat)
    
    dat$tmp.check <- NA
    
    # Looping through columns
    for(nutrient in nutrient.list) {
      
        # Extracting numbers and decimals
        # Do this in two steps
      
      # (1) Extracting numeric values
      dat[grep(".",dat[,paste0(nutrient)]),paste0(nutrient,"_value")] <-
        str_extract(dat[grep(".",dat[,paste0(nutrient)]),nutrient],
                    "[0-9]{1,4}(\\s)?.(\\s)?[0-9]{1,3}|[0-9]{1,4}|
                    [0-9]{1,4}(\\s)?,(\\s)?[0-9]{1,3}")
      
      # Trimming white space
      dat[,paste0(nutrient,"_value")] <-
        gsub("[[:space:]]", "", dat[,paste0(nutrient,"_value")])
      
      # Converting to numeric
        dat[,paste0(nutrient,"_value")] <-
          as.numeric(dat[,paste0(nutrient,"_value")])
        
        # Extracting unit
        dat[,paste0(nutrient,"_unit")] <-
          str_extract(dat[,nutrient],
                      "mg|g|kcal|calorie|kj|j|kiloj")
        
        # Adjusting units
        dat[paste0(nutrient,"_unit") %in% 'kcal',paste0(nutrient,"_value")] <-
          dat[paste0(nutrient,"_unit") %in% 'kcal',paste0(nutrient,"_value")] * 4.184
        
        dat[paste0(nutrient,"_unit") %in% 'mg',paste0(nutrient,"_value")] <-
          dat[paste0(nutrient,"_unit") %in% 'mg',paste0(nutrient,"_value")] / 1000
        
        # And correcting
        if(nutrient %in% c('Energy_pack')) {
          dat[(dat$Energy_pack_unit %in% c('g','mg')),'tmp.check'] <-
            1
        } else if (nutrient %in% c('Sodium_pack')) {
          dat[dat$Sodium_pack_unit %in% c('mg'),'Sodium_pack_value'] <-
            dat[dat$Sodium_pack_unit %in% c('mg'),'Sodium_pack_value'] / 1000
          dat[dat$Sodium_pack_unit %in% c('mg'),'unit'] <-
            'g'

          dat[!(dat[,'Sodium_pack_unit'] %in% c('g')),'tmp.check'] <-
            1

        } else {
          dat[dat[,paste0(nutrient,"_unit")] %in% 'mg',paste0(nutrient,"_value")] <-
            dat[dat[,paste0(nutrient,"_unit")] %in% 'mg',paste0(nutrient,"_value")] / 1000

          dat[dat[,paste0(nutrient,"_unit")] %in% 'mg',paste0(nutrient,"_unit")] <- 
          'g'

          dat[!(dat[,paste0(nutrient,"_unit")] %in% 'g'),'tmp.check'] <-
            1
        }
        dat[!is.na(dat$tmp.check),paste0(nutrient,"_value")] <- NA
        dat$tmp.check <- NA
    } 
    
    # Updating values
    return(dat)
  }


### 
# Function to calculate nutriscore
#####
###
# Calculating nutriscore

nutriscore.function = function(dat) {
  
  nutriscore.solid <-
    dat %>%
    mutate(Calories = ifelse(is.na(Calories), 0, Calories),
           Protein = ifelse(is.na(Protein), 0, Protein),
           Fat = ifelse(is.na(Fat), 0, Fat),
           SaturatedFat = ifelse(is.na(SaturatedFat), 0, SaturatedFat),
           Fiber = ifelse(is.na(Fiber), 0, Fiber),
           Sodium = ifelse(is.na(Sodium), 0, Sodium),
           Sugar = ifelse(is.na(Sugar), 0, Sugar),
           FVNO = ifelse(is.na(FVNO), 0, FVNO)) %>%
    mutate(tot_nutrition = Calories + Protein + Fat + SaturatedFat + Fiber + Sodium + Sugar) %>%
    mutate(NutriCal = ifelse(Calories * 4.184 > 3350, 10, # Getting cutoffs for ingredients included in nutriscore calculations
                             ifelse(Calories * 4.184 > 3015, 9,
                                    ifelse(Calories * 4.184 > 2680, 8,
                                           ifelse(Calories * 4.184 > 2345, 7,
                                                  ifelse(Calories * 4.184 > 2010, 6,
                                                         ifelse(Calories * 4.184 > 1675, 5,
                                                                ifelse(Calories * 4.184 > 1340, 4,
                                                                       ifelse(Calories * 4.184 > 1005, 3,
                                                                              ifelse(Calories * 4.184 > 670, 2,
                                                                                     ifelse(Calories * 4.184 > 335, 1,0))))))))))) %>%
    mutate(NutriSugar = ifelse(Sugar > 45, 10,
                               ifelse(Sugar>40, 9,
                                      ifelse(Sugar>36,8,
                                             ifelse(Sugar>31,7,
                                                    ifelse(Sugar>27,6,
                                                           ifelse(Sugar>22.5,5,
                                                                  ifelse(Sugar>18,4,
                                                                         ifelse(Sugar>13.5,3,
                                                                                ifelse(Sugar>9,2,
                                                                                       ifelse(Sugar>4.5,1,0))))))))))) %>%
    mutate(NutriSatFats = ifelse(SaturatedFat > 10, 10,
                                 ifelse(SaturatedFat > 9, 9,
                                        ifelse(SaturatedFat > 8, 8,
                                               ifelse(SaturatedFat > 7, 7,
                                                      ifelse(SaturatedFat > 6, 6,
                                                             ifelse(SaturatedFat > 5, 5,
                                                                    ifelse(SaturatedFat > 4, 4,
                                                                           ifelse(SaturatedFat > 3, 3,
                                                                                  ifelse(SaturatedFat > 2, 2,
                                                                                         ifelse(SaturatedFat > 1, 1,0))))))))))) %>%
    mutate(NutriSodium = ifelse(Sodium > 900,10,
                                ifelse(Sodium > 810, 9,
                                       ifelse(Sodium > 720, 8,
                                              ifelse(Sodium > 630, 7,
                                                     ifelse(Sodium > 540, 6,
                                                            ifelse(Sodium > 450, 5,
                                                                   ifelse(Sodium > 360, 4,
                                                                          ifelse(Sodium > 270, 3,
                                                                                 ifelse(Sodium > 180, 2,
                                                                                        ifelse(Sodium > 90, 1, 0))))))))))) %>%
    mutate(NutriFatRatio = SaturatedFat / Fat) %>% # Oils calculated based on ratio of sat fat : fat
    mutate(NutriFatRatioScore = ifelse(NutriFatRatio < .1, 0,
                                       ifelse(NutriFatRatio < .16, 1,
                                              ifelse(NutriFatRatio < .22, 2,
                                                     ifelse(NutriFatRatio < .28, 3,
                                                            ifelse(NutriFatRatio < .34, 4,
                                                                   ifelse(NutriFatRatio < .4, 5,
                                                                          ifelse(NutriFatRatio < .46, 6,
                                                                                 ifelse(NutriFatRatio < .52, 7,
                                                                                        ifelse(NutriFatRatio < .58, 8,
                                                                                               ifelse(NutriFatRatio < .64, 9, 10))))))))))) %>%
    mutate(NutriFVNO = ifelse(FVNO > 80, 5,
                              ifelse(FVNO > 60, 2,
                                     ifelse(FVNO > 40, 1, 0)))) %>%
    mutate(NutriFiber = ifelse(Fiber > 3.5, 5, 
                               ifelse(Fiber > 2.8, 4,
                                      ifelse(Fiber > 2.1, 3,
                                             ifelse(Fiber > 1.4, 2,
                                                    ifelse(Fiber > .7, 1, 0)))))) %>%
    mutate(NutriProtein = ifelse(Protein > 8, 5,
                                 ifelse(Protein > 6.4, 4,
                                        ifelse(Protein > 4.8, 3,
                                               ifelse(Protein > 3.2, 2,
                                                      ifelse(Protein > 1.6, 1, 0)))))) %>%
    mutate(NutriScoreNeg = NutriCal + NutriSugar + NutriSatFats + NutriSodium) %>% # Calculating negative and positive points
    mutate(NutriScorePos = NutriFiber + NutriFVNO + NutriProtein) %>%
    mutate(NutriScorePoints = NutriScoreNeg - NutriScorePos) %>% # Calculating nutriscore for general products
    mutate(NutriScorePoints = ifelse(NutriScoreNeg >= 11 & NutriFVNO < 5, NutriScoreNeg - (NutriFiber + NutriFVNO), NutriScorePoints)) %>% # Exception for products with negative value >= 11
    mutate(NutriScorePoints = ifelse(cheese %in% 'Cheese',NutriScoreNeg - NutriScorePos, NutriScorePoints)) %>% # Exception for cheese
    mutate(NutriScorePoints = ifelse(fat.oil %in% 'Fats.Oils', NutriCal + NutriSugar + NutriFatRatioScore + NutriSodium - (NutriFiber + NutriProtein), NutriScorePoints)) %>% # Exception for added fats 
    mutate(NutriScoreLetter =  # Calculating nutriscore
             ifelse(NutriScorePoints <= -1, 'A',
                    ifelse(NutriScorePoints <= 2, 'B',
                           ifelse(NutriScorePoints <= 10, 'C',
                                  ifelse(NutriScorePoints <= 18, 'D',
                                         ifelse(NutriScorePoints >= 19, 'E',NA)))))) %>%
    mutate(NutriScore_Scaled = (NutriScorePoints + 15) / 55) %>% # Scaling to range from 0 to 1 (0 = best, 1 = worst)
    filter(drinks %in% 'No')
  
  nutriscore.drinks <-
    dat %>%
    mutate(Calories = ifelse(is.na(Calories), 0, Calories),
           Protein = ifelse(is.na(Protein), 0, Protein),
           Fat = ifelse(is.na(Fat), 0, Fat),
           SaturatedFat = ifelse(is.na(SaturatedFat), 0, SaturatedFat),
           Fiber = ifelse(is.na(Fiber), 0, Fiber),
           Sodium = ifelse(is.na(Sodium), 0, Sodium),
           Sugar = ifelse(is.na(Sugar), 0, Sugar),
           FVNO = ifelse(is.na(FVNO), 0, FVNO)) %>%
    mutate(tot_nutrition = Calories + Protein + Fat + SaturatedFat + Fiber + Sodium + Sugar) %>%
    mutate(NutriCal = ifelse(Calories * 4.184 > 270, 10, # Getting cutoffs for ingredients included in nutriscore calculations
                             ifelse(Calories * 4.184 > 240, 9,
                                    ifelse(Calories * 4.184 > 210, 8,
                                           ifelse(Calories * 4.184 > 180, 7,
                                                  ifelse(Calories * 4.184 > 150, 6,
                                                         ifelse(Calories * 4.184 > 120, 5,
                                                                ifelse(Calories * 4.184 > 90, 4,
                                                                       ifelse(Calories * 4.184 > 60, 3,
                                                                              ifelse(Calories * 4.184 > 30, 2,
                                                                                     ifelse(Calories * 4.184 > 0, 1,0))))))))))) %>%
    mutate(NutriSugar = ifelse(Sugar > 13.5, 10,
                               ifelse(Sugar>12, 9,
                                      ifelse(Sugar>10.5,8,
                                             ifelse(Sugar>9,7,
                                                    ifelse(Sugar>7.5,6,
                                                           ifelse(Sugar>6,5,
                                                                  ifelse(Sugar>4.5,4,
                                                                         ifelse(Sugar>3,3,
                                                                                ifelse(Sugar>1.5,2,
                                                                                       ifelse(Sugar>0,1,0))))))))))) %>%
    mutate(NutriSatFats = ifelse(SaturatedFat > 10, 10,
                                 ifelse(SaturatedFat > 9, 9,
                                        ifelse(SaturatedFat > 8, 8,
                                               ifelse(SaturatedFat > 7, 7,
                                                      ifelse(SaturatedFat > 6, 6,
                                                             ifelse(SaturatedFat > 5, 5,
                                                                    ifelse(SaturatedFat > 4, 4,
                                                                           ifelse(SaturatedFat > 3, 3,
                                                                                  ifelse(SaturatedFat > 2, 2,
                                                                                         ifelse(SaturatedFat > 1, 1,0))))))))))) %>%
    mutate(NutriSodium = ifelse(Sodium > 900,10,
                                ifelse(Sodium > 810, 9,
                                       ifelse(Sodium > 720, 8,
                                              ifelse(Sodium > 630, 7,
                                                     ifelse(Sodium > 540, 6,
                                                            ifelse(Sodium > 450, 5,
                                                                   ifelse(Sodium > 360, 4,
                                                                          ifelse(Sodium > 270, 3,
                                                                                 ifelse(Sodium > 180, 2,
                                                                                        ifelse(Sodium > 90, 1, 0))))))))))) %>%
    mutate(NutriFatRatio = SaturatedFat / Fat) %>% # Oils calculated based on ratio of sat fat : fat
    mutate(NutriFatRatioScore = ifelse(NutriFatRatio < .1, 0,
                                       ifelse(NutriFatRatio < .16, 1,
                                              ifelse(NutriFatRatio < .22, 2,
                                                     ifelse(NutriFatRatio < .28, 3,
                                                            ifelse(NutriFatRatio < .34, 4,
                                                                   ifelse(NutriFatRatio < .4, 5,
                                                                          ifelse(NutriFatRatio < .46, 6,
                                                                                 ifelse(NutriFatRatio < .52, 7,
                                                                                        ifelse(NutriFatRatio < .58, 8,
                                                                                               ifelse(NutriFatRatio < .64, 9, 10))))))))))) %>%
    mutate(NutriFVNO = ifelse(FVNO > 80, 10,
                              ifelse(FVNO > 60, 4,
                                     ifelse(FVNO > 40, 2, 0)))) %>%
    mutate(NutriFiber = ifelse(Fiber > 4.7, 5, 
                               ifelse(Fiber > 3.7, 4,
                                      ifelse(Fiber > 2.8, 3,
                                             ifelse(Fiber > 1.9, 2,
                                                    ifelse(Fiber > .9, 1, 0)))))) %>%
    mutate(NutriProtein = ifelse(Protein > 8, 5,
                                 ifelse(Protein > 6.4, 4,
                                        ifelse(Protein > 4.8, 3,
                                               ifelse(Protein > 3.2, 2,
                                                      ifelse(Protein > 1.6, 1, 0)))))) %>%
    mutate(NutriScoreNeg = NutriCal + NutriSugar + NutriSatFats + NutriSodium) %>% # Calculating negative and positive points
    mutate(NutriScorePos = NutriFiber + NutriFVNO + NutriProtein) %>%
    mutate(NutriScorePoints = NutriScoreNeg - NutriScorePos) %>% # Calculating nutriscore for general products
    mutate(NutriScorePoints = ifelse(NutriScoreNeg >= 11 & NutriFVNO < 5, NutriScoreNeg - (NutriFiber + NutriFVNO), NutriScorePoints)) %>% # Exception for products with negative value >= 11
    mutate(NutriScoreLetter =  # Calculating nutriscore
             ifelse(NutriScorePoints <= -20, 'B',
                    ifelse(NutriScorePoints <= 1, 'B',
                           ifelse(NutriScorePoints <= 5, 'C',
                                  ifelse(NutriScorePoints <= 9, 'D',
                                         ifelse(NutriScorePoints >= 10, 'E', NA)))))) %>%
    mutate(NutriScore_Scaled = (NutriScorePoints + 15) / 55) %>% # Scaling to range from 0 to 1 (0 = best, 1 = worst)
    filter(drinks %in% 'Drinks') %>%
    mutate(NutriScoreLetter = ifelse(grepl('Water',product_name,ignore.case = TRUE),'A',NutriScoreLetter))
  
  # Binding
  nutriscore <-
    rbind(nutriscore.solid,
          nutriscore.drinks)
    
  
  return(nutriscore)
  
}


###
# Env estimate function
env.estimate.function = function(dat,
                                 lca.dat) {
  # And taking summary by product id
  dat <-
    left_join(dat,
              lca.dat) %>%
    mutate(GHGs = GHG / 10 * percent / 100, # Calculating impacts
           Land = Land / 10 * percent / 100,
           Eut = Eut / 10 * percent / 100,
           WatScar = WatScar / 10 * percent / 100) %>%
    group_by(id, product_name, Retailer, Department, Aisle, Shelf) %>%
    summarise(GHGs_100g = sum(GHGs, na.rm = TRUE), # Impacts by product, retailer, department, etc
              Land_100g = sum(Land, na.rm = TRUE),
              Eut_100g = sum(Eut, na.rm = TRUE),
              WatScar_100g = sum(WatScar, na.rm = TRUE))
  # Returning data set
  return(dat)
}

env.scaling.function =
  function(dat, env.indicators) {
    # Listed of scaled names
    cols.loops <- names(dat)[grep(env.indicators, names(dat))]
    scaled.names <- paste0('scaled_', cols.loops)
    
    # Looping through these to scale from 0 (no impact) to 100 (highest impact)
    for(i in 1:length(cols.loops)) {
      # Scaling from 0 to 1
      dat[,scaled.names[i]] <-
        dat[,cols.loops[i]] / max(dat[,cols.loops[i]], na.rm = TRUE)
      # Scaling from 0 to 100
      dat[,scaled.names[i]] <- dat[,scaled.names[i]] * 100
    }
    
    # Returning data set
    return(dat)
  }




# scaling.function =
#   function(dat, env.indicators) {
#     # Listed of scaled names
#     cols.loops <- names(dat)[grep(env.indicators, names(dat))]
#     scaled.names <- paste0('scaled_', cols.loops)
#     
#     # Looping through these to scale from 0 (no impact) to 100 (highest impact)
#     for(i in 1:length(cols.loops)) {
#       # Scaling from 0 to 1
#       dat[,scaled.names[i]] <-
#         dat[,cols.loops[i]] / max(dat[,gsub("[^serving_].*_","mean_",cols.loops[i])], na.rm = TRUE)
#       # Scaling from 0 to 100
#       dat[,scaled.names[i]] <- dat[,scaled.names[i]] * 100
#     }
#     
#     # And scaling nutriscore
#     dat[,'NutriScore_Scaled'] <-
#       (dat[,'NutriScorePoints'] + abs(min(dat[,'NutriScorePoints'], na.rm = TRUE))) /
#       (max(dat[,'NutriScorePoints'], na.rm = TRUE) + abs(min(dat[,'NutriScorePoints'], na.rm = TRUE)))
#     
#     dat[,'NutriScore_Scaled'] <- dat[,'NutriScore_Scaled'] * 100
#     
#     # And getting tot env scaled for 100g and for serving
#     dat[,'Tot_env_serving_scaled_lower_ci'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_serving_lower',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_serving_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_serving_scaled_upper_ci'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_serving_upper',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_serving_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_serving_min'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_serving_min',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_serving_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_serving_max'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_serving_max',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_serving_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_serving_scaled'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_serving_mean',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_serving_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_100g_scaled_lower_ci'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_lower',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_100g_scaled_upper_ci'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_upper',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_100g_scaled_min'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_min',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_100g_scaled_max'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_max',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     dat[,'Tot_env_100g_scaled'] <-
#       (rowSums(dat[,names(dat)[grep('scaled_mean',names(dat))]]) / 
#          max(rowSums(dat[,names(dat)[grep('scaled_mean',names(dat))]]), na.rm = TRUE)) * 100
#     
#     
#     # Returning data set
#     return(dat)
#   }

scaling.function <-
  function(dat, env.indicators) {
    # Listed of scaled names
    cols.loops <- names(dat)[grep(env.indicators, names(dat))]
    scaled.names <- paste0('scaled_', cols.loops)
    
    # Looping through these to scale from 0 (no impact) to 100 (highest impact)
    for(i in 1:length(cols.loops)) {
      # Scaling from 0 to 1
      dat[,scaled.names[i]] <-
        dat[,cols.loops[i]] / max(dat[,gsub("[^serving_].*_","mean_",cols.loops[i])], na.rm = TRUE)
      # Scaling from 0 to 100
      dat[,scaled.names[i]] <- dat[,scaled.names[i]] * 100
    }
    
    # And scaling nutriscore
    dat[,'NutriScore_Scaled'] <-
      (dat[,'NutriScorePoints'] + abs(min(dat[,'NutriScorePoints'], na.rm = TRUE))) /
      (max(dat[,'NutriScorePoints'], na.rm = TRUE) + abs(min(dat[,'NutriScorePoints'], na.rm = TRUE)))
    
    dat[,'NutriScore_Scaled'] <- dat[,'NutriScore_Scaled'] * 100
    
    # And udpating the rest for env
    dat <-
      dat %>%
      mutate(Tot_env_serving_scaled_lower_ci = (scaled_serving_lower_ci_GHG + scaled_serving_lower_ci_Eut + scaled_serving_lower_ci_Land + scaled_serving_lower_ci_WatScar)/4,
             Tot_env_serving_scaled_upper_ci = (scaled_serving_upper_ci_GHG + scaled_serving_upper_ci_Eut + scaled_serving_upper_ci_Land + scaled_serving_upper_ci_WatScar)/4,
             Tot_env_serving_scaled_min = (scaled_serving_min_GHG + scaled_serving_min_Eut + scaled_serving_min_Land + scaled_serving_min_WatScar)/4,
             Tot_env_serving_scaled_max = (scaled_serving_max_GHG + scaled_serving_max_Eut + scaled_serving_max_Land + scaled_serving_max_WatScar)/4,
             Tot_env_serving_scaled_lower_fifth = (scaled_serving_lower_fifth_GHG + scaled_serving_lower_fifth_Eut + scaled_serving_lower_fifth_Land + scaled_serving_lower_fifth_WatScar)/4,
             Tot_env_serving_scaled_upper_ninetyfifth = (scaled_serving_upper_ninetyfifth_GHG + scaled_serving_upper_ninetyfifth_Eut + scaled_serving_upper_ninetyfifth_Land + scaled_serving_upper_ninetyfifth_WatScar)/4,
             Tot_env_serving_scaled = (scaled_serving_mean_GHG + scaled_serving_mean_Eut + scaled_serving_mean_Land + scaled_serving_mean_WatScar)/4) %>%
      mutate(Tot_env_serving_scaled_lower_ci = Tot_env_serving_scaled_lower_ci / max(Tot_env_serving_scaled, na.rm = TRUE) * 100,
             Tot_env_serving_scaled_upper_ci = Tot_env_serving_scaled_upper_ci / max(Tot_env_serving_scaled, na.rm = TRUE) * 100,
             Tot_env_serving_scaled_min = Tot_env_serving_scaled_min / max(Tot_env_serving_scaled, na.rm = TRUE) * 100,
             Tot_env_serving_scaled_max = Tot_env_serving_scaled_max / max(Tot_env_serving_scaled, na.rm = TRUE) * 100,
             Tot_env_serving_scaled_lower_fifth = Tot_env_serving_scaled_lower_fifth / max(Tot_env_serving_scaled, na.rm = TRUE) * 100,
             Tot_env_serving_scaled_upper_ninetyfifth = Tot_env_serving_scaled_upper_ninetyfifth / max(Tot_env_serving_scaled, na.rm = TRUE) * 100,
             Tot_env_serving_scaled = Tot_env_serving_scaled / max(Tot_env_serving_scaled, na.rm = TRUE) * 100) %>%
      mutate(Tot_env_100g_scaled_lower_ci = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar)/4,
             Tot_env_100g_scaled_upper_ci = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar)/4,
             Tot_env_100g_scaled_min = (scaled_min_GHG + scaled_min_Eut + scaled_min_Land + scaled_min_WatScar)/4,
             Tot_env_100g_scaled_max = (scaled_max_GHG + scaled_max_Eut + scaled_max_Land + scaled_max_WatScar)/4,
             Tot_env_100g_scaled_lower_fifth = (scaled_lower_fifth_GHG + scaled_lower_fifth_Eut + scaled_lower_fifth_Land + scaled_lower_fifth_WatScar)/4,
             Tot_env_100g_scaled_upper_ninetyfifth = (scaled_upper_ninetyfifth_GHG + scaled_upper_ninetyfifth_Eut + scaled_upper_ninetyfifth_Land + scaled_upper_ninetyfifth_WatScar)/4,
             Tot_env_100g_scaled = (scaled_mean_GHG + scaled_mean_Eut + scaled_mean_Land + scaled_mean_WatScar)/4) %>%
      mutate(Tot_env_100g_scaled_lower_ci = Tot_env_100g_scaled_lower_ci / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
             Tot_env_100g_scaled_upper_ci = Tot_env_100g_scaled_upper_ci / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
             Tot_env_100g_scaled_min = Tot_env_100g_scaled_min / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
             Tot_env_100g_scaled_max = Tot_env_100g_scaled_max / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
             Tot_env_100g_scaled_lower_fifth = Tot_env_100g_scaled_lower_fifth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
             Tot_env_100g_scaled_upper_ninetyfifth = Tot_env_100g_scaled_upper_ninetyfifth / max(Tot_env_100g_scaled, na.rm = TRUE) * 100,
             Tot_env_100g_scaled = Tot_env_100g_scaled / max(Tot_env_100g_scaled, na.rm = TRUE) * 100)
    # And returning
    return(dat)
  }

  

### 
# Extracting serving size and unit
extract.serving.dat <-
  function(dat, 
           serving.col) {
    
    dat <- as.data.frame(dat)
    ###
    # Identifying rows with numbers in the serving data column
    rows.nums <- grep("[0-9]",dat[,serving.col])
    
    ###
    # Extracting numbers from these rows
    serving_value_new = str_extract_all(dat[,serving.col], "[0-9]{1,4}(\\s)?(g|kg|l|ml)(\\b|[^A-z])")
    serving_value_new2 = str_extract_all(dat[,serving.col], "[0-9]{1,4}(\\s)?\\.(\\s)?[0-9]{1,4}(\\s)?(g|kg|l|ml)(\\b|[^A-z])")
    serving_value_new3 = str_extract_all(dat[,serving.col],"1(\\s)?/(\\s)?[0-9]{1,2}")
    
    # Getting priorities
    # Value 2 over value 1
    # Value 2 and 1 over value 3
    # so long as numeric value in 2 and/or 1 does not == 1
    
    # Products with multiple numeric values identified
    multiple.list = c()
    for(i in 1:length(serving_value_new)) {
      # serving value 2 takes precedence over serving value 1
      if(serving_value_new2[i] != 'character(0)' & !is.na(serving_value_new2[i])) {
        serving_value_new[[i]] <- serving_value_new2[[i]]
      }
      tmp.value = gsub("[^0-9]","",serving_value_new[[i]])
      if(!is.numeric(tmp.value)) {
        tmp.value = 1
      }
      # Serving value 3 takes precedence over value 2 and 1 if value 2and/or1 != 1
      if(serving_value_new3[i] != 'character(0)' & !is.na(serving_value_new3[i]) & tmp.value == 1) {
        serving_value_new[[i]] <- serving_value_new3[[i]]
      }
      # Finding which have multiple values reported
      if(length(serving_value_new[[i]]) > 1) {
        multiple.list <- c(multiple.list,i)
      }
    }
    
    multiple.list2 = c()
    for(i in multiple.list) {
      tmp.value = NA
      # Series of logical statements
      if(is.na(tmp.value)) {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,}(\\s)?g(\\b|[^A-z])")
      }
      if(is.na(tmp.value) | tmp.value == 'character(0)') {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,4}(\\s)?\\.(\\s)?[0-9]{1,4}(\\s)?g(\\b|[^A-z])")
      }
      if(is.na(tmp.value) | tmp.value == 'character(0)') {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,}(\\s)?ml(\\b|[^A-z])")
      }
      if(is.na(tmp.value) | tmp.value == 'character(0)') {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,4}(\\s)?\\.(\\s)?[0-9]{1,4}(\\s)?ml(\\b|[^A-z])")
      }
      
      # Series to check if length still > 1
      # This catches pastas
      if(length(tmp.value[[1]]) > 1) {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,}g(\\s)dry") %>% gsub("(\\s)dry","",.)
      }
      # This catches pre-mixed noodle dishes
      if(length(tmp.value[[1]]) > 1 | tmp.value == 'character(0)' | tmp.value == '0g') {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,}(\\.)?([0-9]{1,})?g(\\s)(no|st|to)") %>%
          str_extract_all(.,"[0-9]{1,}(\\.)?([0-9]{1,})?g") %>%
          unlist(.) %>% gsub("g","",.) %>% as.numeric(.) %>% sum(.) %>% paste0(.,'g')
      }
      # This catches filled cookies. Specific, I know.
      if(length(tmp.value[[1]]) > 1 | tmp.value == 'character(0)' | tmp.value == '0g') {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,}(\\s)?g(\\s)?=(\\s)?1") %>% gsub("(\\s)=.*","",.) %>% gsub("(\\s)","",.)
      }
      # This catches tea
      if(length(tmp.value[[1]]) > 1 | tmp.value == 'character(0)' | tmp.value == '0g') {
        tmp.value = str_extract_all(dat[i,serving.col],"[0-9]{1,}(\\s)?ml") %>% unlist(.) %>% .[1]
      }
      
      serving_value_new[[i]] <- tmp.value
    }
    
    # Creating new colum for serving data
    # And updating this column
    dat[,'serving_data_new'] = NA
    for(i in 1:nrow(dat)) {
      if(length(serving_value_new[[i]]) != 1) {
        dat[i,'serving_data_new'] <- NA
      } else {
        dat[i,'serving_data_new'] <- serving_value_new[[i]]
      }
    }
    
    # Updating values that are e.g. "1/2" or "1/3"
    rows.updates <- grep("[0-9]/[0-9]",dat$serving_data_new)
    
    serving_value_new = str_extract_all(dat[,serving.col], "[0-9]{1,4}(\\s)?(g|kg|l|ml)(\\b|[^A-z])")
    serving_value_new2 = str_extract_all(dat[,serving.col], "[0-9]{1,4}(\\s)?\\.(\\s)?[0-9]{1,4}(\\s)?(g|kg|l|ml)(\\b|[^A-z])")
    
    for(i in rows.updates) {
      # Products with multiple numeric values identified
      multiple.list = c()
      # for(i in 1:length(serving_value_new)) {
        # serving value 2 takes precedence over serving value 1
        if(serving_value_new2[i] != 'character(0)' & !is.na(serving_value_new2[i])) {
          serving_value_new[[i]] <- serving_value_new2[[i]]
        # }
        }
      
      if(length(serving_value_new[[i]] <= 1)) {
        dat$serving_data_new[i] <- serving_value_new[[i]]
      }
    }
    
    # Creating new columns for serving value and unit
    dat[,'serving_value_new'] = NA
    dat[,'serving_unit_new'] = NA
    # And updating these columns
    dat[,'serving_value_new'] = gsub("[A-z].*","",dat[,'serving_data_new'], perl = TRUE)
    dat[,'serving_unit_new'] = gsub("[0-9]","",dat[,'serving_data_new'], perl = TRUE)
    dat[,'serving_unit_new'] = gsub("\\.","",dat[,'serving_unit_new'], perl = TRUE)
    
    # Extracting pack size for products where serving unit == [0-9]/[0-9]
    dat[,'pack_size_new'] = NA
    dat[,'pack_size_new_2'] = NA
    dat[,'pack_size_new'] = str_extract(dat$product_name,"[0-9]{1,}(\\s)?(g|ml|mL|Ml|ML|G|ML|kg|kG|Kg|KG)")
    dat[,'pack_size_new_2'] = str_extract(dat$product_name,"[0-9]{1,}.[0-9]{1,}(\\s)?(g|ml|mL|Ml|ML|G|ML|kg|kG|Kg|KG)")
    
    # Updating for periods
    dat$pack_size_new[grepl("\\.",dat$pack_size_new_2)] <-
      dat$pack_size_new_2[grepl("\\.",dat$pack_size_new_2)]
    
    # L or KG
    dat[,'pack_unit'] <- NA
    dat[,'pack_unit'] <- str_extract(dat$pack_size_new, "g|ml|mL|Ml|ML|G|ML|kg|kG|Kg|KG")
    
    # Removing units from pack size
    dat$pack_size_new <- gsub("[A-z]{1,}","",dat$pack_size_new)
    
    # Updating for kg and L
    dat$pack_size_new[dat$pack_unit %in% c('L','l','kg','kG','Kg','KG')] <-
      as.numeric(dat$pack_size_new[dat$pack_unit %in% c('l','L','kg','kG','Kg','KG')]) * 1000
    
    dat$pack_size_new <- as.numeric(dat$pack_size_new)
    
    # And updating for products with e.g. 9x37g listed as their pack size
    dat$pack_size_new[grepl("[0-9]{1,}(x|X)[0-9]{1,}",dat$pack_size_new_2)] <-
      as.numeric(gsub(".*(X|x)","",dat$pack_size_new_2[grepl("[0-9]{1,}(x|X)[0-9]{1,}",dat$pack_size_new_2)]) %>% gsub("(ml|mL|Ml|ML|g)","",.)) *
      as.numeric(gsub("(X|x).*","",dat$pack_size_new_2[grepl("[0-9]{1,}(x|X)[0-9]{1,}",dat$pack_size_new_2)]))
    
    # And updating pack unit
    dat$pack_unit[dat$pack_unit %in% c('l','L')] <- 'ml'
    dat$pack_unit[dat$pack_unit %in% c('kg','Kg','kG','KG')] <- 'g'
    
    # To lower case
    dat$pack_unit <- tolower(dat$pack_unit)
    
    # And removing white spaces
    dat$pack_size_new <- trimws(dat$pack_size_new, which = 'both')
    
    # Now updating serving size for products where serving = e.g. 1/16 of gateau
    dat$serving_value_new[grepl("[0-9]/[0-9]{1,}",dat$serving_value_new)] <- 
      as.numeric(gsub("/.*","",dat$serving_value_new[grepl("[0-9]/[0-9]{1,}",dat$serving_value_new)])) / # Converting e.g. 1/16 to decimal
      as.numeric(gsub(".*/","",dat$serving_value_new[grepl("[0-9]/[0-9]{1,}",dat$serving_value_new)])) * # And multiplying by pack size
      as.numeric(dat$pack_size_new[grepl("[0-9]/[0-9]{1,}",dat$serving_value_new)])
    
    # Converting serving to numeric values
    dat <- 
      dat %>%
      mutate(serving_value_new = as.numeric(serving_value_new),
             pack_size_new = as.numeric(pack_size_new))
    
    # Serving size cannot be greater than pack size
    dat$serving_value_new[dat$serving_value_new > dat$pack_size_new & !is.na(dat$serving_value_new) & !is.na(dat$pack_size_new)] <-
      NA
    
    # Updating column names
    dat <-
      dat %>%
      dplyr::select(-serving_data) %>% dplyr::select(-serving_value) %>% dplyr::select(-serving_unit) %>%
      dplyr::select(-pack_size_new_2) %>% dplyr::select(-pack_unit) %>%
      dplyr::rename(serving_data = serving_data_new, serving_value = serving_value_new, serving_unit = serving_unit_new) %>%
      mutate(serving_unit = ifelse(serving_value >= 1000, NA, serving_unit),
             serving_value = ifelse(serving_value >= 1000, NA, serving_value))
    # And returning the data
    return(dat)
  }


# Function to estimate impacts per serving, calorie, and protein
env.estimate.nutrition.function = 
  function(dat = dat,
           nutrients,
           thresholds) {
    
    # Estimating impacts
    dat <-
      dat %>%
      mutate(GHGs_serving = GHGs_100g * (as.numeric(serving_value_updated) / 100),
             Eut_serving = Eut_100g * (as.numeric(serving_value_updated) / 100),
             WatScar_serving = WatScar_100g * (as.numeric(serving_value_updated) / 100),
             Bio_serving = Bio_100g * (as.numeric(serving_value_updated) / 100),
             GHGs_10gProtein = GHGs_100g / (Protein / 10),
             Eut_10gProtein = Eut_100g / (Protein / 10),
             WatScar_10gProtein = WatScar_100g / (Protein / 10),
             Bio_10gProtein = Bio_100g / (Protein / 10),
             GHGs_100Calories = GHGs_100g / (Calories / 100),
             Eut_100Calories = Eut_100g / (Calories / 100),
             WatScar_100Calories = WatScar_100g / (Calories / 100),
             Bio_100Calories = Bio_100g / (Calories / 100))
    
    # Getting rid of estimates for products with below nutrition threshold
    # for(nutrient in nutrients) {
    #   index.drop = which(dat[,nutrient] < thresholds[which(nutrients %in% nutrient)])
    #   dat[index.drop,grep(paste0("[0-9](g)?",nutrient), names(dat))] <- NA
    # }
    
    # Returning data
    return(dat)
  }


# Scaling env impacts
env.scaling.function <-
  function(dat) {
    dat <-
      dat %>%
      mutate(GHGs_100g_scaled = GHGs_100g / max(.$GHGs_100g, na.rm = TRUE),
             Eut_100g_scaled = Eut_100g / max(.$Eut_100g, na.rm = TRUE),
             WatScar_100g_scaled = WatScar_100g / max(.$WatScar_100g, na.rm = TRUE),
             Bio_100g_scaled = Bio_100g / max(.$Bio_100g, na.rm = TRUE),
             GHGs_serving_scaled = GHGs_serving / max(.$GHGs_serving, na.rm = TRUE),
             Eut_serving_scaled = Eut_serving / max(.$Eut_serving, na.rm = TRUE),
             WatScar_serving_scaled = WatScar_serving / max(.$WatScar_serving, na.rm = TRUE),
             Bio_serving_scaled = Bio_serving / max(.$Bio_serving, na.rm = TRUE),
             GHGs_protein_scaled = GHGs_10gProtein / max(.$GHGs_10gProtein, na.rm = TRUE),
             Eut_protein_scaled = Eut_10gProtein / max(.$Eut_10gProtein, na.rm = TRUE),
             WatScar_protein_scaled = WatScar_10gProtein / max(.$WatScar_10gProtein, na.rm = TRUE),
             Bio_protein_scaled = Bio_10gProtein / max(.$Bio_10gProtein, na.rm = TRUE),
             GHGs_calories_scaled = GHGs_100Calories / max(.$GHGs_100Calories, na.rm = TRUE),
             Eut_calories_scaled = Eut_100Calories / max(.$Eut_100Calories, na.rm = TRUE),
             WatScar_calories_scaled = WatScar_100Calories / max(.$WatScar_100Calories, na.rm = TRUE),
             Bio_calories_scaled = Bio_100Calories / max(.$Bio_100Calories, na.rm = TRUE)) %>%
      mutate(TotEnv_100g_scaled = (GHGs_100g_scaled + Eut_100g_scaled + WatScar_100g_scaled + Bio_100g_scaled) / 4,
             TotEnv_serving_scaled = (GHGs_serving_scaled + Eut_serving_scaled + WatScar_serving_scaled + Bio_serving_scaled) / 4,
             TotEnv_protein_scaled = (GHGs_protein_scaled + Eut_protein_scaled + WatScar_protein_scaled + Bio_protein_scaled) / 4,
             TotEnv_calories_scaled = (GHGs_calories_scaled + Eut_calories_scaled + WatScar_calories_scaled + Bio_calories_scaled) / 4)
    
    return(dat)
  }

# Scaling env impacts
env.scaling.function.validate <-
  function(dat) {
    dat <-
      dat %>%
      mutate(GHGs_100g_scaled = GHGs_100g / max(.$GHGs_100g[.$Validate %in% 'No'], na.rm = TRUE),
             Eut_100g_scaled = Eut_100g / max(.$Eut_100g[.$Validate %in% 'No'], na.rm = TRUE),
             WatScar_100g_scaled = WatScar_100g / max(.$WatScar_100g[.$Validate %in% 'No'], na.rm = TRUE),
             Bio_100g_scaled = Bio_100g / max(.$Bio_100g[.$Validate %in% 'No'], na.rm = TRUE)) %>%
      mutate(TotEnv_100g_scaled = (GHGs_100g_scaled + Eut_100g_scaled + WatScar_100g_scaled + Bio_100g_scaled) / 4)
    
    return(dat)
  }

###
# Function to update serving sizes
update.serving.size = 
  function(dat) {
    
    # Summary by shelf
    shelf.sum <-
      dat %>% 
      group_by(Retailer, Department, Aisle, Shelf) %>%
      summarise(serving_shelf = mean(serving_value, na.rm = TRUE))
    # Summary by aisle
    aisle.sum <-
      dat %>% 
      group_by(Retailer, Department, Aisle) %>%
      summarise(serving_aisle = mean(serving_value, na.rm = TRUE))
    
    # Summary by department
    department.sum <-
      dat %>% 
      group_by(Retailer, Department) %>%
      summarise(serving_department = mean(serving_value, na.rm = TRUE))
    
    # Merging and updating
    dat <-
      left_join(dat,
                shelf.sum) %>%
      left_join(., aisle.sum) %>%
      left_join(., department.sum) %>%
      mutate(serving_value_updated = ifelse(!is.na(serving_value), serving_value,
                                            ifelse(!is.na(serving_shelf), serving_shelf,
                                                   ifelse(!is.na(serving_aisle), serving_aisle,
                                                          ifelse(!is.na(serving_department), serving_department, NA))))) %>%
      group_by(product_name) %>%
      summarise(serving_value_updated = mean(serving_value_updated, na.rm = TRUE))
  }



###
# RR function
estimate.rr <-
  function(dat, # used only for identifying SSBs
           stacked.dat.rr) {
    
    # Importing relative risk data
    rr.dat <- 
      read.csv("/Volumes/Citadel/Oxford/Research Projects/Env and Health Snapshot of FoodDB/Data Inputs/RR_Data_16September2020.csv",
               stringsAsFactors = FALSE) %>%
      rbind(data.frame(GroupingGroup = 'Potatoes',
                       DiseaseOutcomeGrouped = 'ACM',
                       Mean.RR = .88,
                       Upper.95..RR = 1.12,
                       Lower.95..RR = .69))
    
    
    # Adding serving sizes
    serving.dat <-
      data.frame(Food_Group = c('Chicken','Dairy','Eggs','Fish','Fruits','Legumes','Nuts',
                 'Olive oil','Potatoes','Processed red meat','Unprocessed red meat',
                 'Refined grains','SSBs','Vegetables','Whole grains'),
                 Serving_size = c(100,200,50,100,100,50,28,10,150,50,100,30,225,100,30))
    
    # Adjusting everything to 100g
    rr.dat <-
      left_join(rr.dat %>% dplyr::rename(Food_Group = GroupingGroup), serving.dat) %>%
      mutate(Mean.RR = Mean.RR ^ (100 / Serving_size),
             Lower.95..RR = Lower.95..RR ^ (100 / Serving_size),
             Upper.95..RR = Upper.95..RR ^ (100 / Serving_size)) %>%
      unique(.) %>%
      dplyr::select(-Serving_size)
    
    # Sorting into rr_groups
    stacked.dat.rr <-
      stacked.dat.rr %>%
      mutate(rr_group = NA)
    
    # Getting list of negative products
    neg.prods <-
      unique(stacked.dat.rr$product_name[stacked.dat.rr$percent < 0])
    
    # SSBs
    # Allocate SSBs on aisle/shelf
    
    # Identifying SSBs
    # To update later
    ssb.list <-
      unique(c(dat$product_name[dat$drinks %in% 'Drinks' & dat$Sugar_pack_value >= 8],
               dat$product_name[dat$drinks %in% 'Drinks' & dat$Sugar >= 8]))
    
    alcohol.list <-
      unique(dat$product_name[dat$alcohol %in% 'Alcohol'])
    
             #   dat$product_name[grepl('\\bPepsi\\b|\\bCoke\\b|\\bCoca\\b|\\bCola\\b|\\bTropicana\\b|\\bSprite\\b|\\bFanta\\b|\\bGatorade\\b|\\bDr Pepper\\b|\\bMountain Dew\\b|
             #                          \\bRed Bull\\b|\\bPoweraid\\b|\\bPower-Aid\\b|\\bSweetened Iced Tea\\b|\\bJuice\\b|\\bSmoothie\\b', dat$product_name, ignore.case = TRUE) & 
             #                      dat$Sugar_pack_value >= 8],
             # dat$product_name[grepl('\\bPepsi\\b|\\bCoke\\b|\\bCoca\\b|\\bCola\\b|\\bTropicana\\b|\\bSprite\\b|\\bFanta\\b|\\bGatorade\\b|\\bDr Pepper\\b|\\bMountain Dew\\b|
             #                          \\bRed Bull\\b|\\bPoweraid\\b|\\bPower-Aid\\b|\\bSweetened Iced Tea\\b|\\bJuice\\b|\\bSmoothie\\b', dat$product_name, ignore.case = TRUE) & 
             #                    dat$Sugar >= 8]))
    
    
    # Processed meats
    # processed red meat
    stacked.dat.rr <-
      stacked.dat.rr %>% 
      mutate(rr_group = ifelse(grepl('bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?', value, ignore.case = TRUE) & Food_Category %in% c('Pig Meat','Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)'), 'Processed red meat', rr_group)) %>%
      mutate(rr_group = ifelse(grepl('bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|smoked|cured|salted', product_name, ignore.case = TRUE) & Food_Category %in% c('Pig Meat','Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)') & is.na(rr_group), 'Processed red meat', rr_group))
    
    # processed meat
    stacked.dat.rr <-
      stacked.dat.rr %>% 
      mutate(rr_group = ifelse(grepl('bacon|hot(\\s)?dog|sausage|bratwurst|jerky|pate(s)?|paté(s)?|pâté(s)?', value, ignore.case = TRUE) & Food_Category %in% c('Poultry','Fish (farmed)','Crustaceans (farmed)') & is.na(rr_group), 'Processed white meat', rr_group)) %>%
      mutate(rr_group = ifelse(grepl('bacon|hot(\\s)?dog|sausage|bratwurst|jerky|pate(s)?|paté(s)?|pâté(s)?|smoked|cured|salted', product_name, ignore.case = TRUE) & Food_Category %in% c('Poultry','Fish (farmed)','Crustaceans (farmed)') & is.na(rr_group), 'Processed white meat', rr_group))
    
    # Whole grains
    stacked.dat.rr <-
      stacked.dat.rr %>%
      mutate(rr_group = ifelse(grepl('whole(\\s)?grain|whole(\\s)?meal|brown(\\s)?rice|whole|stoneground(\\s)?whole|oats|oatmeal|porridge', value, ignore.case = TRUE) & Food_Category %in% c('Barley (Beer)','Cereals & Oilcrops Misc.','Maize (Meal)','Pasta','Rice','Wheat & Rye (Bread)') & is.na(rr_group), 'Whole grains', rr_group)) %>%
      mutate(rr_group = ifelse(grepl('whole(\\s)?grain|whole(\\s)?meal|brown(\\s)?rice|whole|stoneground(\\s)?whole|oats|oatmeal|porridge', product_name, ignore.case = TRUE) & Food_Category %in% c('Barley (Beer)','Cereals & Oilcrops Misc.','Maize (Meal)','Pasta','Rice','Wheat & Rye (Bread)') & is.na(rr_group), 'Whole grains', rr_group))
      
    # Allocate remaining groups
    dat.remaining <-
      stacked.dat.rr %>%
      filter(is.na(rr_group)) %>%
      mutate(rr_group = ifelse(Food_Category %in% 'Poultry Meat', 'Chicken',
                               ifelse(Food_Category %in% c('Milk','Cheese','Butter, Cream & Ghee'), 'Dairy',
                                      ifelse(Food_Category %in% 'Eggs','Eggs',
                                             ifelse(Food_Category %in% c('Fish (farmed)','Crustaceans (farmed)'),'Fish',
                                                    ifelse(Food_Category %in% c('Apples','Bananas','Berries & Grapes','Citrus Fruit','Other Fruit'),'Fruits',
                                                           ifelse(Food_Category %in% c('Brassicas','Onions & Leeks','Root Vegetables','Tomatoes'),'Vegetables',
                                                                  ifelse(Food_Category %in% c('Peas','Other Pulses','Tofu'),'Pulses',
                                                                         ifelse(Food_Category %in% c('Nuts','Sunflower seeds','Groundnuts'),'Nuts',
                                                                                ifelse(Food_Category %in% c('Olive Oil','Rapeseed Oil','Sunflower Oil'),'Olive Oil',
                                                                                       ifelse(Food_Category %in% c('Potatoes','Cassava'),'Potatoes',
                                                                                              ifelse(Food_Category %in% c('Bovine Meat (beef herd)','Bovine Meat (dairy herd)','Lamb & Mutton','Pig Meat'), 'Unprocessed Red Meat',
                                                                                                     ifelse(Food_Category %in% c('Barley (Beer)','Cereals & Oilcrops Misc.','Maize (Meal)','Pasta','Rice','Wheat & Rye (Bread)'), 'Refined grains', rr_group)))))))))))))
    
    # rbinding
    dat.out <-
      rbind(stacked.dat.rr %>% filter(!is.na(rr_group)),
            dat.remaining) %>%
      unique(.)
    
    # Removing products with negative percentages
    dat.out <-
      dat.out %>%
      filter(!(product_name %in% neg.prods)) %>%
      filter(!(product_name %in% alcohol.list))
    
    # Taking summary by product and rr_group
    dat.out <-
      dat.out %>%
      mutate(value = tolower(value)) %>%
      group_by(product_name, rr_group, value) %>%
      summarise(percent = mean(percent, na.rm = TRUE)) %>%
      as.data.frame(.) %>%
      group_by(product_name, rr_group) %>%
      summarise(percent = sum(percent, na.rm = TRUE))
    
    # Updating SSBs
    dat.out <-
      dat.out %>%
      mutate(rr_group = ifelse(product_name %in% ssb.list, 'SSBs', rr_group))
    
    # Updating names
    dat.out$rr_group[dat.out$rr_group %in% 'Unprocessed Red Meat'] <- "Unprocessed red meat"
    dat.out$rr_group[dat.out$rr_group %in% 'Processed white meat'] <- "Processed red meat"
    dat.out$rr_group[dat.out$rr_group %in% 'Pulses'] <- "Legumes"
    dat.out$rr_group[dat.out$rr_group %in% 'Olive Oil'] <- "Olive oil"
    
    # Merging in health dat
    dat.out.rr <-
      left_join(dat.out,
                rr.dat %>% dplyr::rename (rr_group = Food_Group))
    
    # Updating NAs to 1
    dat.out.rr <-
      dat.out.rr %>%
      filter(!is.na(rr_group)) %>%
      mutate(Mean.RR = ifelse(is.na(Mean.RR), 1, Mean.RR)) %>%
      mutate(Lower.95..RR = ifelse(is.na(Lower.95..RR), 1, Lower.95..RR)) %>%
      mutate(Upper.95..RR = ifelse(is.na(Upper.95..RR), 1, Upper.95..RR))
    
    # Calculating based on percent composition
    dat.out.rr <-
      dat.out.rr %>%
      mutate(percent = ifelse(percent > 100, 100, percent)) %>%
      mutate(Mean_RR = Mean.RR ^ (percent / 100),
             Lower_RR = Lower.95..RR ^ (percent / 100),
             Upper_RR = Upper.95..RR ^ (percent / 100)) 
    
    # Log transforming so that these can be summed
    dat.out.rr <-
      dat.out.rr %>%
      mutate(Mean_RR = log(Mean_RR),
             Lower_RR = log(Lower_RR),
             Upper_RR = log(Upper_RR))
    
    # Summing by product
    dat.out.rr <-
      dat.out.rr %>%
      group_by(product_name,DiseaseOutcomeGrouped) %>%
      summarise(Mean_RR = sum(Mean_RR, na.rm = TRUE),
                Lower_RR = sum(Lower_RR, na.rm = TRUE),
                Upper_RR = sum(Upper_RR, na.rm = TRUE))
    
    # And re transforming
    dat.out.rr <-
      dat.out.rr %>%
      mutate(Mean_RR = exp(Mean_RR),
             Lower_RR = exp(Lower_RR),
             Upper_RR = exp(Upper_RR))

    # Returning data frame
    return(dat.out.rr)
  }



###
# Search terms to sort ingredients into the relative risk groups
search.rr.function <-
  function(dat) {
    
    # Creating new column
    dat <-
      dat %>%
      mutate(rr_group = NA)
    
    # Need search terms for red meat
    # processed red meat
    dat <-
      dat %>% 
      mutate(rr_group = ifelse(grepl('bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|smoked|cured|salted', value, ignore.case = TRUE) & Food_Category %in% c('Pigmeat','Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)'), 'Processed red meat')) %>%
      mutate(rr_group = ifelse(grepl('bacon|ham|hot(\\s)?dog|beef(\\s)?jerky|biltong|pepperoni|sausage|bratwurst|ham|roast(\\s)?beef|pastrami|corned(\\s)?beef|salami|deli(\\s)?meat|pate(s)?|paté(s)?|pâté(s)?|smoked|cured|salted', product_name, ignore.case = TRUE) & Food_Category %in% c('Pigmeat','Lamb & Mutton','Bovine Meat (beef herd)','Bovine Meat (dairy herd)') & is.na(rr_group), 'Processed red meat'))
    
    # processed meat
    dat <-
      dat %>% 
      mutate(rr_group = ifelse(grepl('bacon|hot(\\s)?dog|sausage|bratwurst|jerky|pate(s)?|paté(s)?|pâté(s)?|smoked|cured|salted', value, ignore.case = TRUE) & Food_Category %in% c('Poultry','Fish (farmed)','Crustaceans (farmed)') & is.na(rr_group), 'Processed white meat')) %>%
      mutate(rr_group = ifelse(grepl('bacon|hot(\\s)?dog|sausage|bratwurst|jerky|pate(s)?|paté(s)?|pâté(s)?|smoked|cured|salted', product_name, ignore.case = TRUE) & Food_Category %in% c('Poultry','Fish (farmed)','Crustaceans (farmed)') & is.na(rr_group), 'Processed white meat'))
    
    # Whole grains
    dat <-
      dat %>%
      mutate(rr_group = ifelse(grepl('whole(\\s)?grain|whole(\\s)?meal|brown(\\s)?rice|whole|stoneground(\\s)?whole|oats|oatmeal|porridge', value, ignore.case = TRUE) & Food_Category %in% c('Barley (Beer)','Cereals & Oilcrops Misc.','Maize (Meal)','Pasta','Rice','Wheat & Rye (Bread)') & is.na(rr_group), 'Whole grains')) %>%
      mutate(rr_group = ifelse(grepl('whole(\\s)?grain|whole(\\s)?meal|brown(\\s)?rice|whole|stoneground(\\s)?whole|oats|oatmeal|porridge', product_name, ignore.case = TRUE) & Food_Category %in% c('Barley (Beer)','Cereals & Oilcrops Misc.','Maize (Meal)','Pasta','Rice','Wheat & Rye (Bread)') & is.na(rr_group), 'Whole grains')) %>%
    
    
      
    # dat <-
    #   dat %>%
    #   mutate(rr_group = ifelse(Aisle %in% c('Coca Cola Shop','Cordials',''), 'SSBs',NA)) %>%
    #   mutate(rr_group = ifelse(Shelf %in% c(), 'SSBs',NA))
    
    
    # Allocate chicken, dairy, eggs, fish, fruits, legumes, nuts
    # olive oil (and other oils), potatoes, and vegetables based on LCA categories
    dat.remaining <-
      dat %>%
      filter(is.na(rr_group)) %>%
      mutate(rr_group = ifelse(Food_Category %in% 'Poultry', 'Chicken',
                               ifelse(Food_Category %in% c('Milk','Cheese','Butter, Cream & Ghee'), 'Dairy',
                                      ifelse(Food_Category %in% 'Eggs','Eggs',
                                             ifelse(Food_Category %in% c('Fish (farmed)','Crustaceans (farmed)'),'Fish',
                                                    ifelse(Food_Category %in% c('Apples','Bananas','Berries & Grapes','Citrus Fruit','Other Fruit'),'Fruits',
                                                           ifelse(Food_Category %in% c('Brassicas','Onions & Leeks','Root Vegetables','Tomatoes'),'Vegetables',
                                                                  ifelse(Food_Category %in% c('Peas','Other Pulses','Tofu'),'Pulses',
                                                                         ifelse(Food_Category %in% c('Nuts','Sunflower seeds','Groundnuts'),'Nuts',
                                                                                ifelse(Food_Category %in% c('Olive Oil','Rapeseed Oil','Sunflower Oil'),'Olive Oil',
                                                                                       ifelse(Food_Category %in% c('Potatoes','Cassava'),'Potatoes',
                                                                                              ifelse(Food_Category %in% c('Bovine Meat (beef herd)','Bovine Meat (dairy herd)','Lamb & Mutton','Pig Meat'), 'Unprocessed Red Meat',
                                                                                                     ifelse(Food_Category %in% c('Barley (Beer)','Cereals & Oilcrops Misc.','Maize (Meal)','Pasta','Rice','Wheat & Rye (Bread)'), 'Refined grains')))))))))))))
  }



# monte carlo function for lca estimates
# WIth organic and fish incorporated
monte.carlo.lca <-
  function(product.list) {
    # setting for reproducability
    set.seed(19)
    
    # updating to work with code below
    lca <- 
      lca.dat %>% 
      filter(!is.na(Weight)) %>%
      mutate(Weight = Weight + 0.1) %>% # This is to ensure weighting for foods isn't 0
      mutate(Food_Category_sub = ifelse(Food_Category_sub %in% '',NA,Food_Category_sub)) %>%
      mutate(Food_Category_sub_sub = ifelse(Food_Category_sub_sub %in% '',NA,Food_Category_sub_sub)) %>%
      mutate(Food_Category_sub = ifelse(Food_Category %in% 'Butter, Cream & Ghee',NA,Food_Category_sub)) %>%
      mutate(Food_Category = ifelse(grepl('Cheese',Food_Category_sub),'Cheese',Food_Category))# %>%
      # mutate(Food_Category_sub_sub = ifelse(grepl('yes',Average_of_sub_category,ignore.case=TRUE),NA,Food_Category_sub_sub)) %>%
      # mutate(Food_Category_sub = ifelse(grepl('yes',Average_of_original_category,ignore.case=TRUE),NA,Food_Category_sub))
    
    # List of which LCA subcategories are averages of the main category
    subcats.keep <- 
      read.csv(paste0(getwd(),'/Data Inputs/Search words, second round, 22Jan2022.csv')) %>%
      dplyr::select(LCA_Category, LCA_sub_category, LCA_sub_sub_category,
                    Average_of_original_category, Average_of_sub_category) %>%
      dplyr::rename(Food_Category = LCA_Category,
                    Food_Category_sub = LCA_sub_category,
                    Food_Category_sub_sub = LCA_sub_sub_category) %>%
      unique() %>%
      filter(grepl('yes',Average_of_original_category,ignore.case=TRUE) |
               grepl('yes',Average_of_sub_category,ignore.case=TRUE)) %>%
      mutate(Food_Category_sub_sub = ifelse(Food_Category_sub_sub %in% '', NA, Food_Category_sub_sub))
    
    
    # Getting count of organic observations by food category
    organic.count <-
      lca %>%
      dplyr::group_by(Food_Category, Food_Category_sub, Food_Category_sub_sub, Sys) %>%
      dplyr::summarise(organic_count = n()) %>%
      filter(Sys %in% 'O') %>%
      filter(organic_count >= 5)
    
    # indicators to be used to update name later...
    indicators <- names(lca)[!(grepl('Category|Weight|^Sys$',names(lca), ignore.case=TRUE))]
    
    # And indicators used to scale the impacts -- keeping other indicators available for use, but only some are used for the overall impact score
    indicators.scale <- c('GHG','Eut','WatScar','Land')
    
    # Which indicators are these in terms of numbers?
    # This makes it easier to deal with all of these at once later
    which.indicators.scale <- paste0('impact_',which(indicators.scale %in% indicators))
    
    # impacts 
    names(lca)[!(grepl('Category|Weight|^Sys$',names(lca), ignore.case=TRUE))] <-
      paste0('impact_',1:length(indicators))
    
    # Converting to numbers - raw data has '-' for missing values
    # Why, I don't know?
    lca[lca == '-'] <- NA
    # And converting to numeric values
    # Doing this in a loop - updating multiple rows at once doesn't seem to work
    for(k in grep('impact_[0-9]',names(lca))) {
      lca[,k] <- as.numeric(lca[,k])
    }
    
    # and adjusting units on lca estimates
    lca[,grepl('impact_[0-9]',names(lca))] <-
      lca[,grepl('impact_[0-9]',names(lca))]/10
    
    # Number of reps
    n_rep = 1000
    # Fifth and 95th percentiles
    fifth.obs = round(n_rep * .05, digits = 0)
    ninetyfifth.obs = round(n_rep * .95, digits = 0)
    tenth.obs = round(n_rep * .1, digits = 0)
    ninety.obs = round(n_rep * .9, digits = 0)
    twentyfifth.obs = round(n_rep * .25, digits = 0)
    seventyfifth.obs = round(n_rep * .75, digits = 0)
    fifty.obs = round(n_rep * .5, digits = 0)


    # tmp food df
    # filtering to avoid processing power if doing this in parallel
    tmp.food.df <- 
      food.df %>%
      filter(product_name %in% product.list) %>%
      mutate(amount = amount / 100) # adjusting to proportional composition
    
    # Adjusting food categories that aren't present in the LCA database
    # This will need to be changed as HESTIA is updated
    tmp.food.df <-
      left_join(tmp.food.df,
                lca %>%
                  dplyr::select(Food_Category,Food_Category_sub,Food_Category_sub_sub,
                                Average_of_original_category,Average_of_sub_category) %>%
                  unique()) %>%
      mutate(Food_Category_sub_sub = ifelse(grepl('yes',Average_of_sub_category,ignore.case=TRUE), NA, Food_Category_sub_sub)) %>%
      mutate(Food_Category_sub = ifelse(grepl('yes',Average_of_original_category,ignore.case=TRUE), NA, Food_Category_sub))
    
    # And removing similar sub cats from the lca database that don't have more than 5 observations
    lca <- 
      lca %>%
      mutate(Food_Category_sub_sub = ifelse(grepl('yes',Average_of_sub_category,ignore.case=TRUE),NA,Food_Category_sub_sub)) %>%
      mutate(Food_Category_sub = ifelse(grepl('yes',Average_of_original_category,ignore.case=TRUE),NA,Food_Category_sub))
    
    # Creating data frame
    # out.df <- matrix(nrow = length(product.list), ncol = (4+length(indicators)*13))
    # out.df <- as.data.frame(out.df)
    # names(out.df) <- c('product_name','id',paste0('mean_',1:length(indicators)),
    #                   paste0('se_',1:length(indicators)),
    #                    paste0('lower_ci_',1:length(indicators)),
    #                    paste0('upper_ci_',1:length(indicators)),
    #                    paste0('min_',1:length(indicators)),
    #                    paste0('max_',1:length(indicators)),
    #                    paste0('lower_fifth_',1:length(indicators)),
    #                    paste0('lower_tenth_',1:length(indicators)),
    #                    paste0('lower_twentyfifth_',1:length(indicators)),
	# 	       paste0('fifty_',1:length(indicators)),
    #                    paste0('upper_seventyfifth_',1:length(indicators)),
    #                    paste0('upper_ninety_',1:length(indicators)),
    #                    paste0('upper_ninetyfifth_',1:length(indicators)),
    #                    'check_index_min','check_index_max')
    # out.df$product_name <- product.list

	out.df <- data.frame()

    # LCA baseline for scaling impacts
    # This isn't quite perfect, but it is close
    # Multiplying by global weighting
    lca.scaling <- 
      lca[,c(paste0('impact_',which(indicators%in%indicators.scale)))] * lca[,'Weight'] / 100
    lca.scaling$Weight <- lca$Weight
    lca.scaling[,'Weight'] <- lca.scaling[,'Weight'] / 100
    # Getting weighted impacts
    lca.scaling <-rowsum(lca.scaling,group=lca$Food_Category)
    # Adjusting for representation of global weighting
    lca.scaling[,paste0('impact_',which(indicators%in%indicators.scale))] <- 
      lca.scaling[,paste0('impact_',which(indicators%in%indicators.scale))] * (1/lca.scaling[,'Weight'])
    
    # Adding identifiers for wild and aqua fish
    lca <-
      lca %>%
      mutate(aquaculture = NA) %>%
      mutate(aquaculture = ifelse(Food_Category %in% c('Fish','Crustaceans'),'Yes',
                                  ifelse(Food_Category %in% c('Fish (capture)','Crustaceans (capture)'),'No',aquaculture))) %>%
      mutate(Food_Category = ifelse(Food_Category %in% 'Fish (capture)','Fish',
                                    ifelse(Food_Category %in% 'Crustaceans (capture)','Crustaceans',Food_Category)))
    # Getting count by sub sub category - using these for checks with 5 or more observations
    lca.count.sub.sub <-
      lca %>%
      dplyr::group_by(Food_Category,Food_Category_sub,Food_Category_sub_sub,aquaculture) %>%
      dplyr::summarise(count = n()) %>%
      filter(count >= 5)
    # Getting count by sub category - using these for checks with 5 or more observations
    lca.count.sub <-
      lca %>%
      dplyr::group_by(Food_Category,Food_Category_sub,aquaculture) %>%
      dplyr::summarise(count = n()) %>%
      filter(count >= 5)
    
    # Find the categories that need to have rows duplicated to meet min count of 5 observations
    lca.count <-
      lca %>%
      dplyr::group_by(Food_Category) %>%
      dplyr::summarise(count = n()) %>%
      filter(count < 5)
    
    lca <-
      rbind(lca,
            do.call(rbind, 
                    replicate(5,# 5 times
                              lca %>% filter(Food_Category %in% lca.count$Food_Category), # Only food categories with fewer than 5 observations
                              simplify = FALSE)))
    
    # And making sure categories are identical
    tmp.food.df <-
      tmp.food.df %>%
      mutate(Food_Category_sub_sub = 
               ifelse(paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub) %in%
                        paste0(lca.count.sub.sub$Food_Category,lca.count.sub.sub$Food_Category_sub,lca.count.sub.sub$Food_Category_sub_sub),
                      Food_Category_sub_sub,NA)) %>% # If in LCA categories, keep categorisation, else, convert to NA
      mutate(Food_Category_sub = 
               ifelse(paste0(Food_Category,Food_Category_sub) %in%
                        paste0(lca.count.sub$Food_Category,lca.count.sub$Food_Category_sub),
                      Food_Category_sub,NA)) # If in LCA categories, keep categorisation, else, convert to NA
    
    
    # looping across foods
    for(i in product.list) {
      
      # Doing checks on : 
      # (a) fish
      # (b) organic systems
      # These require exceptions
      tmp.lca.sample.aqua <- data.frame()
      tmp.lca.sample.fish <- data.frame()
      tmp.lca.sample.org <- data.frame()
      
      # Checking fish
      if(sum(grepl('Fish|Crustacean',tmp.food.df$Food_Category[tmp.food.df$product_name %in% i],ignore.case=TRUE)) > 0) { # Is there any seafood in the product
        # If yes, sample 500 points from aquaculture
        # And 500 points from wild-caught
        
        # Sampling points from aquaculture
        # selecting one producer from the category
        tmp.lca.sample.aqua <- 
          lca %>%
          filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                   Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i] &
                   Food_Category_sub_sub %in% tmp.food.df$Food_Category_sub_sub[tmp.food.df$product_name %in% i]) %>%
          filter(Food_Category %in% c('Fish','Crustaceans')) %>%
          filter(aquaculture %in% 'Yes') %>%
          filter(paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub,aquaculture) %in%
                   paste0(lca.count.sub.sub$Food_Category,lca.count.sub.sub$Food_Category_sub,lca.count.sub.sub$Food_Category_sub_sub,lca.count.sub.sub$aquaculture))
        
        # Filtering to foods with more than 5 observations
        
        # Sample if more than 5 observations
        if(nrow(tmp.lca.sample.aqua) > 5) {
          tmp.lca.sample.aqua <- 
            tmp.lca.sample.aqua %>%
            group_by(Food_Category, Food_Category_sub, Food_Category_sub_sub) %>% 
            sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category,.$Food_Category_sub,.$Food_Category_sub_sub)))))
          
          if(nrow(tmp.lca.sample.aqua) != # How many samples do we have
             nrow(tmp.food.df[tmp.food.df$product_name%in%i & grepl('Fish|Crustacean',tmp.food.df$Food_Category),])*500) { # How many should we have {
            # If not the same, need to find which food cat we don't have
            tmp.lca.sample.aqua.addendum <- 
              lca %>%
              filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                       Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i]) %>%
              filter(Food_Category %in% c('Fish','Crustaceans')) %>%
              filter(aquaculture %in% 'Yes') %>%
              filter(paste0(Food_Category,Food_Category_sub,aquaculture) %in%
                       paste0(lca.count.sub$Food_Category,lca.count.sub$Food_Category_sub,lca.count.sub$aquaculture)) %>%
              filter(!(paste0(Food_Category,Food_Category_sub) %in% paste0(tmp.lca.sample.aqua$Food_Category,tmp.lca.sample.aqua$Food_Category_sub)))
          
          if(nrow(tmp.lca.sample.aqua.addendum) >= 5) {
            # If more than 5 observations, sample
            tmp.lca.sample.aqua.addendum <- 
              tmp.lca.sample.aqua.addendum %>%
              group_by(Food_Category, Food_Category_sub) %>% 
              sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
              .[order(.$Food_Category),] %>%
              as.data.frame(.) %>%
              mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category,.$Food_Category_sub))))) %>%
              mutate(Food_Category_sub_sub = NA)
            # And rbinding
            tmp.lca.sample.aqua <-
              rbind(tmp.lca.sample.aqua,
                    tmp.lca.sample.aqua.addendum)
            
            # ANd another check for primary category
            if(nrow(tmp.lca.sample.aqua) != # How many samples do we have
               nrow(tmp.food.df[tmp.food.df$product_name%in%i & grepl('Fish|Crustacean',tmp.food.df$Food_Category),])*500) {
              # Sampling
              tmp.lca.sample.aqua.addendum <- 
                lca %>%
                filter(Food_Category %in% c('Fish','Crustaceans')) %>%
                filter(aquaculture %in% 'Yes') %>%
                group_by(Food_Category) %>% 
                sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
                .[order(.$Food_Category),] %>%
                as.data.frame(.) %>%
                mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category))))) %>%
                mutate(Food_Category_sub_sub = NA, Food_Category_sub = NA)
              # And rbinding
              tmp.lca.sample.aqua <-
                rbind(tmp.lca.sample.aqua,
                      tmp.lca.sample.aqua.addendum)
              
            }
            
          } else { # If not enough rows, then sample based on primary category
            tmp.lca.sample.aqua.addendum <- 
              lca %>%
              filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i]) %>%
              filter(Food_Category %in% c('Fish','Crustaceans')) %>%
              filter(aquaculture %in% 'Yes') %>%
              # filter(!(paste0(Food_Category) %in% paste0(tmp.lca.sample.aqua$Food_Category))) %>%
              group_by(Food_Category) %>% 
              sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
              .[order(.$Food_Category),] %>%
              as.data.frame(.) %>%
              mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category))))) %>%
              mutate(Food_Category_sub_sub = NA,
                     Food_Category_sub = NA)
            # And rbinding
            tmp.lca.sample.aqua <-
              rbind(tmp.lca.sample.aqua,
                    tmp.lca.sample.aqua.addendum)
            
          } # End of if statement for sub category sampling
          
          
             } # End of if statement for sub sub category sampling 
          
        } else { # If fewer than 5 observations
          # By category and sub category
          tmp.lca.sample.aqua <- 
            lca %>%
            filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                     Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i]) %>%
            filter(Food_Category %in% c('Fish','Crustaceans')) %>%
            filter(aquaculture %in% 'Yes') %>%
            filter(paste0(Food_Category,Food_Category_sub,aquaculture) %in%
                     paste0(lca.count.sub$Food_Category,lca.count.sub$Food_Category_sub,lca.count.sub$aquaculture))
          # If more than 5 observations, then sample
          if(nrow(tmp.lca.sample.aqua) > 5) {
            tmp.lca.sample.aqua <- 
              tmp.lca.sample.aqua %>%
              group_by(Food_Category, Food_Category_sub) %>% 
              sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
              .[order(.$Food_Category),] %>%
              as.data.frame(.) %>%
              mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category,.$Food_Category_sub))))) %>%
              mutate(Food_Category_sub_sub = NA)
            
            if(nrow(tmp.lca.sample.aqua) !=
               nrow(tmp.food.df[tmp.food.df$product_name%in%i & grepl('Fish|Crustacean',tmp.food.df$Food_Category),])*500) {
              # And a check to make sure everything has matched...
              tmp.lca.sample.aqua.addendum <- 
                lca %>%
                filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i]) %>%
                filter(Food_Category %in% c('Fish','Crustaceans')) %>%
                filter(aquaculture %in% 'Yes') %>%
                filter(!(paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub) %in% paste0(tmp.lca.sample.aqua$Food_Category,tmp.lca.sample.aqua$Food_Category_sub,tmp.lca.sample.aqua$Food_Category_sub_sub))) %>%
                group_by(Food_Category) %>% 
                sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
                .[order(.$Food_Category),] %>%
                as.data.frame(.) %>%
                mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category))))) %>%
                mutate(Food_Category_sub_sub = NA,
                       Food_Category_sub = NA)
              # And rbinding
              tmp.lca.sample.aqua <-
                rbind(tmp.lca.sample.aqua,
                      tmp.lca.sample.aqua.addendum) 
            }
          } else { # If less than 5 observations
            tmp.lca.sample.aqua <- 
              lca %>%
              filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i]) %>%
              filter(Food_Category %in% c('Fish','Crustaceans')) %>%
              filter(aquaculture %in% 'Yes') %>%
              group_by(Food_Category) %>% 
              sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
              .[order(.$Food_Category),] %>%
              as.data.frame(.) %>%
              mutate(sample_index = rep(1:(n_rep/2), length(unique(paste0(.$Food_Category))))) %>%
              mutate(Food_Category_sub = NA,
                     Food_Category_sub_sub = NA)
          } # End sampling for aquaculture
        }
        
        # And sampling wild caught fish
        tmp.lca.sample.fish <- 
          lca %>%
          filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                   Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i]) %>%
          filter(Food_Category %in% c('Fish','Crustaceans')) %>%
          filter(aquaculture %in% 'No') %>%
          filter(paste0(Food_Category,Food_Category_sub,aquaculture) %in%
                   paste0(lca.count.sub$Food_Category,lca.count.sub$Food_Category_sub,lca.count.sub$aquaculture))
        # If observations, then randomly sample
        if(nrow(tmp.lca.sample.fish) > 0) { # Don't need threshold of 5, as there are ~100 observations for each wild caught fish
          tmp.lca.sample.fish <- 
            tmp.lca.sample.fish %>%
            group_by(Food_Category, Food_Category_sub) %>% 
            sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep((n_rep/2+1):(n_rep), length(unique(paste0(.$Food_Category,.$Food_Category_sub))))) %>%
            mutate(Food_Category_sub_sub = NA)
          
          # Only keeping observations that match with food df dataset
          tmp.lca.sample.fish <-
            tmp.lca.sample.fish %>%
            filter(paste0(Food_Category, Food_Category_sub) %in%
                     paste0(tmp.food.df$Food_Category[tmp.food.df$product_name %in% i],tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i]))
          
          # Now checking if there are the right number of sampling
          # If not, then sampling based on primary category
          if(nrow(tmp.lca.sample.fish) !=
             nrow(tmp.food.df[tmp.food.df$product_name%in%i & grepl('Fish|Crustacean',tmp.food.df$Food_Category),])*500) {
            # If not the same number of rows, then need to sample again
            tmp.lca.sample.fish.addendum <- 
              lca %>%
              filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i]) %>%
              filter(!(paste0(Food_Category)%in%paste0(tmp.lca.sample.fish$Food_Category))) %>%
              filter(Food_Category %in% c('Fish','Crustaceans')) %>%
              filter(aquaculture %in% 'No')
            
            if(nrow(tmp.lca.sample.fish.addendum)>0) {
              tmp.lca.sample.fish.addendum <-
                tmp.lca.sample.fish.addendum %>%
                group_by(Food_Category) %>% 
                sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
                .[order(.$Food_Category),] %>%
                as.data.frame(.) %>%
                mutate(sample_index = rep((n_rep/2+1):(n_rep), length(unique(paste0(.$Food_Category))))) %>%
                mutate(Food_Category_sub_sub = NA)

              # And rbinding
              tmp.lca.sample.fish <-
                rbind(tmp.lca.sample.fish,
                      tmp.lca.sample.fish.addendum)
              }
          }
        } else { # If not, randomly sample based only on food category
          tmp.lca.sample.fish <- 
            lca %>%
            filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i]) %>%
            filter(Food_Category %in% c('Fish','Crustaceans')) %>%
            filter(aquaculture %in% 'No') %>%
            group_by(Food_Category) %>% 
            sample_n(n_rep/2, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep((n_rep/2+1):(n_rep), length(unique(paste0(.$Food_Category))))) %>%
            mutate(Food_Category_sub = NA)
        }
      } # End of sampling for fish
      
      # Checking organic
      if(sum(tmp.food.df$Organic_ingredient[tmp.food.df$product_name%in%i] %in% 'Organic') > 0) { # If more than one organic ingredient
        # Looking through LCA database to see if there are more than 5 organic observations for this food
        tmp.lca.sample.org <-
          lca %>%
          filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                   Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i] &
                   Food_Category_sub_sub %in% tmp.food.df$Food_Category_sub_sub[tmp.food.df$product_name %in% i]) %>%
          filter(Food_Category %in% organic.count$Food_Category &
                   Food_Category_sub %in% organic.count$Food_Category_sub &
                   Food_Category_sub_sub %in% organic.count$Food_Category_sub_sub) %>%
          filter(Sys %in% 'O')
        
        # If the above data frame contains at least 1 row 
        # The criteria for 5 observations already filtered above
        if(nrow(tmp.lca.sample.org) >= 1) {
          tmp.lca.sample.org <-
            tmp.lca.sample.org %>%
            group_by(Food_Category,Food_Category_sub,Food_Category_sub_sub) %>%
            sample_n(n_rep, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep(1:n_rep, length(unique(paste0(.$Food_Category,.$Food_Category_sub,.$Food_Category_sub_sub)))))
        }
      }
      
      # Randomly selecting food producers
      # If else checks to make sure observations are matching
      
      tmp.lca.sample <- 
        lca %>%
        filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                 Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i] &
                 Food_Category_sub_sub %in% tmp.food.df$Food_Category_sub_sub[tmp.food.df$product_name %in% i])
      
      if(nrow(tmp.lca.sample)>1) { # If there's an observation
        tmp.lca.sample <-
          tmp.lca.sample %>%
          group_by(Food_Category,Food_Category_sub,Food_Category_sub_sub) %>% 
          sample_n(n_rep, weight = Weight, replace = TRUE) %>%
          .[order(.$Food_Category),] %>%
          as.data.frame(.) %>%
          mutate(sample_index = rep(1:n_rep, length(unique(paste0(.$Food_Category,.$Food_Category_sub,.$Food_Category_sub_sub)))))
      } else { # If the above hasn't matched
        # Search only based on sub category
        tmp.lca.sample <- 
          lca %>%
          filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i] &
                   Food_Category_sub %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i])
        if(nrow(tmp.lca.sample)>1) { # If there's an observation
          tmp.lca.sample <-
            tmp.lca.sample %>%
            group_by(Food_Category, Food_Category_sub) %>% 
            sample_n(n_rep, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep(1:n_rep, length(unique(paste0(.$Food_Category,.$Food_Category_sub))))) %>%
            mutate(Food_Category_sub_sub = NA)
        } else { # if not, search based only on food category
          tmp.lca.sample <- 
            lca %>%
            filter(Food_Category %in% tmp.food.df$Food_Category[tmp.food.df$product_name %in% i]) %>%
            group_by(Food_Category) %>% 
            sample_n(n_rep, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep(1:n_rep, length(unique(.$Food_Category)))) %>%
            mutate(Food_Category_sub_sub = NA,
                   Food_Category_sub = NA)
        }
      }
      
      # Rbinding fish and organic data back on
      # Fish data
      if(sum(grepl('Fish|Crustacean',tmp.food.df$Food_Category[tmp.food.df$product_name %in% i],ignore.case=TRUE)) > 0) {
        tmp.lca.sample <-
          rbind(tmp.lca.sample %>% filter(!grepl('Fish|Crustacean',Food_Category,ignore.case=TRUE)), # Getting rid of the sampled aquaculture data
                tmp.lca.sample.aqua, # And adding on the aqua and capture fishery data
                tmp.lca.sample.fish)
        # Exception for organic and fish
        if(sum(tmp.food.df$Organic_ingredient[tmp.food.df$product_name%in%i] %in% 'Organic') > 0) {
          tmp.lca.sample <-
            rbind(tmp.lca.sample.org,
                  tmp.lca.sample %>%
                    filter(!(paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub) %in%
                               paste0(tmp.lca.sample.org$Food_Category,tmp.lca.sample.org$Food_Category_sub,tmp.lca.sample.org$Food_Category_sub_sub) &
                               !grepl('Fish|Crustacean',tmp.lca.sample$Food_Category))))
          tmp.lca.sample.org <- data.frame()
        }
      } else if (sum(tmp.food.df$Organic_ingredient[tmp.food.df$product_name%in%i] %in% 'Organic') > 0) {
        if(nrow(tmp.lca.sample.org)>0) {# if data frame is 0 rows long, then don't rbind
          tmp.lca.sample <-
            rbind(tmp.lca.sample.org,
                  tmp.lca.sample %>%
                    filter(!(paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub) %in%
                               paste0(tmp.lca.sample.org$Food_Category,
                                      tmp.lca.sample.org$Food_Category_sub,
                                      tmp.lca.sample.org$Food_Category_sub_sub))))
        } # End if statement for organic foods
      } # End if statement for fish & organic foods
      
      # Making sure LCA info is present for all products
      # These are the ones that have matched
      tmp.have <- 
        tmp.lca.sample %>%
        dplyr::select(Food_Category,Food_Category_sub,Food_Category_sub_sub) %>%
        unique(.) %>%
        mutate(have = 'yes') %>%
        mutate(check = paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub))
      
      # The ones that are needed
      tmp.need <-
        tmp.food.df %>%
        filter(product_name %in% i) %>%
        dplyr::select(Food_Category,Food_Category_sub,Food_Category_sub_sub) %>%
        unique(.) %>%
        mutate(check = paste0(Food_Category,Food_Category_sub,Food_Category_sub_sub))
      
      # The ones that haven't matched
      if(sum(!(tmp.need$check %in% tmp.have$check)) > 0) {
        tmp.needed <-
          left_join(tmp.need,tmp.have) %>%
          filter(is.na(have)) %>%
          filter(!grepl('Fish|Crustacean',Food_Category,ignore.case=TRUE))
        
        # Creating empty data frames
        tmp.lca.sample.addendum.sub <- data.frame()
        tmp.lca.sample.addendum <- data.frame()
        
        # Addendums for sub sub categories that haven't matched
        if(sum(!is.na(tmp.needed$Food_Category_sub)) > 0) {
          tmp.lca.sample.addendum.sub <- 
            lca %>%
            filter(Food_Category %in% tmp.needed$Food_Category &
                     Food_Category_sub %in% tmp.needed$Food_Category_sub) 
          
          if(nrow(tmp.lca.sample.addendum.sub) > 5) {
            tmp.lca.sample.addendum.sub <- 
              tmp.lca.sample.addendum.sub %>%
              group_by(Food_Category, Food_Category_sub) %>% 
              sample_n(n_rep, weight = Weight, replace = TRUE) %>%
              .[order(.$Food_Category),] %>%
              as.data.frame(.) %>%
              mutate(sample_index = rep(1:n_rep, length(unique(.$Food_Category)))) %>%
              mutate(Food_Category_sub_sub = NA)
          } else {
            tmp.lca.sample.addendum.sub <- 
              lca %>%
              filter(Food_Category %in% tmp.needed$Food_Category) %>%
              group_by(Food_Category) %>% 
              sample_n(n_rep, weight = Weight, replace = TRUE) %>%
              .[order(.$Food_Category),] %>%
              as.data.frame(.) %>%
              mutate(sample_index = rep(1:n_rep, length(unique(.$Food_Category)))) %>%
              mutate(Food_Category_sub_sub = NA)
            }
        }
        
        # Addendums for sub categories that haven't matched
        if(sum(!is.na(tmp.needed$Food_Category[is.na(tmp.needed$Food_Category_sub)])) > 0) {
          # Random sampling these
          tmp.lca.sample.addendum <- 
            lca %>%
            filter(Food_Category %in% tmp.needed$Food_Category[is.na(tmp.needed$Food_Category_sub)]) %>%
            group_by(Food_Category) %>% 
            sample_n(n_rep, weight = Weight, replace = TRUE) %>%
            .[order(.$Food_Category),] %>%
            as.data.frame(.) %>%
            mutate(sample_index = rep(1:n_rep, length(unique(.$Food_Category)))) %>%
            mutate(Food_Category_sub = NA, Food_Category_sub_sub = NA)
        }
        # And adding these to the random sampling data frame
        tmp.lca.sample <-
          rbind(tmp.lca.sample,
                tmp.lca.sample.addendum,
                tmp.lca.sample.addendum.sub)
      }  # End of statement adding on the additional sampling information
      
      
      # Merging LCA impacts with ingredients info
      # Doing this in two parts - fish, then not fish
      
      # Empty data frame
      tmp.food.fish <- data.frame()
      # Need exception to see if there is fish
      if(sum(grepl('Fish|Crustacean',tmp.food.df$Food_Category[tmp.food.df$product_name %in% i],ignore.case=TRUE)) > 0) {
        tmp.food.trout <- data.frame()
        tmp.food.other.aqua <- data.frame()
        tmp.food.shrimp.prawn <- data.frame()
        tmp.food.other.fish <- data.frame()
        # For aqua
        # If sub sub category in trout, merge on sub sub category
        if('Trout' %in% tmp.food.df$Food_Category_sub_sub[tmp.food.df$product_name %in% i]) {
          tmp.food.trout <- 
            left_join(tmp.food.df %>%
                        filter(product_name %in% i) %>%
                        filter(Food_Category %in% c('Fish','Crustaceans')) %>%
                        filter(Food_Category_sub_sub %in% 'Trout'), # limiting food info to trout
                      tmp.lca.sample %>%
                        dplyr::select(-Average_of_original_category) %>%
                        dplyr::select(-Average_of_sub_category) %>%
                        filter(Food_Category_sub_sub %in% 'Trout')) # limiting lca info to trout - don't have sub categories for capture, so don't need to worry about this
          
          tmp.food.other.aqua <-
            left_join(tmp.food.df %>%
                        filter(product_name %in% i) %>%
                        filter(Food_Category %in% c('Fish','Crustaceans')) %>%
                        filter(!(Food_Category_sub_sub %in% 'Trout')), # Limitng to everything but trout
                      tmp.lca.sample %>%
                        dplyr::select(-Average_of_original_category) %>%
                        dplyr::select(-Average_of_sub_category) %>%
                        dplyr::select(-Food_Category_sub_sub) %>%
                        filter(aquaculture %in% 'Yes')) # Limiting to aquaculture
          
        } else {
          tmp.food.other.aqua <-
            left_join(tmp.food.df %>%
                        filter(product_name %in% i) %>%
                        filter(Food_Category %in% c('Fish','Crustaceans')) %>%
                        filter(!(Food_Category_sub_sub %in% 'Trout')), # Limitng to everything but trout
                      tmp.lca.sample %>%
                        dplyr::select(-Average_of_original_category) %>%
                        dplyr::select(-Average_of_sub_category) %>%
                        dplyr::select(-Food_Category_sub_sub) %>%
                        filter(aquaculture %in% 'Yes')) # Limiting to aquaculture
        }
        
        # For capture fish
        # Need exception for prawn and shrimp
        if(sum(c('Shrimp','Prawn') %in% tmp.food.df$Food_Category_sub[tmp.food.df$product_name %in% i])>0) {
          # Shrimp and prawn are only capture seafood with sub category information
          tmp.food.shrimp.prawn <- 
            left_join(tmp.food.df %>%
                        filter(product_name %in% i) %>%
                        filter(Food_Category %in% c('Crustaceans')) %>%
                        filter(Food_Category_sub %in% c('Shrimp','Prawn')), # Limiting to shrimp and prawn
                      tmp.lca.sample %>%
                        dplyr::select(-Average_of_original_category) %>%
                        dplyr::select(-Average_of_sub_category) %>%
                        filter(Food_Category_sub %in% c('Shrimp','Prawn')) %>%
                        filter(aquaculture %in% 'No')) # Limiting to capture fishers
          # And other fish
          tmp.food.other.fish <-
            left_join(tmp.food.df %>%
                        filter(product_name %in% i) %>%
                        filter(Food_Category %in% c('Crustaceans','Fish')) %>%
                        filter(!(Food_Category_sub %in% c('Shrimp','Prawn'))), # Getting non shrimp and prawn info - these are only capture fish with info for sub categories
                      tmp.lca.sample %>%
                        dplyr::select(-Average_of_original_category) %>%
                        dplyr::select(-Average_of_sub_category) %>%
                        filter(!(Food_Category_sub %in% c('Shrimp','Prawn'))) %>% # Limiting to everythin but shrimp and prawn
                        filter(aquaculture %in% 'No') %>% # Limiting to capture fish
                        dplyr::select(-Food_Category_sub) %>% dplyr::select(-Food_Category_sub_sub)) # And dropping sub category info for merging purposes
          
          
        } else {
          # Info for other capture
          tmp.food.other.fish <-
            left_join(tmp.food.df %>%
                        filter(product_name %in% i) %>%
                        filter(Food_Category %in% c('Crustaceans','Fish')) %>%
                        filter(!(Food_Category_sub %in% c('Shrimp','Prawn'))), # Getting non shrimp and prawn info - these are only capture fish with info for sub categories
                      tmp.lca.sample %>%
                        dplyr::select(-Average_of_original_category) %>%
                        dplyr::select(-Average_of_sub_category) %>%
                        filter(!(Food_Category_sub %in% c('Shrimp','Prawn'))) %>% # Limiting to everythin but shrimp and prawn
                        filter(aquaculture %in% 'No') %>% # Limiting to capture fish
                        dplyr::select(-Food_Category_sub) %>% dplyr::select(-Food_Category_sub_sub)) # And dropping sub category info for merging purposes
        }
        
        # And stacking data frames
        tmp.food.fish <- 
          rbind(tmp.food.other.fish,
                tmp.food.shrimp.prawn,
                tmp.food.other.aqua,
                tmp.food.trout)
      }
      
      # For non-fishy things
      # First a check
      # And making an empty data frame
      tmp.food.not.fish <- data.frame()
      # Checking for non-fish items
      if(sum(!grepl('Fish|Crustacean',tmp.food.df$Food_Category[tmp.food.df$product_name %in% i],ignore.case=TRUE)) > 0) {
        tmp.food.not.fish <- 
          left_join(tmp.food.df %>% 
                      filter(product_name %in% i) %>%
                      filter(!(Food_Category %in% c('Fish','Crustaceans'))), 
                    tmp.lca.sample %>% 
                      dplyr::select(-Average_of_original_category) %>%
                      dplyr::select(-Average_of_sub_category)) 
      }
      
      # And rbinding info for fish and not fish
      tmp.food <-
        rbind(tmp.food.fish,
              tmp.food.not.fish)
      
      # And checking to see if there are 1000 observations for each food
      # Merging this in later for testing purposes
      tmp.food.check <-
        tmp.food %>%
        dplyr::group_by(sample_index) %>%
        dplyr::summarise(count = n())
      
      
      # Multiplying impacts
      tmp.food[,paste0('tmp.impact_',1:length(indicators))] <-
        tmp.food[,'amount'] * tmp.food[,grepl('impact_[0-9]',names(tmp.food))]
      
      
      
      # Adjusting for coffee (e.g. is it brewed?)
      if(sum(tmp.food$brewed_coffee %in% 'Coffee') >= 1) {
        tmp.food[tmp.food$brewed_coffee %in% 'Coffee',paste0('tmp.impact_',1:length(indicators))] <-
          tmp.food[tmp.food$brewed_coffee %in% 'Coffee',paste0('tmp.impact_',1:length(indicators))] * (6/100) # average of 6 grams coffee beans per 100 ml brewed coffee
        
        tmp.food[tmp.food$brewed_coffee %in% 'Tea',paste0('tmp.impact_',1:length(indicators))] <-
          tmp.food[tmp.food$brewed_coffee %in% 'Tea',paste0('tmp.impact_',1:length(indicators))] * (2/250) # One cup of tea uses ~2 grams of leaves
      }
      
      
      # getting mean and sd and ci for each indicator
      # Creating template
      mean.se.ci.out <- data.frame(matrix(nrow = 1000, ncol = (3 + length(indicators))))
      # Names
      names(mean.se.ci.out) <- c('product_name','id','sample_index',paste0('tmp.impact_',1:length(indicators)))
      # And updating values
      mean.se.ci.out <-
        mean.se.ci.out %>%
        mutate(product_name = unique(tmp.food$product_name),
               id = unique(tmp.food$id),
               sample_index = 1:1000)
      
      # Looping through indicators
      # Would rather do this with dplyr::summarise or equivalent
      # By looping gives more flexibility to add or take out indicators as desired
      mean.se.ci.out[,grepl('tmp.impact_[0-9]',names(mean.se.ci.out))] <- 
        rowsum(tmp.food[,grepl('tmp.impact_[0-9]',names(tmp.food))],
       	       group = tmp.food$sample_index)

    # updating big data frame
      out.df <- rbind(out.df, mean.se.ci.out)
    }
   
    # Data frame to return to main file
    return.df <- out.df
    
    # and updating names again
    for(n in 1:length(indicators)) {
      names(return.df)[grepl(paste0("_",n), names(return.df))] <- 
        gsub(paste0("_",n),paste0("_",indicators[n]),names(return.df)[grepl(paste0("_",n), names(return.df))])
    }
    
    #return.df$check <- return.df$check_index_min != return.df$check_index_max
    
    return(return.df)
    
  }

# Conversion functions for e.g. soybeans to tofu
conversion.function <-
  function(indicators,
           percentiles) {
    # ratios for conversion
    ratios <- read.csv(paste0(getwd(),"/Data Inputs/LCA_Ratios_Import.csv"),
                       stringsAsFactors = FALSE) %>%
      mutate(Data.S2.Name = ifelse(Data.S2.Name %in% 'Dark Chocolate (70% cocoa)','Dark Chocolate',Data.S2.Name))# %>%
      # filter(!grepl('Cheese',Data.S2.Name))
    # data set to apend rows to
    converted.df <- data.frame()
    
    food.groups <- 
      sort(unique(ratios$Data.S2.Name))
    
    # Looping through food groups
    # for(i in c(4,10,11)) {
    for(i in 1:length(food.groups)) {
      # Temporary df for layout
      tmp.df <- 
        lca.dat[lca.dat$Data.S2.Name %in% ratios$Original_Food[ratios$Data.S2.Name %in% food.groups[i]],]
      
      if(nrow(tmp.df)>0) {
        # Updating food group name
        tmp.df$food.group <- food.groups[i]
        tmp.df$Data.S2.Name <- food.groups[i]
        
        # Dividing by ratios
        for(j in 1:length(indicators)) {
          tmp.ratio <- ratios[ratios$Data.S2.Name %in% food.groups[i],
                              names(ratios)[grep(indicators[j],names(ratios))]]
          
          tmp.df[,names(tmp.df)[grep(indicators[j], names(tmp.df))]] <-
            as.numeric(tmp.df[,names(tmp.df)[grep(indicators[j], names(tmp.df))]]) / tmp.ratio
        }
        
        # Exception for cheese
        if(grepl('Cheese',food.groups[i])) {
          if(grepl('Semi',food.groups[i])) {
            tmp.df <- tmp.df %>% filter(LCA_Category_sub %in% 'Medium Cheese')
          } else {
            tmp.df <- tmp.df %>% filter(LCA_Category_sub %in% Data.S2.Name)
          }
        }
        # Rbinding conversion data
        converted.df <- rbind(converted.df, tmp.df)
      }# End of if statement
    } # End of loop for foods
    
    return(converted.df)
  }


brewed.coffee.tea <-
  function(dat) {
    # Identifying brewed coffee
    tmp.coffee <-
      which(grepl('brew|latt|flat white|macchiato|cappu|cold press|affogato|
              mocha|au lait|frapp|iced|lungo|ristretto', dat$product_name, ignore.case = TRUE) & 
              dat$Food_Category %in% 'Coffee' & 
              !grepl('instant|packet|pod|capsule|tin|bag', dat$product_name, ignore.case=TRUE) |
              grepl('brew|latt|flat white|macchiato|cappu|cold press|affogato|
              mocha|au lait|frapp|iced|lungo|ristretto', dat$value, ignore.case = TRUE) & 
              dat$Food_Category %in% 'Coffee' & 
              !grepl('instant|packet|pod|capsule|tin|bag', dat$product_name, ignore.case=TRUE)) # Did not search ingredient list because this returned no matches
      
      # Identifying coffee
    tmp.tea <- 
      which(grepl('brew', dat$product_name, ignore.case = TRUE) & 
              dat$Food_Category %in% 'Tea' & 
              !grepl('bag|sachet|loose', dat$product_name, ignore.case=TRUE))
    # Unique list of entries
    tmp.coffee.tea <-
      unique(c(tmp.coffee,tmp.tea))
    
    return(tmp.coffee.tea)
  }


# Identifying organic ingredients
# Doing this based on ingredient name and product name
organic.ingredients <-
  function(dat) {
    # Based on ingredient name
    dat <-
      dat %>%
      mutate(Organic_ingredient = NA) %>% # creating column
      mutate(Organic_ingredient = ifelse(grepl('organic',value, ignore.case = TRUE), 'Organic','Not Organic')) %>% # search based on ingredient name
      mutate(Organic_ingredient = ifelse(grepl('organic',product_name,ignore.case=TRUE),'Organic',Organic_ingredient)) # Search based on product name
    
    # Returning data set
    return(dat)
  }

# Identify broth and stock
# Doing this based on ingredient name
broth.stock <-
  function(dat) {
    
    dat <-
      dat %>%
      mutate(broth_stock = NA) %>% # creating column
      mutate(broth_stock = # Identifying based on ingredient name
               ifelse(grepl('broth|stock|boullion|bouillon', value, ignore.case = TRUE), # Logic term
                      'Yes', # if logic matched, then ingredient is a stock
                      'No')) %>%
      mutate(percent = ifelse(broth_stock %in% 'Yes',
                              percent/10, # If ingredient is a broth, divide composition by 10. Don't have LCA info on stock/broth, so adjusting this manually
                              percent)) # Else leave as is
    
    # Am not identifying by product name, as this already has water, etc, included in the ingredients list
    # Removing column
    dat <- dat %>% dplyr::select(-broth_stock)
    # And returning data frame
    return(dat)
  }





# Data

fish.env.function <-
  function(yay) {
    # Setting seed for replicability
    set.seed(19)
    # Importing data
    # This is the data from Gephart et al 2021
    # Original file name = 41586_2021_3889_MOESM5_ESM.csv
    dat <- 
      # read.csv("/Users/macuser/Desktop/fish_env_info.csv") %>%
      read.csv(paste0(getwd(),'/Data Inputs/fish_env_info.csv')) %>%
      filter(production %in% 'capture') %>% # only capture fish
      filter(weight %in% 'edible') # impacts by edible weight
    
    # Randomly sampling within distribution
    # Assuming normally distributed data
    # Empty data frame
    out.df <- data.frame()   
    # Looping throug fish types
    for(i in 1:nrow(dat)) {
      tmp.df <-
        data.frame(fish = dat$taxa[i],
                   impact = rnorm(n = 100,
                                  mean = dat$median[i],
                                  sd = mean(c(dat$upper_95[i] - dat$median[i], dat$median[i] - dat$lower_95[i]))/ 1.96))
      
      out.df <-
        rbind(out.df,tmp.df)
    } # End of loop through fish types
    
    # Getting rid of negative observations
    # Need to do this because we're randomly sampling from within a normal distribution
    out.df <- 
      out.df %>% 
      filter(impact > 0) 
    
    # Categorizing fish into categories
    # to match with the JP data
    # And with FAO production data
    out.df <-
      rbind(out.df,
            out.df %>% filter(fish %in% 'Shrimps') %>% mutate(fish = 'Prawns')) %>% # Adding rows for prawns
      mutate(weight = 1) %>%
      mutate(LCA_Category = 
               ifelse(fish %in% c('Lobsters','Shrimps','Prawns'),
                      'Crustaceans (capture)','Fish (capture)'))
    
    # Translation between categories
    fish.translation <-
      read.csv(paste0(getwd(),'/Data Inputs/fish_matching.csv'))
      # read.csv("/Users/macuser/Desktop/fish_matching.csv")
    
    # Weighting based on global fish production
    fish.weights <-
      read.csv(paste0(getwd(),'/Data Inputs/fish_weighting.csv'))
      # read.csv("/Users/macuser/Desktop/fish_weighting.csv")
    
    # And matching and weighting
    out.df.matches <-
      left_join(out.df,fish.translation) %>% 
      mutate(LCA_Category_sub = ifelse(is.na(Gephart.Match),NA,Gephart.Match)) %>%
      unique(.)
    
    # Updating matches with fao data
    out.df.matches <-
      out.df.matches %>%
      mutate(FAO.Match = 
               ifelse(fish %in% c('Cephalopods'),'Cephalopods',
                      ifelse(fish %in% 'Bivalves','Scallop',
                             ifelse(fish %in% 'Lobsters','Crustaceans',
                                    ifelse(fish %in% c('Shrimps','Prawns'),'Shrimp',FAO.Match))))) %>%
      unique() %>%
      filter(!(fish %in% c('Shrimps','Prawns','Lobsters') & Gephart.Match != ''))
    
    # Summing fish weights by type
    fish.weights.merge <-
      fish.weights %>%
      dplyr::rename(fish = Gephart.Category) %>%
      mutate(LCA_Category_sub = ifelse(grepl('other',LCA_Category_sub,ignore.case=TRUE),
                                       '',LCA_Category_sub)) %>%
      mutate(LCA_Category = paste0(LCA_Category, ' (capture)')) %>%
      dplyr::group_by(fish,LCA_Category,LCA_Category_sub) %>%
      dplyr::summarise(Weight = sum(Weighting))
    
    
    
    # And weighting based on global fish
    out.df.weights <-
      left_join(out.df.matches,
                fish.weights.merge) %>%
      unique(.) %>%
      mutate(Weight = ifelse(LCA_Category %in% 'Fish (capture)' & is.na(Weight),
                             sum(fish.weights.merge$Weight[fish.weights.merge$LCA_Category %in% 'Fish (capture)' &
                                                             fish.weights.merge$fish %in% '']), 
                             Weight)) %>%
      mutate(Weight = ifelse(LCA_Category %in% 'Crustaceans (capture)' & is.na(Weight),
                             sum(fish.weights.merge$Weight[fish.weights.merge$LCA_Category %in% 'Crustaceans (capture)' &
                                                             fish.weights.merge$fish %in% '']), 
                             Weight)) %>%
      mutate(LCA_Category_sub = ifelse(fish %in% c('Shrimps'),'Shrimp',
                                       ifelse(fish %in% 'Prawns','Prawn',LCA_Category_sub)))
    
    # Adjusting units
    # Raw data in kg CO2e/tonne, need kg CO2e/100g
    out.df.weights <-
      out.df.weights %>%
      mutate(impact = impact / 1000)
    
    # And adjusting formatting to match the lca data
    out.df.weights <- 
      out.df.weights %>%
      dplyr::select(Food_Category = LCA_Category, Food_Category_sub = LCA_Category_sub,
                    GHG = impact, Weight)
    
    # Adding columns
    cols.need <- names(lca.dat)
    for(i in cols.need) {
      if(i %in% names(out.df.weights)) {
        # Do nothing
      } else {
        out.df.weights[,i] <- 0
      }
    }
    
    # Ordering columns
    out.df.weights <- out.df.weights[,names(lca.dat)]
    
    # And updating system and category columns
    out.df.weights <- 
      out.df.weights %>%
      mutate(Food_Category_sub_sub = '',
             Sys = 'C')
    
    # Returning data frame
    return(out.df.weights)
  }



# rsion functions for e.g. soybeans to tofu
conversion.function <-
  function(indicators,
           percentiles) {
    # ratios for conversion
    ratios <- read.csv(paste0(getwd(),"/Data Inputs/LCA_Ratios_Import.csv"),
                       stringsAsFactors = FALSE) %>%
      mutate(Data.S2.Name = ifelse(Data.S2.Name %in% 'Dark Chocolate (70% cocoa)','Dark Chocolate',Data.S2.Name))# %>%
    # filter(!grepl('Cheese',Data.S2.Name))
    # data set to apend rows to
    converted.df <- data.frame()

    food.groups <-
      sort(unique(ratios$Data.S2.Name))

    # Looping through food groups
    # for(i in c(4,10,11)) {
    for(i in 1:length(food.groups)) {
      # Temporary df for layout
      tmp.df <-
        lca.dat[lca.dat$Data.S2.Name %in% ratios$Original_Food[ratios$Data.S2.Name %in% food.groups[i]],]

      if(nrow(tmp.df)>0) {
        # Updating food group name
        tmp.df$food.group <- food.groups[i]
        tmp.df$Data.S2.Name <- food.groups[i]

        # Dividing by ratios
        for(j in 1:length(indicators)) {
          tmp.ratio <- ratios[ratios$Data.S2.Name %in% food.groups[i],
                              names(ratios)[grep(indicators[j],names(ratios))]]

          tmp.df[,names(tmp.df)[grep(indicators[j], names(tmp.df))]] <-
            as.numeric(tmp.df[,names(tmp.df)[grep(indicators[j], names(tmp.df))]]) / tmp.ratio
        }

        # Exception for cheese
        if(grepl('Cheese',food.groups[i])) {
          if(grepl('Semi',food.groups[i])) {
            tmp.df <- tmp.df %>% filter(LCA_Category_sub %in% 'Medium Cheese')
          } else {
            tmp.df <- tmp.df %>% filter(LCA_Category_sub %in% Data.S2.Name)
          }
        }
        # Rbinding conversion data
        converted.df <- rbind(converted.df, tmp.df)
      }# End of if statement
    } # End of loop for foods

    return(converted.df)
  }
