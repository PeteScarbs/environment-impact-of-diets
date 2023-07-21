# environment-impact-of-diets
This repository contains the code used for analysis in the paper Scarborough P, Clark M, Cobiac LJ, Papier K, Knuppel A, Lynch J, Harrington RA, Key T, Springmann M. Vegans, vegetarians, fish-eaters and meat-eaters in the UK show discrepant environmental impacts. Nature Food, 2023.

The NATFOOD GitHub Code.R script is for an analysis that combines data from 1000 iterations of a Monte Carlo analysis. Each iteration produces an estimate of the greenhouse gas emissions, land use, water use, eutrophication and biodiversity impact of six diet groups in the UK. The six diet groups are: vegans, vegetarians, fish-eaters, low meat eaters (<50g/d); medium meat eaters (50-100g/d); high meat eaters (>100g/d).

Each iteration combines data on the environmental impact of diets drawn from a review of 570 life cycle analyses covering results from over 38,000 farms in 119 countries covering five continents. Combining the iterations provides an estimate of the average impact for each diet group, and an uncertainty range representing variation due to different methods of food production and sourcing.

The datasets to support this code are openly available at the following URLs: http://dx.doi.org/10.5287/ora-5zebayaog and https://ora.ox.ac.uk/objects/uuid:fd3bfe80-1b8d-4d68-b471-a5621def5c24

The other scripts that are available are described below, and are the scripts that are used to generate the 1000 iterations used by the NATFOOD GitHub Code.R script.


**Description of Analysis: 
**
This analysis investigates the environmental impacts and variation in them of vegans, vegetarians, fish-eaters, and meat-eaters in the UK.  

This was done by using food survey data derived from the EPIC-Oxford cohort study, and pairing this food survey data with information on the environmental impacts of food products at retail stores (from foodDB) and agricultural commodities (from Poore and Nemecek 2018). This pairing was completed by assigning food products from foodDB and Poore and Nemecek (2018) into each of the >300 EPIC food codes. 

To investigate variation in the environmental impacts of a given individual’s diet, we conducted a monte carlo analysis based on uncertainty in the environmental impact data on food products and commodities. In this monte carlo analysis, we derived 1,000 possible estimates of the environmental impacts of each EPIC food code, based on the potential variation in sourcing of each food product and agricultural commodity that was paired into each EPIC food code.  

Across the 1,000 monte carlo iterations, each individual data point from the monte carlo analysis randomly selects farm-level environmental impact information, such that each iteration is indicative of what the food code’s impact could be if the food was sourced from different locations and production systems. When sampling for the monte carlo analysis, production systems were randomly selected based on their representativeness of the global food system. Therefore, the 1,000 iterations illustrate the potential variation and distribution in environmental impacts of each EPIC food code, whereas the median impact estimate is approximately equivalent to the environmental impacts of each EPIC food code assuming ‘global average sourcing’.  

**Description of Scripts: 
**
The names of the scripts used in the analysis, and a brief description of them, are below: 

0.0_Functions_Estimating_Impacts_18Feb2022.R 
This contains functions used in the other scripts. 

1.0_Monte_Carlo_Estimates_11Mar2022.R 

1.1_Monte_Carlo_Estimates_Commodities_11Mar2022.R 
These scripts conduct a monte carlo analysis to assess the potential variation in environmental impacts of food products (1.0) and agricultural commodities (1.1) paired with each of the >300 EPIC food codes.  

The output from these scripts is a series of csv files (1.0), or a single csv file (1.1) containg results from each of the monte carlo iterations. 

2.0_Managing_Food_Code_Data_11March2022.R 
This script aggregates the results from the monte carlo iteration into a single, larger csv file that is used as an input into the 3.0 script. 

3.0_Calculating_Results_11March2022.R 
This script calculates the environmental impacts of each participant in each monte carlo analysis. In short, it takes the output from the 2.0 script, and pairs it with food consumption information for each EPIC participant, to derive 1,000 estimates of the environmental of each EPIC participant. 

4.0_Aggergating_Results_11March2022_NoKcalAdjustment.R 

4.0_Aggergating_Results_11March2022.R 
These scripts aggregate results from the 3.0 script, to derive average results by age, sex, and diet category for each monte carlo analysis. The output from these scripts are therefore 1,000 data points for each age, sex, and diet category, where each individual data points indicates the environmental impacts for that age * sex * diet category in that monte carlo analysis. 

This is conducted in two ways, first by assuming there is no kcal adjustment to a standardised consumption of 2000 kcal per person per day, and the second assuming there is a standardised consumption fo 2000 kcal per person per day. 

**Running the Scripts: 
**
The scripts should be run in increasing numeric order, such that script 1.0 is run first, then script ‘1.1’, then etc. The scripts are designed to be run linearly, whereby outputs created in one script are used as inputs into the script. 

The 0.0 script contains functions called in the other scripts, and as such does not need to be run. 

**Data Availability: 
**
Anonymised and aggregated data inputs outputs that cannot be linked back to individual EPIC participants or to individual food products are available here: https://ora.ox.ac.uk/objects/uuid:fd3bfe80-1b8d-4d68-b471-a5621def5c24. Other sensitive data inputs (listed below) are available on request (see section on ‘Requesting Other Data Inputs’). 


**Non Available Data: 
**
Some data is not provided because it is potentially sensitive, for instance personal information that breaches GDPR regulations, or that is commercially sensitive. This data is as follows: 

Raw information on food products available at UK food retail stores (from foodDB). 

Estimated ingredient composition of food products available at UK food retail stores (from foodDB). 

Estimated environmental impacts of food products available at UK food retail stores (from foodDB). 

Recorded dietary intake for individuals (from EPIC Oxford). 


**Requesting Other Data Inputs: 
**
Some of the data used in this analysis is protected by GDPR (General Data Protection Regulation) and commercial copyright limitations. This means we are not able to freely provide all data used in the analysis. 

The data we cannot freely share is:  

1. Diet consumption information of participants in the EPIC Oxford cohort study. 

2. Environmental impact information on specific and non-anonymised food products. 


To request the above data, please email peter.scarborough@phc.ox.ac.uk 
