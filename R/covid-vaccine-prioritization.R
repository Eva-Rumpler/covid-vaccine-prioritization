#########################################
###        Vaccine prioritization     ###
###            Race/Age/Job           ###
###             Eva Rumpler           ###
###            December 2022          ###
#########################################

######################
### Load libraries ###
######################

library(reshape2)
library(tidyverse)
library(readr)
library(readxl)
library(tidycensus)
library(knitr)
library(viridis)
library(RColorBrewer)

#####################################################################
###                            DEATH DATA                         ###
### Extract COVID death data by race, age categories and US state ###
###           that did *not* take place in NH or LTCF.            ### 
#####################################################################

### Load CDC COVID deaths dataset
# From CDC website : https://data.cdc.gov/NCHS/Deaths-involving-coronavirus-disease-2019-COVID-19/ks3g-spdg
# Data from : January 1st 2020
# Data up to : January 30th 2021
Deaths_CDC_raw <- read_csv("data/Deaths_involving_coronavirus_disease_2019__COVID-19.csv")
Deaths_CDC <- Deaths_CDC_raw %>%
   rename(Age = 'Age group',
          Race = 'Race and Hispanic Origin Group',
          COVIDdeaths = 'COVID-19 Deaths') %>%
   select(State, Age, Race, COVIDdeaths) %>% 
   filter(Age == "15-24 years"| Age == "25-34 years"| Age == "35-44 years"|
          Age == "45-54 years" | Age == "55-64 years"| Age == "65-74 years", # Restrict to age groups of interest 15-64 and 65-74.
          State != "United States" & State != "Puerto Rico", # 51 states common to all three datasets (Number deaths, Place death and ACS PUMS data)
          Race  != "Unknown") # No "Unknown" in ACS.
Deaths_CDC <- Deaths_CDC[with(Deaths_CDC, order(State, Race, Age)),]
Deaths_CDC
nrow(Deaths_CDC)

## NY City and NY State deaths are coded separately and need to be joined into one "New York" category. 
unique(Deaths_CDC$State) # 52 States, need to remove "New York City"
#Deaths_CDC[Deaths_CDC$State == "New York",]; Deaths_CDC[Deaths_CDC$State == "New York City",]
Deaths_CDC_NY <- Deaths_CDC %>%
   filter(State == "New York City" | State == "New York") %>%
   group_by(Age, Race) %>%
   mutate(COVIDdeaths = ifelse(is.na(COVIDdeaths), 0, COVIDdeaths),
          COVIDdeaths = sum(COVIDdeaths)) %>% 
   filter(State == "New York") %>%
   mutate(COVIDdeaths = ifelse(COVIDdeaths == 0, NA, COVIDdeaths))
Deaths_CDC[Deaths_CDC$State == "New York",] <- Deaths_CDC_NY
Deaths_CDC <- Deaths_CDC %>%
   filter(State != "New York City")
Deaths_CDC
nrow(Deaths_CDC)
unique(Deaths_CDC$State) # 51 states ok ! 

## Missing deaths data
# Empty data cells represent counts between 1-9 that have been suppressed in accordance with NCHS confidentiality standards. Source : https://www.cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm
sort(unique(Deaths_CDC$COVIDdeaths))
# Replace those missing death data (NAs) by 5, which is the median of 1:9.
Deaths_CDC1A <- Deaths_CDC
Deaths_CDC1A$COVIDdeaths <- replace_na(Deaths_CDC1A$COVIDdeaths, 5) 
Deaths_CDC1A <- Deaths_CDC1A[with(Deaths_CDC1A, order(State, Race, Age)),]
Deaths_CDC1A

## For each State, add the total number of deaths across all races in each age cat
df1A <- Deaths_CDC1A %>%
   group_by(State, Age) %>%
   summarise("COVIDdeaths" = sum(COVIDdeaths))
df1A <- cbind(df1A, rep("All_races", nrow(df1A)))
colnames(df1A) <- c("State", "Age", "COVIDdeaths", "Race")
df1A
Deaths_CDC1A <- rbind(as.data.frame(Deaths_CDC1A), as.data.frame(df1A))
Deaths_CDC1A <- Deaths_CDC1A[with(Deaths_CDC1A, order(State, Race, Age)),]
Deaths_CDC1A

### Subtract the deaths that took place in nursing-homes (NH) and long-term care facilities (LTCF)
# We have the number of COVID death data by race, age categories and state.
# We want to subtract the deaths that took place in nursing-homes (NH) and long-term care facilities (LTCF), for each age group, race and State.
# Dataset from CDC Website : https://data.cdc.gov/dataset/NVSS-Provisional-COVID-19-Deaths-by-Place-of-Death/4va6-ph5s
# Data from: January 4th 2020 
# Data up to : January 30th 2021
# Dates correspond to the CDC deaths data above. Only difference in 3 days in January 2020.
# This dataset gives State, Age and Location of COVID deaths, but *not* race.
# We have to assume that proportion of COVID deaths in NH and LTCF are constant across races for each State and age category.
# Place of death comes from the death certificate and is determined by where the death was pronounced and on the physical location where the death occurred. Source : https://data.cdc.gov/NCHS/NVSS-Provisional-COVID-19-Deaths-by-Place-of-Death/4va6-ph5s
PlaceDeaths <- as.data.frame(read_csv("data/NVSS_Provisional_COVID-19_Deaths_by_Place_of_Death_and_Age.csv"))
PlaceDeaths <- PlaceDeaths %>%
   rename(Age = 'Age group',
          COVIDdeaths = 'COVID-19 Deaths',
          Place_death = 'Place of Death') %>%
   select(Age, State, COVIDdeaths, Place_death) %>%
   filter(Age == "15-44 years"| Age == "45-64 years" | Age == "65-74 years" &
             Place_death != "All places of death" &   # "All places of death" data only available for all the "All ages" category.
             State != "Region total",
             State != "United States" & State != "Puerto Rico") # 51 States common to all three datasets (Number deaths, Place death and ACS PUMS data)
PlaceDeaths <- PlaceDeaths[with(PlaceDeaths, order(State, Place_death, Age )),]
PlaceDeaths

## NY City and NY State deaths are coded separately and need to be joined into one "New York" category. 
unique(PlaceDeaths$State) # 52 States, need to remove "New York City"
PlaceDeaths[PlaceDeaths$State == "New York",]
PlaceDeaths[PlaceDeaths$State == "New York City",]
PlaceDeaths_NY <- PlaceDeaths %>%
   filter(State == "New York City" | State == "New York") %>%
   group_by(Age, Place_death) %>%
   mutate(COVIDdeaths = ifelse(is.na(COVIDdeaths), 0, COVIDdeaths),
          COVIDdeaths = sum(COVIDdeaths)) %>% 
   filter(State == "New York") %>%
   mutate(COVIDdeaths = ifelse(COVIDdeaths == 0, NA, COVIDdeaths))
PlaceDeaths_NY
PlaceDeaths[PlaceDeaths$State == "New York",] <- PlaceDeaths_NY
PlaceDeaths <- PlaceDeaths %>%
   filter(State != "New York City")
PlaceDeaths
nrow(PlaceDeaths)
unique(PlaceDeaths$State)
unique(PlaceDeaths$Place_death)
sum(PlaceDeaths$COVIDdeaths[PlaceDeaths$Place_death == "Nursing home/longterm care facility"], na.rm = T) / sum(PlaceDeaths$COVIDdeaths, na.rm = T) *100
# 7.65% of COVID-19 deaths took place in NH and LTCF (before imputation)

## Missing place deaths data
# Empty data cells represent counts between 1-9 that have been suppressed in accordance with NCHS confidentiality standards. Source : https://www.cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm
sort(unique(PlaceDeaths$COVIDdeaths))

# Replace those missing death data (NAs) by 5 (median of 1:9)
PlaceDeaths1A <- PlaceDeaths
PlaceDeaths1A$COVIDdeaths <- replace_na(PlaceDeaths1A$COVIDdeaths, 5)
PlaceDeaths1A

##  For each State and Age group, compute # deaths in NH/LTCF and # outside of them (or total number) to compute proportion of deaths in LTCF/NH.
PlaceDeaths1A <- PlaceDeaths1A %>%
   group_by(State, Age) %>%
   summarise(deathsNH =  sum(COVIDdeaths[Place_death == "Nursing home/longterm care facility"]),
             deathstot = sum(COVIDdeaths)) %>%
   mutate(propNH = deathsNH/deathstot,
          propoutsideNH = 1 - propNH)
PlaceDeaths1A$propNH <- replace_na(PlaceDeaths1A$propNH, 0)
PlaceDeaths1A$propoutsideNH <- replace_na(PlaceDeaths1A$propoutsideNH, 0)
PlaceDeaths1A
# Percentage of deaths that took place in NH/LTCF
sum(PlaceDeaths1A$deathsNH) / sum(PlaceDeaths1A$deathstot)

### Combine two datasets (PlaceDeaths1A and Deaths_CDC1A) to subtract a proportion of deaths in each state and age cat that took place in NH and LTCF.
# PlaceDeaths1A and Deaths_CDC1A don't have the same number of age categories, so copy rows to have same age coding.
# unique(PlaceDeaths1A$Age); unique(Deaths_CDC1A$Age)
PlaceDeaths2A <- PlaceDeaths1A[rep(seq_len(nrow(PlaceDeaths1A)), rep(c(3, 2, 1), length(unique(PlaceDeaths1A$State)))), ]
head(PlaceDeaths2A, 20)
head(Deaths_CDC1A, 20)
# Now repeat each 6 lines of PlaceDeaths2A 8 times (for each race in Deaths_CDC1A)
PlaceDeaths3A <- do.call("rbind", replicate(length(unique(Deaths_CDC1A$Race)), PlaceDeaths2A, simplify = FALSE))
PlaceDeaths3A <- PlaceDeaths3A[order(PlaceDeaths3A$State),]
PlaceDeaths3A
# Cbind propNH from PlaceDeaths3 to Deaths_CDC1A and multiply two columns to get the deaths that did not happen in NH/LTCF by state and age cat
Deaths_CDC2A <- cbind(Deaths_CDC1A, PlaceDeaths3A[,6])
Deaths_CDC2A <- Deaths_CDC2A %>%
   mutate(COVIDdeaths_outNH = COVIDdeaths * propoutsideNH) #(1-propNH))
Deaths_CDC2A

# Now transform to wide format with 6 age groups of interest
Deaths_CDC3A <- dcast(Deaths_CDC2A, State + Race ~ Age, value.var = "COVIDdeaths_outNH")
head(Deaths_CDC3A)

# Rename Race variable to match other datasets : 
Deaths_CDC3A$Race[Deaths_CDC3A$Race == "Non-Hispanic White"] <- "NH_White"
Deaths_CDC3A$Race[Deaths_CDC3A$Race == "Non-Hispanic Black"] <- "NH_Black"
Deaths_CDC3A$Race[Deaths_CDC3A$Race == "Non-Hispanic Asian"] <- "NH_Asian"
Deaths_CDC3A$Race[Deaths_CDC3A$Race == "Non-Hispanic American Indian or Alaska Native" ] <- "NH_Americanindian"
Deaths_CDC3A$Race[Deaths_CDC3A$Race == "Non-Hispanic More than one race" ] <- "NH_morethanonerace"
Deaths_CDC3A$Race[Deaths_CDC3A$Race == "Non-Hispanic Native Hawaiian or Other Pacific Islander"] <- "NH_Hawaiian"
unique(Deaths_CDC3A$Race)
head(Deaths_CDC3A)

# 1) Rename variable to indicate that those are deaths numbers in each age group, and 
# 2) Create deaths 16-24 instead of 15-24 by assuming 10% of deaths among 15-24yo took place in 15 year olds.
# 3) Assume that deaths remaining represent 20% of the deaths that took place up to January 2021.
Deaths_CDC5A <- Deaths_CDC3A %>%
   rename(deaths_1524 = "15-24 years",
          deaths_2534 = "25-34 years",
          deaths_3544 = "35-44 years",
          deaths_4554 = "45-54 years",
          deaths_5564 = "55-64 years",
          deaths_6574 = "65-74 years")  %>%
   mutate(deaths_1624 = 0.9 * deaths_1524,
          deaths_1624 = 0.2 * deaths_1624,
          deaths_2534 = 0.2 * deaths_2534,
          deaths_3544 = 0.2 * deaths_3544,
          deaths_4554 = 0.2 * deaths_4554,
          deaths_5564 = 0.2 * deaths_5564,
          deaths_6574 = 0.2 * deaths_6574,
          deaths_1664 = deaths_1624 + deaths_2534 + deaths_3544 + deaths_4554 + deaths_5564) %>%
   select(-deaths_1524)
# Order by State and Race
Deaths_CDC5A <- Deaths_CDC5A[with(Deaths_CDC5A, order(State, Race)),]
# Order columns
Deaths_CDC5A <- Deaths_CDC5A[, c("State", "Race", "deaths_1664", "deaths_1624", "deaths_2534", "deaths_3544", "deaths_4554", "deaths_5564", "deaths_6574")]
colnames(Deaths_CDC5A)
Deaths_CDC5A
head(Deaths_CDC5A, 30)
sort(unique(Deaths_CDC5A$Race)) # 8 races
nrow(Deaths_CDC5A) # 408 rows

#####################################################################
###                   EXPECTATIONS OF LIFE DATA                   ###
###                 Expectations of life at age x                 ###
###                  WHO website, 2019 data, USA                  ### 
#####################################################################

### Load data
# From Global Health Observatory data repository : https://apps.who.int/gho/data/view.main.61780?lang=en
LifeExpectationsUSA19 <- read_excel("data/WHO-USA-2019-ExpectationsLife.xlsx")
LifeExpectationsUSA19 <- LifeExpectationsUSA19 %>%
   rename(Age = 'Age Group',
          Expectationoflife = 'Both sexes') %>%
   filter(Age %in% c("15-19  years", "20-24 years", "25-29 years", "30-34 years", "35-39 years",
                     "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                     "65-69 years", "70-74 years")) %>%
   mutate(Agecat = rep(c("1524", "2534", "3544", "4554", "5564", "6574"), each = 2)) %>%
   transform(Expectationoflife = as.numeric(Expectationoflife)) %>%
   select(-c(Indicator))
LifeExpectationsUSA19

# I average over two age categories to get 10-year age bin
LifeExpectationsUSA19_2 <- LifeExpectationsUSA19 %>%
   group_by(Agecat) %>%
   summarize(Expectationoflife2 = mean(Expectationoflife)) %>%
   ungroup()
LifeExpectationsUSA19_2

# I don't think it makes much sense to compute SLE for 15-64. 
LifeExpectationsUSA19_3 <- rbind( c("1564", as.numeric(lapply((as.list(LifeExpectationsUSA19_2[(1:5),2])),mean))),
                                  LifeExpectationsUSA19_2)
LifeExpectationsUSA19_3 <- LifeExpectationsUSA19_3 %>%
   transform(Expectationoflife2 = as.numeric(Expectationoflife2))
LifeExpectationsUSA19_3

#saveRDS(LifeExpectationsUSA19_3, "data/LifeExpectationsUSA19_3.rds")


# Generate Supplementary Table showing SLE.
LifeExpectationsUSA19_2 %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   kable(. ,"latex")

#head(Deaths_CDC5A)
Deaths_CDC6A <- Deaths_CDC5A %>%
   mutate(YLL_1664 = deaths_1664 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 1564],
          YLL_1624 = deaths_1624 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 1524],
          YLL_2534 = deaths_2534 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 2534],
          YLL_3544 = deaths_3544 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 3544],
          YLL_4554 = deaths_4554 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 4554],
          YLL_5564 = deaths_5564 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 5564],
          YLL_6574 = deaths_6574 * LifeExpectationsUSA19_3$Expectationoflife2[LifeExpectationsUSA19_3$Agecat == 6574])
head(Deaths_CDC6A)
Deaths_CDC6A

#################################################################################################
###                            ALL ADULTS and FRONT LINE WORKERS DATA                         ###
### Extract number of individuals (regardless of occupation) and number of front-line workers ###
###                             For each State, Race and Age category                         ###
###                            Using the American Community Survey (ACS)                      ###
###                                    2019 1-year estimates                                  ###
#################################################################################################

### Using tidycensus package to access PUMS (raw microdata)
# Data from 2019 1-year ACS
# Tidycensus package: https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
# Usefull documentation: https://walker-data.com/tidycensus/articles/pums-data.html
# 2019 PUMS data dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019.pdf

## Define names of US States we're interested in (51 states common to all 3 datasets) 
data(fips_codes)
fips_codes2 <- fips_codes %>%
  select (- county_code, -county) %>%
  unique()
fips_codes2 <- fips_codes2[(fips_codes2$state_name != "American Samoa" &
                              fips_codes2$state_name != "Guam" &
                              fips_codes2$state_name != "Northern Mariana Islands" &
                              fips_codes2$state_name != "U.S. Minor Outlying Islands" &
                              fips_codes2$state_name != "U.S. Virgin Islands" &
                              fips_codes2$state_name != "Puerto Rico") ,]
nrow(fips_codes2)
c(fips_codes2$state)
c(fips_codes2$state_name) 

## Extract data from ACS
# Load api key needed to access ACS data using the tidycensus package.
#census_api_key("", install = TRUE) # Insert your key
readRenviron("~/.Renviron") #Reload environment to use the key without having to restart R
Sys.getenv("CENSUS_API_KEY") #Check key

# pums_agerace2019 <- get_pums(survey = "acs1",
#                              year = 2019,
#                              state = c(fips_codes2$state),
#                              variables = c("ST", "RAC1P", "HISP", "AGEP", "OCCP"),
#                              recode = TRUE)
pums_agerace2019 <- readRDS("data/pums_agerace2019.rds")
# "OCCP" and "SOCP" are two variables for detailed occupation
# "RAC1P" for race and "HISP" for Hispanic origin
# "AGEP" gives continuous age

## Restrict to 16-74 year olds
# In the ACS, occupation is only reported for individuals 16+, so restrict to individuals above 16 years old.
pums_agerace4 <- pums_agerace2019 %>%
   filter(AGEP %in%  c(16:74)) 
pums_agerace4

### Recode State and Race
# Recode race variable to join the information from the RAC1P and HISP variables
RACEcat <- c(rep(NA, nrow(pums_agerace4)))
pums_agerace4 <- cbind(pums_agerace4, RACEcat) #pums_agerace4 <- data.frame(pums_agerace4, RACEcat)
pums_agerace4$RACEcat <- NA
pums_agerace4$RACEcat[pums_agerace4$HISP != "01"] <- "Hispanic" 
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & pums_agerace4$RAC1P == "1"] <- "NH_White"
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & pums_agerace4$RAC1P == "2"] <- "NH_Black"
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & (pums_agerace4$RAC1P %in% c("3", "4","5"))] <- "NH_Americanindian" # Also includes Alaskans ! 
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & pums_agerace4$RAC1P == "6"] <- "NH_Asian"
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & pums_agerace4$RAC1P == "7"] <- "NH_Hawaiian"
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & pums_agerace4$RAC1P == "8"] <- "NH_Other"
pums_agerace4$RACEcat[pums_agerace4$HISP == "01" & pums_agerace4$RAC1P == "9"] <- "NH_morethanonerace"
sum(is.na(pums_agerace4$RACEcat))
unique(pums_agerace4$RACEcat) 

# Restrict analysis to 7 race categories (+all races) that are in other datasets
pums_agerace4 <- pums_agerace4 %>%
   subset(pums_agerace4$RACEcat != "NH_Other")
sort(unique(pums_agerace4$RACEcat))

# Recode State var to be just State name consistent w deaths data
pums_agerace4$ST_label <- pums_agerace4$ST_label %>%
   gsub('.{3}$', '', .)
unique(pums_agerace4$ST_label)

# Rename variable
pums_agerace4 <- pums_agerace4 %>% rename("State" = "ST_label",
                                          "Race" = "RACEcat")
pums_agerace4

###  Extract data of interest :
# Part 1) 
# Get N(S) (ijk): number of 65-74 year olds all occupations by Race and State.
# Get N    (ijk): number of individuals all occupations by Race and State in each of the five age categories between 16 and 64 yo.
(All_AgeRace2 <- pums_agerace4 %>%
      group_by(State, Race) %>%
      summarize(all_1674 = sum(PWGTP[AGEP >= 16 & AGEP <= 74]),
                all_1664 = sum(PWGTP[AGEP >= 16 & AGEP <= 64]),
                all_1624 = sum(PWGTP[AGEP >= 16 & AGEP <= 24]), 
                all_2534 = sum(PWGTP[AGEP >= 25 & AGEP <= 34]),
                all_3544 = sum(PWGTP[AGEP >= 35 & AGEP <= 44]),
                all_4554 = sum(PWGTP[AGEP >= 45 & AGEP <= 54]),
                all_5564 = sum(PWGTP[AGEP >= 55 & AGEP <= 64]),
                all_6574 = sum(PWGTP[AGEP >= 65 & AGEP <= 74]) ))
# Check missing rows # nrow(All_AgeRace2) == length(unique(All_AgeRace2$State)) * length(unique(All_AgeRace2$Race))

# For each State, compute the total number of individuals across all races and in All_AgeRace2 dataframe.
unique(All_AgeRace2$Race)
df7 <- All_AgeRace2 %>%
   group_by(State) %>%
   summarise("all_1674" = sum(all_1674),
             "all_6574" = sum(all_6574),
             "all_1664" = sum(all_1664),
             "all_1624" = sum(all_1624),
             "all_2534" = sum(all_2534),
             "all_3544" = sum(all_3544),
             "all_4554" = sum(all_4554),
             "all_5564" = sum(all_5564) )
df7$Race <- "All_races" 
df7
All_AgeRace3 <- rbind(as.data.frame(All_AgeRace2), df7)
All_AgeRace3 <- All_AgeRace3[with(All_AgeRace3, order(State, Race)),] #All_AgeRace3[order(All_AgeRace3$State),]
#
All_AgeRace3
nrow(All_AgeRace3)
head(All_AgeRace3, 20)
#

# Part 2) 
# Get N(F) (ijk): number of flw by Race and State and each of my 5 age categories.
# Filter observations with occupation corresponding to flw
# Select occupations that match the MA 'other workers' (flw) definition, details can be found in the 2018 data dictionnary.
OCCPflw_var <- c("2300", "2310", "2320", "2330",
                 "4000", "4010", "4020", "4030", "4040", "4055", "4110", "4120", "4130", "4140", "4150", "4160",
                 "4461", "4465",
                 "4700", "4720", "4740", "4750", "4760", "4950",
                 "7810",
                 "9030", "9040", "9050",
                 "9110", "9121", "9122", "9141", "9142" ) 
#OCCPflw_var
pums_agerace_flw <- pums_agerace4 %>%
   filter(OCCP %in%  OCCPflw_var) 
unique(pums_agerace_flw$OCCP_label)
pums_agerace_flw

# Extract number of flw by State, Age categories and Race
(Flw_AgeRace <- pums_agerace_flw %>%
   group_by(State, Race) %>%
   summarize(flw_1664 = sum(PWGTP[AGEP >= 16 & AGEP <= 64]),
             flw_1624 = sum(PWGTP[AGEP >= 16 & AGEP <= 24]),
             flw_2534 = sum(PWGTP[AGEP >= 25 & AGEP <= 34]),
             flw_3544 = sum(PWGTP[AGEP >= 35 & AGEP <= 44]),
             flw_4554 = sum(PWGTP[AGEP >= 45 & AGEP <= 54]),
             flw_5564 = sum(PWGTP[AGEP >= 55 & AGEP <= 64]) ))
# Some missing rows: # nrow(Flw_AgeRace) == length(unique(Flw_AgeRace$State)) * length(unique(Flw_AgeRace$Race))
# Some state and race combinations have missing values for flw count.
# unique(Flw_AgeRace$Race)
nrow(All_AgeRace2) - nrow(Flw_AgeRace) # 12 missing
nrow(Flw_AgeRace[Flw_AgeRace$Race == "NH_Hawaiian",]) # All 12 missing are Hawaiian
nrow(Flw_AgeRace[Flw_AgeRace$Race == "Hispanic",])
nrow(Flw_AgeRace[Flw_AgeRace$Race == "NH_White",])
nrow(Flw_AgeRace[Flw_AgeRace$Race == "NH_Americanindian",])
nrow(Flw_AgeRace[Flw_AgeRace$Race == "NH_Asian",])
nrow(Flw_AgeRace[Flw_AgeRace$Race == "NH_Black",])
nrow(Flw_AgeRace[Flw_AgeRace$Race == "NH_morethanonerace",])

# 12 States with missing flw Hawaiians :
# Flw_AgeRace %>% filter(Race == "Hispanic" | Race == "NH_Hawaiian") %>% View()
# Add missing rows by creating empty counts : 
Flw_AgeRace <- Flw_AgeRace%>%
   ungroup() %>%
   add_row("State" = "Delaware", "Race" = "NH_Hawaiian", .before = 54) %>%
   add_row("State" = "District of Columbia", "Race" = "NH_Hawaiian", .before = 61) %>%
   add_row("State" = "Indiana", "Race" = "NH_Hawaiian", .before = 103) %>%
   add_row("State" = "Iowa", "Race" = "NH_Hawaiian", .before = 110) %>%
   add_row("State" = "Maryland", "Race" = "NH_Hawaiian", .before = 145) %>%
   add_row("State" = "Michigan", "Race" = "NH_Hawaiian", .before = 159) %>%
   add_row("State" = "Montana", "Race" = "NH_Hawaiian", .before = 187) %>%
   add_row("State" = "New Mexico", "Race" = "NH_Hawaiian", .before = 222) %>%
   add_row("State" = "Rhode Island", "Race" = "NH_Hawaiian", .before = 278) %>%
   add_row("State" = "South Dakota", "Race" = "NH_Hawaiian", .before = 292) %>%
   add_row("State" = "Vermont", "Race" = "NH_Hawaiian", .before = 320) %>%
   add_row("State" = "West Virginia", "Race" = "NH_Hawaiian", .before = 341)

Flw_AgeRace <- Flw_AgeRace %>% mutate(flw_1664 = replace_na(flw_1664, 0),
                                        flw_1624 = replace_na(flw_1624, 0),
                                        flw_2534 = replace_na(flw_2534, 0),
                                        flw_3544 = replace_na(flw_3544, 0),
                                        flw_4554= replace_na(flw_4554, 0),
                                        flw_5564= replace_na(flw_5564, 0),)
#View(Flw_AgeRace)
# Fiwed it: # nrow(Flw_AgeRace) == length(unique(Flw_AgeRace$State)) * length(unique(Flw_AgeRace$Race))
Flw_AgeRace2 <- Flw_AgeRace

# For each State, compute the total number of flw across all races.
unique(Flw_AgeRace2$Race)
df8 <- Flw_AgeRace2 %>%
   group_by(State) %>%
   summarise("flw_1664" = sum(flw_1664),
             "flw_1624" = sum(flw_1624),
             "flw_2534" = sum(flw_2534),
             "flw_3544" = sum(flw_3544),
             "flw_4554" = sum(flw_4554),
             "flw_5564" = sum(flw_5564) )
df8$Race <- "All_races"
df8
Flw_AgeRace2 <- rbind(as.data.frame(Flw_AgeRace2), df8)
Flw_AgeRace2 <- Flw_AgeRace2[with(Flw_AgeRace2, order(State, Race)),] #Flw_AgeRace2[order(Flw_AgeRace2$State),]
#
Flw_AgeRace2
nrow(Flw_AgeRace2)
head(Flw_AgeRace2, 20)

#######################
### Mortality rates ###
#######################

## Compute the mortality rate by sex and race 

# a) get number of covid deaths (whole country by age cat and race)
Deaths_CDC_raw
MortalityRate <- Deaths_CDC_raw %>%
   rename(Age = 'Age group',
          Race = 'Race and Hispanic Origin Group',
          COVIDdeaths = 'COVID-19 Deaths') %>%
   select(State, Age, Race, COVIDdeaths) %>%
   filter(State == "United States" & Race != "Total Deaths" & Race != "Unknown") %>%
   filter(#Age == "Under 1 year" | Age == "1-4 years" | Age == "5-14 years" | # Age == "75-84 years" | Age == "85 years and over")
             Age == "15-24 years"| Age == "25-34 years"| Age == "35-44 years" |
             Age == "45-54 years" | Age == "55-64 years"| Age == "65-74 years" ) %>%
   select(-State)

unique(MortalityRate$Age)
MortalityRate
sum(MortalityRate$COVIDdeaths)

MortalityRate$Race[MortalityRate$Race == "Non-Hispanic White"] <- "NH_White"
MortalityRate$Race[MortalityRate$Race == "Non-Hispanic Black"] <- "NH_Black"
MortalityRate$Race[MortalityRate$Race == "Non-Hispanic Asian"] <- "NH_Asian"
MortalityRate$Race[MortalityRate$Race == "Non-Hispanic American Indian or Alaska Native" ] <- "NH_Americanindian"
MortalityRate$Race[MortalityRate$Race == "Non-Hispanic More than one race" ] <- "NH_morethanonerace"
MortalityRate$Race[MortalityRate$Race == "Non-Hispanic Native Hawaiian or Other Pacific Islander"] <- "NH_Hawaiian"
MortalityRate
# Number of COVID deaths summed over all States and age groups

# Transform MortalityRate from long to wide
MortalityRateA <-  spread(MortalityRate, key = Age, value = COVIDdeaths)
MortalityRateA

# b) get number of individuals (whole country by age cat and race)
table(pums_agerace4$AGEP)
MortalityRateB <- pums_agerace4 %>%
      group_by(Race) %>%
      summarize(all_1524 = sum(PWGTP[AGEP >= 15 & AGEP <= 24]), 
                all_2534 = sum(PWGTP[AGEP >= 25 & AGEP <= 34]),
                all_3544 = sum(PWGTP[AGEP >= 35 & AGEP <= 44]),
                all_4554 = sum(PWGTP[AGEP >= 45 & AGEP <= 54]),
                all_5564 = sum(PWGTP[AGEP >= 55 & AGEP <= 64]),
                all_6574 = sum(PWGTP[AGEP >= 65 & AGEP <= 74]))
MortalityRateB
#

# c) Join the two datasets: number of deaths and number of individuals to get the moratlity rates
MortalityRateA <- MortalityRateA %>%
   rename(deaths1524 = "15-24 years",
          deaths2534 = "25-34 years",
          deaths3544 = "35-44 years",
          deaths4554 = "45-54 years",
          deaths5564 = "55-64 years",
          deaths6574 = "65-74 years")
MortalityRateA

MortalityRateB <- MortalityRateB %>%
   rename(n1524 = "all_1524",
          n2534 = "all_2534",
          n3544 = "all_3544",
          n4554 = "all_4554",
          n5564 = "all_5564",
          n6574 = "all_6574")
MortalityRateB

MortalityRateA
MortalityRateB
MortalityRate <- left_join(MortalityRateA, MortalityRateB, by = "Race")
MortalityRate2 <- MortalityRate %>%
   mutate("15-24 years" = deaths1524 / n1524 * 100000,
          "25-34 years" = deaths2534 / n2534 * 100000,
          "35-44 years" = deaths3544 / n3544 * 100000,
          "45-54 years" = deaths4554 / n4554 * 100000,
          "55-64 years" = deaths5564 / n5564 * 100000,
          "65-74 years" = deaths6574 / n6574 * 100000) %>%
   select(c(Race, "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years"))
   #select(c(Race, rate1524 , rate2534, rate3544, rate4554, rate5564, rate6574))
MortalityRate2

# Plot mortality rate for Supplementary material: 
# These mortality rates are different from those in Table 1
# The number of individuals in denominator are the same, 
# But the number of deaths are different.
# At least 3 reasons why:
# Table 1 looks at deaths that tool place outside of nursing homes
#         has the number of deaths multiplied by 0.2
#         Summing over all states does not exactly equal the number of total deaths in the "United States"
# Importantly, the age categories are pretty large, this is not very precise and not ideal for comparison across races.
MortalityRate3 <- melt(MortalityRate2, id.vars = "Race")
MortalityRate3
MortalityRate3$Race[MortalityRate3$Race == "NH_White"] <- "NH White"
MortalityRate3$Race[MortalityRate3$Race == "NH_Black"] <- "NH Black"
MortalityRate3$Race[MortalityRate3$Race == "NH_Asian"] <-"NH Asian"
MortalityRate3$Race[MortalityRate3$Race == "NH_Americanindian"] <- "NH AIAN"
MortalityRate3$Race[MortalityRate3$Race == "NH_morethanonerace"] <- "NH 2+ races"
MortalityRate3$Race[MortalityRate3$Race == "NH_Hawaiian"] <- "NH NHPI"
MortalityRate3$Race <- factor(MortalityRate3$Race,levels = c("NH White", "Hispanic", "NH Black","NH Asian", "NH 2+ races", "NH AIAN", "NH NHPI"))
MortalityRate3
order_race_shorter_7 <- c("NH White", "Hispanic", "NH Black", "NH Asian", "NH 2+ races", "NH AIAN", "NH NHPI")

ggplot(MortalityRate3, aes(Race, variable, fill= value)) +
   geom_tile() +
   geom_text(aes(label = round(value, 1)), colour = "white") + 
   labs(title = "COVID-19 mortality rates per 100,000 by age and race/ethnicity\ncategories in the US in 2020",
        y = "Age categories",
        x = "Race/ethnicity categories") +
   scale_fill_viridis(option = "plasma", # viridis
                      name = "COVID-19\nmortality rate\nper 100,000",
                      limits = c(0, 576)) +
   scale_x_discrete(expand=c(0,0)) +
   scale_y_discrete(expand=c(0,0)) +
   theme(legend.key.height= unit(0.6, 'cm'),
         legend.key.width= unit(0.6, 'cm'),
         axis.text.x = element_text(angle = 45, hjust = 1),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank())

dev.copy2pdf(file = "figures/SupplFigure3.pdf",
             useDingbats=FALSE,
             width = 6.5, height = 4)


####################
### Calculations ###
####################

## Check States and Race categories are identical in all three datasets
# Same var names
colnames(All_AgeRace3); colnames(Flw_AgeRace2); colnames(Deaths_CDC5A)
#Same values Race : 8 Races
sort(unique(All_AgeRace3$Race)); sort(unique(Flw_AgeRace2$Race)); sort(unique(Deaths_CDC5A$Race))
setequal( c(sort(unique(All_AgeRace3$Race))), c(sort(unique(Flw_AgeRace2$Race))) )
setequal( c(sort(unique(All_AgeRace3$Race))), c(sort(unique(Deaths_CDC5A$Race))) )
#Same values States : 51 States
sort(unique(All_AgeRace3$State)); sort(unique(Flw_AgeRace2$State)); sort(unique(Deaths_CDC5A$State))
setequal( c(sort(unique(All_AgeRace3$State))), c(sort(unique(Flw_AgeRace2$State))) )
setequal( c(sort(unique(All_AgeRace3$State))), c(sort(unique(Deaths_CDC5A$State))) )
#Same nrow
nrow(All_AgeRace3); nrow(Flw_AgeRace2); nrow(Deaths_CDC5A)


### Merge the three datasets
DFA <- merge(All_AgeRace3, Flw_AgeRace2, by=c("State","Race"),  all = TRUE) %>%
   merge(., Deaths_CDC6A, by=c("State","Race"),  all = TRUE)
#saveRDS(DFA, "data/DFA.rds")

DFA <- readRDS("data/DFA.rds")
LifeExpectationsUSA19_3 <- readRDS("data/LifeExpectationsUSA19_3.rds")

colnames(DFA)
nrow(DFA)
head(DFA)

### Input other variable(s) : 
## Increased risk of death for front-lien workers
R1 = 1.56 # From BMJ paper, Mutambudzi et al. https://oem.bmj.com/content/early/2020/12/01/oemed-2020-106731

## Number of vaccine courses
# Fixing value for v.
v = 1000 * 0.95 # 1 thousand courses per State with 95% efficacy (2 thousand doses per State if two-shots vaccine).

# Create vectors to re-order Race variable in the Tables : Order by the proportion of individuals of each race in the US.
order_race_long <- c("All races","Non-Hispanic White", "Hispanic",
                "Non-Hispanic Black", "Non-Hispanic Asian",
                "Non-Hispanic Two or more races",
                "Non-Hispanic American Indian or Alaska Native",
                "Non-Hispanic Native Hawaiian or Other Pacific Islander")
order_race_long

order_race_short <- c("All_races","NH_White", "Hispanic",
                "NH_Black", "NH_Asian",
                "NH_morethanonerace",
                "NH_Americanindian",
                "NH_Hawaiian")
order_race_short

order_race_shorter <- c("All races","NH White", "Hispanic",
                        "NH Black", "NH Asian",
                        "NH 2+ races",
                        "NH AIAN", #Native
                        "NH NHPI")
order_race_shorter

###########################
### Generate Table 2, 3 and 4 : Mortality, Proportion of doses received, Sj, Fj and ratio for the whole country.
# Generate df for all of US : sum across all states by race categories
DF_USA <- as.data.frame(DFA %>%
                          select(-State) %>%
                          group_by(Race) %>%
                          summarise_all(funs(sum)))
DF_USA

## Generate Table 2 : Mortality rates and Proportion of doses for each race under both policies.
# Compute all the steps to ratio (here without sampling from beta distribution)
DF_US2A <- DF_USA %>%
   mutate(mortality_F1624 = R1 * deaths_1624 / all_1624, # Mortality rate among flw of each age subcategories.
          mortality_F2534 = R1 * deaths_2534 / all_2534,
          mortality_F3544 = R1 * deaths_3544 / all_3544,
          mortality_F4554 = R1 * deaths_4554 / all_4554,
          mortality_F5564 = R1 * deaths_5564 / all_5564,

          mortality_Fcrude  = R1 * deaths_1664 / all_1664, # Mortality rate among flw 1664 when not considering the age distribution of flw. For information/comparison, do not use.

          mortality_Fagecat = (mortality_F1624 * flw_1624 +
                                       mortality_F2534 * flw_2534 +
                                       mortality_F3544 * flw_3544 +
                                       mortality_F4554 * flw_4554 +
                                       mortality_F5564 * flw_5564 ) / flw_1664, # Mortality rate among flw 1664 when considering the age subcategories inside 1664. Computed this for Table 1.

          prop_F1624 = flw_1624 / flw_1664[Race == "All_races"], # Proportion of flw of a given race and age subcat among all flw (all races all ages)
          prop_F2534 = flw_2534 / flw_1664[Race == "All_races"],
          prop_F3544 = flw_3544 / flw_1664[Race == "All_races"],
          prop_F4554 = flw_4554 / flw_1664[Race == "All_races"],
          prop_F5564 = flw_5564 / flw_1664[Race == "All_races"],
          prop_F1664 = flw_1664 / flw_1664[Race == "All_races"],
          prop_F1664sum = prop_F1624 + prop_F2534 + prop_F3544 + prop_F4554 + prop_F5564, # This should be equal to prop_F1664, computed it to check.

          mortality_S = deaths_6574 / all_6574, # Mortality among 65-74 year olds
          prop_S = all_6574 / all_6574[Race == "All_races"],  # Proportion of 64-75 yo of a given race

          Fj1624 = v * mortality_F1624 * prop_F1624, # Number of lives saved under policy F among each age subgroup
          Fj2534 = v * mortality_F2534 * prop_F2534,
          Fj3544 = v * mortality_F3544 * prop_F3544,
          Fj4554 = v * mortality_F4554 * prop_F4554,
          Fj5564 = v * mortality_F5564 * prop_F5564,
          FjagecatA = Fj1624 + Fj2534 + Fj3544 + Fj4554 + Fj5564, # Total number of lives saved under policy F
          FjagecatB =  v * mortality_Fagecat * prop_F1664, # This should be equal to FjagecatA, computed it to check.
          Fjcrude = v * mortality_Fcrude * prop_F1664, # For information/comparison, do not use.

          Sj = v * mortality_S * prop_S, # Total number of lives saved under policy S
          Sjtest = v * deaths_6574 / all_6574[Race == "All_races"],

          ratioagecatA = Sj / FjagecatA,
          ratioagecatB= Sj / FjagecatB, # This should be equal to ratioagecatA, computed it to check.
          ratiocrude= Sj / Fjcrude, # Ratio of number of lives saved

          # Including YLL here !
          Fj1624YLL = v * mortality_F1624 * prop_F1624 * LifeExpectationsUSA19_3[2,2],
          Fj2534YLL = v * mortality_F2534 * prop_F2534 * LifeExpectationsUSA19_3[3,2],
          Fj3544YLL = v * mortality_F3544 * prop_F3544 * LifeExpectationsUSA19_3[4,2],
          Fj4554YLL = v * mortality_F4554 * prop_F4554 * LifeExpectationsUSA19_3[5,2],
          Fj5564YLL = v * mortality_F5564 * prop_F5564 * LifeExpectationsUSA19_3[6,2],

          FjYLL = FjagecatA * LifeExpectationsUSA19_3[1,2],
          FjYLL2 = Fj1624YLL + Fj2534YLL + Fj3544YLL + Fj4554YLL + Fj5564YLL,

          SjYLL = Sj *  LifeExpectationsUSA19_3[7,2],
          SjYLL2 = v * mortality_S * prop_S *  LifeExpectationsUSA19_3[7,2],

          ratioYLL = SjYLL / FjYLL,
          ratioYLL2 = SjYLL2 / FjYLL2 ) %>%

   select(Race,
          mortality_F1624, mortality_F2534, mortality_F3544, mortality_F4554, mortality_F5564,
          mortality_Fcrude, mortality_Fagecat,
          prop_F1624, prop_F2534, prop_F3544, prop_F4554, prop_F5564, prop_F1664, prop_F1664sum,
          Fj1624, Fj2534, Fj3544, Fj4554, Fj5564,
          FjagecatA, FjagecatB, Fjcrude,
          mortality_S, prop_S, Sj, Sjtest,
          ratioagecatA, ratioagecatB, ratiocrude,
          Fj1624YLL, Fj2534YLL, Fj3544YLL, Fj4554YLL, Fj5564YLL, FjYLL, FjYLL2,
          SjYLL, SjYLL2, ratioYLL, ratioYLL2)

Table1A <- DF_US2A %>%
   select(Race, prop_S, prop_F1664, mortality_S, mortality_Fagecat) %>%
   mutate(mortality_S = mortality_S * 1e5,
          mortality_Fagecat = mortality_Fagecat * 1e5) %>%
   mutate_if(is.numeric, round, digits = 6) %>%
   arrange(-prop_F1664)
Table1A
Table1A %>%
   mutate(prop_S = prop_S * 100,
          prop_F1664 = prop_F1664 * 100) %>%
   mutate_if(is.numeric, round, digits = 1)
Table1A %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   kable(. ,"latex")



## Generate Table 3 : Sj, Fj and ratio for each race.
# Include measure of uncertainty and sampling from beta distribution 
n_rep <- 10000
DF_US3A <- DF_USA %>%
 #  as.data.frame() %>%
   group_by(Race) %>%
   mutate(## LS (number of lives saved - policy S) : 
          #Sj_short = v * deaths_6574 / all_6574[Race == "All_races"],
          #Sj_short = v * deaths_6574 / 31567241,
          Sj_short = v * deaths_6574 / DF_USA$all_6574[DF_USA$Race == "All_races"],
          #Sj_short_vec = list(v * rbeta(seq(1,n_rep), (0 + deaths_6574), (0 + all_6574[Race == "All_races"] - deaths_6574) ))
          Sj_short_vec = list(v * rbeta(seq(1,n_rep), (0 + deaths_6574), (0 + DF_USA$all_6574[DF_USA$Race == "All_races"] - deaths_6574) )),
          
          ## LF (number of lives saved - policy F) : 
          mortality_F1624 = R1 * deaths_1624 / all_1624,
          mortality_F1624_vec = list(R1 * rbeta(seq(1,n_rep), (0 + deaths_1624), (0 + all_1624 - deaths_1624))),
          mortality_F2534 = R1 * deaths_2534 / all_2534,
          mortality_F2534_vec = list(R1 * rbeta(seq(1,n_rep), (0 + deaths_2534), (0 + all_2534 - deaths_2534))),
          mortality_F3544 = R1 * deaths_3544 / all_3544,
          mortality_F3544_vec = list(R1 * rbeta(seq(1,n_rep), (0 + deaths_3544), (0 + all_3544 - deaths_3544))),
          mortality_F4554 = R1 * deaths_4554 / all_4554,
          mortality_F4554_vec = list(R1 * rbeta(seq(1,n_rep), (0 + deaths_4554), (0 + all_4554 - deaths_4554))),
          mortality_F5564 = R1 * deaths_5564 / all_5564,
          mortality_F5564_vec = list(R1 * rbeta(seq(1,n_rep), (0 + deaths_5564), (0 + all_5564 - deaths_5564))),
          
          prop_F1624 = flw_1624 / DF_USA$flw_1664[DF_USA$Race == "All_races"], 
          prop_F1624_vec = list(rbeta(seq(1,n_rep), (0 + flw_1624), (0 + DF_USA$flw_1664[DF_USA$Race == "All_races"] - flw_1624) )),
          prop_F2534 = flw_2534 / DF_USA$flw_1664[DF_USA$Race == "All_races"], 
          prop_F2534_vec = list(rbeta(seq(1,n_rep), (0 + flw_2534), (0 + DF_USA$flw_1664[DF_USA$Race == "All_races"] - flw_2534) )),
          prop_F3544 = flw_3544 / DF_USA$flw_1664[DF_USA$Race == "All_races"], 
          prop_F3544_vec = list(rbeta(seq(1,n_rep), (0 + flw_3544), (0 + DF_USA$flw_1664[DF_USA$Race == "All_races"] - flw_3544) )),
          prop_F4554 = flw_4554 / DF_USA$flw_1664[DF_USA$Race == "All_races"], 
          prop_F4554_vec = list(rbeta(seq(1,n_rep), (0 + flw_4554), (0 + DF_USA$flw_1664[DF_USA$Race == "All_races"] - flw_4554) )),
          prop_F5564 = flw_5564 / DF_USA$flw_1664[DF_USA$Race == "All_races"], 
          prop_F5564_vec = list(rbeta(seq(1,n_rep), (0 + flw_5564), (0 + DF_USA$flw_1664[DF_USA$Race == "All_races"] - flw_5564) )),

          Fj1624 = v * mortality_F1624 * prop_F1624,
          Fj1624_vec = list(v * mortality_F1624_vec[[1]] * prop_F1624_vec[[1]]),
          Fj2534 = v * mortality_F2534 * prop_F2534,
          Fj2534_vec = list(v * mortality_F2534_vec[[1]] * prop_F2534_vec[[1]]),
          Fj3544 = v * mortality_F3544 * prop_F3544,
          Fj3544_vec = list(v * mortality_F3544_vec[[1]] * prop_F3544_vec[[1]]),
          Fj4554 = v * mortality_F4554 * prop_F4554,
          Fj4554_vec = list(v * mortality_F4554_vec[[1]] * prop_F4554_vec[[1]]),
          Fj5564 = v * mortality_F5564 * prop_F5564,
          Fj5564_vec = list(v * mortality_F5564_vec[[1]] * prop_F5564_vec[[1]]),

          FjagecatA = Fj1624 + Fj2534 + Fj3544 + Fj4554 + Fj5564,
          FjagecatA_vec = list(Fj1624_vec[[1]] + Fj2534_vec[[1]] + Fj3544_vec[[1]] + Fj4554_vec[[1]] + Fj5564_vec[[1]]),
          
          ## Ratio L (number of lives saved) : 
          ratioagecatA = Sj_short / FjagecatA,
          ratioagecatA_vec = list(Sj_short_vec[[1]] / FjagecatA_vec[[1]]),
          
          ## YS (years of lives saved - policy S) : 
          SjYLL = Sj_short *  LifeExpectationsUSA19_3[7,2],
          SjYLL_vec = list(Sj_short_vec[[1]] * LifeExpectationsUSA19_3[7,2]),

          ## YF (years of lives saved - policy F) : 
          Fj1624YLL = v * mortality_F1624 * prop_F1624 * LifeExpectationsUSA19_3[2,2],
          Fj1624YLL_vec = list(v * mortality_F1624_vec[[1]] * prop_F1624_vec[[1]] * LifeExpectationsUSA19_3[2,2]),
          Fj2534YLL = v * mortality_F2534 * prop_F2534 * LifeExpectationsUSA19_3[3,2],
          Fj2534YLL_vec = list(v * mortality_F2534_vec[[1]] * prop_F2534_vec[[1]] * LifeExpectationsUSA19_3[3,2]),
          Fj3544YLL = v * mortality_F3544 * prop_F3544 * LifeExpectationsUSA19_3[4,2],
          Fj3544YLL_vec = list(v * mortality_F3544_vec[[1]] * prop_F3544_vec[[1]] * LifeExpectationsUSA19_3[4,2]),
          Fj4554YLL = v * mortality_F4554 * prop_F4554 * LifeExpectationsUSA19_3[5,2],
          Fj4554YLL_vec = list(v * mortality_F4554_vec[[1]] * prop_F4554_vec[[1]] * LifeExpectationsUSA19_3[5,2]),
          Fj5564YLL = v * mortality_F5564 * prop_F5564 * LifeExpectationsUSA19_3[6,2],
          Fj5564YLL_vec = list(v * mortality_F5564_vec[[1]] * prop_F5564_vec[[1]] * LifeExpectationsUSA19_3[6,2]),
          
          FjYLL2 = Fj1624YLL + Fj2534YLL + Fj3544YLL + Fj4554YLL + Fj5564YLL,
          FjYLL2_vec = list(Fj1624YLL_vec[[1]] + Fj2534YLL_vec[[1]] + Fj3544YLL_vec[[1]] + Fj4554YLL_vec[[1]] + Fj5564YLL_vec[[1]]),
          
          ## Ratio Y (years of life saved) : 
          ratioYLL = SjYLL / FjYLL2,
          ratioYLL_vec = list(SjYLL_vec[[1]] / FjYLL2_vec[[1]])
          
          ) %>% ungroup()

Table2A_vec <- DF_US3A %>%
   select(Race, Sj_short_vec, FjagecatA_vec, ratioagecatA_vec) %>%
   group_by(Race) %>%
   mutate(Sj_short_vec_mean = mean(unlist(Sj_short_vec)),
          Sj_short_vec_lb = quantile(unlist(Sj_short_vec), probs=c(0.025)),
          Sj_short_vec_ub = quantile(unlist(Sj_short_vec), probs=c(0.975)),
          FjagecatA_vec_mean = mean(unlist(FjagecatA_vec)),
          FjagecatA_vec_lb = quantile(unlist(FjagecatA_vec), probs=c(0.025)),
          FjagecatA_vec_ub = quantile(unlist(FjagecatA_vec), probs=c(0.975)),
          ratioagecatA_vec_mean = mean(unlist(ratioagecatA_vec)), # put unlist(), or [[1]] at the end.
          ratioagecatA_vec_95lb = quantile(unlist(ratioagecatA_vec), probs=c(0.025)),
          ratioagecatA_vec_95ub = quantile(unlist(ratioagecatA_vec), probs=c(0.975))
          ) %>%
   mutate_if(is.numeric, round, digits = 4) %>%
   #example to update si jamais envie mutate(Sj_short_vec_mean = ifelse(Race == "NH_Hawaiian", Sj_short_vec_mean, round(Sj_short_vec_mean, 3))) %>%
   mutate(ratioagecatA_vec_mean = round(ratioagecatA_vec_mean, 2), 
          ratioagecatA_vec_95lb = round(ratioagecatA_vec_95lb, 2),
          ratioagecatA_vec_95ub = round(ratioagecatA_vec_95ub, 2)) %>%
   mutate(Race = factor(Race, levels = order_race_short)) %>%
   arrange(Race) %>% 
   select(-c(Sj_short_vec, FjagecatA_vec, ratioagecatA_vec))
   
Table2A_vec
write.table(Table2A_vec, file = "tables/Table2A_vec.txt", sep = ",", quote = FALSE, row.names = F)

## Generate Table 4 : YLS
# Include measure of uncertainty and sampling from beta distribution 
Table3A_vec <- DF_US3A %>%
   select(Race, SjYLL_vec, FjYLL2_vec, ratioYLL_vec) %>%
   group_by(Race) %>%
   mutate(SjYLL_vec_mean = mean(unlist(SjYLL_vec)),
          SjYLL_vec_lb = quantile(unlist(SjYLL_vec), probs=c(0.025)),
          SjYLL_vec_ub = quantile(unlist(SjYLL_vec), probs=c(0.975)),
          FjYLL2_vec_mean = mean(unlist(FjYLL2_vec)),
          FjYLL2_vec_lb = quantile(unlist(FjYLL2_vec), probs=c(0.025)),
          FjYLL2_vec_ub = quantile(unlist(FjYLL2_vec), probs=c(0.975)),
          ratioYLL_vec_mean = mean(unlist(ratioYLL_vec)), # put unlist(), or [[1]] at the end.
          ratioYLL_vec_95lb = quantile(unlist(ratioYLL_vec), probs=c(0.025)),
          ratioYLL_vec_95ub = quantile(unlist(ratioYLL_vec), probs=c(0.975))
   ) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   mutate(ratioYLL_vec_mean = round(ratioYLL_vec_mean, 2), 
          ratioYLL_vec_95lb = round(ratioYLL_vec_95lb, 2),
          ratioYLL_vec_95ub = round(ratioYLL_vec_95ub, 2)) %>%
   mutate(Race = factor(Race, levels = order_race_short)) %>%
   arrange(Race) %>% 
   select(-c(SjYLL_vec, FjYLL2_vec, ratioYLL_vec))
Table3A_vec
write.table(Table3A_vec, file = "tables/Table3A_vec.txt", sep = ",", quote = FALSE, row.names = F)


###########################
### Generate Supplementary Table 3, 4 and 5 : Mortality, Proportion of doses received, Sj, Fj and ratio for each state !

## Generate Supplementary Table 3 : Mortality rates and Proportion of doses for each race under both policies.
# Compute all the steps to ratio (here without sampling from beta distribution)
DF_StatesA <- DFA %>%
   group_by(State) %>%
   mutate(mortality_F1624 = R1 * deaths_1624 / all_1624, # Mortality rate among flw of each age subcategories.
          mortality_F2534 = R1 * deaths_2534 / all_2534,
          mortality_F3544 = R1 * deaths_3544 / all_3544,
          mortality_F4554 = R1 * deaths_4554 / all_4554,
          mortality_F5564 = R1 * deaths_5564 / all_5564,

          mortality_Fcrude  = R1 * deaths_1664 / all_1664, # Mortality rate among flw 1664 when not considering the age distribution of flw. For information, do not use.

          mortality_Fagecat = (mortality_F1624 * flw_1624 +
                                  mortality_F2534 * flw_2534 +
                                  mortality_F3544 * flw_3544 +
                                  mortality_F4554 * flw_4554 +
                                  mortality_F5564 * flw_5564 ) / flw_1664, # Mortality rate among flw 1664 when considering the age subcategories inside 1664. Computed this for Table 1.


          prop_F1624 = flw_1624 / flw_1664[Race == "All_races"], # Proportion of flw of a given race and age subcat among all flw (all races all ages)
          prop_F2534 = flw_2534 / flw_1664[Race == "All_races"],
          prop_F3544 = flw_3544 / flw_1664[Race == "All_races"],
          prop_F4554 = flw_4554 / flw_1664[Race == "All_races"],
          prop_F5564 = flw_5564 / flw_1664[Race == "All_races"],
          prop_F1664 = flw_1664 / flw_1664[Race == "All_races"],
          prop_F1664sum = prop_F1624 + prop_F2534 + prop_F3544 + prop_F4554 + prop_F5564,

          mortality_S = deaths_6574 / all_6574, # Mortality among 65-74 year olds
          prop_S = all_6574 / all_6574[Race == "All_races"],  # Proportion of 64-75 yo of a given race

          Fj1624 = v * mortality_F1624 * prop_F1624, # Number of lives saved under policy F among each age subgroup
          Fj2534 = v * mortality_F2534 * prop_F2534,
          Fj3544 = v * mortality_F3544 * prop_F3544,
          Fj4554 = v * mortality_F4554 * prop_F4554,
          Fj5564 = v * mortality_F5564 * prop_F5564,
          FjagecatA = Fj1624 + Fj2534 + Fj3544 + Fj4554 + Fj5564, # Total number of lives saved under policy F
          FjagecatB =  v * mortality_Fagecat * prop_F1664,
          Fjcrude = v * mortality_Fcrude * prop_F1664,

          Sj = v * mortality_S * prop_S,  # Total number of lives saved under policy S
          Sjtest = v * deaths_6574 / all_6574[Race == "All_races"],

          ratioagecatA = Sj / FjagecatA,
          ratioagecatB= Sj / FjagecatB,
          ratiocrude= Sj / Fjcrude, # %>% # Ratio of number of lives saved

          # Include YLL here
          Fj1624YLL = v * mortality_F1624 * prop_F1624 * LifeExpectationsUSA19_3[2,2],
          Fj2534YLL = v * mortality_F2534 * prop_F2534 * LifeExpectationsUSA19_3[3,2],
          Fj3544YLL = v * mortality_F3544 * prop_F3544 * LifeExpectationsUSA19_3[4,2],
          Fj4554YLL = v * mortality_F4554 * prop_F4554 * LifeExpectationsUSA19_3[5,2],
          Fj5564YLL = v * mortality_F5564 * prop_F5564 * LifeExpectationsUSA19_3[6,2],

          FjYLL = FjagecatA * LifeExpectationsUSA19_3[1,2],
          FjYLL2 = Fj1624YLL + Fj2534YLL + Fj3544YLL + Fj4554YLL + Fj5564YLL,

          SjYLL = Sj *  LifeExpectationsUSA19_3[7,2],
          SjYLL2 = v * mortality_S * prop_S *  LifeExpectationsUSA19_3[7,2],

          ratioYLL = SjYLL / FjYLL,
          ratioYLL2 = SjYLL2 / FjYLL2 )
select(State, Race,
       mortality_F1624, mortality_F2534, mortality_F3544, mortality_F4554, mortality_F5564,
       mortality_Fcrude, mortality_Fagecat,
       prop_F1624, prop_F2534, prop_F3544, prop_F4554, prop_F5564, prop_F1664, prop_F1664sum,
       Fj1624, Fj2534, Fj3544, Fj4554, Fj5564,
       FjagecatA, FjagecatB, Fjcrude,
       mortality_S, prop_S, Sj, Sjtest,
       ratioagecatA, ratioagecatB, ratiocrude)
DF_StatesA <- as.data.frame(DF_StatesA)
DF_StatesA
head(DF_StatesA, 20)
# Replace the race/state combinations for which there are less then 2 deaths reported in 1664 or 6574 by NAs.
DF_States2A <- DF_StatesA %>%
  mutate(ratioagecatnew = ratioagecatA,
         ratioagecatnew = case_when(
           deaths_1664 < 2 ~ NA_real_,
           deaths_6574 < 2 ~ NA_real_,
           # flw_1664 < 100 ~ NA_real_,
           # all_6574 < 100 ~ NA_real_,
           ratioagecatA == 0 ~ NA_real_,
           TRUE ~ as.numeric(ratioagecatA)))
DF_States2AYLL <- DF_StatesA %>%
  mutate(ratioYLLnew =  ratioYLL2,
         ratioYLLnew = case_when(
           deaths_1664 < 2 ~ NA_real_,
           deaths_6574 < 2 ~ NA_real_,
           # flw_1664 < 100 ~ NA_real_,
           # all_6574 < 100 ~ NA_real_,
           ratioYLL2 == 0 ~ NA_real_,
           TRUE ~ as.numeric(ratioYLL2)))
# Generate Supplementary Table 1 : Mortality rates and Proportion of doses for each race under both policies.
TableS1A <- DF_States2A %>%
   select(State, Race, prop_S, prop_F1664, mortality_S, mortality_Fagecat) %>%
   mutate(mortality_S = mortality_S * 1e5,
          mortality_Fagecat = mortality_Fagecat * 1e5) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   arrange(State)
TableS1A
#Rename races
TableS1A$Race[TableS1A$Race == "All_races"] <- "All races"
TableS1A$Race[TableS1A$Race == "NH_Americanindian"] <- "NH AIAN"
TableS1A$Race[TableS1A$Race == "NH_White"] <- "NH White"
TableS1A$Race[TableS1A$Race == "NH_Asian"] <- "NH Asian"
TableS1A$Race[TableS1A$Race == "NH_Black"] <- "NH Black"
TableS1A$Race[TableS1A$Race == "NH_morethanonerace"] <- "NH 2+ races"
TableS1A$Race[TableS1A$Race == "NH_Hawaiian"] <- "NH NHPI"
# Replace mortality Inf, NaN by NA.
TableS1A$mortality_S[sapply(TableS1A$mortality_S, is.infinite)] <- NA
TableS1A$mortality_S[sapply(TableS1A$mortality_S, is.na)] <- NA
TableS1A$mortality_Fagecat[sapply(TableS1A$mortality_Fagecat, is.infinite)] <- NA
TableS1A$mortality_Fagecat[sapply(TableS1A$mortality_Fagecat, is.na)] <- NA
#Change order of races to match order in Table 2 All, white, hisp etc.
TableS1A <- TableS1A %>%
   mutate(Race = factor(Race, levels = order_race_shorter)) %>%
   arrange(State, Race)
head(TableS1A, 15)
TableS1A
TableS1A %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   kable(. ,"latex")


## Generate Supplementary Table 4, with 95% uncertainty intervals ! : Sj, Fj and ratio for each race.
n_rep
DFA <- readRDS("data/DFA.rds")
DFA[DFA$State == "Arkansas" & DFA$Race == "NH_Hawaiian",] <- DFA[DFA$State == "Arkansas" & DFA$Race == "NH_Hawaiian",] %>% mutate(deaths_5564 = 0) #deaths_5564) 
DFA <- DFA %>% 
   group_by(State) %>% 
   mutate(all_6574_allraces = all_6574[Race == "All_races"],
          flw_1664_allraces = flw_1664[Race == "All_races"]) # We need to group by Race AND State to use rbeta, but we need to reference the all_races values for those two variables.

DF_StatesA_vec <- DFA %>%
   group_by(Race, State) %>% 
   mutate(## LS (number of lives saved - policy S) : 
      Sj_short = v * deaths_6574 / all_6574_allraces,
      Sj_short_vec = list(v * rbeta(seq(1,n_rep), (0 +  deaths_6574), (0 +  all_6574_allraces - deaths_6574) )),
      
      ## LF (number of lives saved - policy F) : 
      mortality_F1624 = R1 * deaths_1624 / all_1624,
      mortality_F1624_vec = list(R1 * rbeta(seq(1,n_rep), (0 +  deaths_1624), (0 +  all_1624 - deaths_1624))),
      mortality_F2534 = R1 * deaths_2534 / all_2534,
      mortality_F2534_vec = list(R1 * rbeta(seq(1,n_rep), (0 +  deaths_2534), (0 +  all_2534 - deaths_2534))),
      mortality_F3544 = R1 * deaths_3544 / all_3544,
      mortality_F3544_vec = list(R1 * rbeta(seq(1,n_rep), (0 +  deaths_3544), (0 +  all_3544 - deaths_3544))),
      mortality_F4554 = R1 * deaths_4554 / all_4554,
      mortality_F4554_vec = list(R1 * rbeta(seq(1,n_rep), (0 +  deaths_4554), (0 +  all_4554 - deaths_4554))),
      mortality_F5564 = R1 * deaths_5564 / all_5564,
      mortality_F5564_vec = list(R1 * rbeta(seq(1,n_rep), (0 +  deaths_5564), (0 +  all_5564 - deaths_5564))),
      
      prop_F1624 = flw_1624 / flw_1664_allraces, 
      prop_F1624_vec = list(rbeta(seq(1,n_rep), (0 +  flw_1624), (0 +  flw_1664_allraces - flw_1624) )),
      prop_F2534 = flw_2534 / flw_1664_allraces, 
      prop_F2534_vec = list(rbeta(seq(1,n_rep), (0 +  flw_2534), (0 +  flw_1664_allraces - flw_2534) )),
      prop_F3544 = flw_3544 / flw_1664_allraces, 
      prop_F3544_vec = list(rbeta(seq(1,n_rep), (0 +  flw_3544), (0 +  flw_1664_allraces - flw_3544) )),
      prop_F4554 = flw_4554 / flw_1664_allraces, 
      prop_F4554_vec = list(rbeta(seq(1,n_rep), (0 +  flw_4554), (0 +  flw_1664_allraces - flw_4554) )),
      prop_F5564 = flw_5564 / flw_1664_allraces, 
      prop_F5564_vec = list(rbeta(seq(1,n_rep), (0 +  flw_5564), (0 +  flw_1664_allraces - flw_5564) )),
      
      Fj1624 = v * mortality_F1624 * prop_F1624,
      Fj1624_vec = list(v * mortality_F1624_vec[[1]] * prop_F1624_vec[[1]]),
      Fj2534 = v * mortality_F2534 * prop_F2534,
      Fj2534_vec = list(v * mortality_F2534_vec[[1]] * prop_F2534_vec[[1]]),
      Fj3544 = v * mortality_F3544 * prop_F3544,
      Fj3544_vec = list(v * mortality_F3544_vec[[1]] * prop_F3544_vec[[1]]),
      Fj4554 = v * mortality_F4554 * prop_F4554,
      Fj4554_vec = list(v * mortality_F4554_vec[[1]] * prop_F4554_vec[[1]]),
      Fj5564 = v * mortality_F5564 * prop_F5564,
      Fj5564_vec = list(v * mortality_F5564_vec[[1]] * prop_F5564_vec[[1]]),
      
      FjagecatA = Fj1624 + Fj2534 + Fj3544 + Fj4554 + Fj5564,
      FjagecatA_vec = list(Fj1624_vec[[1]] + Fj2534_vec[[1]] + Fj3544_vec[[1]] + Fj4554_vec[[1]] + Fj5564_vec[[1]]),
      
      ## Ratio L (number of lives saved) : 
      ratioagecatA = Sj_short / FjagecatA,
      ratioagecatA_vec = list(Sj_short_vec[[1]] / FjagecatA_vec[[1]]),
      
      ## YS (years of lives saved - policy S) : 
      SjYLL = Sj_short *  LifeExpectationsUSA19_3[7,2],
      SjYLL_vec = list(Sj_short_vec[[1]] * LifeExpectationsUSA19_3[7,2]),
      
      ## YF (years of lives saved - policy F) : 
      Fj1624YLL = v * mortality_F1624 * prop_F1624 * LifeExpectationsUSA19_3[2,2],
      Fj1624YLL_vec = list(v * mortality_F1624_vec[[1]] * prop_F1624_vec[[1]] * LifeExpectationsUSA19_3[2,2]),
      Fj2534YLL = v * mortality_F2534 * prop_F2534 * LifeExpectationsUSA19_3[3,2],
      Fj2534YLL_vec = list(v * mortality_F2534_vec[[1]] * prop_F2534_vec[[1]] * LifeExpectationsUSA19_3[3,2]),
      Fj3544YLL = v * mortality_F3544 * prop_F3544 * LifeExpectationsUSA19_3[4,2],
      Fj3544YLL_vec = list(v * mortality_F3544_vec[[1]] * prop_F3544_vec[[1]] * LifeExpectationsUSA19_3[4,2]),
      Fj4554YLL = v * mortality_F4554 * prop_F4554 * LifeExpectationsUSA19_3[5,2],
      Fj4554YLL_vec = list(v * mortality_F4554_vec[[1]] * prop_F4554_vec[[1]] * LifeExpectationsUSA19_3[5,2]),
      Fj5564YLL = v * mortality_F5564 * prop_F5564 * LifeExpectationsUSA19_3[6,2],
      Fj5564YLL_vec = list(v * mortality_F5564_vec[[1]] * prop_F5564_vec[[1]] * LifeExpectationsUSA19_3[6,2]),
      
      FjYLL2 = Fj1624YLL + Fj2534YLL + Fj3544YLL + Fj4554YLL + Fj5564YLL,
      FjYLL2_vec = list(Fj1624YLL_vec[[1]] + Fj2534YLL_vec[[1]] + Fj3544YLL_vec[[1]] + Fj4554YLL_vec[[1]] + Fj5564YLL_vec[[1]]),
      
      ## Ratio Y (years of life saved) : 
      ratioYLL = SjYLL / FjYLL2,
      ratioYLL_vec = list(SjYLL_vec[[1]] / FjYLL2_vec[[1]])
      
   ) %>% ungroup()
DF_StatesA_vec
### Replace the race/state combinations for which there are less then 2 deaths reported in 1664 or 6574 by NAs.
DF_States2A_vec <- DF_StatesA_vec %>%
   group_by(Race, State) %>%
   mutate(ratioagecatnew_vec = ratioagecatA_vec, 
          ratioagecatnew_vec = case_when(
             deaths_1664 < 2 ~ list(rep(NA, n_rep)),
             deaths_6574 < 2 ~ list(rep(NA, n_rep)),
             ratioagecatA == 0 ~ list(rep(NA, n_rep)),
             (Race == "NH_Hawaiian" & State == "Arkansas") ~ list(rep(NA, n_rep)),
             TRUE ~ ratioagecatA_vec )) 

DF_States2AYLL_vec <- DF_StatesA_vec %>%
   group_by(Race, State) %>%
   mutate(ratioYLLnew_vec = ratioYLL_vec, 
          ratioYLLnew_vec = case_when(
             deaths_1664 < 2 ~ list(rep(NA, n_rep)),
             deaths_6574 < 2 ~ list(rep(NA, n_rep)),
             ratioYLL == 0 ~ list(rep(NA, n_rep)),
             (Race == "NH_Hawaiian" & State == "Arkansas") ~ list(rep(NA, n_rep)),
             TRUE ~ ratioYLL_vec )) 
# Generate Table 4 
TableS2A_vec <- DF_States2A_vec %>%
   select(State, Race, Sj_short_vec, FjagecatA_vec, ratioagecatnew_vec) %>%
   group_by(State, Race) %>%
   mutate(Sj_short_vec_mean = mean(unlist(Sj_short_vec), na.rm = T),
          Sj_short_vec_lb = quantile(unlist(Sj_short_vec), probs=c(0.025), na.rm = T),
          Sj_short_vec_ub = quantile(unlist(Sj_short_vec), probs=c(0.975), na.rm = T),
          FjagecatA_vec_mean = mean(unlist(FjagecatA_vec), na.rm = T),
          FjagecatA_vec_lb = quantile(unlist(FjagecatA_vec), probs=c(0.025), na.rm = T),
          FjagecatA_vec_ub = quantile(unlist(FjagecatA_vec), probs=c(0.975), na.rm = T),
          ratioagecatA_vec_mean = mean(unlist(ratioagecatnew_vec), na.rm = T), # put unlist(), or [[1]] at the end.
          ratioagecatA_vec_95lb = quantile(unlist(ratioagecatnew_vec), probs=c(0.025), na.rm = T), #, na.rm = T
          ratioagecatA_vec_95ub = quantile(unlist(ratioagecatnew_vec), probs=c(0.975), na.rm = T) #, na.rm = T
   ) %>%
   #mutate_if(is.numeric, round, digits = 3) %>%
   # mutate(ratioagecatA_vec_mean = round(ratioagecatA_vec_mean, 2), 
   #        ratioagecatA_vec_95lb = round(ratioagecatA_vec_95lb, 2),
   #        ratioagecatA_vec_95ub = round(ratioagecatA_vec_95ub, 2)) %>%
   mutate(Race = factor(Race, levels = order_race_short)) %>%
   arrange(State, Race) %>% 
   select(-c(Sj_short_vec, FjagecatA_vec, ratioagecatnew_vec))
TableS2A_vec


## Generate Supplementary Table 6, with bayesian 95% intervals ! : Sj, Fj and ratio for each race for YLS.
DF_States2AYLL_vec
TableS3A_vec <- DF_States2AYLL_vec %>%
  select(c(State, Race, SjYLL_vec, FjYLL2_vec, ratioYLLnew_vec)) %>%
  group_by(State, Race) %>%
  mutate(SjYLL_vec_mean = mean(unlist(SjYLL_vec), na.rm = T),
         SjYLL_vec_lb = quantile(unlist(SjYLL_vec), probs=c(0.025), na.rm = T),
         SjYLL_vec_ub = quantile(unlist(SjYLL_vec), probs=c(0.975), na.rm = T),
         FjYLL2_vec_mean = mean(unlist(FjYLL2_vec), na.rm = T),
         FjYLL2_vec_lb = quantile(unlist(FjYLL2_vec), probs=c(0.025), na.rm = T),
         FjYLL2_vec_ub = quantile(unlist(FjYLL2_vec), probs=c(0.975), na.rm = T),
         ratioYLL_vec_mean = mean(unlist(ratioYLLnew_vec), na.rm = T), # put unlist(), or [[1]] at the end.
         ratioYLL_vec_95lb = quantile(unlist(ratioYLLnew_vec), probs=c(0.025), na.rm = T), 
         ratioYLL_vec_95ub = quantile(unlist(ratioYLLnew_vec), probs=c(0.975), na.rm = T) 
  ) %>%
  mutate(Race = factor(Race, levels = order_race_short)) %>%
  arrange(State, Race) %>% 
  select(-c(SjYLL_vec, FjYLL2_vec, ratioYLLnew_vec))
TableS3A_vec



###########################
### Generate Figures 1 and 2 : heatmaps with the * showing significance levels

## Figure 1 
DF_States2A_heatmat_vec <- TableS2A_vec %>%
   select(c(State, Race, ratioagecatA_vec_mean, ratioagecatA_vec_95lb)) %>% #ratioagecatA_vec_95ub
   mutate(ratioagecatA_vec_mean2 = case_when(is.na(ratioagecatA_vec_mean) ~ 0,
                                             TRUE ~ ratioagecatA_vec_mean),
          under1 = ifelse((ratioagecatA_vec_95lb > 1 & is.na(ratioagecatA_vec_mean) == F), "*", ""),
          HHS_region = NA_real_) %>%
#   replace(is.na(.), 0) %>%
   mutate(Race = case_when(Race == "All_races" ~ "All races",
                           Race == "NH_Americanindian" ~ "NH AIAN",
                           Race == "NH_White" ~ "NH White",
                           Race == "NH_Asian" ~ "NH Asian",
                           Race == "NH_Black" ~ "NH Black",
                           Race == "NH_morethanonerace" ~ "NH 2+ races",
                           Race == "NH_Hawaiian" ~ "NH NHPI", 
                           Race == "Hispanic" ~ "Hispanic"),
          HHS_region = case_when(
             # HHS Region 1 
             State == "Connecticut" ~ 1, State == "Maine" ~ 1, State == "Massachusetts" ~ 1, State == "New Hampshire" ~ 1, State == "Rhode Island" ~ 1, State == "Vermont" ~ 1,
             # HHS Region 2
             State == "New Jersey" ~ 2, State == "New York" ~ 2,
             # HHS Region 3 
             State == "Delaware" ~ 3, State == "District of Columbia" ~ 3, State == "Maryland" ~ 3, State == "Pennsylvania" ~ 3, State == "Virginia" ~ 3, State == "West Virginia" ~ 3,
             # HHS Region 4
             State == "Alabama" ~ 4, State == "Florida" ~ 4, State == "Georgia" ~ 4, State == "Kentucky" ~ 4, State == "Mississippi" ~ 4, State == "North Carolina" ~ 4, State == "South Carolina" ~ 4, State == "Tennessee" ~ 4,
             # HHS Region 5 
             State == "Illinois" ~ 5, State == "Indiana" ~ 5, State == "Michigan" ~ 5, State == "Minnesota" ~ 5, State == "Ohio" ~ 5, State == "Wisconsin" ~ 5,
             # HHS Region 6 
             State == "Arkansas" ~ 6, State == "Louisiana" ~ 6, State == "New Mexico" ~ 6, State == "Oklahoma" ~ 6, State == "Texas" ~ 6,
             # HHS Region 7 
             State == "Iowa" ~ 7, State == "Kansas" ~ 7, State == "Missouri" ~ 7, State == "Nebraska" ~ 7,
             # HHS Region 8 
             State == "Colorado" ~ 8, State == "Montana" ~ 8, State == "North Dakota" ~ 8, State == "South Dakota" ~ 8, State == "Utah" ~ 8, State == "Wyoming" ~ 8,
             # HHS Region 9 
             State == "Arizona" ~ 9, State == "California" ~ 9, State == "Hawaii" ~ 9, State == "Nevada" ~ 9,
             # HHS Region 10
             State == "Alaska" ~ 10, State == "Idaho" ~ 10, State == "Oregon" ~ 10, State == "Washington" ~ 10,
             TRUE ~ NA_real_ ))
DF_States2A_heatmat_vec

ggplot(DF_States2A_heatmat_vec, aes(factor(Race, level = order_race_shorter), State)) +
   geom_tile(aes(fill= ratioagecatA_vec_mean)) +
   geom_text(aes(label = under1, vjust = 0.7)) +
   scale_fill_viridis(option = "viridis", # "plasma@
                      direction = -1,
                      values = scales::rescale(c(0, 0.99, 1, 10, 21)),
                      limits = c(0, 21),
                      breaks = c(0, 1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
                      na.value = 'grey70', name = "Ratio L") +
   scale_y_discrete(expand=c(0,0), limits=rev) +
   scale_x_discrete(expand=c(0,0)) +
   theme(legend.key.height= unit(1.1, 'cm'),
         legend.key.width= unit(0.6, 'cm'),
         axis.text.x = element_text(angle = 45, hjust = 1),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank()) +
   xlab("Race/ethnicity categories") +
   ylab("State and HHS Region") +
   facet_grid("HHS_region", scales = "free", space = "free", switch = "y") +
   theme(strip.text.y.left = element_text(angle = 0)) +
   theme(panel.spacing = unit(0.1, "lines")) +
   theme(text=element_text(size=10, face = "bold")) #,  family="Times New Roman"

dev.copy2pdf(file = "figures/Figure1.pdf",
             useDingbats=FALSE,
             width = 6.5, height = 6)


## Figure 2
DF_States3A_heatmat_vec <- TableS3A_vec %>%
   select(c(State, Race, ratioYLL_vec_mean, ratioYLL_vec_95lb)) %>% 
   mutate(ratioYLL_vec_mean2 = case_when(is.na(ratioYLL_vec_mean) ~ 0,
                                             TRUE ~ ratioYLL_vec_mean),
          under1 = ifelse((ratioYLL_vec_95lb > 1 & is.na(ratioYLL_vec_mean2) == F), "*", ""),
          HHS_region = NA_real_) %>%
   #   replace(is.na(.), 0) %>%
   mutate(Race = case_when(Race == "All_races" ~ "All races",
                           Race == "NH_Americanindian" ~ "NH AIAN",
                           Race == "NH_White" ~ "NH White",
                           Race == "NH_Asian" ~ "NH Asian",
                           Race == "NH_Black" ~ "NH Black",
                           Race == "NH_morethanonerace" ~ "NH 2+ races",
                           Race == "NH_Hawaiian" ~ "NH NHPI", 
                           Race == "Hispanic" ~ "Hispanic"),
          HHS_region = case_when(
             # HHS Region 1 
             State == "Connecticut" ~ 1, State == "Maine" ~ 1, State == "Massachusetts" ~ 1, State == "New Hampshire" ~ 1, State == "Rhode Island" ~ 1, State == "Vermont" ~ 1,
             # HHS Region 2
             State == "New Jersey" ~ 2, State == "New York" ~ 2,
             # HHS Region 3 
             State == "Delaware" ~ 3, State == "District of Columbia" ~ 3, State == "Maryland" ~ 3, State == "Pennsylvania" ~ 3, State == "Virginia" ~ 3, State == "West Virginia" ~ 3,
             # HHS Region 4
             State == "Alabama" ~ 4, State == "Florida" ~ 4, State == "Georgia" ~ 4, State == "Kentucky" ~ 4, State == "Mississippi" ~ 4, State == "North Carolina" ~ 4, State == "South Carolina" ~ 4, State == "Tennessee" ~ 4,
             # HHS Region 5 
             State == "Illinois" ~ 5, State == "Indiana" ~ 5, State == "Michigan" ~ 5, State == "Minnesota" ~ 5, State == "Ohio" ~ 5, State == "Wisconsin" ~ 5,
             # HHS Region 6 
             State == "Arkansas" ~ 6, State == "Louisiana" ~ 6, State == "New Mexico" ~ 6, State == "Oklahoma" ~ 6, State == "Texas" ~ 6,
             # HHS Region 7 
             State == "Iowa" ~ 7, State == "Kansas" ~ 7, State == "Missouri" ~ 7, State == "Nebraska" ~ 7,
             # HHS Region 8 
             State == "Colorado" ~ 8, State == "Montana" ~ 8, State == "North Dakota" ~ 8, State == "South Dakota" ~ 8, State == "Utah" ~ 8, State == "Wyoming" ~ 8,
             # HHS Region 9 
             State == "Arizona" ~ 9, State == "California" ~ 9, State == "Hawaii" ~ 9, State == "Nevada" ~ 9,
             # HHS Region 10
             State == "Alaska" ~ 10, State == "Idaho" ~ 10, State == "Oregon" ~ 10, State == "Washington" ~ 10,
             TRUE ~ NA_real_ ))
DF_States3A_heatmat_vec

ggplot(DF_States3A_heatmat_vec, aes(factor(Race, level = order_race_shorter), State)) +
   geom_tile(aes(fill= ratioYLL_vec_mean)) +
   geom_text(aes(label = under1, vjust = 0.7)) +
   scale_fill_viridis(option = "plasma",
                      direction = -1,
                      values = scales::rescale(c(0, 0.99, 1, 5.5, 12)),
                      limits = c(0, 12),
                      breaks = c(0, 1, 2, 4, 6, 8, 10, 12),
                      na.value = 'grey70', name = "Ratio Y") +
   scale_y_discrete(expand=c(0,0), limits=rev) +
   scale_x_discrete(expand=c(0,0)) +
   theme(legend.key.height= unit(1.1, 'cm'),
         legend.key.width= unit(0.6, 'cm'),
         axis.text.x = element_text(angle = 45, hjust = 1),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank()) +
   xlab("Race/ethnicity categories") +
   ylab("State and HHS Region") +
   facet_grid("HHS_region", scales = "free", space = "free", switch = "y") +
   theme(strip.text.y.left = element_text(angle = 0)) +
   theme(panel.spacing = unit(0.1, "lines")) +
   theme(text=element_text(size=10, face = "bold")) #,  family="Times New Roman"

dev.copy2pdf(file = "figures/Figure2.pdf",
             useDingbats=FALSE,
             width = 6.5, height = 6)
# min(DF_States3A_heatmat_vec$ratioYLL_vec_mean, na.rm = T)
# max(DF_States3A_heatmat_vec$ratioYLL_vec_mean, na.rm = T)



###########################
### Additional Supplementary Tables and Figures :

## Supplementary Table 1 : Front-line workers 
# Table professions included in flw definition + proportion of each in total and among each race.
# Make a table showing for each racial category the proportion of flw in each flw job
# For the whole of the US.
Table_flw_prop_all <- pums_agerace_flw %>%
   filter(AGEP %in%  c(16:64)) %>%
   count(OCCP_label, wt = PWGTP) %>%
   mutate(prop = n / sum(n),
          prop = prop * 100) %>%
   select(-n) %>%
   arrange(-prop)
Table_flw_prop_all

Table_flw_prop_races <- pums_agerace_flw %>%
   filter(AGEP %in%  c(16:64)) %>%
   group_by(Race) %>%
   count(OCCP_label, wt = PWGTP) %>%
   mutate(prop = n / sum(n),
          prop = prop * 100) %>%
   select(-n) %>%
   pivot_wider(names_from = Race, values_from = prop)
Table_flw_prop_races

Table_flw_prop <- merge(Table_flw_prop_all, Table_flw_prop_races, by=c("OCCP_label")) %>%
   arrange(-prop) %>%
   rename("Occupation" = "OCCP_label",
          "All races"= "prop",
          "Non-Hispanic Asian" = "NH_Asian",
          "Non-Hispanic Black" = "NH_Black",
          "Non-Hispanic White" = "NH_White")  %>%
   mutate(Category = Occupation ) %>%
   relocate(Category, .after = Occupation) %>%
   mutate_if(is.numeric, round, digits = 2)
Table_flw_prop$Occupation <- Table_flw_prop$Occupation %>%
   gsub('^.{4}', '', .)
Table_flw_prop$Category <- Table_flw_prop$Category %>%
   substr(., 1, 3) %>%
   str_replace_all(c("SAL" = "Sales", # "Sales and related occupations"
                     "EDU" = "Education",
                     "EAT" = "Food industry", # "Food preparation and serving related occupations"
                     "TRN" = "Transportation",
                     "PRD" = "Food industry",
                     "PRS" = "Funeral workers"))
Table_flw_prop <- Table_flw_prop[, c(1, 2, 3, 10 ,4, 7, 6, 9, 5, 8)]
head(Table_flw_prop)
Table_flw_prop
Table_flw_prop %>%
   kable(. ,"latex")

## Supplementaty Figure 2 : Age distribution of flw 1664 vs all adults 1664
fig_US_age <- pums_agerace4 %>%
   filter(AGEP %in%  c(16:64)) %>%
   mutate(AGEcat = ifelse(AGEP %in% 16:24, 1624,
                          ifelse(AGEP %in% 25:34, 2534,
                                 ifelse(AGEP %in% 35:44, 3544, 
                                        ifelse(AGEP %in% 45:54, 4554,
                                               ifelse(AGEP %in% 55:64, 5564,"F")))))) %>%
   count(AGEcat, wt = PWGTP) %>%
   mutate(prop = n / sum(n)) %>%
   add_column("who" =  "Adults aged 16-64")
fig_US_age$AGEcat <- fig_US_age$AGEcat %>%
   gsub('(^.{2})', '\\1-\\2', .) 

fig_flw_US_age <- pums_agerace_flw %>%
   filter(AGEP %in%  c(16:64)) %>%
   mutate(AGEcat = ifelse(AGEP %in% 16:24, 1624,
                          ifelse(AGEP %in% 25:34, 2534,
                                 ifelse(AGEP %in% 35:44, 3544, 
                                        ifelse(AGEP %in% 45:54, 4554,
                                               ifelse(AGEP %in% 55:64, 5564,"F")))))) %>%
   count(AGEcat, wt = PWGTP) %>%
   mutate(prop = n / sum(n))  %>%
   add_column("who" = "Front-line workers aged 16-64")
fig_flw_US_age$AGEcat <- fig_flw_US_age$AGEcat %>%
   gsub('(^.{2})', '\\1-\\2', .) 

fig_flw_US_age
fig_US_age
sum(fig_US_age$prop); sum(fig_flw_US_age$prop)

# table
table_US_age <- fig_US_age %>%
   rename("Age category" = AGEcat,
          "Adults" = prop) %>%
   select(-n, -who)
table_flw_US_age <- fig_flw_US_age %>%
   rename("Age category" = AGEcat,
          "Front-line workers" = prop) %>%
   select(-n, -who)
table_US_age
table_flw_US_age
Table_age <- merge(table_US_age, table_flw_US_age, by=c("Age category")) %>%
   mutate_if(is.numeric, round, digits = 3)

Table_age
Table_age %>%
   kable(. ,"latex")

# plot
ggplot(rbind(fig_US_age, fig_flw_US_age), aes(x = AGEcat, weight = prop, fill = who)) +
   geom_bar(position = position_dodge(preserve = "single")) +
   theme_minimal() +
   labs(title = "Age breakdown of (a) adults aged 16-64, shown in red;\nand (b) front-line workers aged 16-64, shown in blue",
        x = "Age categories",
        y = "Proportion") +
   theme(legend.title = element_blank()) +
   scale_fill_manual(values=c("#d53e4f", "#3288bd")) #c("#d73027","#4575b4")) #c("#b2182b", "#2166ac"))  #c("#e41a1c", "#377eb8"))

dev.copy2pdf(file = "figures/SupplFigure2.pdf",
             useDingbats=FALSE,
             width = 7, height = 4)


## Supplementaty Figure 1 : Race distribution of flw 1664 vs all adults 1664 vs  proportion of 6574
fig_US_race <- pums_agerace4 %>%
   filter(AGEP %in%  c(16:64)) %>%
   count(Race, wt = PWGTP) %>%
   mutate(prop = n / sum(n)) %>%
   add_column("who" =  "Adults aged 16-64")
fig_flw_US_race <- pums_agerace_flw %>%
   filter(AGEP %in%  c(16:64)) %>%
   count(Race, wt = PWGTP) %>%
   mutate(prop = n / sum(n))  %>%
   add_column("who" = "Front-line workers aged 16-64")
fig_US_race
fig_flw_US_race
sum(fig_US_race$prop); sum(fig_flw_US_race$prop)

# table
table_US_race <- fig_US_race %>%
   rename("Adults" = prop) %>%
   select(-n, -who)
table_flw_US_race <- fig_flw_US_race %>%
   rename("Front-line workers" = prop) %>%
   select(-n, -who)
table_US_race
table_flw_US_race
# Table_race <- merge(table_US_race, table_flw_US_race, by=c("Race")) %>%
#    mutate_if(is.numeric, round, digits = 3)
# Table_race
# Table_race %>%
#    kable(. ,"latex")

fig_6574_US_race <- pums_agerace_flw %>%
   filter(AGEP %in%  c(65:74)) %>%
   count(Race, wt = PWGTP) %>%
   mutate(prop = n / sum(n))  %>%
   add_column("who" = "Older adults aged 65-74")
fig_6574_US_race
sum(fig_6574_US_race$prop)

table_6574_US_race <- fig_6574_US_race %>%
   rename("65-74" = prop) %>%
   select(-n, -who)
table_6574_US_race

Table_race2 <- merge(table_US_race, table_flw_US_race, by=c("Race")) %>%
   merge(., table_6574_US_race, by=c("Race")) %>%
   mutate_if(is.numeric, round, digits = 3)
Table_race2

# plot 
SupplFig_proportions <- full_join(full_join(fig_US_race, fig_flw_US_race), fig_6574_US_race)
SupplFig_proportions$Race[SupplFig_proportions$Race == "NH_White"] <- "NH White"
SupplFig_proportions$Race[SupplFig_proportions$Race == "NH_Black"] <- "NH Black"
SupplFig_proportions$Race[SupplFig_proportions$Race == "NH_Asian"] <-"NH Asian"
SupplFig_proportions$Race[SupplFig_proportions$Race == "NH_Americanindian"] <- "NH AIAN"
SupplFig_proportions$Race[SupplFig_proportions$Race == "NH_morethanonerace"] <- "NH 2+ races"
SupplFig_proportions$Race[SupplFig_proportions$Race == "NH_Hawaiian"] <- "NH NHPI"
SupplFig_proportions$Race <- factor(SupplFig_proportions$Race,levels = c("NH White", "Hispanic", "NH Black","NH Asian", "NH 2+ races", "NH AIAN", "NH NHPI"))
SupplFig_proportions

SupplFig_proportions$who[SupplFig_proportions$who == "Older adults aged 65-74"] <- "Adults aged 65-74"

ggplot(SupplFig_proportions, aes(x = Race, weight = prop, fill = who )) +
   geom_bar(position = position_dodge(preserve = "single")) +
   theme_minimal() +
   labs(title = "Race/ethnicity breakdown of (a) adults aged 16-64, shown in red; (b) adults aged\n65-74, shown in yellow; and (c) front-line workers aged 16-64, shown in blue",
        x = "Race/ethnicity categories",
        y = "Proportion") +
   theme(legend.title = element_blank()) +
   scale_fill_manual(values=c("#d53e4f", "yellow", "#3288bd")) #c("#d73027","#4575b4")) #c("#b2182b", "#2166ac"))  #c("#e41a1c", "#377eb8"))

dev.copy2pdf(file = "figures/SupplFigure1.pdf",
             useDingbats=FALSE,
             width = 7, height = 4)


