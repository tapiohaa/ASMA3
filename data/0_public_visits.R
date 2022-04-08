
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 0_public_visits.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Since 2019, the Avohilmo registry also contains
#           private outpatient care which we want to be excluded from
#           this study.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(readxl)           # Reading xlsx files.

# Inputs:
input_munies = "W:/ASMA3/data/raw/sote_munies_2021.csv"
input_topi = "W:/ASMA3/data/raw/topi_unit_register.csv"
input_sote = "W:/ASMA3/data/raw/sote_organisation_register.xlsx"
input_visits = "W:/ASMA3/data/interim/visits_" # visits_XX.csv, XX in 19:20

# Outputs:
output_visits = "W:/ASMA3/data/interim/visits_public_" # visits_public_XX.rds, XX in 19:20


###
###


# First, read the 2022 municipalities in mainland Finland.
munies = data.table::fread(
  input_munies, encoding = 'UTF-8',
  select = c('Kunta-koodi', 'Kunta', 'Terveyskeskus',
             'Asukasluku 31.12.2019') 
)
colnames(munies) = c('municipality_code', 'name', 'primary_care_area',
                     'population')

# Municipalities on the Åland Islands:
to_drop = c(478, 60, 65, 76, 170, 736, 771, 43, 417,
            438, 35, 62, 295, 318, 766, 941)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read TOPI unit register and extract health stations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

topi = data.table::fread(
  input_topi, encoding='UTF-8',
  select = c('topi_code', 'topi_code_addition', 'name', 'year', 
             'service_area_code', 'municipality_code')
)

# Take the service area codes that are related to health centers
# (120, 121, and 122):
topi = topi[grepl('120', service_area_code, fixed = TRUE) |
              grepl('121', service_area_code, fixed = TRUE) |
              grepl('122', service_area_code, fixed = TRUE), 
            topi_pc := 1]

# Take years from 2019 and drop providers from the Åland Islands:
topi =  topi[year >= 2019 & !(municipality_code %in% to_drop)]


# When extracting the primary care units, we observe 257 and 258 municipalities 
# out of 293. Not every municipality has a health station or a health center, 
# but let's examine this closer.
topi[topi_pc==1, .(munies = length(unique(municipality_code))), by='year']

DT = setdiff(unique(munies$municipality_code), 
             unique(topi[topi_pc==1]$municipality_code))

test = munies[municipality_code %in% DT]

# CONCLUSION: It turns out that the municipalities that are not observed to
# have units with service area codes 120:122 are a) small and b) all part
# of a larger primary care area with many municipalities. Thus, it is not
# surprising that they are not observed.


# Same topi_codes appear in multiple rows. Take only unique combinations:

topi = topi[, mget(c('topi_code', 'year', 'topi_pc'))]
topi = unique(topi)

topi = topi[, .(topi_pc = sum(topi_pc, na.rm=TRUE)), 
            by=c('topi_code', 'year')
            ][, topi_pc := as.integer(topi_pc > 0)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read SOTE organization register and extract public sector units. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Read data on social and healthcare organizations:
sote = setDT(readxl::read_excel(input_sote))

# Extract relevant columns:
cols = c('Tunniste', 'Org.Yks.lyhenne', 
         'Hierarkiataso', 'Sektori', 'Sijainti kunta')
sote = sote[, mget(cols)]
setnames(sote, old=cols, new=c('sote_code', 'name', 
                               'hierarchy', 'sector', 'municipality'))

# Tidy the sector column:
sote = sote[, sector := as.integer(substr(sector, 1, 1))]

# For merging, keep only sote_code and sector:
sote = sote[, mget(c('sote_code', 'sector'))]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Primary care visits, starting from 2019. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

years = c(19:20)

visits = lapply(years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_visits, yr, ".csv", sep="")
  df = data.table::fread(source)
  
  # Link topi and visits via topi_code:
  topi_yr = topi[year == 2000 + yr][, year := NULL]
  df = merge(df, topi_yr, by='topi_code', all.x = TRUE)
  
  # Link sote and visits via sote_code:
  df = merge(df, sote, by='sote_code', all.x = TRUE)
  
  df[topi_code=='', topi_code := NA_character_]
  df[sote_code=='', sote_code := NA_character_]
  
  return(df)
  
})
visits = do.call(rbind.data.frame, visits)
visits = visits[, mget(colnames(visits))]


# All rows contain topi_code, but in 4% of the rows the linking did not work.
# 0.04% of the rows do not contain sote_code, and in 1% of the rows the
# linking did not work.
colMeans(is.na(visits))
visits[, ':=' (sote_code = NULL, topi_code = NULL)]
visits[, .N, by=c('year', 'topi_pc')]
visits[, .N, by=c('year', 'sector')]


# Create an indicator for the unit being a health station or a public provider.
visits[is.na(topi_pc), topi_pc := -1]
visits[is.na(sector), sector := -1]
visits[, pc_or_public := as.integer(topi_pc == 1 | sector == 1)]


# Clearly, private clinics started to provide data on outpatient contacts
# in 2020:
summary(visits[year==2019]$pc_or_public) 
summary(visits[year==2020]$pc_or_public) 


# Keep those visits where the provider was either a health center (TOPI) or
# a public sector organisation (SOTE):
visits = visits[pc_or_public==1
                ][, ':=' (topi_pc = NULL,
                          sector = NULL, 
                          pc_or_public = NULL)]

# Save by year:
lapply(years, function(yr) {
  
  print(yr)
  visits_yr = visits[year == 2000 + yr]
  source = paste(output_visits, yr, ".rds", sep="")
  saveRDS(visits_yr, file=source)
  
})

# End.
