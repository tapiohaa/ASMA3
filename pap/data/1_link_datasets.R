
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_link_datasets.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Merge visits and sosioeconomic data.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_folk = 
  "W:/ASMA3/data/interim/folk_data_20" # folk_data_20XX.csv, XX in 12:19
input_visits = "W:/ASMA3/data/interim/visits_" # visits_XX.csv, XX in 12:18
input_visits_late = "W:/ASMA3/data/interim/visits_public_" # visits_public_XX.csv, XX in 19:20
input_refs = "W:/ASMA3/data/interim/referrals_20" # referrals_XX.csv, XX in 12:20
input_drugs = "W:/ASMA3/data/interim/prescriptions_" # prescriptions_X_YY.csv, X in 1:2 and YY in 2018:2020

# Outputs:
output_visits = "W:/ASMA3/data/interim/visit_data_aggr.rds"


###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read FOLK data and tidy data on social assistance. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

years_folk = c(12:20)

folk = lapply(years_folk, function(yr) {
  print(yr)
  
  # Currently, we use data from 2019 as a replacement for 2020 as the
  # actual data for 2020 is not ready yet:
  if(yr==20) { 
    yr_input = 19
  } else { yr_input = yr}
  
  source = paste(input_folk, yr_input, ".csv", sep="")
  df = data.table::fread(source)
  df[, year := 2000 + yr]
})
folk = do.call(rbind.data.frame, folk)


# Tidy data on social assistance:

assistance = folk[year %in% c(2012:2018), 
                  mget(c('year', 'id', 'jan', 'feb', 'mar', 'apr', 'may', 
                         'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec'))]

folk = folk[, mget(setdiff(colnames(folk), 
                           c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                             'july', 'aug', 'sep', 'oct', 'nov', 'dec')))]
folk[is.na(basic_social_assistance), basic_social_assistance := 0]

# Pivot longer:
assistance = melt(
  assistance, id.vars = c('id', 'year'),
  measure.vars = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                   'july', 'aug', 'sep', 'oct', 'nov', 'dec'),
  variable.name = 'month', value.name = 'social_assistance',
  variable.factor = FALSE
)

# To save memory, drop (for now) rows where social assistance is not received:
assistance = assistance[social_assistance > 0]

# Currently, social_assistance contains the number of social
# assistance applicants who received social assistance (may be >1). Next,
# we mutate these values to dummies whether the family received the benefit.
assistance[social_assistance > 1, social_assistance := 1]

# Replace month abbreviations with dates:

months = data.table(month_no = c(1:12),
                    month = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                              'july', 'aug', 'sep', 'oct', 'nov', 'dec'))

assistance = merge(assistance, months, by='month', all.x = TRUE)
assistance[, month := NULL]
setnames(assistance, old='month_no', new='month')


# Let's return to the folk data:

# No missing values.
colMeans(is.na(folk))

# Percentage of rows where the family equivalized income is exactly zero:
100 * nrow(folk[income_eq==0]) / nrow(folk) # %

# We drop individuals with family equivalized income exactly 
# zero (missing data):
folk = folk[income_eq > 0]

# Keep only those aged 25 or more (at the end of year):
folk = folk[age >= 25]

# Create variable income_decile (by year):
folk[, income_decile := cut(income_eq,
                            breaks = quantile(income_eq, probs = 0:10/10),
                            labels = 0:9, right = FALSE), by='year']
folk = folk[!is.na(income_decile)][, income_eq := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Compute population size in each subgroup. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

cols = 'income_decile'

population = lapply(cols, function(x) {
  
  data = folk[, .(population = .N), by=c('municipality', 'year', x)
              ][, dimension := x]
  setnames(data, old=x, new='group')
  
  return(data)
})
population = do.call(rbind.data.frame, population)

# Population sizes by year:
test = population[, .(population = sum(population)), by=c('year','dimension')
                  ][order(year)]
test


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Read GP visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

years_pc = c(12:20)

visits = lapply(years_pc, function(yr) {
  print(yr)
  
  # Read visits:
  
  if(yr >= 19) {
    source = paste(input_visits_late, yr, ".rds", sep="")
    df = setDT(readRDS(source))
  } else {
    source = paste(input_visits, yr, ".csv", sep="")
    df = data.table::fread(source)
  }
  
  # Aggregate at the ID-month-profession-curative-level:
  df = df[, .(contacts = .N), by=c('id', 'month', 'year', 
                                   'profession', 'curative')]
  
})
visits = do.call(rbind.data.frame, visits)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Merge GP visits and FOLK data and aggregate. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Merge:
visits = merge(visits, folk, on=c('id','year'), all.x = TRUE)
print("Merge is done")

# Approximately 21% of the rows are dropped because the ID cannot 
# be merged to FOLK data (we took those aged 25 or more).
colMeans(is.na(visits))
visits = visits[!is.na(municipality)]


# Aggregate contacts at the municipality-month-year-subgroup-profession-curative
# level:

cols = 'income_decile'

df_visits = lapply(cols, function(x) {
  
  data = visits[, .(contacts = sum(contacts)), 
                by=c('municipality', 'year', 'month', 
                     'profession', 'curative', x)
                ][, dimension := x]
  setnames(data, old=x, new='group')
  
  return(data)
})
df_visits = do.call(rbind.data.frame, df_visits)

# Show the number of contacts by year:
test = df_visits[, .(contacts = sum(contacts)), by=c('year','dimension')
                 ][order(year)]
test

rm(visits)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Read referrals to specialized healthcare. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We observed that almost all referrals contain time stamp for the start of
# the contact. To put it differently, we observe only those referrals that
# have led to an actual contact. Similarly, we observe that some of the
# contacts have not ended, especially when we approach the date of data 
# extraction (January 2022). It does not matter in our analyses.

# We also observed that the coding rate of where the referral was written
# (e.g. publicly-funded primary care) was not that close to 1 in 2012-2018.
# Therefore, we include all referrals (even if many of them were written
# in private primary care).

years_refs = c(12:20)

refs = lapply(years_refs, function(year) {
  print(year)
  
  # Read visits:
  source = paste(input_refs, year, ".csv", sep="")
  df = data.table::fread(source, drop=c('discharged', 'ref_origin'))
  
  # Take those rows where we observe the contact start date:
  df = df[contact_started==1]
  
  # Take unique id-date pairs. More than one referrals could be written
  # on each day, but in our analyses we create a dummy on whether an 
  # individual X received any referrals on date Y:
  df = unique(df)
  
  # Aggregate at the ID-month level:
  df = df[, .(contacts = .N), by=c('id', 'month', 'year')]
  
})
refs = do.call(rbind.data.frame, refs)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Merge referrals and FOLK data and aggregate. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Merge:
refs = merge(refs, folk, on=c('id','year'), all.x = TRUE)
print("Merge is done")

# Approximately 22% of the rows are dropped because the ID cannot 
# be merged to FOLK data (we took those aged 25 or more).
colMeans(is.na(refs))
refs = refs[!is.na(municipality)]


# Aggregate contacts at the municipality-month-year-subgroup level:

cols = 'income_decile'

df_refs = lapply(cols, function(x) {
  
  data = refs[, .(contacts = sum(contacts)), by=c('municipality', 'year', 
                                                  'month', x)
              ][, dimension := x]
  setnames(data, old=x, new='group')
  
  return(data)
})
df_refs = do.call(rbind.data.frame, df_refs)


# We will separate referrals from primary care nurse and GP visits by giving
# it profession==3:
df_refs = df_refs[, mget(colnames(df_refs))]
df_refs[, profession := 3]

# Show the number of contacts by year:
test = df_refs[, .(contacts = sum(contacts)), by=c('year','dimension')
               ][order(year)]
test

rm(refs)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Read drug prescriptions. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

years_drugs = c(2018:2020)

# Loop over study years:
drugs = lapply(years_drugs, function(yr) {
  
  print(yr)
  
  # Loop over subsets of data:
  drugs.year = lapply(1:2, function(subset) {
    
    # Read visits:
    source = paste(input_drugs, subset, '_', yr, ".csv", sep="")
    df = data.table::fread(source)
    
    # Tidy column names:
    colnames(df) = tolower(colnames(df))
    setnames(df, old='shnro', new='id')
    
    # Create variables for month and year:
    df[, ':=' (year = as.integer(format(date_pk, format="%Y")),
               month = as.integer(format(date_pk, format="%m")))]
    
  })
  drugs.year = do.call(rbind.data.frame, drugs.year)
  
  # Aggregate:
  DT = drugs.year[, .(prescriptions = .N), 
                  by=c('id', 'year', 'month', 'sector', 'med_init_code')]
  
})
drugs = do.call(rbind.data.frame, drugs)

# Both med_init_code and sector are coded in almost every row:
colMeans(is.na(drugs))

# Drop rows where the sector is not observed:
drugs = drugs[!is.na(sector)]

# Take only prescriptions written by the public sector. This excludes 
# prescriptions that were written at health centers that were run 
# by private firms as a results of outsourcing. We will later handle these
# issues in 1_visits_quality.R.
drugs = drugs[sector==1][, sector := NULL]


# Compute the sum of drug initiations and the sum of all prescriptions
# and concatenate to a table:

# We will use profession=41 for drug therapy initializations, and 
# profession=40 for all prescriptions.

drugs_init = drugs[med_init_code == 1
                   ][, ':=' (med_init_code = NULL,
                             profession = 41)]

drugs = drugs[, .(prescriptions = sum(prescriptions)),
              by=c('id', 'year', 'month')
              ][, profession := 40]

# Concatenate:
drugs = rbind(drugs, drugs_init)
setnames(drugs, old='prescriptions', new='contacts')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Merge prescriptions and FOLK data and aggregate. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Merge:
drugs = merge(drugs, folk, on=c('id','year'), all.x = TRUE)
print("Merge is done")

# Approximately 16% of the rows are dropped because the ID cannot 
# be merged to FOLK data (we took those aged 25 or more).
colMeans(is.na(drugs))
drugs = drugs[!is.na(municipality)]


# Aggregate contacts at the municipality-month-year-subgroup level:

cols = 'income_decile'

df_drugs = lapply(cols, function(x) {
  
  data = drugs[, .(contacts = sum(contacts)), by=c('municipality', 'year', 
                                                   'month', 'profession', x)
               ][, dimension := x]
  setnames(data, old=x, new='group')
  
  return(data)
})
df_drugs = do.call(rbind.data.frame, df_drugs)


# Show the number of contacts by year:
test = df_drugs[profession==40, .(contacts = sum(contacts)), 
                by=c('year','dimension')
                ][order(year)]
test

rm(drugs, drugs_init)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 8) Merge data on social assistance and FOLK data and aggregate. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# First, aggregate the sum of basic social assistance (in euros)
# to the municipality-year-subgroup level:

cols = 'income_decile'

df_benefit_yr = lapply(cols, function(x) {
  
  data = folk[, .(basic_social_assistance = sum(basic_social_assistance,
                                                na.rm = TRUE)), 
              by=c('municipality', 'year', x)
              ][, dimension := x]
  setnames(data, old=x, new='group')
  
  return(data)
})
df_benefit_yr = do.call(rbind.data.frame, df_benefit_yr)

# Check that the panel is balanced:
nrow(df_benefit_yr)
length(unique(df_benefit_yr$municipality)) *
  length(unique(df_benefit_yr$year)) * 10

assistance_years = c(2012:2018)
df_benefit_yr = df_benefit_yr[year %in% assistance_years]


# Next, we merge FOLK data to the assistance months table:
assistance = merge(assistance, folk, on=c('id','year'), all.x = TRUE)
print("Merge is done")

# Approximately 41% of the rows are dropped because the ID cannot 
# be merged to FOLK data (we took only those aged 25 or more). This is not
# surprising: students and single-parent families with many children are 
# overrepresented among social assistance recipients.
colMeans(is.na(assistance))
assistance = assistance[!is.na(municipality)]


# Aggregate at the municipality-month-year-subgroup level:

cols = 'income_decile'

df_benefit = lapply(cols, function(x) {
  
  data = assistance[, .(social_assistance = sum(social_assistance)), 
                    by=c('municipality', 'year', 'month', x)
                    ][, dimension := x]
  setnames(data, old=x, new='group')
  
  return(data)
})
df_benefit = do.call(rbind.data.frame, df_benefit)

# Show the number of social assistance recipients by year:
test = df_benefit[, .(social_assistance = sum(social_assistance)), 
                  by=c('year','dimension')
                  ][order(year)]
test

rm(assistance)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 9) Expand datasets to create panels. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# df_visits:

# Make sure each municipality-month-year-profession-curative-subgroup has a 
# cell in the data. That is, expand the dataset:

data_visits = lapply(cols, function(x) {
  
  # Subset the data:
  data = df_visits[dimension == x]
  
  # Expand it:
  setkey(setDT(data), municipality, year, month, group, 
         dimension, profession, curative)
  combs = data[, CJ( unique(municipality), unique(year), unique(month), 
                     unique(group), unique(dimension), 
                     unique(profession), unique(curative))]
  data = data[combs][is.na(contacts), contacts := 0]
  setkey(data, NULL)
  
  return(data)
  
  
})
data_visits = do.call(rbind.data.frame, data_visits)


# df_refs:

# Make sure each municipality-month-year-profession-subgroup has a 
# cell in the data. That is, expand the dataset:

data_refs = lapply(cols, function(x) {
  
  # Subset the data:
  data = df_refs[dimension == x]
  
  # Expand it:
  setkey(setDT(data), municipality, year, month, group, 
         dimension, profession)
  combs = data[, CJ( unique(municipality), unique(year), unique(month), 
                     unique(group), unique(dimension), 
                     unique(profession))]
  data = data[combs][is.na(contacts), contacts := 0]
  setkey(data, NULL)
  
  return(data)
  
  
})
data_refs = do.call(rbind.data.frame, data_refs)


# df_drugs:

# Make sure each municipality-month-year-profession-subgroup has a 
# cell in the data. That is, expand the dataset:

data_drugs = lapply(cols, function(x) {
  
  # Subset the data:
  data = df_drugs[dimension == x]
  
  # Expand it:
  setkey(setDT(data), municipality, year, month, group, 
         dimension, profession)
  combs = data[, CJ( unique(municipality), unique(year), unique(month), 
                     unique(group), unique(dimension), 
                     unique(profession))]
  data = data[combs][is.na(contacts), contacts := 0]
  setkey(data, NULL)
  
  return(data)
  
  
})
data_drugs = do.call(rbind.data.frame, data_drugs)


# df_benefits:

# Make sure each municipality-month-year-subgroup has a 
# cell in the data. That is, expand the dataset:

data_benefit = lapply(cols, function(x) {
  
  # Subset the data:
  data = df_benefit[dimension == x]
  
  # Expand it:
  setkey(setDT(data), municipality, year, month, group, dimension)
  combs = data[, CJ( unique(municipality), unique(year), unique(month), 
                     unique(group), unique(dimension))]
  data = data[combs
              ][is.na(social_assistance), social_assistance := 0]
  setkey(data, NULL)
  
  return(data)
  
  
})
data_benefit = do.call(rbind.data.frame, data_benefit)


# Concatenate data_visits, data_referrals, data_drugs, and data_benefit, and
# df_benefit_yr:

data_visits = data_visits[, mget(colnames(data_visits))]
data_refs = data_refs[, mget(colnames(data_refs))]
data_drugs = data_drugs[, mget(colnames(data_drugs))]
data_benefit = data_benefit[, mget(colnames(data_benefit))]

data = rbind(data_visits, data_refs, data_drugs, data_benefit,
             df_benefit_yr, fill=TRUE)


# Merge population sizes to the 'data':
data = merge(data, population, by=c('municipality', 'year', 'dimension', 
                                    'group'), all.x = TRUE)
data[is.na(population), population := 0]

# Add annualized contacts per capita:
data[, contacts_per_capita := 12 * contacts / population]

# Add a date variable for the start of each month:
data[, date := as.Date(paste(as.character(year), 
                             as.character(month), '01', sep='-'))]

# Save:
saveRDS(data, output_visits)

# End.
