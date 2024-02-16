
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 1_hospital_visits.R        ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Clean the data on emergency department visits and inpatient spells.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_diagnoses = "W:/ASMA3/data/interim/hospital_diagnoses.csv"
input_contacts = "W:/ASMA3/data/interim/hospital_visits.csv"

# Outputs:
output_contacts = "W:/ASMA3/data/interim/hospital_visits_clean.csv"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Diagnoses ####
### ### ### ### ### ### ### ### ### ### ### ### ###

df = data.table::fread(input_diagnoses)
colnames(df) = tolower(colnames(df))


# Ambulatory care-sensitive conditions:
# (We only look at main diagnoses)

codes = 
  c(# VACCINE PREVENTABLE:
    # 1) bacterial pneumonia and influenza:
    'J09', 'J10', 'J11', 'J13', 'J14', 'J153', 'J154', 
    'J157', 'J159', 'J168', 'J181', 'J188', 'J189',
    # 2) immunization-related and preventable conditions:
    'A35', 'A36', 'A37', 'A80', 'B05', 'B06', 'B161', 'B169', 
    'B180', 'B181', 'B26', 'G000', 'M014',
    
    # CHRONIC:
    # 1) Iron deficienct anaemia:
    'D501', 'D502', 'D503', 'D504', 'D505', 'D506', 'D507', 'D508', 'D509',
    # 2) Diabetes complications: 
    'E100', 'E101', 'E102', 'E103', 'E104', 'E105', 'E106', 'E107', 'E108',
    'E110', 'E111', 'E112', 'E113', 'E114', 'E115', 'E116', 'E117', 'E118',
    'E120', 'E121', 'E122', 'E123', 'E124', 'E125', 'E126', 'E127', 'E128',
    'E130', 'E131', 'E132', 'E133', 'E134', 'E135', 'E136', 'E137', 'E138',
    'E140', 'E141', 'E142', 'E143', 'E144', 'E145', 'E146', 'E147', 'E148',
    # 3) Nutrional deficiencies:
    'E40', 'E41', 'E42', 'E43', 'E550', 'E643',
    # 4) Hypertension:
    'I10', 'I119',
    # 5) Congestive heart disease:
    'I11.0', 'I50', 'J81',
    # 6) Angina:
    'I20', 'I240', 'I248', 'I249',
    # 7) Acute bronchitis:
    'J20',
    # 8) Chronic obstructive pulmonary disease (COPD):
    'J41', 'J42', 'J43', 'J44', 'J47',
    # 9) Asthma:
    'J45', 'J46',
    
    # ACUTE:
    # 1) Dental conditions:
    'A690', 'K02', 'K03', 'K04', 'K05', 'K06', 'K08', 'K098','K099','K12','K13',
    # 2) Convulsions and epilepsy:
    'G40', 'G41', 'O15', 'R56',
    # 3) Dehydration and gastroenteritis:
    'E86', 'K522', 'K528', 'K529',
    # 4) Severe ear, nose and throat infections:
    'H66', 'H67', 'J02', 'J03', 'J06', 'J312',
    # 5) Perforated or bleeding ulcer:
    'K250', 'K251', 'K252', 'K254', 'K255', 'K256', 
    'K260', 'K261', 'K262', 'K264', 'K265', 'K266',
    'K270', 'K271', 'K272', 'K274', 'K275', 'K276',
    'K280', 'K281', 'K282', 'K284', 'K285', 'K286',
    # 6) Cellulitis:
    'L03', 'L04', 'L080', 'L088', 'L88', 'L980',
    # 7) Kidney and urinary tract infections:
    'N10', 'N11', 'N12', 'N136',
    # 8) Pelvic inflammatory disease:
    'N70', 'N73', 'N74',
    # 9) Gangrene:
    'R02'
    
  )

codes = paste(codes, collapse = '|')

# Create a dummy for whether the ICD-10 code is an 
# ambulatory care sensitive condition:
df[, acsc := as.integer(grepl(codes, icd10o))]
df[, mean(acsc)]

# If one or more of the ICD-10 codes represents ACSC, the events has ACSC:
df = df[, .(acsc = as.integer(sum(acsc) > 0)), by='kaynti_id']


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Emergency care visits and inpatient episodes. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Separate inpatient episodes and emergency department visits:
DT = data.table::fread(input_contacts)
colnames(DT) = tolower(colnames(DT))
DT.i = DT[inpatient==1]
DT.e = DT[emergency_dep==1]
rm(DT)


# For emergency department visits, drop and rename some columns and take 
# unique person-by-by-year-by-month-by-day observations where year 
# is in 2013-2019 (our study window). Finally, mark profession==50

DT.e[, ':=' (kaynti_id=NULL, endyear=NULL, endmonth=NULL, endday=NULL,
             emergency_dep=NULL, inpatient=NULL, urgent_inpatient=NULL)]
setnames(DT.e, old = c('startyear', 'startmonth'), new = c('year', 'month'))
DT.e = unique(DT.e[year %in% c(2013:2019)])
DT.e[, ':=' (startday=NULL, profession=50)]


# For inpatient episodes, create arrival and discharge dates:

DT.i[, ':=' (arrival = as.Date(paste(startyear, startmonth, startday, sep='-')),
             discharge = as.Date(paste(endyear, endmonth, endday, sep='-')))]
DT.i[, ':=' (startday=NULL, endyear=NULL, endmonth=NULL, endday=NULL,
             emergency_dep=NULL)]
DT.i[is.na(discharge), discharge := as.Date('2021-01-01')]
setnames(DT.i, old = c('startyear', 'startmonth'), new = c('year', 'month'))

# Compute inpatient spell duration and keep rows where duration %in% c(0:100) d:
# This drops 0.6% of the spells.
DT.i[, duration := as.integer(discharge - arrival)]
DT.i = DT.i[duration %in% c(0:100)]

# Merge inpatient episodes and ACSC data:
DT.i = merge(DT.i, df, by='kaynti_id', all.x = TRUE)
DT.i[, ':=' (kaynti_id = NULL, duration = NULL)]

# First, take and store those persons with one episode in the data:
persons = DT.i[, .N, by='id'][N==1, id]
DT.i1 = DT.i[id %in% persons]
DT.i = DT.i[!(id %in% persons)][, inpatient := NULL]

# Drop some variables and add profession=60. Take inpatient episodes with a
# ACSC diagnosis
DT.i1 = DT.i1[acsc==1 & urgent_inpatient==1]
DT.i1[, ':=' (inpatient=NULL, arrival=NULL, discharge=NULL, acsc=NULL, 
              urgent_inpatient=NULL, profession=60)]


# Second, take those person-by-arrival-date observations which are observed
# more than once. We reduce these to one observation where discharge date
# is the most recent one.

# Drop some duplicate rows:
DT.i = unique(DT.i)
DT.i = DT.i[order(id, arrival)]

DT.i = 
  DT.i[, .(discharge = max(discharge, na.rm=TRUE),
           urgent_inpatient = as.integer(sum(urgent_inpatient, na.rm=T) >= 1),
           acsc = as.integer(sum(acsc, na.rm=T) >= 1)), 
       by=c('id', 'arrival', 'year', 'month')]

# Take those rows where the person has only one row in the data (episode):
persons = DT.i[, .N, by='id'][N==1, id]
DT.i2 = DT.i[id %in% persons & acsc==1 & urgent_inpatient==1 &
               (discharge - arrival) %in% c(0:100), .(id, year, month)]
DT.i2[, profession := 60]
DT.i1 = rbind(DT.i1, DT.i2) 
DT.i = DT.i[!(id %in% persons)]


# Drop spells that are subspells of longer spells:
DT.i = DT.i[order(id, arrival)]
DT.i[discharge==as.Date('2021-01-01'), discharge := NA]
DT.i[, end_prev := shift(discharge, n=1, fill=NA, type='lag'), by='id']
DT.i[, days := as.integer(arrival - end_prev)]
DT.i = DT.i[days >= 0 | is.na(days)]

# If there is more than one week between spells, consider them as separate:
DT.i[, newspell := as.integer(is.na(days) | days > 7)]

# We are only interested in new spells. Keep urgent inpatient episodes with a 
# ACSC and drop some variables:
DT.i[, mean(newspell)] # 75 % of the spells remaining are new spells:
DT.i = DT.i[newspell==1 & urgent_inpatient==1 & acsc==1,
            .(id, year, month)]
DT.i[, profession := 60]

DT.i = rbind(DT.i1, DT.i)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Aggregate and save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Combine tables and aggregate at the ID-by-month-by-profession level, and save:
DT = rbind(DT.i, DT.e)
DT = DT[, .(visits = .N), by=c('id', 'year', 'month', 'profession')]

# Save:
write.csv(DT, output_contacts)

# End.
