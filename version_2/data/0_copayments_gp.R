
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            R script 0_copayments_gp.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits     ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Tidy data on GP visit copayments.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating data.

# Inputs:
input_policies = "W:/ASMA3/data/raw/copay_raw.csv"
input_munies_13 = "W:/ASMA3/data/raw/municipalities_2013.csv"

# Outputs:
output_data = "W:/ASMA3/data/interim/copayments_gp.rds"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Create a data frame for copayments. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Data that contains all municipalities existing in 2013.
# Expand it to cover all months in 2013-2018.

data = as.data.table(
  read.csv(file=input_munies_13, sep = ";", header=FALSE, encoding = "UTF-8")
)
colnames(data) = c('no', 'name')

month = c(1:12)
year = c(2013:2018)

# Expand:
setkey(data, no)
combs = data[, CJ(unique(no), month, year)]
data = data[combs]
setkey(data, NULL)


# Read the raw copayment data and remove empty rows.

df = data.table::fread(
  input_policies, dec = ',',
  select = c('no', 'date', 'copayment', 'policy', 'annual_pay', 'note')
)
df = df[!is.na(no)]

# Transform the date variable to the Date format:

df = df[, c('day', 'month', 'year') := tstrsplit(date, '\\.')
        ][, date := as.Date(paste(year, month, day, sep='-'))
          ][, ':=' (day = NULL, year = as.integer(year), 
                    month = as.integer(month))]

# Merge data and df:
copay = merge(data, df, by=c("no", "year", "month"), all.x = T)


# Right now, we have only copayment changes (time of change by month and year) 
# in the matrix.

# Find rows that contain a date and save row numbers to a vector.
filled_rows = copay[!is.na(date), which = TRUE]


# Fill up empty rows between policy changes.

for (i in 1:length(filled_rows)) {
  
  row_no = filled_rows[i]
  municipality = copay[row_no, no]
  values = unlist(copay[row_no, c('copayment', 'policy', 'annual_pay')])
  
  while(is.na(copay$date[row_no+1]) & copay$no[row_no+1] == municipality) {
    copay[row_no+1, ':=' (copayment = values[1],
                          policy = values[2],
                          annual_pay = values[3])]
    row_no = row_no + 1
    if(row_no == nrow(copay)) {
      break
    }
  }
  
}

copay[, time := 
        as.Date(paste(as.character(year), as.character(month), '1', sep='-'))]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Clean the data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Copayment data were collected by observing websites of health centers, 
# and it contains some uncertainties. These uncertainties are stated in Finnish
# in variable "note". Remainder of this script takes the uncertainty into 
# account. We exclude some observations and include some of the others by 
# making assumptions.

# First, some cleaning without dropping or making assumptions:

# Municipalities that were merged with another municipalities and 
# ceased to exist:
copay[no %in% c(476, 413, 838) & year >= 2015, policy := NA]
copay[no %in% c(164, 283, 319, 532) & year >= 2016, policy := NA]
copay[no %in% c(442, 174) & year >= 2017, policy := NA]

# Adding a specific policy code (50) to Helsinki (no copayments).
copay[no==91, policy := 50]

# Some policy changes occurred after the beginning of a month. We define that 
# such changes took into effect from the beginning of the next full month.
copay[no %in% c(905, 399) & year==2017 & month==3, policy := 3]
copay[no %in% c(105, 205, 290, 578, 697, 765, 777) & year==2016 & month==2, 
      policy := 3]
copay[no %in% c(224, 927, 607) & year==2014 & month==1, copayment := 13.8]
copay[no==607 & year==2015 & month==1, copayment := 14.7]

# Some observations have to be removed (namely set policy = NA) because of too 
# much uncertainty or conflicting information. In addition, complete the 
# observations of Lavia in 2013.
copay[no %in% c(140, 263, 762, 925, 102, 52, 233, 403, 143) & year<=2015, 
      policy := NA]
copay[no==18 & year==2018 & month==1, policy := NA]
copay[no %in% c(686, 778), policy := NA]
copay[no==620 & year==2014 & month==2, policy := NA]
copay[no==413 & year==2013, policy := 3]
copay[no %in% c(604, 922) & year==2015, policy := NA]
copay[no==413 & year==2013, copayment := 13.8]

# There were 11 municipality mergers between 2014 and 2022.

# In eight of these cases, the pre-merger policies were similar.
# However, in one case the policies differed in 2015. Lahti had a per-visit
# copayment while Nastola allowed patients to choose between a per-visit
# copayment and an annual copayment. This does not matter in our case.

# Drop observations where policy is not observed:
copay = copay[!is.na(policy)]

# Drop the municipalities that were merged between 2014 and 2022:
copay = copay[!(no%in% c(838, 413, 476, 532, 319, 283, 164, 174, 442, 911, 99))]

# Few area only provide the annual copayment option. Divide the annual amount 
# by two to construct a "per-visit copayment" for better comparisons.
copay[policy==0, copayment := annual_pay / 2]

# Extract relevant columns:
copay = copay[, .(no, time, copayment)]
setnames(copay, old=c('no', 'time'), new=c('municipality', 'date'))

saveRDS(copay, file = output_data)

# End.

