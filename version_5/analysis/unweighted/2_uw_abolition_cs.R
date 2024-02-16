
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 2_uw_abolition_cs.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze the effects of the copayment abolition by 
#           using the CS estimator.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file. 
library(did)              # Staggered DID designs.


# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output_dd1_main = "W:/ASMA3/analysis/unweighted/tables/uw_abol_dd_cs_contacts_per_capita"
output_dd1_all = "W:/ASMA3/analysis/unweighted/tables/uw_abol_dd_cs_all_contacts_per_capita"

###
###

df = readRDS(input_data)
df = df[!is.na(profession)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create functions for data construction and estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract.data.cs = function(
  data, dim_treat, treat, outcome='contacts_per_capita', visit_type=2,
  treatment_year) {
  # INPUTS:
  # data: 'df'
  # dim_treat: dimension from which the treatment group is extracted
  # treat: the treatment group(s)
  # outcome: either 'contacts_per_capita', 'contacts_per_capita_log',
  #       'social_assistance', or 'basic_social_assistance'
  # visit_type: 2 for nurse visits, 1 for GP visits
  # treatment_year: the real treatment began on 1.7.2021. We can create 
  #         placebo studies by writing, e.g., 2018 In that case, the analysis
  #         proceeds as if the treatment started on 1.7.2018. Note that only
  #         2018 or 2019 should be used.
  # OUTPUT:
  # a data.table
  

  # Extract data:
  DT = data[dimension==dim_treat & group %in% treat & profession==visit_type
            ][!is.na(treat_abol)]
  
  # Use a 1-year bandwidth around the treatment_year. 
  start_month = as.Date(paste(treatment_year-1, '-07-01', sep=''))
  end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))
  DT = DT[date >= start_month & date <= end_month]
  
  
  # Next, we will drop observations due to issues in the visit
  # data quality (see 1_visits_quality.R). If the treatment year is 
  # the actual treatment year (2021), we drop municipalities that have 
  # drop_pandemic==1 at some point in the study window. If the treatment
  # year is a placebo year, we drop municipalities that have
  # drop_pre_pandemic==1 in that specific window.
  
  if(treatment_year == 2021) {
    drop_munies = unique(DT[drop_pandemic==1, municipality])
    DT = DT[!(municipality %in% drop_munies)]
  } else if (treatment_year %in% c(2018:2019)) {
    drop_munies = unique(DT[drop_pre_pandemic==1, municipality])
    DT = DT[!(municipality %in% drop_munies)]
  }
  

  # did::att_gt() assumes that the cohort is 0 for the never-treated units:
  DT[treat_abol==0, treat_group := 0]
  
  # get the treatment cohort:
  reform_month = as.Date(paste(treatment_year, '-07-01', sep=''))
  cohort = unique(DT[date == reform_month, relative_time])
  DT[treat_abol==1, treat_group := cohort]
  
  
  # Aggregate at the municipality-time level:
  
  DT = DT[, .(contacts = sum(contacts),
              population = sum(population),
              contacts_per_capita = weighted.mean(contacts_per_capita, 
                                                  w=population)),
          by=c('municipality', 'date', 'relative_time', 'treat_group')]
  
  # Create a variable for log annualized visits per capita:
  DT[, contacts_per_capita_log := log(contacts_per_capita)]
  
  # If outcome is 'contacts_per_capita_log', drop log(0) values:
  DT[, outcome := mget(outcome)]
  if(outcome=='contacts_per_capita_log') {
    DT = DT[contacts_per_capita != 0] }
  
  return(DT)
  
}

test1 = extract.data.cs(
  df, dim_treat = 'income_decile', treat = c(0:3),
  outcome='contacts_per_capita', visit_type=2, treatment_year = 2021
) 


### A function that estimates a CS (2021) staggered DD estimator. ###

estimate.cs = function(data, controls) {
  # INPUTS:
  # data: output from extract.data.cs()
  # controls: either 'nevertreated' or 'notyeattreated'
  # OUTPUTS:
  # a data.table containing results
  
  
  # Set seed for bootstrapping:
  set.seed(12345)
  
  # Estimate CS with an outcome regression DD estimator:
  # (see eq. 2.2 in Sant' Anna and Zhao, 2020)
  
  reg = did::att_gt(
    yname = 'outcome', 
    tname='relative_time',
    idname = 'municipality', 
    gname = 'treat_group',
    data = data, 
    allow_unbalanced_panel = FALSE,
    control_group = controls,
    bstrap=TRUE,
    cband=FALSE,
    clustervars = 'municipality',
    #weightsname = 'population',
    est_method = 'reg'
  )
  
  
  # A weighted average of all group-time ATTs with weights proportional
  # to group size:
  agg.simple = did::aggte(reg, type = 'simple')
  
  
  # Collect ATT results to a table:
  
  estimate = agg.simple$overall.att
  std_error = agg.simple$overall.se
  conf_low = estimate - qnorm(0.05/2,lower=F) * std_error
  conf_high = estimate + qnorm(0.05/2,lower=F) * std_error
  n_visits = data[, sum(contacts)]
  n_munies = reg$DIDparams$n
  pre_mean = data[relative_time < max(treat_group) & 
                    treat_group == max(treat_group), 
                  .(level = mean(outcome))]
                  #.(level = weighted.mean(outcome, w=population))]
  
  
  table.att = data.table(
    pre.mean=pre_mean, estimate=estimate, std_error=std_error, 
    conf_low=conf_low, conf_high=conf_high, change = 100 * estimate / pre_mean,
    n_visits=n_visits, n_munies=n_munies
  )
  
  return(table.att)
  
}

test = estimate.cs(test1, controls = 'nevertreated')
test


### A function that estimates the result for a set of models and for
# a set of specifications (see Section 2 of this script to see examples
# of models and specifications). ###

results.cs = function(models, specs, data) {
  
  dd.cs = lapply(models, function(mod) {
    
    print(mod[[5]])
    
    specs = lapply(1:nrow(specs), function(row) {
      
      # Store parameters outcome, visit type, and treatment year:
      spec = specs[row, ]
      
      # Extract the data:
      DT = extract.data.cs(
        data, dim_treat = mod[[1]], treat = mod[[2]], outcome=spec$outcome, 
        visit_type=spec$visit_types, treatment_year = spec$treat_year
      ) 
      
      # Estimate the results:
      results = estimate.cs(DT, controls = 'nevertreated')
      
      # Add information about the specification:
      results[, ':=' (outcome = spec$outcome,
                      visit_type = spec$visit_types,
                      treat_year = spec$treat_year)]
      
      return(results)
    })
    specs = do.call(rbind.data.frame, specs)
    specs = specs[, mget(colnames(specs))]
    specs[, mod := mod[[5]]]
  })
  dd.cs = do.call(rbind.data.frame, dd.cs)
  dd.cs = dd.cs[, mget(colnames(dd.cs))
                ][outcome=='contacts_per_capita_log', change := NA_integer_]
  
  return(dd.cs)
  
}


### A function that mutates the raw results tables to tidy tables. ###

table.dd.cs = function(models, specs, results) {
  # INPUTS:
  # models: e.g. c('Bottom 40%', 'Top 40%') or 'Bottom 40%, top 40%'
  # results: the output from results.dd()
  
  
  # Collect outcomes and visit types which we will loop over:
  outcome = unique(specs$outcome)
  visit_types = c(2,1,40) # referrals (3) excluded
  print(outcome)
  print(visit_types)
  
  
  # Loop over outcomes:
  table.dd = lapply(outcome, function(otc) {
    
    # Loop over visit types (nurse or GP):
    table.type = lapply(visit_types, function(type) {
      
      
      # Extract the right results:
      data = results[treat_year==2021 & mod %in% models &
                       visit_type==type & outcome==otc,
                     .(pre.mean.level, estimate, std_error,
                       change.level, n_visits, n_munies, mod)]
    
      if(otc=='contacts_per_capita_log') {
        data[, ':=' (estimate = 100*estimate, std_error = 100*std_error)] }
      
      
      # Rename:
      setnames(
        data, old=c('pre.mean.level', 'estimate', 'std_error', 
                    'change.level', 'n_visits', 'n_munies'),
        new=c('Level', 'Estimate', 'Std. error', 
              'Change (%)', 'Contacts', 'Municipalities'), 
        skip_absent = TRUE
      )
      
      
      if(otc=='contacts_per_capita_log') {
        data[, ':=' ('Change (%)' = NULL, Level = NULL)]  }
      
      # Remove subgroup labels from the table:
      data[, mod := NULL]
      
      # Row-bind the estimates from the groups of interest:
      row_names = colnames(data)
      group1 = data[1,]
      group2 = data[2,]
      data = data.table('Metric' = rep(row_names, times=2),
                        type = c(group1, group2))
      
      # Change data types to numeric:
      data[, type := as.numeric(type)]
      
      return(data)
    })
    
    table.type = do.call(cbind.data.frame, table.type)[, c(1,2,4, 6)]
    colnames(table.type) = c(colnames(table.type)[2], 
                             'Nurse Visits', 'GP Visits', 'Prescriptions')
    
    return(table.type)
    
  })
  names(table.dd) = outcome
  
  return(table.dd)
}


### A function that saves tables in three formats: tex, xlsx, rds ###

save.table = function(table, output, label_tex, title_tex) {
  # INPUTS:
  # table: a table to be saved
  # output: file path excluding the file type (e.g. .csv)
  # label_tex: character label for .tex tables
  # title_tex: character title for .tex tables
  
  # tex:
  stargazer::stargazer(
    table, type = "text", summary = F,
    out = paste(output, "tex", sep = "."),
    title = title_tex, label = label_tex,
    rownames = F, header=F)
  
  # xlsx:
  openxlsx::write.xlsx(table, file = paste(output, "xlsx", sep = "."),
                       overwrite = TRUE)
  
  #rds:
  saveRDS(table, file = paste(output, "rds", sep = "."))
  
}



### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# We will estimate the effects in the following subgroups:

models = list(
  D14 = list('income_decile', c(0:3), NULL, NULL, 'Bottom 40%'), 
  D710 = list('income_decile', c(6:9), NULL, NULL, 'Top 40%'),
  all = list('income_decile', c(0:9), NULL, NULL, 'all')
)

# We will loop over contact type, treatment year, and outcome:

visit_types = c(2, 1, 40) # referrals (3) are excluded
treat_year = c(2021)
outcome = c('contacts_per_capita', 'contacts_per_capita_log')
specs = CJ(visit_types, treat_year, outcome)


# Estimate the results:
main = results.cs(models=models, specs=specs, data=df)


# Mutate results to tidy tables:
table = table.dd.cs(models=c('Bottom 40%','Top 40%'), specs=specs, results=main)
table = rbind(table$contacts_per_capita, table$contacts_per_capita_log)

# The table on all individuals requires a bit more tidying:
table.all = table.dd.cs(models=c('all'), specs=specs, results=main)
table.all = as.data.table(
  rbind(table.all$contacts_per_capita, table.all$contacts_per_capita_log)
)
table.all = table.all[!is.na(Prescriptions)]


# Save tables:

save.table(table, output=output_dd1_main,
           label_tex = 'tab:abol_dd_cs_visits_per_capita',
           title_tex = 'Abolition: CS DID.')

save.table(table.all, output=output_dd1_all,
           label_tex = 'tab:abol_dd_cs_all_visits_per_capita',
           title_tex = 'Abolition: CS DID.')

# End.
