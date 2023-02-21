
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script 2_introduction_rs.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze the effects of the staggered copayment introduction
#           by estimating difference-in-differences (DD) models 
#           using Roth and Sant' Anna (2022).
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(staggered)        # Roth & Sant'Anna (2022): staggered DID

# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output = 'W:/ASMA3/data/cleaned/intro_results_rs.rds'
output_dd1 = "W:/ASMA3/analysis/tables/intro_dd_rs"
output_dd1_log = "W:/ASMA3/analysis/tables/intro_dd_rs_log"


###
###

df = readRDS(input_data)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create functions for data construction and estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract.data.rs = function(
  data, dim_treat, treat, outcome='contacts_per_capita', 
  visit_type=2, start_year=2013, end_year=2019, comparison='latertreated') {
  # INPUTS:
  # data: 'df'
  # dim_treat: dimension from which the treatment group is extracted
  # treat: the treatment group(s)
  # outcome: either 'contacts_per_capita', 'contacts_per_capita_log',
  #       'social_assistance', or 'basic_social_assistance'
  # visit_type: 2 for nurse visits, 1 for GP visits
  # start_year: year when the study period starts as an integer
  # end_year: year when the study period ends as an integer
  # comparison: 'latertreated' or 'notyettreated'
  # OUTPUT:
  # a data.table
  
  
  # Municipalities whose policies are observed:
  munies = unique(data[!is.na(treat_intro), municipality])
  
  # Extract data:
  DT = data[dimension==dim_treat & group %in% treat & year >= start_year 
            & year <= end_year & municipality %in% munies]
  setnames(DT, old='relative_time', new='time_no')
  
  # Find the treatment cohort for each treated municipality:
  cohort = DT[treat_intro==1
              ][, .(treat_group = min(time_no)), by='municipality']
  DT = merge(DT, cohort, by='municipality', all.x = TRUE)
  
  # Relative times need to merged to the municipality-year panel
  # on social assistance (later).
  rel_times = unique(DT[, mget(c('date', 'time_no'))])
  
  
  # Depending on the outcome, extract relevant data:
  if(!is.null(visit_type)) {
    DT = DT[profession == visit_type]
    
  } else if(outcome=='social_assistance') {
    DT = DT[is.na(profession) & !is.na(date)]
    
  } else if (outcome=='basic_social_assistance') {
    DT = DT[is.na(profession) & is.na(date)]
    DT[, ':=' (date = as.Date(paste(as.character(year), '-01-01', sep='')),
               time_no = NULL)]
    DT = merge(DT, rel_times, by='date', all.x=TRUE)
  }
  
  
  # Impute 0 for the never-treted:
  DT[is.na(treat_group), treat_group := 0]
  
  
  # Each event should have at least a 12-month follow-up:
  if(outcome=='basic_social_assistance') {
    DT = DT[treat_group <= max(DT$time_no)]
  } else {
    DT = DT[max(DT$time_no) - treat_group + 1 >= 12 | treat_group==0] }
  
  # Drop those observations that are already treated at the start of 
  # the panel:
  DT = DT[min(time_no) != treat_group]
  
  
  # When outcome=='basic_social_assistance' (annual sums), we take only 
  # the never-treated municipalities, and municipalities that have event
  # on 1.1.201X:
  if(outcome=='basic_social_assistance') {
    events = unique(DT[treat_group==time_no, treat_group])
    DT = DT[treat_group %in% c(0, events)]
  }
  
  
  # Next, we will drop municipality-year observations due to issues 
  # in the data quality (see 1_visits_quality.R).
  DT = DT[drop_pre_pandemic == 0]
  
  
  # Aggregate at the municipality-time level, time period depending 
  # on the outcome:
  
  if(!is.null(visit_type)) {
    
    DT = DT[, .(contacts = sum(contacts),
                population = sum(population),
                contacts_per_capita = weighted.mean(contacts_per_capita, 
                                                    w=population)),
            by=c('municipality', 'date', 'time_no', 'treat_group')]
    
    # Create a variable for log annualized visits per capita:
    DT[, contacts_per_capita_log := log(contacts_per_capita)]
    
    # If outcome is 'contacts_per_capita_log', drop log(0) values:
    DT[, outcome := mget(outcome)]
    if(outcome=='contacts_per_capita_log') {
      DT = DT[contacts_per_capita != 0] }
    
    
  } else if (outcome=='social_assistance') {
    
    DT = DT[, .(N = .N,
                social_assistance = sum(social_assistance),
                population = sum(population)),
            by=c('municipality', 'date', 'time_no', 'treat_group')
            # The share of those in a family receiving social assistance
            # in month m as a percentage.
            ][, outcome := 100 * social_assistance / population]
    
  } else if (outcome == 'basic_social_assistance') {
    
    DT = DT[, .(N = .N,
                basic_social_assistance = sum(basic_social_assistance),
                population = sum(population)),
            by=c('municipality', 'date', 'time_no', 'treat_group')
            # The share of those in a family receiving social assistance
            # in month m as a percentage.
            ][, outcome := basic_social_assistance / population
              ][treat_group > 0, treat_group := (treat_group-1)/12
                ][, time_no := (time_no-1)/12]
    
  }
  
  
  # Require a balanced panel in calendar time:
  to_keep = DT[, .N, by='municipality'][N == max(N), municipality]
  DT = DT[municipality %in% to_keep]
  
  # Drop treatment cohorts having a single cross-sectional unit:
  to_keep = DT[, .(munies = length(unique(municipality))), by='treat_group'
               ][munies > 1, treat_group]
  DT = DT[treat_group %in% to_keep]
  
  
  # The comparison group:
  
  if(comparison=='latertreated') {
    DT = DT[treat_group != 0] # exclude the never-treated
    
  } else if (comparison=='notyettreated') {
    # staggered() assumes that the cohort is Inf for the never-treated:
    DT[, treat_group := as.double(treat_group)]
    DT[treat_group == 0, treat_group := Inf] }
  
  return(DT)
  
}

test1 = extract.data.rs(
  df, outcome='contacts_per_capita', dim_treat = 'income_decile', 
  treat = c(0:3), visit_type=2, start_year = 2013, end_year=2019
)

test2 = extract.data.rs(
  df, outcome='social_assistance', dim_treat = 'income_decile', 
  treat = c(0:9), visit_type=NULL, start_year = 2014, end_year=2018
)

test3 = extract.data.rs(
  df, outcome='basic_social_assistance', dim_treat = 'income_decile', 
  treat = c(0:9), visit_type=NULL, start_year = 2014, end_year=2018
)


### A function that estimates a RS (2022) staggered DD estimator. ###

estimate.rs = function(data) {
  # INPUTS:
  # data: output from extract.data.cs()
  # OUTPUTS:
  # a data.table containing results
  
  
  estimands = c('simple', 'calendar', 'cohort')
  
  # Loop over estimands:
  
  results = lapply(estimands, function(x) {
    
    # Estimate:
    # (If var_conservative is less than adjustmentFactor, use the se_neyman)
    
    reg = tryCatch(
      
      {staggered::staggered(df=data, i='municipality', t='time_no', 
                            g='treat_group', y='outcome', estimand=x)}, 
      
      warning=function(w) {
        
        reg = staggered::staggered(
          df=data, i='municipality', t='time_no', g='treat_group', 
          y='outcome', estimand=x)

        reg = as.data.table(reg)
        reg[, se := NULL]
        setnames(reg, old='se_neyman', new='se')
        
      })
    
    # Collect ATT results to a table:
    
    estimate = reg$estimate
    std_error = reg$se
    pre_mean = data[treat_group < Inf & (treat_group - time_no) %in% c(1:12), 
                    mean(outcome)]
    change = 100 * estimate / pre_mean
    conf_low = estimate - qnorm(0.05/2,lower=F) * std_error
    conf_high = estimate + qnorm(0.05/2,lower=F) * std_error
    n_events = length(setdiff(data[, unique(treat_group)], Inf))
    n_munies = data[, length(unique(municipality))]
    
    table.att = data.table(
      estimate=estimate, std_error=std_error, pre_mean=pre_mean, change=change,
      conf_low=conf_low, conf_high=conf_high, n_events, n_munies, estimand = x
    )
    
  })
  
  results = do.call(rbind.data.frame, results)
  return(results)
  
}

test = estimate.rs(test1)
test
test = estimate.rs(test2)
test


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Estimate. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will estimate the effects in the following subgroups:

models_pri = list(
  D14 = list('income_decile', c(0:3), NULL, NULL, 'Bottom 40%'), 
  D710 = list('income_decile', c(6:9), NULL, NULL, 'Top 40%')
)


# For main analysis, we will loop over visit type (nurse / GP), 
# outcome, start year, and end year:

visit_types = c(2, 1)
start_years = c(2013, 2014)
end_years = c(2018, 2019)
outcomes = c('contacts_per_capita', 'contacts_per_capita_log')

specs_pri = CJ(visit_types, start_years, end_years, outcomes)

# Add specification numbers to controls-start_year combinations:
spec_no = CJ(start_years, end_years)
spec_no[, spec_no := c(1:nrow(spec_no))]
specs_pri = 
  merge(specs_pri, spec_no, by=c('start_years', 'end_years'), all.x=TRUE)


### A function that loops over models and specifications to 
# estimate the results: ###

results.rs = function(data, models, specs) {
  
  rs = lapply(models, function(mod) {
    
    print(mod[[5]])
    
    # Loop over specifications:
    results.specs = lapply(1:nrow(specs), function(row) {
      
      print(paste('Specification', row))
      
      # Store parameters:
      spec = specs[row, ]
      
      if('visit_types' %in% colnames(spec)) {
        visit_type_param = spec$visit_types
        visit_type_param2 = spec$visit_types
      } else { 
        visit_type_param = NULL
        visit_type_param2 = NA
      }
      
      
      # Extract the data:
      DT = extract.data.rs(
        data, outcome = spec$outcomes,
        dim_treat = mod[[1]], treat = mod[[2]],
        visit_type = visit_type_param, start_year = spec$start_years,
        end_year = spec$end_years, comparison = 'latertreated'
      )
      
      # Estimate the regressions:
      results = estimate.rs(DT)
      
      # Add information about the specification:
      results = results[, mget(colnames(results))]
      results[, ':=' (outcome = spec$outcomes,
                      visit_type = visit_type_param2,
                      start_year = spec$start_years,
                      end_year = spec$end_years,
                      spec_no = spec$spec_no)]
      
      return(results)
    })
    results.specs = do.call(rbind.data.frame, results.specs)
    results.specs = results.specs[, mget(colnames(results.specs))]
    results.specs[, mod := mod[[5]]]
  })
  rs = do.call(rbind.data.frame, rs)
  rs = rs[, mget(colnames(rs))]
  
  return(rs)
  
}

rs.pri = results.rs(data=df, models = models_pri, specs = specs_pri)
saveRDS(rs.pri, output)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Save results to table. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


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


# Create a new variable to the table that includes the effect estimate, the
# standard error estimate and the estimated percentage change rounded and in the 
# following character form: "X.XXX (Y.YYY) [Z.Z%]".

# Sign of the estimate:
rs.pri[, sign := ifelse(estimate < 0, '-', '+')]

# Round:
rs.pri[, ':=' (estimate_r = paste(sign, 
                                  trimws(format(round(abs(estimate), digits=3), 
                                                nsmall=3)), sep=''),
               se = format(round(std_error, digits=3), nsmall=3))]

# Change in percentages only for outcomes in levels:
rs.pri[outcome=='contacts_per_capita', 
       change_r := paste(sign, trimws(format(round(abs(change), digits=1),
                                             nsmall=1)), sep='')]

# Concatenate:
rs.pri[, results := ifelse(outcome=='contacts_per_capita', 
                           paste(estimate_r, ' (', se, ') [', change_r, '\\%]', 
                                 sep=''),
                           paste(estimate_r, ' (', se, ')', sep=''))]


# We will loop over the following features:
outcomes = c('contacts_per_capita', 'contacts_per_capita_log')
professions = c(2, 1)
income_groups = c('Bottom 40%', 'Top 40%')
estimands = c('simple', 'cohort', 'calendar')
specifications = 1:4


# Create the tables:

tables_otc = lapply(outcomes, function(otc) {
  
  tables_prof = lapply(professions, function(prof) {
    
    tables_inc = lapply(income_groups, function(grp) {
      
      tables_est = lapply(estimands, function(est) {
        
        tables_spec = lapply(specifications, function(spec) {
  
          # Extract the results:
          result = rs.pri[outcome==otc & visit_type==prof & mod==grp &
                            estimand==est & spec_no==spec, results]
          otp = data.table(Window=spec, value=result)
          
        })
        tables_spec = do.call(rbind.data.frame, tables_spec)
        otp = data.table(Estimand=est, tables_spec)
        
      })
      tables_est = do.call(rbind.data.frame, tables_est)
      otp = data.table(Income=grp, tables_est)
       
    })
    tables_inc = do.call(cbind.data.frame, tables_inc)
    otp = data.table(Profession=prof, tables_inc)
    otp = otp[, c(3, 4, 5, 9)]
    
  })
  tables_prof = do.call(cbind.data.frame, tables_prof)
  otp = as.data.table(tables_prof[, c(1:4, 7:8)])
  colnames(otp) = c('Estimand', 'Window', 'Nurse B40', 
                    'Nurse T40', 'GP B40', 'GP T40')
  
  
  otp[, Window := as.character(Window)]
  otp[Window=='1', Window := '2013-18']
  otp[Window=='2', Window := '2013-19']
  otp[Window=='3', Window := '2014-18']
  otp[Window=='4', Window := '2014-19']
  
  return(otp)
  
})
names(tables_otc) = outcomes

# Save:

save.table(
  tables_otc$contacts_per_capita, output=output_dd1,
  label_tex = 'tab:intro_dd_rs',
  title_tex='Adoption: Between--Municipality RS DD Comparisons, Primary Outcomes.'
)

save.table(
  tables_otc$contacts_per_capita_log, output=output_dd1_log,
  label_tex = 'tab:intro_dd_rs',
  title_tex='Adoption: Between--Municipality RS DD Log Comparisons, Primary Outcomes.'
)

# End.
