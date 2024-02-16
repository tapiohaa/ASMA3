
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 2_abolition_dd.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze the effects of the copayment abolition by estimating
#           difference-in-differences (DD) and triple differences (DDD) models.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file. 
library(fastDummies)      # Fast creation of dummy columns.
library(lfe)              # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.

# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output_dd1_main = "W:/ASMA3/analysis/tables/abol_dd_main_contacts_per_capita"
output_dd1_main_log = 
  "W:/ASMA3/analysis/tables/abol_dd_main_contacts_per_capita_log"
output_dd1_all = "W:/ASMA3/analysis/tables/abol_dd_all_contacts_per_capita"
output_ddd_main = "W:/ASMA3/analysis/tables/abol_ddd_main_contacts_per_capita"


###
###

df = readRDS(input_data)
df = df[!is.na(profession)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create shared functions for both DD and DDD models. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract_data = function(data, type, dim_treat, treat, dim_control=NA, 
                        control=NA, outcome='contacts_per_capita',
                        visit_type=2, treatment_year=2021) {
  # INPUTS:
  # data: 'df'
  # type: 'dd1', 'dd2', 'dd3', or 'ddd'.
  #       'dd1': DD model comparing the outcomes across municipalities 
  #           within a given group.
  #       'dd2': DD model comparing the outcomes between groups within 
  #           treatment municipalities.
  #       'dd3': DD model comparing the outcomes between groups within 
  #           comparison municipalities.
  #       'ddd': triple differences.
  # dim_treat: dimension from which the treatment group is extracted
  # treat: the treatment group(s)
  # dim_control: dimension from which the comparison group is extracted
  # control: the comparison group(s)
  # outcome: either 'contacts_per_capita' or 'contacts_per_capita_log'
  # visit_type: 2 for nurse visits, 1 for GP visits
  # treatment_year: the real treatment began on 1.7.2021. We can create 
  #         placebo studies by writing, e.g., 2018 In that case, the analysis
  #         proceeds as if the treatment started on 1.7.2018. Note that only
  #         2018 or 2019 should be used.
  # OUTPUT:
  # a data.table
  

  # Extract data:
  DT = data[profession==visit_type][!is.na(treat_abol)]
  DT[, contacts_per_capita_log := log(contacts_per_capita)]
  DT[, outcome := mget(outcome)]
  
  # Use a 1-year bandwidth around the treatment_year. 
  start_month = as.Date(paste(treatment_year-1, '-07-01', sep=''))
  end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))
  DT = DT[date >= start_month & date <= end_month]
  
  
  # If outcome is 'contacts_per_capita_log', drop log(0) values:
  
  if(outcome=='contacts_per_capita_log') {
    
    DT = DT[contacts_per_capita != 0] 
    
    keep_obs = DT[, .N, by=c('municipality', 'date')
                  ][N == max(N)
                    ][, keep := 1]
    DT = merge(DT, keep_obs, by=c('municipality', 'date'), all.x = TRUE)
    DT = DT[keep == 1][, keep := NULL]
  }

  
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

  
  # Create an indicator for post-treatment periods:
  DT[, post := as.integer(date >= as.Date(paste(treatment_year, 
                                                '-07-01', sep='')))]
  
  if(type == 'dd1') {
    
    DT = DT[dimension == dim_treat & group %in% treat
            ][, treat := treat_abol]
                                                        
  } else if (type == 'dd2') {
    
    DT = DT[treat_abol == 1 &
              ((dimension==dim_treat & group %in% treat) | 
                 (dimension==dim_control & group %in% control))
            ][, treat := as.integer(dimension==dim_treat & group %in% treat)]
      
  } else if (type == 'dd3') {
    
    DT = DT[treat_abol == 0 &
              ((dimension==dim_treat & group %in% treat) | 
                 (dimension==dim_control & group %in% control))
            ][, treat := as.integer(dimension==dim_treat & group %in% treat)]
    
  } else if (type == 'ddd') {
    
    DT = DT[(dimension==dim_treat & group %in% treat) |
              (dimension==dim_control & group %in% control)
            # group='treat' for the supposedly more vulnerable population group
            ][, group := as.character(dimension==dim_treat & group %in% treat)
              ][, group := fcase(group == 'FALSE', 'control',
                                 group == 'TRUE', 'treat')]
    
  }
  
 
  # Center relative_time at the treatment date:
  treat_time = DT[post==1, min(relative_time)]
  DT[, relative_time := relative_time - treat_time]
  
  
  # Create dynamic treatment dummies for post-treatment periods for
  # DD specifications:
  
  if(type %in% c('dd1', 'dd2', 'dd3')) {
   
    DT[relative_time >= 0 & treat==1, treat_dummies := relative_time]
    DT = fastDummies::dummy_cols(
      .data=DT, select_columns = 'treat_dummies', ignore_na = TRUE
    )
    
    # Fill zeroes to NA values in 'treat_dummies_':
    cols = colnames(DT)[startsWith(colnames(DT), 'treat_dummies_')]
    DT[, (cols) := lapply(.SD, nafill, fill=0), .SDcols=cols]
    
  }
  
  return(DT)
  
}

test1 = extract_data(df, type = 'dd1', outcome='contacts_per_capita',
                     dim_treat = 'income_decile', treat = c(0:3),
                     visit_type=2, treatment_year = 2021) 
test2 = extract_data(df, type = 'dd2', outcome='contacts_per_capita',
                     dim_treat = 'income_decile', treat = c(0:3),
                     dim_control = 'income_decile', control = c(6:9),
                     visit_type=2, treatment_year = 2021)
test3 = extract_data(df, type = 'dd3', outcome='contacts_per_capita',
                     dim_treat = 'income_decile', treat = c(0:3),
                     dim_control = 'income_decile', control = c(6:9),
                     visit_type=2, treatment_year = 2021)
test4 = extract_data(df, type = 'ddd', outcome='contacts_per_capita',
                     dim_treat = 'income_decile', treat = c(0:3),
                     dim_control = 'income_decile', control = c(6:9),
                     visit_type=2, treatment_year = 2021)


### A function that estimates the results and returns a results table. ###

estimate_lm = function(data, type) {
  # INPUTS:
  # data: tidied data.table from 'extract_data()'
  # type: 'dd1', 'dd2', 'dd3', or 'ddd'.
  #       'dd1': DD model comparing the outcomes across municipalities 
  #           within a given group.
  #       'dd2': DD model comparing the outcomes between groups within 
  #           treatment municipalities.
  #       'dd3': DD model comparing the outcomes between groups within 
  #           comparison municipalities.
  #       'ddd': triple differences.
  # OUTPUTS:
  # regression results as a data.table
  
  
  # Copy the data to not mutate the original source:
  cols = colnames(data)
  DT = data[, mget(cols)]
  
  
  # The estimation specification depends on the type of the model:
  
  n_lags = max(DT$relative_time)
  lags = paste0("treat_dummies_", 0:n_lags)
  lags = paste(lags, collapse = ' + ')
  
  
  if(type == 'dd1') {
    
    spec = 
      'outcome ~ treat:post | municipality + relative_time | 0 | municipality'
    
    spec.dyn =
      paste('outcome ~ ', lags, 
            '+ treat:relative_time | municipality + relative_time | 0 | municipality')
    
  } else if (type %in% c('dd2', 'dd3')) {
    
    spec = 'outcome ~ treat:post | treat + relative_time | 0 | municipality' 
    
    spec.dyn =
      paste('outcome ~ ', lags, 
            '+ treat:relative_time | treat + relative_time | 0 | municipality')
    
  } else if (type == 'ddd') {
    spec = 'outcome ~ treat_abol + group + post + treat_abol:group + treat_abol:post + group:post + treat_abol:group:post | 0 | 0 | municipality'
  }
  

  # Estimate the model without a linear pre-trend difference:
  reg = lfe::felm(as.formula(spec), data = DT, weights = DT$population)
  reg = setDT(tidy(reg, conf.int=TRUE, se.type='cluster'))
  reg[, dynamic := 0]
  
  
  if(type %in% c('dd1', 'dd2', 'dd3')){
    
    # Estimate the model with a linear pre-trend difference:
    reg.dyn = lfe::felm(as.formula(spec.dyn), data = DT,
                        weights = DT$population)
    reg.dyn = setDT(tidy(reg.dyn, conf.int=TRUE, se.type='cluster'))
    reg.dyn = reg.dyn[startsWith(term, 'treat_dummies_')
                      ][, t := as.integer(gsub('treat_dummies_', '', term))]
    reg.dyn[, dynamic := 1]
    
    # The mean of dynamic treatment effects:
    mean_lags = mean(reg.dyn$estimate)
    
    # Concatenate tidy regression tables:
    results = rbind(reg, reg.dyn, fill=TRUE)
    results[, trends.mean.lags := mean_lags]
    
    
  } else if (type=='ddd') {
    
    results = reg[term=='treat_abol:grouptreat:post'] }
  
  
  
  # Next, we will collect relevant data from the estimated model:
  
  # Pre-treatment level in treated group:
  if(type=='ddd') {
    pre_mean = DT[post==0 & treat_abol==1 & group=='treat', 
                  .(level = weighted.mean(outcome, w=population))]
  } else {
    pre_mean = DT[post==0 & treat==1, 
                  .(level = weighted.mean(outcome, w=population))]
  }
  
  pre_mean = pre_mean$level
  
  # Sample sizes:
  n_visits = sum(DT$contacts)
  n_munies = length(unique(DT$municipality))
  
  
  results[dynamic==0, ':=' (pre.mean = pre_mean,
                            n.visits = n_visits,
                            n.munies = n_munies,
                            change = 100 * estimate / pre_mean,
                            treat.year = min(DT[post==1, year]))]
  
  if(type %in% c('dd1', 'dd2', 'dd3')) {
    results[dynamic==0, change.trends := 100 * trends.mean.lags / pre_mean]
  }

  return(results)
}

# With the function:
print(estimate_lm(test1, type = 'dd1'))
print(estimate_lm(test2, type = 'dd2'))
print(estimate_lm(test3, type = 'dd3'))
print(estimate_lm(test4, type = 'ddd'))

# Without the function:
spec = 'outcome ~ treat:post | municipality + relative_time | 0 | municipality'
reg = lfe::felm(as.formula(spec), data = test1, weights = test1$population)
summary(reg, robust=TRUE)$coefficients
spec = 'outcome ~ treat:post | treat + relative_time | 0 | municipality'
reg = lfe::felm(as.formula(spec), data = test2, weights = test2$population)
summary(reg, robust=TRUE)$coefficients
reg = lfe::felm(as.formula(spec), data = test3, weights = test3$population)
summary(reg, robust=TRUE)$coefficients
spec = 'outcome ~ treat_abol + group + post + treat_abol:group + treat_abol:post + group:post + treat_abol:group:post | 0 | 0 | municipality'
reg = lfe::felm(as.formula(spec), data = test4, weights = test4$population)
summary(reg, robust=TRUE)$coefficients


### A function that estimates the result for a set of models and for
# a set of specifications (see Section 2 of this script to see examples
# of models and specifications). ###

results.dd = function(models, specs, data) {
  
  dd = lapply(models, function(mod) {
    
    print(mod[[5]])
    
    specs = lapply(1:nrow(specs), function(row) {
      
      # Store parameters outcome, visit type, and treatment year:
      spec = specs[row, ]
      
      # Extract the data:
      DT = extract_data(
        data, type = spec$dd_type, outcome = spec$outcome,
        dim_treat = mod[[1]], treat = mod[[2]],
        dim_control = mod[[3]], control = mod[[4]],
        visit_type = spec$visit_types, treatment_year = spec$treat_year
      )
      
      # Estimate the regressions:
      results = estimate_lm(DT, type = spec$dd_type)
      
      # Add information about the specification:
      results[, ':=' (outcome = spec$outcome,
                      visit_type = spec$visit_types,
                      treat_year = spec$treat_year,
                      dd_type = spec$dd_type)]
      
      return(results)
    })
    specs = do.call(rbind.data.frame, specs)
    specs = specs[, mget(colnames(specs))]
    specs[, mod := mod[[5]]]
  })
  dd = do.call(rbind.data.frame, dd)
  dd = dd[, mget(colnames(dd))
          ][outcome=='contacts_per_capita_log', change := NA_integer_]
  
  return(dd)
  
}


### A function that mutates the raw results tables to tidy tables. ###

table.dd = function(models, specs, results, dd_type) {
  # INPUTS:
  # models: e.g. c('Bottom 40%', 'Top 40%') or 'Bottom 40%, top 40%'
  # results: the output from results.dd()
  # dd_type: either 'dd1', 'dd2_dd3' or 'ddd'
  
  
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
                       visit_type==type & outcome==otc & dynamic == 0]
      
      
      if(dd_type == 'dd2_dd3') {
        setnames(data, old=c('mod', 'dd_type'), new=c('model', 'mod')) }
      
      if(dd_type %in% c('dd1','dd2_dd3')) {
        
        data = data[, .(pre.mean, estimate, std.error, p.value, change, 
                        trends.mean.lags, change.trends,
                        n.visits, n.munies, mod)]
        
      } else if (dd_type=='ddd') {
        data = data[, .(pre.mean, estimate, std.error, p.value, change, 
                        n.visits, n.munies, mod)]
      }
      
      
      if(otc=='contacts_per_capita_log') {
        
        data[, ':=' (estimate = 100*estimate,
                     std.error = 100*std.error)]
        
        if(dd_type != 'ddd') {
          data[, trends.mean.lags := 100* trends.mean.lags] }
        
      }
      
      
      # Rename:
      setnames(
        data, old=c('pre.mean', 'estimate', 'std.error', 
                    'p.value', 'change', 'trends.mean.lags', 'change.trends',
                    'n.visits', 'n.munies'),
        new=c('Level', 'Estimate', 'Std. error', 
              'P-value', 'Change (%)', 'Estimate (trends)', 
              'Change (%) (trends)', 'Contacts', 'Municipalities'), 
        skip_absent = TRUE
      )
      
      
      if(otc=='contacts_per_capita_log') {
        
        data[, ':=' ('Change (%)' = NULL, Level = NULL)] 
        
        if(dd_type != 'ddd') {
          data[, 'Change (%) (trends)' := NULL] }
        
      }
      
      # Remove subgroup labels from the table:
      data[, mod := NULL]
      
      # Row-bind the estimates from the groups of interest:
      
      row_names = colnames(data)
      
      if(dd_type %in% c('dd1', 'dd2_dd3')) {
        
        group1 = data[1,]
        group2 = data[2,]
        data = data.table('Metric' = rep(row_names, times=2),
                          type = c(group1, group2))
        
      } else if (dd_type == 'ddd') {
        
        group = data[1,]
        data = data.table('Metric' = row_names, type = c(group))
        
      }
      
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
#### 2: DD models: municipal variation in copayments. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will estimate the effects in the following subgroups:

models = list(
  D14 = list('income_decile', c(0:3), NULL, NULL, 'Bottom 40%'), 
  D710 = list('income_decile', c(6:9), NULL, NULL, 'Top 40%'),
  all = list('income_decile', c(0:9), NULL, NULL, 'all')
)

# We will loop over contact type, treatment year, and outcome:

visit_types = c(2, 1, 40) # referrals (3) excluded
treat_year = c(2021)
outcome = c('contacts_per_capita', 'contacts_per_capita_log')
dd_type = 'dd1'

specs = CJ(visit_types, treat_year, outcome, dd_type)


# Estimate the results:
dd1 = results.dd(models=models, specs=specs, data=df)

# Mutate results to tidy tables:
table_dd1 = table.dd(models=c('Bottom 40%', 'Top 40%'), 
                     specs=specs, results=dd1, dd_type = 'dd1')

# The table on all individuals requires a bit more tidying:
table.all = table.dd(models=c('all'), specs=specs, results=dd1, dd_type = 'dd1')
table.all = as.data.table(
  rbind(table.all$contacts_per_capita, table.all$contacts_per_capita_log)
)
table.all = table.all[!is.na(Prescriptions)]


# Save tables:

save.table(table_dd1$contacts_per_capita, output=output_dd1_main,
           label_tex = 'tab:abol_dd_main_visits_per_capita',
           title_tex = 'Abolition: Between--Municipality DD Comparisons.')

save.table(
  table_dd1$contacts_per_capita_log, output=output_dd1_main_log,
  label_tex = 'tab:abol_dd_main_visits_per_capita_log',
  title_tex = 'Abolition: Between--Municipality DD Comparisons, Logarithmized.'
)

save.table(table.all, output=output_dd1_all,
           label_tex = 'tab:abol_dd_all_visits_per_capita',
           title_tex = 'Abolition: Between--Municipality DD Comparisons.')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: DDD models. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We'll estimate the following models:

models = list(
  D14_D710 = list('income_decile', c(0:3), 'income_decile', c(6:9), 
                  "Bottom 40%, top 40%")
)

# We will loop over contact types, treatment year, and outcome:

visit_types = c(2, 1, 40) # referrals (3) are excluded
treat_year = c(2021)
outcome = c('contacts_per_capita', 'contacts_per_capita_log')
dd_type = 'ddd'

specs = CJ(visit_types, treat_year, outcome, dd_type)

                
# Estimate the results:
ddd = results.dd(models=models, specs=specs, data=df)

# Mutate results to tidy tables:
table_ddd = table.dd(models=c('Bottom 40%, top 40%'), 
                     specs=specs, results=ddd, dd_type = 'ddd')


# Save tables:

table_ddd_main = rbind(table_ddd$contacts_per_capita,
                       table_ddd$contacts_per_capita_log)

save.table(table_ddd_main, output=output_ddd_main,
           label_tex = 'tab:abol_ddd_main_visits_per_capita',
           title_tex = 'Abolition: Between--Municipality DDD Comparisons.')

# End.
