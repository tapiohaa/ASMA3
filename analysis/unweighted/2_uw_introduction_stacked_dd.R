
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###    r-script 2_uw_introduction_stacked_dd.R    ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze the effects of the staggered copayment introduction
#           by estimating difference-in-differences (DD) and triple 
#           differences (DDD) models on stacked datasets.
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

# To install a package in Fiona:
# 1) go to Data (D:) and open one of the CRAN folders
# 2) copy the package folder manually to folder
#   C:/Program Files/R/R-3.6.2/library.
# 3) load the package with library()
# 4) install also the potential dependencies


# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output_trends = "W:/ASMA3/data/cleaned/uw_intro_trends_data.rds"
output_dyn_plot_pri = "W:/ASMA3/analysis/unweighted/figures/uw_intro_dynamic_plot_pri.pdf"
output_dyn_plot_sec = "W:/ASMA3/analysis/unweighted/figures/uw_intro_dynamic_plot_sec.pdf"
output_dd1_pri = "W:/ASMA3/analysis/unweighted/tables/uw_intro_dd_pri"
output_dd1_sec = "W:/ASMA3/analysis/unweighted/tables/uw_intro_dd_sec"
output_dd1_pri_rob = "W:/ASMA3/analysis/unweighted/tables/uw_intro_dd_pri_rob"
output_ddd_pri = "W:/ASMA3/analysis/unweighted/tables/uw_intro_ddd_pri"
output_results = "W:/ASMA3/data/cleaned/uw_intro_results_stacking.rds"

###
###

df = readRDS(input_data)

# We do not what that Helsinki's GP visit copayment abolition in 1/2013 
# confounds our results, therefore drop observations before 2013:
df = df[year >= 2013]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create shared functions. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract.data.stacking = function(
  data, type, dim_treat, treat, dim_control=NA, control=NA, 
  outcome='contacts_per_capita', visit_type=2, balanced=0) {
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
  # outcome: either 'contacts_per_capita', 'contacts_per_capita_log',
  #       'social_assistance', or 'basic_social_assistance'
  # visit_type: 2 for nurse visits, 1 for GP visits
  # balanced: should each experiment have balanced data (1=yes, 0=no)
  # OUTPUT:
  # a data.table
  
  
  # Municipalities whose policies are observed:
  munies = unique(data[!is.na(treat_intro), municipality])
  DT = data[municipality %in% munies & year < 2020]
  DT[, contacts_per_capita_log := log(contacts_per_capita)]
  DT[, outcome := mget(outcome)]
  setnames(DT, old='relative_time', new='time_no')
  
  # Find the treatment cohort for each treated municipality:
  cohort = DT[treat_intro==1
              ][, .(treat_group = min(time_no)), by='municipality']
  DT = merge(DT, cohort, by='municipality', all.x = TRUE)
  DT[, relative_time := time_no - treat_group]
  
  # Relative times need to merged to the municipality-year panel
  # on social assistance (later).
  rel_times = unique(DT[, mget(c('date', 'time_no'))])
  
  # Impute values for the never treated municipalities:
  DT[is.na(treat_group), ':=' (treat_group = 1000,
                               relative_time = 1000)]
  
  
  # Depending on the outcome, extract relevant data:
  if(!is.null(visit_type)) {
    DT = DT[profession == visit_type]
    
  } else if(outcome=='social_assistance') {
    DT = DT[is.na(profession) & !is.na(date)
            ][, outcome := 100 * outcome / population]
    
  } else if (outcome=='basic_social_assistance') {
    DT = DT[is.na(profession) & is.na(date)
            ][, outcome := outcome / population]
    DT[, ':=' (date = as.Date(paste(as.character(year), '-01-01', sep='')),
               time_no = NULL)]
    DT = merge(DT, rel_times, by='date', all.x=TRUE)
    DT[is.na(relative_time), relative_time := time_no - treat_group]
  }
  
  
  # Next, we will drop municipality-year observations due to issues 
  # in the data quality (see 1_data_quality.R).
  DT = DT[drop_pre_pandemic == 0]
  
  # If outcome is 'contacts_per_capita_log', drop log(0) values:
  
  if(outcome=='contacts_per_capita_log') {
    
    DT = DT[contacts_per_capita != 0] 
    
    keep_obs = DT[, .N, by=c('municipality', 'date')
                  ][N == max(N)
                    ][, keep := 1]
    DT = merge(DT, keep_obs, by=c('municipality', 'date'), all.x = TRUE)
    DT = DT[keep == 1][, keep := NULL]
  }
  
  
  # Treatment cohorts (make sure that each experiment has at least
  # 12 post-reform months):
  if(outcome != 'basic_social_assistance') {
    cohorts = sort(unique(DT[treat_group <= max(time_no) - 11, treat_group]))
    
  } else if (outcome == 'basic_social_assistance') {
    cohorts = unique(DT[treat_group==time_no, treat_group])
  }
  

  # Loop over treatment cohorts to stack the datasets:
  
  dfs = lapply(cohorts, function(cohort) {

    # Keep treated units and all units not treated within -24 to 11:
    data = DT[treat_group==cohort | treat_group >= cohort + 12]
    
    # Keep just months -24 to 11:
    data = data[time_no %in% c((cohort-24):(cohort+11))]
    
    # Create an indicator for the dataset:
    data[, df := cohort]
    
    # If municipality is not in the cohort, impute 1000 for relative time:
    data[treat_group != cohort, relative_time := 1000]
    
    # Take only a balanced panel (if chosen so):
    if(balanced == 1) {
      keep_munies = data[, .N, by=c('municipality')][N == max(N), municipality]
      data = data[municipality %in% keep_munies]
      
      # If there are no treated municipalities, return NULL data.table:
      if(length(unique(data[treat_group==cohort, municipality])) == 0) {
        return(data.table()) }
  
    }
    
    # Create an indicator for post-treatment periods:
    data[, post := as.integer(time_no >= cohort)]
    
    if(outcome == 'basic_social_assistance') {
      data[relative_time != 1000, relative_time := relative_time / 12]
    }
    
    # Create dynamic treatment dummies:
    data[relative_time==1000, relative_time := NA_integer_]
    if(type=='dd1') {
      data = fastDummies::dummy_cols(
        .data=data, select_columns = 'relative_time', ignore_na = TRUE
      )
    }
    
    return(data)
                                                     
  })
  
 
  DT = rbindlist(dfs, fill=TRUE)
  columns = colnames(DT)
  DT = DT[, mget(columns)]
  
  # Fill zeroes to NA values in 'relative_time_':
  if(type=='dd1') {
    cols = colnames(DT)[startsWith(colnames(DT), 'relative_time_')]
    DT[, (cols) := lapply(.SD, nafill, fill=0), .SDcols=cols]
  }
  
  DT[, municipality_df := paste(municipality, df, sep='_')]
  
  
  if(type == 'dd1') {
    
    DT = DT[dimension == dim_treat & group %in% treat
            ][, treat := as.integer(!(is.na(relative_time)))]
    
  } else if (type == 'dd2') {
    
    DT = DT[(!is.na(relative_time)) &
              ((dimension==dim_treat & group %in% treat) | 
                 (dimension==dim_control & group %in% control))
            ][, treat := as.integer(dimension==dim_treat & group %in% treat)]
    
  } else if (type == 'dd3') {
    
    DT = DT[is.na(relative_time) &
              ((dimension==dim_treat & group %in% treat) | 
                 (dimension==dim_control & group %in% control))
            ][, treat := as.integer(dimension==dim_treat & group %in% treat)]
    
  } else if (type == 'ddd') {
    
    DT = DT[(dimension==dim_treat & group %in% treat) |
              (dimension==dim_control & group %in% control)
            # group='treat' for the supposedly more vulnerable population group
            ][, group := as.character(dimension==dim_treat & group %in% treat)
              ][, group := fcase(group == 'FALSE', 'control',
                                 group == 'TRUE', 'treat')
                ][, treat := as.integer(!(is.na(relative_time)))
                  ][, treat_group_post := treat * post * 
                      as.integer(group=='treat')]
    
  }
  
  # Finally, create a variable for time relative to treatment for ALL
  # municipalities, not just the treated municipalities:
  DT[, relative_time := time_no - df]
  
  if(outcome == 'basic_social_assistance') {
    DT[relative_time != 1000, relative_time := relative_time / 12] }
  
  return(DT)
  
}


test0 = extract.data.stacking(
  df, type = 'ddd', outcome='contacts_per_capita',
  dim_treat = 'income_decile', treat = c(0:3), 
  dim_control = 'income_decile', control = c(6:9), visit_type=2, balanced=1
)

test1 = extract.data.stacking(
  df, type = 'dd1', outcome='contacts_per_capita',
  dim_treat = 'income_decile', treat = c(0:3), visit_type=2, balanced=1
)

test2 = extract.data.stacking(
  df, type = 'dd1', outcome='social_assistance',
  dim_treat = 'income_decile', treat = c(0:9), visit_type=NULL, balanced=1
)

test3 = extract.data.stacking(
  df, type = 'dd1', outcome='basic_social_assistance',
  dim_treat = 'income_decile', treat = c(0:9), visit_type=NULL, balanced=1
)


### A function that estimates the results and returns a results table. ###

estimate.stacking = function(data, type) {
  # INPUTS:
  # data: tidied data.table from 'extract.data.stacking()'
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
  lags = paste0("relative_time_", 0:n_lags)
  lags = paste(lags, collapse = ' + ')
  
  
  # The estimation specification depends on the type of the model:
  if(type == 'dd1') {
    
    spec = 'outcome ~ treat:post | factor(municipality):factor(df) + factor(time_no):factor(df) | 0 | municipality'

    spec.dyn =
      paste('outcome ~ ', lags, 
            '+ treat:relative_time:factor(df) | factor(municipality):factor(df) + factor(time_no):factor(df) | 0 | municipality')
    
  } else if (type %in% c('dd2', 'dd3')) {
    spec = 'outcome ~ treat:post | factor(treat):factor(df) + factor(time_no):factor(df) | 0 | municipality'
  } else if (type == 'ddd') {
    spec = 'outcome ~ treat_group_post | factor(treat):factor(df) + factor(group):factor(df) + factor(post):factor(df) + factor(treat):factor(group):factor(df) + factor(treat):factor(post):factor(df) + factor(group):factor(post):factor(df) | 0 | municipality'
  }

  # Estimate the model without a linear pre-trend difference:
  reg = lfe::felm(as.formula(spec), data = DT) #, weights = DT$population)
  reg = setDT(tidy(reg, conf.int=TRUE, se.type='cluster'))
  reg[, dynamic := 0]
  
  
  if(type == 'dd1'){
    
    # Estimate the model with a linear pre-trend difference:
    reg.dyn = lfe::felm(as.formula(spec.dyn), data = DT) #, weights = DT$population)
    reg.dyn = setDT(tidy(reg.dyn, conf.int=TRUE, se.type='cluster'))
    reg.dyn = reg.dyn[startsWith(term, 'relative_time_')
                      ][, t := as.integer(gsub('relative_time_', '', term))]
    reg.dyn[, dynamic := 1]
    
    # The mean of dynamic treatment effects:
    mean_lags = mean(reg.dyn$estimate)
    
    # Concatenate tidy regression tables:
    results = rbind(reg, reg.dyn, fill=TRUE)
    results[, trends.mean.lags := mean_lags]
    
    
  } else if (type %in% c('dd2', 'dd3')) {
    
    results = reg[term=='treat:post'] }
  
  else if (type=='ddd') {
    
    results = reg[term=='treat_group_post'] }
  

  # Next, we will collect relevant data from the estimated model:
  
  # Pre-treatment level in treated group:
  if(type=='ddd') {
    pre_mean = DT[post==0 & treat==1 & group=='treat', 
                  .(level = mean(outcome))]
                  #.(level = weighted.mean(outcome, w=population))]
  } else {
    pre_mean = DT[post==0 & treat==1, 
                  .(level = mean(outcome))]
                  #.(level = weighted.mean(outcome, w=population))]
  }
  
  pre_mean = pre_mean$level
  
  # Sample sizes:
  n_cohorts = length(unique(DT$df))
  n_treated = length(unique(DT[treat==1, municipality]))
  n_munies = length(unique(DT$municipality))
  
  
  results[dynamic==0, ':=' (pre.mean = pre_mean,
                            n.cohorts = n_cohorts,
                            n.treated = n_treated,
                            n.munies = n_munies,
                            change = 100 * estimate / pre_mean)]
  
  if(type == 'dd1') {
    results[dynamic==0, change.trends := 100 * trends.mean.lags / pre_mean]
  }
  
  return(results)
  
}

# With the function:
print(estimate.stacking(test0, type = 'ddd'))
print(estimate.stacking(test1, type = 'dd1'))
print(estimate.stacking(test2, type = 'dd1'))
print(estimate.stacking(test3, type = 'dd1'))


### A function that estimates the result for a set of models and for
# a set of specifications (see Section 4.1 of this script to see examples
# of models and specifications). ###

results.dd = function(data, models, specs) {
  
  dd = lapply(models, function(mod) {
    
    print(mod[[5]])
    
    # Loop over specifications:
    specs = lapply(1:nrow(specs), function(row) {
      
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
      DT = extract.data.stacking(
        data, type = spec$dd_type, outcome = spec$outcomes,
        dim_treat = mod[[1]], treat = mod[[2]],
        dim_control = mod[[3]], control = mod[[4]],
        visit_type = visit_type_param, balanced = spec$balanced
      )
      
      # Estimate the regressions:
      results = estimate.stacking(DT, type = spec$dd_type)
      
      # Add information about the specification:
      results[, ':=' (outcome = spec$outcome,
                      visit_type = visit_type_param2,
                      balanced = spec$balanced,
                      dd_type = spec$dd_type)]
      
      return(results)
    })
    specs = do.call(rbind.data.frame, specs)
    specs = specs[, mget(colnames(specs))]
    specs[, mod := mod[[5]]]
  })
  dd = do.call(rbind.data.frame, dd)
  dd = dd[, mget(colnames(dd))
          ][outcome=='contacts_per_capita_log', 
            ':=' (change = NA_integer_, change.trends = NA_integer_)]
  
  return(dd)
  
}


### A function that mutates the raw results tables to tidy tables. ###

table.dd = function(models, specs, results, dd_type) {
  # INPUTS:
  # models: e.g. c('Bottom 40%', 'Top 40%') or 'Bottom 40%, top 40%'
  # results: the output from results.dd()
  # dd_type: either 'dd1', 'dd2_dd3' or 'ddd'
  
  
  results = results[dynamic==0]
  
  # Collect outcomes and visit types which we will loop over:
  outcome = unique(specs$outcome)
  balanced = sort(unique(specs$balanced), decreasing = TRUE)
  visit_types = sort(unique(specs$visit_types), decreasing = TRUE)
  print(outcome)
  print(balanced)
  print(visit_types)
  
  
  # Loop over whether the dataset is balanced or not:
  table.dd = lapply(balanced, function(df_bal) {
    
    # Loop over outcomes:
    table.outcome = lapply(outcome, function(otc) {
      
      if(is.null(visit_types)) { visit_types = NA } 
      
      # Loop over visit types (nurse or GP):
      table.type = lapply(visit_types, function(type) {
        
        print(type)
        
        # Extract the right results:
        
        if(is.na(type)) {
          data = results[mod %in% models & is.na(visit_type) & 
                           outcome==otc & balanced==df_bal ]
        } else {
          data = results[mod %in% models & visit_type==type & 
                           outcome==otc & balanced==df_bal ]
        }
        
        if(dd_type == 'dd2_dd3') {
          setnames(data, old=c('mod', 'dd_type'), new=c('model', 'mod')) }
        
        if(dd_type == 'dd1') {
          
          data = data[, .(pre.mean, estimate, std.error, p.value, change, 
                          trends.mean.lags, change.trends,
                          n.cohorts, n.treated, n.munies, mod)]
          
        } else if (dd_type %in% c('dd2_dd3', 'ddd')) {
          
          data = data[, .(pre.mean, estimate, std.error, p.value, change, 
                          n.cohorts, n.treated, n.munies, mod)]
          
        }
        
        print(data$mod)
        
        if(otc=='contacts_per_capita_log') {
          
          data[, ':=' (estimate = 100*estimate,
                       std.error = 100*std.error)]
          
          if(dd_type == 'dd1') {
            data[, trends.mean.lags := 100* trends.mean.lags] }
          
        }
        
        
        # Rename:
        setnames(
          data, old=c('pre.mean', 'estimate', 'std.error', 
                      'p.value', 'change', 'trends.mean.lags', 'change.trends',
                      'n.cohorts', 'n.treated', 'n.munies'),
          new=c('Level', 'Estimate', 'Std. error', 
                'P-value', 'Change (%)', 'Estimate (trends)', 
                'Change (%) (trends)', 'Events', 'Treated areas', 'All areas'), 
          skip_absent = TRUE
        )
        
        if(otc=='contacts_per_capita_log') {
          
          data[, ':=' ('Change (%)' = NULL, Level = NULL)] 
          
          if(dd_type == 'dd1') {
            data[, 'Change (%) (trends)' := NULL] }
          
        }
        
        # Remove subgroup labels from the table:
        data[, mod := NULL]
        
        # Row-bind the estimates from the groups of interest:
        
        row_names = colnames(data)
        
        if(dd_type %in% c('dd1', 'dd2_dd3')) {
          
          if(length(models) == 2) {
            
            group1 = data[1,]
            group2 = data[2,]
            data = data.table('Metric' = rep(row_names),
                              col1 = c(group1),
                              col2 = c(group2))
            colnames(data) = c('Metric', models)
            
          } else if (length(models) == 1) {
            
            group = data[1,]
            data = data.table('Metric' = row_names, type = c(group))
          }
          
          
        } else if (dd_type == 'ddd') {
          
          group = data[1,]
          data = data.table('Metric' = row_names, type = c(group))
          
        }
        
        # Change data types to numeric:
        data[, type := as.numeric(type)]
        
        return(data)
      })
      
      
      if(length(models) == 2 & dd_type == 'dd1') {
        
        table.type = setDT(do.call(cbind.data.frame, table.type)[, c(1:3,6:7)])
        cols = colnames(table.type)[2:5]
        table.type[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
        
      } else if (dd_type == 'ddd') {
        
        table.type = do.call(cbind.data.frame, table.type)[, c(1,2,4)]
        colnames(table.type) = c(colnames(table.type)[2], 
                                 'Nurse Visits', 'GP Visits')
        
      } else if (length(models) == 1) {
        
        table.type = do.call(cbind.data.frame, table.type)[, c(1,2)]
        colnames(table.type) = c(colnames(table.type)[2], otc)
      }
      
      
      return(table.type)
      
    })
    names(table.outcome) = outcome
    return(table.outcome)
    
  })
  names(table.dd) = paste0('balanced_', balanced)
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
#### 2: Trends plot with the stacked data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# List the combinations that will be plotted:

models = list(
  list(deciles = c(0:3), visit_type = 2, bottom_d = 1, 
       outcome = 'contacts_per_capita',
       title = 'Bottom 40%: Nurse Visits'),
  list(deciles = c(0:3), visit_type = 1, bottom_d = 1,
       outcome = 'contacts_per_capita',
       title = 'Bottom 40%: GP Visits'),
  list(deciles = c(6:9), visit_type = 2, bottom_d = 0,
       outcome = 'contacts_per_capita',
       title = 'Top 40%: Nurse Visits'),
  list(deciles = c(6:9), visit_type = 1, bottom_d = 0,
       outcome = 'contacts_per_capita',
       title = 'Top 40%: GP Visits'),
  list(deciles = c(0:9), visit_type = NULL, bottom_d = NA,
       outcome = 'social_assistance',
       title = 'Share receiving social assistance'),
  list(deciles = c(0:9), visit_type = NULL, bottom_d = NA,
       outcome = 'basic_social_assistance',
       title = 'Sum of basic social assistance')
)


# Loop over models to extract the stacked datasets:
dfs = lapply(models, function(mod) {
  
  print(mod$title)
  
  # Extract the data:
  data = extract.data.stacking(
    df, type = 'dd1', outcome=mod$outcome,
    dim_treat = 'income_decile', treat = mod$deciles, 
    visit_type=mod$visit_type, balanced=1
  )

  # Aggregate:
  data = data[, .(value = mean(outcome)),
              #.(value = weighted.mean(outcome, population)),
              by=c('relative_time', 'treat')]
  
  # Add columns:
  if(is.null(mod$visit_type)) {
    data[, profession := NA]
  } else {
    data[, profession := mod$visit_type]
  }
  data[, ':=' (bottom_d = mod$bottom_d, outcome = mod$outcome)]

  return(data)
  
})

df_trends = do.call(rbind.data.frame, dfs)

# Save:
saveRDS(df_trends, output_trends)
# These data will be plotted with 2_trend_plots.R.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Dynamic analysis with TWFE regression models. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will plot dynamic regression plots for the bottom 40% and the top 40%
# using visits per capita (nurse visits and GP visits separately) as the 
# outcome. Additionally, we will plot dynamic regression plots using
# social assistance as the outcome.


# Write the estimation formula:

covs.mo = c(paste0("`relative_time_", -24:-2, "`"),
            paste0("relative_time_", 0:11, ""))

form.mo = as.formula(
  paste('outcome ~ ', paste(covs.mo, collapse = ' + '),
        '| factor(municipality):factor(df) + factor(time_no):factor(df) | 0 | municipality_df')
)

covs.yr = c(paste0("`relative_time_", -2, "`"),
            paste0("relative_time_", 0, ""))

form.yr = as.formula(
  paste('outcome ~ ', paste(covs.yr, collapse = ' + '),
        '| factor(municipality):factor(df) + factor(time_no):factor(df) | 0 | municipality_df')
)


# Loop over models:
dfs = lapply(models, function(mod) {
  
  print(mod$title)
  
  # Extract the stacked data:
  data = extract.data.stacking(
    df, type = 'dd1', outcome = mod$outcome,
    dim_treat = 'income_decile', treat = mod$deciles, 
    visit_type = mod$visit_type, balanced=1
  )
  
  # Estimate the dynamic TWFE model:
  if(mod$outcome == 'basic_social_assistance') {
    reg = lfe::felm(form.yr, data=data) #, weights = data$population)
    coefs = data.table(summary(reg)$coefficients)
    coefs[, t := c(-2, 0)]
  } else {
    reg = lfe::felm(form.mo, data=data) #, weights = data$population)
    coefs = data.table(summary(reg)$coefficients)
    coefs[, t := c(-24:-2, 0:11)]
  }
  
  # We create labels for faceting:
  
  if(is.null(mod$visit_type) & mod$outcome == 'social_assistance') {
    type = 'Share receiving social assistance'
  } else if (is.null(mod$visit_type) & 
             mod$outcome == 'basic_social_assistance') {
    type = 'Sum of basic social assistance'
  } else if (mod$visit_type==2) {
    type = 'Nurse visits'
  } else if (mod$visit_type==1) {
    type = 'GP visits' 
  } 
  
  if(is.na(mod$bottom_d)) {
    group = NA
  } else if(mod$bottom_d==1) {
    group = 'Bottom 40%'
  } else if (mod$bottom_d==0) {
    group = 'Top 40%' 
  } 
  
  # Add columns:
  coefs[, ':=' (profession = type,
                bottom_d = group,
                title = mod$title,
                conf_low = Estimate - 
                  qnorm(0.05/2,lower=F) * get('Cluster s.e.'),
                conf_high = Estimate + 
                  qnorm(0.05/2,lower=F) * get('Cluster s.e.'))]
  
  
  # Add the reference period -1:
  
  reference = data.table(
    Estimate=0, conf_low=0, conf_high=0, t=-1,
    profession = type, 
    bottom_d = group)
  
  coefs = rbind(coefs, reference, fill=TRUE)
  
  
  return(coefs)
  
})
dfs = do.call(rbind.data.frame, dfs)


# Order the levels of 'profession' to edit the order of facets:
dfs$profession_f = factor(dfs$profession, 
                          levels=c('Nurse visits', 'GP visits',
                                   'Share receiving social assistance',
                                   'Sum of basic social assistance'))


# Plot:

plot.dynamic = function(data, outcome) {
  
  if(outcome=='pc') {
    results = data[profession %in% c('Nurse visits', 'GP visits')]
    x_title = 'Months from the adoption'
    y_title = 'Ann. contacts per capita'
  } else if (outcome == 'Share receiving social assistance') {
    results = data[profession == outcome]
    x_title = 'Months from the adoption'
    y_title = 'Percentage points'
  } else if (outcome == 'Sum of basic social assistance') {
    results = data[profession == outcome]
    x_title = 'Years from the adoption'
    y_title = 'Euros'
  }
  
  p = ggplot(results, aes(x=t, y=Estimate)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin=conf_low, ymax=conf_high), alpha = 0.1,
                color='black', linetype='dotted') + 
    labs(x = x_title, y = y_title) +
    theme(text = element_text(size=15),     # windowsFonts()
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom") +
    geom_vline(xintercept = -1, linetype="dashed") +
    geom_hline(yintercept = 0, linetype="dashed") 
  
  if(outcome == 'pc') {
    p = p + facet_grid(profession_f ~ bottom_d)
  } else {
    p = p + ggtitle(outcome)
  }
  
  return(p)
    
}

p1 = plot.dynamic(dfs, outcome='pc')
p2 = plot.dynamic(dfs, outcome='Share receiving social assistance')
p3 = plot.dynamic(dfs, outcome='Sum of basic social assistance')


# Save:
cairo_pdf(filename = output_dyn_plot_pri, width = 10.0, height = 8.0)
print(p1) 
dev.off()

cairo_pdf(filename = output_dyn_plot_sec, width = 10.0, height = 4.0)
print(p2 + p3) 
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: Static analysis with TWFE regression models. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4.1: DD models: municipal variation in copayments. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will estimate the effects in the following subgroups:

models_pri = list(
  D14 = list('income_decile', c(0:3), NULL, NULL, 'Bottom 40%'), 
  D710 = list('income_decile', c(6:9), NULL, NULL, 'Top 40%')
)

models_sec = list(
  all = list('income_decile', c(0:9), NULL, NULL, 'all')
)


# For main analysis, we will loop over visit type (nurse / GP), 
# outcome, and balanced/unbalanced dataset:

visit_types = c(2, 1)
balanced = c(1, 0)
outcomes = c('contacts_per_capita', 'contacts_per_capita_log')
dd_type = 'dd1'

specs_pri = CJ(visit_types, balanced, outcomes, dd_type)

balanced = c(1, 0)
outcomes = c('social_assistance', 'basic_social_assistance')
dd_type = 'dd1'

specs_sec = CJ(balanced, outcomes, dd_type)


# Estimate the results:
dd1.pri = results.dd(data=df, models = models_pri, specs = specs_pri)
dd1.sec = results.dd(data=df, models = models_sec, specs = specs_sec)


# Mutate results to tidy tables:
table_dd1_pri = table.dd(models=c('Bottom 40%', 'Top 40%'), 
                         specs=specs_pri, results=dd1.pri, dd_type = 'dd1')
table_dd1_sec = table.dd(models='all', 
                         specs=specs_sec, results=dd1.sec, dd_type = 'dd1')
table_dd1_sec = cbind(
  table_dd1_sec$balanced_1$social_assistance,
  table_dd1_sec$balanced_1$basic_social_assistance)[, c(1,2,4)]


# Save tables:

save.table(
  table_dd1_pri$balanced_1$contacts_per_capita, output=output_dd1_pri,
  label_tex = 'tab:intro_dd_pri',
  title_tex='Adoption: Between--Municipality DD Comparisons, Primary Outcomes.'
)

save.table(
  table_dd1_sec, output=output_dd1_sec, label_tex = 'tab:intro_dd_sec',
  title_tex='Adoption: Between--Municipality DD Comparisons, Secondary Outcomes.'
)

table_pri_rob = rbind(table_dd1_pri$balanced_0$contacts_per_capita,
                      table_dd1_pri$balanced_1$contacts_per_capita_log)

save.table(
  table_pri_rob, output=output_dd1_pri_rob,
  label_tex = 'tab:intro_dd_pri_rob',
  title_tex = 'Adoption: Between--Municipality DD Comparisons, Robustness'
)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4.2: DDD models. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We'll estimate the following models:

models = list(
  D14_D710 = list('income_decile', c(0:3), 'income_decile', c(6:9), 
                  "Bottom 40%, top 40%")
)

# We will loop over visit type (nurse / GP), outcome, and balanced / not
# balanced dataset:

visit_types = c(2, 1)
balanced = c(1, 0)
outcomes = c('contacts_per_capita', 'contacts_per_capita_log')
dd_type = 'ddd'

specs = CJ(visit_types, balanced, outcomes, dd_type)

# Estimate the results:
ddd = results.dd(data=df, models=models, specs=specs)

# Mutate results to tidy tables:
table_ddd = table.dd(models=c('Bottom 40%, top 40%'), 
                     specs=specs, results=ddd, dd_type = 'ddd')

# Save tables:

table_ddd_main = rbind(table_ddd$balanced_1$contacts_per_capita, 
                       table_ddd$balanced_1$contacts_per_capita_log)

table_ddd_unbalanced = rbind(table_ddd$balanced_0$contacts_per_capita, 
                             table_ddd$balanced_0$contacts_per_capita_log)

table_ddd_main = cbind(table_ddd_main, table_ddd_unbalanced)[, c(1:3, 5:6)]

save.table(table_ddd_main, output=output_ddd_pri,
           label_tex = 'tab:intro_ddd_pri',
           title_tex = 'Adoption: Between--Municipality DDD Comparisons.')


# Finally, save all results:
results = rbind(dd1.pri, dd1.sec, ddd, fill=TRUE)
saveRDS(results, output_results)

# End.
