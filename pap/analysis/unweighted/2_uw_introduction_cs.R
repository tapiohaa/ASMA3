
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script 2_uw_introduction_cs.R      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze the effects of the staggered copayment introduction
#           by estimating difference-in-differences (DD) and triple 
#           differences (DDD) models using Callaway and Sant' Anna (2021).
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.
library(did)              # Staggered DID designs.

# To install a package in Fiona:
# 1) go to Data (D:) and open one of the CRAN folders
# 2) copy the package folder manually to folder
#   C:/Program Files/R/R-3.6.2/library.
# 3) load the package with library()
# 4) install also the potential dependencies


# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output = 'W:/ASMA3/data/cleaned/uw_intro_results_cs.rds'
output_dyn_cs_pri_short = 
  "W:/ASMA3/analysis/unweighted/figures/uw_intro_dynamic_plot_cs_pri_short.pdf"
output_dyn_cs_sec_short = 
  "W:/ASMA3/analysis/unweighted/figures/uw_intro_dynamic_plot_cs_sec_short.pdf"
output_att_plot_cs_pri = "W:/ASMA3/analysis/unweighted/figures/uw_intro_att_plot_cs_pri.pdf"
output_att_plot_cs_pri_log = 
  "W:/ASMA3/analysis/unweighted/figures/uw_intro_att_plot_cs_pri_log.pdf"
output_att_plot_cs_sec = "W:/ASMA3/analysis/unweighted/figures/uw_intro_att_plot_cs_sec.pdf"

###
###

df = readRDS(input_data)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create functions for data construction and estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract.data.cs = function(
  data, dim_treat, treat, outcome='contacts_per_capita', 
  visit_type=2, start_year=2014, end_year=2018) {
  # INPUTS:
  # data: 'df'
  # dim_treat: dimension from which the treatment group is extracted
  # treat: the treatment group(s)
  # outcome: either 'contacts_per_capita', 'contacts_per_capita_log',
  #       'social_assistance', or 'basic_social_assistance'
  # visit_type: 2 for nurse visits, 1 for GP visits
  # start_year: year when the study period starts as an integer
  # end_year: year when the study period ends as an integer
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
  
  
  # did::att_gt() assumes that the cohort is 0 for the never-treated units:
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
  
  return(DT)
  
}

test1 = extract.data.cs(
  df, outcome='contacts_per_capita', dim_treat = 'income_decile', 
  treat = c(0:3), visit_type=2, start_year = 2014, end_year=2018
)

test2 = extract.data.cs(
  df, outcome='social_assistance', dim_treat = 'income_decile', 
  treat = c(0:9), visit_type=NULL, start_year = 2014, end_year=2018
)

test3 = extract.data.cs(
  df, outcome='basic_social_assistance', dim_treat = 'income_decile', 
  treat = c(0:9), visit_type=NULL, start_year = 2014, end_year=2018
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
  # (see eq. 2.2 in the paper by Sant' Anna and Zhao, 2020)
  
  reg = did::att_gt(
    yname = 'outcome', 
    tname='time_no',
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
  
  # Aggregate group-time ATTs to event study coefficients:
  agg.es = did::aggte(reg, type = 'dynamic')

  # Collect ES results to a table:
  
  relative_time = agg.es$egt
  estimate = agg.es$att.egt
  std_error = agg.es$se.egt
  conf_low = estimate - qnorm(0.05/2,lower=F) * std_error
  conf_high = estimate + qnorm(0.05/2,lower=F) * std_error
    
  table.es = data.table(
    relative_time=relative_time,
    estimate=estimate, std_error=std_error,
    conf_low=conf_low, conf_high=conf_high,
    event_study = 1
  )
  

  # A weighted average of all group-time ATTs with weights proportional
  # to group size:
  agg.simple = did::aggte(reg, type = 'simple')
  
  # Collect ATT results to a table:
  
  estimate = agg.simple$overall.att
  std_error = agg.simple$overall.se
  conf_low = estimate - qnorm(0.05/2,lower=F) * std_error
  conf_high = estimate + qnorm(0.05/2,lower=F) * std_error
  n_events = reg$DIDparams$nG
  n_munies = reg$DIDparams$n
  
  table.att = data.table(
    estimate=estimate, std_error=std_error, conf_low=conf_low,
    conf_high=conf_high, n_events, n_munies,
    event_study = 0
  )
  
  table = rbind(table.att, table.es, fill=TRUE)
  return(table)
  
}

test = estimate.cs(test1, controls = 'nevertreated')
test
test = estimate.cs(test3, controls = 'nevertreated')
test


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Estimate. ####
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
# comparison group, outcome, start year, and end year:

visit_types = c(2, 1)
controls = c('nevertreated', 'notyettreated')
start_years = c(2013, 2014)
end_years = c(2018, 2019)
outcomes = c('contacts_per_capita', 'contacts_per_capita_log')

specs_pri = CJ(visit_types, controls, start_years, end_years, outcomes)

# Add specification numbers to controls-start_year combinations:
spec_no = CJ(controls, start_years, end_years)
spec_no[, spec_no := c(1:nrow(spec_no))]
specs_pri = merge(specs_pri, spec_no, by=c('controls', 'start_years',
                                           'end_years'), all.x=TRUE)


# For secondary outcomes, we loop over comparison group, outcome, start year,
# and end year.

controls = c('nevertreated', 'notyettreated')
start_years = c(2013)
end_years = c(2018)
outcomes = c('social_assistance', 'basic_social_assistance')

specs_sec = CJ(controls, start_years, end_years, outcomes)

# Add specification numbers to controls-start_year combinations:
spec_no = CJ(controls, start_years, end_years)
spec_no[, spec_no := c(1:nrow(spec_no))]
specs_sec = merge(specs_sec, spec_no, by=c('controls', 'start_years',
                                           'end_years'), all.x=TRUE)


### A function that loops over models and specifications to 
# estimate the results: ###

results.cs = function(data, models, specs) {
  
  cs = lapply(models, function(mod) {
    
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
      DT = extract.data.cs(
        data, outcome = spec$outcomes,
        dim_treat = mod[[1]], treat = mod[[2]],
        visit_type = visit_type_param, start_year = spec$start_years,
        end_year = spec$end_years
      )
      
      # Estimate the regressions:
      results = estimate.cs(DT, controls = spec$controls)
      
      # Add information about the specification:
      results[, ':=' (outcome = spec$outcomes,
                      visit_type = visit_type_param2,
                      start_year = spec$start_years,
                      end_year = spec$end_years,
                      controls = spec$controls,
                      spec_no = spec$spec_no)]
      
      return(results)
    })
    results.specs = do.call(rbind.data.frame, results.specs)
    results.specs = results.specs[, mget(colnames(results.specs))]
    results.specs[, mod := mod[[5]]]
  })
  cs = do.call(rbind.data.frame, cs)
  cs = cs[, mget(colnames(cs))]
  
  return(cs)
  
}

cs.pri = results.cs(data=df, models = models_pri, specs = specs_pri)
cs.sec = results.cs(data=df, models = models_sec, specs = specs_sec)
saveRDS(rbind(cs.pri, cs.sec, fill=TRUE), output)

#cs = readRDS(output)
#cs.pri = cs[outcome %in% c('contacts_per_capita', 'contacts_per_capita_log')]
#cs.sec = cs[outcome %in% c('social_assistance', 'basic_social_assistance')]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3.1: Event study plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that plots event study plots. ###

plot.es = function(data, otc, short_d=1, start_yr) {
  # INPUTS:
  # data: either cs.pri or cs.sec
  # otc: 'contacts_per_capita', 'social_assistance', or 
  #       'basic_social_assistance'
  # short_d: whether we only show the coefficients for two years pre and one
  #       year post the reform.
  # start_yr: year when the study period starts as an integer
  # OUTPUTS:
  # a ggplot object
  
  
  # Extract data:
  DT = data[event_study==1 & controls=='notyettreated' & 
              outcome==otc & start_year==start_yr & end_year==2018]
  
  # The baseline choice is to show periods -24 to 11 (months). An exception
  # is outcome ''basic_social_assistance' with periods -2 to 0 (years).
  if(short_d==1) {
    if(otc=='basic_social_assistance') {
      DT = DT[relative_time %in% c(-2:0)]
    } else { DT = DT[relative_time %in% c(-24:11)] }
  }

  # If our outcome is 'contacts_per_capita' (in primary care), we will plot
  # a faceted plot. To this end, order the levels of 'profession' to 
  # edit the order of facets:
  if(otc=='contacts_per_capita') {
    DT[visit_type == 2, profession := 'Nurse visits']
    DT[visit_type == 1, profession := 'GP visits']
    DT$profession_f = factor(DT$profession, 
                             levels=c('Nurse visits', 'GP visits'))
  }
  
  # Axis titles:
  if(otc=='contacts_per_capita') {
    y_lab = 'Ann. visits per capita'
    x_lab = 'Months from the adoption'
  } else if (otc=='social_assistance') {
    y_lab = 'Percentage points'
    x_lab = 'Months from the adoption'
  } else if (otc=='basic_social_assistance') {
    y_lab = 'Euros per capita' 
    x_lab = 'Years from the adoption'
  }
  
  
  # Plot:
  
  p = ggplot(DT, aes(x=relative_time, y=estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin=conf_low, ymax=conf_high), alpha = 0.1,
                color='black', linetype='dotted') + 
    labs(x = x_lab, y = y_lab) +
    theme(text = element_text(size=15),   
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom") +
    geom_vline(xintercept = -0.5, linetype="dashed") +
    geom_hline(yintercept = 0, linetype="dashed")
  
  if(otc=='contacts_per_capita') {
    p = p + facet_grid(profession_f ~ mod) }
  
  if(short_d==1) {
    p = p + geom_point() }
  
  return(p)
  
}

p1 = plot.es(data=cs.pri, otc='contacts_per_capita', start_yr=2014)
p2 = plot.es(data=cs.sec, otc='social_assistance', start_yr=2013) +
  ggtitle('A. Share receiving social assistance')
p3 = plot.es(data=cs.sec, otc='basic_social_assistance', start_yr=2013) +
  ggtitle('B. Sum of basic social assistance')


# Save:

cairo_pdf(filename = output_dyn_cs_pri_short, width = 10.0, height = 8.0)
print(p1) 
dev.off()

cairo_pdf(filename = output_dyn_cs_sec_short, width = 10.0, height = 4.0)
print(p2 + p3) 
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3.2: Aggregate ATT estimates. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that plots ATT plots. ###

plot.att = function(data, otc, base_spec) {
  # INPUTS:
  # data: either cs.pri or cs.sec
  # otc: 'contacts_per_capita', 'social_assistance', or 
  #       'basic_social_assistance'
  # base_spec: the number of the base specification, highlighted by red.
  # OUTPUTS:
  # a ggplot object
  
  
  # Extract data:
  DT = data[event_study==0 & outcome==otc]
  
  # Create a character variable on sample sizes:
  DT[, N := paste('(', n_events, ') \n [', n_munies, ']', sep='')]

  
  # If our outcome is 'contacts_per_capita' (in primary care), we will plot
  # a faceted plot. To this end, order the levels of 'profession' to 
  # edit the order of facets:
  if(otc %in% c('contacts_per_capita', 'contacts_per_capita_log')) {
    DT[visit_type == 2, profession := 'Nurse visits']
    DT[visit_type == 1, profession := 'GP visits']
    DT$profession_f = factor(DT$profession, 
                             levels=c('Nurse visits', 'GP visits'))
  }

  
  # Axis titles:
  if(otc=='contacts_per_capita') {
    y_lab = 'Ann. visits per capita'
  } else if (otc=='contacts_per_capita_log') {
    y_lab = 'Log ann. visits per capita'
  } else if (otc=='social_assistance') {
    y_lab = 'Percentage points'
  } else if (otc=='basic_social_assistance') {
    y_lab = 'Euros per capita' }
  
  
  # X axis limits and geom_text location:
  if(otc %in% c('contacts_per_capita', 'contacts_per_capita_log')) {
    lmts = c(0,8)
    loc = 1.5
  } else if (otc %in% c('social_assistance', 'basic_social_assistance')) {
    lmts = c(0,2) 
    loc = 2
  }
  
  # Plot:
  
  p = ggplot(DT, aes(x=spec_no, y=estimate, label=N)) +
    geom_point(data=DT[spec_no!=base_spec]) +
    geom_point(data=DT[spec_no==base_spec], color='red') +
    geom_pointrange(data=DT[spec_no!=base_spec], 
                    aes(ymin=conf_low, ymax=conf_high)) +
    geom_pointrange(data=DT[spec_no==base_spec], 
                    aes(ymin=conf_low, ymax=conf_high), color='red') +
    geom_text(hjust=loc) +
    scale_x_continuous(breaks = c(0:8), labels = c('', as.character(1:8)), 
                       limits = lmts) +
    labs(x = 'Specification', y = y_lab) +
    theme(text = element_text(size=15),   
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom") +
    geom_hline(yintercept = 0, linetype="dashed")
  
  if(otc %in% c('contacts_per_capita', 'contacts_per_capita_log')) {
    p = p + facet_grid(profession_f ~ mod) }
  
  return(p)
  
}

p1 = plot.att(data=cs.pri, otc='contacts_per_capita', base_spec=7)
p2 = plot.att(data=cs.pri, otc='contacts_per_capita_log', base_spec=7)
p3 = plot.att(data=cs.sec, otc='social_assistance', base_spec=2) +
  ggtitle('A. Share receiving social assistance')
p4 = plot.att(data=cs.sec, otc='basic_social_assistance', base_spec=2) +
  ggtitle('B. Sum of basic social assistance')


# Save:

cairo_pdf(filename = output_att_plot_cs_pri, width = 12.0, height = 8.0)
print(p1) 
dev.off()

cairo_pdf(filename = output_att_plot_cs_pri_log, width = 12.0, height = 8.0)
print(p2) 
dev.off()

cairo_pdf(filename = output_att_plot_cs_sec, width = 10.0, height = 4.0)
print(p3 + p4) 
dev.off()

# End.
