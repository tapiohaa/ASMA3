
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script 3_additional_analyses.R      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Additinal analyses proposed in the review process.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.
library(fixest)           # linear fixed effects estimation.
library(lfe)              # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.
library(dplyr)            # data wrangling, required by HonestDID
library(TruncatedNormal)  # truncated distributions, required by HonestDID
library(HonestDiD)        # Robust inference for DD designs.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file. 

# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"
input_folk = 
  "W:/ASMA3/data/interim/folk_data_20" # folk_data_20XX.csv, XX in 13:19
input_visits = "W:/ASMA3/data/interim/visits_20" # visits_20XX.csv, XX in 13:18
input_visits_late = "W:/ASMA3/data/interim/visits_public_20" # visits_public_XX.csv, XX=19


# Outputs:
output_nurse_use_deciles = "W:/ASMA3/analysis/figures/plot_nurse_use_deciles.pdf"
output_nurse_visits = "W:/ASMA3/analysis/figures/quality_nurse_visits.pdf"
output_quality_issues = "W:/ASMA3/analysis/figures/quality_issues.pdf"
output_honestdid = "W:/ASMA3/analysis/figures/plot_honestdid.pdf"
output_nurse_use = "W:/ASMA3/analysis/figures/plot_nurse_use.pdf"
output_means_sds = "W:/ASMA3/analysis/tables/intro_means_sds"

###
###

df = readRDS(input_data)

# We do not what that Helsinki's GP visit copayment abolition in 1/2013 
# confounds our results, therefore drop observations before 2013:
df = df[year >= 2013]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 0: Public primary care use by income decile. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

years = c(13:19)


# Read socioeconomic data:

folk = lapply(years, function(yr) {
  print(yr)

  source = paste(input_folk, yr, ".csv", sep="")
  df = data.table::fread(source, select = c('id', 'age', 'income_eq'))
  df[, year := 2000 + yr]
})
folk = do.call(rbind.data.frame, folk)

# No missing values.
colMeans(is.na(folk))

# Percentage of rows where the family equivalized income is exactly zero:
100 * nrow(folk[income_eq==0]) / nrow(folk)

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


# Read nurse and GP visits

visits = lapply(years, function(yr) {
  print(yr)
  
  if(yr >= 19) {
    source = paste(input_visits_late, yr, ".rds", sep="")
    df = setDT(readRDS(source))
  } else {
    source = paste(input_visits, yr, ".csv", sep="")
    df = data.table::fread(source)
  }
  
  # Take only curative nurse visits:
  df = df[curative==1 & profession==2]
  
  # Aggregate at the ID-year-profession-curative-level:
  df = df[, .(contacts = .N), by=c('id', 'year')]
  
})
visits = do.call(rbind.data.frame, visits)

# Merge: 
visits = visits[, .(contacts = sum(contacts)), by=c('id', 'year')]
dt = merge(folk, visits, by=c('id', 'year'), all.x=TRUE)
dt[is.na(contacts), contacts := 0]
dt[age < 65, group := 'Aged 25 to 64']
dt[age >= 65, group := 'Aged 65 or more']


# First, compute means by income decile:
dt.1 = dt[, .(otc = mean(contacts)), by=c('income_decile')]
dt.1[, group := 'Entire population']

# Second, compute means by income decile separately for the two age groups:
dt.2 = dt[, .(otc = mean(contacts)), by=c('income_decile', 'group')]

dt = rbind(dt.1, dt.2)
dt[, income_decile_int := as.integer(income_decile)]


# Plot:

p = ggplot(data=dt, aes(x=income_decile_int, y=otc)) +
  geom_bar(stat='identity', fill='grey', color='black') +
  geom_text(aes(label = round(otc, digits=2)), vjust = -0.5, size=3) +
  labs(y='Annualized nurse visits per capita', x='Income decile') +
  ylim(0, 1.05 * dt[, max(otc)]) +
  scale_x_continuous(breaks=c(1:10)) + 
  theme(text = element_text(size=20),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major = element_line(size=0.25, linetype='solid', 
                                        colour='lightgrey'),
        panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                        colour='lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, size=0.5)) +
  facet_wrap(~ factor(group, 
                      levels = c('Entire population', 'Aged 25 to 64',
                                 'Aged 65 or more')))

# Save:
cairo_pdf(filename = output_nurse_use_deciles, width = 15, height = 6)
print(p)
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: The relation of the quality issues and the copayment adoption. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract.data.stacking.quality = function(
  data, dim_treat, treat, outcome='contacts_per_capita', years=c(2013:2019)) {
  # INPUTS:
  # data: 'df'
  # dim_treat: dimension from which the treatment group is extracted
  # treat: the treatment group(s)
  # outcome: 'contacts_per_capita'
  # OUTPUT:
  # a data.table
  
  
  # Municipalities whose policies are observed:
  munies = unique(data[!is.na(treat_intro), municipality])
  DT = data[municipality %in% munies & year %in% years]
  DT[, outcome := mget(outcome)]
  setnames(DT, old='relative_time', new='time_no')
  
  # Find the treatment cohort for each treated municipality:
  cohort = DT[treat_intro==1
              ][, .(treat_group = min(time_no)), by='municipality']
  DT = merge(DT, cohort, by='municipality', all.x = TRUE)
  DT[, relative_time := time_no - treat_group]
  
  # Impute values for the never treated municipalities:
  DT[is.na(treat_group), ':=' (treat_group = 1000,
                               relative_time = 1000)]
  
  # Extract relevant data:
  DT = DT[profession == 2] # nurse visits
  
  # Each experiment should have at least 12 post-reform (follow-up) months:
  cohorts = 
    sort(unique(DT[treat_group <= max(time_no) - (12-1), treat_group]))
  
  
  # Loop over treatment cohorts to stack the datasets:
  
  dfs = lapply(cohorts, function(cohort) {
    
    # Keep treated units and all units not treated within -24 to 11:
    data = DT[treat_group==cohort | treat_group >= cohort + 12]
    
    # Keep just months -24 to 11:
    data = data[time_no %in% c((cohort-24):(cohort+(12-1)))]
    
    # Create an indicator for the dataset:
    data[, df := cohort]
    
    # If municipality is not in the cohort, impute 1000 for relative time:
    data[treat_group != cohort, relative_time := 1000]
    
    # Create an indicator for post-treatment periods:
    data[, post := as.integer(time_no >= cohort)]
    data[relative_time==1000, relative_time := NA_integer_]
    
    return(data)
    
  })
  

  DT = rbindlist(dfs, fill=TRUE)
  columns = colnames(DT)
  DT = DT[, mget(columns)]
  
  DT[, municipality_df := paste(municipality, df, sep='_')]
  
  
  DT = DT[dimension == dim_treat & group %in% treat
          ][, treat := as.integer(!(is.na(relative_time)))]
  
  # Finally, create a variable for time relative to treatment for ALL
  # municipalities, not just the treated municipalities:
  DT[, relative_time := time_no - df]
  
  
  return(DT)
  
}


### Create a function that plots trend plots. ###

plot.treatcontrol.quality = function(data, type) {
  # INPUTS:
  # data: 'DT'
  # type: 'raw', 'smooth' or 'diff'
  # OUTPUT:
  # a trend plot
  
  
  # Extract the right data:
  DT = data[, mget(colnames(data))]
  
  # Treatment groups as factors:
  DT[treat==1, treat.f := 'Treatment']
  DT[treat==0, treat.f := 'Comparison']
  DT[, treat.f := factor(DT$treat.f, levels=c('Treatment', 'Comparison'))]
  
  # Next, we proceed to plotting:
  
  x_title = "Months relative to the adoption" 
  y_title = 'Quality issues'
  p_title = 'The Correlation of Quality Issues and Copayment Adoption'
  
  
  if(type %in% c('raw', 'smooth')) {
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=value, linetype=treat.f)
    ) +
      ggtitle(p_title) +
      labs(x = x_title, y = y_title) +
      theme(text = element_text(size=20),   
            axis.text.x = element_text(hjust = 1),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill= NA, size = 0.5),
            legend.position = "bottom") +
      geom_vline(xintercept = -0.5, linetype="dashed")
    
    
    if (type=="smooth") {
      
      p = p + geom_smooth(se=FALSE, color='black') + 
        scale_linetype_discrete(name='Municipalities')
      
    } else if (type=="raw") { 
      
      p = p + geom_line(size=1) +
        scale_linetype_discrete(name='Municipalities') 
      
    }
    
    
  } else if (type == 'diff') {
    
    
    # Mutate the data:
    DT = dcast(DT, relative_time ~ treat.f, value.var = 'value' )
    DT[, value := Treatment-Comparison]
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=value)
    ) +
      geom_line(aes(colour='Raw')) +
      geom_point(aes(colour='Raw')) + 
      geom_smooth(se=FALSE, aes(colour='Smoothed'))+
      scale_colour_manual(
        name='Difference', values = c('grey40', 'black'),
        guide = guide_legend(override.aes = list(shape = c(19, NA),
                                                 linetype = c(1, 1)))) + 
      ggtitle(p_title) +
      labs(x = x_title, y = y_title) +
      theme(text = element_text(size=20),    
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
    
  }
  
  
  return(p)
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Trend plots without accounting for quality issues. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Extract data:
DT = extract.data.stacking.quality(
  df, outcome='contacts_per_capita', dim_treat = 'income_decile', treat = c(0:9)
)

# Aggregate:
DT = DT[, .(value = weighted.mean(outcome, w=population)),
        by=c('relative_time', 'treat')]

# Plot:
p.raw = plot.treatcontrol.quality(DT, type='raw') +
  ggtitle('Raw data') + ylab('Ann. contacts per capita')
p.smooth =plot.treatcontrol.quality(DT, type='smooth') + 
  ggtitle('Smoothed') + ylab('Ann. contacts per capita')
p.diff = plot.treatcontrol.quality(DT, type='diff') +
  ggtitle('Difference (T-C)') + ylab('Ann. contacts per capita')

# Save:
cairo_pdf(filename = output_nurse_visits, width = 15.0, height = 4.5)
print(wrap_elements(
  panel = p.smooth + p.raw + guides(linetype='none') +
    plot_layout(ncol = 2, guides = 'collect') & 
    theme(text = element_text(size=20), legend.position = 'bottom')) +
    wrap_elements(
      panel = p.diff + plot_layout(ncol = 1, guides = 'collect') & 
        theme(text = element_text(size=20), legend.position = 'bottom')) +
    plot_layout(ncol = 2, widths = c(2, 1)))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: The correlation of quality issues and the copayment adoption. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Data from 2013-2019:
DT.1 = extract.data.stacking.quality(
  df, outcome='drop_pre_pandemic', dim_treat = 'income_decile', treat = c(0:9)
)
DT.1 = DT.1[, .(value = weighted.mean(outcome, w=population)),
            by=c('relative_time', 'treat')]

# Data from 2014-2019:
DT.2 = extract.data.stacking.quality(
  df, outcome='drop_pre_pandemic', dim_treat = 'income_decile', treat = c(0:9),
  years = c(2014:2019)
)
DT.2 = DT.2[, .(value = weighted.mean(outcome, w=population)),
            by=c('relative_time', 'treat')]

# Data from 2015-2019:
DT.3 = extract.data.stacking.quality(
  df, outcome='drop_pre_pandemic', dim_treat = 'income_decile', treat = c(0:9),
  years = c(2015:2019)
)
DT.3 = DT.3[, .(value = weighted.mean(outcome, w=population)),
            by=c('relative_time', 'treat')]

# Plot:
p.1 = plot.treatcontrol.quality(DT.1, type='raw') + ylim(0, 0.11) +
  ggtitle('Data from 2013-2019')
p.2 = plot.treatcontrol.quality(DT.2, type='raw') + ylim(0, 0.11) +
  ggtitle('Data from 2014-2019')
p.3 = plot.treatcontrol.quality(DT.3, type='raw') + ylim(0, 0.11) +
  ggtitle('Data from 2015-2019')

# Save:
cairo_pdf(filename = output_quality_issues, width = 15.0, height = 4.5)
print(p.1 + p.2 + p.3 + plot_layout(ncol = 3, guides = 'collect') & 
        theme(text = element_text(size=20), legend.position = 'bottom'))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: Rambachan & Roth (2021) robust inference. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# First, tidy the data:

DT = df[profession==2  &
          !is.na(treat_abol) & 
          dimension == 'income_decile' &
          date >= as.Date('2020-07-01') & 
          date <= as.Date('2022-06-01')]

DT[, ':=' (outcome = contacts_per_capita,
           treat = treat_abol)]

# Drop observations due to issues in the visit data quality:
drop_munies = unique(DT[drop_pandemic==1, municipality])
DT = DT[!(municipality %in% drop_munies)]

# Create an indicator for post-treatment periods:
DT[, post := as.integer(date >= as.Date('2021-07-01'))]

# Center relative_time at the treatment date:
treat_time = DT[post==1, min(relative_time)]
DT[, relative_time := relative_time - treat_time]

# We will loop over the following subgroups:
groups = list('Bottom 40%' = c(0:3), 'Top 40%' = c(6:9))


# Estimate and plot:

results.rr = lapply(c(1:length(groups)), function(i) {
  
  # Extract the data:
  DT.i = DT[group %in% groups[[i]]]
  
  # Estimate the event-study-specification:
  reg = summary(
    fixest::feols(
      outcome ~ i(relative_time, treat, ref=-1) | municipality + relative_time, 
      weights = DT.i$population, data=DT.i
    )
  )
  
  # Create l_vec to define the parameter of interest, the 
  # average of post-treatment effects:
  l_vector = rep(1/11, times=11)
  
  # Construct robust confidence intervals for Delta^{RM}(M):
  robust = HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = reg$coefficients, sigma = reg$cov.scaled,
    numPrePeriods=11, numPostPeriods=11, l_vec = l_vector,
    Mbarvec = c(0.05, 0.10, 0.15), alpha=0.05
  )
  
  # Transform estimates to percentage changes:
  robust = as.data.table(robust)
  pre_mean = DT.i[treat==1 & relative_time < 0, 
                  weighted.mean(outcome, w=population)]
  robust[, ':=' (lb_change = 100 * lb / pre_mean,
                 ub_change = 100 * ub / pre_mean)]

  # Original results:
  robust.orig = HonestDiD::constructOriginalCS(
    betahat = reg$coefficients, sigma = reg$cov.scaled,
    numPrePeriods=11, numPostPeriods=11, l_vec = l_vector
  )
  robust.orig = as.data.table(robust.orig)
  robust.orig[, ':=' (lb_change = 100 * lb / pre_mean,
                      ub_change = 100 * ub / pre_mean,
                      Mbar = 0)]
  
  robust = rbind.data.frame(robust, robust.orig)
  print(robust)
  
  # M vector and labels for plotting:
  m_vector = c(0, 0.05, 0.10, 0.15)
  m_labels = c('Orig.', '0.05', '0.10', '0.15')
  
  
  # Plot:
  
  p.level = ggplot(data=robust, aes(x=Mbar, ymin=lb, ymax=ub)) + 
    geom_errorbar() +
    geom_hline(yintercept = 0, linetype='dashed') +
    ylab('Effect: Nurse Visits') + xlab('Relative Magnitudes Restriction') + 
    scale_x_continuous(breaks=m_vector, labels = m_labels) +
    ggtitle(names(groups)[i]) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill='white', colour='white'),
          panel.grid.major = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
  p.change = ggplot(data=robust, aes(x=Mbar, ymin=lb_change, ymax=ub_change)) + 
    geom_errorbar() +
    geom_hline(yintercept = 0, linetype='dashed') +
    ylab('Effect: Change in Nurse Visits (%)') + 
    xlab('Relative Magnitudes Restriction') + 
    scale_x_continuous(breaks=m_vector, labels = m_labels) +
    ggtitle(names(groups)[i]) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill='white', colour='white'),
          panel.grid.major = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
  return(list(data=robust, level=p.level, change=p.change))
  
})


# Save plots:

cairo_pdf(filename = output_honestdid, width = 15, height = 10)
print(results.rr[[1]]$level + results.rr[[2]]$level + 
        results.rr[[1]]$change + results.rr[[2]]$change)
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5: Plots on nurse use relative to GP use. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Extract the right data:
DT = df[year %in% c(2013:2019) & 
          profession %in% c(2, 1) &
          dimension == 'income_decile' & 
          drop_pre_pandemic == 0]

# Aggregate at the municipality-profession level:
DT = DT[, .(otc = weighted.mean(contacts_per_capita, w=population),
            population = mean(population)), 
        by=c('municipality', 'profession')]

# Pivot wider:
DT = dcast(DT, municipality + population ~ paste0('type', profession), 
           value.var = 'otc')
DT[, ratio_nurse := type2 / type1]


# Plot a scatter plot of nurse and GP use by municipality:

p.1 = ggplot(data=DT, aes(x=type1, y=type2)) + 
  geom_point() +
  labs(title = 'Scatter Plot: Nurse and GP Visits',
       x = 'GP visits per capita',
       y = 'Nurse visits per capita') + 
  geom_smooth(method = 'lm', color='black', se=FALSE) +
  geom_segment(x=0, y=0, xend=4, yend=4, linetype='dashed') +
  ylim(0, 3.5) + 
  theme(text = element_text(size=20),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major = element_line(size=0.25, linetype='solid', 
                                        colour='lightgrey'),
        panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                        colour='lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, size=0.5))


# Plot the distribution of the ratio of nurse and GP visits per municipality

p.2 = ggplot(data=DT, aes(x=ratio_nurse)) + 
  geom_histogram(color='white', fill='grey45') + 
  labs(title = 'Histogram: Ratio of Nurse and GP Visits',
       x = 'Ratio of nurse and GP visits per capita',
       y = 'Municipalities') + 
  theme(text = element_text(size=20),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major = element_line(size=0.25, linetype='solid', 
                                        colour='lightgrey'),
        panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                        colour='lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, size=0.5))

# Save:
cairo_pdf(filename = output_nurse_use, width = 15, height = 5)
print(p.1 + p.2)
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6: Outcome means and standard deviations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns an analysis dataset. ###

extract.data.stacking = function(
  data, type, dim_treat, treat, dim_control=NA, control=NA, 
  outcome='contacts_per_capita', visit_type=2, balanced=0, follow_up=12) {
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
  # follow_up: follow-up as months; 12 (baseline) or 24 (robustness).
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
  # 12 (or 24) post-reform (follow-up) months):
  if(outcome != 'basic_social_assistance') {
    cohorts = 
      sort(unique(DT[treat_group <= max(time_no) - (follow_up-1), treat_group]))
    
  } else if (outcome == 'basic_social_assistance') {
    cohorts = unique(DT[treat_group==time_no, treat_group])
  }
  
  
  # Loop over treatment cohorts to stack the datasets:
  
  dfs = lapply(cohorts, function(cohort) {
    
    # Keep treated units and all units not treated within -24 to 11 (or 23):
    data = DT[treat_group==cohort | treat_group >= cohort + follow_up]
    
    # Keep just months -24 to 11 (or 24):
    data = data[time_no %in% c((cohort-24):(cohort+(follow_up-1)))]
    
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


test1 = extract.data.stacking(
  df, type = 'dd1', outcome='contacts_per_capita',
  dim_treat = 'income_decile', treat = c(0:3), visit_type=2, balanced=1
)


### A function that calculates weighted variance. ###

weighted.var = function(x, w) sum(w * (x - weighted.mean(x, w)) ^ 2) / sum(w)


### A function that estimates the results and returns a results table. ###

estimate.stacking.supp = function(data) {
  # INPUTS:
  # data: tidied data.table, dd1, from 'extract.data.stacking()'
  # OUTPUTS:
  # regression results as a data.table
  
  # Copy the data to not mutate the original source:
  cols = colnames(data)
  DT = data[, mget(cols)]
  
  # The regression model:
  spec = 'outcome ~ treat:post | factor(municipality):factor(df) + factor(time_no):factor(df) | 0 | municipality'
  
  # Estimate the model without a linear pre-trend difference:
  reg = lfe::felm(as.formula(spec), data = DT, weights = DT$population)
  reg = setDT(tidy(reg, conf.int=TRUE, se.type='cluster'))
  
  # Compute weighted mean and standard deviation:
  mean.sd = DT[, .(mean = weighted.mean(outcome, w=population),
                   sd = sqrt(weighted.var(outcome, w=population))),
               by=c('treat', 'post')]
  
  # Tidy results:
  mean.sd = melt(mean.sd, id.vars = c('treat', 'post'),
                 measure.vars = c('mean', 'sd'))
  mean.sd = dcast(mean.sd, treat + variable ~ post,
                  value.var = 'value')
  mean.sd = mean.sd[order(-treat)]
  setnames(mean.sd, old=c('0', '1'), new=c('Pre', 'Post'))
  
  # Collect estimates to a table:
  estimates = data.table(
    'Post' = unlist(c(
      reg$estimate,
      100 * reg$estimate / mean.sd[treat==1 & variable=='mean', 'Pre'],
      reg$estimate / mean.sd[treat==1 & variable=='sd', 'Pre']
      ))
  )
  
  # Bind rows:
  results = rbind(mean.sd, estimates, fill=TRUE)
  
  # Tidy row names:
  results[, Metric := c('Treated mean', 'Treated SD',
                        'Control mean', 'Control SD',
                        'Effect', 
                        'Relative effect (%)',
                        'Std. effect size')]
  
  results = results[, .(Metric, Pre, Post)]
  return(results)
  
}

print(estimate.stacking.supp(test1))


# Loop over profession and income group:

models = list(
  list(deciles = c(0:3), visit_type = 2, bottom_d = 1, 
       title = 'Bottom 40%: Nurse Visits'),
  list(deciles = c(0:3), visit_type = 1, bottom_d = 1,
       title = 'Bottom 40%: GP Visits'),
  list(deciles = c(6:9), visit_type = 2, bottom_d = 0,
       title = 'Top 40%: Nurse Visits'),
  list(deciles = c(6:9), visit_type = 1, bottom_d = 0,
       title = 'Top 40%: GP Visits')
)

dfs = lapply(models, function(mod) {
  
  print(mod$title)
  
  # Extract the data:
  data = extract.data.stacking(
    df, type = 'dd1', outcome='contacts_per_capita',
    dim_treat = 'income_decile', treat = mod$deciles, 
    visit_type=mod$visit_type, balanced=1
  )
  
  # Estimate the results:
  results = estimate.stacking.supp(data)
  return(results)
  
})


# Tidy and save the table:
table = cbind(rbind(dfs[[1]], dfs[[2]]),
              rbind(dfs[[3]], dfs[[4]]))
table = table[, c(1:3, 5:6)]


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

# Save tables:

save.table(
  table, output=output_means_sds,
  label_tex = 'tab:intro_means_sds',
  title_tex='Adoption: Between--Municipality DD Comparisons, Primary Outcomes.'
)

# End.
