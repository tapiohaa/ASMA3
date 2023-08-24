
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
library(dplyr)            # data wrangling, required by HonestDID
library(TruncatedNormal)  # truncated distributions, required by HonestDID
library(HonestDiD)        # Robust inference for DD designs.

# Inputs:
input_data = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output_nurse_visits = "W:/ASMA3/analysis/figures/quality_nurse_visits.pdf"
output_quality_issues = "W:/ASMA3/analysis/figures/quality_issues.pdf"
output_honestdid = "W:/ASMA3/analysis/figures/plot_honestdid.pdf"
output_nurse_use = "W:/ASMA3/analysis/figures/plot_nurse_use.pdf"

###
###

df = readRDS(input_data)

# We do not what that Helsinki's GP visit copayment abolition in 1/2013 
# confounds our results, therefore drop observations before 2013:
df = df[year >= 2013]


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
#### 4: Rambachan & Roth (2023) robust inference. ####
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

# End.
