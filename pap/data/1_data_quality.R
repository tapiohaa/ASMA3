
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_data_quality.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze data quality, and create analysis data:
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.

# Inputs:
input_visits = "W:/ASMA3/data/interim/visit_data_aggr.rds"
input_intro = "W:/ASMA3/data/interim/adoption_policies_placebo.rds" 
# Replace the placebo policies above with actual policies once we registered the 
# pre-analysis plan.
input_abol = "W:/ASMA3/data/interim/abolition_policies.rds" 

# Outputs:
output_plot_pandemic = "W:/ASMA3/analysis/figures/visits_pandemic.pdf"
output_plot_weird_intro_1 = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_introduction_weird_1.pdf"
output_plot_weird_intro_2 = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_introduction_weird_2.pdf"
output_plot_weird_intro_3 = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_introduction_weird_3.pdf"
output_plot_weird_abol = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_abolition_weird.pdf"
output_plot_weird_abol_refs_1 = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_abolition_weird_refs_1.pdf"
output_aggr = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"


###
###


# Read data and drop municipalities on the Åland Islands:
df = readRDS(input_visits)
to_drop = c(478, 60, 65, 76, 170, 736, 771, 43, 417,
            438, 35, 62, 295, 318, 766, 941)
df = df[!(municipality %in% to_drop)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Assess coding rates on whether the care is curative or preventive. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# To extract curative nurse outpatient visits, we need to distinguish 
# curative contacts from preventive contacts. Here, we analyze the coding rate 
# of this variable over time:

data = df[dimension=='income_decile' & profession==2
          ][, missing := as.integer(curative==-1)
            ][, .(contacts = sum(contacts)), 
              by=c('year', 'month', 'missing')]

# Pivot wider:
data = dcast(data, year + month ~ 
               paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing profession:
data[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(data)


# Conclusion: the share of contacts that do not contain information on whether
# the contact is curative or preventive is approximately 7% in 2012 and 
# very close to zero between 1/2013-8/2019. After 9/2019, the share of 
# missing values is approximately 1%. If we extract data with this variable, we
# would plausibly miss some contacts in 2012. Therefore, we choose to start
# our analysis with the primary care outcomes from January 1st 2013.

# Take only curative primary care contacts starting from January 1st 2013:
df = df[profession %in% c(2,1,-1) & curative == 1 | 
          profession %in% c(3, 40, 41, NA_integer_)][, curative := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Analyze the share of missing profession over time. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# First, we mutate data so that we can compute the share of visits 
# with missing profession for primary care contacts:

data = df[dimension=='income_decile' & profession %in% c(2,1,-1)
          ][, missing := as.integer(profession==-1)
            ][, .(contacts = sum(contacts)),
              by=c('year', 'month', 'missing')]

# Pivot wider:
data = dcast(data, year + month ~ 
               paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing profession:
data[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(data)


# Conclusion: The share of contacts with missing profession (or other than
# nurse or GP) has been stable since 2012. However, we exclude year 2012 as
# the share of contacts with missing curative/preventive observation was 
# relatively high in 2012. This only applies to the primary care data:

# Include nurse and GP visits and exclude 2012 from the primary care data:
df = df[profession %in% c(2,1) & year >= 2013 | 
          profession %in% c(3, 40, 41, NA_integer_)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) The evolution of healthcare use during the COVID-19 pandemic. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# When we analyze the copayment abolition in July 2021, we naturally need
# pre-treatment data, but we would not be willing to include the months
# early in the pandemic when the healthcare use was abnormally low for
# several reasons (both demand and supply side causes). 

# Our choice, registered in the pre-analysis plan, is to use a 12-month 
# bandwidth around the policy change. In 3/20-6/20, the no. of nurse visits
# decreased 25 to 45 percent relative to pre-pandemic baseline. Since 7/20,
# the utilization stabilized to a level of approximately -15% reduction
# relative to the pre-pandemic baseline.


# The year when the intervention starts:
yr = 2021
years = c((yr-3):(yr+1))

# Compute the sum of nurse visits by year:
data = df[year %in% years & 
            dimension=='income_decile' & profession==2
          ][, .(contacts = sum(contacts),
                population = sum(population)), by=c('year', 'month', 'date')]


# The number of workdays in a given month vary year to year.
#   We want to account for this supply side variation.
#   1) We compute the number of workdays of each calendar month.
#   2) We divide our outcome measure (contacts per 100,000 residents in month m) 
#       by the number of workdays in that month.
#   3) Finally, we multiply the outcome measure by the mean number of workdays
#       over the study period.

dates = data.table(
  dates = as.Date(
    c(as.Date(paste(as.character(min(years)), '-01-01', sep='')) : 
        as.Date(paste(as.character(max(years)), '-12-31', sep=''))), 
    origin="1970-01-01")
)

# Drop weekends:
dates[, weekday := weekdays(dates)]
dates = dates[!(weekday %in% c('Saturday', 'Sunday'))]

# Drop remaining national holidays:

holidays = c(
  
  "2022-01-06", "2022-04-15", "2022-04-18", "2022-05-26", "2022-06-24",
  "2022-12-06", "2022-12-26",
  
  "2021-01-01", "2021-01-06", "2021-04-02", "2021-04-05", "2021-05-13",
  "2021-06-25", "2021-12-06", "2021-12-24",
  
  "2020-01-01", "2020-01-06", "2020-04-10", "2020-04-13", "2020-01-05", 
  "2020-05-21", "2020-06-19", "2020-12-24", "2020-12-25",
  
  "2019-01-01", "2019-04-19", "2019-04-22", "2019-05-01", "2019-05-30",
  "2019-06-21", "2019-12-06", "2019-12-24", "2019-12-25", "2019-12-26",
  
  "2018-01-01", "2018-03-30", "2018-04-02", "2018-05-01","2018-05-10",
  "2018-06-22", "2018-12-06", "2018-12-24", "2018-12-25", "2018-12-26",
  
  "2017-01-06", "2017-04-14", "2017-04-17", "2017-05-01", "2017-05-25",
  "2017-06-23", "2017-12-06", "2017-12-25", "2017-12-26",
  
  "2016-01-01", "2016-01-06", "2016-03-25", "2016-03-28", "2016-05-05",
  "2016-06-24", "2016-12-06", "2016-12-26",
  
  "2015-01-01", "2015-01-06", "2015-04-03", "2015-04-06", "2015-05-01",
  "2015-05-14", "2015-06-19", "2015-12-24", "2015-12-25",
  
  "2014-01-01", "2014-01-06", "2014-04-18", "2014-04-21", "2014-05-01",
  "2014-05-29", "2014-06-20", "2014-12-24", "2014-12-25", "2014-12-26"
)
holidays = as.Date(holidays)

# Compute the number of workdays in each calendar month:
dates = dates[!(dates %in% holidays)
              ][, ':=' (year = year(dates),
                        month = as.integer(format(dates, "%m")))
                ][, .(workdays = .N), keyby=.(year, month)]

# Compute monthly means over the five-year period:
mns = dates[, .(workdays_mean = mean(workdays)), by='month']

dates = merge(dates, mns, by=c('month'), all.x = TRUE)


# Adjust for the monthly number of workdays:
data = merge(data, dates, by=c('month', 'year'), all.x=TRUE)
data[, ':=' (contacts_per_100k = contacts * (100000 / population ) * 
               (workdays_mean / workdays),
             workdays = NULL, workdays_mean = NULL)
     ][, outcome := 12 * contacts_per_100k / 100000]


# Compute baseline and merge to 'data':
base = data[year %in% years[1:2]
            ][, .(baseline = weighted.mean(outcome, w=population)),
              by='month']
data = data[!(year %in% years[1:2])]
data  = merge(data, base, by='month')

# Compute change (%) relative to baseline:
data[, change := 100 * (outcome - baseline) / baseline]


# Plot trends plot:

plot.trends.gaps = function(df, type) {
  # INPUTS:
  # df: 'data' from above
  # type: either 'trends' or 'gaps'
  # OUTPUT:
  # a ggplot object
  
  
  if(type=='trends') {
    y_lab = 'Ann. visits per capita'
    data = df[, mget(colnames(df))][, outcome := outcome]
  } else if (type=='gaps'){
    data = df[, mget(colnames(df))][, outcome := change]
    y_lab = 'Change (%) from baseline' }
  
  
  p = ggplot(data, aes(x=month, y=outcome, group=as.factor(year), 
                       color=as.factor(year))) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks=1:12) +
    ylab(y_lab) + 
    xlab('Month') +
    theme(text = element_text(size=15),     
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = 'bottom') +
    labs(color = 'Year', group='Year')
  
  if(type=='trends') {
    p = p + geom_line(aes(x=month, y= baseline), 
                      linetype='dashed', color='black')
  } else if (type=='gaps'){
    p = p + geom_hline(yintercept = 0, linetype='dashed') }
  
  return(p)
  
}

trends = plot.trends.gaps(data, type='trends')
gaps = plot.trends.gaps(data, type='gaps')

# Save:

cairo_pdf(filename = output_plot_pandemic, width = 10.0, height = 5.0)
print(trends + gaps + plot_layout(guides='collect') &
        theme(legend.position = 'bottom'))
dev.off()


# We drop observations from 1.1.2020-30.6.2020 due to the start of the
# COVID-19 pandemic and resulting reduction in healthcare use. 
# (The exact dates may change after we have examined the above plots
# with data from 2020).
df = df[date >= as.Date('2020-07-01') |
          date < as.Date('2020-01-01') | is.na(date)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Primary care: data quality issues. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# It is a known issue that in early years of AvoHilmo primary care data 
# collection some areas had problems in transferring data from their IT 
# systems to the national register. This results in very low numbers of 
# visits per capita for some periods (note that this figure does not need 
# to be exactly zero as residents may also have used services in other 
# municipalities). Here, we try to discover those municipality-period pairs 
# so that these municipalities can be dropped from analysis data.


# Impute relative time to the data frame:

years = 2012:2022

time = data.table(
  month = rep(c(1:12), times=length(years)),
  year = rep(years, each=12))
time$relative_time = c(1:nrow(time))

df = merge(df, time, by=c('year','month'), all.x = TRUE)


# First, aggregate nurse and GP visits at the municipality-month level:

data = df[profession %in% c(2,1) & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)), 
            by=c('municipality', 'relative_time', 'month', 'year',
                 'date', 'profession')
            ][, .(contacts = sum(contacts, na.rm=TRUE),
                  population = mean(population, na.rm=TRUE)), 
              by=c('municipality', 'relative_time', 'month', 'year', 'date')
              ][population > 0, 
                contacts_per_capita := 12 * contacts / population]


# We will proceed in the following way:
# 	1) For each municipality, compute a distribution of mean contacts
#			by dropping every combination of four consecutive months.
#	2) Take the largest mean.
#	3) Mark an observation "weirdly low" if it is less than 40 % of the mean.
# Note that we will do this separately for 1.1.2013-31.12.2019 and
# 1.7.2020-30.6.2022. For the pandemic period, we may have to adjust the 40 %
# threshold.


find.weird.obs = function(data, period, threshold=0.40, treatment_year=2018) { # use 2019 in actual analysis
  # INPUTS:
  # data: 'data' from above
  # period: either 'pandemic' (for the abolition) or 'pre-pandemic'
  #         (for the staggered adoption)
  # threshold: mark an observation "weirdly low" if it is less than 
  #         X % of the largest mean, X determined by the threshold.
  # treatment_year: in the PAP, we analyse a placebo intervention
  #         that occurred earlier in time. The year depends on the outcome.
  # OUTPUT:
  # a list containing a mutated copy of 'data' and a table containing the
  # weird municipality-year observations.
  
  
  # Extract the relevant subset of data:
  if(period=='pre-pandemic') {
    
    df = data[date < as.Date('2020-01-01')]
    
  } else if (period=='pandemic') {
    
    # Use a 1-year bandwidth around the treatment_year:
    start_month = as.Date(paste(treatment_year-1, '-07-01', sep=''))
    end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))
    
    df = data[date >= start_month & date <= end_month] 
  
  } 
  
  
  # Create combinations of four consecutive months:
  
  min_month = min(df$relative_time)
  months = max(df$relative_time) - 3
  municipalities = unique(df$municipality)
  combinations = lapply(c(min_month:months), function(int) { 
    vec = c(0:3) + int 
  })
  print(combinations[[1]])
  
  
  # Compute the largest mean for every municipality:
  
  means = lapply(municipalities, function(muni) {
    
    avr = lapply(combinations, function(combo) {
      table = df[municipality==muni & !(relative_time %in% combo), 
                 .(avr = mean(contacts_per_capita, na.rm = TRUE))]
    })
    avr = max(unlist(avr))
    
    table = data.table(municipality=muni, avr_largest=avr)
    
  })
  means = do.call(rbind.data.frame, means)
  
  
  # Merge 'means' to 'df' and find 'weirdly' low values:
  
  df = merge(df, means, by='municipality', all.x=TRUE)
  df[, weird := (contacts_per_capita - avr_largest) / avr_largest
     ][, weird_60 := as.integer(weird < -(1-threshold) & month != 7)]
  
  # These municipality-year pairs will be dropped:
  weird_60 = unique(df[weird_60 == 1, mget(c('municipality', 'year', 
                                             'weird_60'))])
  setnames(weird_60, old='weird_60', new='drop')
  
  # In many cases, weird values are observed in 2013:
  print(weird_60[, .N, by='year'])
  
  return(weird_60)
  
}

pre_pandemic = find.weird.obs(data=data, period = 'pre-pandemic')
pandemic = find.weird.obs(data=data, period = 'pandemic', treatment_year = 2018) # use 2021 in actual analysis


# Pre-pandemic:

weird_munies_pre = as.character(unique(pre_pandemic[, municipality]))
normal_munies_pre = as.character(setdiff(unique(data$municipality), 
                                         weird_munies_pre))

# Out of the 293 municipalities in mainland Finland, 86 have weird
# municipality-year observations that are dropped:
length(weird_munies_pre)
length(normal_munies_pre)


# Pandemic:

weird_munies = as.character(unique(pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, 13 have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)


# Merge the drop indicators to 'data':

data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')

data = merge(data, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pandemic')


# For each municipality, we will plot the evolution in GP visits per capita:

plot.problems = function(data, period, treatment_year=2018) { # use 2021 in actual analysis
  # INPUTS:
  # data: 'data' from above
  # period: either 'pandemic' (for the abolition) or 'pre-pandemic'
  #         (for the staggered adoption)
  # treatment_year: in the PAP, we analyse a placebo intervention
  #         that occurred earlier in time. The year depends on the outcome.
  # OUTPUT:
  # a list of ggplot objects
  
  
  # Copy the data and store the vector of municipalities:
  df = data[, mget(colnames(data))]
  municipalities = sort(unique(df$municipality))
  
  if(period=='pre-pandemic') {
    
    df[, drop := drop_pre_pandemic]
    df = df[date < as.Date('2020-01-01')]
    
  } else if (period=='pandemic') {
    
    df[, drop := drop_pandemic]
    
    # Use a 1-year bandwidth around the treatment_year:
    start_month = as.Date(paste(treatment_year-1, '-07-01', sep=''))
    end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))
    
    df = df[date >= start_month & date <= end_month] 
    
    # In real analysis: 
    # df[date >= as.Date('2020-07-01') & date < as.Date('2022-07-01')] 
    
  }
  
  
  plots = lapply(municipalities, function(muni) {
    
    # Initiate a plot:
    
    p = ggplot2::ggplot() +
      scale_x_continuous(breaks = seq(1, length(c(2012:2023)) * 12, by=12),
                         labels = as.character(c(12:23))) + 
      ggtitle(muni) + 
      theme(text = element_text(size=15),     
            axis.text.x = element_text(hjust = 1),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) 
    
    
    # Highlight by pink background those municipality-year pairs that contain 
    # weird months:
    
    DT = df[municipality == muni & drop == 1 & month %in% c(1, 6, 7, 12), 
            mget(c('year', 'relative_time')) ]
    
    if(nrow(DT) > 0) {
      DT = DT[, .(min = min(relative_time),
                  max = max(relative_time)), by='year']
      
      p = p +
        geom_rect(data = DT, 
                  mapping = aes(xmin=min, xmax=max, ymin=-Inf, ymax=Inf, 
                                fill='pink'), alpha=0.5) +
        guides(fill = "none")
      
    }
    
    # Finally, plot the evolution of GP visits:
    
    p = p +
      geom_line(data = df[municipality == muni], 
                mapping = aes(x=relative_time, y=contacts_per_capita),
                colour="black")
    
    return(p)
    
  })
  
  
  names(plots) = municipalities
  return(plots)
  
}

plots_pre_pandemic = plot.problems(data=data, period = 'pre-pandemic')
plots_pandemic = plot.problems(data=data, period = 'pandemic',
                               treatment_year = 2018) # use 2021 in actual analysis


print( patchwork::wrap_plots(plots_pre_pandemic[normal_munies_pre][1:30], 
                             ncol=6, byrow=TRUE) )


# Save weird municipalities as plots:

cairo_pdf(filename = output_plot_weird_intro_1, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre][1:30], 
                             ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_weird_intro_2, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre][31:60], 
                             ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_weird_intro_3, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre][61:86], 
                             ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_weird_abol, width = 12.5, height = 7.5)
print( patchwork::wrap_plots(plots_pandemic[weird_munies], 
                             ncol=5, byrow=TRUE) )
dev.off()


# Finally, merge data on observations to be dropped to 'df:

df_visits = df[profession %in% c(2,1)]

df_visits = merge(df_visits, pre_pandemic, by=c('municipality', 'year'), 
                  all.x=TRUE)
setnames(df_visits, old='drop', new='drop_pre_pandemic')

df_visits = merge(df_visits, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_visits, old='drop', new='drop_pandemic')

# Tidy some variables:
df_visits[is.na(drop_pandemic), drop_pandemic := 0]
df_visits[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Referrals: data quality issues. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# The aggregate number of referrals almost doubled between 2012-18 which
# is a clear sign that the coding rate w.r.t. the arrival of the referral
# was bad in early years of our panel. As we extract contacts with that
# variable, we'll end up with missing referrals. Let's further examine the 
# quality issues before making conclusions.


# First, aggregate referrals at the municipality-month level:

data = df[profession == 3 & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)), 
            by=c('municipality', 'relative_time', 'month', 'year',
                 'date')
            ][population > 0, contacts_per_capita := 12 * contacts / population]


# Find weird observations defined by the algorithm:
# Use a higher-than-default threshold to make the algorithm more sensitive:
pre_pandemic = find.weird.obs(data=data, period = 'pre-pandemic',
                              threshold=0.6)
pandemic = find.weird.obs(data=data, period = 'pandemic',
                          threshold=0.6, treatment_year = 2019) # use 2021 in actual analysis


# Pre-pandemic:

weird_munies_pre = as.character(unique(pre_pandemic[, municipality]))
normal_munies_pre = as.character(setdiff(unique(data$municipality), 
                                         weird_munies_pre))

# Out of the 293 municipalities in mainland Finland, 210 have weird
# municipality-year observations that are dropped:
length(weird_munies_pre)
length(normal_munies_pre)


# Pandemic:

weird_munies = as.character(unique(pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, 18 have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)


# Merge the drop indicators to 'data':

data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')

data = merge(data, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pandemic')


# Plot:
plots_pre_pandemic = plot.problems(data=data, period = 'pre-pandemic')
plots_pandemic = plot.problems(data=data, period = 'pandemic',
                               treatment_year = 2019)


# Show some of the plots:

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre][1:30], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[normal_munies_pre][1:30], 
                             ncol=6, byrow=TRUE) )


cairo_pdf(filename = output_plot_weird_abol_refs_1, width = 15.0, height = 7.5)
print( patchwork::wrap_plots(plots_pandemic[weird_munies][1:18], 
                             ncol=6, byrow=TRUE) )
dev.off()


# Conclusions: We clearly observe missing referrals. Most of the weird 
# observations caught up by our algorithm are from 2013 and 2014. However,
# the algorithm does not appear to work as well as with the primary care
# contacts. The reason is that with primary care contacts the periods with 
# missing observations have values much closer to zero. Many of the issues
# are not detected by the algorithm. The data appear to be more stable and,
# thus, more reliable after 2017. We decide to use the data to analyze the 
# copayment abolition in a two year window around 1.7.2021, but do not use
# the data to analyze the staggered copayment adoption.


# Finally, merge data on observations to be dropped to 'df:

df_refs = df[profession == 3]

df_refs = merge(df_refs, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_refs, old='drop', new='drop_pre_pandemic')

df_refs = merge(df_refs, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_refs, old='drop', new='drop_pandemic')

df_refs[is.na(drop_pandemic), drop_pandemic := 0]
df_refs[date <= as.Date('2017-06-01'), drop_pre_pandemic := 1]
df_refs[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Prescriptions: data quality issues. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# First, aggregate prescriptions at the municipality-month level:

data = df[profession == 40 & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)), 
            by=c('municipality', 'relative_time', 'month', 'year', 'date')
            ][population > 0, contacts_per_capita := 12 * contacts / population]


# Find weird observations defined by the algorithm:
pandemic = find.weird.obs(data=data, period = 'pandemic',
                          treatment_year = 2019) # use 2021 in actual analysis


# Pandemic:

weird_munies = as.character(unique(pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, none have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)


# Merge the drop indicators to 'data':

data[, drop_pre_pandemic := 0]
data = merge(data, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pandemic')


# Plot:
plots_pandemic = plot.problems(data=data, period = 'pandemic',
                               treatment_year = 2019) # use 2021 in actual analysis


# Show some of the plots:

print( patchwork::wrap_plots(plots_pandemic[normal_munies][1:30], 
                             ncol=6, byrow=TRUE) )


# Finally, merge data on observations to be dropped to 'df:

df_drugs = df[profession %in% c(40, 41)]
df_drugs[, drop_pre_pandemic := 0]

df_drugs = merge(df_drugs, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_drugs, old='drop', new='drop_pandemic')

df_drugs[is.na(drop_pandemic), drop_pandemic := 0]
df_drugs[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Social assistance: data quality issues. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# We are not aware that there are similar problems with the social assistance
# data than there were with the primary care data. Still, we run the same 
# algorithm to look for potentially weird observations.


# First, aggregate social assistance recipients at the municipality-month level:

data = df[is.na(profession) & !is.na(date) & dimension == 'income_decile'
          ][, .(contacts = sum(social_assistance, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)), 
            by=c('municipality', 'relative_time', 'month', 'year', 'date')
            ][population > 0, contacts_per_capita := contacts / population]

# Find weird observations:
pre_pandemic = find.weird.obs(data=data, period = 'pre-pandemic')


# Pre-pandemic:

weird_munies_pre = as.character(unique(pre_pandemic[, municipality]))
normal_munies_pre = as.character(setdiff(unique(data$municipality), 
                                         weird_munies_pre))

# Out of the 293 municipalities in mainland Finland, 19 are singled out
# by the algorithm:
length(weird_munies_pre)
length(normal_munies_pre)

# Merge the drop indicators to 'data':

data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')

# Plot and print:

plots_pre_pandemic = plot.problems(data=data, period = 'pre-pandemic')

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre], 
                             ncol=6, byrow=TRUE) )


# Conclusion: even if 19 municipalities were singled out by the algorithm,
# our assessment that only one of these had clear data quality issues
# (municipality 892 in 2016). We exclude that observation, but keep others:
df_benefit = df[is.na(profession) & !is.na(date)]
df_benefit[municipality == 892 & year == 2016, drop_pre_pandemic := 1]
df_benefit[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]

df_benefit_yr = df[is.na(profession) & is.na(date)][, drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Read and merge data on copayment policies. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Concatenate df_visits, df_refs,df_drugs, df_benefit, and df_benefit_yr:
df = rbind(df_visits, df_refs, df_drugs, df_benefit, df_benefit_yr, fill=TRUE)

# Read data on copayment policies:
intro = readRDS(input_intro)
abol = readRDS(input_abol)

# Merge into 'df':
df = merge(df, abol, by.x='municipality', 
           by.y='municipality_code', all.x = TRUE)
df = merge(df, intro, by.x=c('municipality', 'date'), 
           by.y=c('municipality_code', 'date'), all.x = TRUE)

# Save:
saveRDS(df, output_aggr)

# End.
