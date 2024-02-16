
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_data_quality.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2023 by T. Haaga                ###
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
input_intro = "W:/ASMA3/data/interim/adoption_policies.rds" 
input_abol = "W:/ASMA3/data/interim/abolition_policies.rds" 
input_copay_gp = "W:/ASMA3/data/interim/copayments_gp.rds"

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
output_plot_normal_intro = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_introduction_normal.pdf"
output_plot_normal_abol = 
  "W:/ASMA3/analysis/figures/visits_quality_aggr_abolition_normal.pdf"
#output_plot_weird_abol_refs_1 = 
#  "W:/ASMA3/analysis/figures/visits_quality_aggr_abolition_weird_refs_1.pdf"
output_aggr = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"
output_trends_aggr = "W:/ASMA3/analysis/figures/visits_trends_aggr.pdf"

###
###


# Read data and drop municipalities on the Åland Islands:
df = readRDS(input_visits)
to_drop = c(478, 60, 65, 76, 170, 736, 771, 43, 417,
            438, 35, 62, 295, 318, 766, 941)
df = df[!(municipality %in% to_drop)]

# Currently, we have sufficient data to the end of 5/2022:
df = df[!(year==2022 & month %in% c(6:12))]


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


# Observations: the share of contacts that do not contain information on whether
# the contact is curative or preventive is approximately 7% in 2012 and 
# very close to zero between 1/2013-8/2019. Between 9/2019 and 4/2021, the share
# of missing values is approximately 1%. After that, the share of missing
# values is 3 to 5 percent. 

# What causes these changes after 8/2019? The answer is the Apotti EHR system.
# Vantaa (92) adopted it in 5/2019 and Helsinki (91), Kauniainen (235) and
# Kerava (245) in 4/2021. After the adoptions, the share of missing observations
# increase in these areas.

# If we exclude these municipalities after the Apotti adoption, the coding
# rate is good in 2013-2022.

data = 
  df[dimension=='income_decile' & profession==2 
     ][!(municipality==92 & (year > 2019 | (year==2019 & month %in% c(5:12))))
       ][!(municipality %in% c(91, 235, 245) & 
             (year==2022 | year==2021 & month %in% c(4:12)))
         ][, missing := as.integer(curative==-1)
           ][, .(contacts = sum(contacts)), by=c('year', 'month', 'missing')]

# Pivot wider:
data = dcast(data, year + month ~ 
               paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing profession:
data[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(data)

# Create a new profession value 25 for preventive nurse visits:
df[profession==2 & curative==0, profession := 25]


# For the analysis of the adoption, we start our analysis with the primary care
# outcomes from 1/2013 due to lower coding rates before that.

# Take only curative primary care contacts starting from January 1st 2013:
df = df[profession %in% c(2,1,-1) & curative == 1 | 
          profession %in% c(25, 3, 40, 41, 50, 60, NA_integer_)
        ][, curative := NULL]


# The Apotti areas need to be dropped from analyses starting from the month in
# which they adopt Apotti. It turns out that our algorithm in Section 3, that is 
# to detect abnormally low primary care use, is able to detect Vantaa in the 
# pre-pandemic period (1/13-12/19) and Helsinki, Kauniainen and Kerava in the 
# pandemic period (7/20-6/22). So, they will be dropped shortly. However, we 
# need to manually drop Vantaa from the pandemic period which we will do in 
# Section 3 of this script.


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
# nurse or GP) has varied between 4.2% and 9.8% between 2012 and 2022.

# Consistent with earlier choices, include nurse and GP visits and exclude 2012 
# from the primary care data:
df = df[profession %in% c(2,1,25) & year >= 2013 | 
          profession %in% c(3, 40, 41, 50, 60, NA_integer_)]


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
# the utilization has partly recovered but is still 10-15 % below the 
# pre-pandemic levels.


# The year when the intervention starts:
yr = 2021
years = c((yr-3):(yr+1))

# Compute the sum of nurse visits by year.
# Apotti municipalities c(91, 92, 235, 245) are excluded because the number
# of nurse visits mechanically decreases in them due to lower coding rates
# of the curative/preventive indicator after the adoption:

data = df[year %in% years & 
            dimension=='income_decile' & profession==2
          ][!(municipality %in% c(91, 92, 235, 245))
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
                ][, .(workdays = .N), by=c('year', 'month')]

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
                       shape=as.factor(year))) +
    geom_line(color='grey40') +
    geom_point(size=4) + 
    scale_x_continuous(breaks=1:12) +
    ylab(y_lab) + 
    xlab('Month') +
    theme(text = element_text(size=20),     
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = 'bottom') +
    guides(linetype='none') + 
    labs(shape = 'Year', group='Year')
  
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

cairo_pdf(filename = output_plot_pandemic, width = 15.0, height = 7.5)
print(trends + gaps + plot_layout(guides='collect') &
        theme(legend.position = 'bottom'))
dev.off()


# We drop observations from 1.1.2020-30.6.2020 due to the start of the
# COVID-19 pandemic and resulting reduction in healthcare use. 
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

# We also observe that there are some weird spike-shaped increases in visits 
# for some municipalities. We also try to detect and drop these municipality-
# period pairs.


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
# 1) For each municipality, compute a distribution of mean contacts
#		by dropping every combination of four consecutive months.
#	2) Take the largest mean.
#	3) Mark an observation "weirdly low" if it is less than X% of the mean.
# 4) Compute again a distribution of mean contacts by dropping every 
#   combination of four consecutive months AFTER excluding the "weirdly low"
#   observations.
# 5) Take the largest mean.
# 6) Mark an observation "weirdly high" if it is more than 120+X% of the mean.
# Note that we will do this separately for 1.1.2013-31.12.2019 and
# 1.7.2020-30.6.2022. 


find.weird.obs = function(data, period, threshold=0.40, treatment_year=2021) {
  # INPUTS:
  # data: 'data' from above
  # period: either 'pandemic' (for the abolition) or 'pre-pandemic'
  #         (for the staggered adoption)
  # threshold: mark an observation "weirdly low" if it is less than 
  #         X % of the largest mean, X determined by the threshold. Mark an
  #         observation "weirdly high" if it is more than 20 + X % of the
  #         largest mean (after excluding weirdly low observations)
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
     ][, weird_d := as.integer(weird < -(1-threshold) & month != 7)
       ][, avr_largest := NULL]

  
  # Compute again the largest mean for every municipality. This time,
  # exclude the weirdly low observations before computing the means:
  
  means = lapply(municipalities, function(muni) {
    
    avr = lapply(combinations, function(combo) {
      table = df[municipality==muni & !(relative_time %in% combo) & 
                   weird_d != 1, 
                 .(avr = mean(contacts_per_capita, na.rm = TRUE))]
    })
    avr = max(unlist(avr))
    
    table = data.table(municipality=muni, avr_largest=avr)
    
  })
  means = do.call(rbind.data.frame, means)
  
  
  # Merge 'means' to 'df' and find 'weirdly' large values:
  
  df = merge(df, means, by='municipality', all.x=TRUE)
  df[, weird_high := (contacts_per_capita - avr_largest) / avr_largest
     ][, weird_d_high := as.integer(weird_high > (1-threshold+0.20))]
  
  # These municipality-year pairs will be dropped:
  weird = df[weird_d == 1 | weird_d_high == 1][, drop := 1]
  weird = unique(weird[, mget(c('municipality', 'year', 'drop'))])
  
  # In many cases, weird values are observed in 2013:
  print(weird[, .N, by='year'])
  
  return(weird)
  
}

pre_pandemic = find.weird.obs(data=data, period = 'pre-pandemic', 
                              threshold = 0.40)
pandemic = find.weird.obs(data=data, period = 'pandemic', treatment_year = 2021,
                          threshold = 0.55) 


# Pre-pandemic:

weird_munies_pre = as.character(unique(pre_pandemic[, municipality]))
normal_munies_pre = as.character(setdiff(unique(data$municipality), 
                                         weird_munies_pre))

# Out of the 293 municipalities in mainland Finland, 90 have weird
# municipality-year observations that are dropped:
length(weird_munies_pre)
length(normal_munies_pre)


# Pandemic:

weird_munies = as.character(unique(pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, 8 have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)


# Merge the drop indicators to 'data':

data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')

data = merge(data, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pandemic')


# For each municipality, we will plot the evolution in GP visits per capita:

plot.problems = function(data, period, treatment_year=2021, y_lim=c(0,9)) {
  # INPUTS:
  # data: 'data' from above
  # period: either 'pandemic' (for the abolition) or 'pre-pandemic'
  #         (for the staggered adoption)
  # treatment_year: in the PAP, we analyse a placebo intervention
  #         that occurred earlier in time. The year depends on the outcome.
  # y_lim: limits for the y-axis.
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
    
  }
  
  
  plots = lapply(municipalities, function(muni) {
    
    # Initiate a plot:
    
    p = ggplot2::ggplot() +
      scale_x_continuous(breaks = seq(1, length(c(2012:2023)) * 12, by=12),
                         labels = as.character(c(12:23))) + 
      ggtitle(muni) + 
      ylim(y_lim[1], y_lim[2]) + 
      theme(text = element_text(size=20),     
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) 
    
    
    # Highlight by yellow background those municipality-year pairs that contain 
    # weird months:
    
    DT = df[municipality == muni & drop == 1 & month %in% c(1, 5, 6, 7, 12), 
            mget(c('year', 'relative_time')) ]
    
    if(nrow(DT) > 0) {
      DT = DT[, .(min = min(relative_time),
                  max = max(relative_time)), by='year']
      
      p = p +
        geom_rect(data = DT, 
                  mapping = aes(xmin=min, xmax=max, ymin=-Inf, ymax=Inf, 
                                alpha=0.5), fill='grey') +
        guides(alpha = "none")
      
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
                               treatment_year = 2021, y_lim = c(0,8)) 


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
print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre][61:90], 
                             ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_weird_abol, width = 15, height = 7.5)
print( patchwork::wrap_plots(plots_pandemic[weird_munies],
                             ncol=4, byrow=TRUE) )
dev.off()


# Save normal municipalities as plots:

cairo_pdf(filename = output_plot_normal_intro, width = 25, height = 52.5)
print( patchwork::wrap_plots(plots_pre_pandemic[normal_munies_pre],
                             ncol=10, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_normal_abol, width = 25, height = 72.5)
print( patchwork::wrap_plots(plots_pandemic[normal_munies],
                             ncol=10, byrow=TRUE) )
dev.off()


# We also manually drop Vantaa from the pandemic period because it was not 
# detected by the algorithm of this section. Vantaa needs to be excluded from
# the pandemic period because the number of curative nurse visits is 
# artificially low because the curative/preventive categorical has been often
# missing after the municipality adopted the Apotti EHR system in 5/2019.

pandemic = rbind(pandemic, data.table(municipality=92, year=2022, drop=1))


# Finally, merge data on observations to be dropped to 'df':

df_visits = df[profession %in% c(2,1,25)]

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

# Currently, we only observe referrals from visits that have actually occurred.
# As wait times are often some months, we have a lot of missing referrals in
# 2022. For this reason, we do not use referrals as an outcome in this study.
# The plan is to later include if if we get a new batch of data in 2023.


# First, aggregate referrals at the municipality-month level:

data = df[profession == 3 & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)), 
            by=c('municipality', 'relative_time', 'month', 'year',
                 'date')
            ][population > 0, contacts_per_capita := 12 * contacts / population]


# Find weird observations defined by the algorithm:
# Use a higher-than-default threshold to make the algorithm more sensitive:
pandemic = find.weird.obs(data=data, period = 'pandemic',
                          threshold=0.7, treatment_year = 2021) 

# Pandemic:
weird_munies = as.character(unique(pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, all have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)

# Merge the drop indicators to 'data':
data = merge(data, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pandemic')


# Plot:
#plots_pandemic = plot.problems(data=data, period = 'pandemic',
#                               treatment_year = 2021)

#cairo_pdf(filename = output_plot_weird_abol_refs_1, width = 15.0, height = 7.5)
#print( patchwork::wrap_plots(plots_pandemic[weird_munies][1:17], 
#                             ncol=6, byrow=TRUE) )
#dev.off()


# Finally, merge data on observations to be dropped to 'df:

df_refs = df[profession == 3][, drop_pre_pandemic := 1]

df_refs = merge(df_refs, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_refs, old='drop', new='drop_pandemic')

df_refs[is.na(drop_pandemic), drop_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) ED visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# For ED visits and inpatient spells, we use only 2013-2019:

df = df[profession %in% c(50, 60) & year %in% c(2013:2019) | 
          profession %in% c(2, 1, 25, 3, 40, 41, NA_integer_)]


# First, aggregate ED visits at the municipality-month level:

data = df[profession == 50 & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)),
            by=c('municipality', 'relative_time', 'month', 'year', 'date')
            ][population > 0, contacts_per_capita := 12 * contacts / population]


# Find weird observations defined by the algorithm:
# 0.30 is more conservative than the 0.4 for the primary care analysis.
pre_pandemic = find.weird.obs(data=data, period = 'pre-pandemic', 
                              threshold = 0.30)

# Pandemic:
weird_munies = as.character(unique(pre_pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, 57 have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)

# Merge the drop indicators to 'data':
data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')


# Plot:
plots_pre_pandemic = plot.problems(data=data, period = 'pre-pandemic',
                                   y_lim = c(0, 0.75))

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][1:30], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][31:57], 
                             ncol=6, byrow=TRUE) )


# Finally, merge data on observations to be dropped to 'df':

df_ed = df[profession == 50][, drop_pandemic := 1]

df_ed = merge(df_ed, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_ed, old='drop', new='drop_pre_pandemic')

df_ed[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Inpatient episodes. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# First, aggregate inpatient admissions at the municipality-month level:

data = df[profession == 60 & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)),
            by=c('municipality', 'relative_time', 'month', 'year', 'date')
            ][population > 0, contacts_per_capita := 12 * contacts / population]


# Find weird observations defined by the algorithm:
# 0.30 is more conservative than the 0.4 for the primary care analysis.
pre_pandemic = find.weird.obs(data=data, period = 'pre-pandemic', 
                              threshold = 0.30)

# Pandemic:
weird_munies = as.character(unique(pre_pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, 182 have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)

# Merge the drop indicators to 'data':
data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')


# Plot:
plots_pre_pandemic = plot.problems(data=data, period = 'pre-pandemic',
                                   y_lim = c(0, 0.1))

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][1:30], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][31:60], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][61:90], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][91:120], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][121:150], 
                             ncol=6, byrow=TRUE) )

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies][151:180], 
                             ncol=6, byrow=TRUE) )


# Conclusion: the data on inpatient admissions is just too noisy (small N)
# for our algorithm to detect weird observations. For this reason, we
# do not drop any observations for "quality reasons", assuming that the 
# potential data quality issues are uncorrelated with the copayment adoption.

df_inp = df[profession == 60][, ':=' (drop_pandemic=1, drop_pre_pandemic=0)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Prescriptions: data quality issues. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# First, aggregate prescriptions at the municipality-month level:

data = df[profession == 40 & dimension == 'income_decile'
          ][, .(contacts = sum(contacts, na.rm=TRUE),
                population = sum(population, na.rm=TRUE)), 
            by=c('municipality', 'relative_time', 'month', 'year', 'date')
            ][population > 0, contacts_per_capita := 12 * contacts / population]


# Find weird observations defined by the algorithm:
pandemic = find.weird.obs(data=data, period = 'pandemic',
                          treatment_year = 2021, threshold = 0.6)

# Pandemic:
weird_munies = as.character(unique(pandemic[, municipality]))
normal_munies = as.character(setdiff(unique(data$municipality), weird_munies))

# Out of the 293 municipalities in mainland Finland, none have weird
# municipality-year observations that are dropped:
length(weird_munies)
length(normal_munies)


# Merge the drop indicators to 'data':

data[, drop_pre_pandemic := 1]
data = merge(data, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pandemic')

# Finally, merge data on observations to be dropped to 'df:

df_drugs = df[profession == 40]
df_drugs[, drop_pre_pandemic := 0]

df_drugs = merge(df_drugs, pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(df_drugs, old='drop', new='drop_pandemic')

df_drugs[is.na(drop_pandemic), drop_pandemic := 0]
df_drugs[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 8) Social assistance: data quality issues. ####
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
pre_pandemic = find.weird.obs(data=data, period='pre-pandemic', threshold = 0.4)


# Pre-pandemic:

weird_munies_pre = as.character(unique(pre_pandemic[, municipality]))
normal_munies_pre = as.character(setdiff(unique(data$municipality), 
                                         weird_munies_pre))

# Out of the 293 municipalities in mainland Finland, 24 are singled out
# by the algorithm:
length(weird_munies_pre)
length(normal_munies_pre)

# Merge the drop indicators to 'data':
data = merge(data, pre_pandemic, by=c('municipality', 'year'), all.x=TRUE)
setnames(data, old='drop', new='drop_pre_pandemic')

# Plot and print:

plots_pre_pandemic = 
  plot.problems(data=data, period = 'pre-pandemic', y_lim = c(0,0.05))

print( patchwork::wrap_plots(plots_pre_pandemic[weird_munies_pre], 
                             ncol=6, byrow=TRUE) )


# Conclusion: even if 24 municipalities were singled out by the algorithm,
# our assessment that only one of these had clear data quality issues
# (municipality 892 in 2016). We exclude that observation, but keep others:
df_benefit = df[is.na(profession) & !is.na(date)]
df_benefit[municipality == 892 & year == 2016, drop_pre_pandemic := 1]
df_benefit[is.na(drop_pre_pandemic), drop_pre_pandemic := 0]

df_benefit_yr = df[is.na(profession) & is.na(date)][, drop_pre_pandemic := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 9) Read and merge data on copayment policies. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Concatenate df_visits, df_refs, df_ed, df_inp, df_drugs, df_benefit, and 
# df_benefit_yr:
df = rbind(df_visits, df_refs, df_ed, df_inp, df_drugs, df_benefit, 
           df_benefit_yr, fill=TRUE)

# Read data on copayment policies:
intro = readRDS(input_intro)
abol = readRDS(input_abol)

# Merge into 'df':
df = merge(df, abol, by.x='municipality', 
           by.y='municipality_code', all.x = TRUE)
df = merge(df, intro, by.x=c('municipality', 'date'), 
           by.y=c('municipality_code', 'date'), all.x = TRUE)

# Read data on GP visit copayments and merge into 'df':
copay = readRDS(input_copay_gp)
df = merge(df, copay, by=c('municipality', 'date'), all.x = TRUE)

# Save:
saveRDS(df, output_aggr)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 10) Plot trends in aggregate visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Extract and aggregate the data:

df.aggr = df[year < 2020 & profession %in% c(1, 2) & drop_pre_pandemic==0
             ][, .(contacts_per_capita = 
                     weighted.mean(contacts_per_capita, w=population)),
                  by=c('year', 'profession')
                  ][, profession := as.factor(profession)]

df.aggr[profession==1, Profession := 'GP']
df.aggr[profession==2, Profession := 'Nurse']
df.aggr$Profession = factor(df.aggr$Profession, levels = c('Nurse', 'GP'))


# Plot:

p = ggplot(data=df.aggr, aes(x=year, y=contacts_per_capita, group=Profession,
                         shape=Profession)) +
  geom_line(color='grey40') +
  geom_point(size=4) +
  ylab('Visits per capita') + 
  ylim(0.7, 1.2) + 
  theme(text = element_text(size=20),   
        axis.text.x = element_text(hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.position = "bottom",
        axis.title.x = element_blank())

# Save:

cairo_pdf(filename = output_trends_aggr, width = 7.5, height = 7.5)
print(p)
dev.off()

# End.
