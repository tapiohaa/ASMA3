
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script 2_uw_trend_plots.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot the evolution of primary care use to assess pre-trends.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.

# Inputs:
input_intro = "W:/ASMA3/data/cleaned/uw_intro_trends_data.rds"
input_abol = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output_intro_nurse = "W:/ASMA3/analysis/unweighted/figures/uw_intro_trends_plot_nurse.pdf"
output_intro_gp = "W:/ASMA3/analysis/unweighted/figures/uw_intro_trends_plot_gp.pdf"
output_intro_sa = "W:/ASMA3/analysis/unweighted/figures/uw_intro_trends_plot_sa.pdf"
output_abol_nurse = "W:/ASMA3/analysis/unweighted/figures/uw_abol_trends_plot_nurse.pdf"
output_abol_gp = "W:/ASMA3/analysis/unweighted/figures/uw_abol_trends_plot_gp.pdf"
output_abol_drugs = "W:/ASMA3/analysis/unweighted/figures/uw_abol_trends_plot_drugs.pdf"
output_abol_refs = "W:/ASMA3/analysis/unweighted/figures/uw_abol_trends_plot_refs.pdf"


###
###


# Read datasets:

intro = readRDS(input_intro)
intro[outcome == 'social_assistance', profession := 50]
intro[outcome == 'basic_social_assistance', profession := 60]

abol = readRDS(input_abol)
abol = abol[!is.na(treat_abol) & !is.na(profession)]


# Use a 1-year bandwidth around the treatment_year. For primary care visits
# and referrals, use 2018 as the placebo treatment year. For prescriptions,
# use 2019. 

# However, first take 23 pre-treatment months to drop those municipalities that 
# adopted the copayment less than 12 months before the start of the study 
# window. Note that this is already done in 0_copayment_policies.R for the
# final analysis. For the time placebo analysis, however, we need to do that
# here.

treatment_year = 2018 # use 2021 in the actual analysis.
start_month = as.Date(paste(treatment_year-2, '-08-01', sep='')) # -1 and -07-01 in the actual analysis
end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))

abol1 = abol[profession %in% c(2,1) & date >= start_month & date <= end_month]
abol1[, treat_year :=  treatment_year]

treatment_year = 2019 # use 2021 in the actual analysis.
start_month = as.Date(paste(treatment_year-2, '-08-01', sep='')) # -1 and -07-01 in the actual analysis
end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))

abol2 = abol[profession %in% c(3, 40,41) & date >= start_month & date <= end_month]
abol2[, treat_year :=  treatment_year]

# Extract the right data:
abol = rbind(abol1, abol2)
abol = abol[dimension=='income_decile' & group %in% c(0:3, 6:9)
            ][, bottom_d := as.integer(group %in% c(0:3))]


# Drop municipalities for whom we observe a policy change in the 
# study window or less than 12 months before it. 
# This is not done for the window around the actual treatment
# year as we have already taken care of it in 0_copayment_policies.R.

keep_munies = abol[, .(policies = length(unique(treat_intro))), 
                   by=c('municipality', 'treat_year')
                   ][policies==1]
abol = abol[treat_year==2018 & municipality %in% 
              keep_munies[treat_year==2018, municipality] |
              treat_year==2019 & municipality %in% 
              keep_munies[treat_year==2019, municipality]]


# Now, take only 12 pre-treatment months (currently we have 23):

treatment_year = 2018 # use 2021 in the actual analysis.
start_month_1 = as.Date(paste(treatment_year-1, '-07-01', sep='')) 
treatment_year = 2019 # use 2021 in the actual analysis.
start_month_2 = as.Date(paste(treatment_year-1, '-07-01', sep='')) 

abol = abol[(treat_year==2018 & date >= start_month_1) |
              (treat_year==2019 & date >= start_month_2)]
              

# Next, we will drop observations due to issues in the data quality 
# (see 1_data_quality.R). 

drop_obs = unique(abol[drop_pre_pandemic==1, .(municipality, profession)])
#drop_obs = unique(abol[drop_pandemic==1, .(municipality, profession)]) # use this in the actual analysis
drop_obs[, drop := 1]
abol = merge(abol, drop_obs, by=c('municipality', 'profession'), all.x=TRUE)
abol = abol[is.na(drop), drop := 0][drop == 0][, drop := NULL]


# Aggregate:
abol = abol[, .(value = mean(contacts_per_capita)),
                  #weighted.mean(contacts_per_capita, w=population),
            by=c('treat_abol', 'bottom_d', 'relative_time', 
                 'date', 'profession')]

# Center relative time at the treatment date:
abol[profession %in% c(2,1), relative_time := relative_time - 79]
abol[profession %in% c(3, 40, 41), relative_time := relative_time - 91]

setnames(abol, old='treat_abol', new='treat')


### Create a function that plots trend plots. ###

plot.treatcontrol = function(data, type, income_bottom, visit_type, 
                             title, data_type) {
  # INPUTS:
  # data: 'intro' or 'abol'
  # type: 'raw', 'smooth' or 'diff'
  # income_bottom: 1 (bottom 40%) or 0 (top 40%) or NULL (all)
  # visit_type: 2 for nurse visits, 1 for GP visits, 3 for referrals,
  #             40 for prescriptions, 50 for the share of those receiving
  #             social assistance, and 60 for the sum of received social 
  #             assistance.
  # title: plot title
  # data_type: either 'intro' or abol'
  # OUTPUT:
  # a trend plot
  
  
  # Extract the right data:
  DT = data[profession == visit_type][, treat := as.factor(treat)]
  
  if(!is.null(income_bottom)) {
    DT = DT[bottom_d == income_bottom] }
  
  
  # Next, we proceed to plotting:
  
  if(data_type=='abol') {
    x_title = "Months relative to the abolition"
  } else if (data_type=='intro' & visit_type %in% c(2,1,50)) {
    x_title = "Months relative to the adoption" 
  } else if (data_type=='intro' & visit_type == 60 ) {
    x_title = "Years relative to the adoption" }
  
  
  if(visit_type %in% c(2,1,40,3)) {
    y_title = 'Ann. contacts per capita'
  } else if (visit_type == 50) {
    y_title = 'Percentage points'
  } else if (visit_type == 60) {
    y_title = 'Euros'
  }
  
  
  if(type %in% c('raw', 'smooth')) {
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=value, color=treat, shape=treat)
    ) +
      ggtitle(title) +
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
      geom_vline(xintercept = -0.5, linetype="dashed")
    
    
    if (type=="smooth") {
      
      p = p + geom_smooth(se=FALSE) + 
        scale_color_discrete(name='Municipalities', 
                             labels=c('Comparison', 'Treatment')) +
        scale_linetype_discrete(name='Municipalities', 
                                labels=c('Comparison', 'Treatment'))
      
    } else if (type=="raw") { 
      
      p = p + geom_line() + geom_point() +
        scale_color_discrete(name='Municipalities', 
                             labels=c('Comparison', 'Treatment')) +
        scale_shape_discrete(name='Municipalities', 
                             labels=c('Comparison', 'Treatment'))
      
    }
    
    
  } else if (type == 'diff') {
    
    
    # Mutate the data:
    DT = dcast(DT, bottom_d + relative_time ~ treat, 
                 value.var = 'value' )
    setnames(DT, old=c('0', '1'), new=c('comparison', 'treat'))
    DT[, value := treat-comparison]
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=value)
    ) +
      geom_line(color='grey') +
      geom_point(color='grey') + 
      geom_smooth(se=FALSE, color='#00BFC4') +
      ggtitle(title) +
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
      geom_vline(xintercept = -0.5, linetype="dashed") +
      geom_hline(yintercept = 0, linetype="dashed")
    
  }
  
  
  return(p)
}

plot.treatcontrol(intro, type='diff', income_bottom=1, visit_type = 2,
                  title = 'Bottom 40%, Smoothed.', data_type='intro')


# We will plot the following subplots:

plot.types = c('smooth', 'raw', 'diff')
income.types = c(1,0, NA)

specs = CJ(plot.types, income.types)

specs[income.types==1, title1 := 'Bottom 40%, ']
specs[income.types==0, title1 := 'Top 40%, ']
specs[is.na(income.types), title1 := '']
specs[plot.types=='smooth', title2 := 'Smoothed.']
specs[plot.types=='raw', title2 := 'Raw Data.']
specs[plot.types=='diff', title2 := 'Difference.']
specs[, title := paste(title1, title2, sep='')]
specs = specs[order(-plot.types, title)]


# Trend plots for the adoption:

DT = intro[, mget(colnames(intro))] 
visit_types = c(2,1, 50,60)
models = specs[, mget(colnames(specs))]

# Loop over contact types:

plots.intro = lapply(visit_types, function(type) {
  
  # Loop over models:
  plots.models = lapply(1:nrow(models), function(row) {
    
    mod = models[row, ]
    
    if(is.na(mod$income.types)) {
      income_param = NULL
    } else {
      income_param = mod$income.types }
    
    if(type %in% c(2, 1) & is.null(income_param)) { return(NULL) }
    if(type %in% c(50, 60) & !is.null(income_param)) { return(NULL) }
    
    p = plot.treatcontrol(data = DT, type = mod$plot.types,
                          income_bottom = income_param,
                          visit_type = type,
                          title = mod$title,
                          data_type = 'intro')
    
  })
  
})
names(plots.intro) = visit_types


# Trend plots for the abolition:

DT = abol[, mget(colnames(abol))] 
visit_types = c(2,1, 3, 40)
models = specs[!is.na(income.types)]

# Loop over contact types:

plots.abol = lapply(visit_types, function(type) {
  
  # Loop over models:
  plots.models = lapply(1:nrow(models), function(row) {
    
    mod = models[row, ]
    
    p = plot.treatcontrol(data = DT, type = mod$plot.types,
                          income_bottom = mod$income.types,
                          visit_type = type,
                          title = mod$title,
                          data_type = 'abol')
    
  })
  
})
names(plots.abol) = visit_types


# Save plots:

cairo_pdf(filename = output_intro_nurse, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(plots.intro$`2`[lengths(plots.intro$`2`) != 0],
                            ncol=2)) 
dev.off()

cairo_pdf(filename = output_intro_gp, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(plots.intro$`1`[lengths(plots.intro$`1`) != 0],
                            ncol=2)) 
dev.off()

cairo_pdf(filename = output_intro_sa, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(c(plots.intro$`50`[lengths(plots.intro$`50`) != 0],
                            plots.intro$`60`[lengths(plots.intro$`60`) != 0]),
                            ncol=2, byrow=FALSE)) 
dev.off()

cairo_pdf(filename = output_abol_nurse, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(plots.abol$'2', ncol=2)) 
dev.off()

cairo_pdf(filename = output_abol_gp, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(plots.abol$`1`, ncol=2)) 
dev.off()

cairo_pdf(filename = output_abol_drugs, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(plots.abol$'40', ncol=2)) 
dev.off()

cairo_pdf(filename = output_abol_refs, width = 10.0, height = 9.0)
print(patchwork::wrap_plots(plots.abol$`3`, ncol=2)) 
dev.off()

# End.
