
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 2_trend_plots.R            ###
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
input_intro = "W:/ASMA3/data/cleaned/intro_trends_data.rds"
input_abol = "W:/ASMA3/data/cleaned/analysis_data_aggr.rds"

# Outputs:
output_intro_copay_gp = 
  "W:/ASMA3/analysis/figures/intro_trends_plot_copay_gp.pdf"
output_intro_nurse = "W:/ASMA3/analysis/figures/intro_trends_plot_nurse.pdf"
output_intro_gp = "W:/ASMA3/analysis/figures/intro_trends_plot_gp.pdf"
output_intro_sa = "W:/ASMA3/analysis/figures/intro_trends_plot_sa.pdf"
output_intro_all = "W:/ASMA3/analysis/figures/intro_trends_plot_all.pdf"
output_intro_preventive = 
  "W:/ASMA3/analysis/figures/intro_trends_plot_preventive.pdf"
output_abol_nurse = "W:/ASMA3/analysis/figures/abol_trends_plot_nurse.pdf"
output_abol_gp = "W:/ASMA3/analysis/figures/abol_trends_plot_gp.pdf"
output_abol_drugs = "W:/ASMA3/analysis/figures/abol_trends_plot_drugs.pdf"
#output_abol_refs = "W:/ASMA3/analysis/figures/abol_trends_plot_refs.pdf"
output_abol_all = "W:/ASMA3/analysis/figures/abol_trends_plot_all.pdf"


###
###


# Read datasets:

intro = readRDS(input_intro)
intro[outcome == 'social_assistance', profession := 50]
intro[outcome == 'basic_social_assistance', profession := 60]

abol = readRDS(input_abol)
abol = abol[!is.na(treat_abol) & !is.na(profession)]


# Use a 1-year bandwidth around the treatment_year for the abolition analyses. 
treatment_year = 2021 
start_month = as.Date(paste(treatment_year-1, '-07-01', sep='')) 
end_month = as.Date(paste(treatment_year+1, '-06-01', sep=''))
abol = abol[date >=start_month & date <= end_month & dimension=='income_decile']
abol[, treat_year :=  treatment_year]


# Next, we will drop observations due to issues in the data quality 
# (see 1_data_quality.R). 
drop_obs = unique(abol[drop_pandemic==1, .(municipality, profession)])
drop_obs[, drop := 1]
abol = merge(abol, drop_obs, by=c('municipality', 'profession'), all.x=TRUE)
abol = abol[is.na(drop), drop := 0][drop == 0][, drop := NULL]

# Center relative time at the treatment date:
abol[, relative_time := relative_time - 115]
setnames(abol, old='treat_abol', new='treat')


# Aggregate (by income groups):

abol[group %in% c(0:3), bottom_d := 1]
abol[group %in% c(6:9), bottom_d := 0]

abol = abol[, .(value = weighted.mean(contacts_per_capita, w=population),
                population = sum(population)),
            by=c('treat', 'bottom_d', 'relative_time', 'date', 'profession')]

# Aggregate for all individuals:
abol.all = abol[, .(value = weighted.mean(value, w=population),
                    population = sum(population)),
                by=c('treat', 'relative_time', 'date', 'profession')]
abol.all = abol.all[profession %in% c(2, 1)]
abol.all[profession==2, title := 'Nurse visits']
abol.all[profession==1, title := 'GP visits']

# Row bind:
abol = abol[!is.na(bottom_d)]
abol = rbind(abol, abol.all, fill=TRUE)

# The outcomes are in levels:
abol[, outcome := 'contacts_per_capita']


### Create a function that plots trend plots. ###

plot.treatcontrol = function(data, type, income_bottom, visit_type, 
                             p.title, data_type, otc='contacts_per_capita') {
  # INPUTS:
  # data: 'intro' or 'abol'
  # type: 'raw', 'smooth' or 'diff'
  # income_bottom: 1 (bottom 40%) or 0 (top 40%) or NULL (all)
  # visit_type: 2 for nurse visits, 1 for GP visits, 3 for referrals,
  #             40 for prescriptions, 50 for the share of those receiving
  #             social assistance, and 60 for the sum of received social 
  #             assistance.
  # p.title: plot title
  # data_type: either 'intro' or abol'
  # otc: if data_type=='intro', specify either 'contacts_per_capita' or 
  #     'contacts_per_capita_log'
  # OUTPUT:
  # a trend plot
  
  
  # Extract the right data:
  DT = data[profession == visit_type]
  
  if(otc=='contacts_per_capita') { 
    DT = DT[outcome != 'contacts_per_capita_log']
  } else if (otc=='contacts_per_capita_log') {
    DT = DT[outcome != 'contacts_per_capita'] }
  
  if(!is.null(income_bottom)) {
    DT = DT[bottom_d == income_bottom] }
  
  
  # Treatment groups as factors:
  DT[treat==1, treat.f := 'Treatment']
  DT[treat==0, treat.f := 'Comparison']
  DT[, treat.f := factor(DT$treat.f, levels=c('Treatment', 'Comparison'))]
  
  
  # Next, we proceed to plotting:
  
  if(data_type=='abol') {
    x_title = "Months relative to the abolition"
  } else if (data_type=='intro' & visit_type %in% c(2,1,25,50)) {
    x_title = "Months relative to the adoption" 
  } else if (data_type=='intro' & visit_type == 60 ) {
    x_title = "Years relative to the adoption" }
  
  
  if(visit_type %in% c(2,1,25,40)) { # referrals excluded (3)
    
    if(p.title=='GP visit copayment') { y_title = 'Euros'}
    else { y_title = 'Ann. contacts per capita'}
    
  } else if (visit_type == 50) {
    y_title = 'Percentage points'
  } else if (visit_type == 60) {
    y_title = 'Euros'
  }
  
  if(p.title=='GP visit copayment') {
    DT[, value := NULL]
    setnames(DT, old='copay', new='value')
    DT = DT[title=='GP visit copayment']
  } else if(p.title %in% c('Nurse visits', 'GP visits')) {
    DT = DT[title==p.title] }
  
  
  if(type %in% c('raw', 'smooth')) {
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=value, linetype=treat.f)
    ) +
      ggtitle(p.title) +
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
        scale_linetype_discrete(name='Municipalities') + 
        guides(linetype='none')
      
    }
    
    
  } else if (type == 'diff') {
    
    
    # Mutate the data:
    DT = dcast(DT, bottom_d + relative_time ~ treat.f, value.var = 'value' )
    DT[, value := Treatment-Comparison]
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=value)
    ) +
      geom_line(color='grey40') +
      geom_point(color='grey40') + 
      geom_smooth(se=FALSE, color='black') +
      ggtitle(p.title) +
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

plot.treatcontrol(intro, type='raw', income_bottom=1, visit_type = 2,
                  p.title = 'Bottom 40%, Smoothed.', data_type='intro')

plot.treatcontrol(intro, type='smooth', income_bottom=1, visit_type = 2,
                  p.title = 'Bottom 40%, Smoothed.', data_type='intro')

plot.treatcontrol(intro, type='diff', income_bottom=1, visit_type = 2,
                  p.title = 'Bottom 40%, Smoothed.', data_type='intro')


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
models[, outcome.type := 'contacts_per_capita']

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
                          p.title = mod$title,
                          data_type = 'intro', otc = mod$outcome.type)
    
  })
  
})
names(plots.intro) = visit_types


# The trends in GP visit copayments:

p1 = plot.treatcontrol(intro, type='smooth', income_bottom=NULL, visit_type = 2,
                       p.title = 'GP visit copayment', data_type='intro') +
  ggtitle('Smoothed')

p2 = plot.treatcontrol(intro, type='raw', income_bottom=NULL, visit_type = 2,
                       p.title = 'GP visit copayment', data_type='intro') +
  ggtitle('Raw data')

p3 = plot.treatcontrol(intro, type='diff', income_bottom=NULL, visit_type = 2,
                       p.title = 'GP visit copayment', data_type='intro') +
  ggtitle('Difference')


# Introduction: trend plots for the whole population:

visit_types = c(2,1, 25)
plot.types = c('smooth', 'raw', 'diff')


# Loop over contact types:

plots.intro.all = lapply(visit_types, function(type) {
  
  if(type==2) {mod.title = 'Nurse visits'}
  if(type==1) {mod.title = 'GP visits'}
  if(type==25) {mod.title = 'Preventive nurse visits'}
  
  # Loop over models:
  plots.models = lapply(plot.types, function(plot.type) {
    
    p = plot.treatcontrol(
      data = DT, type = plot.type, income_bottom = NULL,
      visit_type = type, p.title = mod.title, data_type = 'intro')
    
    return(p)
  })
  names(plots.models) = c('smooth', 'raw', 'diff')
  return(plots.models)
  
})
names(plots.intro.all) = paste0('contact.', visit_types)


# Trend plots for the abolition:

DT = abol[, mget(colnames(abol))] 
visit_types = c(2,1, 40) # referrals (3) excluded
models = specs[!is.na(income.types)]
models[, outcome.types := 'contacts_per_capita']

# Loop over contact types:

plots.abol = lapply(visit_types, function(type) {
  
  # Loop over models:
  plots.models = lapply(1:nrow(models), function(row) {
    
    mod = models[row, ]
    
    p = plot.treatcontrol(data = DT, type = mod$plot.types,
                          income_bottom = mod$income.types,
                          visit_type = type,
                          p.title = mod$title,
                          data_type = 'abol', otc = mod$outcome.types)
    
  })
  
})
names(plots.abol) = visit_types


# Abolition: trend plots for the whole population:

visit_types = c(2,1)
plot.types = c('smooth', 'raw', 'diff')


# Loop over contact types:

plots.abol.all = lapply(visit_types, function(type) {
  
  if(type==2) {mod.title = 'Nurse visits'}
  if(type==1) {mod.title = 'GP visits'}
  
  # Loop over models:
  plots.models = lapply(plot.types, function(plot.type) {
    
    p = plot.treatcontrol(
      data = DT, type = plot.type, income_bottom = NULL,
      visit_type = type, p.title = mod.title, data_type = 'abol')
    
    return(p)
  })
  names(plots.models) = c('smooth', 'raw', 'diff')
  return(plots.models)
  
})
names(plots.abol.all) = paste0('contact.', visit_types)


# Save plots:

cairo_pdf(filename = output_intro_copay_gp, width = 15.0, height = 4.5)
print(p1 + p2 + p3 + plot_layout(guides='collect') & 
        theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = output_intro_nurse, width = 15.0, height = 8.0)
print(patchwork::wrap_plots(
  plots.intro$`2`[lengths(plots.intro$`2`) != 0], 
  ncol=3, byrow = FALSE, guides='collect') & 
    theme(legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_intro_gp, width = 15.0, height = 8.0)
print(patchwork::wrap_plots(
  plots.intro$`1`[lengths(plots.intro$`1`) != 0], 
  ncol=3, byrow = FALSE, guides='collect') & 
    theme(legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_intro_sa, width = 15.0, height = 8.0)
print(patchwork::wrap_plots(
  c(plots.intro$`50`[lengths(plots.intro$`50`) != 0],
    plots.intro$`60`[lengths(plots.intro$`60`) != 0]),
  ncol=3, byrow=TRUE, guides='collect') &
    theme(legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_intro_all, width = 15.0, height = 8.0)
print(
  plots.intro.all$contact.2$smooth + ggtitle('Nurse visits: smoothed') + 
    plots.intro.all$contact.2$raw + ggtitle('Nurse visits: raw data') + 
      guides(linetype='none') +  
    plots.intro.all$contact.2$diff + ggtitle('Nurse visits: difference') + 
    plots.intro.all$contact.1$smooth + ggtitle('GP visits: smoothed') +
    plots.intro.all$contact.1$raw + ggtitle('GP visits: raw data') +
      guides(linetype='none') + 
    plots.intro.all$contact.1$diff + ggtitle('GP visits: difference') +
    plot_layout(ncol = 3, guides = 'collect') & 
    theme(text = element_text(size=20), legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_intro_preventive, width = 15.0, height = 4.5)
print(
  plots.intro.all$contact.25$smooth + ggtitle('Nurse visits: smoothed') + 
    plots.intro.all$contact.25$raw + ggtitle('Nurse visits: raw data') + 
      guides(linetype='none') +  
    plots.intro.all$contact.25$diff + ggtitle('Nurse visits: difference') + 
    plot_layout(ncol = 3, guides = 'collect') & 
    theme(text = element_text(size=20), legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_abol_nurse, width = 15.0, height = 8.0)
print(patchwork::wrap_plots(
  plots.abol$'2', ncol=3, byrow = FALSE, guides='collect') & 
    theme(legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_abol_gp, width = 15.0, height = 8.0)
print(patchwork::wrap_plots(
  plots.abol$'1', ncol=3, byrow = FALSE, guides='collect') & 
    theme(legend.position = 'bottom')
) 
dev.off()

cairo_pdf(filename = output_abol_drugs, width = 15.0, height = 8.0)
print(patchwork::wrap_plots(
  plots.abol$'40', ncol=3, byrow = FALSE, guides='collect') & 
    theme(legend.position = 'bottom')
) 
dev.off()

#cairo_pdf(filename = output_abol_refs, width = 15.0, height = 10.0)
#print(patchwork::wrap_plots(
#  plots.abol$'3', ncol=3, byrow = FALSE, guides='collect') & 
#    theme(legend.position = 'bottom')
#) 
#dev.off()

cairo_pdf(filename = output_abol_all, width = 15.0, height = 8.0)
print(
  plots.abol.all$contact.2$smooth + ggtitle('Nurse visits: smoothed') + 
    plots.abol.all$contact.2$raw + ggtitle('Nurse visits: raw data') + 
    guides(linetype='none') +  
    plots.abol.all$contact.2$diff + ggtitle('Nurse visits: difference') + 
    plots.abol.all$contact.1$smooth + ggtitle('GP visits: smoothed') +
    plots.abol.all$contact.1$raw + ggtitle('GP visits: raw data') +
    guides(linetype='none') + 
    plots.abol.all$contact.1$diff + ggtitle('GP visits: difference') +
    plot_layout(ncol = 3, guides = 'collect') & 
    theme(text = element_text(size=20), legend.position = 'bottom')
) 
dev.off()

# End.
