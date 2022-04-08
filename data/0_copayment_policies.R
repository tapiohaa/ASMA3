
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         R script 0_copayment_policies.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits     ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Tidy data on nurse visit copayment policies and visualize the 
#   staggered adoption and the abolition of the copayment.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating data.
library(ggplot2)          # Plot figures.
library(patchwork)        # Layout of the plots.


# Inputs:
input_policies_11_19 = "W:/ASMA3/data/raw/policies_11_19.csv"
input_policies_21 = "W:/ASMA3/data/raw/sote_areas_2021.csv"
input_munies_22 = "W:/ASMA3/data/raw/sote_munies_2021.csv"
input_munigeom = "W:/ASMA3/data/raw/munies_geom_2020.rds"

# Outputs:
output_intro = "W:/ASMA3/data/interim/adoption_policies.rds"
output_intro_placebo = "W:/ASMA3/data/interim/adoption_policies_placebo.rds"
output_abol = "W:/ASMA3/data/interim/abolition_policies.rds"
output_plot = "W:/ASMA3/analysis/figures/copay_policies.pdf"
output_policies = "W:/ASMA3/analysis/figures/copay_policies_euros.pdf"
output_map_intro = "W:/ASMA3/analysis/figures/copay_policies_map_intro.pdf"
output_map_abol = "W:/ASMA3/analysis/figures/copay_policies_map_abol.pdf"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Staggered adoption. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read data and keep only those municipalities whose
# policy is observed:
policies = setDT(read.csv(input_policies_11_19, sep=';', encoding = 'UTF-8'))
colnames(policies)[1] = 'municipality_code'
policies = policies[nurse_copay_by_12_19 %in% c(1,0) & 
                      !is.na(nurse_copay_introduction)]

# We will use the 2022 municipal boundaries, but currently 
# 'policies' contains municipalities from 2011. Thus, drop 
# municipalities that were merged into larger municipalities
# in the study period. We checked manually that the pre-merger
# policies were similar.
policies = 
  policies[!(municipality_code %in% c(
    863, 248, 534, 223, 540, 696, 775, 84, 255, 567, 972, 926, 
    254, 246, 618, 942, 413, 476, 838, 283, 164, 442, 911, 99
    ))]

# There was one name change during the study period:
policies = policies[!(municipality_code==445 & year==2011)]

# The following mergers were not dropped due to a merger because
# they were already dropped due to NA values in policies:
# - 319	Köyliö merged on 1.1.2016 to 783	Säkylä.

# The following areas are dropped because pre-merger policies 
# were different:
# - 532	Nastola	merged on 1.1.2016	to 398	Lahti.
# - 174	Juankoski merged on	1.1.2017 to	297	Kuopio.
policies = policies[!(municipality_code %in% c(532, 398, 174, 297))]

# Select relevant columns and tidy the date column:
policies = 
  policies[nurse_copay_by_12_19==0, 
           nurse_copay_introduction := NA_character_
           ][, .(municipality_code, nurse_copay_by_12_19, 
                 nurse_copay_introduction)
             ][, nurse_copay_date := 
                      as.Date(nurse_copay_introduction, '%d.%m.%Y')]
policies = policies[, nurse_copay_introduction := NULL]

# Read data on population sizes:
df = setDT(read.csv(input_munies_22, sep=';', encoding='UTF-8'))
colnames(df)[1] = 'municipality_code'
df = df[, .(municipality_code, Asukasluku.31.12.2019)]
setnames(df, old='Asukasluku.31.12.2019', new='population')

# Population size to integer:
df[, population := as.integer(gsub(' ', '', population))]

# Merge:
df = merge(df, policies, by='municipality_code')


# Next, we will create a municipality-month panel:

# We will create a grid over months, years, and municipalities.
months = 1:12
years = 2012:2019
municipalities = unique(df$municipality_code)

# Create the panel.
panel = CJ(municipalities, years, months)
colnames(panel) = c('municipality_code', 'year', 'month')

# Create date variable to the panel:
panel[, date := as.Date(
  paste(as.character(year), as.character(month), '01', sep = '-') )]

# Merge copayment data to the panel:
panel = merge(panel, df, by='municipality_code', all.x=TRUE)

# Create treatment variable:
panel = panel[, treat_intro := as.integer(date >= nurse_copay_date)
              ][nurse_copay_by_12_19==0, treat_intro := 0]

# Drop some variables:
panel[, ':=' (year = NULL, month = NULL, nurse_copay_date = NULL)]

# Create treatment variable:
panel[, treat := treat_intro]

# Aggregate:
aggr1 = panel[, .(municipalities = .N, 
                  population = sum(population)), by=c('date', 'treat')
              ][, treat := as.factor(treat)
                ][date >= as.Date('2013-01-01')
                  ][, population := population / 1000]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Abolition. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read data on municipalities (2022 boundaries), and select variables:
munies = setDT(read.csv(input_munies_22, sep=';', encoding='UTF-8'))
colnames(munies)[1] = 'Kunta.koodi'
munies = munies[, .(Kunta.koodi, Kunta, Asukasluku.31.12.2019, Terveyskeskus)]

# Translate column names to English:
setnames(munies, 
         old = c('Kunta.koodi', 'Kunta', 'Asukasluku.31.12.2019', 
                 'Terveyskeskus'),
         new = c('municipality', 'name', 'population', 'health_center'))

# Transform population size to integer:
munies[, ':=' (population = as.integer(gsub(' ', '', population)),
               health_center = gsub(' ', '', health_center))]


# Read data on policies, and select variables:
policies = setDT(read.csv(input_policies_21, sep=";", encoding='UTF-8'))
policies = policies[, .(kunta_alue, terveyskeskus, maksu_varma, muuta, alku,
                        kertamaksu, tapa, vuosimaksu)]

# Translate column names to English:
setnames(policies, 
         old = c('kunta_alue', 'terveyskeskus', 'maksu_varma', 'muuta', 'alku',
                 'kertamaksu', 'tapa', 'vuosimaksu'),
         new = c('area', 'health_center', 'policy', 'notes', 'introduction',
                 'per_visit', 'option', 'per_year'))

# We will merge 'policies' to 'munies' via 'health_center'. For that end, 
# we first clean the health_center variable.
policies = 
  policies[, health_center := gsub(' ', '', health_center)
           ][health_center=='PeruspalvelukuntayhtymäKallio/terveyskeskus',
             health_center := 'PeruspalvelukuntayhtymäKallio'
             ][health_center=='Järvi-Pohjanmaanterveyskeskus',
               health_center := 'Järvi-Pohjanmaanperusturva,yhteistoiminta-alue'
               ][health_center=='Itä-Savonsairaanhoitopiiri/terveyskeskus',
                 health_center := 'Itä-Savonsairaanhoitopiirinkuntayhtymä'
                 ][health_center=='Pöytyänterveyskeskus',
                   health_center := 'Pöytyänkansanterveystyönky']

# Merge:
df2 = merge(munies, policies, by='health_center', all.x = TRUE)
df2[, health_center := NULL]

# Transform the introduction date to date:
df2[, introduction := as.Date(introduction, format='%d.%m.%Y')]


# Next, we start to define the sample:

# Policies were not observed for these municipalities. Drop them:
print(df2[is.na(policy), name])
df2 = df2[!is.na(policy)]

# Naantali and Taivalkoski were dropped, because we could not be
# sure whether the copayment for primary care visits contained only
# GP visits (as was the case in other municipalities).

# Kinnula and Kaskinen (both dropped above) abolished the copayment 
# already on 1.3.2021 and 1.1.2021, respectively, and are thus dropped.

# Furthermore, Karkkila and Vihti are dropped because they introduced 
# the copayment on 1.1.2021, within our 12-month pre-treatment window.
print(df2[municipality %in% c(224, 927), name])
df2 = df2[!(municipality %in% c(224, 927))]

# The following municipalities are dropped because their copayment covers
# only a small subset of nurse visits:
print(df2[municipality %in% c(52, 233), name])
df2 = df2[!(municipality %in% c(52, 233))]

# Treatment effects may not be fully realized instantly. They may accumulate.
# We only include those copayment municipalities who had 12 months or more
# under the copayment before 7/2020, the start of our abolition window.
# That is, municipalities with copayment introduction between 8/2019-6/2020
# are excluded.
print(df2[introduction >= as.Date('2019-08-01') &
            introduction <= as.Date('2020-06-01'), name])
df2 = df2[!(introduction >= as.Date('2019-08-01') &
              introduction <= as.Date('2020-06-01')) | is.na(introduction)]

# We have to check manually for the following municipalities whether
# the copayment was introduced before 1.8.2019 when our study period begins.
test = df2[policy==1 & is.na(introduction) & notes =='']

# The following municipalities are dropped because we cannot verify that their
# copayment was introduced before 1.8.2019.
print(df2[municipality %in% c(499, 946), name])
df2 = df2[!(municipality %in% c(499, 946))
          ][, ':=' (notes=NULL, introduction=NULL)]


# We will again create a municipality-month-year panel:
months = 1:12
years = 2020:2022
municipalities = unique(df2$municipality)

# Create the panel:
panel2 = CJ(municipalities, years, months)
colnames(panel2) = c('municipality_code', 'year', 'month')

# Create date variable to the panel:
panel2[, date := as.Date(
  paste(as.character(year), as.character(month), '01', sep='-') )]

# Take 1.7.2020 - 31.6.2021:
panel2 = panel2[date >= as.Date('2020-07-01') & date < as.Date('2022-07-01')]

# Merge 'df2' to 'panel2':
panel2 = merge(panel2, df2, by.x = 'municipality_code',
               by.y = 'municipality', all.x = TRUE)

# Create treatment variable:
panel2[, treat := policy]
panel2 = panel2[date >= as.Date('2021-07-01'), treat := 0] 

# Aggregate:
aggr2 = panel2[, .(municipalities = .N,
                   population = sum(population)), by=c('date', 'treat')
               ][, ':=' (treat = as.factor(treat),
                         population = population / 1000)]

# Expand:
exp = CJ(unique(aggr2$date), unique(aggr2$treat))
colnames(exp) = c('date', 'treat')
exp = merge(exp, aggr2, by=c('date', 'treat'), all.x=TRUE)
exp[is.na(municipalities), ':=' (municipalities = 0,
                                 population = 0)]

table = data.table(
  date = as.Date(c('2021-06-30', '2021-06-30')), treat=c(0,1),
  population = NA, municipalities = NA
)

aggr2 = rbind(exp, table)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Plot: population and municipalities. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A plotting function. ###

plot.copay = function(data, outcome, abolition=0) {
  # INPUTS:
  # data: 'data' created above
  # outcome: either 'municipalities' or 'population'
  # abolition: 0 for the staggered adoption and 1 for the abolition
  # OUTPUT:
  # a ggplot object
  
  
  # Extract the data:
  df = data[, mget(colnames(data))]
  setnames(df, old=outcome, new='outcome')
  
  # Set details based on the outcome:
  if(outcome=='municipalities') {
    ylab_title = 'No. of municipalities'
    brks = seq(0, 300, by=50)
    y_lim = NULL
    if(abolition==1) { y_lim = c(0, 300) }
  } else if (outcome=='population') {
    ylab_title = 'Residents (in thousands)'
    brks = seq(0, 5000, by=1000)
    y_lim = NULL
    if(abolition==1) { y_lim = c(0, 5500) }
  }
  
  
  # Plot:
  p = ggplot(df, aes(x=date, y=outcome, color=treat, group=treat)) + 
    ylab(ylab_title) +
    scale_y_continuous(breaks = brks, limits = y_lim) + 
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    scale_colour_discrete(name = 'Has a nurse visit copayment',
                          labels = c('No', 'Yes')) + 
    theme(text = element_text(size=15),   
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom",
          axis.title.x = element_blank())
  
  if(abolition == 0) {
    p = p + geom_step()
  } else if (abolition==1) {
    p = p + geom_line(na.rm = TRUE) +
      geom_vline(xintercept = as.Date('2021-07-01'), linetype='dashed')
  }
  
  return(p)
}


# Draw the subplots:
p1 = plot.copay(aggr1, outcome='municipalities')
p2 = plot.copay(aggr1, outcome='population')
p3 = plot.copay(aggr2, outcome='municipalities', abolition=1)
p4 = plot.copay(aggr2, outcome='population', abolition=1)

# Save the plot:
cairo_pdf(filename = output_plot, width = 10.0, height = 10.0)
print(p1 + ggtitle('Staggered Adoption: Municipalities.') + 
        p2 + ggtitle('Staggered Adoption: Population.') + 
        p3 + ggtitle('Abolition: Municipalities.') + 
        p4 + ggtitle('Abolition: Population.') + 
        plot_layout(guides = "collect") & 
        theme(legend.position='bottom'))
dev.off()



### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Plot: policy options and copayment levels. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# First, create a bar chart that shows how common different policy options are:

# Extract data and label policy options:
data = df2[policy==1][, ':=' (policy = NULL, option = as.factor(option))]
data[option==0, Policy := 'Only annual']
data[option==1, Policy := 'Annual or visit-based']
data[option==3, Policy := 'Visit-based, three visits']
data[option==5, Policy := 'Visit-based, five visits']
data[option==45, Policy := 'Visit-based, times not specified']

# Aggregate:
DT.1 = data[, .(munies=.N, population=sum(population)), by='Policy']
DT.1[, ':=' (all_munies = sum(munies), all_pop = sum(population))]
DT.1[, ':=' (munies = 100 * munies / all_munies,
             population = 100 * population / all_pop)]

# Pivot longer:
setnames(DT.1, old=c('munies', 'population'), 
         new=c('Municipalities', 'Population'))
DT.1 = melt(DT.1, id.vars = c('Policy'), 
            measure.vars = c('Municipalities', 'Population'),
            variable.name='Measure') 

# Plot:
p.1 = ggplot(data=DT.1, aes(x=value, y=Policy, group=Measure, fill=Measure)) +
  geom_bar(stat='identity', position = 'dodge') +
  xlab('Share (%)') +
  theme(text = element_text(size=15),   
        axis.text.x = element_text(hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = "solid", colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.position = 'bottom',
        axis.title.y = element_blank())


# Next, we will create a plot that shows the distribution of the level
# of per-visit copayments:

# Extract data:
data = df2[policy==1][, ':=' (policy = NULL, option = as.factor(option))]
data[per_visit=='', per_visit := NA_character_]
data[, level := as.numeric(gsub(',', '.', per_visit))]
data = data[!is.na(level)]

# Aggregate:
DT.2 = data[, .(population=sum(population)), by='level']
DT.2[, all_pop := sum(population)]
DT.2[, population := 100 * population / all_pop]
setnames(DT.2, old='population', new='Population')

# Plot:
p.2 = ggplot(data=DT.2, aes(x=level, y=Population)) +
  geom_bar(stat = 'identity') +
  geom_vline(xintercept = weighted.mean(DT.2$level, w= DT.2$Population),
             color='red', linetype='dashed') +
  xlab('Copayment per visit in euros') +
  ylab('Share of population (%)') +
  theme(text = element_text(size=15),   
        axis.text.x = element_text(hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = "solid", colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

# Save:

cairo_pdf(filename = output_policies, width = 10.0, height = 5.0)
print(p.1 + ggtitle('Copayment Policies.') + 
        p.2 + ggtitle('Copayment Levels.'))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Save datasets. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# First, take the dataset on the staggered adoption:

# Municipalities and population (year 2019):
print(length(unique(panel$municipality_code)))
print(sum(panel[date==max(date), population]))

panel[, ':=' (population = NULL, treat = NULL,
              nurse_copay_by_12_19 = NULL)]

# Save the real dataset:
saveRDS(panel, output_intro)

# We need a placebo dataset in order to write (and run) the analysis codes
# without seeing the actual estimates.

# The municipalities in the data:
municipalities = sort(unique(panel$municipality_code))

# Randomly shuffle the vector:
set.seed(12345)
munies_rand = sample(municipalities)

# Map actual codes to randomized codes:
munies = data.table(municipality_code = municipalities,
                    municipality_rand = munies_rand)

# Merge into panel:
panel_placebo = merge(panel, munies, by='municipality_code', all.x = TRUE)
panel_placebo[, municipality_code := NULL]
setnames(panel_placebo, old='municipality_rand', new='municipality_code')

saveRDS(panel_placebo, output_intro_placebo)


# Next, save the dataset on the abolition:

# Municipalities and population (year 2019):
print(nrow(df2))
print(sum(df2$population))

df2[, population := NULL]
setnames(df2, old=c('policy', 'municipality'), 
         new=c('treat_abol', 'municipality_code'))
saveRDS(df2, output_abol)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Copayment policies on maps. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# We will create a plot that shows on a map copayment policy at the end
# of each year for each municipality.

# Tidy datasets on adoption and abolition for plotting:

intro = panel[, .(municipality_code, date, treat_intro)]
setnames(intro, old='treat_intro', new='policy')

intro[, ':=' (year = as.integer(format(date, "%Y")),
              month = as.integer(format(date, "%m")))]
intro = intro[year %in% c(2014:2019) & month==12
              ][, ':=' (date=NULL, month=NULL)]

abol = df2[, .(municipality_code, treat_abol)]
setnames(abol, old='treat_abol', new='policy')
abol[, year := 2021]

intro_abol = rbind(intro, abol)


# Read data on municipal boundaries for plotting:
munies = setDT(readRDS(input_munigeom))

# Create municipality-year panel to which we merge polygons and policies:
df = CJ(unique(munies$no), c(2014:2019, 2021))
colnames(df) = c('no', 'year')

# Merge:
df = merge(df, munies, by='no', all.x = TRUE)
setnames(df, old='no', new='municipality_code')
df = merge(df, intro_abol, by=c('municipality_code', 'year'), all.x = TRUE)

# Policy as a factor:
df[is.na(policy), policy.f := 'Missing']
df[policy==0, policy.f := 'No']
df[policy==1, policy.f := 'Yes']

df$policy.f = factor(df$policy.f, levels = c('Yes', 'No', 'Missing'))


# Adoption:

DT = df[year %in% c(2014:2019)]

cairo_pdf(filename = output_map_intro, width = 15.0, height = 6.0)

print(ggplot() +
        facet_grid(~ DT$year) +
        geom_sf(data=DT$geom, aes(fill=DT$policy.f)) +
        scale_fill_manual(name = 'Has a nurse visit copayment',
                          values=c("#00BFC4", "#F8766D", "gray100")) + 
        coord_sf(datum = NA) + 
        theme(text = element_text(size=15),   
              panel.background = element_rect(fill = "white", colour = "white"),
              legend.position = "bottom",
              axis.title.x = element_blank()))

dev.off()


# Abolition by municipality:

DT = df[year==2021]

p.abol.1 = ggplot() +
  geom_sf(data=DT$geom, aes(fill=DT$policy.f)) +
  scale_fill_manual(name = 'Has a nurse visit copayment',
                    values=c("#00BFC4", "#F8766D", "gray100")) + 
  coord_sf(datum = NA) + 
  theme(text = element_text(size=15),   
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom",
        axis.title.x = element_blank())


# Abolition by municipality and population size:

p.abol.2 = ggplot() +
  geom_sf(data=DT$geom, fill="gray") +
  stat_sf_coordinates(data=DT$geom, 
                      aes(color=DT$policy.f, size=DT$population)) +
  scale_color_manual(name = 'Has a nurse visit copayment',
                     values=c("#00BFC4", "#F8766D", "gray100"),
                     guide='none') + 
  scale_size_continuous(guide='none') + 
  coord_sf(datum = NA) +
  theme(text = element_text(size=15),   
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# Save the plots:

cairo_pdf(filename = output_map_abol, width = 6.0, height = 6.0)
print(p.abol.1 + p.abol.2 + plot_layout(guides='collect') &
        theme(legend.position = 'bottom'))
dev.off()

# End.
