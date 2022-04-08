
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script master_script_r.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Implement the R-scripts required to replicate the analysis.

# R version 4.0.5, RStudio version 1.2.5033.
# Running time approximately 105 minutes.
rm(list=ls())

# First, make sure that the SAS scripts (listed in master_script_sas.sas) 
# have run successfully.


# To install a package in Fiona:
# 1) Run: cat("RSTUDIO_DISABLE_PACKAGES=TRUE \nR_LIBS_USER=D:/cran-405", 
#             file=".Renviron")
#     The command creates a file called .Renviron under your Documents folder
# 2) Restart RStudio. Now you can load CRAN packages from N:
# 3) Use the library() function to load packages.

# The packages loaded are listed below.
library(data.table)       # Mutating data. 
library(readxl)           # Reading xlsx files. 
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 
library(openxlsx)         # Save as excel file. 
library(stargazer)        # Save as tex file. 
library(fastDummies)      # Fast creation of dummy columns. 
library(lfe)              # Linear fixed effects estimation. 
library(broom)            # Statistical objects into tidy tibbles. 
library(did)              # Staggered DID designs.

writeLines(capture.output(sessionInfo()), 'sessionInfo.txt')


###
###

Sys.time()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 0) Preliminary steps. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read and tidy data on copayments:
source(file="W:/ASMA3/data/0_copayment_policies.R")
#     Inputs: policies_11_19.csv + sote_areas_2021.csv + sote_munies_2021.csv +
#			munies_geom_2020.rds
#     Outputs: adoption_policies.rds + adoption_policies_placebo.rds +
#           abolition_policies.rds + copay_policies.pdf +
#           copay_policies_euros.pdf + copay_policies_map_intro.pdf +
#           copay_policies_map_abol.pdf
# Running time <0.5 minutes.


# Extract only publicly-funded primary care visits from 2019-2022:
source(file="W:/ASMA3/data/0_public_visits.R")
#     Inputs: sote_munies_2021.csv + topi_unit_register.csv + 
#             sote_organisation_register.xlsx + visits_XX.csv (XX in 19:20)
#     Outputs: visits_public_XX.rds (XX in 19:20)
# Running time 3 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Create analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Link visits, referrals, prescriptions, and social assistance data
# to the socioeconomic data and aggregate outcome:
source(file="W:/ASMA3/data/1_link_datasets.R")
#     Input: folk_data_20XX.csv (XX in 12:19) +
#             visits_XX.csv (XX in 12:18) + visits_public_XX.csv (XX in 19:20) + 
#             referrals_XX.csv (XX in 12:20) + 
#             prescriptions_X_YY.csv (X in 1:2 and YY in 2018:2020)
#     Outputs: visit_data_aggr.rds
# Running time approximately 26 minutes.

# Analyse visit data quality, and create analysis data:
source(file="W:/ASMA3/data/1_data_quality.R")
#     Input: visit_data_aggr.rds + adoption_policies_placebo.rds + # NOTE: CHANGE THIS (PLACEBO) IN ACTUAL ANALYSIS
#           abolition_policies.rds
#     Outputs: visits_pandemic.pdf + 
#           visits_quality_aggr_introduction_weird_1.pdf + 
#           visits_quality_aggr_introduction_weird_2.pdf + 
#           visits_quality_aggr_introduction_weird_3.pdf +
#           visits_quality_aggr_abolition_weird.pdf + 
#           visits_quality_aggr_abolition_weird_refs_1.pdf + 
#           visits_quality_aggr_abolition_weird_refs_2.pdf +
#           analysis_data_aggr.rds 
# Running time 4 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Main analyses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Analyze the effects of the staggered adoption of the copayment:

# Adoption: Stacked DD and DDD models:
source(file="W:/ASMA3/analysis/2_introduction_stacked_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: intro_trends_data.rds + intro_dynamic_plot_pri.pdf +
#               intro_dynamic_plot_sec.pdf + 
#               intro_dd_pri(xlsx + tex + rds) +
#               intro_dd_sec(xlsx + tex + rds) +
#               intro_dd_pri_log(xlsx + tex + rds) +
#               intro_dd_pri_unbalanced(xlsx + tex + rds) +
#               intro_ddd_pri(xlsx + tex + rds) +
#               intro_results_stacking.rds
# Running time 5 minutes. 

# Adoption: CS estimator:
source(file="W:/ASMA3/analysis/2_introduction_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: results.rds + intro_dynamic_plot_cs_pri_short.pdf +
#               intro_dynamic_plot_cs_sec_short.pdf + 
#               intro_att_plot_cs_pri.pdf + intro_att_plot_cs_pri_log.pdf +
#               intro_att_plot_cs_sec.pdf
# Running time 25 minutes.


# Analyze the effects of the copayment abolition of 1.7.2021:

# Adoption and Abolition: assess pre-trends:
source(file="W:/ASMA3/analysis/2_trend_plots.R")
#     Inputs: intro_trends_data.rds + analysis_data_aggr.rds
#     Outputs: intro_trends_plot_nurse.pdf + intro_trends_plot_gp.pdf +
#               intro_trends_plot_sa.pdf + 
#               abol_trends_plot_nurse.pdf + abol_trends_plot_gp.pdf +
#               abol_trends_plot_drugs.pdf + abol_trends_plot_refs.pdf
# Running time <0.5 minutes.

# Abolition: DD and DDD models:
source(file="W:/ASMA3/analysis/2_abolition_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: abol_dd_main_contacts_per_capita(xlsx + tex + rds) + 
#               abol_dd_main_contacts_per_capita_log(xlsx + tex + rds) + 
#               abol_dd_placebo_main.pdf +
#               abol_ddd_main_contacts_per_capita(xlsx + tex + rds) +
#               abol_ddd_placebo_main.pdf
# Running time <0.5 minutes. 

# Abolition: CS estimator:
source(file="W:/ASMA3/analysis/2_abolition_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: abol_dd_main_cs_contacts_per_capita(xlsx + tex + rds)
# Running time <0.5 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Unweighted estimates. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Analyze the effects of the staggered adoption of the copayment:

# Adoption: Stacked DD and DDD models:
source(file="W:/ASMA3/analysis/unweighted/2_uw_introduction_stacked_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_intro_trends_data.rds + uw_intro_dynamic_plot_pri.pdf +
#               uw_intro_dynamic_plot_sec.pdf + 
#               uw_intro_dd_pri(xlsx + tex + rds) +
#               uw_intro_dd_sec(xlsx + tex + rds) +
#               uw_intro_dd_pri_log(xlsx + tex + rds) +
#               uw_intro_dd_pri_unbalanced(xlsx + tex + rds) +
#               uw_intro_ddd_pri(xlsx + tex + rds) +
#               uw_intro_results_stacking.rds
# Running time 5 minutes. 

# Adoption: CS estimator:
source(file="W:/ASMA3/analysis/unweighted/2_uw_introduction_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_results.rds + uw_intro_dynamic_plot_cs_pri_short.pdf + 
#               uw_intro_dynamic_plot_cs_sec_short.pdf + 
#               uw_intro_att_plot_cs_pri.pdf + 
#               uw_intro_att_plot_cs_pri_log.pdf +
#               uw_intro_att_plot_cs_sec.pdf
# Running time 25 minutes.


# Analyze the effects of the copayment abolition of 1.7.2021:

# Adoption and Abolition: assess pre-trends:
source(file="W:/ASMA3/analysis/unweighted/2_uw_trend_plots.R")
#     Inputs: uw_intro_trends_data.rds + analysis_data_aggr.rds
#     Outputs: uw_intro_trends_plot_nurse.pdf + uw_intro_trends_plot_gp.pdf +
#               uw_intro_trends_plot_sa.pdf + 
#               uw_abol_trends_plot_nurse.pdf + uw_abol_trends_plot_gp.pdf +
#               uw_abol_trends_plot_drugs.pdf + uw_abol_trends_plot_refs.pdf
# Running time <0.5 minutes.

# Abolition: DD and DDD models:
source(file="W:/ASMA3/analysis/unweighted/2_uw_abolition_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_abol_dd_main_contacts_per_capita(xlsx + tex + rds) + 
#               uw_abol_dd_main_contacts_per_capita_log(xlsx + tex + rds) + 
#               uw_abol_dd_placebo_main.pdf +
#               uw_abol_ddd_main_contacts_per_capita(xlsx + tex + rds) +
#               uw_abol_ddd_placebo_main.pdf
# Running time <0.5 minutes. 

# Abolition: CS estimator:
source(file="W:/ASMA3/analysis/unweighted/2_uw_abolition_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_abol_dd_main_cs_contacts_per_capita(xlsx + tex + rds)
# Running time <0.5 minutes.


Sys.time()

# End.

