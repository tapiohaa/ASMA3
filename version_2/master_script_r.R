
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script master_script_r.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Implement the R-scripts required to replicate the analysis.

# R version 4.0.5, RStudio version 1.2.5033.
# Running time approximately 170 minutes. 
rm(list=ls())

# First, make sure that the SAS scripts (listed in master_script_sas.sas) 
# have run successfully.


# To install packages from a CRAN mirror repository in FIONA:
# 1) Create .Rprofile file to the root where the project is:
#     local({
#       r <- getOption("repos")
#       r["CRAN"] <- "https://cran.isaacus.local/"
#       options(repos = r)
#     })
# 2) Restart RStudio. Now you can load CRAN packages by install.packages():
# 3) Use the library() function to load packages.


# The packages loaded are listed below (versions in parentheses).
library(data.table)       # Mutating data. 
library(readxl)           # Reading xlsx files. 
library(ggplot2)          # Plotting data. 
library(viridis)          # colorblind-friendly color maps.
library(patchwork)        # Print multiple plots into same figure. 
library(sf)               # simple features.
library(openxlsx)         # Save as excel file. 
library(stargazer)        # Save as tex file. 
library(fastDummies)      # Fast creation of dummy columns. 
library(lfe)              # Linear fixed effects estimation. 
library(broom)            # Statistical objects into tidy tibbles. 
library(did)              # Staggered DID designs. 
library(staggered)        # Roth & Sant'Anna (2022): staggered DID

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
#			      munies_geom_2020.rds
#     Outputs: adoption_policies.rds + abolition_policies.rds + 
#           copay_policies_adopt.pdf + copay_policies_abolition.pdf +
#           copay_policies_euros.pdf + 
#           copay_policies_map_intro.pdf + copay_policies_map_abol.pdf
# Running time <0.5 minutes.


# Read and tidy data on GP visit copayments:
source(file="W:/ASMA3/data/0_copayments_gp.R")
#     Inputs: copay_raw.csv + municipalities_2013.csv + copayments_gp.rds 
#     Outputs: copayments_gp.rds
# Running time <0.5 minutes.


# Extract only publicly-funded primary care visits from 2019-2022:
source(file="W:/ASMA3/data/0_public_visits.R")
#     Inputs: sote_munies_2021.csv + topi_unit_register_2022_06.csv + 
#             sote_organisation_register.xlsx + sote_2022_02.xlsx + 
#             visits_20XX.csv (XX in 19:22)
#     Outputs: visits_public_20XX.rds (XX in 19:20)
# Running time 7 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Create analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Content: Clean the data on ED visits and inpatient spells.
source(file="W:/ASMA3/data/1_hospital_visits.R")
#     Input: hospital_diagnoses.csv + hospital_visits.csv
#     Outputs: hospital_visits_clean.csv
# Running time approximately 42 minutes.


# Link visits, referrals, prescriptions, and social assistance data
# to the socioeconomic data and aggregate outcome:
source(file="W:/ASMA3/data/1_link_datasets.R")
#     Input: folk_data_20XX.csv (XX in 12:20) + visits_20XX.csv (XX in 12:18) + 
#           visits_public_20XX.csv (XX in 19:22) + 
#           referrals_20XX.csv (XX in 20:22) + 
#			hospital_visits_clean.csv + 
#           prescriptions_X.csv (X in 1:5)
#     Outputs: social_assistance_by_deciles.pdf + visit_data_aggr.rds
# Running time approximately 28 minutes.


# Analyze visit data quality, and create analysis data:
source(file="W:/ASMA3/data/1_data_quality.R")
#     Input: visit_data_aggr.rds + adoption_policies.rds + 
#           abolition_policies.rds + copayments_gp.rds
#     Outputs: visits_pandemic.pdf + 
#           visits_quality_aggr_introduction_weird_1.pdf + 
#           visits_quality_aggr_introduction_weird_2.pdf + 
#           visits_quality_aggr_introduction_weird_3.pdf +
#           visits_quality_aggr_abolition_weird.pdf + 
#           visits_quality_aggr_introduction_normal.pdf +
#           visits_quality_aggr_abolition_normal.pdf
#           analysis_data_aggr.rds 
# Running time 5 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Main analyses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Analyze the effects of the staggered adoption of the copayment:

# Adoption: Stacked DD and DDD models:
source(file="W:/ASMA3/analysis/2_introduction_stacked_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: intro_trends_data.rds + intro_dynamic_plot_pri.pdf +
#               intro_dynamic_plot_sec.pdf + intro_dynamic_plot_all.pdf + 
#               intro_dynamic_plot_pri_log.pdf + 
#               intro_dynamic_plot_preventive.pdf
#               intro_dd_pri(xlsx + tex + rds) +
#               intro_dd_sec(xlsx + tex + rds) +
#               intro_dd_pri_rob(xlsx + tex + rds) +
#               intro_dd_all(xlsx + tex + rds) + 
#               intro_dd_inc.pdf + 
#               intro_ddd_pri(xlsx + tex + rds) +
#               intro_results_stacking.rds +
#               intro_dd_pri_24m(xlsx + tex + rds)
# Running time 7 minutes. 

# Adoption: CS estimator:
source(file="W:/ASMA3/analysis/2_introduction_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: intro_results_cs.rds + intro_dynamic_plot_cs_pri_short.pdf +
#               intro_dynamic_plot_cs_sec_short.pdf + 
#               intro_dynamic_plot_cs_all_short.pdf + 
#               intro_dynamic_plot_cs_pri_log.pdf + 
#               intro_dynamic_plot_cs_pri_long.pdf + 
#				intro_dynamic_plot_cs_ter_long.pdf +
#               intro_att_plot_cs_pri.pdf + intro_att_plot_cs_pri_log.pdf +
#               intro_att_plot_cs_sec.pdf + intro_att_plot_cs_all.pdf +
#               intro_attevent_plot_cs_pri.pdf + intro_att_plot_cs_ter.pdf
# Running time 40 minutes.

# Adoption: RS estimator:
source(file="W:/ASMA3/analysis/2_introduction_rs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: intro_results_rs.rds + intro_dd_rs(xlsx + tex + rds) + 
#               intro_dd_rs_log(xlsx + tex + rds)
# Running time 2 minutes.


# Analyze the effects of the copayment abolition of 1.7.2021:

# Adoption and Abolition: assess pre-trends:
source(file="W:/ASMA3/analysis/2_trend_plots.R")
#     Inputs: intro_trends_data.rds + analysis_data_aggr.rds
#     Outputs: intro_trends_plot_copay_gp.pdf + intro_trends_plot_nurse.pdf + 
#             intro_trends_plot_gp.pdf + intro_trends_plot_sa.pdf + 
#               intro_trends_plot_all.pdf + intro_trends_plot_preventive.pdf + 
#               abol_trends_plot_nurse.pdf + abol_trends_plot_gp.pdf +
#               abol_trends_plot_drugs.pdf + abol_trends_plot_all.pdf
# Running time <0.5 minutes.

# Abolition: DD and DDD models:
source(file="W:/ASMA3/analysis/2_abolition_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: abol_dd_main_contacts_per_capita(xlsx + tex + rds) + 
#               abol_dd_main_contacts_per_capita_log(xlsx + tex + rds) + 
#               abol_ddd_main_contacts_per_capita(xlsx + tex + rds)
# Running time <0.5 minutes. 

# Abolition: CS estimator:
source(file="W:/ASMA3/analysis/2_abolition_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: abol_dd_main_cs_contacts_per_capita(xlsx + tex + rds) +
#             abol_dd_cs_all_contacts_per_capita(xlsx + tex + rds)
# Running time <0.5 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Unweighted estimates. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Analyze the effects of the staggered adoption of the copayment:

# Adoption: Stacked DD and DDD models:
source(file="W:/ASMA3/analysis/unweighted/2_uw_introduction_stacked_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_intro_trends_data.rds + uw_intro_dynamic_plot_pri.pdf +
#               uw_intro_dynamic_plot_sec.pdf + uw_intro_dynamic_plot_all.pdf + 
#               uw_intro_dynamic_plot_pri_log.pdf + 
#               uw_intro_dd_pri(xlsx + tex + rds) +
#               uw_intro_dd_sec(xlsx + tex + rds) +
#               uw_intro_dd_pri_rob(xlsx + tex + rds) +
#               uw_intro_dd_all(xlsx + tex + rds) +
#               uw_intro_dd_inc.pdf + 
#               uw_intro_ddd_pri(xlsx + tex + rds) +
#               uw_intro_results_stacking.rds
# Running time 7 minutes. 

# Adoption: CS estimator:
source(file="W:/ASMA3/analysis/unweighted/2_uw_introduction_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_results.rds + uw_intro_dynamic_plot_cs_pri_short.pdf + 
#               uw_intro_dynamic_plot_cs_sec_short.pdf + 
#               uw_intro_dynamic_plot_cs_all_short.pdf +
#               uw_intro_dynamic_plot_cs_pri_log.pdf + 
#               uw_intro_dynamic_plot_cs_pri_long.pdf
#               uw_intro_att_plot_cs_pri.pdf + 
#               uw_intro_att_plot_cs_pri_log.pdf +
#               uw_intro_att_plot_cs_sec.pdf + uw_intro_att_plot_cs_all.pdf +
#               uw_intro_attevent_plot_cs_pri.pdf
# Running time 32 minutes.


# Analyze the effects of the copayment abolition of 1.7.2021:

# Adoption and Abolition: assess pre-trends:
source(file="W:/ASMA3/analysis/unweighted/2_uw_trend_plots.R")
#     Inputs: uw_intro_trends_data.rds + analysis_data_aggr.rds
#     Outputs: uw_intro_trends_plot_copay_gp.pdf + 
#             uw_intro_trends_plot_nurse.pdf + uw_intro_trends_plot_gp.pdf +
#             uw_intro_trends_plot_sa.pdf + uw_intro_trends_plot_all.pdf + 
#             uw_abol_trends_plot_nurse.pdf + uw_abol_trends_plot_gp.pdf +
#             uw_abol_trends_plot_drugs.pdf + uw_abol_trends_plot_all.pdf
# Running time <0.5 minutes.

# Abolition: DD and DDD models:
source(file="W:/ASMA3/analysis/unweighted/2_uw_abolition_dd.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_abol_dd_main_contacts_per_capita(xlsx + tex + rds) + 
#               uw_abol_dd_main_contacts_per_capita_log(xlsx + tex + rds) + 
#               uw_abol_ddd_main_contacts_per_capita(xlsx + tex + rds) +
#               uw_abol_ddd_placebo_main.pdf
# Running time <0.5 minutes. 

# Abolition: CS estimator:
source(file="W:/ASMA3/analysis/unweighted/2_uw_abolition_cs.R")
#     Inputs: analysis_data_aggr.rds
#     Outputs: uw_abol_dd_main_cs_contacts_per_capita(xlsx + tex + rds) +
#             uw_abol_dd_cs_all_contacts_per_capita(xlsx + tex + rds)
# Running time <0.5 minutes.

Sys.time()

# End.

