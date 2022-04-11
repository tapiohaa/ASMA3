# README for replication files 

Article: “Cost Sharing and Primary Care Use: Evidence from Staggered Copayment Adoption and Later Abolition” (Haaga, Böckerman, Kortelainen, and Tukiainen, 2022)

Pre-analysis plan: https://doi.org/10.17605/OSF.IO/FV2GA

Date: April 2022 <br>


## The contents

This replication folder contains the statistical programs used to clean the data and conduct the analyses. The root of the replication folder includes a folder for each publicly available version of the text, from the registered pre-analysis plan to the final research report. Besides the statistical programs, we include in subfolders all result tables and figures that do not appear in the paper nor in its appendix.

## Access to the data

We link several Finnish administrative individual-level registries via unique person IDs. To get access to the data, one must apply for data permits. Two applications must be sent: one to Findata and another to Statistics Finland. The former grants permits to Finnish healthcare data while the latter provides the socioeconomic and sociodemographic data used in the study. The higher the quality of the initial submission, the shorter it takes to get an approval. Once the data permits are approved, the data controllers (THL and Kela in this study) transfer their data to Statistics Finland for pseudonymization. Researchers get access to the pseudonymized data for a fixed period of time via Fiona, a remote access system provided by Statistics Finland.

Each case is naturally different, but we would expect the entire process from application submission to the point where the data work can begin to take approximately a year. However, we would not characterize the process as easy and swift for first-timers. 

There are other barriers to access besides the wait times and filling the application forms correctly. Accessing the data via a remote access system while being in another country is counted as a transfer of personal data, which is regulated by Finland and the EU. The application process, data extractions, and the remote access use naturally contain costs.

## Reproducing the analysis

Exactly reproducing the results is unfortunately likely not possible because receiving exactly the same data that we use is unlikely due to reasons not in our control. First and foremost, the registries we use are continuously updated. The data from 2015 extracted in 2021 may be different from the data from 2015 extracted in 2023. We do not extract the raw data from databases. Instead, this is done by the data controllers (THL and Kela). However, the differences should be small and the estimates should be robust to the underlying sample changing a little bit.

### Software

Several statistical software are offered in the Fiona remote access system. We first use SAS 9.4 to extract smaller subsets of the larger data tables. Most of the data cleaning, construction, and analyses are conducted with R 4.0.5. 

### Folder structure

Suppose we want to reproduce the placebo results in the pre-analysis plan (PAP). First, create a folder called *ASMA3* to the root of the work directory (W:/ASMA3). Then, copy all the files in the *pap* folder of this replication folder to the *ASMA3* folder in Fiona, and create an R project called *ASMA3* to the root of that folder.

The root of the *ASMA3* folder then contains master scripts for R and SAS codes and File *sessionInfo.txt* that list the R packages and their versions used in analysis. The master scripts contain a lot of important information: 1) the list of all scripts, 2) the order in which the scripts should be run, 3) inputs and outputs of each script, and 4) a short comment on what each script does. Furthermore, they report the running time of each script.

Folder *data* includes the R and SAS scripts used to clean and process the data. Raw, publicly available data are in Subfolder *raw*. Intermediate, processed datasets should be in Folder *interim* while the final, analysis-ready datasets should be in Folder *cleaned*. Folder *analysis* contains the R scripts for analyses and Subfolders *figures* and *tables* for results. Make sure that all these folders are created.

### File paths

All file paths appearing at the top of each script are absolute file paths. Note that you will have to edit the file paths to the raw microdata depending on where your data is in the data directory (D:/). Once these paths are correct and you have created the folder structure described above, the rest of the absolute file paths should work fine.

### Implementation

SAS scripts need to be run before R scripts. Ideally, the analysis could be reproduced by simply running the following scripts in the order:
1. W:/ASMA3/master_script_sas.sas
2. W:/ASMA3/master_script_r.R

However, when running the scripts at the first time, we highly propose running each of them script by script, guided by the above master scripts.
