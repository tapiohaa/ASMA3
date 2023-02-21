
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         SAS script master_script_sas.sas      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###          Funded Primary Care: Nurse Visits    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This proposes an order with which to run SAS scripts. 

Note: We do not recommend running all the scripts at once as the SAS work
	library should be (manually) made empty between runs. 

Libnames: */

* Folders for processed datasets;
libname raw "W:\ASMA3\data\raw";
libname interm "W:\ASMA3\data\interim";

* Data from 2011-2020;

* Avohilmo visit data;
libname hilmo "D:\d66\external";
libname hilmon "D:\d66\external\THL_aineisto_2019_2020";

* Data on drug prescriptions;
libname drugs "D:\d66\external\laakemaaraykset2018_20";

* Data on social assistance;
libname toimtu "D:\d66\external";

* Data from 2020-2022;
* Primary care contacts and referrals; drug prescriptions;
libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

* FOLK modules basic, income, and family;
libname fbasic "D:\ready-made\CONTINUOUS\FOLK_PERUS_C";
libname fincome "D:\ready-made\CONTINUOUS\FOLK_TULO_C";
libname ffamily "D:\ready-made\CONTINUOUS\FOLK_PERH_C";

/*###
###*/

* Remove comment symbols before running the scripts one at a time;

/* 

filename storage "W:\ASMA3\data";

* Extract socioeconomic data;
%inc storage("0_folk_data.sas");					* Approximately 62 minutes;
* Inputs: folk_20112020_tua_perus22tot_1 + folk_tulo_20XX_1 where XX in (11:20) + ;
*		folk_20112020_tua_perh21tot_1 + tutkpalv_u1418_toitu_2012_2018_s (.sas7bdat) + ;
*		toitu_2019_s;
* Output: folk_data_20xx where xx in (11:20) (.csv);

* Extract primary care data;
%inc storage("0_visits.sas");						* Approximately 140 minutes;
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			tutkpalv_u1418_avohilmo201X_s where x in (2:8) + ;
*			thl4768_avohilmo_2019_Y_s where Y in (1:2) + ;
*			tutkpalv_1418_avohilmo_2021_Y_s where Y in (1:2) (.sas7bdat);
* Outputs: visits_20xx where xx in (12:22) (.csv);

* Extract data on drug prescriptions;
%inc storage("0_drug_prescriptions.sas");			* Approximately 8 minutes;
* Inputs: t141_522_2021_lm_2020_Y_s where Y in 1:2 + lm_72_21_01_06_s + ;
*			lm_72_21_07_12_s + lm_72_22_01_05_s (.sas7bdat) ;
* Outputs: prescriptions_Y where Y in 1:5 (.csv);

* Extract data on referrals to specialized healthcare;
%inc storage("0_referrals.sas");					* Approximately 6 minutes;
* Inputs: tutkpalv_1418_hilmo_2021_s (.sas7bdat);
* Outputs: referrals_20YY where YY in (20:22) (.csv);

* Extract data on ED visits and ACSC inpatient admissions;
%inc storage("0_hospital_visits.sas");				* Approximately 19 minutes;
* Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2) + ;
* 			tutkpalv_1418_thl4768_hilmo_diag (.sas7bdat);
* Outputs: hospital_visits + hospital_diagnoses (.csv);

*/


* End;


