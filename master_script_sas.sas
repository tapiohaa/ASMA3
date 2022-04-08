
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

* Avohilmo visit data;
libname hilmo "D:\d66\external";
libname hilmon "D:\d66\external\THL_aineisto_2019_2020";

* Data on referrals to specialized healthcare;
libname refs "D:\d66\external\THL_aineisto_2019_2020";

* Data on drug prescriptions;
libname drugs "D:\d66\external\laakemaaraykset2018_20";

* Data on social assistance;
libname toimtu "D:\d66\external";

* FOLK modules basic, income, and family;
libname fbasic "D:\ready-made\FOLK_perus_11a";
libname fincome "D:\ready-made\FOLK_tulo_11a";
libname ffamily "D:\ready-made\FOLK_perh_11a";

/*###
###*/

* Remove comment symbols before running the scripts one at a time;

/* 

filename storage "W:\ASMA3\data";

* Extract socioeconomic data;
%inc storage("0_folk_data.sas");					* Approximately XX minutes;
* Inputs: folk_20112020_tua_perus21tot_1 + folk_20112019_tua_tulo21tot_1 + ;
*		folk_20112020_tua_perh21tot_1 + tutkpalv_u1418_toitu_2012_2018_s (.sas7bdat);
* Output: folk_data_20xx where xx in (11:19) (.csv);

* Extract primary care data;
%inc storage("0_visits.sas");						* Approximately XX minutes;
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			tutkpalv_u1418_avohilmo201X_s where x in (2:8) (.sas7bdat) + ;
*			thl4768_avohilmo_20XX_Y_s where xx in (19:20) and y in (1:2) (.sas7bdat);
* Outputs: visits_xx where xx in (12:20) (.csv);

* Extract data on drug prescriptions;
%inc storage("0_drug_prescriptions.sas");			* Approximately XX minutes;
* Inputs: t141_522_2021_lm_20XX_Y_s where XX in 18:20 and Y in 1:2 (.sas7bdat);
* Outputs: prescriptions_Y_20XX where XX in 18:20 and Y in 1:2 (.csv);

* Extract data on referrals to specialized healthcare;
%inc storage("0_referrals.sas");					* Approximately XX minutes;
* Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2) (.sas7bdat);
* Outputs: referrals_YY where YY in (12:20) (.csv);

*/


* End;


