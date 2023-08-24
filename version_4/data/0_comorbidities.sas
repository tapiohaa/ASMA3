
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           SAS script 0_comorbidities.sas      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits     ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This script extracts data on drug prescriptions and checks whether
			a person has had a prescription related to specific drugs (e.g diabetes). 

Inputs: t141_522_2021_lm_2018_1_s + t141_522_2021_lm_2018_2_s +
		t141_522_2021_lm_2019_1_s + t141_522_2021_lm_2019_2_s
Output: comorbidities

Libnames: */

libname laake "D:\d66\external\laakemaaraykset2018_20";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

/* Extract prescriptions with the following ATC therapeutic subgroups:
('A10', 'C02', 'C03', 'C07', 'C08', 'C09') (diabetes or hypertension) 
*/

data presc_1;
	set laake.t141_522_2021_lm_2018_1_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro) and
		substr(atc_code, 1, 3) in ('A10', 'C02', 'C03', 'C07', 'C08', 'C09');

	keep shnro;
run;

data presc_2;
	set laake.t141_522_2021_lm_2018_2_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro) and
		substr(atc_code, 1, 3) in ('A10', 'C02', 'C03', 'C07', 'C08', 'C09');

	keep shnro;
run;

data presc_3;
	set laake.t141_522_2021_lm_2019_1_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro) and
		substr(atc_code, 1, 3) in ('A10', 'C02', 'C03', 'C07', 'C08', 'C09');

	keep shnro;
run;

data presc_4;
	set laake.t141_522_2021_lm_2019_2_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro) and
		substr(atc_code, 1, 3) in ('A10', 'C02', 'C03', 'C07', 'C08', 'C09');

	keep shnro;
run;


data presc;
	set presc_1 presc_2 presc_3 presc_4;
run;

proc sort data=presc out=morb nodupkey;
	by _all_;
run;

data morb;
	set morb;
	morbidity = 1;
run;

/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

proc export data=morb
	outfile= "W:\ASMA3\data\interim\morbidities.csv"
	dbms=csv;
run;

* End;
