
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###        SAS script 0_drug_prescriptions.sas    ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits     ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This script extracts data on drug prescriptions and saves them to csv. 

Inputs: t141_522_2021_lm_2020_Y_s where Y in 1:2 +
			lm_72_21_01_06_s + lm_72_21_07_12_s + lm_72_22_01_05_s
Output: prescriptions_Y where Y in 1:5


Libnames: */

libname drugs "D:\d66\external\laakemaaraykset2018_20";
libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* 2020;

%MACRO read_data;
%DO i = 1 %TO 2;

data presc_&i;
	set drugs.t141_522_2021_lm_2020_&i._s;

	* Take only prescriptions written by the public sector. This excludes;
	* prescriptions that were written at health centers that were run;
	* by private firms as a results of outsourcing. ;

	where doc_type_code = 1 and not missing(shnro) and sector = 1;

	keep shnro date_pk;
run;

%END;
%MEND read_data;

%read_data;


* 2021-2022;

data presc_3;
	set hilmot.lm_72_21_01_06_s;

	* Take only prescriptions written by the public sector. This excludes;
	* prescriptions that were written at health centers that were run;
	* by private firms as a results of outsourcing. ;

	where doc_type_code = 1 and not missing(shnro) and sector = 1;

	keep shnro date_pk;
run;

data presc_4;
	set hilmot.lm_72_21_07_12_s;

	* Take only prescriptions written by the public sector. This excludes;
	* prescriptions that were written at health centers that were run;
	* by private firms as a results of outsourcing. ;

	where doc_type_code = 1 and not missing(shnro) and sector = 1;

	keep shnro date_pk;
run;

data presc_5;
	set hilmot.lm_72_22_01_05_s;

	* Take only prescriptions written by the public sector. This excludes;
	* prescriptions that were written at health centers that were run;
	* by private firms as a results of outsourcing. ;

	where doc_type_code = 1 and not missing(shnro) and sector = 1;

	keep shnro date_pk;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 1 %TO 5;

proc export data=presc_&i
	outfile= "W:\ASMA3\data\interim\prescriptions_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
