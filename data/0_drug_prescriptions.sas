
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

Inputs: t141_522_2021_lm_20XX_Y_s where XX in 18:20 and Y in 1:2
Output: prescriptions_Y_20XX where XX in 18:20 and Y in 1:2

Libnames: */

libname drugs "D:\d66\external\laakemaaraykset2018_20";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO read_data;
%DO i = 2018 %TO 2020;

data presc_1_&i;
	set drugs.t141_522_2021_lm_&i._1_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro);

	keep shnro date_pk med_init_code sector;
run;

%END;
%MEND read_data;

%read_data;


%MACRO read_data;
%DO i = 2018 %TO 2020;

data presc_2_&i;
	set drugs.t141_522_2021_lm_&i._2_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro);

	keep shnro date_pk med_init_code sector;
run;

%END;
%MEND read_data;

%read_data;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 2018 %TO 2020;

proc export data=presc_1_&i
	outfile= "W:\ASMA3\data\interim\prescriptions_1_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;


%MACRO save_data;
%DO i = 2018 %TO 2020;

proc export data=presc_2_&i
	outfile= "W:\ASMA3\data\interim\prescriptions_2_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;


* End;
