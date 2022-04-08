
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           SAS script 0_referrals.sas          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read and mutate referrals to specialized healthcare from Hilmo.

Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2)
Output: Output: referrals_YY where YY in (12:20)

Libnames: */

libname refs "D:\d66\external\THL_aineisto_2019_2020";
libname interm "W:\ASMA3\data\interim";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and mutate data on referrals to specialized healthcare. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, read the data;

options nolabel;

%MACRO read_data;
%DO i = 1 %TO 2;

data refs_&i;
	set refs.tutkpalv_1418_thl_hilmo&i._s;

	* IDs must be observed;
	* The date of arrival of the referral must be observed;
	where not missing(shnro) and not missing(lanttupva);
	
	* Create a variable for weekday, month and year of the arrival of the referral;
	date = input(substr(strip(lanttupva), 1, 10), DDMMYY10.);
	year = year(date);
	month = month(date);
	day = day(date);

	* Create a dummy for whether we observe the date when the patient came in;
	if not missing(tupva) then contact_started = 1;

	* Create a dummy for whether we observe when the patient is discharged;
	if not missing(lpvm) then discharged = 1;
	
	* Keep only relevant variables;
	keep shnro lant year month day discharged contact_started;

	* Rename columns;
	rename shnro=id lant=ref_origin;

	length month year day discharged contact_started 3;

run;

%END;
%MEND read_data;

%read_data;


* Concatenate;
data refs;
	set refs_1 refs_2;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 2012 %TO 2020;

proc export data=refs(where= (year=&i.))
	outfile= "W:\ASMA3\data\interim\referrals_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
