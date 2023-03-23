
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          SAS script 0_hospital_visits.sas     ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read and mutate a) hospitalizations for ambulatory care-sensitive 
conditions and b)urgent visits to specialized healthcare from Hilmo.

Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2) + 
		tutkpalv_1418_thl4768_hilmo_diag
Output: hospital_visits + hospital_diagnoses

Libnames: */

libname hosp "D:\d66\external\THL_aineisto_2019_2020";
libname interm "W:\ASMA3\data\interim";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and mutate data on hospital use. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

options nolabel;

%MACRO read_data;
%DO i = 1 %TO 2;

data hospital_use_&i;
	set hosp.tutkpalv_1418_thl_hilmo&i._s;

	* Person and visit IDs and arrival date must be observed;
	where not missing(shnro) and not missing(kaynti_id) and not missing(tupva);
	
	* Create a variable for weekday, month and year of the arrival and discharge;

	startdate = input(substr(strip(tupva), 1, 10), DDMMYY10.);
	startyear = year(startdate);
	startmonth = month(startdate);
	startday = day(startdate);

	enddate = input(substrn(strip(lpvm), 1, 10), DDMMYY10.);
	endyear = year(enddate);
	endmonth = month(enddate);
	endday = day(enddate);

	* Dummies for inpatient visits and urgent inpatient visits;
	if pala=1 or yhteystapa='R80' then inpatient=1;
	if (pala=1 and satap=1) or (yhteystapa='R80' and kiireellisyys='3') then urgent_inpatient=1;

	* Dummies for emergency department visits;
	if pala=91 or (kiireellisyys='6' and yhteystapa='R10') then emergency_dep=1;

	* Keep only relevant variables;
	keep shnro kaynti_id
		inpatient urgent_inpatient emergency_dep
		startyear startmonth startday endyear endmonth endday;

	* Rename columns;
	rename shnro=id;

	length startyear startmonth startday endyear endmonth endday
			inpatient urgent_inpatient emergency_dep 3;

run;

* Keep inpatient episodes and emergency department visits;

data hospital_use_&i;
	set hospital_use_&i;
	where startyear >= 2012 and (inpatient=1 or emergency_dep=1);
run;

%END;
%MEND read_data;

%read_data;


data hospital_use;
	set hospital_use_1 hospital_use_2;
run;

proc export data=hospital_use
	outfile= "W:\ASMA3\data\interim\hospital_visits.csv"
	dbms=csv;
run;

/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save diagnoses to csv.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

data diagnoses;
	set hosp.tutkpalv_1418_thl4768_hilmo_diag;
	drop icd10e jarj;
run;

proc export data=diagnoses
	outfile= "W:\ASMA3\data\interim\hospital_diagnoses.csv"
	dbms=csv;
run;

* End;
