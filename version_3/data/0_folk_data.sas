
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           SAS script 0_folk_data.sas          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Nurse Visits.    ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This script merges FOLK modules 'basic information', 
			'income', and 'family' and data on social assistance;

Inputs: folk_20112020_tua_perus22tot_1 + folk_tulo_20XX_1 where XX in (11:20)+ 
			folk_20112020_tua_perh21tot_1 + tutkpalv_u1418_toitu_2012_2018_s +
			toitu_2019_s
Output folk_data_20XX where XX in (11:20)

Libnames: */

libname fbasic "D:\ready-made\CONTINUOUS\FOLK_PERUS_C";
libname fincome "D:\ready-made\CONTINUOUS\FOLK_TULO_C";
libname ffamily "D:\ready-made\CONTINUOUS\FOLK_PERH_C";
libname toimtu "D:\d66\external";
libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

options nolabel;


* Read the three datasets and keep only relevant variables + sort them by id;

data basic;
	set fbasic.folk_20112020_tua_perus22tot_1;
	where vuosi IN (2011:2020);

	* Keep only relevant variables;
	keep vuosi shnro kunta31_12 ika;

	* Rename columns;
	rename shnro=id kunta31_12=municipality ika=age vuosi=year;

	length vuosi 3;

run;

proc sort Data=basic;
	by id year;
run;


%MACRO read_data;
%DO i = 2011 %TO 2020;

data income_&i;
	set fincome.folk_tulo_&i._1;

	* Keep only relevant variables;
	keep vuosi shnro kturaha;

	* Rename columns;
	rename shnro=id kturaha=income_disp vuosi=year;

	length vuosi 3;

run;

%END;
%MEND read_data;

%read_data;

data income;
	set income_2011 income_2012 income_2013 income_2014 income_2015
		income_2016 income_2017 income_2018 income_2019 income_2020;
run;
	
proc sort Data=income;
	by id year;
run;


data family;
	set ffamily.folk_20112020_tua_perh21tot_1;
	where vuosi IN (2011:2020);

	* Keep only relevant variables;
	keep shnro petu pekoko vuosi;

	* Rename columns;
	rename shnro=id petu=family_id pekoko=family_size vuosi=year;

	length vuosi pekoko 3;

run;

proc sort Data=family;
	by id year;
run;


* Next, merge all three datasets by id;
* If family code is missing or if the person has a single-person;
* family, use personal identifier as the family code;

data folk_data;
	merge basic income family;
	by id year;
run;

* if family id is not observed, use person id;

proc sql;
	alter table folk_data
	modify family_id char(16) format=$16.;
quit;

data folk_data;
	set folk_data;
	if family_id=. then family_id=id;
	*'00000000' = Person does not belong to the family population but;
	* does have a adopted/biological child;
	if family_id='00000000' then family_id=id; 
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read and merge data on social assistance to folk_data.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* 2012-2018;

data social_assistance;
	set toimtu.tutkpalv_u1418_toitu_2012_2018_s;
	where not missing(shnro_hakija);

	* Keep only relevant variables;
	keep tilastovuosi shnro_hakija tammi helmi maalis huhti touko kesa
			heina elo syys loka marras joulu perus_toimeentulotuki_eur;

	* Rename columns;
	rename shnro_hakija=id tilastovuosi=year tammi=jan helmi=feb maalis=mar
			huhti=apr touko=may kesa=june heina=july elo=aug syys=sep 
			loka=oct marras=nov joulu=dec perus_toimeentulotuki_eur=basic_social_assistance;

	length tilastovuosi 3;

run;

* 2019;

data social_assistance_2;
	set hilmot.toitu_2019_s;
	where not missing(shnro_hakija);

	jan = input(tammi, 8.);
	feb = input(helmi, 8.);
	mar = input(maalis, 8.);
	apr = input(huhti, 8.);
	may = input(touko, 8.);
	june = input(kesa, 8.);
	july = input(heina, 8.);
	aug = input(elo, 8.);
	sep = input(syys, 8.);
	oct = input(loka, 8.);
	nov = input(marras, 8.);
	dec = input(joulu, 8.);


	* Keep only relevant variables;
	keep tilastovuosi shnro_hakija jan feb mar apr may june
			july aug sep oct nov dec perus_toimeentulotuki_eur;

	* Rename columns;
	rename shnro_hakija=id tilastovuosi=year perus_toimeentulotuki_eur=basic_social_assistance;

	length tilastovuosi 3;

run;

data social_assistance;
	set social_assistance social_assistance_2;
run;

proc sort Data=social_assistance;
	by id year;
run;


* For some individuals there are more than one row in a given year;
* Aggregate the data in the following way;
* If person X in time Y is observed to have received social assistance;
* in one or more of the potential corresponding rows, define that the;
* person received social assistance;

proc sql;
	create table doubles as
	select id, year, count(id) as N
	from social_assistance
	group by id, year;
quit;

proc sort Data=doubles;
	by id year;
run;

* Merge the count to 'social_assistance';

data social_assistance;
	merge social_assistance doubles;
	by id year;
run;

* Extract the data on those persons who have multiple person-year observations;

data doubles;
	set social_assistance;
	where N > 1;
run;

* Aggregate (a person-year observation may have a value larger than 1);

proc sql;
	create table doubles_aggr as
	select id, year,
		sum(jan) as jan,
		sum(feb) as feb,
		sum(mar) as mar,
		sum(apr) as apr,
		sum(may) as may,
		sum(june) as june,
		sum(july) as july,
		sum(aug) as aug,
		sum(sep) as sep,
		sum(oct) as oct,
		sum(nov) as nov,
		sum(dec) as dec
	from doubles
	group by id, year;
quit;

* Drop those person-year observations that have several rows;

data social_assistance;
	set social_assistance;
	where N = 1;
	drop N;
run;

* Concatenate;

data social_assistance;
	set social_assistance doubles_aggr;
run;

proc sort Data=social_assistance;
	by id year;
run;

* Merge folk_data and social_assistance;

data folk_data;
	merge folk_data social_assistance;
	by id year;
run;

* Keep only persons who are observed in the FOLK data;

data folk_data;
	set folk_data;
	where not missing(municipality);
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Compute equivalized family disposable income.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, calculate for each family the sum of disposable income;
* Then, calculate the equivalized family disposable income;
* Finally, compute the number of individuals in family receiving social assistance;
* and the sum of basic social assistance;

proc sort data=folk_data;
	by family_id year;
run;

data folk_data;
	set folk_data;
	where not missing(family_id);
run;

proc sql;
	create table family_sum as
	select year, family_id, sum(income_disp) as income_disp_family,
		sum(income_disp) / (1+sum(age<14)*0.3+0.5*(count(age)-1-sum(age<14))) as income_eq,
		count(age) as size,
		sum(jan) as jan,
		sum(feb) as feb,
		sum(mar) as mar,
		sum(apr) as apr,
		sum(may) as may,
		sum(june) as june,
		sum(july) as july,
		sum(aug) as aug,
		sum(sep) as sep,
		sum(oct) as oct,
		sum(nov) as nov,
		sum(dec) as dec,
		sum(basic_social_assistance) as basic_social_assistance
	from folk_data
	group by family_id, year;
quit;

data family_sum;
	set family_sum;
	drop income_disp_family size;
run;

* Take only unique rows;

proc sort data=family_sum NODUPRECS;
	by _all_;
run;


* Sort folk_data and family_sum, and merge the latter to the former;

proc sort Data=family_sum;
	by family_id year;
run;
	
data folk_data;
	set folk_data;
	drop income_disp family_size jan feb mar apr may june july aug sep oct nov dec
		basic_social_assistance;
run;

data folk_data;
	merge folk_data family_sum;
	by family_id year;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Use the 2022 municipal boundaries.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* There were 27 municipal mergers between 2012 and 2022. Here, we take this into account by aggregating the data based on the 2020 municipal map;
* Replace the municipality number of a merged municipality by the number of the new municipality;

data folk_data;
	set folk_data;
	where not missing(age);

	* The municipality mergers;

	if municipality='099' then municipality='214'; *2021;

	if municipality='911' then municipality='541'; *2020;

	if municipality='442' then municipality='051'; *2017;
	if municipality='174' then municipality='297';

	if municipality='164' then municipality='301'; *2016;
	if municipality='283' then municipality='098';
	if municipality='319' then municipality='783';
	if municipality='532' then municipality='398';

	if municipality='476' then municipality='297'; *2015;
	if municipality='413' then municipality='609';
	if municipality='838' then municipality='423';

	if municipality='863' then municipality='010'; *2013;
	if municipality='248' then municipality='260';
	if municipality='534' then municipality='297';
	if municipality='223' then municipality='444';
	if municipality='540' then municipality='444';
	if municipality='696' then municipality='491';
	if municipality='775' then municipality='491';
	if municipality='084' then municipality='564';
	if municipality='255' then municipality='564';
	if municipality='567' then municipality='564';
	if municipality='972' then municipality='564';
	if municipality='926' then municipality='678';
	if municipality='254' then municipality='790';
	if municipality='246' then municipality='740';
	if municipality='618' then municipality='740';
	if municipality='942' then municipality='905';

	drop family_id;

run;

proc sort data=folk_data;
	by id year;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Save years separately;

%MACRO save_data;
%DO i = 2011 %TO 2020;

proc export data=folk_data(where= (year=&i.))
	outfile= "W:\ASMA3\data\interim\folk_data_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
