********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: individual_recodes.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file takes the data compiled against the individual IDs in eligible
* couples and recodes for analysis. This takes inspiration from the CPF v1.5
* harmonization file (developed by Konrad Turek): https://cpfdata.com/download/
* as well as KM's other project using these data

********************************************************************************
* Get file
********************************************************************************
use "$temp/gsoep_couple_data_compiled.dta", clear
label language EN

// didnt do this above for all, should I just move this here for ease? okay, these .n / .s actually making my life harder AND for imputation, we need just . so I am updating here
// mvdecode _all, mv(-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc.
// mvdecode _all, mv(-8/-1) // I am an idiot - this is messing with relative duration, and that is actually the only variable that applies here anyway.
// recode * (.n=.)(.s=.) // this is stupid. no longer numeric, so can't use mvdecode. GAH this isn't working either because of those rogue string variables

unab allvars : *
unab excluded : py_*
local selected : list allvars - excluded
display `selected'

foreach var in `selected'{
	recode `var' (.n=.)(.s=.)
}

sort pid syear

********************************************************************************
*Basic demographic info
********************************************************************************
* Sex
// cpf uses from ppathl - sex_pl (this is the only one I have as well)
tab sex_pl, m // okay just 21 missing

* Age
gen age = syear-birthyr_pl if birthyr_pl!=.
tab age, m // 1.46% missing

browse pid syear birthyr_pl
unique pid
unique pid birthyr_pl // ever missing for one year, but not another? no..

* Education
// Years of education - cpf uses d11109 from pequiv, I renamed yrs_educ_cnef. there is also yrs_educ_pg
tab yrs_educ_cnef, m
tab yrs_educ_pg, m // see codebook for how to calculate: https://paneldata.org/soep-core/datasets/pgen/pgbilzeit
tab yrs_educ_pg yrs_educ_cnef, m // oaky they match, basically just pg has a touch more filled in than cnef
// but also - quite a bit missing. is this an age thing?
browse pid syear age yrs_educ* // okay...no...

// Categorized: edu4 (they also have edu3 and edu5, but this feels sufficient for our needs)
recode isced11_pg (0 1=1) (2=2) (3 4=3) (5/8=4) , gen(edu4a)
recode isced97_pg (0 1=1) (2=2) (3 4=3) (5 6=4) , gen(edu4b)
replace edu4b = 3 if syear <2010 & isced97_pg>=5 & casmin_pg==3 & yrs_educ_cnef<12 & yrs_educ_cnef>0
gen edu4=edu4a
replace edu4=edu4b if syear <2010
replace edu4=edu4b if syear >=2010 & (edu4<0|edu4==.)

	drop edu4a edu4b
		
	lab def edu4  1 "[0-1] Primary" 2 "[2] Secondary lower" ///
				  3 "[3-4] Secondary upper" 4 "[5-8] Tertiary" 
	lab val edu4 edu4
	lab var edu4 "Education: 4 levels"

tab edu4, m // this also has a lot of missing
tab age edu4, m
tab syear edu4, m row // coverage very bad in last years of survey - did they change the question or something? need to revisit education.
tab isced97_pg  edu4, m
tab isced11_pg  edu4, m
tab last_degree_pg  edu4, m
tab status_pl   edu4, m row // OH i am dumb - is this just as simple as being in sample? LOL YES...SO - can we fill in between years if same on both sides of education? I think yes -- but possibly do this while wide like normal (believe there is code for that in PSID and / or UKHLS)

browse pid syear status_pl yrs_educ_pg edu4 last_degree_pg  isced97_pg isced11_pg

*** edu3 // I am pulling this in because (with v40) updates to cpf, they use 3 category for maternal / paternal education
/*Note:
- differences in coding between isced97 11 and casmin
- d11109 also gives different results 
*/


recode isced11_pg (0/2=1) (3 4=2) (5/8=3) , gen(edu3a)
recode isced97_pg (0/2=1) (3 4=2) (5/6=3) , gen(edu3b)
replace edu3b = 2 if syear <2010 & isced97_pg>=5 & casmin_pg==3 & yrs_educ_cnef<12 & yrs_educ_cnef>0
gen edu3=edu3a
replace edu3=edu3b if syear <2010
replace edu3=edu3b if syear >=2010 & (edu3<0|edu3==.)

	drop edu3a edu3b
	
	lab def edu3  1 "[0-2] Low" 2 "[3-4] Medium" 3 "[5-8] High" // 2 incl Vocational
	lab val edu3 edu3
	lab var edu3 "Education: 3 levels"

	/* my old code from gendering

	fre last_degree_pg // school leaving degree. most comprehensive
	fre college_type_pg  // type of college degree (doesn't have clear relationship to above, but is 80% missing bc inapplicable)
	// mostly if last degree is upper secondary or isced is higher education
	fre isced11_pg // 55% missing
	fre isced97_pg // <2% missing
	tab last_degree_pg isced97_pg, m
	tab isced97_pg college_type_pg, m // okay, yes, if isced is higher ed, then this is filled in

	browse pid age last_degree_pg college_type_pg isced97_pg isced11_pg // will use isced as education for now, but not really sure what to do about education cross-nationally...

	*/

* Where born information (in / out of Germany. If in Germany, where)
// why aren't these labels in English? The _EN is also not helping...
label values psample_pl psample
label values where_born_state_pl birthregion
label values where_born_ew_pl birthregion_ew
label values where_1989_ew_pl loc1989
label values where_germany_pl sampreg
label values country_born_pl corigin
label values nationality_pb pnat_h
label values nationality_pg pgnation

// Born in Germany Y/N
gen born_in_germany = .
replace born_in_germany = 0 if born_germany_pl == 2
replace born_in_germany = 1 if born_germany_pl == 1

// Where Born in Germany (East-West)
tab born_in_germany where_born_ew_pl, m
tab lastyr_survey_pl where_born_ew_pl, m row // so this makes sense for the missing if exited prior to 2012 - no responses, not sure why so many missing after that?
tab lastyr_survey_pl where_born_ew_pl if born_in_germany==1, m row // oh is because not BORN in Germany? okay, that helps, but still not completely
tab born_in_germany where_born_ew_pl if lastyr_survey_pl>=2012, m
tab syear where_born_ew_pl if lastyr_survey_pl>=2012 & born_in_germany==1, m row
tab birthyr_pl where_born_ew_pl if lastyr_survey_pl>=2012 & born_in_germany==1, m row
tab psample_pl where_born_ew_pl, m row // possibly sample membership can help? is there where born?! or currently live ... like is everyone in group c (East Germany) BORN in East Germany? Or lives there now...
// https://companion.soep.de/Target%20Population%20and%20Samples/The%20SOEP%20Samples%20in%20Detail.html
// "German Residents of the German Democratic Republic (GDR)" consists of individuals in private households in which the household head was a citizen of the German Democratic Republic (GDR)." - Does CITIZEN imply born there then??
// Respondents born after 1989 (GEBJAHR in PPATH) were coded as "(-2) does not apply" on LOC1989
tab psample_pl where_born_ew_pl, row // okay like 93% of those in A were born in West and C were born in East. So - not perfect, but close?

browse pid syear psample_pl firstyr_survey_pl lastyr_survey_pl birthyr_pl where_born_state_pl where_born_ew_pl where_1989_ew_pl
browse pid syear psample_pl firstyr_survey_pl lastyr_survey_pl birthyr_pl where_born_state_pl where_born_ew_pl where_1989_ew_pl if birthyr_pl >=1988
tab where_born_ew_pl where_1989_ew_pl if birthyr_pl >=1985 & birthyr_pl<=1989, row

gen where_born_ew = .
replace where_born_ew = 0 if born_in_germany==0
replace where_born_ew = 1 if where_born_ew_pl == 21 // west
replace where_born_ew = 1 if where_born_ew==. & inrange(where_born_state_pl,1,10) //  a few have state but not east / west
replace where_born_ew = 2 if where_born_ew_pl == 22 // east
replace where_born_ew = 2 if where_born_ew==. & inrange(where_born_state_pl,11,16) //  a few have state but not east / west
replace where_born_ew = 1 if where_born_ew==. & where_1989_ew_pl == 2 & birthyr_pl >=1985 & birthyr_pl<=1989 // let's give a window of 5 years from birth to this question and consider this where born
replace where_born_ew = 2 if where_born_ew==. & where_1989_ew_pl == 1 & birthyr_pl >=1985 & birthyr_pl<=1989 // let's give a window of 5 years from birth to this question and consider this where born

label define where_born_ew 0 "Abroad" 1 "West" 2 "East"
label values where_born_ew where_born_ew

tab where_born_ew, m // okay still 34% missing
tab psample_pl where_born_ew, row // it would add quite a bit if we considered those in sample a / c as born in west / east. let's just leave this for now...

browse pid syear psample_pl firstyr_survey_pl lastyr_survey_pl birthyr_pl where_born_ew where_born_state_pl where_born_ew_pl where_1989_ew_pl

// Where Born in Germany (Federal State)
tab where_born_state_pl born_in_germany, m

gen where_born_state = .
replace where_born_state = 0 if born_in_germany==0
replace where_born_state = where_born_state_pl if born_in_germany==1 & inrange(where_born_state_pl,1,16) // not missing
label values where_born_state birthregion

// Country born
label define cob ///
0 "Germany (Survey Country)" ///
1 "Oceania and Antarctica" ///
2 "North-West Europe" ///
3 "Southern and Eastern Europe" ///
4 "North Africa and the Middle East" ///
5 "South-East Asia" ///
6 "North-East Asia" ///
7 "Southern and Central Asia" ///
8 "Americas" ///
9 "Sub-Saharan Africa" ///
10 "Other"

gen global_region_born=. // okay the CPF file is not correct based on the coding of this variable? ah yes: corigin: The variable has been recoded acccording to ISO-3 country codes in v38.
replace global_region_born = 1 if country_born_pl == 36 // Australia
replace global_region_born = 1 if country_born_pl == 554 // New Zealand
replace global_region_born = 1 if country_born_pl == 882 // Samoa
replace global_region_born = 1 if country_born_pl == 583 // Micronesia (Federated States of)
replace global_region_born = 1 if country_born_pl == 242 // Fiji
replace global_region_born = 1 if country_born_pl == 16 // American Samoa
replace global_region_born = 1 if country_born_pl == 90 // Solomon Islands
replace global_region_born = 1 if country_born_pl == 334 // Heard Island and McDonald Islands
replace global_region_born = 1 if country_born_pl == 776 // Tonga
replace global_region_born = 2 if country_born_pl == 826 // United Kingdom
replace global_region_born = 0 if country_born_pl == 276 // Germany
replace global_region_born = 2 if country_born_pl == 40 // Austria
replace global_region_born = 2 if country_born_pl == 250 // France
replace global_region_born = 2 if country_born_pl == 208 // Denmark
replace global_region_born = 2 if country_born_pl == 752 // Sweden
replace global_region_born = 2 if country_born_pl == 578 // Norway
replace global_region_born = 2 if country_born_pl == 246 // Finland
replace global_region_born = 2 if country_born_pl == 756 // Switzerland
replace global_region_born = 2 if country_born_pl == 352 // Iceland
replace global_region_born = 2 if country_born_pl == 372 // Ireland, Republic of
replace global_region_born = 2 if country_born_pl == 438 // Liechtenstein
replace global_region_born = 2 if country_born_pl == 492 // Monaco
replace global_region_born = 2 if country_born_pl == 831 // Guernsey
replace global_region_born = 3 if country_born_pl == 300 // Greece
replace global_region_born = 3 if country_born_pl == 380 // Italy
replace global_region_born = 3 if country_born_pl == 724 // Spain
replace global_region_born = 3 if country_born_pl == 642 // Romania
replace global_region_born = 3 if country_born_pl == 616 // Poland
replace global_region_born = 3 if country_born_pl == 348 // Hungary
replace global_region_born = 3 if country_born_pl == 620 // Portugal
replace global_region_born = 3 if country_born_pl == 100 // Bulgaria
replace global_region_born = 3 if country_born_pl == 203 // Czech Republic
replace global_region_born = 3 if country_born_pl == 233 // Estonia
replace global_region_born = 3 if country_born_pl == 440 // Lithuania
replace global_region_born = 3 if country_born_pl == 499 // Montenegro
replace global_region_born = 3 if country_born_pl == 643 // Russian Federation
replace global_region_born = 3 if country_born_pl == 56 // Belgium
replace global_region_born = 3 if country_born_pl == 528 // The Netherlands
replace global_region_born = 3 if country_born_pl == 442 // Luxembourg
replace global_region_born = 3 if country_born_pl == 498 // Moldova
replace global_region_born = 3 if country_born_pl == 8 // Albania
replace global_region_born = 3 if country_born_pl == 804 // Ukraine
replace global_region_born = 3 if country_born_pl == 499 // Montenegro
replace global_region_born = 3 if country_born_pl == 688 // Serbia
replace global_region_born = 3 if country_born_pl == 428 // Latvia
replace global_region_born = 3 if country_born_pl == 70 // Bosnia and Herzegovina
replace global_region_born = 3 if country_born_pl == 705 // Slovenia
replace global_region_born = 3 if country_born_pl == 703 // Slovakia
replace global_region_born = 3 if country_born_pl == 807 // North Macedonia
replace global_region_born = 3 if country_born_pl == 191 // Croatia
replace global_region_born = 3 if country_born_pl == 470 // Malta
replace global_region_born = 3 if country_born_pl == 196 // Cyprus
replace global_region_born = 3 if country_born_pl == 616 // Free City of Danzig (now Poland)
replace global_region_born = 3 if country_born_pl == 900 // Kosovo
replace global_region_born = 3 if country_born_pl == 112 // Belarus
replace global_region_born = 3 if country_born_pl == 498 // Bessarabia (Moldova)
replace global_region_born = 4 if country_born_pl == 792 // Turkey
replace global_region_born = 4 if country_born_pl == 760 // Syria
replace global_region_born = 4 if country_born_pl == 818 // Egypt
replace global_region_born = 4 if country_born_pl == 434 // Libya
replace global_region_born = 4 if country_born_pl == 504 // Morocco
replace global_region_born = 4 if country_born_pl == 729 // Sudan
replace global_region_born = 4 if country_born_pl == 788 // Tunisia
replace global_region_born = 4 if country_born_pl == 48 // Bahrain
replace global_region_born = 4 if country_born_pl == 364 // Iran
replace global_region_born = 4 if country_born_pl == 368 // Iraq
replace global_region_born = 4 if country_born_pl == 376 // Israel
replace global_region_born = 4 if country_born_pl == 400 // Jordan
replace global_region_born = 4 if country_born_pl == 414 // Kuwait
replace global_region_born = 4 if country_born_pl == 422 // Lebanon
replace global_region_born = 4 if country_born_pl == 512 // Oman
replace global_region_born = 4 if country_born_pl == 682 // Saudi Arabia
replace global_region_born = 4 if country_born_pl == 784 // United Arab Emirates
replace global_region_born = 4 if country_born_pl == 887 // Yemen
replace global_region_born = 4 if country_born_pl == 12 // Algeria
replace global_region_born = 4 if country_born_pl == 634 // Qatar
replace global_region_born = 4 if country_born_pl == 275 // Palestina
replace global_region_born = 5 if country_born_pl == 104 // Myanmar
replace global_region_born = 5 if country_born_pl == 116 // Cambodia
replace global_region_born = 5 if country_born_pl == 418 // Laos
replace global_region_born = 5 if country_born_pl == 764 // Thailand
replace global_region_born = 5 if country_born_pl == 704 // Vietnam
replace global_region_born = 5 if country_born_pl == 360 // Indonesia
replace global_region_born = 5 if country_born_pl == 458 // Malaysia
replace global_region_born = 5 if country_born_pl == 608 // Philippines
replace global_region_born = 5 if country_born_pl == 702 // Singapore
replace global_region_born = 5 if country_born_pl == 626 // Timor-Leste
replace global_region_born = 5 if country_born_pl == 458 // Malaysia
replace global_region_born = 6 if country_born_pl == 408 // Korea, Democratic People's Republ
replace global_region_born = 6 if country_born_pl == 410 // Korea, Republic of
replace global_region_born = 6 if country_born_pl == 392 // Japan
replace global_region_born = 6 if country_born_pl == 156 // China
replace global_region_born = 6 if country_born_pl == 344 // Hong Kong
replace global_region_born = 6 if country_born_pl == 496 // Mongolia
replace global_region_born = 6 if country_born_pl == 158 // Taiwan
replace global_region_born = 6 if country_born_pl == 446 // Macau
replace global_region_born = 7 if country_born_pl == 50 // Bangladesh
replace global_region_born = 7 if country_born_pl == 398 // Kazakhstan
replace global_region_born = 7 if country_born_pl == 762 // Tajikistan
replace global_region_born = 7 if country_born_pl == 586 // Pakistan
replace global_region_born = 7 if country_born_pl == 860 // Uzbekistan
replace global_region_born = 7 if country_born_pl == 4 // Afghanistan
replace global_region_born = 7 if country_born_pl == 524 // Nepal
replace global_region_born = 7 if country_born_pl == 144 // Sri Lanka
replace global_region_born = 7 if country_born_pl == 462 // Maldives
replace global_region_born = 7 if country_born_pl == 795 // Turkmenistan
replace global_region_born = 7 if country_born_pl == 51 // Armenia
replace global_region_born = 7 if country_born_pl == 268 // Georgia
replace global_region_born = 7 if country_born_pl == 417 // Kyrgyzstan
replace global_region_born = 7 if country_born_pl == 356 // India
replace global_region_born = 7 if country_born_pl == 64 // Bhutan
replace global_region_born = 7 if country_born_pl == 31 // Azerbaijan
replace global_region_born = 7 if country_born_pl == 86 // British Indian Ocean Territory
replace global_region_born = 8 if country_born_pl == 840 // United States of America
replace global_region_born = 8 if country_born_pl == 124 // Canada
replace global_region_born = 8 if country_born_pl == 32 // Argentina
replace global_region_born = 8 if country_born_pl == 170 // Colombia
replace global_region_born = 8 if country_born_pl == 76 // Brazil
replace global_region_born = 8 if country_born_pl == 604 // Peru
replace global_region_born = 8 if country_born_pl == 218 // Ecuador
replace global_region_born = 8 if country_born_pl == 630 // Puerto Rico
replace global_region_born = 8 if country_born_pl == 780 // Trinidad and Tobago
replace global_region_born = 8 if country_born_pl == 600 // Paraguay
replace global_region_born = 8 if country_born_pl == 858 // Uruguay
replace global_region_born = 8 if country_born_pl == 591 // Panama
replace global_region_born = 8 if country_born_pl == 320 // Guatemala
replace global_region_born = 8 if country_born_pl == 340 // Honduras
replace global_region_born = 8 if country_born_pl == 222 // El Salvador
replace global_region_born = 8 if country_born_pl == 188 // Costa Rica
replace global_region_born = 8 if country_born_pl == 558 // Nicaragua
replace global_region_born = 8 if country_born_pl == 44 // Bahamas
replace global_region_born = 8 if country_born_pl == 68 // Bolivia
replace global_region_born = 8 if country_born_pl == 484 // Mexico
replace global_region_born = 8 if country_born_pl == 192 // Cuba
replace global_region_born = 8 if country_born_pl == 662 // Saint Lucia
replace global_region_born = 8 if country_born_pl == 740 // Suriname
replace global_region_born = 8 if country_born_pl == 84 // Belize
replace global_region_born = 8 if country_born_pl == 328 // Guyana
replace global_region_born = 8 if country_born_pl == 214 // Dominican Republic (the)
replace global_region_born = 8 if country_born_pl == 388 // Jamaica
replace global_region_born = 8 if country_born_pl == 862 // Venezuela
replace global_region_born = 8 if country_born_pl == 152 // Chile
replace global_region_born = 8 if country_born_pl == 308 // Grenada
replace global_region_born = 8 if country_born_pl == 332 // Haiti
replace global_region_born = 8 if country_born_pl == 52 // Barbados
replace global_region_born = 8 if country_born_pl == 92 // British Virgin Islands
replace global_region_born = 8 if country_born_pl == 533 // Aruba
replace global_region_born = 8 if country_born_pl == 581 // United States Minor Outlying Islands
replace global_region_born = 8 if country_born_pl == 659 // St Kitts and Nevis
replace global_region_born = 9 if country_born_pl == 24 // Angola
replace global_region_born = 9 if country_born_pl == 404 // Kenya
replace global_region_born = 9 if country_born_pl == 72 // Botswana
replace global_region_born = 9 if country_born_pl == 231 // Ethiopia
replace global_region_born = 9 if country_born_pl == 232 // Eritrea
replace global_region_born = 9 if country_born_pl == 706 // Somalia
replace global_region_born = 9 if country_born_pl == 710 // South Africa
replace global_region_born = 9 if country_born_pl == 204 // Benin
replace global_region_born = 9 if country_born_pl == 288 // Ghana
replace global_region_born = 9 if country_born_pl == 480 // Mauritius
replace global_region_born = 9 if country_born_pl == 566 // Nigeria
replace global_region_born = 9 if country_born_pl == 834 // Tanzania
replace global_region_born = 9 if country_born_pl == 324 // Guinea
replace global_region_born = 9 if country_born_pl == 516 // Namibia
replace global_region_born = 9 if country_born_pl == 854 // Burkina Faso
replace global_region_born = 9 if country_born_pl == 508 // Mozambique
replace global_region_born = 9 if country_born_pl == 694 // Sierra Leone
replace global_region_born = 9 if country_born_pl == 132 // Cabo Verde
replace global_region_born = 9 if country_born_pl == 800 // Uganda
replace global_region_born = 9 if country_born_pl == 450 // Madagascar
replace global_region_born = 9 if country_born_pl == 716 // Zimbabwe
replace global_region_born = 9 if country_born_pl == 270 // The Gambia
replace global_region_born = 9 if country_born_pl == 686 // Senegal
replace global_region_born = 9 if country_born_pl == 466 // Mali
replace global_region_born = 9 if country_born_pl == 120 // Cameroon
replace global_region_born = 9 if country_born_pl == 430 // Liberia
replace global_region_born = 9 if country_born_pl == 894 // Zambia
replace global_region_born = 9 if country_born_pl == 178 // Congo
replace global_region_born = 9 if country_born_pl == 768 // Togo
replace global_region_born = 9 if country_born_pl == 426 // Lesotho
replace global_region_born = 9 if country_born_pl == 646 // Rwanda
replace global_region_born = 9 if country_born_pl == 562 // Niger
replace global_region_born = 9 if country_born_pl == 262 // Djibouti
replace global_region_born = 9 if country_born_pl == 454 // Malawi
replace global_region_born = 9 if country_born_pl == 384 // Côte d'Ivoire
replace global_region_born = 9 if country_born_pl == 148 // Chad
replace global_region_born = 9 if country_born_pl == 690 // Seychelles
replace global_region_born = 9 if country_born_pl == 108 // Burundi
replace global_region_born = 9 if country_born_pl == 140 // Central African Republic
replace global_region_born = 9 if country_born_pl == 174 // Comoros
replace global_region_born = 9 if country_born_pl == 180 // Congo, Democratic Republic
replace global_region_born = 9 if country_born_pl == 226 // Equatorial Guinea
replace global_region_born = 9 if country_born_pl == 266 // Gabon
replace global_region_born = 9 if country_born_pl == 478 // Mauritania
replace global_region_born = 9 if country_born_pl == 624 // Guinea-Bissau
replace global_region_born = 9 if country_born_pl == 728 // South Sudan

lab val global_region_born cob
tab global_region_born, m

inspect country_born_pl global_region_born // okay both have same amount of missing (v small)
tab country_born_pl global_region_born, m

// Note: The CPF has code if the country born varies across years. They use the modal response. Not pasting in for now, let's explore the data first (see rows 886-ish of cpf file)
quietly unique global_region_born if global_region_born!=., by(pid) gen(country_change)
bysort pid (country_change): replace country_change=country_change[1]
tab country_change, m // okay, no one changes their country of origin - there are just 0s for the missing
unique pid
unique pid global_region_born // this validates that

// what is the difference between nationality and where born?
browse pid syear status_pl country_born_pl nationality_pg nationality_pb // pb is more comprehensive, probably bc filled in for off years. also codes match between country born and pb, but NOT country born and pg
	// okay they are largely congruent, but not always
	// also seems like nationality is time varying

gen nationality_region=.
replace nationality_region = 1 if nationality_pb == 36 // Australia
replace nationality_region = 1 if nationality_pb == 554 // New Zealand
replace nationality_region = 1 if nationality_pb == 882 // Samoa
replace nationality_region = 1 if nationality_pb == 583 // Micronesia (Federated States of)
replace nationality_region = 1 if nationality_pb == 242 // Fiji
replace nationality_region = 1 if nationality_pb == 16 // American Samoa
replace nationality_region = 1 if nationality_pb == 90 // Solomon Islands
replace nationality_region = 1 if nationality_pb == 334 // Heard Island and McDonald Islands
replace nationality_region = 1 if nationality_pb == 776 // Tonga
replace nationality_region = 2 if nationality_pb == 826 // United Kingdom
replace nationality_region = 0 if nationality_pb == 276 // Germany
replace nationality_region = 2 if nationality_pb == 40 // Austria
replace nationality_region = 2 if nationality_pb == 250 // France
replace nationality_region = 2 if nationality_pb == 208 // Denmark
replace nationality_region = 2 if nationality_pb == 752 // Sweden
replace nationality_region = 2 if nationality_pb == 578 // Norway
replace nationality_region = 2 if nationality_pb == 246 // Finland
replace nationality_region = 2 if nationality_pb == 756 // Switzerland
replace nationality_region = 2 if nationality_pb == 352 // Iceland
replace nationality_region = 2 if nationality_pb == 372 // Ireland, Republic of
replace nationality_region = 2 if nationality_pb == 438 // Liechtenstein
replace nationality_region = 2 if nationality_pb == 492 // Monaco
replace nationality_region = 2 if nationality_pb == 831 // Guernsey
replace nationality_region = 3 if nationality_pb == 300 // Greece
replace nationality_region = 3 if nationality_pb == 380 // Italy
replace nationality_region = 3 if nationality_pb == 724 // Spain
replace nationality_region = 3 if nationality_pb == 642 // Romania
replace nationality_region = 3 if nationality_pb == 616 // Poland
replace nationality_region = 3 if nationality_pb == 348 // Hungary
replace nationality_region = 3 if nationality_pb == 620 // Portugal
replace nationality_region = 3 if nationality_pb == 100 // Bulgaria
replace nationality_region = 3 if nationality_pb == 203 // Czech Republic
replace nationality_region = 3 if nationality_pb == 233 // Estonia
replace nationality_region = 3 if nationality_pb == 440 // Lithuania
replace nationality_region = 3 if nationality_pb == 499 // Montenegro
replace nationality_region = 3 if nationality_pb == 643 // Russian Federation
replace nationality_region = 3 if nationality_pb == 56 // Belgium
replace nationality_region = 3 if nationality_pb == 528 // The Netherlands
replace nationality_region = 3 if nationality_pb == 442 // Luxembourg
replace nationality_region = 3 if nationality_pb == 498 // Moldova
replace nationality_region = 3 if nationality_pb == 8 // Albania
replace nationality_region = 3 if nationality_pb == 804 // Ukraine
replace nationality_region = 3 if nationality_pb == 499 // Montenegro
replace nationality_region = 3 if nationality_pb == 688 // Serbia
replace nationality_region = 3 if nationality_pb == 428 // Latvia
replace nationality_region = 3 if nationality_pb == 70 // Bosnia and Herzegovina
replace nationality_region = 3 if nationality_pb == 705 // Slovenia
replace nationality_region = 3 if nationality_pb == 703 // Slovakia
replace nationality_region = 3 if nationality_pb == 807 // North Macedonia
replace nationality_region = 3 if nationality_pb == 191 // Croatia
replace nationality_region = 3 if nationality_pb == 470 // Malta
replace nationality_region = 3 if nationality_pb == 196 // Cyprus
replace nationality_region = 3 if nationality_pb == 616 // Free City of Danzig (now Poland)
replace nationality_region = 3 if nationality_pb == 900 // Kosovo
replace nationality_region = 3 if nationality_pb == 112 // Belarus
replace nationality_region = 3 if nationality_pb == 498 // Bessarabia (Moldova)
replace nationality_region = 4 if nationality_pb == 792 // Turkey
replace nationality_region = 4 if nationality_pb == 760 // Syria
replace nationality_region = 4 if nationality_pb == 818 // Egypt
replace nationality_region = 4 if nationality_pb == 434 // Libya
replace nationality_region = 4 if nationality_pb == 504 // Morocco
replace nationality_region = 4 if nationality_pb == 729 // Sudan
replace nationality_region = 4 if nationality_pb == 788 // Tunisia
replace nationality_region = 4 if nationality_pb == 48 // Bahrain
replace nationality_region = 4 if nationality_pb == 364 // Iran
replace nationality_region = 4 if nationality_pb == 368 // Iraq
replace nationality_region = 4 if nationality_pb == 376 // Israel
replace nationality_region = 4 if nationality_pb == 400 // Jordan
replace nationality_region = 4 if nationality_pb == 414 // Kuwait
replace nationality_region = 4 if nationality_pb == 422 // Lebanon
replace nationality_region = 4 if nationality_pb == 512 // Oman
replace nationality_region = 4 if nationality_pb == 682 // Saudi Arabia
replace nationality_region = 4 if nationality_pb == 784 // United Arab Emirates
replace nationality_region = 4 if nationality_pb == 887 // Yemen
replace nationality_region = 4 if nationality_pb == 12 // Algeria
replace nationality_region = 4 if nationality_pb == 634 // Qatar
replace nationality_region = 4 if nationality_pb == 275 // Palestina
replace nationality_region = 5 if nationality_pb == 104 // Myanmar
replace nationality_region = 5 if nationality_pb == 116 // Cambodia
replace nationality_region = 5 if nationality_pb == 418 // Laos
replace nationality_region = 5 if nationality_pb == 764 // Thailand
replace nationality_region = 5 if nationality_pb == 704 // Vietnam
replace nationality_region = 5 if nationality_pb == 360 // Indonesia
replace nationality_region = 5 if nationality_pb == 458 // Malaysia
replace nationality_region = 5 if nationality_pb == 608 // Philippines
replace nationality_region = 5 if nationality_pb == 702 // Singapore
replace nationality_region = 5 if nationality_pb == 626 // Timor-Leste
replace nationality_region = 5 if nationality_pb == 458 // Malaysia
replace nationality_region = 6 if nationality_pb == 408 // Korea, Democratic People's Republ
replace nationality_region = 6 if nationality_pb == 410 // Korea, Republic of
replace nationality_region = 6 if nationality_pb == 392 // Japan
replace nationality_region = 6 if nationality_pb == 156 // China
replace nationality_region = 6 if nationality_pb == 344 // Hong Kong
replace nationality_region = 6 if nationality_pb == 496 // Mongolia
replace nationality_region = 6 if nationality_pb == 158 // Taiwan
replace nationality_region = 6 if nationality_pb == 446 // Macau
replace nationality_region = 7 if nationality_pb == 50 // Bangladesh
replace nationality_region = 7 if nationality_pb == 398 // Kazakhstan
replace nationality_region = 7 if nationality_pb == 762 // Tajikistan
replace nationality_region = 7 if nationality_pb == 586 // Pakistan
replace nationality_region = 7 if nationality_pb == 860 // Uzbekistan
replace nationality_region = 7 if nationality_pb == 4 // Afghanistan
replace nationality_region = 7 if nationality_pb == 524 // Nepal
replace nationality_region = 7 if nationality_pb == 144 // Sri Lanka
replace nationality_region = 7 if nationality_pb == 462 // Maldives
replace nationality_region = 7 if nationality_pb == 795 // Turkmenistan
replace nationality_region = 7 if nationality_pb == 51 // Armenia
replace nationality_region = 7 if nationality_pb == 268 // Georgia
replace nationality_region = 7 if nationality_pb == 417 // Kyrgyzstan
replace nationality_region = 7 if nationality_pb == 356 // India
replace nationality_region = 7 if nationality_pb == 64 // Bhutan
replace nationality_region = 7 if nationality_pb == 31 // Azerbaijan
replace nationality_region = 7 if nationality_pb == 86 // British Indian Ocean Territory
replace nationality_region = 8 if nationality_pb == 840 // United States of America
replace nationality_region = 8 if nationality_pb == 124 // Canada
replace nationality_region = 8 if nationality_pb == 32 // Argentina
replace nationality_region = 8 if nationality_pb == 170 // Colombia
replace nationality_region = 8 if nationality_pb == 76 // Brazil
replace nationality_region = 8 if nationality_pb == 604 // Peru
replace nationality_region = 8 if nationality_pb == 218 // Ecuador
replace nationality_region = 8 if nationality_pb == 630 // Puerto Rico
replace nationality_region = 8 if nationality_pb == 780 // Trinidad and Tobago
replace nationality_region = 8 if nationality_pb == 600 // Paraguay
replace nationality_region = 8 if nationality_pb == 858 // Uruguay
replace nationality_region = 8 if nationality_pb == 591 // Panama
replace nationality_region = 8 if nationality_pb == 320 // Guatemala
replace nationality_region = 8 if nationality_pb == 340 // Honduras
replace nationality_region = 8 if nationality_pb == 222 // El Salvador
replace nationality_region = 8 if nationality_pb == 188 // Costa Rica
replace nationality_region = 8 if nationality_pb == 558 // Nicaragua
replace nationality_region = 8 if nationality_pb == 44 // Bahamas
replace nationality_region = 8 if nationality_pb == 68 // Bolivia
replace nationality_region = 8 if nationality_pb == 484 // Mexico
replace nationality_region = 8 if nationality_pb == 192 // Cuba
replace nationality_region = 8 if nationality_pb == 662 // Saint Lucia
replace nationality_region = 8 if nationality_pb == 740 // Suriname
replace nationality_region = 8 if nationality_pb == 84 // Belize
replace nationality_region = 8 if nationality_pb == 328 // Guyana
replace nationality_region = 8 if nationality_pb == 214 // Dominican Republic (the)
replace nationality_region = 8 if nationality_pb == 388 // Jamaica
replace nationality_region = 8 if nationality_pb == 862 // Venezuela
replace nationality_region = 8 if nationality_pb == 152 // Chile
replace nationality_region = 8 if nationality_pb == 308 // Grenada
replace nationality_region = 8 if nationality_pb == 332 // Haiti
replace nationality_region = 8 if nationality_pb == 52 // Barbados
replace nationality_region = 8 if nationality_pb == 92 // British Virgin Islands
replace nationality_region = 8 if nationality_pb == 533 // Aruba
replace nationality_region = 8 if nationality_pb == 581 // United States Minor Outlying Islands
replace nationality_region = 8 if nationality_pb == 659 // St Kitts and Nevis
replace nationality_region = 9 if nationality_pb == 24 // Angola
replace nationality_region = 9 if nationality_pb == 404 // Kenya
replace nationality_region = 9 if nationality_pb == 72 // Botswana
replace nationality_region = 9 if nationality_pb == 231 // Ethiopia
replace nationality_region = 9 if nationality_pb == 232 // Eritrea
replace nationality_region = 9 if nationality_pb == 706 // Somalia
replace nationality_region = 9 if nationality_pb == 710 // South Africa
replace nationality_region = 9 if nationality_pb == 204 // Benin
replace nationality_region = 9 if nationality_pb == 288 // Ghana
replace nationality_region = 9 if nationality_pb == 480 // Mauritius
replace nationality_region = 9 if nationality_pb == 566 // Nigeria
replace nationality_region = 9 if nationality_pb == 834 // Tanzania
replace nationality_region = 9 if nationality_pb == 324 // Guinea
replace nationality_region = 9 if nationality_pb == 516 // Namibia
replace nationality_region = 9 if nationality_pb == 854 // Burkina Faso
replace nationality_region = 9 if nationality_pb == 508 // Mozambique
replace nationality_region = 9 if nationality_pb == 694 // Sierra Leone
replace nationality_region = 9 if nationality_pb == 132 // Cabo Verde
replace nationality_region = 9 if nationality_pb == 800 // Uganda
replace nationality_region = 9 if nationality_pb == 450 // Madagascar
replace nationality_region = 9 if nationality_pb == 716 // Zimbabwe
replace nationality_region = 9 if nationality_pb == 270 // The Gambia
replace nationality_region = 9 if nationality_pb == 686 // Senegal
replace nationality_region = 9 if nationality_pb == 466 // Mali
replace nationality_region = 9 if nationality_pb == 120 // Cameroon
replace nationality_region = 9 if nationality_pb == 430 // Liberia
replace nationality_region = 9 if nationality_pb == 894 // Zambia
replace nationality_region = 9 if nationality_pb == 178 // Congo
replace nationality_region = 9 if nationality_pb == 768 // Togo
replace nationality_region = 9 if nationality_pb == 426 // Lesotho
replace nationality_region = 9 if nationality_pb == 646 // Rwanda
replace nationality_region = 9 if nationality_pb == 562 // Niger
replace nationality_region = 9 if nationality_pb == 262 // Djibouti
replace nationality_region = 9 if nationality_pb == 454 // Malawi
replace nationality_region = 9 if nationality_pb == 384 // Côte d'Ivoire
replace nationality_region = 9 if nationality_pb == 148 // Chad
replace nationality_region = 9 if nationality_pb == 690 // Seychelles
replace nationality_region = 9 if nationality_pb == 108 // Burundi
replace nationality_region = 9 if nationality_pb == 140 // Central African Republic
replace nationality_region = 9 if nationality_pb == 174 // Comoros
replace nationality_region = 9 if nationality_pb == 180 // Congo, Democratic Republic
replace nationality_region = 9 if nationality_pb == 226 // Equatorial Guinea
replace nationality_region = 9 if nationality_pb == 266 // Gabon
replace nationality_region = 9 if nationality_pb == 478 // Mauritania
replace nationality_region = 9 if nationality_pb == 624 // Guinea-Bissau
replace nationality_region = 9 if nationality_pb == 728 // South Sudan

label values nationality_region cob

tab nationality_region, m
tab global_region_born nationality_region, m // so, definitely different enough - most of the differences are between being classified as Germany in Nationality, but region born as somewhere else. nationality has more missing

quietly unique nationality_region if nationality_region!=., by(pid) gen(nat_change)
bysort pid (nat_change): replace nat_change=nat_change[1]
tab nat_change, m // there is some change over time (though, less than 2.5%) // I also think some people have missing some years but not others - let's possibly fill in while wide. also probably make so not time-varying?

* Current residence (where, type, etc.)
// East-West
tab where_germany_pl, m // very small amount of missing, so this is fine
tab where_germany_pl where_germany_hb, m 
tab where_germany_pl where_germany_hg, m // hg has lots of missing
tab syear where_germany_pl, m 

replace where_germany_pl = 2 if where_germany_pl==.n & where_germany_hb==22

// Federal State
tab federal_state_hb, m // most comprehensive - less than 0.5% missing
label values federal_state_hb bula_h 
tab federal_state_hg, m // lots of missing
tab federal_state_cnef, m // also has lots of missing
tab federal_state_hb federal_state_cnef, m

// Type of residence
tab region_type, m // very few missing

* Housing status
tab housing_status_hl, m
tab housing_status_hg, m // has like 1000 less missing. these codes also differ, but generally congruent

gen home_owner=.
replace home_owner=0 if inrange(housing_status_hg,2,5)
replace home_owner=1 if housing_status_hg==1

* Marital Status (this is all currently from marriage as gendering project)
// updating labels for some variables that are only in german
label define marst 1 "Married" 2 "Register same-sex" 3 "Single, never married"  4 "Divorced" 5 "Widowed"
label values marst marst

tab marst partnered_pl

gen marst_defacto=.
replace marst_defacto=1 if inlist(marst,1,2) // married
replace marst_defacto=1 if inrange(marst,3,5) & inlist(partnered_pl,1,3) // married
replace marst_defacto=2 if inrange(marst,3,5) & inlist(partnered_pl,2,4) // partnered
replace marst_defacto=3 if marst==3 & inlist(partnered_pl,0,5) // not partnered or unclear
replace marst_defacto=4 if marst==4 & inlist(partnered_pl,0,5)
replace marst_defacto=5 if marst==5 & inlist(partnered_pl,0,5)
replace marst_defacto=1 if inlist(partnered_pl,1,3) & marst_defacto==.
replace marst_defacto=2 if inlist(partnered_pl,2,4) & marst_defacto==.
replace marst_defacto=3 if inlist(partnered_pl,0,5) & marst_defacto==.
replace marst_defacto = 2 if marst_defacto==1 & inlist(partnered_pl,2,4) // this seems more accurate

label define marst_defacto 1 "Married" 2 "Partnered" 3 "Never Partnered"  4 "Divorced" 5 "Widowed"
label values marst_defacto marst_defacto

tab marst marst_defacto, m
tab partnered_pl marst_defacto, m

gen partnered_total=.
replace partnered_total = 0 if inlist(marst_defacto,3,4,5)
replace partnered_total = 1 if inlist(marst_defacto,1,2)
tab partnered_pl partnered_total, m // some people labeled as married, but don't have partner - maybe partner doesn't live in HH?
tab marst_defacto partnered_total, m

// make sure partner ids that came from the two different places (e.g. pg v. pl) match once I compile?
browse pid syear status_pl partnered_total partnered_pl partner_id_pl partner_id_pg

gen partner_id_check=.
replace partner_id_check=0 if partner_id_pg!=partner_id_pl & partner_id_pg!=.n & partner_id_pg!=.n & partner_id_pg!=.
replace partner_id_check=1 if partner_id_pg==partner_id_pl & partner_id_pg!=.n & partner_id_pg!=.n & partner_id_pg!=.

tab partner_id_check if partnered_total==1, m // okay, so when both non-missing, they match. BUT there are a bunch where one has an id and the other does not. I BELIEVE this is because of sample status (ppathl fills in when not in sample, pg does not)
tab partner_id_check if partnered_total==1 & status_pl==1, m // yes, when I restrict to in sample - less than 2% are incongruent
inspect partner_id_pl
inspect partner_id_pl if partnered_total==1 // & status_pl==1
inspect partner_id_pl if partnered_total==1 & status_pl==1
inspect partner_id_pg
inspect partner_id_pg if partnered_total==1 & status_pl==1 // 2389 just don't have a partner when partnered

* CPF version
label values marital_status_pg pgfamstd
	
gen parstat6=-3 if partnered_pl<.				
replace parstat6=6 if (marital_status_pg==2|marital_status_pg==6|marital_status_pg==8) & partnered_pl==0 // I have this as marital_status_pg (from pgen)
replace parstat6=5 if marital_status_pg==4 & partnered_pl==0 
replace parstat6=4 if marital_status_pg==5 & partnered_pl==0 
replace parstat6=3 if marital_status_pg==3 & partnered_pl==0
replace parstat6=2 if (marital_status_pg!=1 & marital_status_pg!=7) & (partnered_pl>=1 & partnered_pl<=4) 
replace parstat6=1 if (marital_status_pg==1|marital_status_pg==7)	// marital_status_pg is cleaned, so it has a priority over partner info 

		recode  marital_status_pg (1 7=1)(2 6 8=6)(3=3)(4=5)(5=4)	///
						 (-1=-2) (-3=-1) (-5 -8=-8), gen(temp_parstat6)
		replace parstat6=temp_parstat6 if parstat6==-3 & temp_parstat6>0 & temp_parstat6<.


	lab var parstat6 "Partnership living-status [6]"
	lab def parstat6				///
	1	"Married/registered, with P"	///
	2	"Cohabiting (Not married, Living with P)"				///
	3	"Single, No P" 				///
	4	"Widowed, No P" 				///
	5	"Divorced, No P" 			///
	6	"Separated, No P" 			///
	-1 "-1 MV general" -2 "-2 Item non-response" ///
	-3 "-3 Does not apply" -8 "-8 Question not asked in survey"
	lab val parstat6 parstat6
	
tab parstat6, m
tab parstat6 marst_defacto, m // okay a bunch of their cohabiting, I have as married...
tab parstat6 partnered_pl, m // and that is because of partnered_pl -- labeled as spouse. I am wondering if this is a problem of sample status
tab parstat6 marst_defacto if status_pl==1, m
tab status_pl if marst_defacto==1 & parstat6==2, m // yes, most of these are off years. I trust mine over theirs

sort pid syear
browse pid syear status_pl partnered_pl marst_defacto parstat6 marital_status_pg marst

// save "$created_data/gsoep_couple_data_recoded.dta", replace

* Family background
/* I think the coding / value labels have changed since cpf 1.5 to 2.0 - bc this made me skeptical of 1.5 and I went to website and see that 2.0 has been released, and these notes are new:

fsedu / msedu - Level Of Education Father / Mother 

[1] Ohne Schulabschluss	1.       No school diploma
[2] Hauptschulabs. 8. Kl.	2.       Lower secondary school diploma (after 8th grade)
[3] Realschulabs. 10 Kl.	3.       Intermediate secondary school diploma (after 10th grade)
[4] Abitur	4.       University entrance qualification (Abitur)
[5] Anderer Abschluss	5.       Other qualification
[6] Weiss nicht	6.       Don't know
[7] Fachhochschulreife	7.       University of Applied Sciences entrance qualification 
[8] Abschluss Pflichtschule (Ausland)	8.       Compulsory schooling diploma (abroad)
[9] Abschluss weiterführende Schule (Ausland)	9.       Upper secondary school diploma (abroad)


fprofedu / mprofedu - Vocational Training Father / Mother
[1] Gewerb.Lehre	1.	Trade apprenticeship
[2] Kaufm.Lehre	2.	Commercial apprenticeship
[3] Fachschule	3.	Vocational school 
[4] Beamtenausbildung	4.	Civil servant education
[5] Fachhochschule	5.	University of Applied Sciences 
[6] Universitaet	6.	University
[7] Sonstige Ausbildung	7.	Other education
[8] K.abgeschl.Ausbild.	8.	No completed education
[9] Weiss nicht	9.	Don't know
[10] Berufliche Ausbildung, Lehre	10.	Vocational education
[11] Hochschule (auch Ausl. und Ing. Schule Ost)	11.	Higher education (including foreign and East German engineering schools)
[12] Berufsfachschule Gesundheitswesen	12.	Vocational school in the healthcare sector
[13] Fachschule, Meister	13.	Technical school, master craftsman */

// mother education (they no longer have a 4 cat in cpf so using 3 cat for now)
label values mother_educ_bp msedu
label values mother_educ_bl lb0091_h
label values mother_training lb0111
label values mother_vocational_bp mprofedu

tab mother_educ_bp, m
tab mother_educ_bl, m // this has many missing - bc needs to be copied to most rows I think
tab mother_educ_bp mother_educ_bl // but when both not missing, generally congruent
tab mother_training, m // many missing
tab mother_vocational_bp, m
tab mother_vocational_bp mother_educ_bp, m

recode mother_educ_bp (1 2 8 =1) (3/5 7 9=2) (6=.) (.n .s = .),  gen(medu3) // (min/-1 6=-1)
recode mother_vocational_bp  (7 8 9=1)  (1/4 10 12 13=2) (5 6 11=3)(else=.),  gen(temp_medu3)

replace medu3=temp_medu3 if (medu3==. | medu3<0) & (temp_medu3>0 & temp_medu3<.) 
replace medu3=temp_medu3 if temp_medu3>medu3 & (temp_medu3>0 & temp_medu3<.) 

	lab val medu3 edu3
	lab var medu3 "Mother's education: 3 levels"

drop temp_medu*

tab mother_educ_bp medu3, m
tab mother_vocational_bp  medu3, m

// father education (also using 3 cat)
recode father_educ_bp (1 2 8 =1) (3/5 7 9=2) (6=.) (.n .s = .), gen(fedu3)
recode father_vocational_bp (7 8 9=1)  (1/4 10 12 13=2) (5 6 11=3)(else=.)	,  gen(temp_fedu3)

replace fedu3=temp_fedu3 if (fedu3==. | fedu3<0) & (temp_fedu3>0 & temp_fedu3<.) 
replace fedu3=temp_fedu3 if temp_fedu3>fedu3 & (temp_fedu3>0 & temp_fedu3<.) 

	lab val fedu3 edu3
	lab var fedu3 "Father's education: 3 levels"
	
drop temp_fedu*

tab fedu3, m
tab father_educ_bp fedu3, m
tab father_vocational_bp  medu3, m

*** Fill MV based on other waves // I actually don't think this adds anything...
	foreach p in f m  			{    
	bysort pid: egen `p'temp3=max(`p'edu3) 
	bysort pid (syear): replace `p'edu3=`p'temp3 if (`p'edu3<0 | `p'edu3==.)  
	drop `p'temp3
	}
	
rename fedu3 father_educ
rename medu3 mother_educ

// family structure
tab who_lived_with_bl, m // lb0060
label values who_lived_with_bl lb0060
tab num_yrs_bio_bp, m
tab num_yrs_bio_bl, m // lb0066
tab num_yrs_bio_bl who_lived_with_bl, m
tab num_yrs_bio_bp who_lived_with_bl, m

	// need to figure out if I can population biol info. Some people surveyed more than once though?
	bysort pid: egen bio_int_n = total(biol_int_year)
	tab bio_int_n, m
	
	quietly unique who_lived_with_bl if who_lived_with_bl!=., by(pid) gen(fam_struct_change)
	bysort pid (fam_struct_change): replace fam_struct_change=fam_struct_change[1]
	tab fam_struct_change, m
	
	quietly unique num_yrs_bio_bl if num_yrs_bio_bl!=., by(pid) gen(fam_years_change)
	bysort pid (fam_years_change): replace fam_years_change=fam_years_change[1]
	bysort pid: egen max_yrs_bio_bl = max(num_yrs_bio_bl)
	tab fam_years_change, m
	tab max_yrs_bio_bl, m
	tab num_yrs_bio_bl, m
	tab num_yrs_bio_bl max_yrs_bio_bl, m
	 
	bysort pid (who_lived_with_bl): replace who_lived_with_bl=who_lived_with_bl[1]
	bysort pid (num_yrs_bio_bl): replace num_yrs_bio_bl=num_yrs_bio_bl[1] if fam_years_change<=1
	
	gen yrs_bio_parent = num_yrs_bio_bl
	replace yrs_bio_parent = max_yrs_bio_bl if yrs_bio_parent==. & max_yrs_bio_bl!=.
	
	tab yrs_bio_parent num_yrs_bio_bp, m
	replace yrs_bio_parent = num_yrs_bio_bp if yrs_bio_parent==. & num_yrs_bio_bp!=.
	
sort pid syear
//browse pid syear biol_int_year who_lived_with_bl num_yrs_bio_bl if bio_int_n > 1
//browse pid syear biol_int_year who_lived_with_bl num_yrs_bio_bl if bio_int_n > 1 & fam_years_change > 1
browse pid syear bioyear_bp bio_int_n biol_int_year who_lived_with_bl yrs_bio_parent num_yrs_bio_bp num_yrs_singlemom_bp num_yrs_partneredmom_bp num_yrs_singledad_bp num_yrs_partnereddad_bp num_yrs_otherrel_bp num_yrs_foster_bp num_yrs_inhome_bp

egen yrs_live_accounting = rowtotal(num_yrs_bio_bp num_yrs_singlemom_bp num_yrs_partneredmom_bp num_yrs_singledad_bp num_yrs_partnereddad_bp num_yrs_otherrel_bp num_yrs_foster_bp num_yrs_inhome_bp), missing
egen yrs_live_mom = rowtotal(num_yrs_singlemom_bp num_yrs_partneredmom_bp), missing
egen yrs_live_dad = rowtotal(num_yrs_singledad_bp num_yrs_partnereddad_bp), missing
egen yrs_live_other = rowtotal(num_yrs_otherrel_bp num_yrs_foster_bp num_yrs_inhome_bp), missing

tab yrs_bio_parent who_lived_with_bl, m // there is like no overlap here

gen who_lived_with = .
replace who_lived_with = who_lived_with_bl if who_lived_with_bl!=.
replace who_lived_with = 1 if who_lived_with==. & yrs_bio_parent>=9 & yrs_bio_parent!=. // let's use 60%? (this way also there can be no overlap) or should I go all the way to 8? So more than half? leave for now - might need to impute the underlying variables anyway and recreate?
replace who_lived_with = 2 if who_lived_with==. & yrs_live_mom>=9 & yrs_live_mom!=.
replace who_lived_with = 3 if who_lived_with==. & yrs_live_dad>=9 & yrs_live_dad!=.
replace who_lived_with = 4 if who_lived_with==. & yrs_live_other>=9 & yrs_live_other!=.

label define who_lived_with 1 "Both Parents" 2 "Mom" 3 "Dad" 4 "Other"
label values who_lived_with who_lived_with

tab who_lived_with, m
tab yrs_bio_parent who_lived_with, m

// still live near childhood home
browse pid syear bio_int_n live_fam_bl live_fam_bp
tab live_fam_bl, m
tab live_fam_bp, m
tab live_fam_bp live_fam_bl, m
replace live_fam_bp = live_fam_bl if live_fam_bp==. & live_fam_bl!=.
replace live_fam_bp = . if live_fam_bp==4

label define live_fam 1 "Yes, Still" 2 "Yes, Again" 3 "No" 4 "DNA"
label values live_fam_bp live_fam_bl live_fam

* Religion
label values religious_affiliation plh0258_h
label define religion 1 "Catholic" 2 "Protestant" 3 "Other Christian" 4 "Islam" 5 "Other" 6 "Non-denominational" 7 "Orthodox Christian" 8 "Islam" 9 "Shiite" 10 "Alevi" 11 "Multiple" 12 "Jewish"
label values religious_affiliation religion
tab religious_affiliation, m // the cpf (and I agree) choose to keep this as time-varying. One question is whether it should be filled in based on the same value on either side - especially because this isn't asked every year. do we assume it doesn't change *that* option?

	// this is what I do in UKHLS to fill in. Do I do *this* or only fill in if the religion before and after the gap are same?
	quietly unique religious_affiliation if religious_affiliation!=., by(pid) gen(num_religion)
	bysort pid (num_religion): replace num_religion = num_religion[1]
	tab num_religion, m 

sort pid syear
browse pid syear num_religion religious_affiliation

	bysort pid: egen religion_est = max(religious_affiliation) if num_religion==1
	replace religion_est = religious_affiliation if num_religion > 1
	label values religion_est religion

* Survey / respondent / sample info
fre psample_pl	// which sample are they a part of - this will be good for imputation.
label values survey_status_pl netto
fre survey_status_pl // not sure this level of detail is needed
fre status_pl // this is the recoded version of above
label values survey_status_pb befstat_h
fre survey_status_pb // this is more info like re-survey, new, etc. also not sure if needed
label values person_surveyed_hb befhpmax
tab person_surveyed_hb, m // I actually don't know what this is - the range is 0-10. Is this like within HH person number? Not sure where that info is. Also - there are many missing. maybe it goes with pteil? person number in HH (no - bc that is also HH info...) but without info on who that is, this is not that useful? do I need to combine with relationship to HH head (so then it's like HH head v. other?) could also just use to indicate if respondent is the person surveyed. but this only applies to like - the HH questionaire - the respondent answers their own survey. so this also probably not that useful...

// some people actually never have interview?
tab ever_int, m // let's write this down because I think I want to drop?

// temp save
save "$created_data/gsoep_couple_data_recoded.dta", replace

********************************************************************************
**# Divison of labor variables
********************************************************************************
* Employment status - variable = employment (this code from gendering project)
label define employment 1 "FT" 2 "PT" 3 "Training" 4 "Marginally employed" 5 "PT retired" ///
6 "Military (vol)" 7 "Volunteering" 8 "Disabled" 9 "Not employed" 10 "Internship / ST work" 11 "Military (comp)" 12 "short-time work" 13 "Retired"
label values employment employment
tab employment, m // harmonized employment variable (labels currently in German)
tab employment status_pl, m // yes so missing correspond to not in sample

tab laborforce_pg, m // this is more why not working. okay might be useful for retirement though. that is not in above employment, so let's add
tab laborforce_pg emplst_pg, m // so really a breakdown of 5 above (or 9 for detailed)
tab employment laborforce_pg

gen employment_orig = employment
label values employment_orig employment
replace employment=13 if employment== 9 & laborforce_pg == 2

// less detailed version
label define employment_gp 1 "FT" 2 "PT" 3 "Training" 4 "Marginal Employment" 5 "Not Employed" 6 "Disabled (not comprehensive)" 7 "short-time work (not comprehensive)"
label values emplst_pg  employment_gp
tab emplst_pg, m
tab employment emplst_pg, m // okay these correspond quite well - decide how detailed we should use later

// even less detailed version
	/* Per CNEF codebook, for variable e11104 (working or not working now), they consider working to be:
	If the individual reported being full-time, part-time, or marginally employed, having
	short-time work, performing military/civilian service, on maternity leave, or being
	engaged in in-company training then the individual is considered to be working now.
	If the individual reported not being employed or being unemployed then the individual is considered to be not working now.
	
	CPF uses CNEF version but that is not up to date through v40, only through v39
	
	think either labor force status of 11 (working) or 12 (working but inactive in last 7 days) OR
	emplst_pg of 5
	*/
	
gen employed_binary=.
replace employed_binary = 0 if emplst_pg==5
replace employed_binary = 1 if inlist(emplst_pg,1,2,3,4,6,7)  // hmm in labor force, they consider vocational training as employed. let's see if work hours recorded

tab emplst_pg employed_binary, m
tab laborforce_pg employed_binary, m row // yes, this basically pulls out the 11 / 12, so this is fine (not even sure I use this variable...)

* Employment hours
inspect hours_per_wk // Actual hours per week, including overtime (from pl)
inspect work_hours_pg // actual work time per week (also includes overtime - from pgen) - has less missing. the pgen ones are validated longitudinally and are also what cpf uses
inspect work_hours_agreed_pg // agreed upon work time - cpf includes these as contracted work hours per week
// note, for all of these, I believe missing if not employed, so want to replace those with 0. (the .n specifically I think not just .?)

	// tabstat hours_per_wk work_hours_pg work_hours_agreed_pg, stats(n p50 mean)
	// browse pid syear hours_per_wk work_hours_pg work_hours_agreed_pg overtime_pg

// Overtime variables (Confirm that actual work hours includes OT, I believe it does so I want that)
inspect work_ot_v1 work_ot_v2 // (pl) do you work overtime (1997-2021)
inspect hours_ot // (pl) hours of overtime last month (all except 1984, 1985, 1987)
inspect work_ot_lastmonth // (pl) did you work OT last month same as above
inspect overtime_pg // (pg) overtime per week

tab work_hours_pg employed_binary, m col
tab work_hours_pg emplst_pg, m

gen weekly_work_hrs = .
replace weekly_work_hrs = 0 if employed_binary==0
replace weekly_work_hrs = work_hours_pg if employed_binary==1 & work_hours_pg!=.
replace weekly_work_hrs = hours_per_wk if employed_binary==1 & weekly_work_hrs==. & hours_per_wk!=.
tab weekly_work_hrs employed_binary, m col
tabstat weekly_work_hrs, by(status_pl) stats(N mean)
	
// ft / pt
recode employment (1=1) (2 4 12=2) (3 5/11 13=3), gen(ftpt_r) // this is based on self-reported employment status (cpf code)
tab employment ftpt_r, m

gen ftpt_h=.
replace ftpt_h=1 if weekly_work_hrs>=35 & weekly_work_hrs!=. // these are created above
replace ftpt_h=2 if weekly_work_hrs<35 & weekly_work_hrs>0
replace ftpt_h=3 if weekly_work_hrs==0
// replace fptime_h=3 if emplst5>1 & emplst5<.

lab def ftpt 1 "Full-time" 2 "Part-time/irregular" 3 "Not empl/other"
lab val ftpt_r ftpt_h ftpt

lab var ftpt_r "Employment Level (self-report)"
lab var ftpt_h "Employment Level (based on hours)"

tab ftpt_h ftpt_r, m // loosely congruous

* Earnings (individual)
tabstat gross_labor_inc_pg net_labor_inc_pg, by(employed_binary) stats(N mean)
tab employed_binary
	
gen gross_income_lm = .
replace gross_income_lm = 0 if employed_binary==0 // same thing, want earnings to be 0 if not employed
replace gross_income_lm = gross_labor_inc_pg if employed_binary==1

gen net_income_lm = .
replace net_income_lm = 0 if employed_binary==0
replace net_income_lm = net_labor_inc_pg if employed_binary==1

tabstat weekly_work_hrs gross_income_lm net_income_lm, by(employed_binary)

* Total Income (HH)
gen hh_net_income_mon_est_cnef = hh_net_income_py_cnef / 12 // cnef is annual - how close are they if trned to monthly?
tabstat hh_net_income_monthly_hl hh_net_income_monthly_hg hh_net_income_mon_est_cnef hh_gross_income_py_cnef hh_net_income_py_cnef hh_gross_laborinc_py_cnef, by(syear) stats(N mean p50)
// the two nets (from hl and hgen) seem identical. the cnef ones are perpetually higher. is it possible they also don't include same things?
// okay, I think that the post government includes transfers so is not just net of taxes. when labor market income is 0, sometimes net is higher than gross, and I am thinking this is bc of post-tax government transfers that actually add income.

browse pid syear net_income_lm hh_net_income_monthly_hl hh_net_income_monthly_hg hh_net_income_mon_est_cnef hh_net_income_py_cnef hh_gross_income_py_cnef hh_gross_laborinc_py_cnef

sum hh_net_income_monthly_hg, det // not really 0s here - when zero v. when missing?
sum hh_gross_income_py_cnef, det // these 0s make sense
inspect hh_net_income_monthly_hg hh_net_income_mon_est_cnef if syear<2023
// tab sex_pl if hh_gross_income_py_cnef==0
// tab sex_pl if hh_gross_laborinc_py_cnef==0
browse pid syear status_pl hh_net_income_monthly_hg net_income_lm hh_gross_laborinc_py_cnef hh_net_income_mon_est_cnef

gen hh_income_net_monthly = hh_net_income_monthly_hg
replace hh_income_net_monthly = 0 if hh_net_income_monthly_hg==. & hh_gross_income_py_cnef==0 // let's just do this for now. there are cases that I think the HH are just non-response because there is at least individual labor market income, so HH income should not be 0

* Employment adjacent: retirement
	// not pulling in from CPF for the moment - going to attempt to use the SOEP variable: py_retired and / or employment status
// okay py retired is essentially 12 0/1s stored in one variable, indicating how many months retired in prior year
// okay but there are actually 24, this is very confusing and doesn't really correspond to employment indicator (most common is 01010101...)
browse pid syear py_retired employment
tab py_retired if employment==13

	// so, need to use the CNEF version
* create zeros (obs with any type of information)
recode  employment_orig (1/12=0), gen (ret_cpf)
replace ret_cpf=0 if occupation_position_pg>=10 & occupation_position_pg<.
replace ret_cpf=0 if laborforce_pg>=1 & laborforce_pg<.

* create MV(.) with no information 
replace ret_cpf=. if   (employment_orig<0 | employment_orig==.) & (occupation_position_pg<=0 | occupation_position_pg==.) & (laborforce_pg<0 | laborforce_pg==.)

* Fill MV
		* Code below fills information for individuals who left due to retirement
		* for years after the event
		sort pid syear
		bysort pid: gen  temp_order= _n
		bysort pid: gen  temp_yret=syear if employment_termination==6 | employment_termination==12 // reaching retirement age (or pension - new)
		bysort pid: egen temp_yret2=max(temp_yret)
		bysort pid: gen  temp_ret=1 if syear>=temp_yret2
		
* Criteria for 1
replace ret_cpf=1  if (employment_orig==9 | employment_orig==7 | employment_orig==5) &		/// NW or Voluntary Services (FSJ / FOEJ / BFD) or Near Retirement, Zero Working Hours
					 laborforce_pg!=11 &								/// NW
					 temp_ret==1 & age>=50						// left job becouse of retirement (plb0304_h)
		* Can drop the temporary files 	
		drop temp_*
	
replace ret_cpf=1  if (employment_orig==9 | employment_orig==7 | employment_orig==5) & laborforce_pg!=11 & age>=65	// age>65

replace ret_cpf=1  if (employment_orig==9 | employment_orig==7 | employment_orig==5) &	 laborforce_pg!=11 &	/// 
					  (early_ret_pension_py_pk==1 | old_age_pension_py_pk==1)  &		/// received pension (maybe early)
					  age>=50						//  age 
	
	lab var ret_cpf "Retired fully (NW, old-age pens, 45+)"
	lab val ret_cpf yesno 	

tab employment ret_cpf, m // so good news is that all of my retired by employment status are 1s there, they also have additional people from not employed that are labelled as retired, probably based on these other variables. So, let's use theirs.
browse pid syear status_pl ret_cpf employment // feels like retirement sort of becomes all absorbing, but then not...(aka when not sample - it's still missing) possibly can fill in all after first instance of retirement with retirement status? well - do I fill in now or when "filled in" - because I am going to add even more missing rows later...
gen retired_in_year = syear if ret_cpf==1
bysort pid: egen retirement_yr = min(retired_in_year)
sort pid syear
browse pid syear age status_pl retirement_yr ret_cpf employment // okay - one problem with this is, if entered survey retired, we actually don't know true first year of retirement...
tab age if syear == retirement_yr

* Disability
*   ple0040 ple0041_h m11124
// disability_yn
// disability_amount
// disability_yn_cnef // if 30%+ disability

replace disability_yn = 0 if disability_yn==2
tab disability_yn disability_yn_cnef, m // mostly congruent. the pl version has more filled in, though cnef version has some 0 where pl = missing
replace disability_yn = disability_yn_cnef if disability_yn==. & disability_yn_cnef!=.

tab disability_amount, m // mostly missing - oh, prob because missing if no disability
tab disability_amount disability_yn, m col // about 3% missing if disability = yes
replace disability_amount = 0 if disability_yn==0 // no 0s if yes, so will make 0 no
	
* Self-rated health
 /*Note: 
- ple0008 has more data but better to use m11126 which was cleaned 
- m11126 - see Pequiv manual
- 1984-1991, 1993: Data not available in SOEP
- data for 1992 and since 1994

// so I only have the pl version above (as self_reported_health)
// the other one is pequiv version - as they say, seems to have many more DNA.
// not sure why we wouldn't supplement possibly?
// I pulled in CNEF version as self_reported_health_cnef - that is what CPF uses
*/

label define srh5 1 "Very Good" 2 "Good" 3 "Satisfactory" 4 "Poor" 5 "Bad" // 
label values self_reported_health self_reported_health_cnef srh5

tab self_reported_health, m
tab self_reported_health_cnef, m
tab self_reported_health self_reported_health_cnef, m
replace self_reported_health = self_reported_health_cnef if self_reported_health==. & self_reported_health_cnef!=.

* Unpaid labor - think these are actually mostly fine to use as is

tabstat housework_weekdays housework_saturdays housework_sundays, by(syear) stats(mean p50 N)
tabstat housework_weekdays errands_weekdays repair_weekdays childcare_weekdays, by(syear) stats(mean p50 N)

browse pid sex_pl syear partnered_total housework_weekdays housework_saturdays housework_sundays repair_weekdays errands_weekdays // note that weekdays have been asked always, sat and sun inconsistent and generally in odd years only

// Core HW
tab housework_weekdays if status_pl==1, m // about 4.5% missing if in sample
tab housework_saturdays if status_pl==1, m // more than half missing
tab housework_sundays if status_pl==1, m // more than half missing

// Errands
tab errands_weekdays if status_pl==1, m // less coverage
tab errands_saturdays if status_pl==1, m 
tab errands_sundays if status_pl==1, m 

// Repair work
// okay, actually sundays maybe shouldn't be a simple total. See harmonization notes for: https://paneldata.org/soep-core/datasets/pl/pli0016_h

/*Step 3: The variable pli0016_h is replaced by the variable pli0016_v2 for the year 1990, if pli0016_v2 is greater than 0 and pli0016_v3 is less than 0. Step 4: The variables pli0016_v2 and pli0016_v3 are added and then divided by 2 in order to create a mean and subsequently integrated into the harmonized variable pli0016_h for the year 1990, if pli0016_v2 is greater than 0 and pli0016_v3 is greater than 0. Step 5: The variable pli0016_h is replaced by the variable pli0016_v3 for the year 1990, if pli0016_v3 is greater than 0 and pli0016_v2 is less than 0.

since I recoded - the less than 0 are actually missing - so basically filled in if the other versions are missing
*/

tab repair_weekdays if status_pl==1, m // generally good coverage
tab repair_saturdays if status_pl==1, m
tabstat repair_sundays*, by(syear) stats(mean N)
browse pid syear repair_sundays* // need to figure out sundays
tab repair_sundays_v2 if repair_sundays_v3!=0 & repair_sundays_v3!=. 
tab repair_sundays_v3 if repair_sundays_v2!=0 & repair_sundays_v2!=. 

tab repair_sundays_v1 if repair_sundays_v3!=0 & repair_sundays_v3!=. & syear==1990
tab repair_sundays_v1 if repair_sundays_v2!=0 & repair_sundays_v2!=. & syear==1990 // so v1 always 0 if v2/v3 filled in
tab repair_sundays_v1 if repair_sundays_v3!=0 & repair_sundays_v3!=. & repair_sundays_v2!=0 & repair_sundays_v2!=. & syear==1990
	// v1= 1984-1990, v2=1990, v3=also 1990?, v4=1992-2021 (odd years only past 1993)
	// v2 = with holidays
	// v3 = without holidays
// egen repair_sundays = rowtotal(repair_sundays_v1 repair_sundays_v2 repair_sundays_v3 repair_sundays_v4), missing
egen repair_sundays_mean = rowmean(repair_sundays_v1 repair_sundays_v2 repair_sundays_v3)

gen repair_sundays=.
replace repair_sundays = repair_sundays_v1 if syear<=1989
replace repair_sundays = repair_sundays_v4 if syear>=1991
replace repair_sundays = repair_sundays_v1 if syear==1990 & (repair_sundays_v1!=0 & repair_sundays_v1!=.) & repair_sundays_v2==. & repair_sundays_v3==.
replace repair_sundays = repair_sundays_v2 if syear==1990 & (repair_sundays_v2!=0 & repair_sundays_v2!=.) & repair_sundays_v1==. & repair_sundays_v3==.
replace repair_sundays = repair_sundays_v3 if syear==1990 & (repair_sundays_v3!=0 & repair_sundays_v3!=.) & repair_sundays_v1==. & repair_sundays_v2==.
replace repair_sundays = repair_sundays_mean if syear==1990 & repair_sundays==.

// tab repair_sundays if syear==1990
// tab repair_sundays_v1 if syear==1990 & repair_sundays==., m
// tab repair_sundays_v2 if syear==1990 & repair_sundays==., m
// tab repair_sundays_v3 if syear==1990 & repair_sundays==., m

tab repair_sundays, m
tab repair_sundays if status_pl==1, m
browse pid syear repair_sundays repair_sundays_*

// Childcare (tbd if will recode)
tabstat childcare_weekdays childcare_saturdays childcare_sundays, by(syear) stats(mean p50 N)
tab childcare_weekdays if status_pl==1, m // very few missing, but a lot of 0s
tab childcare_saturdays if status_pl==1, m  // more than half missing
tab childcare_sundays if status_pl==1, m // more than half missing

tab outside_help, m
gen any_outside_help = .
replace any_outside_help = 0 if outside_help==3
replace any_outside_help = 1 if inlist(outside_help,1,2) // regularly (1), occasionally (2)

tab outside_help any_outside_help, m

// temp save
save "$created_data/gsoep_couple_data_recoded.dta", replace

********************************************************************************
**# HH / family composition variables
********************************************************************************
* Type of HH (hh_type_hg hh_type_det_hg)
// this is useful, but there is missing info for non-survey years and I am not sure this is worth imputing relative to the variables below that I could use to create this
label values hh_type_hg  hgtyp1hh
label values hh_type_det_hg hgtyp2hh

tab hh_type_hg status_pl, m
tab hh_type_det_hg status_pl, m

* Parental info
// year of first / last birth
	unique pid if yr_birth_child1_bl!=.
	unique pid yr_birth_child1_bl if yr_birth_child1_bl!=.
	
	quietly unique yr_birth_child1_bl if yr_birth_child1_bl!=., by(pid) gen(b1_change)
	bysort pid (b1_change): replace b1_change=b1_change[1]
	tab b1_change, m
	sort pid syear
	browse pid syear first_birth_year_bh last_birth_year_bh yr_birth_child*_bl if b1_change==2
	
	forvalues b=1/8{
		bysort pid (yr_birth_child`b'_bl): replace yr_birth_child`b'_bl=yr_birth_child`b'_bl[1] if b1_change<2
	}
	
egen first_birth_year_bl = rowmin(yr_birth_child*_bl)
egen last_birth_year_bl = rowmax(yr_birth_child*_bl)

tab num_children_bl, m
bysort pid (num_children_bl): replace num_children_bl=num_children_bl[1]
tab has_no_children_bl, m
bysort pid (has_no_children_bl): replace has_no_children_bl=has_no_children_bl[1]
tab any_births_bh has_no_children_bl, m
tab num_children_bl has_no_children_bl, m

sort pid syear
browse pid syear sumkids first_birth_year_bh first_birth_year_bl last_birth_year_bh last_birth_year_bl yr_birth_child*_bl

gen first_birth_year = first_birth_year_bh
replace first_birth_year = first_birth_year_bl if first_birth_year==. & first_birth_year_bl!=. 
tab first_birth_year any_births_bh, m

gen last_birth_year = last_birth_year_bh
replace last_birth_year = last_birth_year_bl if last_birth_year==. & last_birth_year_bl!=. 
tab last_birth_year any_births_bh, m

// browse pid syear sumkids first_birth_year first_birth_year_bh first_birth_year_bl last_birth_year last_birth_year_bh last_birth_year_bl

// ever parent
tab sumkids any_births_bh, m

// number of births - cpf uses sumkids

* Children in HH (# and ages)
// oh, these are all just yes, no lol: num_children_u16_v1_hl num_children_u16_v2_hl num_children_u17_hl 
browse pid syear num_children_cnef num_hh_0_1_cnef num_hh_2_4_cnef num_hh_5_7_cnef num_hh_8_10_cnef num_hh_11_12_cnef num_hh_13_15_cnef num_hh_16_18_cnef num_children_u16_v1_hl num_children_u16_v2_hl num_children_u17_hl 

// number
// clonevar kidsn_hh17= d11107 // this is from pequiv; I renamed num_children_cnef
// this is making me realize - they might use the CNEF version rather than the fact that they have like 3 different versions of kids in the non-cnef version (that I was going to try to figure out how to harmonize)

tab num_children_cnef num_children_u16_v1_hl
tab num_children_cnef num_children_u16_v2_hl
tab num_children_cnef num_children_u17_hl // these generally make sense

// then the pequiv file also has info by age
	// num_hh_0_1_cnef num_hh_2_4_cnef num_hh_5_7_cnef num_hh_8_10_cnef num_hh_11_12_cnef num_hh_13_15_cnef num_hh_16_18_cnef

// then I made variables from ppathl also split by age - let's validate with above
	// kidsu18_hh kidsu6_hh
	// mine has less missing (bc based on HH info)
	tab kidsu18_hh num_children_cnef, m // okay, they are very close so I feel good about mine
	browse pid syear num_children_cnef kidsu18_h
	
// age of youngest?
	// age_oldest_child age_youngest_child // I made these from ppathl
	browse pid syear kidsu18_hh age_youngest_child age_oldest_child num_hh_0_1_cnef num_hh_2_4_cnef num_hh_5_7_cnef num_hh_8_10_cnef num_hh_11_12_cnef num_hh_13_15_cnef num_hh_16_18_cnef // okay my ages make sense based on cnef counts by age also
	
	replace age_youngest_child = 9999 if kidsu18_hh==0
	
* Other HH comp
// total HH size (we don't really use this variable)
	// hh_size_cnef

// people age 65+
tab num_65up_hh, m
tab age num_65up_hh, m 

// parents in HH
// these are all from biol which doesn't make a ton of sense / utility because that is not really time-varying...
browse pid syear num_parent_in_hh_v1_bl num_parent_in_hh_v2_bl father_in_hh_bl where_father_live_v1_bl where_father_live_v2_bl where_father_live_v3_bl where_mother_live_v1_bl where_mother_live_v2_bl where_mother_live_v3_bl

// wait but if these are from bioparent, they are actually not useful either? because that is truly not time varying? Perhaps knowing if deceased is helpful. but do I need to do what I did with PSID (I think) and use their person ID and HH roster to get this info? Okay but bioparent also tells us what year updated. So I guess I know if it's accurate in that year?
browse pid syear where_father_live_bp father_live_updated_bp where_mother_live_bp mother_live_updated_bp // father_res_bp mother_res_bp

label define parent_live_rec 0 "deceased" 1 "same HH" 2 "same housing" 3 "nearby" 4 "far wi germany" 5 "outside germany"
foreach var in father_live_bp mother_live_bp{
	gen `var'_rec = .
	replace `var'_rec = 0 if where_`var'==0 // deceased
	replace `var'_rec = 1 if where_`var'==1 // same HH
	replace `var'_rec = 2 if where_`var'==2 // same housing
	replace `var'_rec = 3 if inlist(where_`var',3,4) // nearby
	replace `var'_rec = 4 if inlist(where_`var',5,6,8,9,13) // far within Germany
	replace `var'_rec = 5 if inlist(where_`var',7,10,11,12) // far outside Germany
	
	label values `var'_rec parent_live_rec
}

tab father_live_updated_bp // okay so many of these updates do correspond to when the below are measured (aka 1986-2016, every 5 years) so that is probably where it comes from?
tab father_live_bp_rec father_in_hh if syear==father_live_updated_bp, m // so the most congruence is when in same HH, but still not great...wonder if some of this is because there is not a mother / father ID?
tab mother_live_bp_rec mother_in_hh if syear==mother_live_updated_bp, m

// found some additional variables that aren't comprehensive. Compare these to HH roster info...
/*
mother_live_pl_v1 father_live_pl_v1 // 1986 - irrelevant bc I don't have any observations 1986
mother_present_yn_pl father_present_yn_pl // 1991,1996,2001,2006,2011,2016
mother_live_pl_v2 father_live_pl_v2 // 1991,1996,2001,2006,2011,2016
*/

label define parent_live_v2 0 "Same HH" 1 "Same House" 2 "Same Neighborhood" 3 "Same Town" 4 "Another Town" 5 "Further Away" 6 "Abroad" // it is not clear what the differences between same household and same house are...okay, in the bp codebook, it's "same housing" - could that possibly be like same apartment complex?
label values mother_live_pl_v2 father_live_pl_v2 parent_live_v2

tab mother_live_pl_v2 mother_present_yn_pl, m // mother present might actually mean just alive? because all 1s for response to where live and then both missing together
tab father_live_pl_v2 father_present_yn_pl, m
tab mother_live_pl_v2 mother_in_hh, m row // so for same HH, about 70% alignment (I also have as 1), but only 15% alignment for same house, so not actually sure what that means...
tab father_live_pl_v2 father_in_hh, m row

browse pid syear age mother_in_hh mother_pid_bp mother_live_pl_v2 mother_live_bp_rec mother_live_updated_bp father_in_hh father_pid_bp father_live_pl_v2 father_live_bp_rec father_live_updated_bp

gen num_parent_in_hh = .
replace num_parent_in_hh = 0 if father_in_hh==0 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==1 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==0 & mother_in_hh==1
replace num_parent_in_hh = 2 if father_in_hh==1 & mother_in_hh==1

gen any_parent_in_hh = .
replace any_parent_in_hh = 0 if num_parent_in_hh==0
replace any_parent_in_hh = 1 if inrange(num_parent_in_hh,1,4)

// people in HH require aid. will just leave this as descriptive, don't know how much I want to do with this
tab aid_in_hh_hl, m
replace aid_in_hh_hl = 0 if aid_in_hh_hl==2
tab aid_outside_hh_hl, m // these are too small to do anything with
tab aid_inside_hh_hl, m

// temp save
save "$created_data/gsoep_couple_data_recoded.dta", replace

********************************************************************************
**# Do I want to create any of the fixed variables here?
********************************************************************************
// Education (following UKHLS)
unique pid if status_pl==1 // 25582
unique pid edu4 if status_pl==1 // 29003
unique pid isced97_pg if status_pl==1 // 29419
unique pid yrs_educ_pg if status_pl==1 // 29665

browse pid syear status_pl edu4 isced97_pg yrs_educ_pg // pid 2802 = good example of change

quietly unique edu4 if edu4!=., by(pid) gen(edu4_change)
quietly unique isced97_pg if isced97_pg!=., by(pid) gen(isced_change)
quietly unique yrs_educ_pg if yrs_educ_pg!=., by(pid) gen(yrs_educ_change)

foreach var in edu4_change isced_change yrs_educ_change{
	bysort pid (`var'): replace `var'=`var'[1]
	tab `var', m // more granular = more change (aka yrs educ). ranges from 70-78%-ish with more than 1
}

// try to get at first relationship duration, but if not, prioritize earliest measurement
*edu4
gen edu4_fixed = edu4 if edu4_change==1
replace edu4_fixed = edu4 if edu4_fixed==. & relative_duration == min_dur
bysort pid (edu4_fixed): replace edu4_fixed=edu4_fixed[1]
tab edu4_fixed, m // this didn't solve the problem - is it because some people only have missing (is that the 0s I think?)
label values edu4_fixed edu4
tab edu4_change edu4_fixed, m // yes mostly
tab ever_int edu4_fixed, m // I forgot about this - should I just drop those who never had a valid interview? will do in next step

sort pid syear
browse pid syear status_pl ever_int edu4_fixed edu4 isced97_pg yrs_educ_pg

forvalues d=-1/12{
	replace edu4_fixed = edu4 if relative_duration == `d' & edu4_fixed==.
	bysort pid (edu4_fixed): replace edu4_fixed=edu4_fixed[1]
}

*isced_97
gen isced97_fixed = isced97_pg if isced_change==1
replace isced97_fixed = isced97_pg if isced97_fixed==. & relative_duration == min_dur
bysort pid (isced97_fixed): replace isced97_fixed=isced97_fixed[1]

forvalues d=-1/12{
	replace isced97_fixed = isced97_pg if relative_duration == `d' & isced97_fixed==.
	bysort pid (isced97_fixed): replace isced97_fixed=isced97_fixed[1]
}
tab isced_change isced97_fixed, m

label define isced97 0 "in school" 1 "inadequately" 2 "general elem" 3 "middle voc" 4 "voc + abi" 5 "higher voc" 6 "higher educ"
label values isced97_pg isced97_fixed isced97

tab isced97_fixed edu4_fixed, m // loosely correspond

*years of education // this one is most confusing, per codebook
gen yrs_educ_fixed = yrs_educ_pg if yrs_educ_change==1
replace yrs_educ_fixed = yrs_educ_pg if yrs_educ_fixed==. & relative_duration == min_dur
bysort pid (yrs_educ_fixed): replace yrs_educ_fixed=yrs_educ_fixed[1]

forvalues d=-1/12{
	replace yrs_educ_fixed = yrs_educ_pg if relative_duration == `d' & yrs_educ_fixed==.
	bysort pid (yrs_educ_fixed): replace yrs_educ_fixed=yrs_educ_fixed[1]
}
tab yrs_educ_fixed yrs_educ_change, m

// Nationality
// created this above // quietly unique nationality_region if nationality_region!=., by(pid) gen(nat_change)
tab nat_change, m 
// but should I check the base variable? nationality_pb
quietly unique nationality_pb if nationality_pb!=., by(pid) gen(nat_det_change)
bysort pid (nat_det_change): replace nat_det_change=nat_det_change[1]
tab nat_det_change, m

gen nat_region_fixed = nationality_region if nat_change==1
replace nat_region_fixed = nationality_region if nat_region_fixed==. & relative_duration == min_dur
bysort pid (nat_region_fixed): replace nat_region_fixed=nat_region_fixed[1]
tab nat_region_fixed, m
tab nat_region_fixed nat_change, m 
tab relative_duration if nat_change==2 & nat_region_fixed==. & nationality_region!=.

gen nationality_fixed = nationality_pb  if nat_det_change==1
replace nationality_fixed = nationality_pb if nationality_fixed==. & relative_duration == min_dur
bysort pid (nationality_fixed): replace nationality_fixed=nationality_fixed[1]
tab nationality_fixed, m
tab nationality_fixed nat_det_change, m 

forvalues d=-1/12{
	replace nat_region_fixed = nationality_region if relative_duration == `d' & nat_region_fixed==.
	bysort pid (nat_region_fixed): replace nat_region_fixed=nat_region_fixed[1]
	
	replace nationality_fixed = nationality_pb if relative_duration == `d' & nationality_fixed==.
	bysort pid (nationality_fixed): replace nationality_fixed=nationality_fixed[1]
}

label values nationality_fixed pnat_h
label values nat_region_fixed cob

**# Final save
save "$created_data/gsoep_couple_data_recoded.dta", replace

********************************************************************************
**# Missing data checks
********************************************************************************
// clear all
// use "$created_data/gsoep_couple_data_recoded.dta", clear

// let's do a check of the variables I either will use for analysis or will use to impute, so I can be sure I a. properly impute and b. properly recoded
misstable summarize weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays repair_saturdays repair_sundays errands_weekdays errands_saturdays errands_sundays childcare_weekdays childcare_saturdays childcare_sundays aid_in_hh_hl gross_income_lm net_income_lm employment emplst_pg employed_binary edu4 isced97_pg yrs_educ_pg edu4_fixed isced97_fixed yrs_educ_fixed any_births_bh kidsu18_hh age_youngest_child partnered_total marst_defacto hh_income_net_monthly hh_gross_income_py_cnef hh_net_income_py_cnef num_65up_hh num_parent_in_hh any_parent_in_hh born_in_germany where_born_ew where_born_state country_born_pl global_region_born nationality_pb nationality_region nationality_fixed nat_region_fixed father_educ mother_educ who_lived_with yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other where_germany_pl federal_state_hb region_type live_fam_bp housing_status_hg home_owner religious_affiliation religion_est retirement_yr disability_yn disability_amount self_reported_health birthyr_pl first_birth_year eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no psample_pl status_pl survey_status_pb sex_pl, all // so the >. will also help me understand if any of my lingering .n / .s etc remain, because that is where the alphabet missing go

preserve

local varlist "weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays repair_saturdays repair_sundays errands_weekdays errands_saturdays errands_sundays childcare_weekdays childcare_saturdays childcare_sundays aid_in_hh_hl gross_income_lm net_income_lm employment emplst_pg employed_binary edu4 isced97_pg yrs_educ_pg edu4_fixed isced97_fixed yrs_educ_fixed any_births_bh kidsu18_hh age_youngest_child partnered_total marst_defacto hh_income_net_monthly hh_gross_income_py_cnef hh_net_income_py_cnef num_65up_hh num_parent_in_hh any_parent_in_hh born_in_germany where_born_ew where_born_state country_born_pl global_region_born nationality_pb nationality_region nationality_fixed nat_region_fixed father_educ mother_educ who_lived_with yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other where_germany_pl federal_state_hb region_type live_fam_bp housing_status_hg home_owner religious_affiliation religion_est retirement_yr disability_yn disability_amount self_reported_health birthyr_pl first_birth_year eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no psample_pl status_pl survey_status_pb sex_pl"

// can I follow this to export more easily? https://www.statalist.org/forums/forum/general-stata-discussion/general/1766742-export-results-from-misstable-sum-to-excel
frame create results str32 varname `c(obs_t)' (eq_dot gt_dot lt_dot ///
    n_distinct minimum maximum)
    
// ds, has(type numeric)
foreach var in `varlist'{
    misstable summ `var'
    if !missing(r(N_eq_dot), r(N_gt_dot)) {
        frame post results ("`var'") (r(N_eq_dot)) (r(N_gt_dot)) (r(N_lt_dot)) ///
            (r(K_uniq)) (r(min)) (r(max))
    }
}

frame change results
label var varname "Variable"
label var eq_dot "Obs=."
label var gt_dot "Obs>."
label var lt_dot "Obs<."
label var n_distinct "Unique values"
label var minimum "Min"
label var maximum "Max"

export excel using "$root/imputation/gsoep_missingtable_pre.xlsx", firstrow(varlabels) replace

restore

// this replaces the data, so no longer my file
// save "$created_data/gsoep_couple_data_recoded.dta", replace
// in next step - drop those without any valid interviews

