********************************************************************************
********************************************************************************
* Project: Relationship Life Course Analysis
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: c_create_couple_sample.do
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file restricts full UKHLS sample to individuals in eligible couples

* Eligibility
* Relationship started after 1990
* Relationship ended before 2011 (or 10 years from last interview date)
* Had to observe relationship start (duration of 0 or 1)
* Aged between 18 and 60 - do this later so can retain these for imputation years jic


********************************************************************************
* Create list of individuals in eligible couples to match on to main file
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

// first, restrict to couples
inspect partner_id if partnered==0 
inspect partner_id if partnered==1

keep if partnered==1
drop if rel_start_all==. // will get removed anyway

// then restrictions based on relationship dates
* need to add duration variable
gen dur = int_year - rel_start_all
tab dur, m

* then, min and max duration
bysort pidp partner_id: egen min_dur = min(dur)
bysort pidp partner_id: egen max_dur = max(dur)

sort pidp year
browse pidp partner_id int_year marital_status_defacto rel_start_all rel_end_all status_all dur min_dur max_dur first_couple_year last_couple_year 

keep if rel_start_all >= 1991 & inlist(min_dur,0,1) // using 1991 because that is first survey year
keep if rel_start_all <= 2012 // doing 2012 as some interviews in last wave were 2022

tab rel_end_all, m // so about 3% missing. should I update with last couple year? SEe below
tab status_all, m // so <1% missing so prob fine? if ongoing, update current_rel_end_year with last survey year? and call it attrited?
tab rel_end_all status_all, m // oh, the ongoing all already have an end date of last survey year. so it's the ones that ended that i don't know...use last couple year? 

browse pidp partner_id marital_status_defacto int_year rel_start_all rel_end_all status_all dur min_dur max_dur first_year_observed first_couple_year last_year_observed last_couple_year rel_end_year_est

// going to fill in end date with last couple year, made a flag for this in last step
replace rel_end_all = last_couple_year if rel_end_all==.

// restrict to working age? maybe do this later so can use this info for imputing?
// tab age_all employed, row
// keep if (age_all>=18 & age_all<=60)

unique pidp
unique pidp partner_id 
egen couple_id = group(pidp partner_id)
// browse pidp partner_id couple_id
unique couple_id

// get list
preserve

collapse (first) rel_start_all rel_end_all status_all ever_transition min_dur max_dur first_couple_year last_couple_year, by(pidp partner_id couple_id)

gen eligible_couple=1
rename couple_id eligible_couple_id
rename rel_start_all eligible_rel_start_year
rename rel_end_all eligible_rel_end_year
rename status_all eligible_rel_status

gen long eligible_partner = partner_id 
by pidp: egen num_rel = count(partner_id)

browse if num_rel > 1

save "$created_data/ukhls_couple_list.dta", replace

restore

********************************************************************************
* Now merge back on to data to create a filter for individuals
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

// some variables to create
gen dur = int_year - rel_start_all
bysort pidp partner_id: egen min_dur = min(dur)
bysort pidp partner_id: egen max_dur = max(dur)

tab dur, m

merge m:1 pidp partner_id using "$created_data/ukhls_couple_list.dta", keepusing(num_rel eligible_couple eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner) // so I actually want to just merge on pidp because i want to keep them potentially even if not partnered anymore. BUT, can indicate which relationship is the eligible one, if multiple. okay, this won't work because some have multiple partners, so will merge for the specific couple, then create a MAX indicator of whether that person is ever eligible

drop if _merge==2
drop _merge

bysort pidp: egen ever_eligible = max(eligible_couple)
bysort pidp: egen max_eligible_rels = max(num_rel)
replace ever_eligible = 0 if ever_eligible==.
tab ever_eligible eligible_couple, m
tab partnered ever_eligible, m

sort pidp year
browse pidp partner_id int_year ever_eligible max_eligible_rels num_rel eligible_partner rel_start_all eligible_rel_start_year rel_end_all eligible_rel_end_year eligible_rel_status eligible_couple_id // if max_eligible_rels > 1

keep if ever_eligible==1

replace eligible_partner = partner_id if eligible_partner==. & partner_id !=.
replace eligible_rel_start_year = rel_start_all if eligible_rel_start_year==. & rel_start_all !=.
replace eligible_rel_end_year = rel_end_all if eligible_rel_end_year==. & rel_end_all !=.
replace eligible_rel_status = status_all if eligible_rel_status==. & status_all !=.
label values eligible_rel_status status

foreach var in eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner{
	bysort pidp (`var'): replace `var' = `var'[1] if inlist(max_eligible_rels,0,1)
}

sort pidp year
// https://www.stata.com/support/faqs/data-management/replacing-missing-values/

foreach var in eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner{
	replace `var' = `var'[_n-1] if `var'==. & `var'[_n-1]!=. & pidp==pidp[_n-1] & max_eligible_rels > 1
	gsort pidp -year 
	replace `var' = `var'[_n-1] if `var'==. & `var'[_n-1]!=. & pidp==pidp[_n-1] & max_eligible_rels > 1
	sort pidp year
}

/* old
gen temp_year = eligible_rel_start_year
replace temp_year = temp_year[_n-1] if temp_year==. & temp_year[_n-1]!=. & pidp==pidp[_n-1] & max_eligible_rels > 1
// browse pidp year temp_year eligible_rel_start_year
gsort pidp -year 
replace temp_year = temp_year[_n-1] if temp_year==. & temp_year[_n-1]!=. & pidp==pidp[_n-1] & max_eligible_rels > 1
assert temp_year != .
replace eligible_rel_start_year = temp_year if eligible_rel_start_year==.
drop temp_year
*/

assert eligible_rel_start_year!=.
assert eligible_rel_end_year!=.
// assert eligible_rel_status!=.
// assert eligible_partner!=. // some people will never have a partner id I think

sort pidp year
browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels

gen post_marital_status=.
replace post_marital_status = marital_status_defacto if int_year > eligible_rel_end_year
label values post_marital_status marital_status_defacto

gen post_ended=.
replace post_ended = 1 if inlist(post_marital_status,3,4,5)
bysort pidp (post_ended): replace post_ended=post_ended[1]
sort pidp year

replace eligible_rel_status = 0 if post_ended==1 & eligible_rel_status==.
replace eligible_rel_status = 99 if eligible_rel_status==. & eligible_rel_end_year == last_year_observed // calling this "attrition"

// browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels if eligible_partner==.
// browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels if inlist(pidp,683763125, 687391802, 748014291, 749686407, 816866325)

browse pidp partner_id int_year marital_status_defacto post_marital_status post_ended eligible_partner eligible_rel_start_year eligible_rel_end_year rel_start_all rel_end_all status_all max_eligible_rels first_year_observed last_year_observed if eligible_rel_status==.

egen couple_id = group(pidp eligible_partner)
unique couple_id
unique pidp couple_id
unique pidp partner_id
unique pidp eligible_partner

gen relative_duration_v0 = int_year - eligible_rel_start_year
tab relative_duration_v0, m

// fix the duplicate duration issue (identified below) - when people are interviewed twice in same year
bysort pidp couple_id: egen duplicate_dur = rank(year), unique  // because waves aren't in right order have to use year (proxy for wave)
sort pidp year

gen wave_distance = year - year[_n-1] if pidp == pidp[_n-1] & eligible_partner == eligible_partner[_n-1] // because waves aren't in right order

gen relative_duration = relative_duration_v0 if duplicate_dur==1
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur 
replace relative_duration = relative_duration[_n-1] + wave_distance if pidp == pidp[_n-1] & eligible_partner == eligible_partner[_n-1]  
browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur 
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur  if relative_duration==.
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur  if  inlist(pidp, 476653485, 479990245, 682201165)
replace relative_duration = relative_duration_v0 if relative_duration==.

// keep a few durations around 0 to 10
keep if relative_duration >=-2
keep if relative_duration <=12

save "$created_data/ukhls_eligible_for_imputation.dta", replace

********************************************************************************
**# Ensure data is rectangularized and attempt to fill in missing durations
********************************************************************************
// use "$created_data/ukhls_eligible_for_imputation.dta", clear

// do some figuring out of data first so I know how to update once rectangularized
unique pidp partner_id // partner Id missing a lot bc includes non-partnered years, so will be more (bc one missing record, one not)
unique pidp eligible_partner 

// first see what missing data is like with the data that exists
foreach var in jbhrs total_hours howlng aidhrs fimnlabgrs_dv jbstat employed hiqual_dv xw_anychild_dv nchild_dv nkids_dv age_youngest_child partnered marital_status_defacto fihhmngrs_dv xw_ethn_dv xw_racel_dv country_all gor_dv dob_year year_first_birth current_rel_start_year eligible_rel_start_year xw_memorig xw_sampst ivfio xw_sex{
	inspect `var'
}

// see if some variables are fixed or change
unique pidp 
unique pidp country_all // 19191 instead of 18664 so barely changes 
unique pidp gor_dv // 20409 so changes more if I get more specific
quietly unique gor_dv if gor_dv!=., by(pidp) gen(country_change)
bysort pidp (country_change): replace country_change=country_change[1]
tab country_change, m

sort pidp year
browse pidp int_year relative_duration gor_dv country_all country_change 

unique pidp xw_ethn_dv // oh duh this is fixed because I got from cross-wave file. I'm an idiot
unique pidp xw_racel_dv // oh duh this is fixed because I got from cross-wave file. I'm an idiot
/* frome codebook re; race/ethn and their differences:
Respondents are asked the ethnic group question (racel or racel*t) only the first time they are interviewed.
in a few cases, racel is asked multiple times, and in those cases,
racel_dv prioritises the earliest report while ethn_dv prioritises the latest report.
*/

unique pidp hiqual_dv // some more movement here, but barely. 20817 v. 18664
quietly unique hiqual_dv if hiqual_dv!=., by(pidp) gen(educ_change)
bysort pidp (educ_change): replace educ_change=educ_change[1]
tab educ_change, m

// try to get at first relationship duration, but if not, prioritize earliest measurement
gen hiqual_fixed = hiqual_dv if educ_change==1
replace hiqual_fixed = hiqual_dv if hiqual_fixed==. & relative_duration == min_dur & inlist(min_dur,0,1)
bysort pidp (hiqual_fixed): replace hiqual_fixed=hiqual_fixed[1]

forvalues d=0/12{
	replace hiqual_fixed = hiqual_dv if relative_duration == `d' & hiqual_fixed==.
	bysort pidp (hiqual_fixed): replace hiqual_fixed=hiqual_fixed[1]
}

label define hiqual  1 "Degree" 2 "Other higher degree" 3 "A level" 4 "GCSE" 5 "Other qual" 9 "No qual"
label values hiqual_fixed hiqual

sort pidp year
browse pidp int_year relative_duration hiqual_dv hiqual_fixed educ_change

// need to figure out first birth year do I really need this? oh, i actually didn't even use in final imputation (see PSID files, I am dumb)
/* From codebook
Uses files W_CH1BY4 on data file W_INDRESP
W_LCHDOBY4 on datafile W_NATCHILD
W_MNPID WFNPID W_BIRTHY on W_INDALL
W_LCHBY4 on datafile W_NEWBORN
BW_CH1BY on datafile BW_INDRESP/
*/

**********************************
// here, we finally rectangularize
unique pidp
unique pidp eligible_partner
unique pidp eligible_rel_start_year //   19619
unique couple_id // 19588

drop if couple_id==. // this will cause issues later because those are with missing partner info

browse pidp eligible_partner int_year partnered partner_id eligible_rel_start_year relative_duration couple_id

gen orig_record = 1 // want to know if existed or new below

fillin couple_id relative_duration

// quick checks
tab relative_duration // yes, now it perfect aligns
unique couple_id, by(relative_duration)
bysort couple_id: egen rowcount = count(relative_duration)
tab rowcount, m // all should be 15

unique pidp eligible_rel_start_year
unique couple_id

// pull through fixed variables
foreach var in pidp eligible_couple_id eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status min_dur max_dur first_year_observed first_couple_year last_year_observed last_couple_year hiqual_fixed xw_ethn_dv xw_racel_dv dob_year year_first_birth eligible_rel_start_year xw_memorig xw_sampst xw_sex ever_transition year_transitioned mh_status1 mh_status2 mh_status3 mh_status4 mh_status5 mh_status6 mh_status7 mh_status8 mh_status9 mh_status10 mh_status11 mh_status12 mh_status13 mh_status14 mh_starty1 mh_starty2 mh_starty3 mh_starty4 mh_starty5 mh_starty6 mh_starty7 mh_starty8 mh_starty9 mh_starty10 mh_starty11 mh_starty12 mh_starty13 mh_starty14 mh_endy1 mh_endy2 mh_endy3 mh_endy4 mh_endy5 mh_endy6 mh_endy7 mh_endy8 mh_endy9 mh_endy10 mh_endy11 mh_endy12 mh_endy13 mh_endy14 xw_sex xw_coh1m_dv xw_coh1y_dv xw_evercoh_dv xw_lmar1m_dv xw_lmar1y_dv xw_evermar_dv xw_ch1by_dv xw_anychild_dv{
	bysort couple_id (`var'): replace `var'=`var'[1] if `var'==.
}

replace int_year = eligible_rel_start_year + relative_duration if int_year==.
replace age_all = int_year - dob_year if age_all==.

sort pidp int_year
browse pidp eligible_partner int_year age_all dob_year partnered partner_id eligible_rel_start_year eligible_rel_end_year relative_duration couple_id orig_record

// now see the missing again
foreach var in jbhrs total_hours howlng aidhrs fimnlabgrs_dv jbstat employed hiqual_dv hiqual_fixed xw_anychild_dv nchild_dv nkids_dv age_youngest_child partnered marital_status_defacto fihhmngrs_dv xw_ethn_dv xw_racel_dv country_all gor_dv dob_year year_first_birth current_rel_start_year eligible_rel_start_year xw_memorig xw_sampst ivfio xw_sex{
	inspect `var'
}

// Can I fill in any - namely marital status / partnership status based on history variables? anything about children with birth history also?
browse pidp int_year partnered marital_status_defacto eligible_rel_start_year eligible_rel_end_year current_rel_start_year current_rel_end_year mh_*

gen partnered_imp=partnered
gen marital_status_imp=marital_status_defacto
label values marital_status_imp marital_status_defacto

forvalues y=1/14{
	replace partnered_imp = 1 if partnered_imp==. & int_year >= mh_starty`y' & int_year <= mh_endy`y'
	replace marital_status_imp = 1 if marital_status_imp==. & int_year >= mh_starty`y' & int_year <= mh_endy`y' & mh_status`y'==1 // marriage
	replace marital_status_imp = 2 if marital_status_imp==. & int_year >= mh_starty`y' & int_year <= mh_endy`y' & mh_status`y'==2 // cohab
}

replace partnered_imp=0 if partnered_imp==. & int_year < mh_starty1 // not partnered if prior to first rel date
replace marital_status_imp=6 if marital_status_imp==. & int_year < mh_starty1 & mh_status1==1 // never married if prior to first rel date and it's a marriage

browse pidp int_year partnered_imp partnered marital_status_imp marital_status_defacto eligible_rel_start_year eligible_rel_end_year current_rel_start_year current_rel_end_year mh_*

inspect partnered_imp marital_status_imp

save "$created_data/ukhls_couples_alldurs_long.dta", replace

/* need to temporarily export this because i am an idiot and forgot some key variables.
But already imputed, so going to merge for speed, but will update code later - this note is from 3/2/25 for reference

preserve

collapse (first) eligible_rel_start_year eligible_rel_end_year ever_transition year_transitioned, by(pidp eligible_partner)
save "$temp/final_couple_lookup.dta", replace

restore

*/