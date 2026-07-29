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
* Relationship start date not left censored nor both partners with missing relationship start date info
* Had to observe relationship start (duration of 0 or 1)
* Aged between 18 and 60 - do this later so can retain these for imputation years jic

********************************************************************************
* Create version of file for partner relationship variables to clean up start and end dates
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

// just keep necessary variables

local partnervars "marital_status_defacto partnered first_year_observed last_year_observed rel_no current_rel_start_year current_rel_end_year current_rel_ongoing current_rel_left_censor current_rel_start_if ever_transition transition_est year_transitioned missing_rel_start missing_rel_end rel_start_est_flag rel_end_est_flag in_partner_history" 

keep pidp survey wavename year `partnervars'

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	rename `var' `var'_sp
}

// rename pidp to match the name I gave to partner pidp in main file to match
rename pidp partner_id // okay tried to update all to be pidp above - let's see if this will work

save "$temp/UKHLS_partner_lookup.dta", replace

********************************************************************************
* Attempt to clean up relationship start date using both partners' info
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

**********************************************
// first, restrict to couples
**********************************************
inspect partner_id if partnered==0 
inspect partner_id if partnered==1

keep if partnered==1
// drop if partner_id==. // let's impute these for now and drop in step f to follow previous workflow

**********************************************
// then merge on partner info
**********************************************
merge m:1 partner_id wavename using "$temp/UKHLS_partner_lookup.dta"
drop if _merge==2

gen partner_match=0
replace partner_match=1 if _merge==3
drop _merge

// drop if partner_match==0 // let's impute these for now and drop in step f to follow previous workflow

**********************************************
// Now, let's clean up dates between partners
**********************************************
browse pidp partner_id year int_year xw_memorig first_year_observed first_year_observed_sp first_couple_year last_year_observed last_year_observed_sp last_couple_year current_rel_left_censor current_rel_left_censor_sp missing_rel_start missing_rel_start_sp

tab marital_status_defacto marital_status_defacto_sp, m // okay almost perfectly align
tab current_rel_left_censor current_rel_left_censor_sp, m // so these don't always match but I think I need to incorporate this info into my start date updates GAH
tab current_rel_start_if current_rel_start_if_sp, m
 
sort pidp year
browse pidp partner_id year xw_memorig marital_status_defacto ever_transition current_rel_start_year current_rel_end_year current_rel_start_year_sp current_rel_end_year_sp  current_rel_left_censor current_rel_left_censor_sp  current_rel_start_if current_rel_start_if_sp first_couple_year last_couple_year

gen both_left_censored = .
replace both_left_censored = 0 if current_rel_left_censor==0 | current_rel_left_censor_sp==0 // as long as ONE is zero, it's okay. sometimes there are missing and 0, but I can use the 0s. It is more like if one is 1 and missing, that isn't helpful
replace both_left_censored = 1 if current_rel_left_censor==1 & current_rel_left_censor_sp==1

gen both_imputed = .
replace both_imputed = 0 if inlist(current_rel_start_if,0,1) | inlist(current_rel_start_if_sp,0,1)
replace both_imputed = 1 if current_rel_start_if==2 & current_rel_start_if_sp==2

gen rel_start_match = . 
replace rel_start_match = 0 if current_rel_start_year!=current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.
replace rel_start_match = 1 if current_rel_start_year==current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.

gen rel_end_match = .
replace rel_end_match = 0 if current_rel_end_year!=current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.
replace rel_end_match = 1 if current_rel_end_year==current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.

tab rel_start_match, m
// tab rel_start_match both_left_censored, m // interestingly, don't always match even if both left censored...think I take the earliest in those cases. in this survey, it doesn't really matter bc we align start with survey start, but I generally would rather put you too early and accidentally exclude
tab rel_end_match, m

browse pidp partner_id year marital_status_defacto ever_transition current_rel_start_year current_rel_end_year current_rel_start_year_sp current_rel_end_year_sp first_couple_year last_couple_year rel_start_match rel_end_match current_rel_left_censor current_rel_left_censor_sp  current_rel_start_if current_rel_start_if_sp  in_partner_history in_partner_history_sp

gen current_rel_start = .
replace current_rel_start = current_rel_start_year if rel_start_match==1
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_left_censor==0 // prio NON left censored
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_left_censor_sp==0
replace current_rel_start = current_rel_start_year if current_rel_start==. & inlist(current_rel_start_if,0,1) // AND non-imputed
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & inlist(current_rel_start_if_sp,0,1)
replace current_rel_start = current_rel_start_year if current_rel_start==. & rel_start_est_flag==0 // then prio NON estimated (aka recorded not observed based on transitions)
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. &  rel_start_est_flag_sp==0
replace current_rel_start = current_rel_start_year if current_rel_start==. & in_partner_history==1 & in_partner_history_sp==0
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & in_partner_history==0 & in_partner_history_sp==1
replace current_rel_start = current_rel_start_year if current_rel_start==. & both_left_censored==1 & current_rel_start_year < current_rel_start_year_sp
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & both_left_censored==1 & current_rel_start_year_sp < current_rel_start_year
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year==first_couple_year // see like is THIS automatically creating left censoring? But I think first couple year isn't inherently problematic because it's the first year I observed coupled and some people observed prior
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp==first_couple_year
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year!=. & current_rel_start_year_sp==.
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp!=.
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year < current_rel_start_year_sp
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp < current_rel_start_year
replace current_rel_start = first_couple_year if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp==. // add this for posterity, but these people get dropped.

inspect current_rel_start current_rel_start_year current_rel_start_year_sp

gen both_start_missing = 0
replace both_start_missing = 1 if current_rel_start_year==. & current_rel_start_year_sp==. // flag if both start dates missing to add to exclusion sample
tab missing_rel_start missing_rel_start_sp, m // this covers those also with missing partner info

gen both_start_est = 0
replace both_start_est = 1 if rel_start_est_flag==1 & rel_start_est_flag_sp==1

tab both_start_missing both_left_censored, m

gen exclusion_couples = 0
replace exclusion_couples = 1 if both_start_missing==1 | both_left_censored==1
tab exclusion_couples,m 
tab current_rel_start exclusion_couples // does this address heaping now for 0s? YES
	// tab xw_memorig exclusion_couples, row

gen current_rel_end = .
replace current_rel_end = current_rel_end_year if rel_end_match==1
replace current_rel_end = current_rel_end_year if current_rel_end==. & rel_end_est_flag==0
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. &  rel_end_est_flag_sp==0
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year!=. & current_rel_end_year_sp==.
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp!=.
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year==last_couple_year
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp==last_couple_year
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year < current_rel_end_year_sp
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp < current_rel_end_year
replace current_rel_end = last_couple_year if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp==.

inspect current_rel_end current_rel_end_year current_rel_end_year_sp

gen both_end_missing = 0
replace both_end_missing = 1 if current_rel_end_year==. & current_rel_end_year_sp==.

browse pidp partner_id year marital_status_defacto ever_transition current_rel_start current_rel_end current_rel_start_year current_rel_end_year current_rel_start_year_sp current_rel_end_year_sp first_couple_year last_couple_year current_rel_left_censor current_rel_left_censor_sp

// Sense check of start dates now
gen start_match_couple = 0
replace start_match_couple = 1 if first_couple_year==current_rel_start

tab start_match_couple both_left_censored, m // so it's about 20% of total. about half are known left censor, 25% are def not left censor, 25% are unknown. but also, it's not crazy for your first couple year to match your start year right? wouldn't we actually expect that a lot? IF the rel started during the survey, I guess. is that it? i guess the implication is your relationship started before you entered the survey? i am now confusing myself with what to expect here...
tab current_rel_start start_match_couple,m row

gen start_match = 0
replace start_match = 1 if first_year_observed==current_rel_start

gen start_match_sp = 0
replace start_match_sp = 1 if first_year_observed_sp==current_rel_start

tab start_match start_match_sp, m  cell
tab start_match start_match_sp if exclusion_couples==0, m  cell // okay so this is actually very good. when I exclude known left censoring, 88% are that start date doesn't match first year in survey for either but 2% it matches for both. The 0/1s are less concerning bc of what i keep saying - some people probably only enter when they enter couple. 
tab start_match start_match_sp if both_left_censored==0, m  cell // and when i KNOW NO left censoring, still around 2% - meaning it's probably likely that those 2% are chance that both start year and survey start align? or...that 2% is error, which is also possible, but that feels tolerable given distribution of dates

**********************************************
* Now let's adjust transitioners
**********************************************
// FIRST, fix transition year (not sure I actually use later for filling in for imputation, but let's do for posterity.)
tab ever_transition ever_transition_sp, m // these don't match
tab transition_est transition_est_sp, m // nor do these...
tab marital_status_defacto marital_status_defacto_sp, m // do these? ALMOST (closer than above)
tab year_transitioned year_transitioned_sp, m // these also don't match

tab transition_est ever_transition, m

gen all_transitions = 0
replace all_transitions = 1 if transition_est==1 | ever_transition==1 // let's just create this for ease
replace all_transitions = . if transition_est==. & ever_transition==.

gen all_transitions_sp = 0
replace all_transitions_sp = 1 if transition_est_sp==1 | ever_transition_sp==1
replace all_transitions_sp = . if transition_est_sp==. & ever_transition_sp==.

tab all_transitions all_transitions_sp, m // this did NOT really change the congruence

// BUT can i update so based on real marriage year? Instead of needing to use the master start info, do I actually update based on current start year if married (because THOSE are based on history?) so yes, I need the real start date to match cohab start, but can I leverage this info if transitioned because might be more accurate?
tab current_rel_start if all_transitions == 1 & marital_status_defacto==1
tab current_rel_start year_transitioned if all_transitions == 1 & marital_status_defacto==1 // okay, these make sense and it's the same PSID problem that I have year transitioned as first year obseved, but the marriage could have happened after survye in prior year. so some of the current rel starts are in the year prior to my recorded one, which makes sense...

gen transition_yr_marr = .
replace transition_yr_marr = current_rel_start if all_transitions==1 & marital_status_defacto==1
bysort pidp partner_id (transition_yr_marr): replace transition_yr_marr = transition_yr_marr[1]

gen transition_yr_cohab = .
replace transition_yr_cohab = current_rel_start if all_transitions==1 & marital_status_defacto==2
bysort pidp partner_id (transition_yr_cohab): replace transition_yr_cohab = transition_yr_cohab[1]

	// tab transition_yr_marr transition_yr_cohab if all_transitions==1

gen cohab_year = int_year if all_transitions==1 & marital_status_defacto==2
bysort pidp partner_id: egen last_yr_cohab = max(cohab_year)

// based on troubleshooting, there are some anomalies here with people's marriage dates being recorded incorrectly or missing even if have observed status of married. this is so niche though, not 100% how to fix effectively - maybe can flag if transition year and / or last year cohab already match a marriage start date?
gen is_ty_marriage_start=0
forvalues m=1/14{
	replace is_ty_marriage_start = 1 if year_transitioned == mh_starty`m' & mh_status`m'==1 & year_transitioned!=.
}

gen is_lyc_marriage_start=0
forvalues m=1/14{
	replace is_lyc_marriage_start = 1 if last_yr_cohab == mh_starty`m' & mh_status`m'==1 & last_yr_cohab!=.
}

gen is_tym_marriage_start=0
forvalues m=1/14{
	replace is_tym_marriage_start = 1 if transition_yr_marr == mh_starty`m' & mh_status`m'==1 & transition_yr_marr!=.
}

gen est_marriage_date = .
forvalues m=1/14{
	replace est_marriage_date = mh_starty`m' if mh_starty`m' >= current_rel_start & mh_starty`m'<=current_rel_end & mh_status`m'==1
}


tab all_transitions is_ty_marriage_start, m
tab all_transitions is_lyc_marriage_start, m
tab all_transitions is_tym_marriage_start, m
tab all_transitions is_tym_marriage_start if in_partner_history==1, m

sort pidp partner_id year
browse pidp partner_id year int_year marital_status_defacto current_rel_start current_rel_end  all_transitions ever_transition marr_trans year_transitioned transition_yr_marr transition_yr_cohab last_yr_cohab is_ty_marriage_start is_lyc_marriage_start est_marriage_date is_tym_marriage_start in_partner_history mh_status1 mh_starty1 mh_status2 mh_starty2 mh_status3 mh_starty3

// FINALLY adjust dates
rename year_transitioned transition_year_unadj

gen year_transitioned = .
replace year_transitioned = transition_yr_marr if all_transitions==1 & in_partner_history == 1 & is_tym_marriage_start==1 // prio this because SHOULD be based on history
replace year_transitioned = transition_year_unadj if year_transitioned == . & all_transitions==1 & in_partner_history == 1 & is_ty_marriage_start==1 // then old date if matches a marriage date
replace year_transitioned = last_yr_cohab if year_transitioned == . & all_transitions==1 & in_partner_history == 1 & is_lyc_marriage_start==1 
replace year_transitioned = est_marriage_date if year_transitioned == . & all_transitions==1 & in_partner_history == 1 & est_marriage_date!=. 
bysort pidp partner_id (year_transitioned): replace year_transitioned=year_transitioned[1]
replace year_transitioned = transition_year_unadj if year_transitioned == . & all_transitions==1 & in_partner_history == 1 // last attempt
replace year_transitioned = transition_year_unadj if year_transitioned==. & all_transitions==1 & in_partner_history==0 // think use MINE if not in history

gen transition_est_flag = 0
replace transition_est_flag = 1 if year_transitioned==. & all_transitions==1 // in case I regret this later. though then I forget that i do these things and NEVER revisit...

replace year_transitioned = transition_yr_marr if year_transitioned == . & all_transitions==1 & in_partner_history == 1 & transition_yr_marr!=.

// one problem here - the people who disappear and come back won't have a transition_year_unadj so I guess those have to stay missing if they don't have history, let's see if this makes a huge deal. two options, really: we leave as missing OR use as observed. missing is safer? This is less of a problem in UK I think bc SO FEW don't have history.

tab year_transitioned ever_transition, m
tab transition_year_unadj ever_transition, m
tab year_transitioned all_transitions, m 
tab est_marriage_date if all_transitions==1 & year_transitioned==., m // remaining missing have missing start dates in marital history EVEN THOUGH in history
tab transition_year_unadj all_transitions, m

tab year_transitioned transition_year_unadj if all_transitions==1 // so yes it primarily moved some people up 1 year

sort pidp year

// NOW adjust other years
bysort pidp partner_id: egen rel_start_all = min(current_rel_start)
bysort pidp partner_id: egen rel_end_all = max(current_rel_end)
bysort pidp partner_id: egen status_all = max(current_rel_ongoing) if partnered==1

capture label define status 0 "ended" 1 "ongoing"
label values status_all status

inspect rel_start_all rel_end_all status_all

sort pidp year
browse pidp partner_id year marital_status_defacto ever_transition all_transitions marr_trans rel_start_all rel_end_all current_rel_start current_rel_end year_transitioned transition_year_unadj mh_status1 mh_starty1 mh_status2 mh_starty2 mh_status3 mh_starty3

********************************************************************************
**# Sample restrictions
********************************************************************************
// Restrictions based on relationship dates
* need to add duration variable
gen dur = int_year - rel_start_all
tab dur, m

* then, min and max duration
bysort pidp partner_id: egen min_dur = min(dur)
bysort pidp partner_id: egen max_dur = max(dur)

sort pidp year
browse pidp partner_id int_year marital_status_defacto rel_start_all rel_end_all status_all dur min_dur max_dur first_couple_year last_couple_year 

// unique pidp partner_id if rel_start_all >=1991 & rel_start_all<=2019 & inlist(min_dur,0,1), by(rel_start_all)

drop if rel_start_all==. // will get removed anyway (except I temporarily make this non-missing - drop below with exclusion couples)
keep if rel_start_all >= 1991 & inlist(min_dur,0,1) // using 1991 because that is first survey year
keep if rel_start_all <= 2019 // doing 2019 as some interviews in last wave were 2022 (so that is 3 years)
keep if exclusion_couples==0

tab rel_end_all, m // I adjusted this above, "both_end_missing" is the flag
tab status_all, m // about 2% missing so prob fine? if ongoing, update current_rel_end_year with last survey year? and call it attrited?
tab rel_end_all status_all, m // oh, the ongoing all already mostly have an end date of last survey year

browse pidp partner_id marital_status_defacto int_year rel_start_all rel_end_all status_all dur min_dur max_dur first_year_observed first_couple_year last_year_observed last_couple_year rel_end_year_est

// restrict to working age? maybe do this later so can use this info for imputing?
// tab age_all employed, row
// keep if (age_all>=18 & age_all<=60)

unique pidp
unique pidp partner_id 
egen couple_id = group(pidp partner_id)
// browse pidp partner_id couple_id
unique couple_id

********************************************************************************
**# Get list of eligible couples
********************************************************************************
preserve

collapse (first) rel_start_all rel_end_all status_all rel_no_est year_transitioned min_dur max_dur first_couple_year last_couple_year ///
(max) all_transitions current_rel_left_censor current_rel_left_censor_sp both_left_censored both_start_missing both_end_missing both_start_est both_imputed, by(pidp partner_id couple_id)

gen eligible_couple=1
rename couple_id eligible_couple_id
rename rel_start_all eligible_rel_start_year
rename rel_end_all eligible_rel_end_year
rename status_all eligible_rel_status
rename rel_no_est eligible_rel_no
rename both_left_censored eligible_rel_lc_flag
rename both_start_missing eligible_rel_miss_flag
rename both_start_est eligible_rel_est_flag
rename both_imputed eligible_rel_imp_flag
rename all_transitions eligible_transition_status
rename year_transitioned eligible_transition_year

gen long eligible_partner = partner_id 
by pidp: egen num_rel = count(partner_id) // this is how many relationships in this time frame they are contributing, so not quite the same

browse if num_rel > 1

save "$created_data/ukhls_couple_list.dta", replace

restore

********************************************************************************
* Now merge back on to data to create a filter for individuals
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

// some variables to create. because of the way I edited the file, this won't work BUT i can get this info in other file so I believe it's okay. also, these variables already restricted, and i create a different duration later
// gen dur = int_year - rel_start_all
// bysort pidp partner_id: egen min_dur = min(dur)
// bysort pidp partner_id: egen max_dur = max(dur)
// tab dur, m

merge m:1 pidp partner_id using "$created_data/ukhls_couple_list.dta", keepusing(num_rel eligible_couple eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_no eligible_rel_status eligible_partner eligible_rel_lc_flag eligible_rel_miss_flag eligible_rel_est_flag eligible_rel_imp_flag eligible_transition_status eligible_transition_year min_dur max_dur) // so I actually want to just merge on pidp because i want to keep them potentially even if not partnered anymore. BUT, can indicate which relationship is the eligible one, if multiple. okay, this won't work because some have multiple partners, so will merge for the specific couple, then create a MAX indicator of whether that person is ever eligible

drop if _merge==2
drop _merge

bysort pidp: egen ever_eligible = max(eligible_couple)
bysort pidp: egen max_eligible_rels = max(num_rel)
replace ever_eligible = 0 if ever_eligible==.
tab ever_eligible eligible_couple, m
tab partnered ever_eligible, m

sort pidp year
browse pidp partner_id int_year ever_eligible max_eligible_rels num_rel eligible_partner current_rel_start_year  eligible_rel_start_year  eligible_rel_end_year eligible_rel_status eligible_couple eligible_couple_id // if max_eligible_rels > 1 // rel_start_all rel_end_all

keep if ever_eligible==1

/*I am not sure why I do this because I do the below - I don't want to fill in with DIFFERENT relationshp info and that is what happens here
replace eligible_partner = partner_id if eligible_partner==. & partner_id !=.
replace eligible_rel_start_year = rel_start_all if eligible_rel_start_year==. & rel_start_all !=.
replace eligible_rel_end_year = rel_end_all if eligible_rel_end_year==. & rel_end_all !=.
replace eligible_rel_status = status_all if eligible_rel_status==. & status_all !=.
label values eligible_rel_status status
replace eligible_rel_no = rel_no_est if eligible_rel_no==. & rel_no_est !=.
*/

foreach var in eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no eligible_transition_status eligible_transition_year eligible_rel_lc_flag eligible_rel_miss_flag eligible_rel_est_flag eligible_rel_imp_flag eligible_couple_id min_dur max_dur{
	bysort pidp (`var'): replace `var' = `var'[1] if inlist(max_eligible_rels,0,1)
}

sort pidp year
// https://www.stata.com/support/faqs/data-management/replacing-missing-values/

foreach var in eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no eligible_transition_status eligible_transition_year eligible_rel_lc_flag eligible_rel_miss_flag eligible_rel_est_flag eligible_rel_imp_flag eligible_couple_id min_dur max_dur{
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
// assert eligible_rel_no!=.

sort pidp year
browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no max_eligible_rels // rel_start_all rel_end_all status_all  rel_no_est

gen post_marital_status=.
replace post_marital_status = marital_status_defacto if int_year > eligible_rel_end_year
label values post_marital_status marital_status_defacto

gen post_ended=.
replace post_ended = 1 if inlist(post_marital_status,3,4,5,6) // actually never married works as well bc that will be status for cohabitors
bysort pidp (post_ended): replace post_ended=post_ended[1]
sort pidp year

replace eligible_rel_status = 0 if post_ended==1 & eligible_rel_status==.
replace eligible_rel_status = 99 if eligible_rel_status==. & eligible_rel_end_year == last_year_observed // calling this "attrition"

// browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels if eligible_partner==.
// browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels if inlist(pidp,683763125, 687391802, 748014291, 749686407, 816866325)

browse pidp partner_id int_year marital_status_defacto post_marital_status post_ended eligible_partner eligible_rel_start_year eligible_rel_end_year max_eligible_rels first_year_observed last_year_observed if eligible_rel_status==. //  rel_start_all rel_end_all status_all 

egen couple_id = group(pidp eligible_partner)
unique couple_id
unique pidp couple_id
unique pidp partner_id
unique pidp eligible_partner

gen relative_duration_v0 = int_year - eligible_rel_start_year
tab relative_duration_v0, m

// fix the duplicate duration issue (identified below) - when people are interviewed twice in same year
bysort pidp couple_id: egen duplicate_dur = rank(year), unique  // because waves aren't in right order have to use year (proxy for wave)
	// browse pidp eligible_partner eligible_rel_start_year int_year year relative_duration_v0 dup_year_count duplicate_dur
sort pidp year

gen wave_distance = year - year[_n-1] if pidp == pidp[_n-1] & eligible_partner == eligible_partner[_n-1] // because waves aren't in right order

gen relative_duration = relative_duration_v0 if duplicate_dur==1
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur 
replace relative_duration = relative_duration[_n-1] + wave_distance if pidp == pidp[_n-1] & eligible_partner == eligible_partner[_n-1]  
browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur 
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur  if relative_duration==.
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur  if  inlist(pidp, 476653485, 479990245, 682201165)
replace relative_duration = relative_duration_v0 if relative_duration==.

// make sure stays okay here
bysort pidp partner_id: egen min_rel_dur = min(relative_duration) if partner_id!=. & partner_id==eligible_partner
bysort pidp partner_id: egen max_rel_dur = max(relative_duration) if partner_id!=. & partner_id==eligible_partner

sort pidp year
// browse pidp eligible_partner partner_id int_year eligible_rel_start_year relative_duration min_dur min_rel_dur max_dur max_rel_dur

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

// see if some variables are fixed or change
unique pidp 
unique pidp country_all // 19144 instead of 18551 so barely changes 
unique pidp gor_dv //  20809 so changes more if I get more specific
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

unique pidp hiqual_dv // some more movement here, but barely. 20985 v. 18551
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

// why is DOB not fixed?
quietly unique dob_year if dob_year!=., by(pidp) gen(dob_change)
bysort pidp (dob_change): replace dob_change=dob_change[1]
tab dob_change, m
	
rename dob_year dob_year_v0
bysort pidp: egen dob_year = min(dob_year_v0)
	
sort pidp year
browse pidp couple_id int_year dob_year age_all dob_change dob_year_v0

// need to figure out first birth year do I really need this? oh, i actually didn't even use in final imputation (see PSID files, I am dumb)
/* From codebook
Uses files W_CH1BY4 on data file W_INDRESP
W_LCHDOBY4 on datafile W_NATCHILD
W_MNPID WFNPID W_BIRTHY on W_INDALL
W_LCHBY4 on datafile W_NEWBORN
BW_CH1BY on datafile BW_INDRESP/
*/

// first see what missing data is like with the data that exists
misstable summarize jbhrs work_hours total_hours howlng any_aid aid_hours fimnlabgrs_dv employment_status employed hiqual_dv hiqual_fixed ever_parent nchild_dv nkids_dv age_youngest_child partnered marital_status_defacto fihhmngrs_dv npens_dv num_parents_hh xw_ethn_dv xw_racel_dv father_educ mother_educ father_empstatus mother_empstatus family_structure family_structure14_det country_all gor_dv tenure_dv housing_status_alt master_religion religion_est disabled_est sr_health dob_year year_first_birth current_rel_start_year eligible_rel_start_year eligible_rel_no xw_memorig xw_sampst respondent_self xw_sex, showzeros all

// partnered_imp marital_status_imp 

**********************************
// here, we finally rectangularize
unique pidp
unique pidp eligible_partner
unique pidp eligible_rel_start_year //   19619 /  24387 (new) / 19709 (with left censored removed)
unique couple_id // 19588 / 23886 (new) / 18881 (with left censored removed)

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
foreach var in pidp eligible_partner eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no eligible_transition_status eligible_transition_year eligible_rel_lc_flag eligible_rel_miss_flag eligible_rel_est_flag eligible_rel_imp_flag min_dur max_dur min_rel_dur max_rel_dur first_year_observed first_couple_year last_year_observed last_couple_year hiqual_fixed xw_ethn_dv xw_racel_dv dob_year year_first_birth eligible_rel_start_year xw_memorig xw_sampst xw_sex mh_status1 mh_status2 mh_status3 mh_status4 mh_status5 mh_status6 mh_status7 mh_status8 mh_status9 mh_status10 mh_status11 mh_status12 mh_status13 mh_status14 mh_starty1 mh_starty2 mh_starty3 mh_starty4 mh_starty5 mh_starty6 mh_starty7 mh_starty8 mh_starty9 mh_starty10 mh_starty11 mh_starty12 mh_starty13 mh_starty14 mh_endy1 mh_endy2 mh_endy3 mh_endy4 mh_endy5 mh_endy6 mh_endy7 mh_endy8 mh_endy9 mh_endy10 mh_endy11 mh_endy12 mh_endy13 mh_endy14 bh_pp bh_union1 bh_left1 bh_union2 bh_left2 bh_union3 bh_left3 bh_union4 bh_left4 bh_union5 bh_left5 bh_union6 bh_left6 bh_union7 bh_left7 bh_union8 bh_left8 bh_union9 bh_left9 bh_union10 bh_left10 bh_marital bh_cohabitation bh_starty1 bh_starty2 bh_starty3 bh_starty4 bh_starty5 bh_starty6 bh_starty7 bh_starty8 bh_starty9 bh_starty10 xw_sex xw_coh1m_dv xw_coh1y_dv xw_evercoh_dv xw_lmar1m_dv xw_lmar1y_dv xw_evermar_dv xw_ch1by_dv xw_anychild_dv num_bio_kids ever_parent father_educ mother_educ father_empstatus mother_empstatus family_structure family_structure14_det{
	bysort couple_id (`var'): replace `var'=`var'[1] if `var'==. 
}

// remove because got better version: ever_transition year_transitioned 

replace int_year = eligible_rel_start_year + relative_duration if int_year==.
replace age_all = int_year - dob_year if age_all==.

sort pidp int_year
browse pidp eligible_partner int_year age_all dob_year partnered partner_id eligible_rel_start_year eligible_rel_end_year eligible_rel_no relative_duration couple_id orig_record min_dur min_rel_dur max_dur max_rel_dur

// some variables to create now that it's filled in
* parental status based on year of first birth
browse pidp int_year ever_parent xw_ch1by_dv year_first_birth

gen current_parent_status=.
replace current_parent_status = 0 if ever_parent==0
replace current_parent_status = 0 if ever_parent==1 & int_year < year_first_birth & year_first_birth!=0
replace current_parent_status = 1 if ever_parent==1 & int_year >= year_first_birth & year_first_birth!=0

tab ever_parent current_parent_status,m 
tab year_first_birth current_parent_status, m

* first birth timing relative to relationship
gen birth_timing_rel = eligible_rel_start_year - year_first_birth if year_first_birth!=9999 & year_first_birth!=0
replace birth_timing_rel = 9999 if year_first_birth==9999

tab birth_timing_rel ever_parent, m

// fill in respondent info with non-sample year when missing
gen respondent_info = respondent_self
replace respondent_info = 2 if respondent_self==.

label define resp 0 "proxy" 1 "self" 2 "non-sample"
label values respondent_info resp

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

browse pidp eligible_partner int_year partnered_imp partnered marital_status_imp marital_status_defacto eligible_rel_start_year eligible_rel_end_year current_rel_start_year current_rel_end_year eligible_transition_status eligible_transition_year mh_*

inspect partnered_imp marital_status_imp

// now see the missing again
misstable summarize jbhrs work_hours total_hours howlng any_aid aid_hours fimnlabgrs_dv employment_status employed hiqual_dv hiqual_fixed ever_parent nchild_dv current_parent_status nkids_dv age_youngest_child partnered marital_status_defacto partnered_imp marital_status_imp fihhmngrs_dv npens_dv num_parents_hh xw_ethn_dv xw_racel_dv father_educ mother_educ father_empstatus mother_empstatus family_structure family_structure14_det country_all gor_dv tenure_dv housing_status_alt master_religion religion_est disabled_est sr_health dob_year year_first_birth birth_timing_rel current_rel_start_year eligible_rel_start_year eligible_rel_no xw_memorig xw_sampst respondent_self respondent_info xw_sex, showzeros all

save "$created_data/ukhls_couples_alldurs_long.dta", replace

