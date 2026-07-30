********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: identify_couple_sample.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* The file creates a unique list of couples to be included in our analysis
* It is to this list of couples we will merge on all survey information
* (This is modeled off of UKHLS file d)

* Eligibility
* Relationship started after 1990
* Had to observe relationship start (duration of 0 or 1)
* Start date not left censored or missing
* Relationship ended before 2020 (or 3 years from last interview date)
* Aged between 18 and 60 - do this later so can retain these for imputation years jic

// OUTPUT HERE: list of eligible IDS

********************************************************************************
* Create version of file for partner relationship variables to clean up start and end dates
********************************************************************************
// (Moving these steps up July 2026 to facilitate better cleaning up of start dates)
use "$created_data/ppathl_partnership_history.dta", replace 

local partnervars "partnered_pl current_rel_type current_rel_start_yr current_rel_end_yr current_rel_number firstyr_survey_pl lastyr_survey_pl current_rel_left_censored current_rel_start_miss start_yr_missing_flag  end_yr_missing_flag rel_start_est_flag rel_end_est_flag ever_transition transition_est transition_year full_status_pl in_couply in_couplm in_marsy in_any_rel_history"

keep pid syear `partnervars'

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	rename `var' `var'_sp
}

rename pid partner_id_pl

save "$temp/ppathl_partner_rel_info.dta", replace

********************************************************************************
* Attempt to clean up relationship start date using both partners' info
********************************************************************************
use "$created_data/ppathl_partnership_history.dta", replace // use this to just get a unique list of ids in couples (NOT by year) to THEN use with ppathl as base file to THEN merge on rest of info.

label values psample_pl psample

**********************************************
// first, restrict to couples
**********************************************
* need to figure out which values I should keep based on which partnered have ids
inspect partner_id_pl if partnered_pl==0 // all missing
inspect partner_id_pl if inrange(partnered_pl,1,2) // 2 missing
inspect partner_id_pl if inrange(partnered_pl,1,4) // 6 missing
inspect partner_id_pl if inrange(partnered_pl,1,5) 

forvalues p=1/5{
	display `p'
	inspect partner_id_pl if partnered_pl==`p' // so it's the not clear that are the problem
}

keep if inrange(partnered_pl,1,4)

tab current_rel_type partnered_pl, m // lol these...mostly...match. my cohab seems quite off...

browse pid syear partnered_pl current_rel_number current_rel_type current_rel_start_yr current_rel_end_yr partner_id_pl partner_id_rhm full_status_pl

// feel like, for these purposes, the drop out year is confusing, so let's drop that
drop if full_status_pl==0

// there are also some issues with first relationship date when the partner is always no int. let's try to fill in first couple year for those using couple_id
// browse pid partner_id_pl partnered_pl syear current_rel_type firstyr_survey_pl lastyr_survey_pl full_status_pl marr_trans first_couple_year last_couple_year 

gen long partner_1 = cond(pid < partner_id_pl, pid, partner_id_pl)
gen long partner_2 = cond(pid < partner_id_pl, partner_id_pl, pid)

egen long couple_id = group(partner_1 partner_2)

browse pid couple_id partner_id_pl syear partner_1  partner_2  partnered_pl syear current_rel_type firstyr_survey_pl lastyr_survey_pl full_status_pl first_couple_year last_couple_year 

bysort couple_id (first_couple_year): replace first_couple_year=first_couple_year[1]
bysort couple_id (last_couple_year): replace last_couple_year=last_couple_year[1]

sort pid syear

**********************************************
// then merge on partner info
**********************************************
merge m:1 partner_id_pl syear using "$temp/ppathl_partner_rel_info.dta" // has to be many because of the missing partner_id. wait but that's only 2? okay let's try this..
drop if _merge==2

tab _merge // so like 2% don't match
tab _merge if partner_id_pl!=. // and it is not just because of this
unique pid partner_id_pl, by(_merge) // like 7% from this standpoint

gen partner_match=0
replace partner_match=1 if _merge==3

drop _merge

**********************************************
// Now, let's clean up dates between partners
**********************************************
// this will make it easier to use code I know works
rename current_rel_start_yr current_rel_start_year
rename current_rel_start_yr_sp current_rel_start_year_sp
rename current_rel_end_yr current_rel_end_year
rename current_rel_end_yr_sp current_rel_end_year_sp

browse pid partner_id_pl syear partnered_pl partnered_pl_sp current_rel_start_year current_rel_start_year_sp current_rel_left_censored current_rel_left_censored_sp start_yr_missing_flag start_yr_missing_flag_sp rel_start_est_flag rel_start_est_flag_sp current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year

tab partnered_pl partnered_pl_sp, m
tab partnered_pl partnered_pl_sp if partner_match==1, m // basically congruent
tab current_rel_left_censored current_rel_left_censored_sp, m
tab start_yr_missing_flag start_yr_missing_flag_sp, m
tab rel_start_est_flag rel_start_est_flag_sp, m // very few

// flag joint things
gen both_left_censored = .
replace both_left_censored = 0 if current_rel_left_censored==0 | current_rel_left_censored_sp==0 // as long as ONE is zero, it's okay. sometimes there are missing and 0, but I can use the 0s. It is more like if one is 1 and missing, that isn't helpful
replace both_left_censored = 1 if current_rel_left_censored==1 & current_rel_left_censored_sp==1

gen both_start_missing = 0
replace both_start_missing = 1 if current_rel_start_year==. & current_rel_start_year_sp==.
replace both_start_missing = 1 if current_rel_start_miss==1 & current_rel_start_miss_sp==1 // also to note, YES these couples were ALREADY DROPPED. Actually use this version instead of flag above because some partner non-matches will have start year missing here

	// frm below, confirm matches when partner ID matches
	gen either_start_missing=0
	replace either_start_missing = 1 if start_yr_missing_flag==1 | current_rel_start_miss==1

	gen either_start_missing_sp=0
	replace either_start_missing_sp = 1 if start_yr_missing_flag_sp==1 | current_rel_start_miss_sp==1
	
	tab both_start_missing if partner_match==1, m
	tab either_start_missing either_start_missing_sp if partner_match==1, m
	tab both_start_missing if either_start_missing==1 & either_start_missing_sp==1, m
	replace both_start_missing = 1 if either_start_missing==1 & either_start_missing_sp==1 & both_start_missing!=1

gen both_start_est = .
replace both_start_est = 0 if rel_start_est_flag==0 | rel_start_est_flag_sp==0
replace both_start_est = 1 if rel_start_est_flag==1 & rel_start_est_flag_sp==1

tab both_left_censored both_start_missing, m

// flag if matched
gen rel_start_match = .
replace rel_start_match = 0 if current_rel_start_year!=current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.
replace rel_start_match = 1 if current_rel_start_year==current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.

gen rel_end_match = .
replace rel_end_match = 0 if current_rel_end_year!=current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.
replace rel_end_match = 1 if current_rel_end_year==current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.

tab rel_start_match, m
tab rel_end_match, m

// some of the below matches may be because both left censored. how many are recoverable? this info actually needs to be prioritzed FIRST
tab current_rel_left_censored current_rel_left_censored_sp, m cell // okay it's actually quite low relatively
tab current_rel_left_censored either_start_missing, m
tab both_left_censored rel_start_match, m // so it often matches if this is true, but sometimes one is earlier. I end up dropping but let's use earlier one for now

sort pid syear
browse pid partner_id_pl syear partnered_pl in_couply in_couply_sp current_rel_start_year current_rel_start_year_sp current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year current_rel_left_censored current_rel_left_censored_sp start_yr_missing_flag start_yr_missing_flag_sp rel_start_est_flag rel_start_est_flag_sp if rel_start_match==0 | rel_end_match==0 

tab in_couply in_couply_sp // should I prio based on any of these?? this is one with a. least coverage and b. most differences
tab in_couplm in_couplm_sp // almost
tab in_marsy in_marsy // perfectly congruent 

// align dates
gen current_rel_start = .
inspect current_rel_start
replace current_rel_start = current_rel_start_year if rel_start_match==1 // the problem here is they can match and BOTH be left censored, but none of the rest of the code can solve for that, so I think I just have to fill in as is and then just drop if both left censored
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_left_censored==0 // newly added code (when left censored = 0, the missing flags are also all 0, so this works)
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_left_censored_sp==0 // newly added code
replace current_rel_start = current_rel_start_year if current_rel_start==. & either_start_missing==0 & either_start_missing_sp==1 // so use r if spouse is missing
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & either_start_missing==1 & either_start_missing_sp==0 & current_rel_start_year_sp!=. // use sp if r is missing	
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year!=. & current_rel_start_year_sp==. // should have been captured with above, but let's keep anyway // I think this adds some because of the partner match actually
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp!=. // should have been captured with above, but let's keep anyway
replace current_rel_start = current_rel_start_year if current_rel_start==. & rel_start_est_flag==0 // then prio NON estimated (aka recorded not observed based on transitions)
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. &  rel_start_est_flag_sp==0
replace current_rel_start = current_rel_start_year if current_rel_start==. & both_left_censored==1 & current_rel_start_year < current_rel_start_year_sp
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & both_left_censored==1 & current_rel_start_year_sp < current_rel_start_year
replace current_rel_start = current_rel_start_year if current_rel_start==. & in_couply==1 & in_couply_sp==0 // okay prio based on this next
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & in_couply==0 & in_couply_sp==1
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year==first_couple_year
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp==first_couple_year
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year < current_rel_start_year_sp
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp < current_rel_start_year // after this step, the people left are people where neither partner has a start date and those that are identified with left censor variable are left censored. aka all of these people should be excluded. I assign then a date for posterity in next step, but they are removed later with filters.
replace current_rel_start = first_couple_year if current_rel_start==. & both_start_missing==1 // how many people is this? about 7600 (1.3%)

tab both_start_missing both_left_censored, m // core problem is that didn't solve the WRITTEN IN LEFT CENSORING, which is a much larger problem. So the people with both starting missing are always both left censored, which makes snese. HOWEVER some have start dates and are also left censored. THOSE NEED TO BE EXCLUDED.

gen exclusion_couples = 0
replace exclusion_couples = 1 if both_start_missing==1 | both_left_censored==1
tab exclusion_couples,m 
	// tab psample_pl exclusion_couples, row // primarily but not solely refugees

inspect current_rel_start current_rel_start_year current_rel_start_year_sp
tab current_rel_start exclusion_couples, m

///
	
gen current_rel_end = .
inspect current_rel_end
replace current_rel_end = current_rel_end_year if rel_end_match==1
replace current_rel_end = current_rel_end_year if current_rel_end==. & rel_end_est_flag==0
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. &  rel_end_est_flag_sp==0
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year!=. & current_rel_end_year_sp==.
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp!=.
replace current_rel_end = current_rel_end_year if current_rel_end==. & in_couply==1 & in_couply_sp==0 // okay prio based on this next
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & in_couply==0 & in_couply_sp==1
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year==last_couple_year
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp==last_couple_year
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year < current_rel_end_year_sp
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp < current_rel_end_year
replace current_rel_end = last_couple_year if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp==.

gen both_end_missing = 0
replace both_end_missing = 1 if current_rel_end_year==. & current_rel_end_year_sp==. // flag if both end dates missing so using last couple_year

inspect current_rel_end current_rel_end_year current_rel_end_year_sp

browse pid partner_id_pl syear partnered_pl current_rel_start current_rel_start_year current_rel_start_year_sp current_rel_left_censored current_rel_left_censored_sp  either_start_missing either_start_missing_sp current_rel_end current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year

tab current_rel_start, m
tab current_rel_start exclusion_couples, m
tab current_rel_start partnered_pl if exclusion_couples==0, m

// is this a way to check left censoring in addition to above?
gen start_match = 0
replace start_match = 1 if firstyr_survey_pl==current_rel_start

gen start_match_sp = 0
replace start_match_sp = 1 if firstyr_survey_pl_sp==current_rel_start

tab start_match start_match_sp, m  cell
tab start_match start_match_sp if exclusion_couples==0, m  cell // okay so i think this affirms this is a good check. Here, the both match is about 1%
tab start_match start_match_sp if exclusion_couples==1, m  cell // HERE - the both match is 77%. so that is a good RED FLAG INDICATOR

**********************************************
* Now let's adjust transitioners
**********************************************
** 0. Real first step: I drop relationship history info to make file smaller, so need to add on to do the below.
merge m:1 pid using "$created_data/consolidated_rel_history.dta"
drop if _merge==2
drop _merge

// also it's going to behoove me to have a better variable (use this to match others)
tab partnered_pl, m

gen marst_defacto = .
replace marst_defacto = 1 if inlist(partnered_pl, 1,3)
replace marst_defacto = 2 if inlist(partnered_pl, 2,4)

label define marst 1 "Married" 2 "Partnered"
label values marst_defacto marst

tab partnered_pl marst_defacto
tab marst_defacto current_rel_type, m

// AND to make the rel type in the history more sensical... (aka need cohab to be 2 and married to be 1 in the rel_history, currently the opposite...)
label drop rel_type
label define rel_type 1 "married" 2 "cohab"

forvalues m=1/10{
	recode master_rel_type`m' (1=2)(2=1)
	label values master_rel_type`m' rel_type
}

recode current_rel_type (1=2)(2=1)
recode current_rel_type_sp (1=2)(2=1)

label values current_rel_type current_rel_type_sp rel_type

tab marst_defacto current_rel_type, m // ensure still makes sense

** 1. FIRST, fix transition year (I don't actually use later for filling in for imputation, but let's do for posterity.)
// I check alignment, but i use current rel start info for the most part, which I just aligned above
tab ever_transition ever_transition_sp, m // these match
tab transition_est transition_est_sp, m // these do not....but I think the above is more comprehensive (because of the way i do Germany with the gaps, this adds less than in other surveys because they also attempt to fill in relationship details in off years)
tab ever_transition transition_est, m
tab partnered_pl partnered_pl_sp, m
tab transition_year transition_year_sp, m // these match

gen all_transitions = 0
replace all_transitions = 1 if transition_est==1 | ever_transition==1 // let's just create this for ease

gen all_transitions_sp = 0
replace all_transitions_sp = 1 if transition_est_sp==1 | ever_transition_sp==1

tab all_transitions all_transitions_sp, m // so yeah, this is basically congruent

// now ensure matches relationship year
gen transition_yr_marr = .
replace transition_yr_marr = current_rel_start if all_transitions==1 & marst_defacto==1
bysort pid partner_id_pl (transition_yr_marr): replace transition_yr_marr = transition_yr_marr[1]

gen transition_yr_cohab = .
replace transition_yr_cohab = current_rel_start if all_transitions==1 & marst_defacto==2
bysort pid partner_id_pl (transition_yr_cohab): replace transition_yr_cohab = transition_yr_cohab[1]

	// tab transition_yr_marr transition_yr_cohab if all_transitions==1

gen cohab_year = syear if all_transitions==1 & marst_defacto==2
bysort pid partner_id_pl: egen last_yr_cohab = max(cohab_year)

// based on troubleshooting, there are some anomalies here with people's marriage dates being recorded incorrectly or missing even if have observed status of married. this is so niche though, not 100% how to fix effectively - maybe can flag if transition year and / or last year cohab already match a marriage start date?
gen is_ty_marriage_start=0
forvalues m=1/10{
	replace is_ty_marriage_start = 1 if transition_year == master_start_yr`m' & master_rel_type`m'==1 & transition_year!=.
}

gen is_lyc_marriage_start=0
forvalues m=1/10{
	replace is_lyc_marriage_start = 1 if last_yr_cohab == master_start_yr`m' & master_rel_type`m'==1 & last_yr_cohab!=.
}

gen is_tym_marriage_start=0
forvalues m=1/10{
	replace is_tym_marriage_start = 1 if transition_yr_marr == master_start_yr`m' & master_rel_type`m'==1 & transition_yr_marr!=.
}

gen est_marriage_date = .
forvalues m=1/10{
	replace est_marriage_date = master_start_yr`m' if master_start_yr`m' >= current_rel_start & master_start_yr`m'<=current_rel_end & master_rel_type`m'==1
}


tab all_transitions is_ty_marriage_start, m
tab all_transitions is_lyc_marriage_start, m
tab all_transitions is_tym_marriage_start, m

sort pid partner_id_pl syear
browse pid partner_id_pl syear marst_defacto current_rel_start current_rel_end  all_transitions ever_transition marr_trans transition_year transition_yr_marr transition_yr_cohab last_yr_cohab is_ty_marriage_start is_lyc_marriage_start est_marriage_date is_tym_marriage_start master_rel_type1 master_start_yr1 master_rel_type2 master_start_yr2 master_rel_type3 master_start_yr3

// FINALLY adjust dates
rename transition_year transition_year_unadj

gen year_transitioned = .
replace year_transitioned = transition_yr_marr if all_transitions==1 & is_tym_marriage_start==1 // prio this because SHOULD be based on history
replace year_transitioned = transition_year_unadj if year_transitioned == . & all_transitions==1 & is_ty_marriage_start==1 // then old date if matches a marriage date
replace year_transitioned = last_yr_cohab if year_transitioned == . & all_transitions==1 &  is_lyc_marriage_start==1 
replace year_transitioned = est_marriage_date if year_transitioned == . & all_transitions==1 & est_marriage_date!=. 
bysort pid partner_id_pl (year_transitioned): replace year_transitioned=year_transitioned[1]

replace year_transitioned = transition_year_unadj if year_transitioned == . & all_transitions==1  // last attempt BUT this only captures those with NO GAPS (consecutive waves and transition happened between). the below final step - and why I flag - are those with gaps and I attempt to rely on marital history, but not always great

gen transition_est_flag = 0
replace transition_est_flag = 1 if year_transitioned==. & all_transitions==1 // in case I regret this later. though then I forget that i do these things and NEVER revisit...but it's also 26 people so I think it's okay...

replace year_transitioned = transition_yr_marr if year_transitioned == . & all_transitions==1 & transition_yr_marr!=.

tab year_transitioned ever_transition, m
tab transition_year_unadj ever_transition, m
tab year_transitioned all_transitions, m
tab transition_year_unadj all_transitions, m

tab year_transitioned transition_year_unadj if all_transitions==1 // so yes it primarily moved some people up 1 year (as expected)

**2. NOW adjust other years
bysort pid partner_id_pl: egen rel_start_all = min(current_rel_start)
bysort pid partner_id_pl: egen rel_end_all = max(current_rel_end)

sort pid syear
browse pid partner_id_pl syear marst_defacto partnered_pl all_transitions marr_trans rel_start_all rel_end_all current_rel_start current_rel_end year_transitioned transition_year_unadj current_rel_number

inspect rel_start_all rel_end_all current_rel_start current_rel_end

// can I update rel number info as well?
bysort pid partner_id_pl: egen rel_number_all = min(current_rel_number) if partner_id_pl!=. // think it is as simple as this? I just use the first rel number because it's a continuous relationship from that point?

sort pid syear
inspect rel_number_all current_rel_number // no missing
tab  rel_number_all current_rel_number // behaves as expected (some alls are earlier than current)

// probably need to also update "how end" - because we want info from the LAST partnership not the first (like cohab will probably say break up but married will say intact)
tab marst_defacto current_rel_how_end // yeah like is the breakup rate here high??

browse couple_id pid partner_id_pl marst_defacto partnered_pl syear rel_number_all current_rel_type current_rel_how_end all_transitions rel_start_all rel_end_all current_rel_start current_rel_end full_status_pl

bysort pid partner_id_pl: egen status_all_check = min(current_rel_how_end) // since intact is 0, this actually should work? though, works less well for widowed, but then I think we just use it like in UK - 0 is intact, all others are ended?
label values status_all_check how_end 

tab status_all_check current_rel_how_end if all_transitions==0
tab status_all_check current_rel_how_end if all_transitions==1

// replace current_rel_how_end = current_rel_how_end[_n-1] if current_rel_how_end==. & syear==last_couple_year & pid==pid[_n-1] & partner_id_pl==partner_id_pl[_n-1]

gen status_all = current_rel_how_end if syear==rel_end_all
replace status_all = current_rel_how_end if status_all==. & syear==last_couple_year
replace status_all = current_rel_how_end if status_all==. & syear==first_couple_year & all_transitions==0
replace status_all = current_rel_how_end if status_all==. & syear==first_couple_year & all_transitions==1 & current_rel_how_end==0 // fine if it is intact because then that is most likely. I just don't want it to be break up
replace status_all = current_rel_how_end if status_all==. & syear==year_transitioned+1 & all_transitions==1 & year_transitioned!=.
bysort pid partner_id_pl (status_all): replace status_all = status_all[1]

label values status_all how_end 

tab status_all, m
tab status_all status_all_check, m // honestly this got very close
tab current_rel_how_end, m
tab status_all current_rel_how_end, m
tab status_all_check current_rel_how_end, m

sort couple_id pid syear
browse couple_id pid partner_id_pl partnered_pl syear last_couple_year status_all current_rel_how_end all_transitions rel_start_all rel_end_all current_rel_start current_rel_end // for many of these it is like one missing year at the end

drop master_* // I add all of this on again in step 3, so let's remove (will also make this file smaller again)

// let's save this just in case
save "$temp/ppathl_partner_match_cleaned.dta", replace

********************************************************************************
**# Now we finally can do other sample restrictions
********************************************************************************
* Relationship started after 1990 - done
* Had to observe relationship start (duration of 0 or 1) - done
* Start can't be left censored or missing - added July 2026 (well missing was already here, better adjusted for built-in left censoring now)
* Relationship ended before 2020 (or 3 years from last interview date) - done

gen dur = syear - rel_start_all
bysort pid partner_id_pl: egen min_dur = min(dur)
bysort pid partner_id_pl: egen max_dur = max(dur)

tab min_dur, m // okay, only like 34% observed at 0 -- is this crazy?
// I am getting confused - if ONE PARTNER observed earlier - is that fine? or do they both need to be observed? like should we impute the missing info?
// like I am wondering if I have to go back to the other file and do these restrictions?
gen min_dur_alt = rel_start_all - firstyr_survey_pl // here we would want 0 and above right? because if rel started in like 1950 and first survey in 1984, this will be NEGATIVE -34, so that is only 12%?
gen min_dur_alt_sp = rel_start_all - firstyr_survey_pl_sp 

sort pid syear
browse pid syear dur min_dur min_dur_alt min_dur_alt_sp max_dur rel_start_all rel_end_all firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl  // let's make sure that, by restricting to partners / dropping dropouts - I didn't get super misaligned to the survey status info - before I create the realtionship duration / min dur variables
// oh duh I am dumb - if didn't enter survey partnered, then this info will not necessarily match?
// okay, the min dur info is actually better anyway, I think it's because of the way GSOEP fills in info it's okay

unique pid partner_id_pl if rel_start_all >= 1990 & inlist(min_dur,0,1) & rel_start_all <=2020
unique couple_id if rel_start_all >= 1990 & inlist(min_dur,0,1) & rel_start_all <=2020 // okay well this is still a lot of people, so actually fine??
unique pid partner_id_pl if rel_start_all >= 1990 & inlist(min_dur,0,1) & rel_start_all <=2020 & exclusion_couples==0
unique couple_id if rel_start_all >= 1990 & inlist(min_dur,0,1) & rel_start_all <=2020 & exclusion_couples==0

********************************
* Actual restrictions
********************************
drop if rel_start_all==.
keep if rel_start_all >= 1990 & inlist(min_dur,0,1) // keeping up to one in case marriage recorded after survey in year prior
keep if rel_start_all <=2020 // now will be 2020 because updated to 2023 (and assume 1st year of full data is 2021, so that's three years)
keep if exclusion_couples==0

* Also - we are certain of relationship start date (so if both partners were missing / estimated - probably drop?) let's do this after I do the above
tab either_start_missing either_start_missing_sp, m
unique couple_id
unique couple_id if either_start_missing==1 & either_start_missing_sp==1

drop if either_start_missing==1 & either_start_missing_sp==1  // should now be taken care of above, but let's leave (confirmed deleted 0 at this point)

tab rel_end_all, m // I adjusted this above, "both_end_missing" is the flag (<1%)
tab status_all, m // <1% missing so prob fine?
tab rel_end_all status_all, m // the ongoing all already mostly have an end date of last survey year

tab both_left_censored, m // no 1s
tab both_start_missing, m // gone
tab both_start_est, m // this is 71 people
tab both_end_missing, m // <1 %

********************************************************************************
**# Create list of individuals in eligible couples to match on to main file
********************************************************************************
// confirm this info is truly unique
unique pid partner_id_pl 
unique pid partner_id_pl couple_id  rel_start_all rel_end_all status_all rel_number_all year_transitioned min_dur max_dur first_couple_year last_couple_year all_transitions //  current_rel_left_censored current_rel_left_censored_sp both_left_censored both_start_missing both_end_missing both_start_est // do max for the latter

preserve

collapse (first) rel_start_all rel_end_all status_all rel_number_all year_transitioned min_dur max_dur first_couple_year last_couple_year ///
(max) all_transitions current_rel_left_censored current_rel_left_censored_sp both_left_censored both_start_missing both_end_missing both_start_est ///
, by(pid partner_id_pl couple_id)

label values status_all how_end

gen eligible_couple=1
rename couple_id eligible_couple_id
rename rel_start_all eligible_rel_start_year
rename rel_end_all eligible_rel_end_year
rename status_all eligible_rel_status
rename rel_number_all eligible_rel_no
rename both_left_censored eligible_rel_lc_flag
rename both_start_missing eligible_rel_miss_flag
rename both_start_est eligible_rel_est_flag
rename all_transitions eligible_transition_status
rename year_transitioned eligible_transition_year

gen long eligible_partner = partner_id_pl
by pid: egen num_rel = count(partner_id_pl) // this is how many relationships in this time frame they are contributing, so not quite the same as relationship order

browse if num_rel > 1
tab eligible_transition_year eligible_transition_status, m

save "$created_data/gsoep_couple_list.dta", replace

restore