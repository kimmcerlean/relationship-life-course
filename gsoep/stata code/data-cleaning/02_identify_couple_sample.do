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
* Relationship ended before 2020 (or 3 years from last interview date)
* Aged between 18 and 60 - do this later so can retain these for imputation years jic


********************************************************************************
* Create list of individuals in eligible couples to match on to main file
********************************************************************************
// use "$temp/pgen_cleaned.dta", clear // just using the pgen file because that should have all I need for the moment to identify couples (I don't want superfluous variables atm) - using pgen ensures I am restricting to couples who actually completed the survey
use "$created_data/ppathl_partnership_history.dta", replace // alternatively, could use this new file i have created? or do I first use pgen, then merge to this file in order to create base dataset restricted just to durations -2 to 12 on which to merge on the variables I then will recode?
// this is going to drop the durations not in sample anyway because won't be "partnered" so going to do what i did with UKHLS - use this to just get a unique list of ids in couples (NOT by year) to THEN use with ppathl as base file to THEN merge on rest of info.
// OUTPUT HERE: list of eligible IDS

// there are definitely people listed as partnered in this file that I don't have recorded as partnered. So, should I use my normal relationship start / end indicators as a backup? need to do this before I drop non-partnered people (So have the non-partnered year to observe a transition)

inspect current_rel_start_yr if partner_id_pl!=. & partner_id_pl!=.n & full_status_pl!=0 // about 5% (the dropout years I still need to sort out...bc with those, it's more like 13%
browse pid syear partnered_pl current_rel_number current_rel_type current_rel_start_yr current_rel_end_yr partner_id_pl partner_id_rhm full_status_pl

sort pid syear

*enter
gen rel_start=0
replace rel_start=1 if (inrange(partnered_pl,1,4) & partnered_pl[_n-1]==0) & pid==pid[_n-1] & syear==syear[_n-1]+1

*exit
gen rel_end=0
replace rel_end=1 if (partnered_pl==0 & inrange(partnered_pl[_n-1],1,4)) & pid==pid[_n-1] & syear==syear[_n-1]+1

gen rel_end_pre=0
replace rel_end_pre=1 if (inrange(partnered_pl,1,4) & partnered_pl[_n+1]==0) & pid==pid[_n+1] & syear==syear[_n+1]-1

*cohab to marr
gen marr_trans=0
replace marr_trans=1 if (inlist(partnered_pl,1,3) & inlist(partnered_pl[_n-1],2,4)) & pid==pid[_n-1] & partner_id_pl==partner_id_pl[_n-1] & syear==syear[_n-1]+1

* then create indicator of start date
gen current_rel_start_est = syear if rel_start==1
bysort pid partner_id_pl (current_rel_start_est): replace current_rel_start_est=current_rel_start_est[1] if partner_id_pl!=.

gen current_rel_end_est = syear if rel_end_pre==1
bysort pid partner_id_pl (current_rel_end_est): replace current_rel_end_est=current_rel_end_est[1]  if partner_id_pl!=. 

gen transition_year = syear if marr_trans==1
bysort pid partner_id_pl (transition_year): replace transition_year=transition_year[1]  if partner_id_pl!=. 

bysort pid partner_id_pl: egen ever_transition = max(marr_trans) if inrange(partnered_pl,1,4)

sort pid syear

gen current_rel_start_yr_v0 = current_rel_start_yr // I like to retain original copies
gen current_rel_end_yr_v0 = current_rel_end_yr // I like to retain original copies

replace current_rel_start_yr = current_rel_start_est if current_rel_start_yr==. & current_rel_start_est!=. & partner_id_pl!=. & syear>=current_rel_start_est
replace current_rel_end_yr = current_rel_end_est if current_rel_end_yr==. & current_rel_end_est!=. & partner_id_pl!=. & syear<=current_rel_end_est

// first, restrict to couples
* need to figure out which values I should keep based on which partnered have ids
inspect partner_id_pl if partnered_pl==0 // all missing
inspect partner_id_pl if inrange(partnered_pl,1,2) // 2 missing
inspect partner_id_pl if inrange(partnered_pl,1,5) 

forvalues p=1/5{
	display `p'
	inspect partner_id_pl if partnered_pl==`p' // so it's the not clear that are the problem
}
inspect partner_id_pl if inrange(partnered_pl,1,4)

keep if inrange(partnered_pl,1,4)

tab current_rel_type partnered_pl, m // lol these...mostly...match. my cohab seems quite off...

browse pid syear partnered_pl current_rel_number current_rel_type current_rel_start_yr current_rel_end_yr partner_id_pl partner_id_rhm full_status_pl

// feel like, for these purposes, the drop out year is confusing, so let's drop that
drop if full_status_pl==0

// let's try to group couples using what i learned in time use workshop to try to see if the info is congruent or not (before I actually match them)
	// in the UKHLS, I seem to do the relationship history cleanup before I match couples...

gen couple_id = cond(pid < partner_id_pl, pid, partner_id_pl) 
browse couple_id pid partner_id_pl syear // okay yes, this is my worry - see id 601, for example. they have multiple different partners throughout and the id for all of them is now 601, but that isn't what we want.

// think I need to do the detailed way from the Cox article - so first get the partners THEN group these?
gen partner_1 = cond(pid < partner_id_pl, pid, partner_id_pl)
gen partner_2 = cond(pid < partner_id_pl, partner_id_pl, pid)

drop couple_id
egen couple_id = group(partner_1 partner_2)

browse pid couple_id partner_id_pl syear partner_1  partner_2

sort couple_id pid syear
browse couple_id pid partner_id_pl partnered_pl syear current_rel_type current_rel_start_yr current_rel_end_yr full_status_pl // couple ID 7 good example of where info doesn't match

// think I actually want to do this first: clean up info to account for transitions between marriage and cohab 
// there are some people with discontinuous relationship histories though and I am not 100% sure how to account for that...(see couple id 13) -- hard to use rel order in these cases because that isn't perfect with the cohab/ marriage
// okay 13 is actually a bad / good example because one of the partners continues this to be one relationship and the other does not...SO in the absence of no othr info, I guess just treat as continuous relationship

	// this is also becoming problematic with missing ESPECIALLY if only missing for one chunk (either cohab or marriage)
	// so intermission to actually do this and possibly fill in this info as well? I do this for other surveys so I think it is fine. just need to make a flag to use for later - that this isn't REAL start date (also did this for UKHLS)
	bysort pid partner_id_pl: egen first_couple_year = min(syear) if partner_id_pl!=.
	bysort pid partner_id_pl: egen last_couple_year = max(syear) if partner_id_pl!=.
	sort couple_id pid syear
	browse couple_id pid partner_id_pl partnered_pl syear current_rel_type current_rel_start_yr current_rel_end_yr full_status_pl first_couple_year last_couple_year
	
	gen start_yr_missing_flag = 0
	replace start_yr_missing_flag = 1 if current_rel_start_yr==.
	
	// are these congruent?
	tab current_rel_start_miss start_yr_missing_flag, m // lol NOT AT ALL. I *think* this is because I then filled this info in? so these are two different forms of missing - one was taken care of in previous step
	tab current_rel_start_miss couplm_rel1_miss, m // THIS overlap of 1s I believe is what is most important. I guess that is essentially just current_rel_start_miss
	tab current_rel_number if current_rel_start_miss==1 & couplm_rel1_miss==1, m // because that aligns with this as well
	tab couplm_rel1_miss start_yr_missing_flag, m
	
	browse pid syear current_rel_number current_rel_start_yr current_rel_start_yr_v0 first_couple_year current_rel_start_est start_yr_missing_flag current_rel_start_miss couplm_rel1_miss
	
	gen end_yr_missing_flag = 0
	replace end_yr_missing_flag = 1 if current_rel_end_yr==.
	
	replace current_rel_start_yr = first_couple_year if current_rel_start_yr==. & first_couple_year!=. & partner_id_pl!=. & syear>=first_couple_year
	replace current_rel_end_yr = last_couple_year if current_rel_end_yr==. & last_couple_year!=. & partner_id_pl!=. & syear<=last_couple_year
	
bysort pid partner_id_pl: egen rel_start_all = min(current_rel_start_yr) if partner_id_pl!=.
bysort pid partner_id_pl: egen rel_end_all = max(current_rel_end_yr) if partner_id_pl!=.

sort couple_id pid syear
browse couple_id pid partner_id_pl partnered_pl syear current_rel_number current_rel_type ever_transition rel_start_all rel_end_all current_rel_start_yr current_rel_end_yr full_status_pl

// can I update rel number info as well?
bysort pid partner_id_pl: egen rel_number_all = min(current_rel_number) if partner_id_pl!=. // think it is as simple as this? I just use the first rel number because it's a continuous relationship from that point?
// one problem is that SOMETIMES that info is missing. this is primarily problematic if cohab is missing but not marriage - because marriage might be incremented one too many? or if cohab is just missing as a relationship then the marriage number should be applied to cohab and is therefore right? I think this is another like do the best we can with what we have
sort couple_id pid syear

// probably need to also update "how end" - because we want info from the LAST partnership not the first (like cohab will probably say break up but married will say intact)
browse couple_id pid partner_id_pl partnered_pl syear current_rel_number current_rel_type current_rel_how_end ever_transition rel_start_all rel_end_all current_rel_start_yr current_rel_end_yr full_status_pl

replace current_rel_how_end = current_rel_how_end[_n-1] if current_rel_how_end==. & syear==last_couple_year & pid==pid[_n-1] & partner_id_pl==partner_id_pl[_n-1]

gen master_rel_end = current_rel_how_end if syear==rel_end_all
replace master_rel_end = current_rel_how_end if master_rel_end==. & syear==last_couple_year
replace master_rel_end = current_rel_how_end if master_rel_end==. & syear==first_couple_year & ever_transition==0
replace master_rel_end = current_rel_how_end if master_rel_end==. & syear==first_couple_year & ever_transition==1 & current_rel_how_end==0 // fine if it is intact because then that is most likely. I just don't want it to be break up
replace master_rel_end = current_rel_how_end if master_rel_end==. & syear==transition_year+1 & ever_transition==1 & transition_year!=.
bysort pid partner_id_pl (master_rel_end): replace master_rel_end = master_rel_end[1]

label values master_rel_end how_end 

tab master_rel_end, m
tab current_rel_how_end, m
tab master_rel_end current_rel_how_end, m

sort couple_id pid syear
browse couple_id pid partner_id_pl partnered_pl syear last_couple_year master_rel_end current_rel_how_end ever_transition rel_start_all rel_end_all // for many of these it is like one missing year at the end

// now, let's match partners so we know we are using the same info across partners, where possible.
preserve

keep pid syear current_rel_type rel_start_all rel_end_all current_rel_start_yr current_rel_end_yr start_yr_missing_flag current_rel_start_miss end_yr_missing_flag transition_year full_status_pl

rename pid partner_id_pl
foreach var in current_rel_type rel_start_all rel_end_all current_rel_start_yr current_rel_end_yr start_yr_missing_flag current_rel_start_miss end_yr_missing_flag transition_year full_status_pl{
	rename `var' `var'_sp
}

save "$temp/ppathl_partner_rel_info.dta", replace

restore

merge m:1 partner_id_pl syear using "$temp/ppathl_partner_rel_info.dta" // has to be many because of the missing partner_id. wait but that's only 2? okay let's try this..
drop if _merge==2
tab _merge // so like 2% don't match
tab _merge if partner_id_pl!=. // and it is not just because of this
unique pid partner_id_pl, by(_merge) // like 7% from this standpoint
drop _merge

// now with THIS information, attempt to clean up info across partners
unique pid partner_id_pl
unique pid partner_id_pl rel_start_all rel_end_all
unique pid partner_id_pl rel_start_all_sp rel_end_all_sp

// might want to do this differently across spouses
gen either_start_missing=0
replace either_start_missing = 1 if start_yr_missing_flag==1 | current_rel_start_miss==1
gen either_start_missing_sp=0
replace either_start_missing_sp = 1 if start_yr_missing_flag_sp==1 | current_rel_start_miss_sp==1

inspect rel_start_all rel_start_all_sp rel_end_all rel_end_all_sp // right there is missing spouse info for the spouses that didn't match on merge

browse couple_id pid partner_id_pl syear current_rel_type current_rel_type_sp rel_start_all rel_start_all_sp rel_end_all rel_end_all_sp ever_transition either_start_missing either_start_missing_sp end_yr_missing_flag end_yr_missing_flag_sp current_rel_how_end

gen rel_start_yr_couple = .
gen rel_end_yr_couple = .

	// like first - if they match, okay great
	replace rel_start_yr_couple = rel_start_all if rel_start_all==rel_start_all_sp & rel_start_all!=.
	replace rel_end_yr_couple = rel_end_all if rel_end_all==rel_end_all_sp & rel_end_all!=.
	
	// if only one has info - use that person's info
	replace rel_start_yr_couple = rel_start_all if rel_start_yr_couple==. & rel_start_all!=. & rel_start_all_sp==.
	replace rel_end_yr_couple = rel_end_all if rel_end_yr_couple==. & rel_end_all!=. & rel_end_all_sp==.
	
	// don't think the reverse is ever true but let's try - okay yeah this didn't add anything
	replace rel_start_yr_couple = rel_start_all_sp if rel_start_yr_couple==. & rel_start_all==. & rel_start_all_sp!=.
	replace rel_end_yr_couple = rel_end_all_sp if rel_end_yr_couple==. & rel_end_all==. & rel_end_all_sp!=.
	
	// if not - does one spouse have non-missing and the other doesn't - based off of BOTH this missing flag AND the left censor flag
	// does this even happen? okay yes
	tab either_start_missing either_start_missing_sp, m	
	tab end_yr_missing_flag end_yr_missing_flag_sp, m
	
	replace rel_start_yr_couple = rel_start_all if rel_start_yr_couple==. & either_start_missing==0 & either_start_missing_sp==1 // so use r if spouse is missing
	replace rel_start_yr_couple = rel_start_all_sp if rel_start_yr_couple==. & either_start_missing==1 & either_start_missing_sp==0 & rel_start_all_sp!=. // use sp if r is missing
	
	replace rel_end_yr_couple = rel_end_all if rel_end_yr_couple==. & end_yr_missing_flag==0 & inlist(end_yr_missing_flag_sp,.,1) // so use r if spouse is missing
	replace rel_end_yr_couple = rel_end_all_sp if rel_end_yr_couple==. & end_yr_missing_flag==1 & end_yr_missing_flag_sp==0 & rel_end_all_sp!=.
	
	// then - prob just use minimum for start and max for end - this is what I usually do?
	// before proceeding, there are some cases where this info is not copied to every row, so let's first do that
	bysort couple_id (rel_start_yr_couple): replace rel_start_yr_couple = rel_start_yr_couple[1]
	bysort couple_id (rel_end_yr_couple): replace rel_end_yr_couple = rel_end_yr_couple[1]
	sort couple_id pid syear
	
	browse couple_id pid partner_id_pl syear current_rel_type current_rel_type_sp rel_start_yr_couple rel_end_yr_couple rel_start_all rel_start_all_sp rel_end_all rel_end_all_sp ever_transition current_rel_how_end
	
	egen min_start = rowmin(rel_start_all rel_start_all_sp)
	egen max_end = rowmax(rel_end_all rel_end_all_sp)
	
	replace rel_start_yr_couple = min_start if rel_start_yr_couple==.
	replace rel_end_yr_couple = max_end if rel_end_yr_couple==.
	
// let's save this just in case
save "$temp/ppathl_partner_match_cleaned.dta", replace

********************************************************************************
**# Now we finally can do other sample restrictions
********************************************************************************
* Relationship started after 1990 - done
* Had to observe relationship start (duration of 0 or 1) - done
* Relationship ended before 2020 (or 3 years from last interview date) - done

gen dur = syear - rel_start_yr_couple
bysort pid partner_id_pl: egen min_dur = min(dur)
bysort pid partner_id_pl: egen max_dur = max(dur)
tab min_dur, m // okay, only like 34% observed at 0 -- is this crazy?
// I am getting confused - if ONE PARTNER observed earlier - is that fine? or do they both need to be observed? like should we impute the missing info?
// like I am wondering if I have to go back to the other file and do these restrictions?
gen min_dur_alt = rel_start_yr_couple - firstyr_survey_pl // here we would want 0 and above right? because if rel started in like 1950 and first survey in 1984, this will be NEGATIVE -34, so that is only 12%?

sort couple_id pid syear
browse pid syear dur min_dur min_dur_alt max_dur rel_start_yr_couple rel_end_yr_couple firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl  // let's make sure that, by restricting to partners / dropping dropouts - I didn't get super misaligned to the survey status info - before I create the realtionship duration / min dur variables
// oh duh I am dumb - if didn't enter survey partnered, then this info will not necessarily match?

unique pid partner_id_pl if rel_start_yr_couple >= 1990 & inlist(min_dur,0,1) & rel_start_yr_couple <=2020
unique couple_id if rel_start_yr_couple >= 1990 & inlist(min_dur,0,1) & rel_start_yr_couple <=2020 // okay well this is still a lot of people, so actually fine??

********************************
* Actual restrictions
********************************
keep if rel_start_yr_couple >= 1990 & inlist(min_dur,0,1) // keeping up to two, because if got married in 2001, say, might not appear in survey until 2003, which is a problem. 
keep if rel_start_yr_couple <=2020 // now will be 2020 because updated to 2023 (and assume 1st year of full data is 2021, so that's three years)

* Also - we are certain of relationship start date (so if both partners were missing / estimated - probably drop?) let's do this after I do the above
tab either_start_missing either_start_missing_sp, m
unique couple_id
unique couple_id if either_start_missing==1 & either_start_missing_sp==1 // because if only one or the other missing, I used the other partner's info and it is actually fine
// display 1454 / 16557 -- okay so 8%. this probably honestly aligns with other surveys as well wrt true missing start info

drop if either_start_missing==1 & either_start_missing_sp==1 

********************************************************************************
** Get list of couples
********************************************************************************
// confirm this info is truly unique
unique pid partner_id_pl 
unique pid partner_id_pl couple_id rel_start_yr_couple rel_end_yr_couple master_rel_end rel_number_all ever_transition transition_year min_dur max_dur first_couple_year last_couple_year

preserve

collapse (first) rel_start_yr_couple rel_end_yr_couple master_rel_end rel_number_all ever_transition transition_year min_dur max_dur first_couple_year last_couple_year, by(pid partner_id_pl couple_id)

label values master_rel_end how_end

gen eligible_couple=1
rename couple_id eligible_couple_id
rename rel_start_yr_couple eligible_rel_start_year
rename rel_end_yr_couple eligible_rel_end_year
rename master_rel_end eligible_rel_status
rename rel_number_all eligible_rel_no

gen long eligible_partner = partner_id_pl
by pid: egen num_rel = count(partner_id_pl) // this is how many relationships in this time frame they are contributing, so not quite the same as relationship order

browse if num_rel > 1
tab transition_year ever_transition, m

save "$created_data/gsoep_couple_list.dta", replace

restore