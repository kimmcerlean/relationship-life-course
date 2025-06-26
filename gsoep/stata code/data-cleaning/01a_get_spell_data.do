********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: get_spell_data.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes the various relationship and birth history files and attempts
* to create a more cohesive relationship history I can use

********************************************************************************
********************************************************************************
********************************************************************************
**# Relationship history files
********************************************************************************
********************************************************************************
********************************************************************************

// various relationship history files - figure out if partnership history includes marital history or if these are mutually exclusive? okay def includes marital, and even non-cohab relationships
// the challenge is the monthly files are only prospective, and the partnership history that includes cohabitation and is retrospective did not start until 2011

********************************************************************************
**# BIOCOUPLY
*** Couple year, all relationships, retrospective (since year of birth)
*** This is the IDEAL file, but is not comprehensive because only covers people
*** who answered after wave 28
*** But let's use this as main file and then figure out how to use the other 
*** files to supplement
********************************************************************************

use "$GSOEP/biocouply.dta", clear
label language EN
unique pid // 56893, 294615
unique pid, by(spelltyp) // 31309 married in HH, 37906 coupled in HH - okay why are there so many more coupled here, but way less married than above?? i dont understand...

sort pid spellnr
// browse if inlist(pid,601,5303,30561901)

// need to create a flag for records I actually want to keep - some prep work
tab begin spelltyp // okay yes, there are 56983 (matches pid) with single and begin of 0
unique pid if begin==0 & spelltyp==5

gen rel_length_yr = endy - beginy if beginy>0 & endy>0

tab spelltyp remark, m row
tab rel_length_yr remark, m
tab rel_length_yr spelltyp, m

// actually create the flag
gen keep_flag = 0
replace keep_flag = 1 if begin==0 & spelltyp==5 // first, create everyone's first single spell, because for people in here who never have a rel, I want to know they existed 
replace keep_flag = 1 if inlist(spelltyp,1,2) // the seems generally real even if added
replace keep_flag = 1 if remark!=3 & spelltyp==3 // only want cohab if NOT added spell (for those marriages that were not preceded by cohab, they add this spell in)

tab spelltyp keep_flag, m row // yes, so basically only want cohab or marriage, and NOT fake cohab

keep if keep_flag

gen rel_type = .
replace rel_type = 0 if spelltyp == 5
replace rel_type = 1 if spelltyp == 3
replace rel_type = 2 if inlist(spelltyp, 1,2)

label define rel_type 0 "single" 1 "cohab" 2 "married"
label values rel_type rel_type
tab rel_type, m

tab rel_type divorce, m // so one problem is that cohab doesn't have an indicator of whether it ended? divorce ONLY applies to marriage
tab rel_type pdeath, m // cohab can end this way
tab divorce pdeath, m // want to make sure the ones of both of these can't overlap - they can't
// so how do I know if attrited or dissolved? use RC?
tab censor rel_type, m
tab censor if rel_type!=0 // so mostly of these are covered in 0 (non-censored) or 5/9/13 (last spell) - what to do about the others?
browse if rel_type!=0 & inlist(censor,3,4,7,8,10,11,12)
browse if inlist(pid,10303,109706,212803, 231103)

browse if rel_type!=0

gen how_end = .
replace how_end = 0 if rel_type!=0 & inlist(censor,5,6,9,13) // intact (see p 14 of the codebook, very last line)
replace how_end = 1 if rel_type!=0 & inlist(censor,0,1,2) // non divorce break-up (aka the end date is observed and real, so we know it ended. if just left-censored (1/2), then we also know end is real) // one problem with this is that it COULD be a transition frm cohab to marriage, but I think we will use a different file to figure that out...
replace how_end = 1 if rel_type!=0 & inlist(censor,3,4,7,8,11,12) // think these also had to have ended or would be last spell. so if just missing/ gap, I think this means it ended and they just don't actually know when, so they guessed. 
replace how_end = 2 if rel_type!=0 & divorce == 1 // divorce
replace how_end = 3 if rel_type!=0 & pdeath == 1 // partner died

// tab censor if rel_type!=0 & how_end==.

label define how_end 0 "intact" 1 "break-up" 2 "divorce" 3 "widowhood"
label values how_end how_end

tab rel_type how_end, m

// need to rerank relationships
bysort pid: egen rel_no = rank(spellnr) if rel_type!=0
tab rel_no rel_type, m
replace rel_no = 0 if rel_no==. // single states
browse pid spellnr rel_no rel_type beginy endy

tab beginy rel_type, m
tab endy rel_type, m

gen start_yr = beginy
replace start_yr = 9999 if rel_type==0
replace start_yr = . if start_yr < 0
tab start_yr rel_type , m

gen end_yr = endy
replace end_yr = 9999 if rel_type==0
replace end_yr = . if end_yr < 0
tab end_yr rel_type , m

// make wide (instead of by spell)
* only keep certain variables
keep pid rel_no rel_type how_end start_yr end_yr begin end censor

* only keep the single record if they are only EVER single
bysort pid: egen ever_rel = max(rel_type)
bysort pid: egen num_records = count(rel_type)
tab num_records ever_rel, m // these give me the same answer
tab rel_type ever_rel // soo I want to drop if single and ever_rel right?

unique pid // 56893 - has stayed the same the whole time
drop if rel_type==0 & ever_rel!=0
unique pid // ensure hasn't changed - okay it hasn't

drop num_records ever_rel
rename begin begin_age
rename end end_age

* rename so I know where they came from (relationship history, by year)
foreach var in rel_no rel_type how_end start_yr end_yr begin_age end_age censor{
	rename `var' rhy_`var'
}

* Okay finally actually reshape
reshape wide rhy_rel_type rhy_how_end rhy_start_yr rhy_end_yr rhy_begin_age rhy_end_age rhy_censor, j(rhy_rel_no) i(pid)
unique pid

* drop the variables that just correspond to single status (wanted to keep these observations so we knew they were captured in the rel history file)
drop rhy_begin_age0 rhy_end_age0 rhy_censor0 rhy_rel_type0 rhy_how_end0 rhy_start_yr0 rhy_end_yr0

save "$temp/biocouply_wide.dta", replace

********************************************************************************
**# BIOCOUPLM
*** Couple month, all relationships, prospective (since entering SOEP)
*** So this is not a history, but does have cohab information for people
*** not captured in above, so this is next prio history
********************************************************************************
use "$GSOEP/biocouplm.dta", clear
label language EN

sort pid spellnr
// browse if inlist(pid,601,5303,30561901)

unique pid // 123215, 199915
unique pid, by(spelltyp) // 72089 married in HH, 23833 coupled in HH
tab spelltyp, m
browse

// need to create a flag for records I actually want to keep - some prep work
gen rel_length_yr = endy - beginy if beginy>0 & endy>0

tab spelltyp remark, m row
tab rel_length_yr remark, m
tab rel_length_yr spelltyp, m

// actually create the flag
gen keep_flag = 0
replace keep_flag = 1 if inlist(spelltyp,1,2) // the seems generally real even if added
replace keep_flag = 1 if remark!=3 & spelltyp==3 // only want cohab if NOT added spell (for those marriages that were not preceded by cohab, they add this spell in)
replace keep_flag = 1 if remark==3 & spelltyp==3 & rel_length_yr > 0 // there are some that were added but seem real (bc longer than 1 year)

tab spelltyp keep_flag, m row // yes, so basically only want cohab or marriage, and NOT fake cohab

// but do I need to keep 1 record for people who are never eligible? gah I think so.
bysort pid: egen ever_elig_rel = max(keep_flag)
tab ever_elig_rel, m // so if the max is 0, never eligible
tab ever_elig_rel keep_flag, m

keep if keep_flag == 1 | ever_elig_rel==0
unique pid // yes so this still matches

// make easier indicator of type of relationship
gen rel_type = .
replace rel_type = 0 if spelltyp == 5
replace rel_type = 1 if spelltyp == 3
replace rel_type = 2 if inlist(spelltyp, 1,2)
replace rel_type = 3 if inlist(spelltyp,4,6,7,8,98,99)

capture label drop rel_type
label define rel_type 0 "single" 1 "cohab" 2 "married" 3 "non-elig rel"
label values rel_type rel_type
tab rel_type, m

// figure out how ended
tab rel_type divorce, m // so one problem is that cohab doesn't have an indicator of whether it ended? divorce ONLY applies to marriage
tab rel_type pdeath, m // cohab can end this way
tab divorce pdeath, m // want to make sure the ones of both of these can't overlap - they can't
// so how do I know if attrited or dissolved? use RC?
tab censor rel_type, m
tab censor, m // so mostly of these are covered in non-censored, last spell (RC), or missing start date (LC)

	// let's also create a better flag for missing start date (bc will want to attempt to supplement this info with other sources)
	tab censor, m
	tab spellnr censor, m
	tab censor if spellnr==1, m
	tab spellnr if inlist(censor,1,7,8,9,10) // just LC missing (not afer gap)
	
	gen left_censored=0
	replace left_censored=1 if inlist(censor,1,7,8,9,10)

	gen intact=0
	replace intact=1 if inlist(censor,5,6,9,10,13) // this is based on last spell

	gen ended=0
	replace ended=1 if inlist(censor,0,1,2,3,4,7,8,11,12) // we have a clear end date (aka NOT right censored last spell)

gen how_end = .
replace how_end = 0 if keep_flag==1 & intact==1
replace how_end = 1 if keep_flag==1 & ended==1
replace how_end = 2 if keep_flag==1 & divorce == 1 // divorce
replace how_end = 3 if keep_flag==1 & pdeath == 1 // partner died

// tab censor if rel_type!=0 & how_end==.

capture label define how_end 0 "intact" 1 "break-up" 2 "divorce" 3 "widowhood"
label values how_end how_end

tab rel_type how_end, m
tab censor how_end, m

// need to rerank relationships
bysort pid: egen rel_no = rank(spellnr) if keep_flag==1
tab rel_no rel_type, m
replace rel_no = 0 if rel_no==. // non-eligible relationships
sort pid spellnr
browse pid spellnr rel_no rel_type beginy endy coupid partnr

// figure out partner info
sum coupid, detail
gen has_coupid=. // should be -2 aka not partnered
replace has_coupid = 0 if coupid < 0
replace has_coupid = 1 if coupid > 0 & coupid!=.

tab keep_flag has_coupid, m

inspect partnr
gen has_partnerid=.
replace has_partnerid = 0 if partnr < 0
replace has_partnerid = 1 if partnr > 0 & partnr!=.

tab keep_flag has_partnerid, m
tab has_coupid has_partnerid, m // entirely overlaps

tab how_end has_partnerid, m row // so divorce least likely to have partner id
tab rel_type has_partnerid, m row // not related
tab censor has_partnerid, m row
tab event has_partnerid, m row
tab remark has_partnerid, m row
tab beginy has_partnerid, m row
tab endy has_partnerid, m row // worst for those ending 2022
tab spelltyp has_partnerid // oh duh some is bc married not in HH, but that doesn't explain all of it...

browse pid spellnr rel_no rel_type how_end beginy endy coupid partnr

// save "$temp/biocouplm_cleaned.dta", replace

// make wide (instead of by spell)
* GAH - for those with no eligible relationships, they could have many records like that and it is causing problems
bysort pid: egen first_spell = min(spellnr) if keep_flag==0 // oh I am so dumb. if they NEVER have an eligible rel, they will all have a spellnr of 1 that I can keep
tab spellnr if keep_flag==0
unique pid if keep_flag==0
drop if keep_flag==0 & spellnr!=1
unique pid // still matches

* First, just keep relevant variables
keep pid coupid partnr rel_no rel_type how_end censor events left_censored intact ended beginy endy

* Rename so I know where came from (relationship history, by month)
foreach var in coupid partnr rel_no rel_type how_end censor events left_censored intact ended beginy endy{
	rename `var' rhm_`var'
}

* Actually reshape
reshape wide rhm_coupid rhm_partnr rhm_beginy rhm_endy rhm_censor rhm_events rhm_rel_type rhm_left_censored rhm_intact rhm_ended rhm_how_end, j(rhm_rel_no) i(pid)

unique pid // and number of pid still matches
browse
tab rhm_rel_type0, m
tab rhm_rel_type0 rhm_rel_type1, m // want to make sure there is perfect non-overlap and there is (so that everyone with missing on rel1 info had rel0 info before I drop)

* non-eligible records
drop rhm_coupid0 rhm_partnr0 rhm_beginy0 rhm_endy0 rhm_censor0 rhm_events0 rhm_rel_type0 rhm_left_censored0 rhm_intact0 rhm_ended0 rhm_how_end0

save "$temp/biocouplm_wide.dta", replace

********************************************************************************
**# BIOMARSY
*** Couple year, just marriage, retrospective (since year of birth)
*** Only want to use this to fill in the gaps of people who are not in the
*** COUPLY file - so this will ONLY be marital history and if people entered
*** SOEP already cohab (in COUPLM), and NOT in COUPLY, I will not be able to fill
*** In their true relationship start date, so I guess we need to drop?
********************************************************************************
use "$GSOEP/biomarsy.dta", clear // okay, so this file essentially categorizes person's whole life into spells based on their legal maritus status - each row is a new status and is bookended by age / year of that spell - so each "spell" is NOT a new partner; it's a new marital status - aka stay in when divorced, for example
label language EN

sort pid spellnr
// browse if inlist(pid,601,5303,30561901)

unique pid // 116061, 241757
unique pid, by(spelltyp) // 77385 married in HH,  coupled in HH - duh doesn't track coupled

// here I think I can just keep married records?
// oh I should also keep the first single record, as i did before, to retain records for people never married (also - should make sure that info matches across my three files)
tab spelltyp remark, m
tab begin spelltyp
unique pid if begin==0 & spelltyp==1
tab spelltyp if remark==5

gen keep_flag = 0
replace keep_flag = 1 if remark==5 // first spell
replace keep_flag = 1 if spelltyp==2

keep if keep_flag==1

// rel information
gen married=0
replace married = 1 if spelltyp==2

tab censor married // lol there is no info on end - so we have to use censor again?
tab censor if married==1, m // what are the RC missing? okay these also seem like maybe intact? bc many of these are last spells..

browse if married==1 & censor==3

bysort pid: egen max_rel = max(spellnr) if married==1
sort pid spellnr

gen ended=0
replace ended=1 if inlist(censor,0,1,2,4,8,12) // basically if not right censored by last spell or missing
replace ended=1 if inlist(censor,3,7,11) & max_rel != spellnr // if missing and other records follow, then ended)

gen intact=0
replace intact=1 if inlist(censor,5,6,9,10,13,14) // if last spell or death 
replace intact=1 if inlist(censor,3,7,11) & max_rel == spellnr // if missing but last record, then intact

browse pid spellnr married max_rel censor ended intact

gen how_end = .
replace how_end = 0 if intact==1 & married==1
replace how_end = 1 if ended==1 & married==1

capture label define how_end 0 "intact" 1 "break-up" 2 "divorce" 3 "widowhood"
label values how_end how_end

tab married how_end, m row // is this too many intact? idk...

browse pid spellnr married how_end censor beginy endy

// need to rerank relationships and update dates for single folks
bysort pid: egen marr_no = rank(spellnr) if married==1
tab marr_no married, m
replace marr_no = 0 if marr_no==. // single states
browse pid spellnr marr_no married beginy endy

tab beginy married, m
tab endy married, m

gen start_yr = beginy
replace start_yr = 9999 if married==0
replace start_yr = . if start_yr < 0
tab start_yr married , m

gen end_yr = endy
replace end_yr = 9999 if married==0
replace end_yr = . if end_yr < 0
tab end_yr married , m
tab end_yr how_end, m

// make wide (instead of by spell)
* only keep certain variables
keep pid marr_no married how_end start_yr end_yr begin end censor

* only keep the single record if they are only EVER single
bysort pid: egen ever_marr = max(married)
bysort pid: egen num_records = count(married)
tab num_records ever_marr, m // these give me the same answer
tab married ever_marr // soo I want to drop if single but ever married right?

unique pid // 116060
drop if married==0 & ever_marr==1
unique pid // ensure hasn't changed - okay it hasn't

drop num_records
rename begin begin_age
rename end end_age

* rename so I know where they came from (marital history, by year)
foreach var in begin_age end_age censor married how_end marr_no start_yr end_yr ever_marr{
	rename `var' mhy_`var'
}

* Okay finally actually reshape
reshape wide mhy_begin_age mhy_end_age mhy_censor mhy_married mhy_how_end mhy_start_yr mhy_end_yr, j(mhy_marr_no) i(pid mhy_ever_marr) // retain ever married status for reference (oh I probably could have done this for other files oops)
unique pid

tab mhy_married0, m
tab mhy_married1, m
tab mhy_ever_marr mhy_married1, m
tab mhy_married0 mhy_married1, m

* drop the variables that just correspond to single status (wanted to keep these observations so we knew they were captured in the marital history file)
drop mhy_begin_age0 mhy_end_age0 mhy_censor0 mhy_married0 mhy_how_end0 mhy_start_yr0 mhy_end_yr0

save "$temp/biomarsy_wide.dta", replace

********************************************************************************
**# BIOMARSM
*** Couple month, just marriage, prospective (since entering SOEP)
*** This is the least useful file for us because this info is all contained in
*** BIOCOUPM and that also contains cohah. so I will not need to compile these data
********************************************************************************
use "$GSOEP/biomarsm.dta", clear
label language EN

// sort pid spellnr
// browse if inlist(pid,601,5303,30561901)

unique pid // 115873, 123937
unique pid, by(spelltyp) // 69621 married in HH,  coupled in HH - duh doesn't track coupled

********************************************************************************
********************************************************************************
********************************************************************************
**# Master relationship history
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Merge all partner histories together
********************************************************************************
// can I start with ppathl to get a unique list of PIDs and their first and last survey yr? Use that as base to add on the files (bc that will be my analysis base)

use "$temp/ppathl_cleaned.dta", clear
label language EN

unique pid
unique pid firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl // are these unique? yes okay
browse pid firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl status_pl // some people have missing for survey yr (not contact yr) - those people never gave a full pl interview
gen ever_int = 0
replace ever_int = 1 if inrange(firstyr_survey_pl,1984,2022)

tab ever_int, m 
tab firstyr_survey_pl ever_int, m 

// collapse to get just a list of pids
collapse (lastnm) ever_int firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl, by(pid)

save "$temp/all_pids.dta", replace

// first let's merge on the couple year file because that is my ideal file. then from there we can start to see how many people missing
merge 1:1 pid using "$temp/biocouply_wide.dta"
drop if _merge==2
gen in_couply = .
replace in_couply = 0 if _merge==1
replace in_couply = 1 if _merge==3
drop _merge

// okay coverage is QUITE BAD
tab ever_int in_couply, m row // about half when I restrict to those who ever had a pl interview (because we will obviously need those people)

// now merge on couple month - next in order of hierarchy bc at least contains COHAB (but not HISTORY)
merge 1:1 pid using "$temp/biocouplm_wide.dta" // shocking amount of not matched? is it bc of never interviews??
drop if _merge==2
gen in_couplm = .
replace in_couplm = 0 if _merge==1
replace in_couplm = 1 if _merge==3
drop _merge

tab ever_int in_couplm, m row // okay yes, of those with at least one interview, 98%+ coverage
tab in_couplm in_couply if ever_int==1, m row
tab in_couply in_couplm if ever_int==1, m row

// and finally, the marriage year data
merge 1:1 pid using "$temp/biomarsy_wide.dta" // shocking amount of not matched? is it bc of never interviews??
drop if _merge==2
gen in_marsy = .
replace in_marsy = 0 if _merge==1
replace in_marsy = 1 if _merge==3
drop _merge

tab ever_int in_marsy, m row // okay yes, it's the never interviewed
tab in_marsy in_couplm // so more in couplm than in marital history
tab in_marsy in_couply // only 7 people in cohab history but not marital

// get a sense of the data for now
browse pid in_couply in_couplm in_marsy rhy_rel_type1 rhy_how_end1 rhy_start_yr1 rhy_end_yr1 rhm_rel_type1 rhm_how_end1 rhm_beginy1 rhm_endy1 rhm_left_censored1 mhy_married1 mhy_how_end1 mhy_start_yr1 mhy_end_yr1 if ever_int==1

save "$temp/all_rel_history.dta", replace
** Stopped here 6/24/25 because got overwhelmed at the thought of figuring this out
** BUT-  i feel like I had t do something similar-ish with fertility history for that paper for PSID, so look at that code for ideas (basically rerank across multiple sources and recompile a history file - which is what I need to do here)

********************************************************************************
* Attempt to compile into one master history
********************************************************************************
use "$temp/all_rel_history.dta", clear

// so this is currently wide. do I want to make long? to compare aCROSS numbers. bc if the relationships in rhy and rhm match (aka they didn't have any relationships PRE survey), then I can just use rhm? BUT if rhm rel#1 is left-censored, I need to use information to fill it in
// max rels in rhy: 10 | rhm: 9 | mhy: 6

reshape long rhy_begin_age rhy_end_age rhy_censor rhy_rel_type rhy_how_end rhy_start_yr rhy_end_yr rhm_coupid rhm_partnr rhm_beginy rhm_endy rhm_censor rhm_events rhm_rel_type rhm_left_censored rhm_intact rhm_ended rhm_how_end mhy_begin_age mhy_end_age mhy_censor mhy_married mhy_how_end mhy_start_yr mhy_end_yr, i(pid) j(rel_no)

capture label define rel_type 0 "single" 1 "cohab" 2 "married" 3 "non-elig rel"
label values rhy_rel_type rhm_rel_type rel_type

browse pid in_couply rel_no rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr if ever_int==1

// so for the people in couply, I literally just take their whole history from there.
// so I really am just needing to sort out how to stack across these two other datasets. Do I actually keep wide - rename so they are fully in order, then rehape RERANK - and if ties, keep just 1 record?
// okay no, trying some other hacks. because really couplm is also fine, we just need two other pieces of supplemental info from marital history (if not in cohab history)
	*-- true relationship start date
	*-- any relationships prior to the one first observed in SOEP
browse pid in_couply rel_no rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr if ever_int==1 & in_couply==0

// one way to get the true relationship start date of the union they entered soep in (aka first rel in couplm)
gen couplm_rel1_type = rhm_rel_type if rel_no==1
gen couplm_rel1_start = rhm_beginy if rel_no==1
gen couplm_rel1_end = rhm_endy if rel_no==1
label values couplm_rel1_type rel_type

foreach var in couplm_rel1_type couplm_rel1_start couplm_rel1_end{
	bysort pid (`var'): replace `var' = `var'[1]
}

sort pid rel_no
browse pid in_couply rel_no couplm_rel1_type couplm_rel1_start couplm_rel1_end rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr if ever_int==1

gen couplem_rel1_start_real = .
replace couplem_rel1_start_real = rhy_start_yr if in_couply==1 & rhy_end_yr == couplm_rel1_end
replace couplem_rel1_start_real = mhy_start_yr if in_couply==0 & mhy_end_yr == couplm_rel1_end & couplm_rel1_type==2
bysort pid (couplem_rel1_start_real): replace couplem_rel1_start_real = couplem_rel1_start_real[1]

browse pid in_couply rel_no couplm_rel1_type couplem_rel1_start_real couplm_rel1_start couplm_rel1_end rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr if ever_int==1

tab couplm_rel1_type if in_couply==0 // so how many of these are cohab and therefore we will not know? okay like just under 20%
inspect couplem_rel1_start_real if couplm_rel1_type!=.
tab couplem_rel1_start_real couplm_rel1_type, m col // okay so about 7% of marriages missing real start date, but about 50% of cohab are

// now, flag if other marriages prior to this relationship
gen add_to_history = .
replace add_to_history = 1 if mhy_end_yr < couplem_rel1_start_real & couplem_rel1_start_real!=. // so use the real date if it exists
replace add_to_history = 1 if mhy_end_yr < couplm_rel1_start & couplem_rel1_start_real==. & couplm_rel1_type!=. // if it doesn't use the estimated start date (but restrict to at least those with a first relationship)
replace add_to_history = 1 if rel_no==1 & mhy_married==1 & couplm_rel1_type==. // some people have no relationships in SOEP and ONLY have prior rels, so need to keep those as well
// tab couplm_rel1_type mhy_married if rel_no==1, m

browse pid in_couply rel_no couplm_rel1_type couplem_rel1_start_real couplm_rel1_start couplm_rel1_end rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr add_to_history if ever_int==1

// rank to get order of those to then use to rerank the coupm history
by pid: egen marr_history_rank = rank(rel_no) if add_to_history==1
browse pid in_couply rel_no marr_history_rank mhy_married mhy_start_yr mhy_end_yr add_to_history if add_to_history==1
	// oh could I just use rel_no lol
	tab rel_no if add_to_history==1, m
	tab marr_history_rank rel_no if add_to_history==1, m
by pid: egen num_history = max(marr_history_rank)
replace num_history = 0 if num_history==.

// now, trying to create a "real rank" aggregating between the monthly couple and marital history
gen rel_no_adjusted = rel_no + num_history
browse pid in_couply rel_no rel_no_adjusted num_history couplm_rel1_type couplem_rel1_start_real couplm_rel1_start couplm_rel1_end rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr marr_history_rank if ever_int==1

forvalues m=1/3{
	gen marr`m'_type = 2 if marr_history_rank==`m'
	gen marr`m'_start = mhy_start_yr if marr_history_rank==`m'
	gen marr`m'_end = mhy_end_yr if marr_history_rank==`m'
	gen marr`m'_how_end = mhy_how_end if marr_history_rank==`m'
	
	label values marr`m'_type rel_type
	label values marr`m'_how_end how_end
	
	foreach var in marr`m'_type marr`m'_start marr`m'_end marr`m'_how_end{
		bysort pid (`var'): replace `var' = `var'[1]
	}
}

sort pid rel_no
browse pid rel_no rel_no_adjusted marr_history_rank marr1_type marr1_start marr1_end marr1_how_end marr2_type marr2_start marr2_end marr2_how_end mhy_married mhy_start_yr mhy_end_yr mhy_how_end

// actually if no marital history, I can just the coupm data, right? just need to adjust the start date?
* create new blank variables to fill in
gen master_rel_type=.
gen master_how_end=.
gen master_start_yr=.
gen master_end_yr=.

* fill in with couply if in there
replace master_rel_type = rhy_rel_type if in_couply==1
replace master_how_end = rhy_how_end if in_couply==1
replace master_start_yr = rhy_start_yr if in_couply==1
replace master_end_yr = rhy_end_yr if in_couply==1

* fill in with couplm if num_history==0
// browse pid rel_no in_couply num_history rhm_rel_type rhm_how_end rhm_beginy rhm_left_censored couplem_rel1_start_real rhm_endy rhy_start_yr rhy_end_yr mhy_start_yr mhy_end_yr 

replace master_rel_type = rhm_rel_type if in_couply==0 & num_history==0
replace master_how_end = rhm_how_end if in_couply==0 & num_history==0
replace master_start_yr = rhm_beginy if in_couply==0 & num_history==0 & rel_no!=1
	* replace the first relationship with "real start date" // should I keep a copy of the old start date somewhere?
	replace master_start_yr = rhm_beginy if in_couply==0 & num_history==0 & rel_no==1 & rhm_left_censored==0
	replace master_start_yr = couplem_rel1_start_real if in_couply==0 & num_history==0 & rel_no==1 & rhm_left_censored==1
replace master_end_yr = rhm_endy if in_couply==0 & num_history==0

* then, need to do a piecemeal process if there are marriages to add
replace master_rel_type = marr1_type if rel_no==1 & in_couply==0 & num_history!=0 // if you have at least one in history, then you def have a marriage 1
replace master_how_end = marr1_how_end if rel_no==1 & in_couply==0 & num_history!=0
replace master_start_yr = marr1_start if rel_no==1 & in_couply==0 & num_history!=0
replace master_end_yr = marr1_end if rel_no==1 & in_couply==0 & num_history!=0
replace master_rel_type = marr2_type if rel_no==2 & in_couply==0 & inlist(num_history,2,3) // but only have a marriage2 if more than 1 in history
replace master_how_end = marr2_how_end if rel_no==2 & in_couply==0 & inlist(num_history,2,3)
replace master_start_yr = marr2_start if rel_no==2 & in_couply==0 & inlist(num_history,2,3)
replace master_end_yr = marr2_end if rel_no==2 & in_couply==0 & inlist(num_history,2,3)
replace master_rel_type = marr3_type if rel_no==3 & in_couply==0 & num_history==3
replace master_how_end = marr3_how_end if rel_no==3 & in_couply==0 & num_history==3
replace master_start_yr = marr3_start if rel_no==3 & in_couply==0 & num_history==3
replace master_end_yr = marr3_end if rel_no==3 & in_couply==0 & num_history==3

// gah struggling how to now get the cohab info on. I think I just need this to be populated in all columns AGAIN (should I have just left this wide?)
// I think I need to merge back on this history omg
merge m:1 pid using "$temp/biocouplm_wide.dta"
tab in_couplm _merge, m
drop _merge

tab rel_no if rhm_rel_type!=. & in_couply==0 & num_history!=0, m  // gah up to 7 relationships
gen first_chm_lc = rhm_left_censored if rel_no==1
bysort pid (first_chm_lc): replace first_chm_lc = first_chm_lc[1]
sort pid rel_no

browse pid rel_no rel_no_adjusted in_couply num_history master_rel_type master_how_end master_start_yr master_end_yr rhm_beginy couplem_rel1_start_real rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr rhm_beginy1 rhm_endy1 if num_history!=0 & in_couply==0

forvalues r=1/7{
	local s=`r' + 1
	
	replace master_rel_type = rhm_rel_type`r' if rel_no==`s' & in_couply==0 & num_history==1 // if num_history == 1 then the first chm goes in 2 / will adjust the true start date later
	replace master_how_end = rhm_how_end`r' if rel_no==`s' & in_couply==0 & num_history==1 
	replace master_start_yr = rhm_beginy`r' if rel_no==`s' & in_couply==0 & num_history==1 
	replace master_end_yr = rhm_endy`r' if rel_no==`s' & in_couply==0 & num_history==1 
}

forvalues r=1/7{
	local s=`r' + 2
	
	replace master_rel_type = rhm_rel_type`r' if rel_no==`s' & in_couply==0 & num_history==2 // if num_history == 2 then the first chm goes in 3
	replace master_how_end = rhm_how_end`r' if rel_no==`s' & in_couply==0 & num_history==2 
	replace master_start_yr = rhm_beginy`r' if rel_no==`s' & in_couply==0 & num_history==2 
	replace master_end_yr = rhm_endy`r' if rel_no==`s' & in_couply==0 & num_history==2
}

forvalues r=1/7{
	local s=`r' + 3
	
	replace master_rel_type = rhm_rel_type`r' if rel_no==`s' & in_couply==0 & num_history==3 // if num_history == 3 then the first chm goes in 4
	replace master_how_end = rhm_how_end`r' if rel_no==`s' & in_couply==0 & num_history==3
	replace master_start_yr = rhm_beginy`r' if rel_no==`s' & in_couply==0 & num_history==3 
	replace master_end_yr = rhm_endy`r' if rel_no==`s' & in_couply==0 & num_history==3
}

// now need to adjust all start dates gah
replace master_start_yr = couplem_rel1_start_real if rel_no==2 & in_couply==0 & num_history==1 & first_chm_lc ==1
replace master_start_yr = couplem_rel1_start_real if rel_no==3 & in_couply==0 & num_history==2 & first_chm_lc ==1
replace master_start_yr = couplem_rel1_start_real if rel_no==4 & in_couply==0 & num_history==3 & first_chm_lc ==1

* Oh, need to figure out how to add partner_id ESPECIALLY if not using couplm. do I need this? let's come back to this, think I can add on later based on matching information from the wide files...

browse pid rel_no rel_no_adjusted first_chm_lc in_couply num_history master_rel_type master_how_end master_start_yr master_end_yr rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy couplem_rel1_start_real rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr 

label values master_rel_type rel_type
label values master_how_end how_end

* now create a file to save
keep pid rel_no ever_int firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl in_couply in_couplm mhy_ever_marr in_marsy couplm_rel1_start couplem_rel1_start_real master_rel_type master_how_end master_start_yr master_end_yr first_chm_lc

reshape wide master_rel_type master_how_end master_start_yr master_end_yr, j(rel_no) i(pid)
browse pid mhy_ever_marr master_rel_type* master_how_end* master_start_yr* master_end_yr*

tab mhy_ever_marr master_rel_type1 if ever_int==1, m

save "$created_data/consolidated_rel_history.dta", replace

********************************************************************************
* Then want to take this info and add to ppathl to get full file of information
* from 1984 - 2022 for all people (with all relationship start / end dates / 
* statuses / partner ids, etc.)
********************************************************************************
use "$temp/ppathl_cleaned.dta", clear
label language EN

tab syear partnered_pl, m row // is cohab covered all years? in theory, but incidence very low pre like 1996/97. Is that real incidence or survey issue?
tab status_pl partnered_pl, m row // so, I think they do try to fill in partner status in dropout / no interview years, but the incidence of "not clear" is much higher
// so maybe *they* try to fill this in using rel history as well? which takes a lot of the work out for me. I just want the START / END date measures here if possible...
// okay, and also, there is only record of a dropout - none of the years after that have rows, so would need to add those and all would missing because not sure
// also not clear if ppathl has PRE survey years. okay it does not
// the codebook says " In unclear cases, due to temporal non-response for instance, we also consider longitudinal information from previous and prospective waves." -- but so this is really only if temp
// okay so should I do FILL IN so I have all years for everyone? and then it can be like pre-survey, post-survey
// but then I can use the rel history to possibly at least fill in relationship status / info for those years?

browse pid syear status_pl survey_status_pl partnered_pl partner_id_pl firstyr_survey_pl lastyr_survey_pl
browse pid syear status_pl survey_status_pl partnered_pl partner_id_pl firstyr_survey_pl lastyr_survey_pl if inlist(pid,601,5303,30561901,1088902) // example PIDs I am using

// first do a fill-in so I get years for everyone
gen orig_row = 1 // create a flag so I know which rows I've added

// add biocouply

// add biocoupm (only to those missing biocoupy?)
// well the utility of biocoupm for EVERYONE is that it has partner id....

// add marry (only to those missing biocouply?)
// alternatively, get a list of unique pids and add biocoupm / biomarry and make LONG again and use the two together to attempt to fill in history?


********************************************************************************
********************************************************************************
********************************************************************************
**# Fertility history files
********************************************************************************
********************************************************************************
********************************************************************************
// for births AFTER marriage (or - births relative to marriage)
use "$GSOEP/biobirth.dta", clear
label language EN

unique pid // 158946, 158946 -- so this is one record per PID so more of 1:1 lookup

gen any_births=0
replace any_births = 1 if sumkids > 0 & sumkids<100
tab sumkids any_births, m 

tab kidgeb01 any_births, m

gen first_birth_year = kidgeb01
replace first_birth_year=9999 if kidgeb01 < 0

tab first_birth_year any_births, m

keep pid sumkids kidgeb01 kidmon01 any_births first_birth_year

save "$temp/biobirth_cleaned.dta", replace

