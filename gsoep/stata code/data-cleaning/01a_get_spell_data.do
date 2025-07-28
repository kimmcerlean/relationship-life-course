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
unique pid // 59428, 309525
unique pid, by(spelltyp) // 32686 married in HH, 39517 coupled in HH - okay why are there so many more coupled here, but way less married than above?? i dont understand...

sort pid spellnr
// browse if inlist(pid,601,5303,30561901)

// need to create a flag for records I actually want to keep - some prep work
tab begin spelltyp // okay yes, there are  59428 (matches pid) with single and begin of 0
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

unique pid // 59428 - has stayed the same the whole time
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

unique pid // 122318, 201532 // lol the # of pids went down since last version??
unique pid, by(spelltyp) // 71873 married in HH, 23979 coupled in HH
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

// tab censor if inlist(rel_type,1,2) & how_end==.

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
tab endy has_partnerid, m row // worst for those ending 2022 / 2023
tab spelltyp has_partnerid // oh duh some is bc married not in HH, but that doesn't explain all of it...
// okay I also compared to the v39 version and the coverage for married couples in v39 was like 90% but now it's only about 60%...
// I think I can just recover this info from ppathl but I am confused...

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

unique pid // 125156, 259916
unique pid, by(spelltyp) // 77385 married in HH,  coupled in HH - duh doesn't track coupled

// here I think I can just keep married records?
// oh I should also keep the first single record, as i did before, to retain records for people never married (also - should make sure that info matches across my three files)
tab spelltyp remark, m
tab begin spelltyp
unique pid if begin==0 & spelltyp==1
tab spelltyp if remark==5

unique pid if remark==5 // wait so not everyone has a first spell? (my pid goes too low in current state)
tab remark if spellnr == 1
tab spelltyp if spellnr == 1
browse if spellnr==1 & remark!=5

gen keep_flag = 0
// replace keep_flag = 1 if remark==5 // first spell
replace keep_flag = 1 if spellnr==1 // idk why the above isn't working, so let's just use everyone's first spell
replace keep_flag = 1 if spelltyp==2

unique pid if keep_flag==1
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
keep pid marr_no married how_end start_yr end_yr begin end censor remark

* only keep the single record if they are only EVER single
bysort pid: egen ever_marr = max(married)
bysort pid: egen num_records = count(married)
tab num_records ever_marr, m // these give me the same answer
tab married ever_marr // soo I want to drop if single but ever married right?

unique pid // 125156
drop if married==0 & ever_marr==1
unique pid // ensure hasn't changed - okay it hasn't

drop num_records
rename begin begin_age
rename end end_age

* rename so I know where they came from (marital history, by year)
foreach var in begin_age end_age censor married how_end marr_no start_yr end_yr ever_marr remark{
	rename `var' mhy_`var'
}

* Okay finally actually reshape
reshape wide mhy_begin_age mhy_end_age mhy_censor mhy_married mhy_how_end mhy_start_yr mhy_end_yr mhy_remark, j(mhy_marr_no) i(pid mhy_ever_marr) // retain ever married status for reference (oh I probably could have done this for other files oops)
unique pid

tab mhy_married0, m
tab mhy_married1, m
tab mhy_ever_marr mhy_married1, m
tab mhy_married0 mhy_married1, m

* drop the variables that just correspond to single status (wanted to keep these observations so we knew they were captured in the marital history file)
drop mhy_begin_age0 mhy_end_age0 mhy_censor0 mhy_married0 mhy_how_end0 mhy_start_yr0 mhy_end_yr0 mhy_remark0

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

unique pid // 118257, 137441
unique pid, by(spelltyp) // 71219 married in HH,  coupled in HH - duh doesn't track coupled

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
replace ever_int = 1 if inrange(firstyr_survey_pl,1984,2023)

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

tab ever_int in_couplm, m row // okay yes, of those with at least one interview, 93%+ coverage (is that bad?) there are also seemingly people without interviews that are in coupm..
// coverage has gone down since v39...
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
tab in_marsy in_couplm // there are a decently high number of people in either coupm or marsty but not both...
tab in_marsy in_couply // only 120 people in cohab history but not marital

// get a sense of the data for now
browse pid in_couply in_couplm in_marsy rhy_rel_type1 rhy_how_end1 rhy_start_yr1 rhy_end_yr1 rhm_rel_type1 rhm_how_end1 rhm_beginy1 rhm_endy1 rhm_left_censored1 mhy_married1 mhy_how_end1 mhy_start_yr1 mhy_end_yr1 if ever_int==1

save "$temp/all_rel_history.dta", replace

********************************************************************************
* Attempt to compile into one master history
********************************************************************************
use "$temp/all_rel_history.dta", clear

// so this is currently wide. do I want to make long? to compare aCROSS numbers. bc if the relationships in rhy and rhm match (aka they didn't have any relationships PRE survey), then I can just use rhm? BUT if rhm rel#1 is left-censored, I need to use information to fill it in
// max rels in rhy: 10 | rhm: 9 | mhy: 6

reshape long rhy_begin_age rhy_end_age rhy_censor rhy_rel_type rhy_how_end rhy_start_yr rhy_end_yr rhm_coupid rhm_partnr rhm_beginy rhm_endy rhm_censor rhm_events rhm_rel_type rhm_left_censored rhm_intact rhm_ended rhm_how_end mhy_begin_age mhy_end_age mhy_censor mhy_married mhy_how_end mhy_start_yr mhy_end_yr mhy_remark, i(pid) j(rel_no)

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
tab couplem_rel1_start_real couplm_rel1_type, m col // okay so about 5% of marriages missing real start date, but about 50% of cohab are

// now, flag if other marriages prior to this relationship
gen add_to_history = .
replace add_to_history = 1 if mhy_end_yr < couplem_rel1_start_real & couplem_rel1_start_real!=. // so use the real date if it exists
replace add_to_history = 1 if mhy_end_yr < couplm_rel1_start & couplem_rel1_start_real==. & couplm_rel1_type!=. // if it doesn't use the estimated start date (but restrict to at least those with a first relationship)
replace add_to_history = 1 if rel_no==1 & mhy_married==1 & couplm_rel1_type==. // some people have no relationships in SOEP and ONLY have prior rels, so need to keep those as well
// tab couplm_rel1_type mhy_married if rel_no==1, m

browse pid in_couply rel_no couplm_rel1_type couplem_rel1_start_real couplm_rel1_start couplm_rel1_end rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr add_to_history if ever_int==1

// rank to get order of those to then use to rerank the coupm history
by pid: egen marr_history_rank = rank(rel_no) if add_to_history==1
browse pid in_couply rel_no marr_history_rank mhy_married mhy_start_yr mhy_end_yr add_to_history mhy_remark mhy_censor if add_to_history==1
browse pid in_couply rel_no marr_history_rank mhy_married mhy_start_yr mhy_end_yr add_to_history mhy_married mhy_remark mhy_censor if inlist(pid,3189301,30486902,20533402,20480302,21084007,32375502,61799402)
	// oh could I just use rel_no lol
	tab rel_no if add_to_history==1, m
	tab marr_history_rank rel_no if add_to_history==1, m // this doesn't make sense...this feels possibly too low also?
	// is it bc of edited / added spells? (remark of 2/3)
	// no that doesn't explain it. they are just random spellls that have a start date but no end date and the start date matches the next row
	tab mhy_censor if rel_no != marr_history_rank & marr_history_rank!=. // oaky so they are all right censored. this just doesn't make sense...but my rankings look more accurate so we will leave
by pid: egen num_history = max(marr_history_rank)
replace num_history = 0 if num_history==.

// now, trying to create a "real rank" aggregating between the monthly couple and marital history
gen rel_no_adjusted = rel_no + num_history
browse pid in_couply rel_no rel_no_adjusted num_history couplm_rel1_type couplem_rel1_start_real couplm_rel1_start couplm_rel1_end rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr marr_history_rank if ever_int==1

forvalues m=1/4{
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

replace master_rel_type = marr2_type if rel_no==2 & in_couply==0 & inlist(num_history,2,3,4) // but only have a marriage2 if more than 1 in history
replace master_how_end = marr2_how_end if rel_no==2 & in_couply==0 & inlist(num_history,2,3,4)
replace master_start_yr = marr2_start if rel_no==2 & in_couply==0 & inlist(num_history,2,3,4)
replace master_end_yr = marr2_end if rel_no==2 & in_couply==0 & inlist(num_history,2,3,4)

replace master_rel_type = marr3_type if rel_no==3 & in_couply==0 & inlist(num_history,3,4)
replace master_how_end = marr3_how_end if rel_no==3 & in_couply==0 & inlist(num_history,3,4)
replace master_start_yr = marr3_start if rel_no==3 & in_couply==0 & inlist(num_history,3,4)
replace master_end_yr = marr3_end if rel_no==3 & in_couply==0 & inlist(num_history,3,4)

replace master_rel_type = marr4_type if rel_no==4 & in_couply==0 & num_history==4
replace master_how_end = marr4_how_end if rel_no==4 & in_couply==0 & num_history==4
replace master_start_yr = marr4_start if rel_no==4 & in_couply==0 & num_history==4
replace master_end_yr = marr4_end if rel_no==4 & in_couply==0 & num_history==4

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

forvalues r=1/7{
	local s=`r' + 4
	
	replace master_rel_type = rhm_rel_type`r' if rel_no==`s' & in_couply==0 & num_history==4 // if num_history == 4 then the first chm goes in 5
	replace master_how_end = rhm_how_end`r' if rel_no==`s' & in_couply==0 & num_history==4
	replace master_start_yr = rhm_beginy`r' if rel_no==`s' & in_couply==0 & num_history==4 
	replace master_end_yr = rhm_endy`r' if rel_no==`s' & in_couply==0 & num_history==4
}

// now need to adjust all start dates gah
replace master_start_yr = couplem_rel1_start_real if rel_no==2 & in_couply==0 & num_history==1 & first_chm_lc ==1
replace master_start_yr = couplem_rel1_start_real if rel_no==3 & in_couply==0 & num_history==2 & first_chm_lc ==1
replace master_start_yr = couplem_rel1_start_real if rel_no==4 & in_couply==0 & num_history==3 & first_chm_lc ==1
replace master_start_yr = couplem_rel1_start_real if rel_no==5 & in_couply==0 & num_history==4 & first_chm_lc ==1

* Oh, need to figure out how to add partner_id ESPECIALLY if not using couplm. do I need this? let's come back to this, think I can add on later based on matching information from the wide files...

browse pid rel_no rel_no_adjusted first_chm_lc in_couply num_history master_rel_type master_how_end master_start_yr master_end_yr rhy_rel_type rhy_start_yr rhy_end_yr rhm_rel_type rhm_beginy couplem_rel1_start_real rhm_endy rhm_partnr mhy_married mhy_start_yr mhy_end_yr 

label values master_rel_type rel_type
label values master_how_end how_end

* now create a file to save
keep pid rel_no ever_int firstyr_survey_pl lastyr_survey_pl firstyr_contact_pl lastyr_contact_pl in_couply in_couplm mhy_ever_marr in_marsy couplm_rel1_start couplem_rel1_start_real couplm_rel1_end master_rel_type master_how_end master_start_yr master_end_yr first_chm_lc

reshape wide master_rel_type master_how_end master_start_yr master_end_yr, j(rel_no) i(pid)
browse pid mhy_ever_marr master_rel_type* master_how_end* master_start_yr* master_end_yr*

tab mhy_ever_marr master_rel_type1 if ever_int==1, m

save "$created_data/consolidated_rel_history.dta", replace

********************************************************************************
* Then want to take this info and add to ppathl to get full file of information
* from 1984 - 2023 for all people (with all relationship start / end dates / 
* statuses / partner ids, etc.)
********************************************************************************
use "$temp/ppathl_cleaned.dta", clear
label language EN

tab syear partnered_pl, m row // is cohab covered all years? in theory, but incidence very low pre like 1996/97. Is that real incidence or survey issue? This also feels like it has been improved?
tab status_pl partnered_pl, m row // so, I think they do try to fill in partner status in dropout / no interview years, but the incidence of "not clear" is much higher. I actually feel like this has been improved?
// so maybe *they* try to fill this in using rel history as well? which takes a lot of the work out for me. I just want the START / END date measures here if possible...
// okay, and also, there is only record of a dropout - none of the years after that have rows, so would need to add those and all would missing because not sure
// also not clear if ppathl has PRE survey years. okay it does not
// the codebook says " In unclear cases, due to temporal non-response for instance, we also consider longitudinal information from previous and prospective waves." -- but so this is really only if temp missing
// okay so should I do FILL IN so I have all years for everyone? and then it can be like pre-survey, post-survey
// but then I can use the rel history to possibly at least fill in relationship status / info for those years?

browse pid syear status_pl survey_status_pl partnered_pl partner_id_pl firstyr_survey_pl lastyr_survey_pl
browse pid syear status_pl survey_status_pl partnered_pl partner_id_pl firstyr_survey_pl lastyr_survey_pl if inlist(pid,601,5303,30561901,1088902) // example PIDs I am using

// first do a fill-in so I get years for everyone
gen orig_row = 1 // create a flag so I know which rows I've added
fillin pid syear
replace orig_row = 0 if orig_row==.

foreach var in cid sex_pl birthyr_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl psample_pl lastyr_survey_pl born_germany_pl country_born_pl{
	bysort pid (`var'): replace `var' = `var'[1]
}

sort pid syear
browse pid syear status_pl survey_status_pl partnered_pl partner_id_pl firstyr_contact_pl lastyr_contact_pl
tab status_pl if lastyr_contact_pl == syear, m

gen full_status_pl = status_pl
replace full_status_pl = 0 if syear > lastyr_contact_pl & full_status_pl==.
replace full_status_pl = 0 if syear > lastyr_survey_pl & full_status_pl==.
replace full_status_pl = -1 if syear < firstyr_contact_pl & full_status_pl==.

label define full_status -1 "pre-survey" 0 "dropout" 1 "sample" 2 "youth" 3 "no int"
label values full_status_pl full_status
tab full_status_pl, m

browse pid syear status_pl full_status_pl survey_status_pl partnered_pl partner_id_pl firstyr_contact_pl lastyr_contact_pl
// browse pid syear status_pl full_status_pl survey_status_pl partnered_pl partner_id_pl firstyr_contact_pl lastyr_contact_pl firstyr_survey_pl lastyr_survey_pl if full_status_pl==.
// browse pid syear status_pl full_status_pl survey_status_pl partnered_pl partner_id_pl firstyr_contact_pl lastyr_contact_pl firstyr_survey_pl lastyr_survey_pl if inlist(pid,236704,314002,5603202)

// now merge on full relationship history so I can start to fill in the information
merge m:1 pid using "$created_data/consolidated_rel_history.dta"
drop _merge

tab ever_int master_rel_type1, m
browse if (ever_int==0 & master_rel_type1!=.) // not sure about this people seemingly with no interview? I wonder if they are just identified as spouses? so we have that info but nothing else? let's keep for now
keep if ever_int==1 | (ever_int==0 & master_rel_type1!=.) 
// drop if ever_int == 0 // if never interviewed, we a. don't care about them and b. don't have any of this info

tab master_start_yr1 master_rel_type1, m col // okay so there is info missing, especially cohab (about 12%)
tab master_start_yr1 master_rel_type1 if first_chm_lc==0, m col // yes this is way better
tab master_end_yr1 master_rel_type1, m col // end date is much more reliable. should I *just* use end date?

// think I need to create new versions of the start yr that include these that might be right censored
browse pid syear master_rel_type1 master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 first_chm_lc couplm_rel1_start couplm_rel1_end couplem_rel1_start_real
browse pid syear master_rel_type1 master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 first_chm_lc couplm_rel1_start couplm_rel1_end couplem_rel1_start_real if master_rel_type1!=.

// inspect master_start_yr1 if master_rel_type1!=.
// inspect couplm_rel1_start if master_rel_type1!=.
// inspect couplem_rel1_start_real if master_rel_type1!=.
// inspect couplem_rel1_start_real if couplm_rel1_start!=.

gen couplm_rel1_miss = .
replace couplm_rel1_miss = 0 if couplm_rel1_start!=. & couplem_rel1_start_real!=.
replace couplm_rel1_miss = 1 if couplm_rel1_start!=. & couplem_rel1_start_real==.
tab  first_chm_lc couplm_rel1_miss, m

forvalues m=1/10{
	gen master_start_yr`m'_miss = 0
	replace master_start_yr`m'_miss = 1 if master_start_yr`m'==. & master_rel_type`m'!=.
	
	gen master_start_yr`m'_lc = master_start_yr`m' // first make a copy of existing variable
	
	replace master_start_yr`m' = couplm_rel1_start if first_chm_lc==1 & master_start_yr`m'==. & master_end_yr`m' == couplm_rel1_end & master_end_yr`m'!=.
}

// might need to add a flag - like is this rel left censored? come back to this (7/3/25) - bc also possible I can fill this in once I match couples - one might have real info and the other may not - so all hope is not YET lost. only do this if BOTH PARTNERS have left censoring - but we might be able to recover info this way

tab master_start_yr1 master_rel_type1, m col // k yes this is way better
tab master_start_yr1_lc master_rel_type1, m col

// now attempt to match on current rel number
gen current_rel_number=.
forvalues r=1/10{
	capture replace current_rel_number = `r' if current_rel_number==. & syear >= master_start_yr`r' & syear<= master_end_yr`r' // prio previous relationship if they overlap
}

tab current_rel_number, m
tab current_rel_number partnered_pl, m // still not perfect but I think better
tab current_rel_number master_rel_type1, m
tab full_status_pl if inrange(partnered_pl,1,4) & current_rel_number==. // yes, it used to be like 20% in sample, now is only 10%

browse pid syear current_rel_number partnered_pl master_rel_type1 master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2
// browse pid syear current_rel_number partnered_pl master_rel_type1 master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 if inlist(partnered_pl,1,2) & current_rel_number==. 

gen current_rel_type=.
gen current_rel_how_end=.
gen current_rel_start_yr=.
gen current_rel_end_yr=.
gen current_rel_start_miss=.

forvalues r=1/10{
	replace current_rel_type = master_rel_type`r' if current_rel_number==`r'
	replace current_rel_how_end = master_how_end`r' if current_rel_number==`r'
	replace current_rel_start_yr = master_start_yr`r' if current_rel_number==`r'
	replace current_rel_end_yr = master_end_yr`r' if current_rel_number==`r'
	replace current_rel_start_miss = master_start_yr`r'_miss if current_rel_number==`r'
}

label values current_rel_type rel_type
label values current_rel_how_end how_end

browse pid syear current_rel_number partnered_pl partner_id_pl current_rel_type current_rel_how_end current_rel_start_yr current_rel_end_yr full_status_pl // master_rel_type1 master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2
	// browse pid syear current_rel_number partnered_pl partner_id_pl current_rel_type current_rel_how_end current_rel_start_yr current_rel_end_yr full_status_pl master_rel_type1 master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 ///
	// if inrange(partnered_pl,1,4) & current_rel_number==.
	tab partnered_pl if inrange(partnered_pl,1,4) & current_rel_number==. & full_status_pl==1, m
	// browse pid syear current_rel_number partnered_pl partner_id_pl current_rel_type current_rel_how_end current_rel_start_yr current_rel_end_yr full_status_pl master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 master_start_yr3 master_end_yr3 couplm_rel1_start couplem_rel1_start_real if inlist(pid,103,602,2202,6301)
	// some it just feels like i am lacking this info from the history. others, it seems like year right before or after the relationship, so maybe that is just a move in / relationship recording issue? let's see what happens when I match partners here as well because some of this may resolve itself...
	
inspect partner_id_pl if inlist(partnered_pl,1,2)

// merge on biocoupm as well and attempt to fill in partner_id? (especially where me and ppathl diverge in terms of being in a relationship)
merge m:1 pid using "$temp/biocouplm_wide.dta", keepusing(rhm_partnr* rhm_beginy* rhm_endy* rhm_rel_type*)
drop if _merge==2
drop _merge

browse pid syear current_rel_number partnered_pl partner_id_pl current_rel_type current_rel_how_end current_rel_start_yr current_rel_end_yr couplem_rel1_start_real rhm_partnr1 rhm_beginy1 rhm_endy1 rhm_partnr2 rhm_beginy2 rhm_endy2

gen long partner_id_rhm = .
replace partner_id_rhm = rhm_partnr1 if syear >=couplem_rel1_start_real & syear <=rhm_endy1

forvalues r=2/9{
	replace partner_id_rhm = rhm_partnr`r' if partner_id_rhm==. & syear >=rhm_beginy`r' & syear <=rhm_endy`r'
}

browse pid syear current_rel_number partnered_pl partner_id_pl partner_id_rhm current_rel_type current_rel_how_end current_rel_start_yr current_rel_end_yr couplem_rel1_start_real rhm_partnr1 rhm_beginy1 rhm_endy1 rhm_partnr2 rhm_beginy2 rhm_endy2

gen id_check = .
replace id_check=0 if partner_id_pl!=. & partner_id_rhm!=. & partner_id_pl!=partner_id_rhm
replace id_check=1 if partner_id_pl!=. & partner_id_rhm!=. & partner_id_pl==partner_id_rhm

tab id_check, m
// tab partner_id_pl if id_check==0, m // this is mostly because of .n -- since v40, there are now too many options 
inspect partner_id_pl if id_check==0 // only 12% are missing
tab partner_id_rhm if id_check==0, m 
inspect partner_id_rhm if id_check==0 // okay close to 97% are bc partner_id_rhm is -2 - they said they made some changes to the partner ids in ppathl so maybe this why
// is this also because they only have partner id if current. it's seem as though it has gotten worse between v39 and v40 but I can't figure out why from the "what's new" - I think I can just use the info from ppathl anyway.

// it's possible this is also bc of the dropout years that a partner is recorded - that is why pl partner id will be filled in but the rhm will be -2
tab full_status_pl if id_check==0, m // okay that does not explain it at all 

********************************************************************************
* Attempting to fill in missing info (this was previously in step 2 but moving)
********************************************************************************

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

* Can I also figure out the relationship number?
tab current_rel_number if inrange(partnered_pl,1,4), m

browse pid syear status_pl partnered_pl partner_id_pl current_rel_number current_rel_start_yr current_rel_start_est current_rel_end_yr master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 rhm_beginy1 rhm_endy1 rhm_beginy2 rhm_endy2 if inlist(partnered_pl,1,2) & current_rel_number==. 

sort pid syear
replace current_rel_number = current_rel_number[_n-1] if current_rel_number==. & inrange(partnered_pl,1,4) & pid==pid[_n-1] & syear==syear[_n-1]+1 & partner_id_pl==partner_id_pl[_n-1] // & current_rel_start_yr==current_rel_start_yr[_n-1] // the dropout years causing problems

gen rel_counter=0 // see ukhls step c (around row 900)
forvalues r=1/10{
	replace rel_counter = rel_counter + 1 if master_start_yr`r' <= syear // this is meant to cover all relationship types
}

gen rel_no_est = current_rel_number
replace rel_no_est = rel_counter + 1 if rel_no_est==. & inrange(partnered_pl,1,4) // estimate

gen current_rel_number_v0 = current_rel_number
replace current_rel_number = rel_no_est if current_rel_number==.

sort pid syear
browse pid syear status_pl partnered_pl partner_id_pl current_rel_number current_rel_start_yr current_rel_start_est current_rel_end_yr master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 rhm_beginy1 rhm_endy1 rhm_beginy2 rhm_endy2 if inlist(partnered_pl,1,2) & current_rel_number_v0==. 

browse pid syear status_pl partnered_pl partner_id_pl current_rel_number current_rel_number_v0 current_rel_start_yr current_rel_start_est current_rel_end_yr master_start_yr1 master_end_yr1 master_start_yr2 master_end_yr2 rhm_beginy1 rhm_endy1 rhm_beginy2 rhm_endy2 if inlist(pid,103,6301, 25821002,26223302)

// clean up file to make it smaller
drop rhm_partnr* rhm_beginy* rhm_endy* rhm_rel_type* master_rel_type* master_how_end* master_start_yr* master_end_yr* // couplm_rel1_start couplem_rel1_start_real first_chm_lc

save "$created_data/ppathl_partnership_history.dta", replace

// i still think relationship info needs to be adjusted if couples transition from cohab to marriage, but we will return to this later (note as of 6/26/25)
// as with the many notes I added 7/3 - I think a lot will also need to be adjusted once I match partners - to fix missing / incongruent relationship info

********************************************************************************
********************************************************************************
********************************************************************************
**# Fertility history files
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Basic first / last birth info
********************************************************************************

// for births AFTER marriage (or - births relative to marriage)
// Also need to see if I can also use this to figure out mpf and other related created fertility measures I plan to make
// but this does not have other parent id. so, like with other datasets, I need to use the child id and then somehow link both parents to get mom and dad??
// does bioagel? that *might* just be moms? and this is more about like childrearing experiences as the child ages

use "$GSOEP/biobirth.dta", clear
label language EN

mvdecode _all, mv(-9/-1) // here want regular missing for all that will be easier

unique pid // 198140, 198140 -- so this is one record per PID so more of 1:1 lookup - basically matches number in ppathl
tab biovalid, m // there are not a lot of people with valid info - is this an age thing? (like close to 64% have no bio info)
tab bioyear, m
tab gebjahr biovalid, row // so no one born in 2000 or later has info, so some of this is age but coverage still not great
// so according to the codebook, the universe is only people who ever successfully completed a biographical interview BUT i think they try to infer info from people without a valid interview
// also, men's fertility was only collected starting in 2001
tab sumkids, m // like here, only 20% are does not apply
tab biokids, m // so the diff between sumkids and biokids is that this is DIRECTLY reported. the above is inferred

gen any_births=.
replace any_births = 0 if sumkids==0
replace any_births = 1 if sumkids > 0 & sumkids<100
tab sumkids any_births, m
tab  gebjahr any_births, m row // so inferred coverage is somewhat correlated with age but not entirely

browse pid sex kidgeb* // wait are these in REVERSE order? like year of birth first child seems LATER than year of birth second + child (when more than 1...)
// lol yes WTF: The order ranks from the oldest child specified under KIDPNR01 to the youngest child.

tab kidgeb01 any_births, m // there is a non-neglible amount of does not apply for those with births. need to see if coverage here is the same as biol (bc that also records year of first birth)

egen first_birth_year = rowmin(kidgeb*)
replace first_birth_year=9999 if first_birth_year == . & any_births==0

browse pid first_birth_year kidgeb*

tab first_birth_year any_births, m // can we also assume that people with a first birth year but missing on any births have a birth actually?

egen last_birth_year = rowmax(kidgeb*)
replace last_birth_year=9999 if last_birth_year == . & any_births==0
tab last_birth_year any_births, m
browse  pid first_birth_year last_birth_year kidgeb*

keep pid biovalid bioinfo sumkids biokids any_births first_birth_year last_birth_year

foreach var in biovalid bioinfo sumkids biokids any_births first_birth_year last_birth_year{
	rename `var' `var'_bh // bh for birth history (bb feels unclear)
}

save "$temp/biobirth_cleaned.dta", replace

********************************************************************************
* Trying to get fertility history re: mpf
********************************************************************************
// following UKHLS as the structure is very similar
// AH jk the US is a little different because they have bioparent IDs. that is not true here. so..actually need to adapt

use "$GSOEP/biobirth.dta", clear
label language EN

mvdecode _all, mv(-9/-1) // here want regular missing for all that will be easier

// need to figure out how to get a version where KID is the focal and I can match on both of their parents
browse pid sex sumkids biokids kidpnr01 kidpnr02 kidpnr03

keep pid sex sumkids kidpnr*

forvalues k=1/9{
	rename kidpnr0`k' kidpnr`k' // reshape isn't working with leading 0s
}

reshape long kidpnr, i(pid sex) j(child_no)
drop if kidpnr == . & inrange(child_no,2,19) // so keep one record for all, even if not parent

unique kidpnr if kidpnr!=. // there is average 1.8 rows per kid. So, ideally this means no more than 1 parent per kid?
bysort kidpnr: egen num_parents = count(pid) if kidpnr !=.
tab num_parents, m
tab sex if kidpnr != . & num_parents==2 // okay, generally, I think there is just one parent of each gender basically

gen long mom_id = pid if sex==2 & kidpnr!=.
gen long dad_id = pid if sex==1 & kidpnr!=.
bysort kidpnr (mom_id): replace mom_id = mom_id[1]
bysort kidpnr (dad_id): replace dad_id = dad_id[1]

sort kidpnr
browse kidpnr num_parents pid sex mom_id dad_id

// so now we have both parent iDs, but need to figure out which they are
gen is_mom=0
replace is_mom=1 if pid == mom_id & mom_id!=.

gen is_dad=0
replace is_dad=1 if pid == dad_id & dad_id!=.

tab sex is_mom, m // validate
tab sex is_dad, m
tab is_mom is_dad, m

gen which_parent=.
replace which_parent = 1 if is_mom==1
replace which_parent = 2 if is_dad==1

label define which_parent 1 "Mom" 2 "Dad"
label values which_parent which_parent

gen long other_parent_id=.
replace other_parent_id = dad_id if which_parent==1
replace other_parent_id = mom_id if which_parent==2

inspect other_parent_id if which_parent!=.
inspect other_parent_id if which_parent!=. & num_parents==2 // so some are 0s because just 1 parent listed

sort pid child_no
browse pid sex child_no kidpnr which_parent other_parent_id

unique other_parent_id if other_parent_id!=0 & other_parent_id!=., by(pid) gen(num_birth_partners)
bysort pid (num_birth_partners): replace num_birth_partners = num_birth_partners[1]
tab num_birth_partners, m

browse pid sex child_no kidpnr which_parent other_parent_id num_birth_partners

gen any_mpf = .
replace any_mpf = 0 if num_birth_partners==0 | num_birth_partners==1
replace any_mpf = 1 if num_birth_partners>1 & num_birth_partners<1000

save "$temp/biobirth_parent_child_ids.dta", replace 

tab sumkids any_mpf, m 

preserve
collapse (max) any_mpf, by(pid sumkids)
tab sumkids any_mpf, m

save "$temp/mpf_lookup.dta", replace 
restore

********************************************************************************
********************************************************************************
********************************************************************************
**# Other household composition info
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Trying to get age of youngest / oldest child in HH
* OH and I could also possibly use this to get number of people over 65?
* (do I need this since I have actual care hours? Feel like I did this for US
* as proxy for caretaking responsibilities)
********************************************************************************
use "$temp/ppathl_cleaned.dta", clear

// first need to create an age variable
browse pid syear birthyr_pl
gen age = syear - birthyr_pl if birthyr_pl!=.
replace age = 0 if age==-1

browse hid pid syear age

// composition variables
gen kidsu18=0
replace kidsu18 =1 if age < 18

gen kidsu6=0
replace kidsu6 =1 if age < 6

gen age65up=0
replace age65up =1 if  age >=65 & age<200

bysort syear hid: egen kidsu18_hh = total(kidsu18)
tab kidsu18_hh, m

bysort syear hid: egen kidsu6_hh = total(kidsu6)
tab kidsu6_hh, m

bysort syear hid: egen num_65up_hh = total(age65up)
tab num_65up_hh, m

sort hid syear
browse hid pid syear age kidsu18_hh kidsu6_hh num_65up_hh kidsu18 kidsu6 age65up

// can I get age of youngest / oldest?
bysort syear hid: egen age_youngest_child = min(age) if kidsu18==1
bysort syear hid: egen age_oldest_child = max(age) if kidsu18==1
bysort syear hid (age_youngest_child): replace age_youngest_child=age_youngest_child[1]
bysort syear hid (age_oldest_child): replace age_oldest_child=age_oldest_child[1]

tab age_youngest_child kidsu18_hh
tab age_oldest_child kidsu18_hh

sort hid syear
browse hid pid syear age age_youngest_child age_oldest_child kidsu18_hh kidsu6_hh kidsu18 kidsu6

// create lookup files
preserve
drop if hid == . | hid==.n | hid==.s

collapse (max) kidsu18_hh kidsu6_hh num_65up_hh age_oldest_child ///
		(min) age_youngest_child (sum) kidsu18 kidsu6 age65up, by(hid syear) // this at HH level

save "$temp/hh_comp_lookup.dta", replace

restore

********************************************************************************
* Attempting to use mother / father id and / or relationship info
* aka parent / child to get a sense of whether mother / father in HH
* Variables provided by SOEP don't have great coverage so want to see if 
* I can supplement here (this is following PSID code I wrote)
********************************************************************************
use "$temp/pbrutto_cleaned.dta", clear // this file has the relationship info

tabstat relation_pb relation_v1_pb relation_v2_pb relation_v3_pb relation_v4_pb, by(syear)
label values relation_pb stell_h 
tab relation_pb // this might not be that helpful because this isnt a relationship matrix; so only useful if parent of HH head is in the HH, but you don't know if they are parent of someone else...

gen hh_relation_gp = .
replace hh_relation_gp = 0 if relation_pb == 0
replace hh_relation_gp = 1 if inlist(relation_pb,11,12,13)
replace hh_relation_gp = 2 if inrange(relation_pb,20,24)
replace hh_relation_gp = 3 if inrange(relation_pb,30,35)
replace hh_relation_gp = 4 if inrange(relation_pb,25,27)
replace hh_relation_gp = 4 if inrange(relation_pb,36,71)
replace hh_relation_gp = 99 if relation_pb==99

label define relation 0 "HH Head" 1 "Partner" 2 "Child" 3 "Parent" 4 "Other Rel / Non-rel" 99 "Unknown"
label values hh_relation_gp relation

merge m:1 pid using "$temp/bioparen_cleaned.dta"
drop if _merge==2
tab survey_status_pb _merge, m row
drop _merge

gen age = syear - birthyr_pb

browse pid syear hid age hh_relation_gp father_pid_bp mother_pid_bp

// browse pid syear hid cid age hh_relation_gp father_pid_bp mother_pid_bp if hid==.s
// browse pid syear hid cid age hh_relation_gp father_pid_bp mother_pid_bp if inlist(pid, 46803,588503)
// browse pid syear hid cid age hh_relation_gp father_pid_bp mother_pid_bp if inlist(cid, 27, 60, 175, 4685, 40126, 58858)

drop if hid==.s // wanted to see if I could use cid but that feels risky too

// count how many distinct mother / father IDs in HH
quietly unique father_pid_bp if father_pid_bp!=., by(hid syear) gen(num_fathers)
bysort hid syear (num_fathers): replace num_fathers=num_fathers[1]
tab num_fathers, m

quietly unique mother_pid_bp if mother_pid_bp!=., by(hid syear) gen(num_mothers)
bysort hid syear (num_mothers): replace num_mothers=num_mothers[1]
tab num_mothers, m // up to 3 fathers and 4 mothers

bysort hid syear (father_pid_bp): gen father_no = sum(father_pid_bp != father_pid_bp[_n-1]) if father_pid_bp!=.
bysort hid syear (mother_pid_bp): gen mother_no = sum(mother_pid_bp != mother_pid_bp[_n-1]) if mother_pid_bp!=.

forvalues n=1/4{
	gen father_no`n' = father_pid_bp if father_no==`n'
	bysort hid syear (father_no`n'): replace father_no`n'=father_no`n'[1]
	gen mother_no`n' = mother_pid_bp if mother_no==`n'
	bysort hid syear (mother_no`n'): replace mother_no`n'=mother_no`n'[1]
}

forvalues n=1/4{
	capture drop father`n'_in_hh
	capture drop mother`n'_in_hh

	capture gen father`n'_in_hh = .
	replace father`n'_in_hh = 1  if pid == father_no`n'
	bysort hid syear (father`n'_in_hh): replace father`n'_in_hh=father`n'_in_hh[1]
	capture gen mother`n'_in_hh = .
	replace mother`n'_in_hh = 1  if pid == mother_no`n'
	bysort hid syear (mother`n'_in_hh): replace mother`n'_in_hh=mother`n'_in_hh[1]
}

browse pid syear hid age hh_relation_gp num_fathers father*_in_hh father_no* father_pid_bp mother*_in_hh mother_no* num_mothers mother_pid_bp

// now want to designate if that person's father / mother specifically is in HH
gen father_in_hh = 0
gen mother_in_hh = 0

forvalues n=1/4{
	replace father_in_hh = 1 if father_no`n' == father_pid_bp & father`n'_in_hh == 1
	replace mother_in_hh = 1 if mother_no`n' == mother_pid_bp & mother`n'_in_hh == 1
}

browse pid syear hid age hh_relation_gp father_in_hh father_pid_bp num_fathers father*_in_hh father_no*  mother_in_hh mother_pid_bp mother*_in_hh mother_no* num_mothers 

// create lookup file
preserve

collapse (max) father_in_hh mother_in_hh, by(pid syear)
save "$temp/parent_coresidence_lookup.dta", replace

restore