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

********************************************************************************
* Let's see what i can get from ppathl (bc has status + partner id)
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

