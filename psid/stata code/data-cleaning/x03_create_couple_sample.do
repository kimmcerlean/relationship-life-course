********************************************************************************
********************************************************************************
* Project: Relationship Growth Curves
* Owner: Kimberly McErlean
* Started: September 2024
* File: create sample
********************************************************************************
********************************************************************************

// OLD CREATE COUPLE FILE. Brought back July 2026 for troubleshooting.
// I identified 60% of new couples in the partners file created here and can understand why I have them and this fle did not (namely min_dur)
// However, trying to identify the remaining 40%-  are they not labeled as head / wife? did I recover extra info from the family matrix I should have used? etc.
// confirmed: combo of first year cohabitors (not classified in marital pairs or marital status defacto, even for head - considered SINGLE) and non head wives

********************************************************************************
* Description
********************************************************************************
* This files restricts the full PSID data to the analytical sample
* (cohabiting / married couples)

/*how to identify cohabitors (from FAQ)
Prior to 2017, when a new (opposite sex) romantic partner of Head ('Reference Person' starting in the 2017 wave) moved into the FU (family unit), but had been living there less than 1 year at the time of the interview, that person was labeled a Boyfriend or Girlfriend (code 88). However, if the cohabitor had been living in the FU one year or more, the couple was designated (male)Head and "Wife" (code 22 from 1983 on). If a Girlfriend or Boyfriend was still in the FU in the next wave, and the couple were not married, they became (male) Head and "Wife". If the person who moves in is married to the Head, they are of course, male Head and Wife (code 20), regardless of time living in the FU.

Boyfriends and Girlfriends are treated like other family members who are not Reference Person (`Head' prior to 2017), Spouse or Partner. Considerably less information is obtained about them. In the waves since the late 1970s, information typically gathered for a Spouse has been gathered as well about a Partner ("Wife" before 2017).

Prior to 1983, the Relationship to Head ('Reference Person' starting in the 2017 wave) codes did not distinguish between legal Wives and long-term female cohabitors. However, first year cohabitors can be detected prior to 1983 with a little bit of work. For example, their Relationship to Head would be 8 (nonrelative), their gender would be the opposite of Head's, and in subsequent years they may become Wives or Heads, while the Head would stay as Head or become a Wife. Anyone fitting this pattern can be decisively identified as a cohabitor. PSID did not distinctively label same sex cohabitors prior to 2017.
*/

/*
import excel "C:\Users\kmcerlea\Downloads\couple checks.xlsx", sheet("Sheet3") firstrow
gen flag = 1
save "$temp/missing_ids.dta", replace
*/

********************************************************************************
* import data and clean up sample
********************************************************************************
use "$temp/PSID_full_long.dta", clear

merge m:1 unique_id using "$temp/missing_ids.dta"
tab flag _merge, m
drop _merge

sort unique_id survey_yr

replace SEQ_NUMBER_=0 if SEQ_NUMBER==.
bysort id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)
drop if in_sample==0 // people with NO DATA in any year

browse unique_id main_per_id survey_yr SEQ_NUMBER_

gen year = survey_yr if SEQ_NUMBER_!=0

bysort unique_id (year): egen first_survey_yr = min(year)
bysort unique_id (year): egen last_survey_yr = max(year)

sort unique_id survey_yr
browse unique_id main_per_id survey_yr SEQ_NUMBER_ SAMPLE first_survey_yr last_survey_yr YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST PERMANENT_ATTRITION ANY_ATTRITION

keep if SEQ_NUMBER_!=0 | SAMPLE==1 // dropping non-sample years
drop if SEQ_NUMBER_==0 & survey_yr!=1968

tab first_survey_yr if SAMPLE==1
browse unique_id main_per_id survey_yr SEQ_NUMBER_ SAMPLE first_survey_yr last_survey_yr if SAMPLE==1 & first_survey_yr!=1968
replace first_survey_yr = 1968 if SAMPLE==1 & first_survey_yr==1969
drop if survey_yr==1968 & first_survey_yr!=1968

// want consecutive waves to make some things easier later
egen wave = group(survey_yr)

********************************************************************************
* Identify couples including ref person
********************************************************************************
browse survey_yr RELATION_ RELATION_TO_HEAD_
// RELATION_: pre 1983 - 1=head; 2=wife(but think this includes cohabitors)
// RELATION: starting in 1983 - 10=head; 20=legal wife; 22=cohabitor
// RELATION_TO_HEAD is family level, I am not sure - is this maybe like is the head the same?
// marital status ref - added in 1977, treats cohabitors as married? no I think opposite - legally married onlys (see v5502)
// marital status head - has always been asked, treats cohabitors as married I think? (see v5650)

label define marr_defacto 1 "Partnered" 2 "Single" 3 "Widowed" 4 "Divorced" 5 "Separated"
label values MARST_DEFACTO_HEAD_ marr_defacto

label define marr_legal 1 "Married" 2 "Single" 3 "Widowed" 4 "Divorced" 5 "Separated"
label values MARST_LEGAL_HEAD_ marr_legal

label define couple_status 1 "Married" 2 "Partnered" 3 "Uncooperative" 4 "FY Partnered" 5 "Unpartnered"
label values COUPLE_STATUS_HEAD_ couple_status

gen person=0
replace person=1 if RELATION_==1 & survey_yr<1983
replace person=1 if RELATION_==10 & survey_yr>=1983
replace person=2 if RELATION_==2 & survey_yr<1983
replace person=2 if inlist(RELATION_,20,22) & survey_yr>=1983

tab MARITAL_PAIRS_ person, m
gen cohab_est=0
replace cohab_est=1 if MARST_DEFACTO_HEAD_==1 & inlist(MARST_LEGAL_HEAD_,2,3,4,5) // will only apply after 1977
tab RELATION_ cohab_est if survey_yr>=1977
tab MARST_DEFACTO_HEAD_ cohab_est
tab MARST_LEGAL_HEAD_ cohab_est

gen marital_status_updated=.
replace marital_status_updated=1 if MARST_DEFACTO_HEAD_==1 & cohab_est==0
replace marital_status_updated=2 if MARST_DEFACTO_HEAD_==1 & cohab_est==1
replace marital_status_updated=3 if MARST_DEFACTO_HEAD_==2
replace marital_status_updated=4 if MARST_DEFACTO_HEAD_==3
replace marital_status_updated=5 if MARST_DEFACTO_HEAD_==4
replace marital_status_updated=6 if MARST_DEFACTO_HEAD_==5

label define marital_status_updated 1 "Married (or pre77)" 2 "Partnered" 3 "Single" 4 "Widowed" 5 "Divorced" 6 "Separated"
label values marital_status_updated marital_status_updated

tab survey_yr marital_status_updated, row

browse unique_id survey_yr person RELATION_ COUPLE_STATUS_HEAD_ MARITAL_PAIRS_  marital_status_updated if inlist(person,1,2) // so after 1977 (aka 1977-1983) can perhaps identify cohabitors if husband is not legally married? and validate in 1983 when actually tracked?

// Identify relationship transitions
sort unique_id survey_yr
browse unique_id survey_yr wave

*enter
gen rel_start=0
replace rel_start=1 if (inlist(marital_status_updated,1,2) & inlist(marital_status_updated[_n-1],3,4,5,6)) & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

*exit
gen rel_end=0
replace rel_end=1 if (inlist(marital_status_updated,3,4,5,6) & inlist(marital_status_updated[_n-1],1,2)) & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen rel_end_pre=0
replace rel_end_pre=1 if (inlist(marital_status_updated,1,2) & inlist(marital_status_updated[_n+1],3,4,5,6)) & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

*cohab to marr
gen marr_trans=0
replace marr_trans=1 if (marital_status_updated==1 & marital_status_updated[_n-1]==2) & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

browse unique_id survey_yr person RELATION_ AGE_INDV_ marital_status_updated MARITAL_PAIRS_ COUPLE_STATUS_HEAD_ rel_start rel_end rel_end_pre marr_trans

/* July 2026 troubleshooting - confirmed I was basically excluding first yr cohabitors
browse unique_id survey_yr person RELATION_ AGE_INDV_ marital_status_updated MARITAL_PAIRS_ COUPLE_STATUS_HEAD_ rel_start rel_end rel_end_pre marr_trans if inlist(unique_id, 4034,10032,45186,89182,317034,780175,2004180,6064196,6232037,8628170)
tab MARITAL_PAIRS_ if flag==1, m
tab person if flag==1, m
tab MARITAL_PAIRS_ person if flag==1, m cell
tab marital_status_updated if flag==1, m 
tab RELATION_ if flag==1, m
tab RELATION_ if person==0 & flag==1

merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 ego_rel alter_rel partner_unique_id rel_num marr_num)
tab MARITAL_PAIRS_ _merge, m // so those in marital pairs have basically perfect match
tab flag _merge, m
tab flag if MARITAL_PAIRS_==0 & _merge==3, m

drop if _merge==2
drop _merge

browse unique_id partner_unique_id survey_yr person RELATION_ AGE_INDV_ marital_status_updated MX8  MARITAL_PAIRS_ COUPLE_STATUS_HEAD_ rel_start rel_end rel_end_pre marr_trans if flag==1
tab person if flag==1 & MARITAL_PAIRS!=0

tab MARITAL_PAIRS_ MX8,m // I think it's these first year cohabitors really

unique unique_id if flag==1
unique unique_id if flag==1 & MARITAL_PAIRS!=0 // but they don't all have records when marital pairs is not 0
unique unique_id if flag==1 & MX8!=. // so yes, they all have at least some records when MX8 is missing

gen is_first_yr_cohabitor = 0
replace is_first_yr_cohabitor = 1 if RELATION_==88

tab flag is_first_yr_cohabitor, m

browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ RELATION_ is_first_yr_cohabitor // 1887178

bysort FAMILY_INTERVIEW_NUM_ survey_yr: egen has_first_yr_cohabitor = max(is_first_yr_cohabitor)
bysort unique_id: egen ever_first_yr_cohabitor = max (has_first_yr_cohabitor)

tab flag ever_first_yr_cohabitor, m row
tab has_first_yr_cohabitor if MARITAL_PAIRS==0 & MX8!=., m
tab ever_first_yr_cohabitor if flag==1 & MX8!=., m
tab MARITAL_PAIRS if flag==1 & ever_first_yr_cohabitor==1 & MX8!=., m

tab person if MARITAL_PAIRS ==0 & inlist(marital_status_updated,1,2)
tab flag if MARITAL_PAIRS ==0 & inlist(marital_status_updated,1,2), m

tab marital_status_updated if has_first_yr_cohabitor, m // so this is ALSO the problem. these are flagged as NOT PARTNERED
tab MARITAL_PAIRS if has_first_yr_cohabitor, m // yeah so this is why it's removing heads too. beacuse they aren't considered "married" or in a marital pair.
tab cohab_est if has_first_yr_cohabitor, m // yup yup so these are all problems

gen drop_sample = 0
replace drop_sample = 1 if person==0
replace drop_sample = 2 if drop_sample==0 & inlist(marital_status_updated,3,4,5,6)

tab flag drop_sample, m

// so frustrated bc I made all of these notes above and failed to realize that not only are they treated as OFUMs but they aren't considered marital pairs. I got confused bc NON-HEADS aND WIVES CAN BE in marital pairs, so they actually AREN'T treated like other ofums
*/

// drop non-partnered = BUT i think this is household level, so need to also drop the specific individuals not partnered?
keep if inlist(marital_status_updated,1,2) | rel_start==1 | marr_trans==1 | rel_end==1
drop if person==0
// added that pre_rel_end (so I know it was last year married) - so can delete those with marital status that isn't married or partnered
drop if inlist(marital_status_updated,3,4,5,6)

// so might need to restrict to either 1983 or 1977, because better cohab data?
// drop if survey_yr <1977 // first time you could identify cohab

tab survey_yr marital_status_updated
tab survey_yr rel_start
tab survey_yr rel_end_pre
tab survey_yr marr_trans

// tab has_first_yr_cohabitor, m
// tab flag, m
// unique unique_id if flag==1

// okay how to get duration?! next problem GAH

save "$created_data/PSID_partners.dta", replace
