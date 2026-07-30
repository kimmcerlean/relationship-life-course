********************************************************************************
********************************************************************************
* Project: PSID Data Compilation
* Owner: Kimberly McErlean
* Started: September 2024
* Updated significantly: July 2026
* File: master_relationship_history
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files uses the family relationship and observed transitions into and out of 
* relationships and HHs to attempt to create a full relationship history that 
* spans marriage and cohabitation
* Adapted from marriage as gendering paper and Social Forces paper

********************************************************************************
* First try to get marital history data to merge on
********************************************************************************
use "$PSID/mh85_23.dta", clear

gen unique_id = (MH2*1000) + MH3
browse MH3 MH2 unique_id
gen unique_id_spouse = (MH7*1000) + MH8

/* first rename for ease*/
rename MH1 releaseno
rename MH2 fam_id
rename MH3 main_per_id
rename MH4 sex
rename MH5 mo_born
rename MH6 yr_born
rename MH7 fam_id_spouse
rename MH8 per_no_spouse
rename MH9 marrno 
rename MH10 mo_married
rename MH11 yr_married
rename MH12 status
rename MH13 mo_widdiv
rename MH14 yr_widdiv
rename MH15 mo_sep
rename MH16 yr_sep
rename MH17 history
rename MH18 num_marriages
rename MH19 marital_status
rename MH20 num_records

label define status 1 "Intact" 3 "Widow" 4 "Divorce" 5 "Separation" 7 "Other" 8 "DK" 9 "Never Married"
label values status status

egen yr_end = rowmin(yr_widdiv yr_sep)
browse unique_id marrno status yr_widdiv yr_sep yr_end

// this is currently LONG - one record per marriage. want to make WIDE

drop mo_born mo_widdiv yr_widdiv mo_sep yr_sep history
bysort unique_id: egen year_birth = min(yr_born)
drop yr_born

reshape wide unique_id_spouse fam_id_spouse per_no_spouse mo_married yr_married status yr_end, i(unique_id main_per_id fam_id) j(marrno)
gen INTERVIEW_NUM_1968 = fam_id

foreach var in *{
	rename `var' mh_`var' // so I know it came from marital history
}

rename mh_fam_id fam_id
rename mh_main_per_id main_per_id
rename mh_unique_id unique_id
rename mh_year_birth year_birth 
rename mh_INTERVIEW_NUM_1968 INTERVIEW_NUM_1968

save "$temp/marital_history_wide.dta", replace

********************************************************************************
********************************************************************************
**# Use family matrix to get partner list to use later but also to get more accurate info about cohab v. marriage
********************************************************************************
********************************************************************************

use "$PSID/family_matrix_68_23.dta", clear // relationship matrix downloaded from PSID site

unique MX5 MX6 // should match the 85000 in other file, just a touch less

rename MX5 ego_1968_id 
rename MX6 ego_per_num
rename MX2 survey_yr
gen unique_id = (ego_1968_id*1000) + ego_per_num // how they tell you to identify in main file
// egen ego_unique = concat(ego_1968_id ego_per_num), punct(_)
// egen partner_unique = concat(partner_1968_id partner_per_num), punct(_)

recode MX7 (1=1)(2=2)(3/8=3)(9=2)(10=1)(11/19=3)(20/22=2)(23/87=3)(88=2)(89/120=3), gen(ego_rel) // ego relationship to ref. because also only really useful if one is reference person bc otherwise i don't get a ton of info about them
recode MX12 (1=1)(2=2)(3/8=3)(9=2)(10=1)(11/19=3)(20/22=2)(23/87=3)(88=2)(89/120=3), gen(alter_rel) // alter relationship to ref
recode MX8 (10=1)(20=2)(22=3)(23/100=4), gen(ego_alter)

label define rels 1 "Ref" 2 "Spouse/Partner" 3 "Other"
label values ego_rel alter_rel rels

label define ego_alter 1 "Ref" 2 "Spouse" 3 "Partner" 4 "Other"
label values ego_alter ego_alter

// for now, will see if splitting types or keeping together makes sense, need to wrap my head around this file
gen cohab_1968_id = MX10 if MX8==22
gen cohab_per_num = MX11 if MX8==22
gen long cohab_unique_id = (cohab_1968_id*1000) + cohab_per_num

gen spouse_1968_id = MX10 if MX8==20
gen spouse_per_num = MX11 if MX8==20
gen long spouse_unique_id = (spouse_1968_id*1000) + spouse_per_num

gen partner_1968_id = MX10 if MX8==22 | MX8==20
gen partner_per_num = MX11 if MX8==22 | MX8==20
gen long partner_unique_id = (partner_1968_id*1000) + partner_per_num

bysort unique_id survey_yr: egen num_partners = count(ego_alter) if inlist(ego_alter, 2,3)
bysort unique_id survey_yr (num_partners): replace num_partners = num_partners[1]

sort unique_id survey_yr 
browse unique_id survey_yr ego_alter partner_unique_id num_partners

tab num_partners if MX8==22 | MX8==20, m
keep if MX8==22 | MX8==20

// want to get relationship order - this is old code but think it is useful. moves a bit beyond the framework of above
// unique partner_unique_id, by(unique_id) gen(rel_num)
// drop rel_num

egen couple_num = group(unique_id partner_unique_id)

//https://www.statalist.org/forums/forum/general-stata-discussion/general/1437910-trying-to-rank-numbers-without-gaps
sort unique_id survey_yr
by unique_id: egen rank = rank(partner_unique_id), track
egen help_var = group(unique_id rank)

bysort unique_id (rank): gen rel_num = sum(rank != rank[_n-1]) if rank != .

// now do same thing specifically for MARRIAGE order
sort unique_id survey_yr
by unique_id: egen marr_rank = rank(partner_unique_id) if MX8==20, track
egen marr_help_var = group(unique_id marr_rank)

bysort unique_id (marr_rank): gen marr_num = sum(marr_rank != marr_rank[_n-1]) if marr_rank != .

sort unique_id survey_yr
browse unique_id survey_yr partner_unique_id ego_alter rel_num marr_num 

drop rank help_var marr_rank marr_help_var

// rogue dual relationship - keeping the iD that remains in subsequent records. From marriage, see if true here.
// drop if ego_1968_id == 1821 & ego_per_num == 170 & survey_yr==1977 & partner_unique_id== 1821004

save "$temp/PSID_relationship_list_tomatch.dta", replace 

********************************************************************************
********************************************************************************
**# Now incorporate to main file to create history
********************************************************************************
********************************************************************************
use "$temp/PSID_full_long.dta", clear // use long data for now, bc easier to manage
egen wave = group(survey_yr) // this will make years consecutive, easier for later

merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 ego_rel alter_rel partner_unique_id rel_num marr_num)
tab MARITAL_PAIRS_ _merge, m // so those in marital pairs have basically perfect match
drop if _merge==2
drop _merge

rename MX8 rel_type

// also merge on marital history
// merge on marital history - bc in order of prio, it should be marital history for marriages observed, then other variables for not in marital history or cohabitation.
merge m:1 unique_id using "$temp/marital_history_wide.dta"
drop if _merge==2

gen in_marital_history=0
replace in_marital_history=1 if _merge==3
drop _merge

***************************************
* Variables
***************************************
gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

gen in_sample=.
replace in_sample=0 if SEQ_NUMBER_==0 | inrange(SEQ_NUMBER_,60,90)
replace in_sample=1 if inrange(SEQ_NUMBER_,1,59)
replace in_sample=0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968
replace in_sample=1 if survey_yr==1968 & RELATION_!=0 // no seq number in 1968

label define sample 0 "not sample" 1 "original sample" 2 "born-in" 3 "moved in" 4 "joint inclusion" 5 "followable nonsample parent" 6 "nonsample elderly"
label values SAMPLE sample

gen hh_status_=.
replace hh_status_=0 if SEQ_NUMBER_==0 
replace hh_status_=0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968
replace hh_status_=1 if inrange(SEQ_NUMBER_,1,20) // in sample
replace hh_status_=1 if survey_yr==1968 & inrange(RELATION_,1,9) // no seq number in 1968
replace hh_status_=2 if inrange(SEQ_NUMBER_,51,59) // institutionalized
replace hh_status_=3 if inrange(SEQ_NUMBER_,71,80) // new HH 
replace hh_status_=4 if inrange(SEQ_NUMBER_,81,89) // died
label define hh_status 0 "not in sample" 1 "in sample" 2 "institutionalized" 3 "new hh" 4 "died"
label values hh_status_ hh_status

tab survey_yr hh_status,m 

gen relationship=.
replace relationship=0 if RELATION_==0
replace relationship=1 if inlist(RELATION_,1,10)
replace relationship=2 if inlist(RELATION_,2,20,22,88)
replace relationship=3 if inrange(RELATION_,23,87) | inrange(RELATION_,90,98) | inrange(RELATION_,3,9)
label define relationship 0 "not in sample" 1 "head" 2 "partner" 3 "other"
label values relationship relationship

gen partnered=.
replace partnered=0 if in_sample==1 & MARITAL_PAIRS_==0
replace partnered=1 if in_sample==1 & inrange(MARITAL_PAIRS_,1,4)

tab relationship rel_type, m
tab partnered rel_type, m
tab in_sample rel_type, m
replace rel_type = 0 if rel_type==. & in_sample==1 // only want to identify relationship transitions if in sample bc if it's dropout or entrance, we don't know true info

gen moved = 0
replace moved = 1 if inlist(MOVED_,1,2) & inlist(SPLITOFF_,1,3) // moved in
replace moved = 2 if inlist(MOVED_,1,2) & inlist(SPLITOFF_,2,4) // splitoff
replace moved = 3 if inlist(MOVED_,5,6) // moved out
replace moved = 4 if MOVED_==1 & SPLITOFF_==0 // born
replace moved = 5 if MOVED_==7

label define moved 0 "no" 1 "Moved in" 2 "Splitoff" 3 "Moved out" 4 "Born" 5 "Died"
label values moved moved
tab moved in_sample, m
tab AGE_INDV_ moved

gen permanent_attrit=0
replace permanent_attrit=1 if PERMANENT_ATTRITION==1 // attrited
replace permanent_attrit=2 if inlist(PERMANENT_ATTRITION,2,3) // marked as died
label define perm 0 "no" 1 "attrited" 2 "died"
label values permanent_attrit perm

tab MOVED_YEAR_ SPLITOFF_YEAR_ if MOVED_YEAR_!=0 & SPLITOFF_YEAR_ !=0, m

gen change_yr=.
replace change_yr = MOVED_YEAR_ if MOVED_YEAR_ >0 & MOVED_YEAR_ <9000
replace change_yr = SPLITOFF_YEAR_ if SPLITOFF_YEAR_ >0 & SPLITOFF_YEAR_ <9000

// need to create real marital status indicators
tab MARST_DEFACTO_HEAD_ COUPLE_STATUS_HEAD_, m
tab MARST_LEGAL_HEAD_ MARST_DEFACTO_HEAD_ , m
tab relationship partnered,m  
tab relationship MARITAL_PAIRS,m  
// tabstat RELATION_, by(survey_yr) // coding switched in 1983

gen cohab_est_head=0
replace cohab_est_head=1 if MARST_DEFACTO_HEAD_==1 & inlist(MARST_LEGAL_HEAD_,2,3,4,5) // will only apply after 1977
// replace cohab_est_head = 1 if inrange(MARST_LEGAL_HEAD_,2,5) & MARST_DEFACTO_HEAD_==1 // cohab
// replace cohab_est_head = 2 if MARST_LEGAL_HEAD_==1 &  MARST_DEFACTO_HEAD_==1 // married
tab COUPLE_STATUS_HEAD_ cohab_est_head , m

gen marital_status_updated=.
replace marital_status_updated=1 if MARST_DEFACTO_HEAD_==1 & cohab_est_head==0
replace marital_status_updated=2 if MARST_DEFACTO_HEAD_==1 & cohab_est_head==1
replace marital_status_updated=3 if MARST_DEFACTO_HEAD_==2
replace marital_status_updated=4 if MARST_DEFACTO_HEAD_==3
replace marital_status_updated=5 if MARST_DEFACTO_HEAD_==4
replace marital_status_updated=6 if MARST_DEFACTO_HEAD_==5

label define marital_status_updated 1 "Married (or pre77)" 2 "Partnered" 3 "Single" 4 "Widowed" 5 "Divorced" 6 "Separated"
label values marital_status_updated marital_status_updated

// label define marital_status 1 "Married" 2 "Never Married" 3 "Widowed" 4 "Divorced" 5 "Separated"
// label values marital_status marital_status

replace marital_status_updated = . if relationship==3 | relationship==0 // this marital status doesn't apply if you are not head or their partner or you are not in sample
tab marital_status_updated relationship, m col
tab marital_status relationship, m // this is last known - could use for people with 1 relationship and for their last relationship
tab marital_status_updated partnered, m col
tab rel_type partnered, m col // okay, yes, very few rows missing here
tab marital_status_updated rel_type if partnered==1, m

replace marital_status_updated = 1 if rel_type == 20 & marital_status_updated==1
replace marital_status_updated = 2 if rel_type == 22 & marital_status_updated==1 // move the cohab I wasn't sure about
replace marital_status_updated = 1 if rel_type == 20 & marital_status_updated==. // fill in missing
replace marital_status_updated = 2 if rel_type == 22 & marital_status_updated==. 

tab marital_status_updated partnered, m col

bysort unique_id: egen first_survey_yr= min(survey_yr) if in_sample==1
bysort unique_id (first_survey_yr): replace first_survey_yr=first_survey_yr[1]
tab first_survey_yr, m
bysort unique_id: egen last_survey_yr= max(survey_yr) if in_sample==1
bysort unique_id (last_survey_yr): replace last_survey_yr=last_survey_yr[1]
tab last_survey_yr, m

sort unique_id survey_yr
browse unique_id survey_yr first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id
// browse unique_id survey_yr first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id if partnered==0 & rel_type!=.
// browse unique_id survey_yr first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id if inlist(unique_id, 4006, 4170, 4041, 4207, 57183, 57030, 5971170, 5971021) 
// ah okay, these are mostly first-yr cohabitors - so these are identified as partners in the matrix, but not in main file. this might be problematic bc the PSID doesn't collect any info on these people... BUT this is the true rel start, then ...
tab RELATION_ if partnered==0 & inlist(rel_type,20,22), m

***************************************
* relationship transitions - OBSERVED
***************************************

sort unique_id wave
// start rel - observed
gen rel_start=0
replace rel_start=1 if inlist(rel_type,20,22) & rel_type[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen marriage_start=0 // from unpartnered, NOT cohabiting
replace marriage_start=1 if rel_type==20 & rel_type[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen cohab_start=0
replace cohab_start=1 if rel_type==22 & rel_type[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

// end rel
gen rel_end=0
replace rel_end=1 if inlist(rel_type,20,22) & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

gen marriage_end=0
replace marriage_end=1 if rel_type==20 & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

gen cohab_end=0
replace cohab_end=1 if rel_type==22 & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

// coh to marriage
gen marr_trans=0
replace marr_trans=1 if rel_type==20 & rel_type[_n-1]==22 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

browse unique_id survey_yr rel_start marriage_start cohab_start rel_end marriage_end cohab_end first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id mh_yr_married1 mh_yr_married2 mh_yr_married3

// how did it end - this will not cover attriton bc need to observe in next wave - add that here or later based on last observed couple year matching last year in sample?
// i guess - if had multiple relationships - it also def had to be an end not an attrit - we just might not know if widowhood or divorce...but divorce more likely, especially in cohab
sort unique_id wave

gen how_rel_end = .
replace how_rel_end = 1 if marital_status_updated==1 & inlist(marital_status_updated[_n+1],3,5,6) & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // breakup
replace how_rel_end = 1 if marital_status_updated==2 & inlist(marital_status_updated[_n+1],3,5,6) & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // breakup
replace how_rel_end = 2 if marital_status_updated==1 & marital_status_updated[_n+1]==4 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // widowhood
replace how_rel_end = 2 if marital_status_updated==2 & marital_status_updated[_n+1]==4 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // widowhood
replace how_rel_end = 3 if marital_status_updated==2 & marital_status_updated[_n+1]==1 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // transition to marriage

gen how_marr_end = .
replace how_marr_end = 1 if marital_status_updated==1 & inlist(marital_status_updated[_n+1],3,5,6) & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // breakup
replace how_marr_end = 2 if marital_status_updated==1 & marital_status_updated[_n+1]==4 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // widowhood

gen how_cohab_end = .
replace how_cohab_end = 1 if marital_status_updated==2 & inlist(marital_status_updated[_n+1],3,5,6) & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // breakup
replace how_cohab_end = 2 if marital_status_updated==2 & marital_status_updated[_n+1]==4 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // widowhood
replace how_cohab_end = 3 if marital_status_updated==2 & marital_status_updated[_n+1]==1 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // transition to marriage

label define how_rel_end 1 "breakup" 2 "widowhood" 3 "marriage"
label values how_rel_end how_marr_end how_cohab_end how_rel_end

tab rel_end how_rel_end, m row // this is not bad (but again - just observed endings)
tab rel_end how_rel_end if inlist(relationship,1,2), m row // not really any better for just heads / wives

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr in_sample hh_status relationship partnered rel_type marital_status_updated marr_trans rel_end how_rel_end marriage_end how_marr_end cohab_end how_cohab_end

*************************************** 
* now turn these into years - let's start with all relationships
*************************************** 
gen entered_in_rel = .
replace entered_in_rel = 0 if survey_yr == first_survey_yr & rel_type==0
replace entered_in_rel = 1 if survey_yr == first_survey_yr & inlist(rel_type,20,22)
	// think I need to fill these in temporarily but want a flag. Updates 7/16/26: but, i only give oyu a rel start if I see it transition. if you enter in rel, you shouldn't have a rel start for your FIRST relationship. No, no okay you do because below, I give you a rel start using survey info. I have to do this for my rankings to work is the problem...
	// I think I need to update flag - entered in THIS rel. soo below code is mapped ot unique ID. Do i actually map to unique + partner?
	bysort unique_id partner_unique_id: egen entered_in_this_rel = max(entered_in_rel) // have to do this before copying to all rows, right?
	bysort unique_id (entered_in_rel): replace entered_in_rel=entered_in_rel[1]
	replace entered_in_this_rel = 0 if entered_in_this_rel==. // I see the problem in that this works when this file is long but not when I make it wide...need to sort out how to get this by relationship. I guess using the same info I use below? okay what i am realizing. if i map to rel 1 v. 2 v. 3 etc. isn't it just going to match?? like if rel1 is left censored,  that will be left censored and so will entered in rel??
	
	tab rel_type entered_in_this_rel, m

// all relationships 
gen relationship_start = survey_yr if rel_start==1
replace relationship_start = survey_yr if relationship_start==. & survey_yr == first_survey_yr & inlist(rel_type,20,22)
gen relationship_end = survey_yr if rel_end==1

bysort unique_id: egen relno=rank(relationship_start)
tab relno, m
bysort unique_id: egen exitno=rank(relationship_end)
tab exitno, m

forvalues r=1/6{
	gen rel`r'_start=.
	replace rel`r'_start=relationship_start if relno==`r' 
	bysort unique_id (rel`r'_start): replace rel`r'_start=rel`r'_start[1]

	gen rel`r'_end=.
	replace rel`r'_end=relationship_end if exitno==`r'
	bysort unique_id (rel`r'_end): replace rel`r'_end=rel`r'_end[1]
	
	gen rel`r'_type=.
	replace rel`r'_type = rel_type if relno==`r'
	bysort unique_id (rel`r'_type): replace rel`r'_type=rel`r'_type[1]	
	
	gen rel`r'_left_censored=.
	replace rel`r'_left_censored = entered_in_this_rel if relno==`r'
	bysort unique_id (rel`r'_left_censored): replace rel`r'_left_censored=rel`r'_left_censored[1]	
	
	gen rel`r'_how_end=.
	replace rel`r'_how_end=how_rel_end if exitno==`r'
	bysort unique_id (rel`r'_how_end): replace rel`r'_how_end=rel`r'_how_end[1]
	label values rel`r'_how_end how_rel_end
}

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr hh_status partnered marital_status_updated rel_type rel1_start rel1_end rel1_how_end rel2_start rel2_end rel2_how_end rel3_start rel3_end mh_yr_married1 mh_yr_end1 mh_yr_married2 mh_yr_end2 mh_yr_married3 mh_yr_end3 rel_start marriage_start cohab_start rel_end marriage_end cohab_end has_psid_gene 

tab rel1_start rel1_type, m
tab rel1_left_censored, m
tab entered_in_rel, m
tab entered_in_rel rel1_left_censored, m // yes this confirms my idiocy. the entered_in_rel just takes care of this
tab rel2_left_censored, m // although some poeple technically are left censored here??? (theoretically, i'd assume it's just 1)
	sort unique_id survey_yr
	// browse unique_id partner_unique_id survey_yr in_sample first_survey_yr relno relationship_start rel_type entered_in_rel entered_in_this_rel rel1_left_censored rel2_left_censored if rel2_left_censored==1 // okay these are people with GAPS in the history and they are with the same partner through those gaps.

// just marriage
gen marriage_start_yr = survey_yr if marriage_start==1
replace marriage_start_yr = survey_yr if survey_yr == first_survey_yr & rel_type==20
gen marriage_end_yr = survey_yr if marriage_end==1

bysort unique_id: egen marrno=rank(marriage_start_yr)
tab marrno, m
bysort unique_id: egen marr_exitno=rank(marriage_end_yr)
tab marr_exitno, m

forvalues r=1/6{
	gen marr`r'_start=.
	replace marr`r'_start=marriage_start_yr if marrno==`r' 
	bysort unique_id (marr`r'_start): replace marr`r'_start=marr`r'_start[1]

	gen marr`r'_end=.
	replace marr`r'_end=marriage_end_yr if marr_exitno==`r'
	bysort unique_id (marr`r'_end): replace marr`r'_end=marr`r'_end[1]
	
	gen marr`r'_left_censored=.
	replace marr`r'_left_censored = entered_in_this_rel if marrno==`r'
	bysort unique_id (marr`r'_left_censored): replace marr`r'_left_censored=marr`r'_left_censored[1]	
	
	gen marr`r'_how_end=.
	replace marr`r'_how_end=how_marr_end if marr_exitno==`r'
	bysort unique_id (marr`r'_how_end): replace marr`r'_how_end=marr`r'_how_end[1]
	label values marr`r'_how_end how_rel_end
}

// just cohabitation
gen cohab_start_yr = survey_yr if cohab_start==1
replace cohab_start_yr = survey_yr if survey_yr == first_survey_yr & rel_type==22
gen cohab_end_yr = survey_yr if cohab_end==1

bysort unique_id: egen cohno=rank(cohab_start_yr)
tab cohno, m
bysort unique_id: egen coh_exitno=rank(cohab_end_yr)
tab coh_exitno, m

forvalues r=1/6{
	gen coh`r'_start=.
	replace coh`r'_start=cohab_start_yr if cohno==`r' 
	bysort unique_id (coh`r'_start): replace coh`r'_start=coh`r'_start[1]

	gen coh`r'_end=.
	replace coh`r'_end=cohab_end_yr if coh_exitno==`r'
	bysort unique_id (coh`r'_end): replace coh`r'_end=coh`r'_end[1]
	
	gen coh`r'_left_censored=.
	replace coh`r'_left_censored = entered_in_this_rel if cohno==`r'
	bysort unique_id (coh`r'_left_censored): replace coh`r'_left_censored=coh`r'_left_censored[1]	
	
	gen coh`r'_how_end=.
	replace coh`r'_how_end=how_cohab_end if coh_exitno==`r'
	bysort unique_id (coh`r'_how_end): replace coh`r'_how_end=coh`r'_how_end[1]
	label values coh`r'_how_end how_rel_end
}

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr first_survey_yr in_sample partnered entered_in_this_rel rel_type rel1_start rel1_end rel2_start rel2_end rel3_start rel3_end marr1_start marr1_end marr2_start marr2_end coh1_start coh1_end coh2_start coh2_end mh_yr_married1 mh_yr_end1 mh_yr_married2 mh_yr_end2 mh_yr_married3 mh_yr_end3 rel_start marriage_start cohab_start rel_end marriage_end cohab_end has_psid_gene rel1_left_censored marr1_left_censored coh1_left_censored

tab rel1_left_censored entered_in_rel, m
tab marr1_left_censored entered_in_rel, m
tab coh1_left_censored entered_in_rel, m


********************************************************************************
* Just get per unique
********************************************************************************
preserve

drop entered_in_this_rel

collapse 	(mean) rel1_start rel2_start rel3_start rel4_start rel5_start rel6_start rel1_end rel2_end rel3_end rel4_end rel5_end rel6_end ///
					rel1_type rel2_type rel3_type rel4_type rel5_type rel6_type rel1_how_end rel2_how_end rel3_how_end rel4_how_end rel5_how_end rel6_how_end ///
					rel1_left_censored rel2_left_censored rel3_left_censored rel4_left_censored rel5_left_censored rel6_left_censored /// created rel variables
					marr1_start marr2_start marr3_start marr4_start marr5_start marr6_start marr1_end marr2_end marr3_end marr4_end marr5_end marr6_end ///
					coh1_start coh2_start coh3_start coh4_start coh5_start coh6_start coh1_end coh2_end coh3_end coh4_end coh5_end coh6_end ///
					marr1_how_end marr2_how_end marr3_how_end marr4_how_end marr5_how_end marr6_how_end ///
					coh1_how_end coh2_how_end coh3_how_end coh4_how_end coh5_how_end coh6_how_end ///
					marr1_left_censored marr2_left_censored marr3_left_censored marr4_left_censored marr5_left_censored marr6_left_censored ///
					coh1_left_censored coh2_left_censored coh3_left_censored coh4_left_censored coh5_left_censored coh6_left_censored ///
					mh_yr_married1 mh_yr_married2 mh_yr_married3 mh_yr_married4 mh_yr_married5 mh_yr_married6 mh_yr_married7 ///
					mh_yr_married8 mh_yr_married9 mh_yr_married12 mh_yr_married13 /// marital history variables
					mh_yr_end1 mh_yr_end2 mh_yr_end3 mh_yr_end4 mh_yr_end5 mh_yr_end6 mh_yr_end7 mh_yr_end8 mh_yr_end9 mh_yr_end12 mh_yr_end13  ///
					mh_status1 mh_status2 mh_status3 mh_status4 mh_status5 mh_status6 mh_status7 mh_status8 mh_status9 mh_status12 mh_status13 ///
					first_survey_yr last_survey_yr YR_NONRESPONSE_FIRST YR_NONRESPONSE_RECENT ///
			(max) 	partnered relno marrno cohno /// get a sense of ever partnered
, by(unique_id has_psid_gene in_marital_history mh_num_marriages entered_in_rel)

gen partner_unique_id = unique_id // for later matching

save "$temp/psid_relationship_history.dta", replace

tab rel1_start partnered, m col // do most ever partnered people at least have rel1 start date? YES
tab mh_yr_married1 partnered, m col // do most ever partnered people at least have rel1 start date? YES

browse unique_id has_psid_gene partnered in_marital_history first_survey_yr last_survey_yr rel1_start rel1_end rel2_start rel2_end rel3_start rel3_end marr1_start marr1_end marr2_start marr2_end marr3_start marr3_end coh1_start coh1_end coh2_start coh2_end coh3_start coh3_end mh_yr_married1 mh_yr_end1 mh_yr_married2 mh_yr_end2 mh_yr_married3 mh_yr_end3

restore


/*
Old file info for reference:
use "$created_data/psid_composition_history.dta", clear
tab rel1_start partnered, m // do most ever partnered people at least have rel1 start date?
tab hh1_start has_psid_gene, m
tab SAMPLE has_psid_gene, m

browse unique_id has_psid_gene SAMPLE partnered in_marital_history first_survey_yr last_survey_yr YR_NONRESPONSE_FIRST YR_NONRESPONSE_RECENT hh1_start hh1_end hh2_start hh2_end rel1_start rel1_end rel2_start rel2_end mh_yr_married1 mh_yr_end1 mh_yr_married2 mh_yr_end2
*/

********************************************************************************
********************************************************************************
**# Now attempt to create master history (inspired by GSOEP, taken from SF paper)
********************************************************************************
********************************************************************************
use "$temp/psid_relationship_history.dta", clear

// what both of those start with is reshaping to long, but on relationship number. an interesting approach I took in old file is, instead of leave in separate columns (as I do for GSOEP also), I move these created variables into marital history.
// I do wonder - can I do this? so if IN marital history - i prioritize your marital history marriage info and my cohab info (so put them into same column and then rerank so in order). if not in marital history, I actually just use the entire created history? (already in order?). let's reshape as is to explore, but may revisit

// so there are three scenarios. let's actually create variables to flag which one they are in...
tab cohno, m

gen ever_cohab = .
replace ever_cohab = 0 if cohno==.
replace ever_cohab = 1 if inrange(cohno,1,10)

tab in_marital_history ever_cohab, m
gen history_flag = .
replace history_flag = 1 if in_marital_history==1 & ever_cohab==0
replace history_flag = 2 if in_marital_history==1 & ever_cohab==1
replace history_flag = 3 if in_marital_history==0

label define history_flag 1 "History, No Cohab" 2 "History, Cohab" 3 "No History"
label values history_flag history_flag
tab history_flag, m

// 1a. In marital history, no cohabs observed: use marital history as is
// 1b. In marital history, observed at least one cohab: add cohab info to marital history
// 2. Not in marital history, doesn't really matter if have cohab or not - use my rel variables?

reshape long rel@_start rel@_end rel@_type rel@_how_end rel@_left_censored marr@_start marr@_end marr@_how_end marr@_left_censored ///
coh@_start coh@_end coh@_how_end coh@_left_censored mh_yr_married mh_yr_end mh_status, i(unique_id) j(relationship)

tab mh_num_marriages marrno
tab cohno, m

capture label drop how_rel_end 
label define how_rel_end 0 "intact" 1 "breakup" 2 "widowhood" 3 "marriage"
label values rel_how_end coh_how_end marr_how_end  how_rel_end

rename mh_status status // i am trying to align to new and use the mh_status later
capture label define status 1 "intact" 3 "widowhood" 4 "divorce" 5 "separated"
label values status status

recode status (1=0) (3=2) (4/5=1) (7/9=.), gen(mh_status)
label values mh_status how_rel_end
tab status mh_status, m

browse unique_id history_flag relno relationship rel_start rel_end rel_how_end rel_type rel_left_censored mh_yr_married mh_yr_end mh_status marr_start marr_end marr_how_end marr_left_censored coh_start coh_end coh_how_end coh_left_censored
// 4006 good example of history with cohab - my marriage dates off by 1 year, so need to use real marital history data but my cohab data
// and 4008 - there is a marriage in history that I do not even observe
// 2156002 good example of marriage in history who enters in rel, need marital history for true start date

// okay, think I like the approach of putting all in same column, but want to create a few things while long
gen mh_rel_type = .
replace mh_rel_type = 20 if mh_yr_married!=.

drop status

tab mh_status marr_how_end, m // these are actually not bad when observed

reshape wide rel@_start rel@_end rel@_type rel@_how_end rel@_left_censored marr@_start marr@_end marr@_how_end marr@_left_censored ///
coh@_start coh@_end coh@_how_end coh@_left_censored mh_yr_married mh_yr_end mh_status mh_rel_type, i(unique_id) j(relationship)

// create master indicator that is based on above rel type
forvalues r=1/9{
	gen master_rel_start`r'=.
	gen master_rel_end`r'=.
	gen master_rel_type`r'=.
	gen master_rel_how_end`r'=.
	gen master_rel_left_censored`r'=.
	
	replace master_rel_start`r' = mh_yr_married`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_end`r' = mh_yr_end`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_type`r' = mh_rel_type`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_how_end`r' = mh_status`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_left_censored`r' = 0 if in_marital_history==1 // I think I rerank later, but if I am getting the above info from here, then this info is NOT left censored

	replace master_rel_start`r' = rel`r'_start if in_marital_history==0
	replace master_rel_end`r' = rel`r'_end if in_marital_history==0
	replace master_rel_type`r' = rel`r'_type if in_marital_history==0	
	replace master_rel_how_end`r' = rel`r'_how_end if in_marital_history==0	
	replace master_rel_left_censored`r' = rel`r'_left_censored if in_marital_history==0	
	
	label values master_rel_how_end`r' how_rel_end
}

forvalues r=12/13{
	gen master_rel_start`r'=.
	gen master_rel_end`r'=.
	gen master_rel_type`r'=.
	gen master_rel_how_end`r'=.
	gen master_rel_left_censored`r'=.
		
	replace master_rel_start`r' = mh_yr_married`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_end`r' = mh_yr_end`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_type`r' = mh_rel_type`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_how_end`r' = mh_status`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_left_censored`r' = 0 if in_marital_history==1 // I think I rerank later, but if I am getting the above info from here, then this info is NOT left censored

	replace master_rel_start`r' = rel`r'_start if in_marital_history==0
	replace master_rel_end`r' = rel`r'_end if in_marital_history==0
	replace master_rel_type`r' = rel`r'_type if in_marital_history==0	
	replace master_rel_how_end`r' = rel`r'_how_end if in_marital_history==0	
	replace master_rel_left_censored`r' = rel`r'_left_censored if in_marital_history==0	
	
	label values master_rel_how_end`r' how_rel_end
}

gen master_rel_start14 = coh1_start if history_flag==2
gen master_rel_start15 = coh2_start if history_flag==2
gen master_rel_start16 = coh3_start if history_flag==2
gen master_rel_start17 = coh4_start if history_flag==2
gen master_rel_start18 = coh5_start if history_flag==2
gen master_rel_start19 = coh6_start if history_flag==2

gen master_rel_end14 = coh1_end if history_flag==2
gen master_rel_end15 = coh2_end if history_flag==2
gen master_rel_end16 = coh3_end if history_flag==2
gen master_rel_end17 = coh4_end if history_flag==2
gen master_rel_end18 = coh5_end if history_flag==2
gen master_rel_end19 = coh6_end if history_flag==2

gen master_rel_type14 = 22 if history_flag==2 & coh1_start != .
gen master_rel_type15 = 22 if history_flag==2 & coh2_start != .
gen master_rel_type16 = 22 if history_flag==2 & coh3_start != .
gen master_rel_type17 = 22 if history_flag==2 & coh4_start != .
gen master_rel_type18 = 22 if history_flag==2 & coh5_start != .
gen master_rel_type19 = 22 if history_flag==2 & coh6_start != .

gen master_rel_how_end14 = coh1_how_end if history_flag==2
gen master_rel_how_end15 = coh2_how_end if history_flag==2
gen master_rel_how_end16 = coh3_how_end if history_flag==2
gen master_rel_how_end17 = coh4_how_end if history_flag==2
gen master_rel_how_end18 = coh5_how_end if history_flag==2
gen master_rel_how_end19 = coh6_how_end if history_flag==2

gen master_rel_left_censored14 = coh1_left_censored if history_flag==2
gen master_rel_left_censored15 = coh2_left_censored if history_flag==2
gen master_rel_left_censored16 = coh3_left_censored if history_flag==2
gen master_rel_left_censored17 = coh4_left_censored if history_flag==2
gen master_rel_left_censored18 = coh5_left_censored if history_flag==2
gen master_rel_left_censored19 = coh6_left_censored if history_flag==2

label define type 20 "Marriage" 22 "Cohab"
forvalues r=1/19{
	capture label values master_rel_type`r' type 
	capture label values master_rel_how_end`r' how_rel_end
}

browse unique_id history_flag first_survey_yr master_rel_start* master_rel_end* master_rel_type* master_rel_how_end* master_rel_left_censored*

tab master_rel_start1 master_rel_left_censored1, m
tab master_rel_start14 master_rel_left_censored14, m

reshape long master_rel_start master_rel_end master_rel_type master_rel_how_end master_rel_left_censored ///
rel@_start rel@_end rel@_type rel@_how_end rel@_left_censored marr@_start marr@_end marr@_how_end marr@_left_censored ///
coh@_start coh@_end coh@_how_end coh@_left_censored mh_yr_married mh_yr_end mh_status mh_rel_type, i(unique_id) j(relationship)

browse unique_id relationship history_flag first_survey_yr master_rel_start master_rel_end master_rel_type master_rel_how_end master_rel_left_censored

// now actually rank relationships (following x_create_cohab_sample)
gen master_rel_end_orig=master_rel_end
replace master_rel_end=9999 if master_rel_end==. & master_rel_start!=. // I need to do this to rank (I think really to be a tiebreaker?)
drop if master_rel_start==9998 // so I just do this here... is that fine? I mean, it's essentially missing, so I am just making this a missing in the history? is that okay...?

by unique_id: egen rank_start = rank(master_rel_start) // so i still retain the original info generally, it's just I rank on both to ensure I use maximal info.
by unique_id: egen rank_end = rank(master_rel_end)
by unique_id: egen max_rank_start = max(rank_start)
// egen rank_avg = rowmean(rank_start rank_end)
// by unique_id: egen rank = rank(master_rel_start master_rel_end)

browse unique_id relationship history_flag max_rank_start rank_start rank_end master_rel_start master_rel_end master_rel_type
// browse unique_id relationship history_flag max_rank_start rank_start rank_end master_rel_start master_rel_end master_rel_type if rank_start==2.5
// browse unique_id relationship history_flag max_rank_start rank_start rank_end master_rel_start master_rel_end master_rel_type if inlist(unique_id,5186, 351183)

// I want to prio start date, then end date, and i want to break ties using rel type...
gen rel_rank = .
replace rel_rank = rank_start if inlist(rank_start, 1,2,3,4,5,6,7,8,9,10) // whole numbers
replace rel_rank = 1 if rank_start==1.5 & master_rel_type==22 // prio cohab
replace rel_rank = 2 if rank_start==1.5 & master_rel_type==20

forvalues y=2.5(1)7.5{
	local a = `y' - 0.5
	local b = `y' + 0.5
	replace rel_rank = `a' if rank_start==`y' & master_rel_type==22 // prio cohab
	replace rel_rank = `b' if rank_start==`y' & master_rel_type==20
}

inspect rank_start
inspect rel_rank
tab rank_start rel_rank, m
tab rel_rank rank_end, m

browse unique_id first_survey_yr relationship history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type master_rel_left_censored
// browse unique_id relationship history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if rel_rank==. & rank_end!=.
// browse unique_id relationship history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if inlist(unique_id, 1335034, 2432031, 5548010)

replace rel_rank = rank_end if rel_rank==. & inlist(rank_end, 1,2,3,4,5,6,7,8,9,10) // e.g. 4034

tab master_rel_start rel_rank, m
drop if rel_rank==. // let's get rid of these extra rows

bysort unique_id rel_rank: egen duplicate_rank = count(rel_rank)
tab duplicate_rank, m 
// browse unique_id entered_in_rel history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if duplicate_rank==2
// browse unique_id entered_in_rel history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if inlist(unique_id,105181,546003,931174,1480001,1530001,1684195,2001001,2177003,2194021,2269002,2288002,2435183,5419002,5539172,5712173,5747002)
// tab master_rel_start if duplicate_rank>1, m // yeah, these are mostly 9998. Actually just drop those? I did that in other version of this file // fixed this
replace rel_rank = rank_end if duplicate_rank==2 // this should work
bysort unique_id rel_rank: egen duplicate_rank_v2 = count(rel_rank)
replace rel_rank = rank_end if duplicate_rank_v2==2

keep unique_id entered_in_rel history_flag rel_rank  master_rel_start master_rel_end master_rel_type master_rel_how_end master_rel_left_censored
sort unique_id rel_rank

	// tab entered_in_rel master_rel_left_censored, m
	// tab entered_in_rel master_rel_left_censored if rel_rank==1, m // because I can recover from marital history, it's not congruent anymore
	tab entered_in_rel master_rel_left_censored if rel_rank==1 & master_rel_type== 20,m // yeah, here, mostly okay
	tab entered_in_rel master_rel_left_censored if rel_rank==1 & master_rel_type== 22,m // HERE are the problems
	tab rel_rank master_rel_type, m
	tab master_rel_start master_rel_left_censored, m
	tab master_rel_start master_rel_left_censored if master_rel_start >=1984, m
	
reshape wide master_rel_start master_rel_end master_rel_type master_rel_how_end master_rel_left_censored, i(unique_id) j(rel_rank)

unique unique_id // this is currently only restricted to people in relationships (remember this for when I merge later and inevitably panic) // 42743 

save "$created_data/psid_master_relationship_history_wide.dta", replace
