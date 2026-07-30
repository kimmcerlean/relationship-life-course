********************************************************************************
********************************************************************************
* Project: Relationship Growth Curves
* Owner: Kimberly McErlean
* Started: September 2024
* File: create sample
********************************************************************************
********************************************************************************

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

********************************************************************************
********************************************************************************
********************************************************************************
**# First, get individual sample / relationship info
********************************************************************************
********************************************************************************
********************************************************************************
use "$temp/PSID_full_long.dta", clear

sort unique_id survey_yr

// want consecutive waves to make some things easier later
egen wave = group(survey_yr)

// for now, JUST keep BASIC BASIC indicators - first do some recodes
gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

gen in_sample=.
replace in_sample=0 if SEQ_NUMBER_==0 | inrange(SEQ_NUMBER_,60,90)
replace in_sample=1 if inrange(SEQ_NUMBER_,1,59)
replace in_sample=0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968
replace in_sample=1 if survey_yr==1968 & RELATION_!=0 // no seq number in 1968

bysort unique_id (in_sample): egen ever_in_sample = max(in_sample)

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

gen year = survey_yr if insample==1

bysort unique_id (year): egen first_survey_yr = min(year)
bysort unique_id (year): egen last_survey_yr = max(year)

sort unique_id survey_yr
browse unique_id main_per_id survey_yr ever_in_sample in_sample hh_status SEQ_NUMBER_ SAMPLE first_survey_yr last_survey_yr YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST PERMANENT_ATTRITION ANY_ATTRITION

gen relationship=.
replace relationship=0 if RELATION_==0
replace relationship=1 if inlist(RELATION_,1,10)
replace relationship=2 if inlist(RELATION_,2,20,22,88)
replace relationship=3 if inrange(RELATION_,23,87) | inrange(RELATION_,90,98) | inrange(RELATION_,3,9)
label define relationship 0 "not in sample" 1 "head" 2 "partner" 3 "other"
label values relationship relationship

gen in_relationship=.
replace in_relationship=0 if in_sample==1 & MARITAL_PAIRS_==0
replace in_relationship=1 if in_sample==1 & inrange(MARITAL_PAIRS_,1,4)

// Just keep a few variables to make smaller (kim - update this list once you get to other computer)
keep unique_id survey_yr in_sample hh_status SEQ_NUMBER_ SAMPLE first_survey_yr last_survey_yr YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST PERMANENT_ATTRITION ANY_ATTRITION

********************************************************************************
* Then fill in relationship info from history
********************************************************************************
* First, better partnership status
merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 ego_rel alter_rel partner_unique_id rel_num marr_num)
tab MARITAL_PAIRS_ _merge, m // so those in marital pairs have basically perfect match
tab in_relationship _merge, m

drop if _merge==2
drop _merge

rename MX8 rel_type
label define rel_type 20 "Spouse" 22 "Partner"
label values rel_type rel_type

tab in_relationship rel_type, m 

* Second, created history
merge m:1 unique_id using "$created_data/psid_master_relationship_history_wide.dta" 
drop if _merge==2

tab in_relationship _merge, m row 

gen in_marital_history=0
replace in_marital_history=1 if _merge==3
drop _merge

* Now, attempt to put on to individuals
browse unique_id survey_yr in_relationship MX8 partner_unique_id enter_rel exit_rel master_rel_start1 master_rel_end1 master_rel_type1 master_rel_left_censored1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_left_censored2 master_rel_start3 master_rel_end3 master_rel_type3 master_rel_left_censored3

tab marital_status_focal MX8
tab MX8 master_rel_type1, m
tab master_rel_start1 master_rel_left_censored1, m 

gen current_rel_number=.
forvalues r=1/10{
	capture replace current_rel_number = `r' if survey_yr >= master_rel_start`r' & survey_yr<= master_rel_end`r' & MX8 == master_rel_type`r' // prio next in_relationship bc end dates of 9999 causing problems
	// yes - bc current rel_type is based on start type-  so couples who transition frm cohab to marriage will not match (that is currently happening)
	capture replace current_rel_number = `r' if current_rel_number==. & survey_yr >= master_rel_start`r' & survey_yr<= master_rel_end`r' // & MX8!=. // don't need to match, but need to be observed in a rel
}

tab current_rel_number in_relationship, m col
tab current_rel_number MX8, m col
tab matrix_rel_num current_rel_number, m // matrix rel num doesn't include in_relationships prior to survey, so makes sense some of those are lower than here

// browse unique_id survey_yr history_flag in_relationship MX8 partner_unique_id current_rel_number master_rel_start1 master_rel_end1 master_rel_type1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_start3 master_rel_end3 master_rel_type3

gen current_rel_start_year = . // rel_start_all
gen current_rel_end_year = . // rel_end_all
gen current_rel_type = . // rel_type_all
gen current_rel_status = . // rel_status_all
gen current_rel_left_censored = . // rel_left_censored_all

forvalues r=1/10{
	replace current_rel_start_year = master_rel_start`r' if current_rel_number==`r'
	replace current_rel_end_year = master_rel_end`r' if current_rel_number==`r'
	replace current_rel_type = master_rel_type`r' if current_rel_number==`r'
	replace current_rel_status = master_rel_how_end`r' if current_rel_number==`r'
	replace current_rel_left_censored = master_rel_left_censored`r' if current_rel_number==`r'
}

label values rel_type_all type
label values rel_status_all how_rel_end
tab MX8 rel_type_all, m // some are off because transitioned cohab to marriage and mine only captures 1st rel - we are going to update htis the way I normally do with the min / max rel dates. but doing this at a couple-level so can attempt to fill in true start / end dates (if one has non-missing info)
tab MX8 rel_status_all, m // this is def not perfect, especially for cohab - intact not at all labelled yet

tab current_rel_start_year rel_left_censored_all, m
tab current_rel_start_year rel_left_censored_all if MX8==20, m
tab current_rel_start_year rel_left_censored_all if MX8==22, m // am I going to lose all of my cohabitors??
tab current_rel_start_year rel_left_censored_all if MX8==22 & has_psid_gene==0, m // okay OR is it really just that it's because I haven't matched and cleaned to partners when the one with the PSID gene will MASSIVELY help 
tab current_rel_start_year rel_left_censored_all if MX8==22 & has_psid_gene==1, m // like this is actually fine except for 1968 which I remove
	// I think it's these samples really then still causing issues on above: tab sample_type has_psid_gene

browse unique_id survey_yr in_relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all rel_status_all rel_left_censored_all enter_rel exit_rel master_rel_start1 master_rel_end1 master_rel_type1 master_rel_how_end1 master_rel_left_censored1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_how_end2 master_rel_left_censored2 master_rel_start3 master_rel_end3 master_rel_type3 master_rel_left_censored4

// did the left censor seem to work?
tab current_rel_start_yr current_rel_left_censored, m
tab sample_type current_rel_left_censored, row

********************************************************************************
* Atempt to fill in missing info using relationship transitions
********************************************************************************
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

* then create indicator of start date
gen current_rel_start_est = survey_yr if rel_start==1
bysort unique_id partner_unique_id (current_rel_start_est): replace current_rel_start_est=current_rel_start_est[1] if partner_unique_id!=.

gen current_rel_end_est = survey_yr if rel_end_pre==1
bysort unique_id partner_unique_id (current_rel_end_est): replace current_rel_end_est=current_rel_end_est[1]  if partner_unique_id!=. 

sort unique_id survey_yr

gen current_rel_start_yr_v0 = current_rel_start_yr // I like to retain original copies
gen current_rel_end_yr_v0 = current_rel_end_yr // I like to retain original copies

	// before updating, let's create a flag denoting I am using this estimated date
	gen rel_start_est_flag = 0
	replace rel_start_est_flag = 1 if current_rel_start_yr==. & current_rel_start_est!=. & partner_unique_id!=. & survey_yr>=current_rel_start_est
	
	gen rel_end_est_flag = 0
	replace rel_end_est_flag = 1 if current_rel_end_yr==. & current_rel_end_est!=. & partner_unique_id!=. & survey_yr<=current_rel_end_est

replace current_rel_start_yr = current_rel_start_est if current_rel_start_yr==. & current_rel_start_est!=. & partner_unique_id!=. & survey_yr>=current_rel_start_est
replace current_rel_end_yr = current_rel_end_est if current_rel_end_yr==. & current_rel_end_est!=. & partner_unique_id!=. & survey_yr<=current_rel_end_est

// tab sex_pl if current_rel_start_yr_v0==. & current_rel_start_yr!=.
// tab rel_start_est_flag

* And cohab transition info. Also add other transition info
browse unique_id partner_unique_id partnered_pl survey_yr current_rel_type firstyr_survey_pl lastyr_survey_pl full_status_pl marr_trans

gen transition_year = survey_yr if marr_trans==1
bysort unique_id partner_unique_id (transition_year): replace transition_year=transition_year[1] if partner_unique_id!=. 

bysort unique_id partner_unique_id: egen ever_transition = max(marr_trans) if inrange(partnered_pl,1,4)

	// but how many people does this miss if not observed in consecutive years?.
	bysort unique_id partner_unique_id: egen first_couple_year = min(survey_yr) if partner_unique_id!=.
	bysort unique_id partner_unique_id: egen last_couple_year = max(survey_yr) if partner_unique_id!=. 
	
	gen first_rel_type = MX8 if survey_yr==first_couple_yr
	gen last_rel_type = MX8 if survey_yr==last_couple_yr
	
	bysort unique_id partner_unique_id (first_rel_type): replace first_rel_type = first_rel_type[1]
	bysort unique_id partner_unique_id (last_rel_type): replace last_rel_type = last_rel_type[1]
		
	label values first_rel_type last_rel_type partnered
		
	tab first_rel_type last_rel_type, m
	tab first_rel_type last_rel_type if in_relationship==1, m 

	gen transition_est = 0
	replace transition_est = 1 if first_rel_type==22 & last_rel_type==20

	tab transition_est ever_transition, m
	tab transition_est ever_transition if in_sample==1, m 
	
sort unique_id survey_yr
browse unique_id survey_yr wave partner_unique_id MX8  marr_trans ever_transition transition_est transition_year
	
// make these flags here actually so have for next step
gen start_yr_missing_flag = 0
replace start_yr_missing_flag = 1 if current_rel_start_yr==. & inrange(partnered_pl,1,4)
	
gen end_yr_missing_flag = 0
replace end_yr_missing_flag = 1 if current_rel_end_yr==. & inrange(partnered_pl,1,4)

tab current_rel_start_miss current_rel_left_censored, m // so yeah the left censored version covers the previous miss i created. These NEW missing are still missing
tab start_yr_missing_flag current_rel_left_censored, m // all of the 1s are missing on left censored, so these are DIFFERENT problems

save "$created_data/PSID_long_partnership_history.dta"

********************************************************************************
********************************************************************************
********************************************************************************
**# Match partner info and clean up across partners
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* first, get partner variables
********************************************************************************

use "$created_data/PSID_long_partnership_history.dta", clear 

local partnervars "partnered_pl current_rel_type current_rel_start_yr current_rel_end_yr current_rel_number firstyr_survey_pl lastyr_survey_pl current_rel_left_censored current_rel_start_miss start_yr_missing_flag  end_yr_missing_flag rel_start_est_flag rel_end_est_flag ever_transition transition_est transition_year full_status_pl in_couply in_couplm in_marsy in_any_rel_history"

keep unique_id survey_yr `partnervars'

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	rename `var' `var'_sp
}

rename unique_id partner_unique_id

save "$temp/psid_partner_rel_info.dta", replace

********************************************************************************
* Then merge on that info
********************************************************************************
use "$created_data/PSID_long_partnership_history.dta", clear 

tab current_rel_type in_relationship, m 

inspect partner_unique_id if in_relationship==0 
inspect partner_unique_id if in_relationship==1
inspect partner_unique_id if inlist(current_rel_type,20,22)

keep if inlist(current_rel_type,20,22)
// drop if partner_unique_id==.

// drop non-partnered = BUT i think this is household level, so need to also drop the specific individuals not partnered?
// keep if inlist(marital_status_updated,1,2) | rel_start==1 | marr_trans==1 | rel_end==1
// drop if person==0
// added that pre_rel_end (so I know it was last year married) - so can delete those with marital status that isn't married or partnered
// drop if inlist(marital_status_updated,3,4,5,6)

merge m:1 partner_unique_id survey_yr using "$temp/psid_partner_rel_info.dta" // has to be many because of the missing partner_id. wait but that's only 2? okay let's try this..
drop if _merge==2

tab _merge // so like 2% don't match
tab _merge if partner_unique_id!=. // and it is not just because of this
unique unique_id partner_unique_id, by(_merge) // like 7% from this standpoint

gen partner_match=0
replace partner_match=1 if _merge==3

drop _merge

********************************************************************************
// Now, let's clean up dates between partners
********************************************************************************
browse unique_id partner_unique_id survey_yr partnered_pl partnered_pl_sp current_rel_start_year current_rel_start_year_sp current_rel_left_censored current_rel_left_censored_sp start_yr_missing_flag start_yr_missing_flag_sp rel_start_est_flag rel_start_est_flag_sp current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year

tab partnered_pl partnered_pl_sp, m
tab partnered_pl partnered_pl_sp if partner_match==1, m // basically congruent
tab current_rel_left_censored current_rel_left_censored_sp, m
tab start_yr_missing_flag start_yr_missing_flag_sp, m
tab rel_start_est_flag rel_start_est_flag_sp, m // very few

// flag joint things
tab current_rel_left_censored current_rel_left_censored_sp, m cell // see like it's not actually that high???
tab current_rel_left_censored current_rel_left_censored_sp if current_rel_type==20, m cell  // that's almost a rounding error
tab current_rel_left_censored current_rel_left_censored_sp if current_rel_type==22, m cell  // so here is obviously where it's higher. The problem for the PSID perhaps differnetly to other surveys is that this DOES affect cohab (whereas in UK and DE, I feel like this was mostly a not treated issue. so I actually think the implications are going to be much larger here...)

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

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr partnered_pl in_couply in_couply_sp current_rel_start_year current_rel_start_year_sp current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year current_rel_left_censored current_rel_left_censored_sp start_yr_missing_flag start_yr_missing_flag_sp rel_start_est_flag rel_start_est_flag_sp if rel_start_match==0 | rel_end_match==0 

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

browse unique_id partner_unique_id survey_yr partnered_pl current_rel_start current_rel_start_year current_rel_start_year_sp current_rel_left_censored current_rel_left_censored_sp  either_start_missing either_start_missing_sp current_rel_end current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year

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


////// above is frm Germany, here's PSID marriage version to have
gen current_rel_start = .
inspect current_rel_start
replace current_rel_start = current_rel_start_year if rel_start_match==1 // the problem here is they can match and BOTH be left censored, but none of the rest of the code can solve for that, so I think I just have to fill in as is and then just drop if both left censored
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_left_censored==0 // newly added code
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_left_censored_sp==0 // newly added code
replace current_rel_start = current_rel_start_year if current_rel_start==. & inlist(history_flag,1,2) & history_flag_sp==3
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & history_flag==3 & inlist(history_flag_sp,1,2)
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year!=. & current_rel_start_year_sp==.
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp!=.
replace current_rel_start = current_rel_start_year if current_rel_start==. & both_left_censored==1 & current_rel_start_year < current_rel_start_year_sp // new code
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & both_left_censored==1 & current_rel_start_year_sp < current_rel_start_year // new code
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year==first_couple_yr
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp==first_couple_yr
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year < current_rel_start_year_sp
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp < current_rel_start_year // after this, both starts missing. I assign one, but need to create the left censor that drops THOSE AND THE LEFT CENSOR (a la Germany)
replace current_rel_start = first_couple_yr if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp==. // how many people is this? literally 510 lol

gen both_start_missing = 0
replace both_start_missing = 1 if current_rel_start_year==. & current_rel_start_year_sp==. // flag if both start dates missing so using first couple_year
tab both_start_missing, m

tab both_start_missing both_left_censored, m // core problem is that didn't solve the WRITTEN IN LEFT CENSORING, which is a much larger problem.

gen exclusion_couples = 0
replace exclusion_couples = 1 if both_start_missing==1 | both_left_censored==1
tab exclusion_couples,m 
	// tab sample_type exclusion_couples, row // highest for immigrant samples but none break 20%
	
inspect current_rel_start current_rel_start_year current_rel_start_year_sp

tab current_rel_start, m
tab current_rel_start sample_type, m

// 
gen current_rel_end = .
inspect current_rel_end
replace current_rel_end = current_rel_end_year if rel_end_match==1 // so if they are both 9999, this will go here. need to decide if will leave 9999 OR replace with last couple yr (if I did that, I would retain this copy)
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year < 3000 & (current_rel_end_year_sp==. | current_rel_end_year_sp==9999)
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & (current_rel_end_year==. | current_rel_end_year==9999) & current_rel_end_year_sp < 3000
replace current_rel_end = current_rel_end_year if current_rel_end==. & inlist(history_flag,1,2) & history_flag_sp==3 & current_rel_end_year!=9999 & current_rel_end_year!=.
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & history_flag==3 & inlist(history_flag_sp,1,2) & current_rel_end_year_sp!=9999 & current_rel_end_year_sp!=.
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year==last_couple_yr
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp==last_couple_yr
replace current_rel_end = 9999 if current_rel_end==. & (current_rel_end_year==9999 & current_rel_end_year_sp==.) | (current_rel_end_year==. & current_rel_end_year_sp==9999) // these are weird so make 9999 for now 
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year < current_rel_end_year_sp
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp < current_rel_end_year
replace current_rel_end = last_couple_yr if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp==.

gen both_end_missing = 0
replace both_end_missing = 1 if current_rel_end_year==. & current_rel_end_year_sp==. // flag if both end dates missing so using last couple_year

inspect current_rel_end current_rel_end_year current_rel_end_year_sp
tab current_rel_status_est current_rel_status_est_sp, m

tab current_rel_end current_rel_status_est, m col // so majority of 9999s are intact, which makes sense. let's make new varaiable where it becomes last couple yr, but we'll flag. not actually sure I do anything with this info for these data...

gen rel_end_9999 = .
replace rel_end_9999 = 0 if current_rel_end!=9999
replace rel_end_9999 = 1 if current_rel_end==9999

gen current_rel_end_estimated = current_rel_end if rel_end_9999==0
replace current_rel_end_estimated = last_couple_yr if rel_end_9999==1

browse unique_id partner_unique_id survey_yr current_rel_type ever_transition current_rel_start current_rel_start_year current_rel_start_year_sp current_rel_left_censored current_rel_left_censored_sp current_rel_end current_rel_end_estimated  current_rel_end_year  current_rel_end_year_sp  first_couple_yr last_couple_yr history_flag history_flag_sp

// is this a way to check left censoring in addition to above? Okay, duh I do this BEFORE adjusting so this is actually fine because will tell me.

// browse unique_id partner_unique_id survey_yr  current_rel_type current_rel_start current_rel_start_unadj first_survey_yr first_survey_yr_sp entered_in_rel
// I also have this variable: entered_in_rel // which should be left censoring? BUT again, the problem with this is their PARTNER might not have...BUT should this be more central to my aligning of start dates actually??

tab entered_in_rel current_rel_left_censored,m // actualyl small minority bc fixed many w marriage history plus decent amount not in first rel
tab entered_in_rel current_rel_left_censored if current_rel_start >=1980,m
tab entered_in_rel if current_rel_start >=1980, m
// tab current_rel_start entered_in_rel if current_rel_start >=1980, m // col
tab current_rel_start exclusion_couples, m // col

gen start_match = 0
replace start_match = 1 if first_survey_yr==current_rel_start
tab start_match entered_in_rel, m // okay, so when match, most are also entered in rel. HOWEVER, many people also labeled as entered in rel that don't have start match (wonder also if this is because of old relationsips?)
tab start_match exclusion_couples, m col

gen start_match_sp = 0
replace start_match_sp = 1 if first_survey_yr_sp==current_rel_start

tab start_match start_match_sp, m  cell // like, it's actually quite low (4%)
tab start_match start_match_sp if current_rel_start>=1985, m  cell // also think the bias is much worse in early years? (yes for both, though do lose those present since 1968, so the both 0s go down)
tab start_match start_match_sp if current_rel_type==22, cell // as suspected, it is HIGHER for cohab.
tab start_match start_match_sp if current_rel_type==22 & current_rel_start>=1985, cell // though THAT is really driven by early years
tab start_match start_match_sp if current_rel_type==20, cell

tab start_match start_match_sp if exclusion_couples==0, m  cell // okay so i think this affirms this is a good check. Here, the both match is < 1%
tab start_match start_match_sp if exclusion_couples==1, m  cell // HERE - the both match is 88%. so that is a good RED FLAG INDICATOR to use for other surveys (already integrated into UKHLS, need to go back through PSID still)
tab exclusion_couples if start_match==1 & start_match_sp==1, m // alt way of looking

forvalues s=1/7{
	display `s'
	tab start_match start_match_sp if sample_type==`s', cell // okay but it's honestly not crazy for any? 3, 4, and 7 are a little high and this is certainly probably worth looking into, but none of it stands out as crazy yet.
}

******************************************
* Now, try to allocate cohab dates better based on first yr status
******************************************
**# Reflect on this, 7/16/26
// okay, here is a core question: can i recover any left censored first year cohabitors? like if they are "left censored" but marked as first year, I can keep right???!?!

tab current_rel_start current_rel_type, m col // yes, all cohabs odd years since biennial
tab interview_month_created interview_month_created_sp, m // these match between partners (will use based on first yr status)

// first need to designate if first yr cohabitor in couple (since one is head, one is first_yr, need to create at couple level)
tab survey_yr RELATION_ // also note - both partner and first-yr cohabitor split out at same time: 1983 (this actually also supports case of not using years before 1985)

	// get ID variable
	egen first_p = rowmin(unique_id partner_unique_id)
	egen second_p = rowmax(unique_id partner_unique_id)
	egen couple_id_shared = group(first_p second_p)

	browse FAMILY_INTERVIEW_NUM_ unique_id partner_unique_id survey_yr first_p  second_p couple_id_shared

	unique unique_id partner_unique_id
	unique couple_id_shared

gen is_first_yr_cohabitor = 0
replace is_first_yr_cohabitor = 1 if RELATION_==88

bysort couple_id_shared survey_yr: egen has_first_yr_cohabitor = max(is_first_yr_cohabitor)
bysort unique_id partner_unique_id: egen ever_first_yr_cohabitor = max (has_first_yr_cohabitor)

sort couple_id_shared unique_id partner_unique_id survey_yr
browse unique_id partner_unique_id couple_id_shared survey_yr interview_month_created current_rel_start current_rel_type ever_first_yr_cohabitor has_first_yr_cohabitor is_first_yr_cohabitor RELATION_ transition_year exclusion_couples both_left_censored

tab exclusion_couples has_first_yr_cohabitor, m // okay it will litearlly only recover like...166 couples
tab both_left_censored has_first_yr_cohabitor, m 
tab exclusion_couples ever_first_yr_cohabitor, m  // I guess I have to do it this way but still 334 LOL.

replace exclusion_couples = 0 if ever_first_yr_cohabitor == 1 // because then we know it's a real entrance (again, it's SO few)

// Rules to adjust:
	// if first year cohabitor, leave as current year.
	// if NOT first year, do as year prior? (I think I was actually wondering if there is a world in which it could have been TWO YEARS prior post interview, and that is true, but I think that is too chaos of an assumption? so if next year interviewed, they are NOT first year, assume they moved in one year ago.) (Example shared ID: 13834)
	// this should all be based on combo of CURRENT REL TYPE plus ever first year cohabitor? but because I adjust for cohab / marriage below, just update if currently partnered, because married couples could be EVER first yr cohabitors if they transition...(e.g. couple id shared: 9769)
	// second question: do I ONLY adjust for the biennial period or do I adjust all? Right now, adjusting all...I guess cohab is ALWAYS made up, so...fine to adjust all?

tab current_rel_type has_first_yr_cohabitor, m row
tab current_rel_type ever_first_yr_cohabitor, m row

rename current_rel_start current_rel_start_unadj

gen current_rel_start=.
replace current_rel_start = current_rel_start_unadj if current_rel_type==20
replace current_rel_start = current_rel_start_unadj if current_rel_type==22 & ever_first_yr_cohabitor==1 // first yr cohab, assume started that year
replace current_rel_start = current_rel_start_unadj-1 if current_rel_type==22 & ever_first_yr_cohabitor==0 // not first yr - do prior year

inspect current_rel_start current_rel_start_unadj

tab current_rel_start_unadj current_rel_type, m col
tab current_rel_start current_rel_type, m col // bc of allocation, slightly now more even years but much more evenly distributed 

tab hh_hours_type ever_first_yr_cohabitor if current_rel_type==22, col // but they are much more egalitarian still, is this problematic? or is it because there is change over time?

// tab current_rel_start current_rel_start_unadj if survey_yr <1997
// tab current_rel_start current_rel_start_unadj if survey_yr >=1997

******************************************
* Then, fix cohab / marriage dates (so continuous)
******************************************

// FIRST, fix transition year
tab ever_transition ever_transition_sp, m // these match
tab transition_est transition_est_sp, m // same
tab transition_year transition_year_sp, m // these also somehow match

tab transition_est ever_transition, m
gen all_transitions = 0
replace all_transitions = 1 if transition_est==1 | ever_transition==1 // let's just create this for ease

gen all_transitions_sp = 0
replace all_transitions_sp = 1 if transition_est_sp==1 | ever_transition_sp==1

tab all_transitions all_transitions_sp, m // still congruent

// Before I do this, confirm left censoring can't mess this code up
// theoretically, if this is a second relationship (the marriage part of the cohab to marriage transition), it shouldn't, but i don't want to use left censored bad info if that is the case [if I can recover the real info otherwise]

tab all_transitions exclusion_couples, row // wait duh for the US, cohab is the real problem so it makes sense it's HIGHER among transitioners here.
tab all_transitions exclusion_couples if current_rel_type==20, row // THIS is key check. okay still a problem but less so but i guess i will drop eventually anyway because this is couple level variable so it's fine?


// BUT can i update so based on real marriage year? Because these are ALSO all odd years, which doesn't make sense. I actually wonder - instead of needing to use the master start info, do I actually update based on current start year if married (because THOSE are based on history?) so yes, I need the real start date to match cohab start, but can I leverage this info if transitioned because might be more accurate?
tab current_rel_start if all_transitions == 1 & current_rel_type==20
tab current_rel_start transition_year if all_transitions == 1 & current_rel_type==20 // i mean, this sort of makes sense. the current year is usually the two years around the transition year and in off years, it's the 3 years around (it's same problem of like, they could marry in previous survey yr just AFTER the survey, so it's not even they could marry in next year or the off year, but even the year prior, hence this three year spread. I actually think I just use this).
	// based on spot check, it's like...USUALLY correct, but not always - particularly for people with missing marital history I think
	// a small prblem with people marrying in year of survey means the transition can happen in same year cohab starts. I think I need to at least offset it by one for the analysis to work. for this reason, I actually wonder if this is okay? Especially if I DON'T fill in biennial years (because then it's based on next year observed) but I think I DO need to fill in biennial years GAH idk idk
	// side note: not relevant for this paper, but if I look at change over time, 1977 is a major shift because they changed measurement (so heaping happens here)

gen transition_yr_marr = .
replace transition_yr_marr = current_rel_start if all_transitions==1 & current_rel_type==20
bysort unique_id partner_unique_id (transition_yr_marr): replace transition_yr_marr = transition_yr_marr[1]

gen transition_yr_cohab = .
replace transition_yr_cohab = current_rel_start if all_transitions==1 & current_rel_type==22
bysort unique_id partner_unique_id (transition_yr_cohab): replace transition_yr_cohab = transition_yr_cohab[1]

gen cohab_year = survey_yr if all_transitions==1 & current_rel_type==22
bysort unique_id partner_unique_id: egen last_yr_cohab = max(cohab_year)

// based on troubleshooting, there are some anomalies here with people's marriage dates being recorded incorrectly. this is so niche though, not 100% how to fix effectively - maybe can flag if transition year and / or last year cohab already match a marriage start date?
gen is_ty_marriage_start=0
forvalues m=1/10{
	replace is_ty_marriage_start = 1 if transition_year == master_rel_start`m' & master_rel_type`m'==20 & transition_year!=.
}

gen is_lyc_marriage_start=0
forvalues m=1/10{
	replace is_lyc_marriage_start = 1 if last_yr_cohab == master_rel_start`m' & master_rel_type`m'==20 & last_yr_cohab!=.
}

gen is_tym_marriage_start=0
forvalues m=1/10{
	replace is_tym_marriage_start = 1 if transition_yr_marr == master_rel_start`m' & master_rel_type`m'==20 & transition_yr_marr!=.
}

gen est_marriage_date = .
forvalues m=1/10{
	replace est_marriage_date = master_rel_start`m' if master_rel_start`m' >= current_rel_start & master_rel_start`m'<=current_rel_end_estimated & master_rel_type`m'==20
}


tab is_ty_marriage_start all_transitions, m
tab is_lyc_marriage_start all_transitions, m
tab is_tym_marriage_start all_transitions if inlist(history_flag,1,2), m col

sort couple_id_shared unique_id partner_unique_id survey_yr
browse unique_id partner_unique_id survey_yr current_rel_type current_rel_start current_rel_end_estimated all_transitions ever_transition marr_trans transition_year transition_yr_marr transition_yr_cohab last_yr_cohab is_ty_marriage_start is_lyc_marriage_start is_tym_marriage_start history_flag master_rel_start1 master_rel_type1 master_rel_start2 master_rel_type2 master_rel_start3 master_rel_type3 master_rel_start1_sp master_rel_type1_sp master_rel_start2_sp master_rel_type2_sp master_rel_start3_sp master_rel_type3_sp 

	// need an intermission to actually figure out the scope of the problem I have identified. The distribution is still off and there are two reasons I am forcing a move to even years: if start cohab and marriage in same year AND if last year observed is cohab. I actually don't know if the latter is a problem, especially with event study. When I use binary married (and partnership status is cohab), it just puts them in non-married, but this is hard bc HOUSEWORK will reflect what happens at survey start, so cohab is accurate HOWEVER paid work is annual ,so paid work is mixed between cohab and married anyway so it honestly doesn't matter? (and also WHY in event study, this IS actually true year 0 where statuses are mixed...)
	
tab all_transitions
unique unique_id partner_unique_id, by(all_transitions)

tab all_transitions if all_transitions==1 & transition_yr_marr == transition_yr_cohab // so these are people whose marriage and cohab start dates are the same
unique unique_id partner_unique_id if all_transitions==1 & transition_yr_marr == transition_yr_cohab

tab all_transitions if all_transitions==1 & transition_yr_marr == last_yr_cohab // these are people where they marry in survey year, and in that survey year, they were observed cohab (so they married AFTER survey). wait a problem here is that people who marry in same year cohab also captured. so need to see how many people where it's not bc of that
	// aka unique_id 5004 is example where all three match, but unique_id 7047 is the case I am thinking of
unique unique_id partner_unique_id if all_transitions==1 & transition_yr_marr == last_yr_cohab

tab all_transitions if all_transitions==1 & transition_yr_marr == last_yr_cohab & transition_yr_marr != transition_yr_cohab
unique unique_id partner_unique_id if all_transitions==1 & transition_yr_marr == last_yr_cohab & transition_yr_marr != transition_yr_cohab

// tab is_tym_marriage_start if all_transitions==1 & (history_flag==3 | history_flag==.)
// tab is_ty_marriage_start if all_transitions==1 & (history_flag==3 | history_flag==.)
// tab is_lyc_marriage_start if all_transitions==1 & (history_flag==3 | history_flag==.)
// tab history_flag if all_transitions==1 & is_tym_marriage_start==0 & is_ty_marriage_start==0 & is_lyc_marriage_start==0

// FINALLY adjust dates
rename transition_year transition_year_unadj

gen transition_year = .
replace transition_year = transition_yr_marr if all_transitions==1 & inlist(history_flag,1,2) & is_tym_marriage_start==1 // prio this because SHOULD be based on history
replace transition_year = transition_year_unadj if transition_year == . & all_transitions==1 & inlist(history_flag,1,2) & is_ty_marriage_start==1 // then old date
replace transition_year = last_yr_cohab if transition_year == . & all_transitions==1 & inlist(history_flag,1,2) & is_lyc_marriage_start==1 
replace transition_year = est_marriage_date if transition_year == . & all_transitions==1 & inlist(history_flag,1,2) & est_marriage_date!=. 
bysort unique_id partner_unique_id (transition_year): replace transition_year=transition_year[1]
replace transition_year = transition_year_unadj if transition_year == . & all_transitions==1 & inlist(history_flag,1,2)
replace transition_year = transition_yr_marr if all_transitions==1 & (history_flag==3 | history_flag==. ) & is_tym_marriage_start==1 // there are lik 70 people this works for
replace transition_year = transition_year_unadj if transition_year==. & all_transitions==1 & history_flag==3 // think use MINE if not in history
replace transition_year = transition_year_unadj if transition_year==. & all_transitions==1 & history_flag==. // OR if history if missing (v. small amount)
// one problem here - the people who disappear and come back won't have a transition_year_unadj so I guess those have to stay missing if they don't have history, let's see if this makes a huge deal. two options, really: we leave as missing OR use as observed. missing is safer?

sort unique_id survey_yr

ab transition_year if ever_transition==1, m // okay, just literally using marriage year when i have it has significantly solved the problem of over-indexing
tab transition_year if all_transitions==1, m  // so, small amount missing for those not in marital history (per above)
tab transition_year if ever_transition==1 & transition_yr_marr !=transition_yr_cohab, m
tab transition_year if ever_transition==1 & transition_yr_marr ==transition_yr_cohab, m // RIGHT so when this is the case, they will litearlly ALL be even years.  I was going to say but I adjusted the years BUT these would likely be first year cohabitors then, and so I left them with assigned odd year. because cohab is estimated, it's going to be biased to start in an odd year aka a survey year, okay okay right. [and now that i have removed the plus 1, it's all odd again. the problem is - i don't know their true DURATION TRANSITIONED because I don't know TRUE START date. so the reason these are all odd is because I have all of the cohabs starting in odd years - but it's possible they started in an unobserved even year. I guess I try to account for this using first year cohab status, so is it weird these are still all odd? but i guess if they transition this quickly, they never make it PAST first year cohab status. liek should i randomly split half or is that biased? (it would just affect COHORT of transition NOT event time because it's either duration transition of 1 or 2. but that is based on literally no info. let's come back to this, this is probably ROBUSTNESS no main results)
tab transition_year if all_transitions==1 & transition_year !=transition_yr_cohab, m
tab transition_year if all_transitions==1 & transition_year ==transition_yr_cohab, m 
tab transition_year_unadj if ever_transition==1, m
tab transition_year_unadj if all_transitions==1, m

tab transition_year_unadj transition_year if ever_transition==1

// NOW adjust other years
bysort unique_id partner_unique_id: egen rel_start_all_unadj = min(current_rel_start_unadj) // retain this in case I want to compare later
bysort unique_id partner_unique_id: egen rel_start_all = min(current_rel_start)
bysort unique_id partner_unique_id: egen rel_end_all = max(current_rel_end_estimated) // this should work for all bc I didnt change end dates

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr current_rel_type ever_transition all_transitions marr_trans rel_start_all rel_end_all current_rel_start current_rel_end_estimated transition_year transition_year_sp current_rel_start_unadj transition_year_unadj master_rel_start1 master_rel_type1 master_rel_start2 master_rel_type2 master_rel_start3 master_rel_type3 

inspect rel_start_all rel_end_all

// let's save this just in case
save "$temp/psid_long_partners_matched_cleaned.dta", replace
// save "$created_data/PSID_partners.dta", replace // old partner file, not really using

********************************************************************************
********************************************************************************
********************************************************************************
**# NOW, sample restrictions
********************************************************************************
********************************************************************************
********************************************************************************

gen dur=survey_yr - rel_start_all

bysort unique_id partner_id: egen min_dur = min(dur)
bysort unique_id partner_id: egen max_dur = max(dur)
bysort unique_id partner_id: egen last_yr_observed = max(survey_yr)

********************************
* Actual restrictions
********************************
drop if rel_start_all==.
keep if rel_start_all >= 1990 & inlist(min_dur,0,1) // keeping up to two, because if got married in 2001, say, might not appear in survey until 2003, which is a problem. 
// keep if rel_start_all <= 2011 // had 2011 when we had 10 year cutoff.
// keep if rel_start_all <=2018 // now will be 2018 because 3 year cutoff (and assume 1st year of full data is 2019, so that's three years)
keep if rel_start_all <=2020 // now will be 2020 because updated to 2023 (and assume 1st year of full data is 2021, so that's three years)
keep if exclusion_couples==0

tab rel_end_all, m // I adjusted this above, "both_end_missing" is the flag (<1%)
tab status_all, m // <1% missing so prob fine?
tab rel_end_all status_all, m // the ongoing all already mostly have an end date of last survey year

tab both_left_censored, m // no 1s
tab both_start_missing, m // gone
tab both_start_est, m // this is 71 people
tab both_end_missing, m // <1 %