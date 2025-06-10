********************************************************************************
********************************************************************************
* Project: Relationship Growth Curves
* Owner: Kimberly McErlean
* Started: September 2024
* File: get_data
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files gets the data from the PSID and reorganizes it for analysis

********************************************************************************
* Take downloaded data, turn it into Stata format and rename
********************************************************************************
do "$code/stata code/data-cleaning/J348643.do"
do "$code/stata code/data-cleaning/J348643_formats.do"
do "$code/stata code/data-cleaning/00_rename_vars.do"

********************************************************************************
* Import data and reshape so it's long
********************************************************************************
use "$PSID/PSID_full_renamed.dta", clear
browse X1968_PERSON_NUM_1968 X1968_INTERVIEW_NUM_1968 // 30001 = interview; 30002 = person number

rename X1968_PERSON_NUM_1968 main_per_id
rename X1968_INTERVIEW_NUM_1968 main_fam_id

gen unique_id = (main_fam_id*1000) + main_per_id // (ER30001 * 1000) + ER30002
browse main_per_id main_fam_id unique_id

// make this to identify later if the parent of either member of the couple is in the household? these are at an individual level
gen father_unique_id = (FATHER_1968_ID * 1000) + FATHER_PERSON_NUM
gen mother_unique_id = (MOTHER_1968_ID * 1000) + MOTHER_PERSON_NUM
browse father_unique_id FATHER_1968_ID FATHER_PERSON_NUM mother_unique_id MOTHER_1968_ID MOTHER_PERSON_NUM FATHER_1988_ID_HEAD MOTHER_1988_ID_HEAD FATHER_1988_ID_WIFE MOTHER_1988_ID_WIFE

// want to see if I can make this time-fixed sample status
recode main_fam_id (1/2999 = 1 "SRC cross-section") (3001/3441 = 2 "Immigrant 97") (3442/3511 = 3 "Immigrant 99") (4001/4851 = 4 "Immigrant 17/19") (5001/6999  = 5 "1968 Census") (7001/9043 = 6 "Latino 90") (9044/9308 = 7 "Latino 92"), gen(sample_type) 
/* from FAQ:
You will need to look at the 1968 family interview number available in the individual-level files (variable ER30001).
SRC sample families have values less than 3000.
SEO sample families have values greater than 5000 and less than 7000.
Immigrant sample families have values greater than 3000 and less than 5000. (Values from 3001 to 3441 indicate that the original family was first interviewed in 1997; values from 3442 to 3511 indicate the original family was first interviewed in 1999; values from 4001-4851 indicate the original family was first interviewed in 2017; values from 4700-4851 indicate the original family was first interviewed in 2019.)
Latino sample families have values greater than 7000 and less than 9309. (Values from 7001 to 9043 indicate the original family was first interviewed in 1990; values from 9044 to 9308 indicate the original family was first interviewed in 1992.)
*/
tab sample_type, m

// and if original sample or not
tab SAMPLE,m
tab SAMPLE_STATUS_TYPE, m

/* from FAQ
okay, so this is covered already in sample variable
Follow status indicates whether we are interested in continuing to interview an individual. In general, sample members are always considered Followable. Non-Sample Members can be Followable too, if they represent a population of current interest. For example, we have in the past, followed such people as Non-Sample parents of sample children who were aged 25 or younger.

You can tell who is a sample member by looking at the individual's Person Number and Follow Status. Original Sample Members who were living in the original study FU in the first year of interviewing were given Person Numbers in the range of 001-019. Any Reference Person's (the term `Reference Person' has replaced `Head' in 2017) Spouse in the original interviewing year who was living in an institution was given a Person Number of 020. In addition, children of the Reference Person (and Spouse/Partner, if present) who were under age 25 and in an institution the first year were considered Original Sample members and given Person Numbers in the range 0021-029. All of these people are followable.

Individuals who were born into a sample family after the first interviewing year and have a sample parent are considered "born-in Sample Members" and receive Person Numbers in the range of 030-169. All born in sample members are followable.

Some individuals who qualify as sample members (because they have a sample parent) are not born into a study family, but move in later. These "Moved in Sample Members" have Person Numbers of 170 or greater and are Followable.

All other people who have ever lived in a PSID family are not sample individuals. They also receive Person Numbers of 170 or greater, but are not Followable.
*/

inspect STRATUM CLUSTER
unique unique_id STRATUM CLUSTER
// merge 1:1 unique_id using "$PSID/strata.dta", keepusing(stratum cluster) // finally resolved this to be in main file
// drop if _merge==2
// drop _merge

gen id=_n

local reshape_vars "RELEASE_ X1968_PERSON_NUM_ INTERVIEW_NUM_ RELATION_ AGE_INDV_ MARITAL_PAIRS_ MOVED_ YRS_EDUCATION_INDV_ TYPE_OF_INCOME_ TOTAL_MONEY_INCOME_ ANNUAL_HOURS_T1_INDV_ RELEASE_NUM2_ FAMILY_COMPOSITION_ AGE_HEAD_ AGE_WIFE_ SEX_HEAD_ AGE_YOUNG_CHILD_ RESPONDENT_WHO_ RACE_1_HEAD_ EMPLOY_STATUS_HEAD_ MARST_DEFACTO_HEAD_ WIDOW_LENGTH_HEAD_ WAGES_T1_HEAD_ FAMILY_INTERVIEW_NUM_ FATHER_EDUC_HEAD_ HRLY_RATE_T1_HEAD_ HRLY_RATE_T1_WIFE_ REGION_ NUM_CHILDREN_ CORE_WEIGHT_ ANNUAL_HOURS_T1_HEAD_ ANNUAL_HOURS_T1_WIFE_ LABOR_INCOME_T1_HEAD_ LABOR_INCOME_T1_WIFE_ TOTAL_INCOME_T1_FAMILY_ TAXABLE_T1_HEAD_WIFE_ EDUC1_HEAD_ EDUC1_WIFE_ WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ TOTAL_HOUSEWORK_T1_HW_ RENT_COST_V1_ MORTGAGE_COST_ HOUSE_VALUE_ HOUSE_STATUS_ VEHICLE_OWN_ POVERTY_THRESHOLD_ SEQ_NUMBER_ RESPONDENT_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_HEAD_ MOST_HOUSEWORK_T1_ AGE_OLDEST_CHILD_ FOOD_STAMPS_ HRLY_RATE_CURRENT_HEAD_ RELIGION_HEAD_ CHILDCARE_COSTS_ TRANSFER_INCOME_ WELFARE_JOINT_ NEW_WIFE_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ TYPE_TAXABLE_INCOME_ TOTAL_INCOME_T1_INDV_ COLLEGE_HEAD_ COLLEGE_WIFE_ EDUC_HEAD_ EDUC_WIFE_ SALARY_TYPE_HEAD_ FIRST_MARRIAGE_YR_WIFE_ RELIGION_WIFE_ WORK_MONEY_WIFE_ EMPLOY_STATUS_WIFE_ SALARY_TYPE_WIFE_ HRLY_RATE_CURRENT_WIFE_ RESEPONDENT_WIFE_ WORK_MONEY_HEAD_ MARST_LEGAL_HEAD_ EVER_MARRIED_HEAD_ EMPLOYMENT_INDV_ STUDENT_T1_INDV_ BIRTH_YR_INDV_ COUPLE_STATUS_HEAD_ OTHER_ASSETS_ STOCKS_MF_ WEALTH_NO_EQUITY_ WEALTH_EQUITY_ VEHICLE_VALUE_ RELATION_TO_HEAD_ NUM_MARRIED_HEAD_ FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_END_HEAD_ FIRST_WIDOW_YR_HEAD_ FIRST_DIVORCE_YR_HEAD_ FIRST_SEPARATED_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ LAST_WIDOW_YR_HEAD_ LAST_DIVORCE_YR_HEAD_ LAST_SEPARATED_YR_HEAD_ FAMILY_STRUCTURE_HEAD_ RACE_2_HEAD_ NUM_MARRIED_WIFE_ FIRST_MARRIAGE_END_WIFE_ FIRST_WIDOW_YR_WIFE_ FIRST_DIVORCE_YR_WIFE_ FIRST_SEPARATED_YR_WIFE_ LAST_MARRIAGE_YR_WIFE_ LAST_WIDOW_YR_WIFE_ LAST_DIVORCE_YR_WIFE_ LAST_SEPARATED_YR_WIFE_ FAMILY_STRUCTURE_WIFE_ RACE_1_WIFE_ RACE_2_WIFE_ STATE_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ BIRTHS_T1_OFUMS_ BIRTHS_T2_BOTH_ BIRTHS_T2_HEAD_ BIRTHS_T2_OFUMS_ BIRTHS_T2_WIFE_ WELFARE_HEAD_1_ WELFARE_WIFE_1_ LABOR_INCOME_T1_INDV_ RELEASE_NUM_ WAGES_CURRENT_HEAD_ WAGES_CURRENT_WIFE_ WAGES_T1_WIFE_ RENT_COST_V2_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_ WELFARE_HEAD_2_ WELFARE_WIFE_2_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ RACE_3_WIFE_ RACE_3_HEAD_ WAGES_ALT_T1_HEAD_ WAGES_ALT_T1_WIFE_ WEEKLY_HRS_T1_HEAD_ WEEKLY_HRS_T1_WIFE_ RACE_4_HEAD_ COR_IMM_WT_ ETHNIC_WIFE_ ETHNIC_HEAD_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_ CDS_ELIGIBLE_ TOTAL_HOUSING_ HEALTH_INSURANCE_FAM_ BANK_ASSETS_ LABOR_INC_J1_T1_HEAD_ TOTAL_WEEKS_T1_HEAD_ ANNUAL_HOURS2_T1_HEAD_ LABOR_INC_J1_T1_WIFE_ TOTAL_WEEKS_T1_WIFE_ ANNUAL_HOURS2_T1_WIFE_ WEEKLY_HRS_T2_HEAD_ LABOR_INC_J2_T1_HEAD_ LABOR_INC_J3_T1_HEAD_ LABOR_INC_J4_T1_HEAD_ LABOR_INC_J2_T1_WIFE_ LABOR_INC_J3_T1_WIFE_ LABOR_INC_J4_T1_WIFE_ DIVIDENDS_JOINT_ INTEREST_JOINT_ NUM_JOBS_T1_INDV_ BACHELOR_YR_INDV_ STUDENT_CURRENT_INDV_ COLLEGE_INDV_ SEX_WIFE_ BACHELOR_YR_WIFE_ ENROLLED_WIFE_ BACHELOR_YR_HEAD_ ENROLLED_HEAD_ WAGES2_T1_WIFE_ METRO_ CURRENTLY_WORK_HEAD_ CURRENTLY_WORK_WIFE_ EMPLOY_STATUS_T2_HEAD_ EMPLOY_STATUS_T2_WIFE_ WEEKLY_HRS_T2_WIFE_ START_YR_EMPLOYER_HEAD_ START_YR_EMPLOYER_WIFE_ START_YR_CURRENT_HEAD_ START_YR_CURRENT_WIFE_ START_YR_PREV_HEAD_ START_YR_PREV_WIFE_ YRS_CURRENT_EMPLOY_HEAD_ YRS_CURRENT_EMPLOY_WIFE_ LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ WEEKLY_HRS_T2_INDV_ LABOR_INCOME_T2_INDV_ HOUSEWORK_INDV_ RACE_4_WIFE_ HISPANICITY_WIFE_ HISPANICITY_HEAD_ CHILDCARE_HEAD_ CHILDCARE_WIFE_ ADULTCARE_HEAD_ ADULTCARE_WIFE_ TOTAL_INCOME_T2_FAMILY_ WEEKS_WORKED_T2_INDV_ FOLLOW_STATUS_ NUM_IN_HH_ NUM_NONFU_IN_HH_ NEW_WIFE_YEAR_ MOVED_YEAR_ MOVED_MONTH_ SPLITOFF_YEAR_ SPLITOFF_MONTH_ DATA_RECORD_TYPE_ SPLITOFF_ NEW_HEAD_YEAR_ HS_GRAD_HEAD_ ATTENDED_COLLEGE_HEAD_ HIGHEST_DEGREE_HEAD_ HS_GRAD_WIFE_ ATTENDED_COLLEGE_WIFE_ HIGHEST_DEGREE_WIFE_ WHERE_EDUC_HEAD_ FOREIGN_DEG_HEAD_ WHERE_EDUC_WIFE_ FOREIGN_DEG_WIFE_ YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_ DENOMINATION_HEAD_ DENOMINATION_WIFE_ DISABILITY_HEAD_ DISABILITY_WIFE_ DISABLE_HOWMUCH_HEAD_ DISABLE_HOWMUCH_WIFE_ DISABLE_WORK_HEAD_ DISABLE_WORK_WIFE_ SR_HEALTH_HEAD_ SR_HEALTH_WIFE_ SR_HEALTH_INDV_ SR_HEALTH_OTHER_ YR_RETIRED_HEAD_ YR_RETIRED_WIFE_ FAMILY_AREA_WIFE_ FAMILY_AREA_HEAD_ LIVES_FAMILY_HEAD_ LIVES_FAMILY_WIFE_ MOVED_LASTSPRING_HEAD_ MOVED_SPRING_MO_HEAD_ MOVED_SPRING_YR_HEAD_ OFUM1_ID_ OFUM2_ID_ OFUM3_ID_ OFUM4_ID_ OFUM1_REL_ OFUM2_REL_ OFUM3_REL_ OFUM4_REL_"

reshape long `reshape_vars', i(id unique_id sample_type STRATUM CLUSTER) j(survey_yr)

save "$temp/PSID_full_long.dta", replace

********************************************************************************
**# Try to create indicator of MPF from PID
********************************************************************************
use "$PSID/pid23.dta", clear // this is at a child level which makes this a little more complicated..
// should I merge this info on to childbirth history using child?

// (ER30001 * 1000) + ER30002
// (1968 interview ID multiplied by 1000) plus Person Number

gen unique_id_child = (PID2*1000) + PID3 // this is what I will match to below
browse PID2 PID3 unique_id_child

gen unique_id_mom = (PID4*1000) + PID5 // these will be for reference
browse PID4 PID5 unique_id_mom PID19
rename PID19 in_cah_mom

gen unique_id_dad = (PID23*1000) + PID24
browse PID23 PID24 unique_id_dad PID37
rename PID37 in_cah_dad

browse unique_id_child PID2 PID3 unique_id_mom PID4 PID5 unique_id_dad PID23 PID24

keep unique_id_child unique_id_mom unique_id_dad in_cah_mom in_cah_dad

tab in_cah_mom if unique_id_mom!=0
tab in_cah_dad if unique_id_dad!=0

// actually just going to save this and merge on to below
save "$temp/child_parent_lookup.dta", replace

********************************************************************************
**# Prep childbirth history files
********************************************************************************
use "$PSID/cah85_23.dta", clear // think the problem here is that I don't have the id of the *other* parent. that is annoying
// okay, we're going to try out merging both...

gen unique_id = (CAH3*1000) + CAH4
browse CAH3 CAH4 unique_id
gen unique_id_child = (CAH10*1000) + CAH11

bysort unique_id: egen child_check = max (unique_id_child)

gen cah_any_births=.
replace cah_any_births = 0 if child_check==0
replace cah_any_births = 1 if child_check!=0 & child_check!=.

browse unique_id CAH2 unique_id_child child_check cah_any_births // CAH2 is record type. so if no adoptions, get a 0 for that

merge m:1 unique_id_child using "$temp/child_parent_lookup.dta"
// this feels like not a hugely high match rate?
// OH is some of this because don't actually have children? bc think still get 1 record here even if no children. yes this is correct.
drop if _merge==2
tab cah_any_births _merge // so a lot of the nonmatched are because no children but that doesn't explain all of it
tab _merge if unique_id_child!=0 // okay, so it's actually mostly just 0s (so I think all of the adoption rows)
tab CAH2 _merge, row

browse unique_id CAH2 unique_id_child child_check cah_any_births _merge 

drop _merge

/* first rename relevant variables for ease*/
rename CAH3 int_number
rename CAH4 per_num
rename CAH10 child_int_number
rename CAH11 child_per_num
rename CAH2 event_type
rename CAH106 num_children
rename CAH5 parent_sex
rename CAH7 parent_birth_yr
rename CAH6 parent_birth_mon
rename CAH8 parent_marital_status
rename CAH9 birth_order
rename CAH12 child_sex
rename CAH15 child_birth_yr
rename CAH13 child_birth_mon
rename CAH27 child_hispanicity
rename CAH28 child_race1
rename CAH29 child_race2
rename CAH30 child_race3
rename CAH100 mom_wanted
rename CAH101 mom_timing
rename CAH102 dad_wanted
rename CAH103 dad_timing

label define wanted 1 "Yes" 5 "No"
label values mom_wanted dad_wanted wanted
replace mom_wanted = . if inlist(mom_wanted,8,9)
replace dad_wanted = . if inlist(dad_wanted,8,9)

label define timing 1 "Not at that time" 2 "None" 3 "Didn't matter"
label values mom_timing dad_timing timing
replace mom_timing = . if inlist(mom_timing,8,9)
replace dad_timing = . if inlist(dad_timing,8,9)

gen no_children=0
replace no_children=1 if child_int_number==0 & child_per_num==0 

label define sex 1 "Male" 2 "Female"
label values parent_sex sex

// trying to figure out variables for MPF
browse unique_id parent_sex unique_id_child unique_id_mom in_cah_mom unique_id_dad in_cah_dad if no_children==0

gen is_mom=0
replace is_mom=1 if unique_id == unique_id_mom & unique_id!=0

gen is_dad=0
replace is_dad=1 if unique_id == unique_id_dad & unique_id!=0

tab parent_sex is_mom, m // validate
tab parent_sex is_dad, m
tab is_mom is_dad, m

gen which_parent=.
replace which_parent = 1 if is_mom==1
replace which_parent = 2 if is_dad==1

label define which_parent 1 "Mom" 2 "Dad"
label values which_parent which_parent

gen other_parent_id=.
replace other_parent_id = unique_id_dad if which_parent==1
replace other_parent_id = unique_id_mom if which_parent==2

inspect other_parent_id if which_parent!=. // so some are 0s because just 1 parent listed

browse unique_id parent_sex which_parent other_parent_id unique_id_child unique_id_mom unique_id_dad if no_children==0

unique other_parent_id if other_parent_id!=0 & other_parent_id!=. & no_children==0, by(unique_id) gen(num_birth_partners)
bysort unique_id (num_birth_partners): replace num_birth_partners = num_birth_partners[1]
tab num_birth_partners, m
tab num_birth_partners no_children, m col

sort unique_id
browse unique_id which_parent num_birth_partners other_parent_id unique_id_child unique_id_mom unique_id_dad if no_children==0

gen any_mpf = .
replace any_mpf = 0 if num_birth_partners==1
replace any_mpf = 0 if num_birth_partners==0 & cah_any_births==1
replace any_mpf = 1 if num_birth_partners>1 & num_birth_partners<1000

tab any_mpf, m
tab no_children any_mpf, m

browse unique_id event_type no_children which_parent num_birth_partners any_mpf other_parent_id unique_id_child unique_id_mom unique_id_dad 

// this is currently LONG - one record per birth. want to make WIDE
local birthvars "int_number per_num unique_id child_int_number child_per_num unique_id_child event_type num_children parent_sex parent_birth_yr parent_birth_mon parent_marital_status birth_order child_sex child_birth_yr child_birth_mon child_hispanicity child_race1 child_race2 child_race3 mom_wanted mom_timing dad_wanted dad_timing any_mpf num_birth_partners cah_any_births other_parent_id"

keep `birthvars'

rename parent_birth_yr parent_birth_yr_0
bysort unique_id: egen parent_birth_yr = min(parent_birth_yr_0)
drop parent_birth_yr_0

rename parent_birth_mon parent_birth_mon_0
bysort unique_id: egen parent_birth_mon = min(parent_birth_mon_0)
drop parent_birth_mon_0

browse unique_id birth_order * // looks like the 98s are causing problems
sort unique_id birth_order
by unique_id: egen birth_rank = rank(birth_order), unique
browse unique_id birth_order birth_rank child_birth_yr * 
tab birth_rank birth_order

reshape wide child_int_number child_per_num unique_id_child event_type parent_marital_status num_children child_sex child_birth_yr child_birth_mon child_hispanicity child_race1 child_race2 child_race3 mom_wanted mom_timing dad_wanted dad_timing birth_order other_parent_id, i(int_number per_num unique_id parent_sex parent_birth_yr parent_birth_mon)  j(birth_rank)

gen INTERVIEW_NUM_1968 = int_number

tab cah_any_births any_mpf, m row

foreach var in *{
	rename `var' cah_`var' // so I know where it came from
}

rename cah_int_number int_number
rename cah_per_num per_num
rename cah_unique_id unique_id
rename cah_INTERVIEW_NUM_1968 INTERVIEW_NUM_1968
rename cah_cah_any_births cah_any_births
gen partner_id = unique_id

forvalues n=1/20{
	rename cah_parent_marital_status`n' cah_parent_marst`n' // think getting too long for what I want to work
}

browse unique_id cah_any_mpf cah_other_parent*
   
save "$created_data/birth_history_wide.dta", replace

// need to actually increment MPF better GAH bc currently fixed, but in theory, needs to increment with each birth? so I can make time-varying?
// though - the incidence is so small, an "ever" metric also probably works...

********************************************************************************
**# Attempt to get births by individual HH / survey year
********************************************************************************
use "$temp/PSID_full_long.dta", clear

gen relationship=.
replace relationship=0 if RELATION_==0
replace relationship=1 if inlist(RELATION_,1,10)
replace relationship=2 if inlist(RELATION_,2,20,22,88)
replace relationship=3 if inrange(RELATION_,23,87) | inrange(RELATION_,90,98) | inrange(RELATION_,3,9)

label define relationship 0 "not in sample" 1 "head" 2 "partner" 3 "other"
label values relationship relationship

keep main_fam_id unique_id survey_yr relationship FAMILY_INTERVIEW_NUM_ SEQ_NUMBER_ AGE_INDV_ NUM_IN_HH_ NUM_CHILDREN_ NUM_NONFU_IN_HH_ AGE_YOUNG_CHILD_ AGE_OLDEST_CHILD_

merge m:1 unique_id using "$created_data/birth_history_wide.dta", keepusing(cah_child_birth_yr*)
drop if _merge==2

tab AGE_INDV _merge, m row
tab survey_yr _merge, m row

gen child_history_match=0
replace child_history_match=1 if _merge==3
drop _merge

// browse unique_id cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4

gen indiv_births_pre1968 = 0
forvalues c=1/20{
	replace indiv_births_pre1968 = indiv_births_pre1968 + 1 if inrange(cah_child_birth_yr`c',1900,1967)
}

gen individ_birth_in_yr=0
forvalues c=1/20{
	replace individ_birth_in_yr = individ_birth_in_yr + 1 if cah_child_birth_yr`c'==survey_yr
}

browse unique_id survey_yr indiv_births_pre1968 individ_birth_in_yr cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4 cah_child_birth_yr*

forvalues y=1968/2023{
	gen individ_birth_`y' = 0
	forvalues c=1/20{
		replace individ_birth_`y' = individ_birth_`y' + 1 if cah_child_birth_yr`c'==`y'
	}
}	

browse unique_id survey_yr cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4 individ_birth_* 

bysort survey_yr FAMILY_INTERVIEW_NUM_: egen hh_births_in_yr = total(individ_birth_in_yr)
bysort survey_yr FAMILY_INTERVIEW_NUM_: egen hh_births_pre1968 = total(indiv_births_pre1968)

forvalues y=1968/2023{
	bysort survey_yr FAMILY_INTERVIEW_NUM_: egen hh_births_`y' = total(individ_birth_`y')
}

inspect FAMILY_INTERVIEW_NUM_ if SEQ_NUMBER!=0

browse  unique_id FAMILY_INTERVIEW_NUM_ survey_yr hh_births_in_yr individ_birth_in_yr hh_births_pre1968 indiv_births_pre1968 cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4 cah_child_birth_yr*
browse  unique_id FAMILY_INTERVIEW_NUM_ survey_yr cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4 hh_births_* individ_birth_*

save "$temp/hh_birth_history_file.dta", replace

sort unique_id survey_yr
drop if FAMILY_INTERVIEW_NUM_==.
drop if survey_yr < 1985 // so removes those in 1968 family of origin, i think *this* is causing problems

forvalues y=1968/2023{
	drop hh_births_`y'
	bysort survey_yr FAMILY_INTERVIEW_NUM_: egen hh_births_`y' = total(individ_birth_`y')
}

browse  unique_id main_fam_id FAMILY_INTERVIEW_NUM_ survey_yr relationship hh_births_199* individ_birth_199* cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4 

collapse (max) hh_births_1* hh_births_2* individ_birth_1* individ_birth_2*, by(unique_id)

save "$created_data/hh_birth_history_file_byUNIQUE.dta", replace

/*
********************************************************************************
**# This is births by 1968 fam id but not helpful for splitoffs and such
********************************************************************************
// can I reshape wide AGAIN so it's by HH and use later to get incremental number of children??
use "$created_data/birth_history_wide.dta", clear

keep int_number per_num cah_child_birth_yr*
rename int_number main_fam_id 

bysort int_number: egen per_id = rank(per_num)
drop per_num

foreach var in cah_child_birth_yr*{
	rename `var' `var'_x
}

reshape wide cah_child_birth_yr*, i(main_fam_id) j(per_id)

gen hh_births_pre1968 = 0
forvalues p=1/128{
	forvalues c=1/20{
		replace hh_births_pre1968 = hh_births_pre1968 + 1 if inrange(cah_child_birth_yr`c'_x`p',1900,1967)
	}
}

browse main_fam_id hh_births_pre1968 cah*

forvalues y=1968/2023{
	gen hh_births_`y' = 0
	forvalues p=1/128{
		forvalues c=1/20{
			replace hh_births_`y' = hh_births_`y' + 1 if cah_child_birth_yr`c'_x`p'==`y'
		}
	}	
}

browse main_fam_id hh_births_pre1968 hh_births_2001 cah*

save "$created_data/1968hh_birth_history_file.dta", replace
*/

// Stopped here June 10 - the family matrix isn't yet updated...do I try to do something else? or do I wait...

********************************************************************************
**# Try to get a variable for elderly in HH using family matrix
********************************************************************************
// no - this isn't going to work for AGE because age not in this file
// so - should I actually just do this using the full file before I drop non-couples??
// just a bit harder to get the PARENT info in that file because only has relationships to reference (I guess I could use parent and parent-in-law?)
// let's see what makes sense...

use "$PSID/family_matrix_68_21.dta", clear // so this file is by year - the full matrix

gen unique_id = (MX5*1000) + MX6 // 5 = interview number; 6 = person number
gen unique_id_alter = (MX10*1000) + MX11

// browse MX2 unique_id MX5 MX6 unique_id_alter MX8 MX10 MX11
browse MX2 unique_id unique_id_alter MX8

gen lives_with_parent=0
replace lives_with_parent = 1 if inlist(MX8,30,33,35,37,38,39) // okay this is EGO to alter, so if I want ego lives with parent (the alter), they are actually identified as CHILD.
// the relationship will NOT be parent. I guess I can validate this with relationship to ref later?

gen lives_with_grandparent=0
replace lives_with_grandparent = 1 if inlist(MX8,60,61,62,63,63,64,65,80,81) // so same, this is GRANDCHILD (and great grandchild)

tab MX8 lives_with_parent, m
tab MX8 lives_with_grandparent, m

rename MX2 survey_yr

browse unique_id survey_yr lives_with_parent lives_with_grandparent MX8

preserve

collapse (max) lives_with_parent lives_with_grandparent, by(unique_id survey_yr)
save "$temp/parent_coresidence_lookup.dta", replace

restore

********************************************************************************
**# Also create a family composition lookup (based on age of coresidents)
********************************************************************************
// need to do this since couldn't do with above file

use "$temp/PSID_full_long.dta", clear

drop if SEQ_NUMBER_ == 0
browse unique_id FAMILY_INTERVIEW_NUM_ main_fam_id survey_yr AGE_INDV_ RELATION_ 

gen age65up=0
replace age65up =1 if  AGE_INDV_ >=65 & AGE_INDV_<200

bysort survey_yr FAMILY_INTERVIEW_NUM_: egen num_65up_hh = total(age65up)
tab num_65up_hh , m

sort unique_id survey_yr
browse unique_id FAMILY_INTERVIEW_NUM_ survey_yr num_65up_hh age65up AGE_INDV_ RELATION_ main_fam_id 

preserve
drop if FAMILY_INTERVIEW_NUM_ == .

collapse (max) num_65up_hh (sum) age65up, by(FAMILY_INTERVIEW_NUM_ survey_yr)
save "$temp/hh_comp_lookup.dta", replace

restore