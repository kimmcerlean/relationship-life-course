
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: prep_imputation_for_individs
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files uses the individual level data from the couples to impute base data
* necessary for final analysis.


********************************************************************************
**# First get survey responses for each individual in couple from main file
********************************************************************************
use "$PSID/PSID_full_renamed.dta", clear
browse X1968_PERSON_NUM_1968 X1968_INTERVIEW_NUM_1968 // 30001 = interview; 30002 = person number

rename X1968_PERSON_NUM_1968 main_per_id
rename X1968_INTERVIEW_NUM_1968 main_fam_id

gen unique_id = (main_fam_id*1000) + main_per_id // (ER30001 * 1000) + ER30002
browse main_per_id main_fam_id unique_id

browse RELATION_* YRS_EDUC* AGE_INDV_* if inlist(unique_id, 16032, 16176)

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

// figure out what variables i need / can help me figure this out - need indicator of a. in survey and b. relationship status (easy for non-heads) - so need to be INDIVIDUAL, not family variables, right?!
browse unique_id SEQ_NUMBER_1995 SEQ_NUMBER_1996 MARITAL_PAIRS_1995 MARITAL_PAIRS_1996 RELATION_1995 RELATION_1996

forvalues y=1969/1997{
	gen in_sample_`y'=.
	replace in_sample_`y'=0 if SEQ_NUMBER_`y'==0 | inrange(SEQ_NUMBER_`y',60,90)
	replace in_sample_`y'=1 if inrange(SEQ_NUMBER_`y',1,59)
}

forvalues y=1999(2)2023{
	gen in_sample_`y'=.
	replace in_sample_`y'=0 if SEQ_NUMBER_`y'==0 | inrange(SEQ_NUMBER_`y',60,90)
	replace in_sample_`y'=1 if inrange(SEQ_NUMBER_`y',1,59)	
}

label define hh_status 0 "not in sample" 1 "in sample" 2 "institutionalized" 3 "new hh" 4 "died"
foreach y of numlist 1969/1997 1999(2)2023{
	gen hh_status_`y'=.
	replace hh_status_`y'=0 if SEQ_NUMBER_`y'==0 
	replace hh_status_`y'=1 if inrange(SEQ_NUMBER_`y',1,20) // in sample
	replace hh_status_`y'=2 if inrange(SEQ_NUMBER_`y',51,59) // institutionalized
	replace hh_status_`y'=3 if inrange(SEQ_NUMBER_`y',71,80) // new HH 
	replace hh_status_`y'=4 if inrange(SEQ_NUMBER_`y',81,89) // died
	label values hh_status_`y' hh_status
}
	
label define relationship 0 "not in sample" 1 "head" 2 "partner" 3 "other"
forvalues y=1969/1997{
	gen relationship_`y'=.
	replace relationship_`y'=0 if RELATION_`y'==0
	replace relationship_`y'=1 if inlist(RELATION_`y',1,10)
	replace relationship_`y'=2 if inlist(RELATION_`y',2,20,22,88)
	replace relationship_`y'=3 if inrange(RELATION_`y',23,87) | inrange(RELATION_`y',90,98) | inrange(RELATION_`y',3,9)
	label values relationship_`y' relationship
}

forvalues y=1999(2)2023{
	gen relationship_`y'=.
	replace relationship_`y'=0 if RELATION_`y'==0
	replace relationship_`y'=1 if inlist(RELATION_`y',1,10)
	replace relationship_`y'=2 if inlist(RELATION_`y',2,20,22,88)
	replace relationship_`y'=3 if inrange(RELATION_`y',23,87) | inrange(RELATION_`y',90,98) | inrange(RELATION_`y',3,9)
	label values relationship_`y' relationship
}

// browse unique_id in_sample_* relationship_* MARITAL_PAIRS_* HOUSEWORK_WIFE_* HOUSEWORK_HEAD_*
keep unique_id main_fam_id sample_type SAMPLE SAMPLE_STATUS_TYPE PERMANENT_ATTRITION ANY_ATTRITION YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST FIRST_BIRTH_YR LAST_BIRTH_YR NUM_BIRTHS in_sample_* hh_status* relationship_* MARITAL_PAIRS_* SEX AGE_INDV* YRS_EDUCATION_INDV* EDUC1_WIFE_* EDUC1_HEAD_* EDUC_WIFE_* EDUC_HEAD_* LABOR_INCOME_T1_WIFE_* LABOR_INCOME_T2_WIFE_* WAGES_T1_WIFE_* LABOR_INCOME_T1_HEAD_* LABOR_INCOME_T2_HEAD_* WAGES_T1_HEAD_* TAXABLE_T1_HEAD_WIFE_* WEEKLY_HRS1_T1_WIFE_* WEEKLY_HRS_T1_WIFE_* WEEKLY_HRS1_T1_HEAD_* WEEKLY_HRS_T1_HEAD_* HOUSEWORK_HEAD_* HOUSEWORK_WIFE_* TOTAL_HOUSEWORK_T1_HW_* MOST_HOUSEWORK_T1* EMPLOY_STATUS_HEAD_* EMPLOY_STATUS1_HEAD_* EMPLOY_STATUS2_HEAD_* EMPLOY_STATUS3_HEAD_* EMPLOY_STATUS_WIFE_* EMPLOY_STATUS1_WIFE_* EMPLOY_STATUS2_WIFE_* EMPLOY_STATUS3_WIFE_* NUM_CHILDREN_* AGE_YOUNG_CHILD_* AGE_HEAD_* AGE_WIFE_* TOTAL_INCOME_T1_FAMILY_* FAMILY_INTERVIEW_NUM_* EMPLOY_STATUS_T2_HEAD_* EMPLOY_STATUS_T2_WIFE_* WEEKLY_HRS_T2_HEAD_* WEEKLY_HRS_T2_WIFE_* START_YR_EMPLOYER_HEAD_* START_YR_EMPLOYER_WIFE_* START_YR_CURRENT_HEAD_* START_YR_CURRENT_WIFE_* START_YR_PREV_HEAD_* START_YR_PREV_WIFE_* YRS_CURRENT_EMPLOY_HEAD_* YRS_CURRENT_EMPLOY_WIFE_*  WEEKLY_HRS_T2_INDV_* ANNUAL_HOURS_T1_INDV_* ANNUAL_HOURS_T1_HEAD* ANNUAL_HOURS_T1_WIFE* EMPLOYMENT_INDV* LABOR_INCOME_T1_INDV* LABOR_INCOME_T2_INDV* TOTAL_INCOME_T1_INDV* BIRTH_YR_INDV_* RACE_* HOUSEWORK_INDV_* HISPANICITY_* CHILDCARE_HEAD_* CHILDCARE_WIFE_* ADULTCARE_HEAD_* ADULTCARE_WIFE_* TOTAL_INCOME_T2_FAMILY_* WEEKS_WORKED_T2_INDV_* NUM_IN_HH_* NEW_WIFE_YEAR_* MOVED_* MOVED_YEAR_* MOVED_MONTH_* MOVED_LASTSPRING_HEAD* SPLITOFF_YEAR_* SPLITOFF_MONTH_* DATA_RECORD_TYPE_* SPLITOFF_* FAMILY_ID_SO_* COMPOSITION_CHANGE_* NEW_HEAD_* NEW_WIFE_* NEW_HEAD_YEAR_* NEW_WIFE_YEAR_* BIRTHS_T1_HEAD_* BIRTHS_T1_WIFE_* BIRTHS_T1_BOTH_* BIRTHS_T1_OFUMS_* BIRTHS_T2_BOTH_* BIRTHS_T2_HEAD_* BIRTHS_T2_WIFE_* BIRTHS_T2_OFUMS_* COLLEGE_WIFE_* COLLEGE_HEAD_* COLLEGE_INDV_* BACHELOR_YR_INDV_* BACHELOR_YR_WIFE_* BACHELOR_YR_HEAD_* STUDENT_T1_INDV_* STUDENT_CURRENT_INDV_* ENROLLED_WIFE_* ENROLLED_HEAD_* YR_EDUC_UPD_HEAD_* YR_EDUC_UPD_WIFE_* HS_GRAD_HEAD_* ATTENDED_COLLEGE_HEAD_* HIGHEST_DEGREE_HEAD_* HS_GRAD_WIFE_* ATTENDED_COLLEGE_WIFE_* HIGHEST_DEGREE_WIFE_* DISABILITY_HEAD* DISABILITY_WIFE* DISABLE_HOWMUCH_HEAD* DISABLE_HOWMUCH_WIFE* SR_HEALTH_HEAD* SR_HEALTH_WIFE* SR_HEALTH_INDV*  SR_HEALTH_OTHER* YR_RETIRED_HEAD* YR_RETIRED_WIFE* FAMILY_AREA_WIFE* FAMILY_AREA_HEAD* LIVES_FAMILY_HEAD* LIVES_FAMILY_WIFE* FATHER_EDUC_HEAD* MOTHER_EDUC_HEAD* FATHER_EDUC_WIFE* MOTHER_EDUC_WIFE* FAMILY_STRUCTURE_WIFE* FAMILY_STRUCTURE_HEAD* HOUSE_STATUS* RELIGION_HEAD_* RELIGION_WIFE_* DENOMINATION_WIFE_* DENOMINATION_HEAD_* REGION_* RESPONDENT_* RESPONDENT_WHO_* MOVED_SPRING_MO_HEAD* MOVED_SPRING_YR_HEAD* OFUM*_ID_* OFUM*_REL_* father_unique_id FATHER_YR_BORN mother_unique_id MOTHER_YR_BORN PSID_COHORT

gen partner_id = unique_id

foreach y of numlist 1969/1997 1999(2)2023{
	gen in_sample_sp_`y' = in_sample_`y'
	gen relationship_sp_`y' = relationship_`y'
	gen MARITAL_PAIRS_sp_`y' = MARITAL_PAIRS_`y'
	gen MOVED_sp_`y' = MOVED_`y'
	gen MOVED_YEAR_sp_`y' = MOVED_YEAR_`y'
	gen SPLITOFF_sp_`y' = SPLITOFF_`y'
	gen SPLITOFF_YEAR_sp_`y' = SPLITOFF_YEAR_`y'
	gen hh_status_sp_`y' = hh_status_`y'
}

gen SEX_sp = SEX
gen SAMPLE_sp = SAMPLE

forvalues y=1969/1984{ // let's keep a few years to see if we have ANY data for people before they were observed
	drop in_sample_`y'
	drop in_sample_sp_`y'
	drop relationship_`y'
	drop relationship_sp_`y'
	drop MARITAL_PAIRS_`y'
	drop MARITAL_PAIRS_sp_`y'
}

foreach var in AGE_INDV_ YRS_EDUCATION_INDV_ EDUC1_WIFE_ EDUC1_HEAD_ EDUC_WIFE_ EDUC_HEAD_ LABOR_INCOME_T1_WIFE_ LABOR_INCOME_T2_WIFE_ WAGES_T1_WIFE_ LABOR_INCOME_T1_HEAD_ LABOR_INCOME_T2_HEAD_ WAGES_T1_HEAD_ TAXABLE_T1_HEAD_WIFE_ WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_ HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ TOTAL_HOUSEWORK_T1_HW_ MOST_HOUSEWORK_T1_ EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ AGE_HEAD_ AGE_WIFE_ TOTAL_INCOME_T1_FAMILY_ FAMILY_INTERVIEW_NUM_ EMPLOY_STATUS_T2_HEAD_ EMPLOY_STATUS_T2_WIFE_ WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_ START_YR_EMPLOYER_HEAD_ START_YR_EMPLOYER_WIFE_ START_YR_CURRENT_HEAD_ START_YR_CURRENT_WIFE_ START_YR_PREV_HEAD_ START_YR_PREV_WIFE_ YRS_CURRENT_EMPLOY_HEAD_ YRS_CURRENT_EMPLOY_WIFE_  WEEKLY_HRS_T2_INDV_ ANNUAL_HOURS_T1_INDV_ ANNUAL_HOURS_T1_HEAD_ ANNUAL_HOURS_T1_WIFE_ EMPLOYMENT_INDV_ LABOR_INCOME_T1_INDV_ LABOR_INCOME_T2_INDV_ TOTAL_INCOME_T1_INDV_ BIRTH_YR_INDV_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_ RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_4_WIFE_ HOUSEWORK_INDV_ HISPANICITY_HEAD_ HISPANICITY_WIFE_ CHILDCARE_HEAD_ CHILDCARE_WIFE_ ADULTCARE_HEAD_ ADULTCARE_WIFE_ TOTAL_INCOME_T2_FAMILY_ WEEKS_WORKED_T2_INDV_ NUM_IN_HH_ NEW_WIFE_YEAR_ MOVED_ MOVED_YEAR_ MOVED_MONTH_ SPLITOFF_YEAR_ SPLITOFF_MONTH_ DATA_RECORD_TYPE_ SPLITOFF_ MOVED_sp_ MOVED_YEAR_sp_ SPLITOFF_sp_ SPLITOFF_YEAR_sp_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_HEAD_ NEW_WIFE_ NEW_WIFE_YEAR_ hh_status_ hh_status_sp_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ BIRTHS_T1_OFUMS_ BIRTHS_T2_BOTH_ BIRTHS_T2_HEAD_ BIRTHS_T2_WIFE_ BIRTHS_T2_OFUMS_ COLLEGE_WIFE_ COLLEGE_HEAD_ COLLEGE_INDV_ BACHELOR_YR_INDV_ BACHELOR_YR_WIFE_ BACHELOR_YR_HEAD_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ ENROLLED_WIFE_ ENROLLED_HEAD_ NEW_HEAD_YEAR_ YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_ HS_GRAD_HEAD_ ATTENDED_COLLEGE_HEAD_ HIGHEST_DEGREE_HEAD_ HS_GRAD_WIFE_ ATTENDED_COLLEGE_WIFE_ HIGHEST_DEGREE_WIFE_ DISABILITY_HEAD_ DISABILITY_WIFE_ DISABLE_HOWMUCH_HEAD_ DISABLE_HOWMUCH_WIFE_ SR_HEALTH_HEAD_ SR_HEALTH_WIFE_ SR_HEALTH_INDV_ SR_HEALTH_OTHER_ YR_RETIRED_HEAD_ YR_RETIRED_WIFE_ FAMILY_AREA_WIFE_ FAMILY_AREA_HEAD_ LIVES_FAMILY_HEAD_ LIVES_FAMILY_WIFE_ FATHER_EDUC_HEAD_ MOTHER_EDUC_HEAD_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ FAMILY_STRUCTURE_WIFE_ FAMILY_STRUCTURE_HEAD_ HOUSE_STATUS_ RELIGION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_ DENOMINATION_HEAD_ REGION_ MOVED_LASTSPRING_HEAD_ RESPONDENT_ RESPONDENT_WHO_ MOVED_SPRING_MO_HEAD_ MOVED_SPRING_YR_HEAD_ OFUM*_ID_ OFUM*_REL_{
	forvalues y=1968/1984{
		capture drop `var'`y' // in case var not in all years
	}
}

drop *_1968

save "$temp/individual_sample_info.dta", replace

********************************************************************************
**# Now merge this info on to individuals and recode core variables
* JUST for individuals; not doing any couple-level
********************************************************************************
use "$created_data/couple_list_individ.dta", clear

merge m:1 unique_id using "$temp/individual_sample_info.dta" //
drop if _merge==2
drop _merge

drop *_sp_*
drop SEX_sp
drop SAMPLE_sp

merge m:1 partner_id using "$temp/individual_sample_info.dta", keepusing(*_sp_* *_sp) // this way, I know which of them is the OG, which is the moved in, and when that happened
drop if _merge==2
drop _merge

browse unique_id partner_id SEX SEX_sp

save "$temp/individual_vars_imputation_wide.dta", replace

use "$temp/individual_vars_imputation_wide.dta", clear
// misstable summarize LABOR_INCOME_T2_INDV_*, all
// misstable summarize WEEKLY_HRS_T2_INDV_*, all
// misstable summarize ANNUAL_HOURS_T1_INDV_*, all
misstable summarize *_INDV_* RESPONDENT* *OFUMS* OFUM*, all // okay so NO missings ever LOL, always 0
// misstable summarize LABOR_INCOME_T2_HEAD_*, all // okay, it is right for head and wife I think?
misstable summarize *_HEAD_*, all
misstable summarize *_WIFE_*, all

browse in_sample_1999 in_sample_2001 in_sample_2003 in_sample_2005 in_sample_2007 LABOR_INCOME_T2_INDV_*

forvalues y=1985/1997{
	capture replace AGE_INDV_`y'=. if in_sample_`y'==0
	capture replace EMPLOYMENT_INDV_`y'=. if in_sample_`y'==0
	capture replace YRS_EDUCATION_INDV_`y'=. if in_sample_`y'==0
	capture replace TOTAL_INCOME_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace ANNUAL_HOURS_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace LABOR_INCOME_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace LABOR_INCOME_T2_INDV_`y'=. if in_sample_`y'==0
	capture replace WEEKLY_HRS_T2_INDV_`y'=. if in_sample_`y'==0
	capture replace BIRTH_YR_INDV_`y'=. if in_sample_`y'==0
	capture replace HOUSEWORK_INDV_`y'=. if in_sample_`y'==0
	capture replace WEEKS_WORKED_T2_INDV_`y'=. if in_sample_`y'==0
	capture replace STUDENT_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace COLLEGE_INDV_`y'=. if in_sample_`y'==0
	capture replace BACHELOR_YR_INDV_`y'=. if in_sample_`y'==0
	capture replace STUDENT_CURRENT_INDV_`y'=. if in_sample_`y'==0
	capture replace SR_HEALTH_INDV_`y'=. if in_sample_`y'==0
}

forvalues y=1999(2)2023{
	capture replace AGE_INDV_`y'=. if in_sample_`y'==0
	capture replace EMPLOYMENT_INDV_`y'=. if in_sample_`y'==0
	capture replace YRS_EDUCATION_INDV_`y'=. if in_sample_`y'==0
	capture replace TOTAL_INCOME_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace ANNUAL_HOURS_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace LABOR_INCOME_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace LABOR_INCOME_T2_INDV_`y'=. if in_sample_`y'==0
	capture replace WEEKLY_HRS_T2_INDV_`y'=. if in_sample_`y'==0
	capture replace BIRTH_YR_INDV_`y'=. if in_sample_`y'==0
	capture replace HOUSEWORK_INDV_`y'=. if in_sample_`y'==0
	capture replace WEEKS_WORKED_T2_INDV_`y'=. if in_sample_`y'==0
	capture replace STUDENT_T1_INDV_`y'=. if in_sample_`y'==0
	capture replace COLLEGE_INDV_`y'=. if in_sample_`y'==0
	capture replace BACHELOR_YR_INDV_`y'=. if in_sample_`y'==0
	capture replace STUDENT_CURRENT_INDV_`y'=. if in_sample_`y'==0
	capture replace SR_HEALTH_INDV_`y'=. if in_sample_`y'==0
}

misstable summarize *_INDV_*, all // okay NOW there are missings

egen birth_yr = rowmin(BIRTH_YR_INDV_*)
replace birth_yr=. if birth_yr==9999
browse unique_id birth_yr BIRTH_YR_INDV_*

drop BIRTH_YR_INDV_*

reshape long MARITAL_PAIRS_ in_sample_ relationship_ FAMILY_INTERVIEW_NUM_ AGE_INDV_ YRS_EDUCATION_INDV_ EDUC1_WIFE_ EDUC1_HEAD_ EDUC_WIFE_ EDUC_HEAD_ LABOR_INCOME_T1_WIFE_ LABOR_INCOME_T2_WIFE_ WAGES_T1_WIFE_ LABOR_INCOME_T1_HEAD_ LABOR_INCOME_T2_HEAD_ WAGES_T1_HEAD_ TAXABLE_T1_HEAD_WIFE_ WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_ HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ TOTAL_HOUSEWORK_T1_HW_ MOST_HOUSEWORK_T1_ EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ AGE_HEAD_ AGE_WIFE_ TOTAL_INCOME_T1_FAMILY_ EMPLOY_STATUS_T2_HEAD_ EMPLOY_STATUS_T2_WIFE_ WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_ START_YR_EMPLOYER_HEAD_ START_YR_EMPLOYER_WIFE_ START_YR_CURRENT_HEAD_ START_YR_CURRENT_WIFE_ START_YR_PREV_HEAD_ START_YR_PREV_WIFE_ YRS_CURRENT_EMPLOY_HEAD_ YRS_CURRENT_EMPLOY_WIFE_  WEEKLY_HRS_T2_INDV_ ANNUAL_HOURS_T1_INDV_ ANNUAL_HOURS_T1_HEAD_ ANNUAL_HOURS_T1_WIFE_ EMPLOYMENT_INDV_ LABOR_INCOME_T1_INDV_ LABOR_INCOME_T2_INDV_ TOTAL_INCOME_T1_INDV_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_ RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_4_WIFE_ HOUSEWORK_INDV_ HISPANICITY_HEAD_ HISPANICITY_WIFE_ CHILDCARE_HEAD_ CHILDCARE_WIFE_ ADULTCARE_HEAD_ ADULTCARE_WIFE_ TOTAL_INCOME_T2_FAMILY_ WEEKS_WORKED_T2_INDV_ NUM_IN_HH_ MOVED_ MOVED_YEAR_ MOVED_MONTH_ SPLITOFF_YEAR_ SPLITOFF_MONTH_ DATA_RECORD_TYPE_ SPLITOFF_ MOVED_sp_ MOVED_YEAR_sp_ SPLITOFF_sp_ SPLITOFF_YEAR_sp_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_HEAD_ NEW_WIFE_ NEW_HEAD_YEAR_ NEW_WIFE_YEAR_ hh_status_ hh_status_sp_ in_sample_sp_ relationship_sp_ MARITAL_PAIRS_sp_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ BIRTHS_T1_OFUMS_ BIRTHS_T2_BOTH_ BIRTHS_T2_HEAD_ BIRTHS_T2_WIFE_ BIRTHS_T2_OFUMS_ COLLEGE_WIFE_ COLLEGE_HEAD_ COLLEGE_INDV_ BACHELOR_YR_INDV_ BACHELOR_YR_WIFE_ BACHELOR_YR_HEAD_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ ENROLLED_WIFE_ ENROLLED_HEAD_ YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_ HS_GRAD_HEAD_ ATTENDED_COLLEGE_HEAD_ HIGHEST_DEGREE_HEAD_ HS_GRAD_WIFE_ ATTENDED_COLLEGE_WIFE_ HIGHEST_DEGREE_WIFE_ DISABILITY_HEAD_ DISABILITY_WIFE_ DISABLE_HOWMUCH_HEAD_ DISABLE_HOWMUCH_WIFE_ SR_HEALTH_HEAD_ SR_HEALTH_WIFE_ SR_HEALTH_INDV_ SR_HEALTH_OTHER_ YR_RETIRED_HEAD_ YR_RETIRED_WIFE_ FAMILY_AREA_WIFE_ FAMILY_AREA_HEAD_ LIVES_FAMILY_HEAD_ LIVES_FAMILY_WIFE_ FATHER_EDUC_HEAD_ MOTHER_EDUC_HEAD_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ FAMILY_STRUCTURE_WIFE_ FAMILY_STRUCTURE_HEAD_ HOUSE_STATUS_ RELIGION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_ DENOMINATION_HEAD_ REGION_ MOVED_LASTSPRING_HEAD_ RESPONDENT_ RESPONDENT_WHO_ MOVED_SPRING_MO_HEAD_ MOVED_SPRING_YR_HEAD_ OFUM1_ID_ OFUM2_ID_ OFUM3_ID_ OFUM4_ID_ OFUM1_REL_ OFUM2_REL_ OFUM3_REL_ OFUM4_REL_, ///
 i(unique_id partner_id rel_start_all min_dur max_dur rel_end_all last_yr_observed ended SEX) j(survey_yr)

// want consecutive waves to make some things easier later
egen wave = group(survey_yr)

********************************************************************************
**# Now that it's long, recode core variables.
* JUST for individuals; not doing any couple-level
********************************************************************************
//
tab survey_yr SEX if relationship_==1 & MARITAL_PAIRS_==1, row

// fill in missing birthdates from age if possible (And check against age)
browse unique_id survey_yr birth_yr AGE_INDV_
replace AGE_INDV_=. if AGE_INDV_==999
replace AGE_INDV_ = survey_yr - birth_yr if AGE_INDV_==.
browse unique_id survey_yr birth_yr AGE_INDV_ if birth_yr==.
replace birth_yr = survey_yr - AGE_INDV_ if birth_yr==.

gen under18=.
replace under18 = 0 if AGE_INDV_ >=18 & AGE_INDV_!=.
replace under18 = 1 if AGE_INDV_ < 18 & AGE_INDV_!=.

// t-1 income
browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_T1_HEAD_WIFE TOTAL_INCOME_T1_FAMILY LABOR_INCOME_T1_HEAD WAGES_T1_HEAD LABOR_INCOME_T1_WIFE_ WAGES_T1_WIFE_ 

	// to use: WAGES_HEAD_ WAGES_WIFE_ -- wife not asked until 1993? okay labor income??
	// wages and labor income asked for head whole time. labor income wife 1968-1993, wages for wife, 1993 onwards

gen earnings_t1_wife=.
replace earnings_t1_wife = LABOR_INCOME_T1_WIFE_ if inrange(survey_yr,1968,1993)
replace earnings_t1_wife = WAGES_T1_WIFE_ if inrange(survey_yr,1994,2023)
replace earnings_t1_wife=. if earnings_t1_wife== 9999999

gen earnings_t1_head=.
replace earnings_t1_head = LABOR_INCOME_T1_HEAD if inrange(survey_yr,1968,1993)
replace earnings_t1_head = WAGES_T1_HEAD if inrange(survey_yr,1994,2023)
replace earnings_t1_head=. if earnings_t1_head== 9999999

// t-1 weekly hours
browse unique_id survey_yr WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_

gen weekly_hrs_t1_wife = .
replace weekly_hrs_t1_wife = WEEKLY_HRS1_T1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_wife = WEEKLY_HRS_T1_WIFE_ if survey_yr >=1994
replace weekly_hrs_t1_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_WIFE_,9,0)
replace weekly_hrs_t1_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==1
replace weekly_hrs_t1_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==2
replace weekly_hrs_t1_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==3
replace weekly_hrs_t1_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==4
replace weekly_hrs_t1_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==5
replace weekly_hrs_t1_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==6
replace weekly_hrs_t1_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==7
replace weekly_hrs_t1_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_WIFE_ ==8
replace weekly_hrs_t1_wife=. if weekly_hrs_t1_wife==999

gen weekly_hrs_t1_head = .
replace weekly_hrs_t1_head = WEEKLY_HRS1_T1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_head = WEEKLY_HRS_T1_HEAD_ if survey_yr >=1994
replace weekly_hrs_t1_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_HEAD_,9,0)
replace weekly_hrs_t1_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==1
replace weekly_hrs_t1_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==2
replace weekly_hrs_t1_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==3
replace weekly_hrs_t1_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==4
replace weekly_hrs_t1_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==5
replace weekly_hrs_t1_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==6
replace weekly_hrs_t1_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==7
replace weekly_hrs_t1_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_HEAD_ ==8
replace weekly_hrs_t1_head=. if weekly_hrs_t1_head==999

// create individual variable using annual version? no but that's not helpful either, because only through 1993? I guess better than nothing
browse unique_id survey_yr relationship_ ANNUAL_HOURS_T1_INDV
gen weekly_hrs_t1_indv = round(ANNUAL_HOURS_T1_INDV / 52,1)
browse unique_id survey_yr relationship_ weekly_hrs_t1_indv weekly_hrs_t1_head weekly_hrs_t1_wife ANNUAL_HOURS_T1_INDV

// current employment
browse unique_id survey_yr relationship_ EMPLOYMENT_INDV_ EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// 1968-1975: 1 "working now" 2 "unemployed" 3 "retired / disabled" 4 "housewife" 5 "student" 6 "other"
// 1976+: 1 "working now" 2 "temp laid off" 3 "unemployed" 4 "retired" 5 "disabled" 6 "housewife" 7 "student" 8 "other" // since I restricted time, this is fine
// wife not asked until 1976?
// tabstat EMPLOYMENT_INDV_, by(survey_yr) // asked whole time. need to figure out if asked of head and wife

* First, try to make one comprehensive detailed employment status and clean up existing variables
foreach var in EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_{
	recode `var'(0=.)
}

egen num_emp_status_head=rownonmiss(EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ )
browse unique_id survey_yr  num_emp_status_head EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_
tab EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ if num_emp_status_head ==2
 
gen employment_status_head = . // okay just going to call this primary employment status actaully
replace employment_status_head = EMPLOY_STATUS_HEAD_ if inrange(survey_yr,1985,1993)
replace employment_status_head = EMPLOY_STATUS1_HEAD_ if inrange(survey_yr,1994,2023) //  & inlist(num_emp_status_head,0,1)
recode employment_status_head (22/99=.)

gen employment_status_wife = . // okay just going to call this primary employment status actaully
replace employment_status_wife = EMPLOY_STATUS_WIFE_ if inrange(survey_yr,1985,1993)
replace employment_status_wife = EMPLOY_STATUS1_WIFE_ if inrange(survey_yr,1994,2023) //  & inlist(num_emp_status_head,0,1)
recode employment_status_wife (9/99=.)

label define employment_status 1 "working now" 2 "temp laid off" 3 "unemployed" 4 "retired" 5 "disabled" 6 "housewife" 7 "student" 8 "other"
label values employment_status_head employment_status_wife employment_status

replace EMPLOYMENT_INDV_ = . if EMPLOYMENT_INDV_==9

* Then just binary y/n employment (though, not really using this)
gen employ_head=.
replace employ_head=0 if inrange(EMPLOY_STATUS_HEAD_,2,9)
replace employ_head=1 if EMPLOY_STATUS_HEAD_==1
gen employ1_head=.
replace employ1_head=0 if inrange(EMPLOY_STATUS1_HEAD_,2,8)
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=.
replace employ2_head=0 if EMPLOY_STATUS2_HEAD_==0 | inrange(EMPLOY_STATUS2_HEAD_,2,8)
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=.
replace employ3_head=0 if EMPLOY_STATUS3_HEAD_==0 | inrange(EMPLOY_STATUS3_HEAD_,2,8)
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1

browse employ_head employ1_head employ2_head employ3_head
egen employed_head=rowtotal(employ_head employ1_head employ2_head employ3_head), missing
replace employed_head=1 if employed_head==2

gen employ_wife=.
replace employ_wife=0 if inrange(EMPLOY_STATUS_WIFE_,2,9)
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=.
replace employ1_wife=0 if inrange(EMPLOY_STATUS1_WIFE_,2,8)
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=.
replace employ2_wife=0 if EMPLOY_STATUS2_WIFE_==0 | inrange(EMPLOY_STATUS2_WIFE_,2,8)
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=.
replace employ3_wife=0 if EMPLOY_STATUS3_WIFE_==0 | inrange(EMPLOY_STATUS3_WIFE_,2,8)
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1

egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife), missing
replace employed_wife=1 if employed_wife==2

browse unique_id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

browse unique_id survey_yr  EMPLOYMENT_INDV
gen employed_indv=.
replace employed_indv=0 if inrange(EMPLOYMENT_INDV,2,9)
replace employed_indv=1 if EMPLOYMENT_INDV==1

// t-1 employment (need to create based on earnings)
gen employed_t1_head=.
replace employed_t1_head=0 if earnings_t1_head == 0
replace employed_t1_head=1 if earnings_t1_head > 0 & earnings_t1_head!=.

gen employed_t1_wife=.
replace employed_t1_wife=0 if earnings_t1_wife == 0
replace employed_t1_wife=1 if earnings_t1_wife > 0 & earnings_t1_wife!=.

gen employed_t1_indv=.
replace employed_t1_indv=0 if LABOR_INCOME_T1_INDV == 0
replace employed_t1_indv=1 if LABOR_INCOME_T1_INDV > 0 & LABOR_INCOME_T1_INDV!=.

gen ft_pt_t1_head=.
replace ft_pt_t1_head = 0 if weekly_hrs_t1_head==0
replace ft_pt_t1_head = 1 if weekly_hrs_t1_head > 0 & weekly_hrs_t1_head<=35
replace ft_pt_t1_head = 2 if weekly_hrs_t1_head > 35 & weekly_hrs_t1_head < 999

gen ft_pt_t1_wife=.
replace ft_pt_t1_wife = 0 if weekly_hrs_t1_wife==0
replace ft_pt_t1_wife = 1 if weekly_hrs_t1_wife > 0 & weekly_hrs_t1_wife<=35
replace ft_pt_t1_wife = 2 if weekly_hrs_t1_wife > 35 & weekly_hrs_t1_wife < 999

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_t1_head ft_pt_t1_wife ft_pt

gen ft_t1_head=0
replace ft_t1_head=1 if ft_pt_t1_head==2
replace ft_t1_head=. if ft_pt_t1_head==.

gen ft_t1_wife=0
replace ft_t1_wife=1 if ft_pt_t1_wife==2
replace ft_t1_wife=. if ft_pt_t1_wife==.

// housework hours - not totally sure if accurate prior to 1976 (asked annually not weekly - and was t-1. missing head/wife specific in 1968, 1975, 1982
browse unique_id survey_yr HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_ TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1 // total and most HW stopped after 1974, inividual stopped 1986

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
replace housework_head=. if inlist(housework_head,998,999)
gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)
replace housework_wife=. if inlist(housework_wife,998,999)
gen total_housework_weekly = TOTAL_HOUSEWORK_T1_HW / 52

replace CHILDCARE_HEAD = . if inlist(CHILDCARE_HEAD,998,999)
replace CHILDCARE_WIFE = . if inlist(CHILDCARE_WIFE,998,999)
replace ADULTCARE_HEAD = . if inlist(ADULTCARE_HEAD,998,999)
replace ADULTCARE_WIFE = . if inlist(ADULTCARE_WIFE,998,999)

// Education recode
* add SHELF variables for comparison
merge m:1 unique_id survey_yr using "$PSID/datagen_shelf_03_education_long.dta", keepusing(*match)
drop if _merge==2
drop _merge

browse unique_id survey_yr relationship_ SEX  EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_ YRS_EDUCATION_INDV COLLEGE_WIFE_ COLLEGE_HEAD_ COLLEGE_INDV_ BACHELOR_YR_WIFE_ BACHELOR_YR_HEAD_ BACHELOR_YR_INDV_  ENROLLED_WIFE_ ENROLLED_HEAD_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ // can also use yrs education but this is individual not HH, so need to match to appropriate person
foreach var in EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_ YRS_EDUCATION_INDV COLLEGE_WIFE_ COLLEGE_HEAD_ COLLEGE_INDV_ ENROLLED_WIFE_ ENROLLED_HEAD_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ BACHELOR_YR_WIFE_ BACHELOR_YR_HEAD_ BACHELOR_YR_INDV_{
	tabstat `var', by(survey_yr) // want to see which variables asked when	
}

foreach var in YRS_EDUCATION_INDV COLLEGE_INDV_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ BACHELOR_YR_INDV_{
	tabstat `var', by(relationship_) // are they asked for all?
}


/*
educ1 until 1990, but educ started 1975, okay but then a gap until 1991? wife not asked 1969-1971 - might be able to fill in if she is in sample either 1968 or 1972? (match to the id). also look at yrs education (missing 69 and 74?)

codes are also different between the two, use educ1 until 1990, then educ 1991 post
early educ:
0. cannot read
1. 0-5th grade
2. 6-8th grade
3. 9-11 grade
4/5. 12 grade
6. college no degree
7/8. college / advanced degree
9. dk

later educ: years of education
*/

* clean up intermediary variables
label values YRS_EDUCATION_INDV .

gen hs_head=.
replace hs_head=1 if inlist(HS_GRAD_HEAD_,1,2)
replace hs_head=0 if HS_GRAD_HEAD_==3

gen hs_wife=.
replace hs_wife=1 if inlist(HS_GRAD_WIFE_,1,2)
replace hs_wife=0 if HS_GRAD_WIFE_==3

gen attended_college_head=.
replace attended_college_head= 0 if ATTENDED_COLLEGE_HEAD_==5
replace attended_college_head= 1 if ATTENDED_COLLEGE_HEAD_==1

gen attended_college_wife=.
replace attended_college_wife= 0 if ATTENDED_COLLEGE_WIFE_==5
replace attended_college_wife= 1 if ATTENDED_COLLEGE_WIFE_==1

gen completed_college_head=.
replace completed_college_head= 0 if COLLEGE_HEAD_==5
replace completed_college_head= 1 if COLLEGE_HEAD_==1
replace completed_college_head= 0 if attended_college_head==0

gen completed_college_wife=.
replace completed_college_wife= 0 if COLLEGE_WIFE_==5
replace completed_college_wife= 1 if COLLEGE_WIFE_==1
replace completed_college_wife= 0 if attended_college_wife==0

gen completed_college_indv=.
replace completed_college_indv= 0 if COLLEGE_INDV_==5
replace completed_college_indv= 1 if COLLEGE_INDV_==1

gen college_degree_head=.
replace college_degree_head=0 if HIGHEST_DEGREE_HEAD_==0
replace college_degree_head=1 if HIGHEST_DEGREE_HEAD_==1 // associates
replace college_degree_head=2 if inrange(HIGHEST_DEGREE_HEAD_,2,6) // bachelor's plus

gen college_degree_wife=.
replace college_degree_wife=0 if HIGHEST_DEGREE_WIFE_==0
replace college_degree_wife=1 if HIGHEST_DEGREE_WIFE_==1 // associates
replace college_degree_wife=2 if inrange(HIGHEST_DEGREE_WIFE_,2,6) // bachelor's plus

label define degree 0 "No Coll" 1 "Assoc" 2 "BA+"
label values college_degree_head college_degree_wife

tab attended_college_head completed_college_head, m
tab completed_college_head college_degree_head, m

replace NEW_HEAD_YEAR = 1900+NEW_HEAD_YEAR if NEW_HEAD_YEAR>0 & NEW_HEAD_YEAR<100
replace NEW_WIFE_YEAR = 1900+NEW_WIFE_YEAR if NEW_WIFE_YEAR>0 & NEW_WIFE_YEAR<100

recode EDUC1_WIFE_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=.), gen(educ_wife_early)
recode EDUC1_HEAD_ (0/3=1)(4/5=2)(6=3)(7/8=4)(9=.), gen(educ_head_early)
recode EDUC_WIFE_ (1/11=1) (12=2) (13/15=3) (16/17=4) (99=.)(0=.), gen(educ_wife_1975)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head_1975)
recode YRS_EDUCATION_INDV (1/11=1) (12=2) (13/15=3) (16/17=4) (98/99=.)(0=.), gen(educ_completed) // okay no, can't use this, because I guess it's not actually comparable? because head / wife ONLY recorded against those specific ones.

// label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ_completed educ

browse unique_id survey_yr relationship_ under18 YRS_EDUCATION_INDV educ_completed educ_head_early educ_head_1975 hs_head HS_GRAD_HEAD attended_college_head completed_college_head college_degree_head BACHELOR_YR_HEAD_ YR_EDUC_UPD_HEAD_ NEW_HEAD_ NEW_HEAD_YEAR if relationship_==1 // usign head right now to wrap my head around

* create final education variables
gen educ_head_est=.
replace educ_head_est=1 if hs_head==0
replace educ_head_est=2 if hs_head==1 & attended_college_head==0
replace educ_head_est=3 if hs_head==1 & attended_college_head==1 & completed_college_head==0
replace educ_head_est=3 if completed_college_head==1 & college_degree_head==1
replace educ_head_est=4 if completed_college_head==1 & college_degree_head==2

gen educ_head=.
replace educ_head=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head=educ_head_1975 if inrange(survey_yr,1991,2023)

tab educ_head educ_head_est, m
tab educ_completed educ_head_est if relationship_==1, m
tab educ_head educ_completed if educ_head_est==., m
replace educ_head_est = educ_completed if educ_head_est==. & educ_completed!=.
replace educ_head_est = educ_head if educ_head_est==. & educ_head!=.

browse unique_id survey_yr under18 educ_head educ_completed educ_head_est YRS_EDUCATION_INDV  hs_head attended_college_head completed_college_head college_degree_head if relationship_==1 

gen educ_wife_est=.
replace educ_wife_est=1 if hs_wife==0
replace educ_wife_est=2 if hs_wife==1 & attended_college_wife==0
replace educ_wife_est=3 if hs_wife==1 & attended_college_wife==1 & completed_college_wife==0
replace educ_wife_est=3 if completed_college_wife==1 & college_degree_wife==1
replace educ_wife_est=4 if completed_college_wife==1 & college_degree_wife==2

gen educ_wife=.
replace educ_wife=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife=educ_wife_1975 if inrange(survey_yr,1991,2023)
tab survey_yr educ_wife, m 

replace educ_wife_est = educ_completed if educ_wife_est==. & educ_completed!=.
replace educ_wife_est = educ_wife if educ_wife_est==. & educ_wife!=.

tab educ_wife educ_wife_est, m
tab educ_completed educ_wife_est if relationship_==2, m
tab educ_wife educ_completed if educ_wife_est==., m

label values educ_head educ_wife educ_head_est educ_wife_est educ

tab educ_head_est edulevelrp_match if relationship_==1, m // it's still the HS some college, but I 	trust mine more tbh
tab educ_completed edulevel_match, m

browse unique_id survey_yr relationship_ educ_completed edulevel_match edulevelmax_match YRS_EDUCATION_INDV_ educ_head_est edulevelrp_match edulevelmaxrp_match // educ_wife edulevelsp_match edulevelmaxsp_match college_*

* gen indicator of max education
bysort unique_id: egen max_educ_head = max(educ_head_est)
bysort unique_id: egen max_educ_wife = max(educ_wife_est)
bysort unique_id: egen max_educ_indv = max(educ_completed)
label values max_educ_head max_educ_wife max_educ_indv educ

tab max_educ_head edulevelmaxrp_match if relationship_==1, m // these are npt v. congruent. is this bc the head is inconsistent? or bc of the recoding?
tab max_educ_indv edulevelmax_match, m // these closer

	/* trying to fill in missing wife years when possible - don't do this for now, because might not be same wife in question the whole time
	browse unique_id survey_yr educ_wife
	bysort unique_id (educ_wife): replace educ_wife=educ_wife[1] if educ_wife==.
	replace educ_wife=. if relationship_==0
	// can I also use years of education? okay no.
	*/
	
sort unique_id survey_yr

gen college_wife=.
replace college_wife=0 if inrange(educ_wife_est,1,3)
replace college_wife=1 if educ_wife_est==4

gen college_head=.
replace college_head=0 if inrange(educ_head_est,1,3)
replace college_head=1 if educ_head_est==4
tab college_degree_head college_head, m

gen college_indv=.
replace college_indv=0 if inrange(educ_completed,1,3)
replace college_indv=1 if educ_completed==4

// number of children
gen children=.
replace children=0 if NUM_CHILDREN_==0
replace children=1 if NUM_CHILDREN_>=1 & NUM_CHILDREN_!=.

// race
browse unique_id survey_yr RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_
// wait race of wife not asked until 1985?! that's wild. also need to see if codes changed in between. try to fill in historical for wife if in survey in 1985 and prior.
/*
1968-1984: 1=White; 2=Negro; 3=PR or Mexican; 7=Other
1985-1989: 1=White; 2=Black; 3=Am Indian 4=Asian 7=Other; 8 =more than 2
1990-2003: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Latino; 6=Other; 7=Other
2005-2019: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Native Hawaiian/Pac Is; 7=Other
*/

gen race_1_head_rec=.
replace race_1_head_rec=1 if RACE_1_HEAD_==1
replace race_1_head_rec=2 if RACE_1_HEAD_==2
replace race_1_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==3)
replace race_1_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==4)
replace race_1_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==5)
replace race_1_head_rec=6 if RACE_1_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_1_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_1_HEAD_==8)

gen race_2_head_rec=.
replace race_2_head_rec=1 if RACE_2_HEAD_==1
replace race_2_head_rec=2 if RACE_2_HEAD_==2
replace race_2_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==3)
replace race_2_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==4)
replace race_2_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==5)
replace race_2_head_rec=6 if RACE_2_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_2_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_2_HEAD_==8)

gen race_3_head_rec=.
replace race_3_head_rec=1 if RACE_3_HEAD_==1
replace race_3_head_rec=2 if RACE_3_HEAD_==2
replace race_3_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==3)
replace race_3_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==4)
replace race_3_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==5)
replace race_3_head_rec=6 if RACE_3_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_3_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_3_HEAD_==8)

gen race_4_head_rec=.
replace race_4_head_rec=1 if RACE_4_HEAD_==1
replace race_4_head_rec=2 if RACE_4_HEAD_==2
replace race_4_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==3)
replace race_4_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==4)
replace race_4_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==5)
replace race_4_head_rec=6 if RACE_4_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_4_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_4_HEAD_==8)

gen race_1_wife_rec=.
replace race_1_wife_rec=1 if RACE_1_WIFE_==1
replace race_1_wife_rec=2 if RACE_1_WIFE_==2
replace race_1_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==3)
replace race_1_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==4)
replace race_1_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==5)
replace race_1_wife_rec=6 if RACE_1_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_1_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_1_WIFE_==8)

gen race_2_wife_rec=.
replace race_2_wife_rec=1 if RACE_2_WIFE_==1
replace race_2_wife_rec=2 if RACE_2_WIFE_==2
replace race_2_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==3)
replace race_2_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==4)
replace race_2_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==5)
replace race_2_wife_rec=6 if RACE_2_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_2_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_2_WIFE_==8)

gen race_3_wife_rec=.
replace race_3_wife_rec=1 if RACE_3_WIFE_==1
replace race_3_wife_rec=2 if RACE_3_WIFE_==2
replace race_3_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==3)
replace race_3_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==4)
replace race_3_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==5)
replace race_3_wife_rec=6 if RACE_3_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_3_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_3_WIFE_==8)

gen race_4_wife_rec=.
replace race_4_wife_rec=1 if RACE_4_WIFE_==1
replace race_4_wife_rec=2 if RACE_4_WIFE_==2
replace race_4_wife_rec=3 if (inrange(survey_yr,1985,2023) & RACE_4_WIFE_==3)
replace race_4_wife_rec=4 if (inrange(survey_yr,1985,2023) & RACE_4_WIFE_==4)
replace race_4_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==5)
replace race_4_wife_rec=6 if RACE_4_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==6) | (inrange(survey_yr,2005,2023) & RACE_4_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_4_WIFE_==8)

browse unique_id race_1_head_rec race_2_head_rec race_3_head_rec race_4_head_rec

// based on first mention (that is one option they use in SHELF)
gen race_wife=race_1_wife_rec
replace race_wife=7 if race_2_wife_rec!=.

gen race_head=race_1_head_rec
replace race_head=7 if race_2_head_rec!=.

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Latino" 6 "Other" 7 "Multi-racial"
label values race_wife race_head race

// ethnicity
gen hispanic_head=.
replace hispanic_head=0 if HISPANICITY_HEAD_==0
replace hispanic_head=1 if inrange(HISPANICITY_HEAD_,1,7)

gen hispanic_wife=.
replace hispanic_wife=0 if HISPANICITY_WIFE_==0
replace hispanic_wife=1 if inrange(HISPANICITY_WIFE_,1,7)

tab race_head hispanic_head, m

// combined
gen raceth_head=.
replace raceth_head=1 if race_head==1 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=2 if race_head==2
replace raceth_head=3 if hispanic_head==1 & race_head!=2 // hispanic, non-black
replace raceth_head=3 if race_head==5 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=4 if race_head==4 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=5 if inlist(race_head,3,6,7) & (hispanic_head==0 | hispanic_head==.)

tab raceth_head, m
tab race_head raceth_head, m

gen raceth_wife=.
replace raceth_wife=1 if race_wife==1 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=2 if race_wife==2
replace raceth_wife=3 if hispanic_wife==1 & race_wife!=2 // hispanic, non-black
replace raceth_wife=3 if race_wife==5 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=4 if race_wife==4 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=5 if inlist(race_wife,3,6,7) & (hispanic_wife==0 | hispanic_wife==.)

label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
labe values raceth_head raceth_wife raceth

// figure out how to make time invariant, re: SHELF
tab raceth_head in_sample_, m
tab raceth_wife in_sample_, m
browse unique_id survey_yr raceth_head raceth_wife

bysort unique_id: egen raceth_head_fixed = median(raceth_head) // majority
tab raceth_head_fixed, m
gen last_race_head=raceth_head if survey_yr==last_yr_observed // tie break with last reported
bysort unique_id (last_race_head): replace last_race_head = last_race_head[1]
sort unique_id survey_yr
browse unique_id survey_yr last_yr_observed raceth_head raceth_head_fixed last_race_head
replace raceth_head_fixed=last_race_head if inlist(raceth_head_fixed,1.5,2.5,3.5,4.5)
replace raceth_head_fixed=last_race_head if raceth_head_fixed==.

bysort unique_id: egen raceth_wife_fixed = median(raceth_wife) // majority
tab raceth_wife_fixed, m
gen last_race_wife=raceth_wife if survey_yr==last_yr_observed // tie break with last reported
bysort unique_id (last_race_wife): replace last_race_wife = last_race_wife[1]
sort unique_id survey_yr
browse unique_id survey_yr last_yr_observed raceth_wife raceth_wife_fixed last_race_wife
replace raceth_wife_fixed=last_race_wife if inlist(raceth_wife_fixed,1.5,2.5,3.5,4.5)
replace raceth_wife_fixed=last_race_wife if raceth_wife_fixed==.

// home ownership
replace HOUSE_STATUS = . if HOUSE_STATUS==9

// region
recode REGION_ (0=.) (9=.)

// religion
tabstat RELIGION_WIFE_ RELIGION_HEAD_, by(survey_yr) // just to get a sense of when asked to start.
label values RELIGION_WIFE_ RELIGION_HEAD_ . // these values are v wrong
/* head was 1970-1977, 1979-2023. wife was 1976, 1985-2023, but not nec every year (carried through in some cases)
The codes changed wildly over the years?
1970-1984 - 0: No or Other, 1: Baptist, 2: Methodist, 3: Episcopalian, 4: Presbyterian, 5: Lutheran, 6: Unitarian, Mormon, and related, 7: Other Protestant, 8: Catholic, 9: Jewish
1985-1987 - 0: None, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 99: NA/DK
-- in 1987, the label specifically says None, atheist, agnostic
1988-1993 - 0: None, atheist, agnostic, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 99: NA/DK
-- so, up to 20 is the same as above, just added 21-25.
1994-2017 - 0: None, 1: Catholic, 2: Jewish, 8: Protestant unspecified, 10: Other non-Christian, 13: Greek Orthodox, 97: Other, 98: DK, 99: NA // so these large categories do match above in terms of coding (like 8 is the same, 13, etc. just way less groups)
-- In 1994, DENOMINATION was added as a separate question, so all of the detail goes to a separate question (which I don't believe I pulled in at the moment). so, I guess decide if that is worth adding.
2019-2023 - 0: Inapp (no partner), 1: None, 2: Atheist, 3: Agnostic, 4: Roman Catholic, 5: Greek Orthodox, 6: Baptist, 7: Episcopalian, 8: Jehovah's Witness, 9: Lutheran, 10: Methodist, 11: Pentecostal, 12: Presbyterian, 13: Protestant unspecified, 14: Christian, unspecified, 15: Christian, non-denominational, 16: Jewish, 17: Muslim, 18: Buddhist, 19: Other non-christian, 20: Other protestant, 21: LDS, 22: Unitarian, 23: Christian Science, 24: Adventist, 25: Amish, 26: Quaker, 27: Church of God, 28: United Church of Christ, 29: Reformed, 30: Disciples of Christ, 31: Churches of Christ, 97: Other, 98: DK, 99: NA
-- lol so DENOMINATION ends in 2017 and is integrated BACK to this question lord and the codes change AGAIN.

Denomination
1994-2017 - 0: None, atheist, agnostic, not Protestant OR no spouse (this is a lot in one), 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 11: LDS, 12: Jehovah's witness, 14: Christian, 15: Unitarian, 16: Christian Science, 17: Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 98: DK, 99: NA
-- so, I think aligns with how asked 1985-1993. I think if I combine the two I actually get all the same codes 0-25 (that's why some are missing)
*/
tab DENOMINATION_HEAD_ RELIGION_HEAD_ if inrange(survey_yr,1994,2017), m col // want to clarify how these map on so I can decide what catgories to use. so all of these are protestant denominations??

browse unique_id survey_yr RELIGION_HEAD_ DENOMINATION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_

gen religion_head=.
replace religion_head=0 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 0 // no religion
replace religion_head=0 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 0 // no religion
replace religion_head=0 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 1 // no religion
replace religion_head=1 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 2 // atheist
replace religion_head=2 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 3 // agnostic
replace religion_head=3 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 1 // catholic
replace religion_head=3 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 1 // catholic
replace religion_head=3 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 4 // catholic
replace religion_head=4 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 2 // jwish
replace religion_head=4 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 2 // jwish
replace religion_head=4 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 16 // jwish
replace religion_head=5 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 13 // greek orthodox
replace religion_head=5 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 13 // greek orthodox
replace religion_head=5 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 5 // greek orthodox
replace religion_head=6 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 3 // baptist
replace religion_head=6 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 3 // baptist
replace religion_head=6 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 6 // baptist
replace religion_head=7 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 7  // episco
replace religion_head=7 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 7 // episco
replace religion_head=7 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 7  // episco
replace religion_head=8 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 12 // jehovah
replace religion_head=8 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 12 // jehovah
replace religion_head=8 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 8 // jehovah
replace religion_head=9 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 4  // lutheran
replace religion_head=9 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 4 // lutheran
replace religion_head=9 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 9 // lutheran
replace religion_head=10 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 5  // methodist
replace religion_head=10 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 5 // methodist
replace religion_head=10 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 10 // methodist
replace religion_head=11 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 18 // pentecostal 
replace religion_head=11 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 18 // pentecostal
replace religion_head=11 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 11 // pentecostal 
replace religion_head=12 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 6  // presby
replace religion_head=12 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 6 // presby
replace religion_head=12 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 12  // presby
replace religion_head=13 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 8  // protestant un
replace religion_head=13 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 8 // protestant un
replace religion_head=13 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 13 // protestant un
replace religion_head=14 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 9 // other prot
replace religion_head=14 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & inlist(DENOMINATION_HEAD_,9,97,98,99) // other prot
replace religion_head=14 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 20 // other prot
replace religion_head=15 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD,10,14)  // other christian
replace religion_head=15 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 10  // other christian
replace religion_head=15 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 14  // other christian
replace religion_head=15 if inrange(survey_yr,2019,2023) & inlist(RELIGION_HEAD,14,15)  // other christian
replace religion_head=16 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 17 // muslim
replace religion_head=17 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 18 // buddhist
replace religion_head=18 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 19 // other non-christian
replace religion_head=19 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 11 // lds
replace religion_head=19 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 11 // lds
replace religion_head=19 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 21 // lds
replace religion_head=20 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 15 // unitarian
replace religion_head=20 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 15 // unitarian
replace religion_head=20 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 22 // unitarian
replace religion_head=21 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 16 // christian science
replace religion_head=21 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 16 // christian science
replace religion_head=21 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 23 // christian science
replace religion_head=22 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 17 // seventh day
replace religion_head=22 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 17 // seventh day
replace religion_head=22 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 24 // seventh day
replace religion_head=23 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 19 // amish
replace religion_head=23 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 19 // amish
replace religion_head=23 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 25 // amish
replace religion_head=24 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 20 // quaker
replace religion_head=24 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 20 // quaker
replace religion_head=24 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 26 // quaker
replace religion_head=25 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 21 // church of god
replace religion_head=25 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 21 // church of god
replace religion_head=25 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 27 // church of god
replace religion_head=26 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 22 // united church of christ
replace religion_head=26 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 22 // united church of christ
replace religion_head=26 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 28 // united church of christ
replace religion_head=27 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 23 // reformed
replace religion_head=27 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 23 // reformed
replace religion_head=27 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 29 // reformed
replace religion_head=28 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 24 // disciples 
replace religion_head=28 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 24 // disciples 
replace religion_head=28 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 30 // disciples 
replace religion_head=29 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 25 // churches
replace religion_head=29 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 8 & DENOMINATION_HEAD_== 25 // churches
replace religion_head=29 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 31 // churches
replace religion_head=30 if inrange(survey_yr,1985,1993) & RELIGION_HEAD== 97 // other other
replace religion_head=30 if inrange(survey_yr,1994,2017) & RELIGION_HEAD== 97 // other other
replace religion_head=30 if inrange(survey_yr,2019,2023) & RELIGION_HEAD== 97 // other other

replace religion_head=. if inrange(survey_yr,1985,1993) & inrange(RELIGION_HEAD,98,99) // dk / na
replace religion_head=. if inrange(survey_yr,1994,2017) & inrange(RELIGION_HEAD,98,99) // dk / na
replace religion_head=. if inrange(survey_yr,2019,2023) & RELIGION_HEAD==0
replace religion_head=. if inrange(survey_yr,2019,2023) & inrange(RELIGION_HEAD,98,99) // dk / na

gen religion_wife=.
replace religion_wife=0 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 0 // no religion
replace religion_wife=0 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 0 // no religion
replace religion_wife=0 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 1 // no religion
replace religion_wife=1 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 2 // atheist
replace religion_wife=2 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 3 // agnostic
replace religion_wife=3 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 1 // catholic
replace religion_wife=3 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 1 // catholic
replace religion_wife=3 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 4 // catholic
replace religion_wife=4 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 2 // jwish
replace religion_wife=4 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 2 // jwish
replace religion_wife=4 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 16 // jwish
replace religion_wife=5 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 13 // greek orthodox
replace religion_wife=5 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 13 // greek orthodox
replace religion_wife=5 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 5 // greek orthodox
replace religion_wife=6 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 3 // baptist
replace religion_wife=6 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 3 // baptist
replace religion_wife=6 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 6 // baptist
replace religion_wife=7 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 7  // episco
replace religion_wife=7 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 7 // episco
replace religion_wife=7 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 7  // episco
replace religion_wife=8 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 12 // jehovah
replace religion_wife=8 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 12 // jehovah
replace religion_wife=8 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 8 // jehovah
replace religion_wife=9 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 4  // lutheran
replace religion_wife=9 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 4 // lutheran
replace religion_wife=9 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 9 // lutheran
replace religion_wife=10 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 5  // methodist
replace religion_wife=10 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 5 // methodist
replace religion_wife=10 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 10 // methodist
replace religion_wife=11 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 18 // pentecostal 
replace religion_wife=11 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 18 // pentecostal
replace religion_wife=11 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 11 // pentecostal 
replace religion_wife=12 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 6  // presby
replace religion_wife=12 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 6 // presby
replace religion_wife=12 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 12  // presby
replace religion_wife=13 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 8  // protestant un
replace religion_wife=13 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 8 // protestant un
replace religion_wife=13 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 13 // protestant un
replace religion_wife=14 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 9 // other prot
replace religion_wife=14 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & inlist(DENOMINATION_WIFE_,9,97,98,99) // other prot
replace religion_wife=14 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 20 // other prot
replace religion_wife=15 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE,10,14)  // other christian
replace religion_wife=15 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 10  // other christian
replace religion_wife=15 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 14  // other christian
replace religion_wife=15 if inrange(survey_yr,2019,2023) & inlist(RELIGION_WIFE,14,15)  // other christian
replace religion_wife=16 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 17 // muslim
replace religion_wife=17 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 18 // buddhist
replace religion_wife=18 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 19 // other non-christian
replace religion_wife=19 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 11 // lds
replace religion_wife=19 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 11 // lds
replace religion_wife=19 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 21 // lds
replace religion_wife=20 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 15 // unitarian
replace religion_wife=20 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 15 // unitarian
replace religion_wife=20 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 22 // unitarian
replace religion_wife=21 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 16 // christian science
replace religion_wife=21 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 16 // christian science
replace religion_wife=21 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 23 // christian science
replace religion_wife=22 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 17 // seventh day
replace religion_wife=22 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 17 // seventh day
replace religion_wife=22 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 24 // seventh day
replace religion_wife=23 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 19 // amish
replace religion_wife=23 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 19 // amish
replace religion_wife=23 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 25 // amish
replace religion_wife=24 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 20 // quaker
replace religion_wife=24 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 20 // quaker
replace religion_wife=24 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 26 // quaker
replace religion_wife=25 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 21 // church of god
replace religion_wife=25 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 21 // church of god
replace religion_wife=25 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 27 // church of god
replace religion_wife=26 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 22 // united church of christ
replace religion_wife=26 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 22 // united church of christ
replace religion_wife=26 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 28 // united church of christ
replace religion_wife=27 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 23 // reformed
replace religion_wife=27 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 23 // reformed
replace religion_wife=27 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 29 // reformed
replace religion_wife=28 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 24 // disciples 
replace religion_wife=28 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 24 // disciples 
replace religion_wife=28 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 30 // disciples 
replace religion_wife=29 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 25 // churches
replace religion_wife=29 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 8 & DENOMINATION_WIFE_== 25 // churches
replace religion_wife=29 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 31 // churches
replace religion_wife=30 if inrange(survey_yr,1985,1993) & RELIGION_WIFE== 97 // other other
replace religion_wife=30 if inrange(survey_yr,1994,2017) & RELIGION_WIFE== 97 // other other
replace religion_wife=30 if inrange(survey_yr,2019,2023) & RELIGION_WIFE== 97 // other other

replace religion_wife=. if inrange(survey_yr,1985,1993) & inrange(RELIGION_WIFE,98,99) // dk / na
replace religion_wife=. if inrange(survey_yr,1994,2017) & inrange(RELIGION_WIFE,98,99) // dk / na
replace religion_wife=. if inrange(survey_yr,2019,2023) & RELIGION_WIFE==0
replace religion_wife=. if inrange(survey_yr,2019,2023) & inrange(RELIGION_WIFE,98,99) // dk / na

label define religion 0 "No religion" 1 "Atheist" 2 "Agnostic" 3 "Catholic" 4 "Jewish"  5 "Greek Orthodox" 6 "Baptist" 7 "Episcopalian" ///
8 "Jehovah's Witness" 9 "Lutheran"  10 "Methodist" 11 "Pentecostal" 12 "Presbyterian" 13 "Protestant unspecified" 14 "Other Protestant" ///
15 "Other Christian" 16 "Muslim"  17 "Buddhist" 18 "Other non-Christian" 19 "LDS" 20 "Unitarian" 21 "Christian Science" 22 "Seventh Day Adventist" ///
23 "Amish" 24 "Quaker" 25 "Church of God"  26 "United Church of Christ" 27 "Reformed" 28 "Disciples of Christ" 29 "Churches of Christ" 30 "Other Other"
label values religion_head religion_wife religion

tab religion_head, m
tab RELIGION_HEAD_ religion_head, m

tab religion_wife, m // one problem here is that this is also missing  if no wife, but think this will get fixed once I assign to focal
tab RELIGION_WIFE_ religion_wife, m 

// disability status - have those variables plus also can use employment status
	browse unique_id survey_yr EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
	// not numbered until 1994; 1-3 arose in 1994. codes match
	// wife not asked until 1976?

tab EMPLOY_STATUS_HEAD_, m
tab EMPLOY_STATUS1_HEAD_, m
tab EMPLOY_STATUS2_HEAD_, m
tab EMPLOY_STATUS2_HEAD_, m
tab DISABILITY_HEAD, m
tab DISABLE_HOWMUCH_HEAD, m

tab DISABILITY_HEAD DISABLE_HOWMUCH_HEAD, m

gen disabled_head=.
replace disabled_head=0 if inlist(DISABILITY_HEAD,0,5)
replace disabled_head=1 if DISABILITY_HEAD==1

gen disabled_scale_head=.
replace disabled_scale_head=0 if inlist(DISABILITY_HEAD,0,5) // not disabled
replace disabled_scale_head=0 if DISABILITY_HEAD==1 & DISABLE_HOWMUCH_HEAD==7 // not at all - so putting as 0 
replace disabled_scale_head=1 if DISABILITY_HEAD==1 & DISABLE_HOWMUCH_HEAD==5 // just a little
replace disabled_scale_head=2 if DISABILITY_HEAD==1 & DISABLE_HOWMUCH_HEAD==3 // somewhat
replace disabled_scale_head=3 if DISABILITY_HEAD==1 & DISABLE_HOWMUCH_HEAD==1 // a lot

gen disabled_wife=.
replace disabled_wife=0 if DISABILITY_WIFE==5
replace disabled_wife=1 if DISABILITY_WIFE==1

gen disabled_scale_wife=.
replace disabled_scale_wife=0 if DISABILITY_WIFE==5 // not disabled
replace disabled_scale_wife=0 if DISABILITY_WIFE==1 & DISABLE_HOWMUCH_WIFE==7 // not at all - so putting as 0 
replace disabled_scale_wife=1 if DISABILITY_WIFE==1 & DISABLE_HOWMUCH_WIFE==5 // just a little
replace disabled_scale_wife=2 if DISABILITY_WIFE==1 & DISABLE_HOWMUCH_WIFE==3 // somewhat
replace disabled_scale_wife=3 if DISABILITY_WIFE==1 & DISABLE_HOWMUCH_WIFE==1 // a lot

label define dis_scale 0 "Not at all" 1 "A little" 2 "Somewhat" 3 "A lot"
label values disabled_scale_head disabled_scale_wife dis_scale

// tab disabled_head disabled_scale_head, m
// tab disabled_wife disabled_scale_wife, m

gen empstat_disabled_head=0
replace empstat_disabled_head = 1 if EMPLOY_STATUS_HEAD_==5 | EMPLOY_STATUS1_HEAD_==5 | EMPLOY_STATUS2_HEAD_==5 | EMPLOY_STATUS3_HEAD_==5
replace empstat_disabled_head = . if EMPLOY_STATUS_HEAD_==. &  EMPLOY_STATUS1_HEAD_==. & EMPLOY_STATUS2_HEAD_==. & EMPLOY_STATUS3_HEAD_==.
tab empstat_disabled_head, m

gen empstat_disabled_wife=0
replace empstat_disabled_wife = 1 if EMPLOY_STATUS_WIFE_==5 | EMPLOY_STATUS1_WIFE_==5 | EMPLOY_STATUS2_WIFE_==5 | EMPLOY_STATUS3_WIFE_==5
replace empstat_disabled_wife = . if EMPLOY_STATUS_WIFE_==. &  EMPLOY_STATUS1_WIFE_==. & EMPLOY_STATUS2_WIFE_==. & EMPLOY_STATUS3_WIFE_==.
tab empstat_disabled_wife, m

gen empstat_disabled_indv=.
replace empstat_disabled_indv = 0 if inlist(EMPLOYMENT_INDV,1,2,3,4,6,7,8)
replace empstat_disabled_indv = 1 if EMPLOYMENT_INDV ==5
tab empstat_disabled_indv, m

		// 	browse unique_id survey_yr empstat_disabled_head EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ empstat_disabled_wife EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_

tab empstat_disabled_head disabled_head, m // is there overlap between the two ways of calculation? I guess the employment status disabled is that is why not working, but you can be employed and disabled...
// so the disabled own variable is more comprehensive. Think one thing is - does this explain lack of employment but another is that it could explain housework burden? (so both versions useful?)

// health status
tab SR_HEALTH_HEAD, m
recode SR_HEALTH_HEAD (0=.)(8/9=.)
tab SR_HEALTH_WIFE, m
recode SR_HEALTH_WIFE (0=.)(8/9=.)

tab SR_HEALTH_INDV, m // individual was coded same way as head / wife up until 1986, switched to 1 being in poor health and 5 being not in poor health in 1988 year. okay so just one year in that one
tab SR_HEALTH_OTHER, m // this is just yes / no. this might be a HH level variable? so copied to all hh members?
tab SR_HEALTH_OTHER SR_HEALTH_INDV, m  // what is diff?

browse unique_id survey_yr SR_HEALTH_OTHER SR_HEALTH_INDV

gen health_indv = .
replace health_indv = 5 if SR_HEALTH_INDV==1 & inrange(survey_yr,1988,2023) // will become poor
replace health_indv = 3 if SR_HEALTH_INDV==5 & inrange(survey_yr,1988,2023) // will become "good"
replace health_indv = SR_HEALTH_INDV if survey_yr==1986
replace health_indv = . if inlist(health_indv,0,8,9)

label define health 1 "Excellent" 2 "Very Good" 3 "Good" 4 "Fair" 5 "Poor"
label values health_indv health

tab health_indv, m
tab SR_HEALTH_INDV health_indv, m

// retirement status - same here
replace YR_RETIRED_HEAD = 1900 + YR_RETIRED_HEAD if YR_RETIRED_HEAD > 0 & YR_RETIRED_HEAD < 100
replace YR_RETIRED_WIFE = 1900 + YR_RETIRED_WIFE if YR_RETIRED_WIFE > 0 & YR_RETIRED_WIFE < 100

tab YR_RETIRED_HEAD, m
tab YR_RETIRED_WIFE, m

gen empstat_retired_head=0
replace empstat_retired_head = 1 if EMPLOY_STATUS_HEAD_==4 | EMPLOY_STATUS1_HEAD_==4 | EMPLOY_STATUS2_HEAD_==4 | EMPLOY_STATUS3_HEAD_==4
replace empstat_retired_head = . if EMPLOY_STATUS_HEAD_==. &  EMPLOY_STATUS1_HEAD_==. & EMPLOY_STATUS2_HEAD_==. & EMPLOY_STATUS3_HEAD_==.
tab empstat_retired_head, m

gen empstat_retired_wife=0
replace empstat_retired_wife = 1 if EMPLOY_STATUS_WIFE_==4 | EMPLOY_STATUS1_WIFE_==4 | EMPLOY_STATUS2_WIFE_==4 | EMPLOY_STATUS3_WIFE_==4
replace empstat_retired_wife = . if EMPLOY_STATUS_WIFE_==. &  EMPLOY_STATUS1_WIFE_==. & EMPLOY_STATUS2_WIFE_==. & EMPLOY_STATUS3_WIFE_==.
tab empstat_retired_wife, m

gen empstat_retired_indv=.
replace empstat_retired_indv = 0 if inlist(EMPLOYMENT_INDV,1,2,3,5,6,7,8)
replace empstat_retired_indv = 1 if EMPLOYMENT_INDV ==4
tab empstat_retired_indv, m

tab YR_RETIRED_HEAD empstat_retired_head, m
tab YR_RETIRED_WIFE empstat_retired_wife, m

// family background variables - okay think want to try to make these as fixed as possible (max education of parents) BUT can't do that until assign to focal since head / wife can change over time
browse unique_id survey_yr relationship_ FATHER_EDUC_HEAD MOTHER_EDUC_HEAD FATHER_EDUC_WIFE MOTHER_EDUC_WIFE FAMILY_STRUCTURE_HEAD FAMILY_STRUCTURE_WIFE LIVES_FAMILY_HEAD LIVES_FAMILY_WIFE FAMILY_AREA_HEAD FAMILY_AREA_WIFE

* parental education
fre FATHER_EDUC_HEAD MOTHER_EDUC_HEAD FATHER_EDUC_WIFE MOTHER_EDUC_WIFE 

foreach var in FATHER_EDUC_HEAD MOTHER_EDUC_HEAD FATHER_EDUC_WIFE MOTHER_EDUC_WIFE{
 	recode `var' (9/99=.)
 } 
 

* parental coresidence
gen family_structure_head = .
replace family_structure_head = 0 if FAMILY_STRUCTURE_HEAD==5
replace family_structure_head = 1 if FAMILY_STRUCTURE_HEAD==1

gen family_structure_wife = .
replace family_structure_wife = 0 if FAMILY_STRUCTURE_WIFE==5
replace family_structure_wife = 1 if FAMILY_STRUCTURE_WIFE==1

* residence
foreach var in LIVES_FAMILY_HEAD LIVES_FAMILY_WIFE FAMILY_AREA_HEAD FAMILY_AREA_WIFE RESPONDENT_WHO{
	recode `var' (9=.)
}

gen moved_in_lastyr = .
replace moved_in_lastyr = 0 if MOVED_LASTSPRING_HEAD_==5
replace moved_in_lastyr = 1 if MOVED_LASTSPRING_HEAD_==1

browse unique_id survey_yr moved_in_lastyr MOVED_SPRING_MO_HEAD MOVED_SPRING_YR_HEAD REGION_

gen moved_mo_lastyr = MOVED_SPRING_MO_HEAD // in some years, 21 = winter - 1 (Jan), 22 = spring - 4 (April), 23 = summer - 7 (July), 24 = fall - 10 (Oct)
replace moved_mo_lastyr = 1 if MOVED_SPRING_MO_HEAD == 21
replace moved_mo_lastyr = 4 if MOVED_SPRING_MO_HEAD == 22
replace moved_mo_lastyr = 7 if MOVED_SPRING_MO_HEAD == 23
replace moved_mo_lastyr = 10 if MOVED_SPRING_MO_HEAD == 24
replace moved_mo_lastyr = . if inlist(MOVED_SPRING_MO_HEAD,98,99)

fre MOVED_SPRING_YR_HEAD // from 1993-1997, 1 was last year and 2 was this year, 8/9 was na/dk//
// then 1999 switched to specific year (1 was 2 years ago, 2 was a year ago, 3 is current year so 1999, 1=97, 2=98, 3=99)
// then 2003 switched to coding actually reflecting year. 9996 was year unspecified but in range of last 2 years
browse unique_id survey_yr moved_in_lastyr MOVED_SPRING_YR_HEAD moved_mo_lastyr
tab MOVED_SPRING_YR_HEAD moved_in_lastyr, m

gen moved_yr_lastyr = .
replace moved_yr_lastyr = 0 if moved_in_lastyr==0

forvalues y=1993/1997{
	local x=`y'-1
	replace moved_yr_lastyr = `x' if survey_yr==`y' & MOVED_SPRING_YR_HEAD==1
	replace moved_yr_lastyr = `y' if survey_yr==`y' & MOVED_SPRING_YR_HEAD==2
}

forvalues y=1999/2001{
	local w=`y'-2
	local x=`y'-1
	capture replace moved_yr_lastyr = `w' if survey_yr==`y' & MOVED_SPRING_YR_HEAD==1
	capture replace moved_yr_lastyr = `x' if survey_yr==`y' & MOVED_SPRING_YR_HEAD==2
	capture replace moved_yr_lastyr = `y' if survey_yr==`y' & MOVED_SPRING_YR_HEAD==3
}

replace moved_yr_lastyr = MOVED_SPRING_YR_HEAD if inrange(survey_yr,2003,2023) & moved_in_lastyr==1 & inrange(MOVED_SPRING_YR_HEAD,2000,2025)
tab moved_yr_lastyr moved_in_lastyr, m

browse unique_id survey_yr moved_in_lastyr moved_yr_lastyr moved_mo_lastyr

forvalues y=1985/1992{
	local x=`y'-1
	replace moved_yr_lastyr = `x' if survey_yr==`y' & inrange(moved_mo_lastyr,4,12) // trying to estimate based on month. it's since last spring. So let's say April onward is last year, Jan - March is this year?
	replace moved_yr_lastyr = `y' if survey_yr==`y' & inrange(moved_mo_lastyr,1,3) // most interviews began Feb - March (in this time period). I could eventually add month (I did not do that at the moment)
}

* respondent
replace RESPONDENT_WHO = 0 if RESPONDENT_WHO==. & in_sample_==0
label define resp 0 "no sample" 1 "ref" 2 "spouse" 3 "partner" 4 "other hh member" 7 "proxy" 9 "non-survey year"
label values RESPONDENT_WHO resp

// okay, attempt to create indicators of partnership status using move in / out dates
label define sample 0 "not sample" 1 "original sample" 2 "born-in" 3 "moved in" 4 "joint inclusion" 5 "followable nonsample parent" 6 "nonsample elderly"
label values SAMPLE SAMPLE_sp sample
label values hh_status_sp_ hh_status

gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

gen has_psid_gene_sp=0
replace has_psid_gene_sp = 1 if inlist(SAMPLE_sp,1,2)

tab SAMPLE SAMPLE_sp, m
tab has_psid_gene has_psid_gene_sp

gen moved = 0
replace moved = 1 if inlist(MOVED_,1,2) & inlist(SPLITOFF_,1,3) // moved in
replace moved = 2 if inlist(MOVED_,1,2) & inlist(SPLITOFF_,2,4) // splitoff
replace moved = 3 if inlist(MOVED_,5,6) // moved out

gen moved_sp = 0
replace moved_sp = 1 if inlist(MOVED_sp_,1,2) & inlist(SPLITOFF_sp_,1,3) // moved in
replace moved_sp = 2 if inlist(MOVED_sp_,1,2) & inlist(SPLITOFF_sp_,2,4) // splitoff
replace moved_sp = 3 if inlist(MOVED_sp_,5,6) // moved out

label define moved 0 "no" 1 "Moved in" 2 "Splitoff" 3 "Moved out"
label values moved moved_sp moved

gen partnered=.
replace partnered=0 if in_sample_==1 & MARITAL_PAIRS_==0
replace partnered=1 if in_sample_==1 & inrange(MARITAL_PAIRS_,1,3)

gen partnered_sp=.
replace partnered_sp=0 if in_sample_sp_==1 & MARITAL_PAIRS_sp_==0
replace partnered_sp=1 if in_sample_sp_==1 & inrange(MARITAL_PAIRS_sp_,1,3)

browse unique_id partner_id survey_yr rel_start_all last_yr_observed min_dur max_dur has_psid_gene has_psid_gene_sp partnered partnered_sp hh_status_ hh_status_sp_  moved MOVED_YEAR_ SPLITOFF_YEAR_  moved_sp MOVED_YEAR_sp SPLITOFF_YEAR_sp_ COMPOSITION_CHANGE_

tab COMPOSITION_CHANGE_ if survey_yr == rel_start_all

********************************************************************************
**# now need to allocate variables to individual based on relationship
* so that we have FOCAL variables, not head / sex versions
********************************************************************************

* Let's start with t-1 variables
// weekly hours
browse unique_id survey_yr relationship_  weekly_hrs_t1_head weekly_hrs_t1_wife weekly_hrs_t1_indv
gen weekly_hrs_t1_focal=.
replace weekly_hrs_t1_focal=weekly_hrs_t1_head if relationship_==1
replace weekly_hrs_t1_focal=weekly_hrs_t1_wife if relationship_==2
replace weekly_hrs_t1_focal=weekly_hrs_t1_indv if relationship_==3

// annual earnings
browse unique_id survey_yr relationship_ earnings_t1_head earnings_t1_wife LABOR_INCOME_T1_INDV
gen earnings_t1_focal=.
replace earnings_t1_focal=earnings_t1_head if relationship_==1
replace earnings_t1_focal=earnings_t1_wife if relationship_==2
replace earnings_t1_focal=LABOR_INCOME_T1_INDV if relationship_==3

// previously created t1 employment - this was based on earnings
gen employed_t1_earn_focal=.
replace employed_t1_earn_focal=employed_t1_head if relationship_==1
replace employed_t1_earn_focal=employed_t1_wife if relationship_==2
replace employed_t1_earn_focal=employed_t1_indv if relationship_==3

// births - based on PSID variables NOT birth history. can add that later
browse unique_id survey_yr BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ BIRTHS_T1_OFUMS_ BIRTHS_T2_HEAD_ BIRTHS_T2_WIFE_ BIRTHS_T2_BOTH_ BIRTHS_T2_OFUMS_

gen any_births_t1_focal=.
replace any_births_t1_focal = 0 if relationship_==1 & BIRTHS_T1_HEAD_==0 & BIRTHS_T1_BOTH_==0
replace any_births_t1_focal = 1 if relationship_==1 & (inrange(BIRTHS_T1_HEAD_,1,3) |  inrange(BIRTHS_T1_BOTH_,1,3))
replace any_births_t1_focal = 0 if relationship_==2 & BIRTHS_T1_WIFE_==0 & BIRTHS_T1_BOTH_==0
replace any_births_t1_focal = 1 if relationship_==2 & (inrange(BIRTHS_T1_WIFE_,1,3) | inrange(BIRTHS_T1_BOTH_,1,3))

gen any_births_t1_hh=. // because, even if not head/  wife - if head / wife HAD a birth, that is technically a new kid in HH, even if that person didn't have the kid
replace any_births_t1_hh=0 if inlist(BIRTHS_T1_HEAD_,0,9) & inlist(BIRTHS_T1_WIFE_,0,9) & inlist(BIRTHS_T1_BOTH_,0,9) & inlist(BIRTHS_T1_OFUMS_,0,9)
replace any_births_t1_hh = 1 if inrange(BIRTHS_T1_WIFE_,1,3) | inrange(BIRTHS_T1_BOTH_,1,3) | inrange(BIRTHS_T1_WIFE_,1,3) | inrange(BIRTHS_T1_OFUMS_,1,3)

gen any_births_t2_focal=.
replace any_births_t2_focal = 0 if relationship_==1 & BIRTHS_T2_HEAD_==0 & BIRTHS_T2_BOTH_==0
replace any_births_t2_focal = 1 if relationship_==1 & (inrange(BIRTHS_T2_HEAD_,1,3) |  inrange(BIRTHS_T2_BOTH_,1,3))
replace any_births_t2_focal = 0 if relationship_==2 & BIRTHS_T2_WIFE_==0 & BIRTHS_T2_BOTH_==0
replace any_births_t2_focal = 1 if relationship_==2 & (inrange(BIRTHS_T2_WIFE_,1,3) | inrange(BIRTHS_T2_BOTH_,1,3))

gen any_births_t2_hh=. // because, even if not head/  wife - if head / wife HAD a birth, that is technically a new kid in HH, even if that person didn't have the kid
replace any_births_t2_hh=0 if inlist(BIRTHS_T2_HEAD_,0,9) & inlist(BIRTHS_T2_WIFE_,0,9) & inlist(BIRTHS_T2_BOTH_,0,9) & inlist(BIRTHS_T2_OFUMS_,0,9)
replace any_births_t2_hh = 1 if inrange(BIRTHS_T2_WIFE_,1,3) | inrange(BIRTHS_T2_BOTH_,1,3) | inrange(BIRTHS_T2_WIFE_,1,3) | inrange(BIRTHS_T2_OFUMS_,1,3)

// ever parent status
tab NUM_BIRTHS, m
tab FIRST_BIRTH_YR NUM_BIRTHS, m

gen ever_parent_focal = 0
replace ever_parent_focal = 1 if NUM_BIRTHS >=1 & NUM_BIRTHS<=20

gen num_births_focal = .
replace num_births_focal = 0 if NUM_BIRTHS==0
replace num_births_focal = NUM_BIRTHS if NUM_BIRTHS >=1 & NUM_BIRTHS<=20

* t variables
// weekly HW hours
browse unique_id survey_yr relationship_ housework_head housework_wife HOUSEWORK_INDV_
gen housework_focal=.
replace housework_focal=housework_head if relationship_==1
replace housework_focal=housework_wife if relationship_==2
replace housework_focal=HOUSEWORK_INDV_ if relationship_==3
// replace housework_focal=. if relationship_==3

// weekly childcare
gen childcare_focal=.
replace childcare_focal=CHILDCARE_HEAD if relationship_==1
replace childcare_focal=CHILDCARE_WIFE if relationship_==2

// weekly adultcare
gen adultcare_focal=.
replace adultcare_focal=ADULTCARE_HEAD if relationship_==1
replace adultcare_focal=ADULTCARE_WIFE if relationship_==2

// Current employment status: detailed
browse unique_id survey_yr relationship_ employment_status_head employment_status_wife EMPLOYMENT_INDV_
tab EMPLOYMENT_INDV_ employment_status_head if relationship_==1 // so they do match

gen employment_status_focal=.
replace employment_status_focal=employment_status_head if relationship_==1
replace employment_status_focal=employment_status_wife if relationship_==2
replace employment_status_focal=EMPLOYMENT_INDV_ if relationship_==3

label values employment_status_focal employment_status

// Current employment status: binary
browse unique_id survey_yr relationship_ employed_head employed_wife employed_indv
gen employed_focal=.
replace employed_focal=employed_head if relationship_==1
replace employed_focal=employed_wife if relationship_==2
replace employed_focal=employed_indv if relationship_==3

// Education
tab educ_head_est educ_completed if relationship_==1
tab educ_wife_est educ_completed if relationship_==2

gen educ_focal=.
replace educ_focal = educ_completed if educ_completed!=. // let's prioritize indiivdual levels of education because that is asked annually even when head / ref is not, and sometimes updated for head / ref
replace educ_focal=educ_head_est if relationship_==1 & educ_completed==. // then fill in otherwise here
replace educ_focal=educ_wife_est if relationship_==2 & educ_completed==.
replace educ_focal=educ_completed if relationship_==3

bysort unique_id: egen max_educ_focal = max(educ_focal)
label values educ_focal max_educ_focal educ

tab max_educ_focal edulevelmax_match, m // this is MUCH closer now, and it's really about the HS v. some college that I have differently...

browse unique_id survey_yr relationship_ educ_focal max_educ_focal educ_completed educ_head_est educ_wife_est NEW_HEAD_YEAR NEW_WIFE_YEAR YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_  // YRS_EDUCATION_INDV_ edulevel_match edulevelmax_match YRS_EDUCATION_INDV_ completed_college_indv educ_head edulevelrp_match edulevelmaxrp_match max_educ_head educ_wife edulevelsp_match edulevelmaxsp_match max_educ_wife completed_college_head completed_college_wife 

tab educ_focal in_sample_, m

// quietly unique educ_focal, by(unique_id) gen(educ_change)
// bysort unique_id (educ_change): replace educ_change=educ_change[1]
// sort unique_id partner_id survey_yr
// browse unique_id partner_id relationship_ survey_yr educ_focal educ_head educ_wife educ_completed educ_change

gen college_focal=.
replace college_focal = 0 if inrange(educ_focal,1,3)
replace college_focal = 1 if educ_focal==4

// Age
browse unique_id survey_yr relationship_ AGE_*
gen age_focal = AGE_INDV

// race
gen raceth_focal=.
replace raceth_focal=raceth_head if relationship_==1
replace raceth_focal=raceth_wife if relationship_==2

gen raceth_fixed_focal=.
replace raceth_fixed_focal=raceth_head_fixed if relationship_==1
replace raceth_fixed_focal=raceth_wife_fixed if relationship_==2
label values raceth_focal raceth_fixed_focal raceth

// house status
gen house_status_all = .
replace house_status_all = 1 if HOUSE_STATUS_ == 1
replace house_status_all = 2 if HOUSE_STATUS_ == 5
replace house_status_all = 3 if HOUSE_STATUS_ == 8

label define house 1 "Owns" 2 "Rents" 3 "Neither"
label values house_status_all house

// region of residence - think need to update this further once wide with missing years - use same region if did not move
browse unique_id survey_yr relationship_ REGION_ moved_in_lastyr moved_yr_lastyr // the problem with this is that we have no idea where they moved - and the likelihood of it being out of the region is quite low? because there are a shocking amount of heads who move in a given year - but not enough to span regions? (prob not even states) so do I just fill it in based on previous years where I have it and impute for the missing years when it changes? OR do I pull in the whether this year or last year - will that help? if say moved last year and last year is an off year?

// religion
gen religion_focal=.
replace religion_focal=religion_head if relationship_==1
replace religion_focal=religion_wife if relationship_==2
label values religion_focal religion

// family background variables
gen father_educ_focal=.
replace father_educ_focal=FATHER_EDUC_HEAD_ if relationship_==1
replace father_educ_focal=FATHER_EDUC_WIFE_ if relationship_==2

gen mother_educ_focal=.
replace mother_educ_focal=MOTHER_EDUC_HEAD_ if relationship_==1
replace mother_educ_focal=MOTHER_EDUC_WIFE_ if relationship_==2

label define parent_educ 0 "none" 1 "0-5 grades" 2 "6-8 grades" 3 "9-11 grades" 4 "high school" 5 "12+" 6 "some college" 7 "BA" 8 "advanced degree"
label values mother_educ_focal father_educ_focal parent_educ

bysort unique_id: egen father_max_educ_focal = max(father_educ_focal)
bysort unique_id: egen mother_max_educ_focal = max(mother_educ_focal)
label values father_max_educ_focal mother_max_educ_focal parent_educ

sort unique_id survey_yr
browse unique_id survey_yr relationship_ father_max_educ_focal father_educ_focal mother_max_educ_focal mother_educ_focal FATHER_EDUC_HEAD_ FATHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ MOTHER_EDUC_WIFE_

gen family_structure_focal=.
replace family_structure_focal=family_structure_head if relationship_==1
replace family_structure_focal=family_structure_wife if relationship_==2

browse unique_id survey_yr last_yr_observed relationship_ in_sample_ family_structure_focal family_structure_head family_structure_wife
bysort unique_id: egen family_structure_cons_focal = min(family_structure_focal) // use min so if ever say didn't live with parents, that is prioritized

gen lives_family_focal=.
replace lives_family_focal=LIVES_FAMILY_HEAD_ if relationship_==1
replace lives_family_focal=LIVES_FAMILY_WIFE_ if relationship_==2
replace lives_family_focal=. if lives_family_focal==0

label define lives_family 1 "same state" 2 "same region" 3 "diff region"
label values lives_family_focal lives_family

// disability status
browse unique_id survey_yr relationship_ empstat_disabled_head disabled_head disabled_scale_head empstat_disabled_wife disabled_wife disabled_scale_wife empstat_disabled_indv

gen disabled_focal=.
replace disabled_focal=disabled_head if relationship_==1
replace disabled_focal=disabled_wife if relationship_==2

gen empstat_disabled_focal=.
replace empstat_disabled_focal=empstat_disabled_head if relationship_==1
replace empstat_disabled_focal=empstat_disabled_wife if relationship_==2
replace empstat_disabled_focal=empstat_disabled_indv if relationship_==3

gen disabled_scale_focal=.
replace disabled_scale_focal=disabled_scale_head if relationship_==1
replace disabled_scale_focal=disabled_scale_wife if relationship_==2
label values disabled_scale_focal dis_scale

// self-rated health
gen sr_health_focal=.
replace sr_health_focal=SR_HEALTH_HEAD_ if relationship_==1
replace sr_health_focal=SR_HEALTH_WIFE_ if relationship_==2
replace sr_health_focal=health_indv if relationship_==3

label values sr_health_focal health
tab sr_health_focal, m

// retirement status
gen yr_retired_focal=.
replace yr_retired_focal=YR_RETIRED_HEAD if relationship_==1
replace yr_retired_focal=YR_RETIRED_WIFE if relationship_==2

gen empstat_retired_focal=.
replace empstat_retired_focal=empstat_retired_head if relationship_==1
replace empstat_retired_focal=empstat_retired_wife if relationship_==2
replace empstat_retired_focal=empstat_retired_indv if relationship_==3

tab yr_retired_focal empstat_retired_focal, m

** need to also merge these other variables I created (these are either t OR "ever")
// mpf info
merge m:1 unique_id using "$created_data/birth_history_wide.dta", keepusing(cah_any_births cah_num_birth_partners cah_any_mpf)
drop if _merge==2
drop _merge

tab NUM_BIRTHS cah_any_births, m // perfectly corresponds to "no birth history" collected also
tab cah_any_births cah_any_mpf, m // the missing are people with no births. so maybe make that 0 because I don't need to impute?

gen mpf_focal = 0
replace mpf_focal = 1 if cah_any_mpf == 1
tab mpf_focal, m

// coresidence with parents
merge m:1 unique_id survey_yr using "$temp/parent_coresidence_lookup.dta"
drop if _merge==2
tab in_sample_ _merge, m // non-matches are those not in sample
drop _merge

rename moved moved_focal // need to rename so I can do the below

merge m:1 father_unique_id survey_yr using "$temp/parent_details_tomatch.dta", keepusing(fam_id in_sample moved change_yr)
drop if _merge==2
inspect father_unique_id if _merge==1 // why is the match rate so bad? if this is truly coming frm the main file?
tab father_in_hh _merge // okay, so it does match when he is in HH
drop _merge

foreach var in fam_id in_sample moved change_yr{
	rename `var' father_`var'
}

merge m:1 mother_unique_id survey_yr using "$temp/parent_details_tomatch.dta", keepusing(fam_id in_sample moved change_yr)
drop if _merge==2
inspect mother_unique_id if _merge==1
tab mother_in_hh _merge
drop _merge

foreach var in fam_id in_sample moved change_yr{
	rename `var' mother_`var'
}

label values mother_moved father_moved moved

tab father_in_hh, m // do these feel too high? oh, it's because, even though I restrict to couples, right now, I am pulling in extra years, so they might be under age.
tab under18 father_in_hh, m row
tab mother_in_hh, m
tab father_in_hh mother_in_hh, m

gen num_parent_in_hh = .
replace num_parent_in_hh = 0 if father_in_hh==0 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==1 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==0 & mother_in_hh==1
replace num_parent_in_hh = 2 if father_in_hh==1 & mother_in_hh==1

tab OFUM1_REL_ num_parent_in_hh if under18==0, m

label define ofum_rel 0 "no other fam" 1 "ref's parents" 2 "ref's child" 3 "ref's grandparents" 4 "ref's grandchild" 5 "ref's sib" 7 "other"
label values OFUM*_REL_ ofum_rel

browse unique_id survey_yr under18 AGE_INDV num_parent_in_hh father_in_hh mother_in_hh OFUM*_REL_

	// going to see if these are best or if I should use these OFUMS descriptions
	gen parent_in_ofum = .
	replace parent_in_ofum = 0 if OFUM1_REL_==0 & OFUM2_REL_==0 & OFUM3_REL_==0 & OFUM4_REL_==0
	replace parent_in_ofum = 1 if OFUM1_REL_==1 | OFUM2_REL_==1 | OFUM3_REL_==1 | OFUM4_REL_==1
	
	tab parent_in_ofum num_parent_in_hh, m // this correspondence is terrible, but not sure they would consider parents a distinct family unit
	
	label values NUM_IN_HH_ .
	tab NUM_IN_HH partnered, m // do these counts make sense?
	tab NUM_IN_HH num_parent_in_hh if partnered==1, m row
	tab NUM_IN_HH parent_in_ofum if partnered==1, m row
	
	tab in_sample_ num_parent_in_hh, m row
	tab in_sample_ parent_in_ofum, m row // possible they are in HH but the individual is non-response? (which is why the above lookup won't match?) OR is it possible I have a flag for parent in HH but parent is non-sample?
	
gen father_check = 0
replace father_check = 1 if FAMILY_INTERVIEW_NUM_ == father_fam_id & FAMILY_INTERVIEW_NUM_!=.
tab father_check father_in_hh, m

gen mother_check = 0
replace mother_check = 1 if FAMILY_INTERVIEW_NUM_ == mother_fam_id & FAMILY_INTERVIEW_NUM_!=.
tab mother_check mother_in_hh, m

sort unique_id survey_yr
browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ under18 father_in_hh father_check father_in_sample father_fam_id mother_in_hh mother_check mother_in_sample mother_fam_id parent_in_ofum

	// also see if I can identify off years with move in / out / changes in family comp
	browse unique_id partner_id survey_yr father_in_hh father_in_sample father_moved father_change_yr mother_in_hh mother_in_sample mother_moved mother_change_yr COMPOSITION_CHANGE_
	
// hh composition
merge m:1 FAMILY_INTERVIEW_NUM_ survey_yr using "$temp/hh_comp_lookup.dta"
drop if _merge==2
tab in_sample_ _merge, m
drop _merge

* t-2 variables
// weekly hours
browse unique_id survey_yr relationship_ WEEKLY_HRS_T2_HEAD WEEKLY_HRS_T2_WIFE WEEKLY_HRS_T2_INDV 

gen weekly_hrs_t2_focal=.
replace weekly_hrs_t2_focal=WEEKLY_HRS_T2_INDV if inrange(survey_yr,1999,2001)
replace weekly_hrs_t2_focal=WEEKLY_HRS_T2_HEAD if relationship_==1 & inrange(survey_yr,2003,2023)
replace weekly_hrs_t2_focal=WEEKLY_HRS_T2_WIFE if relationship_==2 & inrange(survey_yr,2003,2023)
replace weekly_hrs_t2_focal=WEEKLY_HRS_T2_INDV if relationship_==3 & inrange(survey_yr,2003,2023)
browse unique_id survey_yr relationship_ weekly_hrs_t2_focal WEEKLY_HRS_T2_HEAD WEEKLY_HRS_T2_WIFE WEEKLY_HRS_T2_INDV

// annual earnings
browse unique_id survey_yr relationship_ LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ LABOR_INCOME_T2_INDV_  TOTAL_INCOME_T2_FAMILY_

gen long earnings_t2_focal=.
replace earnings_t2_focal=LABOR_INCOME_T2_INDV_ if inrange(survey_yr,1999,2001)
replace earnings_t2_focal=LABOR_INCOME_T2_HEAD_ if relationship_==1 & inrange(survey_yr,2003,2023)
replace earnings_t2_focal=LABOR_INCOME_T2_WIFE_ if relationship_==2 & inrange(survey_yr,2003,2023)
replace earnings_t2_focal=LABOR_INCOME_T2_INDV_ if relationship_==3 & inrange(survey_yr,2003,2023)
replace earnings_t2_focal=. if earnings_t2_focal==9999999 | earnings_t2_focal==99999999
browse unique_id survey_yr relationship_ earnings_t2_focal LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ LABOR_INCOME_T2_INDV_

// employment status
gen employed_t2_indv=.
replace employed_t2_indv=0 if WEEKS_WORKED_T2_INDV_==0
replace employed_t2_indv=1 if inrange(WEEKS_WORKED_T2_INDV_,1,52)

gen employed_t2_head=.
replace employed_t2_head=0 if EMPLOY_STATUS_T2_HEAD==5
replace employed_t2_head=1 if EMPLOY_STATUS_T2_HEAD==1

gen employed_t2_wife=.
replace employed_t2_wife=0 if EMPLOY_STATUS_T2_WIFE==5
replace employed_t2_wife=1 if EMPLOY_STATUS_T2_WIFE==1

browse unique_id survey_yr relationship_  employed_t2*

gen employed_t2_focal=.
replace employed_t2_focal = employed_t2_indv if inlist(survey_yr,1999,2001)
replace employed_t2_focal=employed_t2_head if relationship_==1 & inrange(survey_yr,2003,2023)
replace employed_t2_focal=employed_t2_wife if relationship_==2 & inrange(survey_yr,2003,2023)
replace employed_t2_focal=employed_t2_indv if relationship_==3 & inrange(survey_yr,2003,2023)
replace employed_t2_focal=1 if inrange(survey_yr,1999,2001) & WEEKLY_HRS_T2_INDV>0 & WEEKLY_HRS_T2_INDV<900
replace employed_t2_focal=0 if inrange(survey_yr,1999,2001) & WEEKLY_HRS_T2_INDV==0

browse unique_id survey_yr relationship_ employed_t2_focal employed_t2_head WEEKLY_HRS_T2_HEAD WEEKLY_HRS_T2_INDV // can I use hours to fill in the gaps?

* employment history to fill the gaps
sum START_YR_CURRENT_HEAD, detail
replace START_YR_CURRENT_HEAD=. if inrange(START_YR_CURRENT_HEAD,9000,9999)
replace START_YR_CURRENT_HEAD=. if START_YR_CURRENT_HEAD==0
tabstat START_YR_CURRENT_HEAD, by(survey_yr)
replace START_YR_CURRENT_HEAD=1900+START_YR_CURRENT_HEAD if START_YR_CURRENT_HEAD<100

sum START_YR_PREV_HEAD, detail
replace START_YR_PREV_HEAD=. if inrange(START_YR_PREV_HEAD,9000,9999)
replace START_YR_PREV_HEAD=. if START_YR_PREV_HEAD==0
tabstat START_YR_PREV_HEAD, by(survey_yr)
replace START_YR_PREV_HEAD=1900+START_YR_PREV_HEAD if START_YR_PREV_HEAD<100

sum START_YR_EMPLOYER_HEAD, detail
replace START_YR_EMPLOYER_HEAD=. if inrange(START_YR_EMPLOYER_HEAD,9000,9999)
replace START_YR_EMPLOYER_HEAD=. if START_YR_EMPLOYER_HEAD==0

gen start_yr_employer_head=.
replace start_yr_employer_head = START_YR_CURRENT_HEAD if inrange(survey_yr,1988,2001) & START_YR_CURRENT_HEAD!=.
replace start_yr_employer_head = START_YR_PREV_HEAD if inrange(survey_yr,1988,2001) & START_YR_PREV_HEAD!=.
replace start_yr_employer_head = START_YR_EMPLOYER_HEAD if inrange(survey_yr,2003,2023)

browse unique_id survey_yr relationship_ start_yr_employer_head START_YR_EMPLOYER_HEAD START_YR_CURRENT_HEAD START_YR_PREV_HEAD YRS_CURRENT_EMPLOY_HEAD

sum START_YR_CURRENT_WIFE, detail
replace START_YR_CURRENT_WIFE=. if inrange(START_YR_CURRENT_WIFE,9000,9999)
replace START_YR_CURRENT_WIFE=. if START_YR_CURRENT_WIFE==0
tabstat START_YR_CURRENT_WIFE, by(survey_yr)
replace START_YR_CURRENT_WIFE=1900+START_YR_CURRENT_WIFE if START_YR_CURRENT_WIFE<100

sum START_YR_PREV_WIFE, detail
replace START_YR_PREV_WIFE=. if inrange(START_YR_PREV_WIFE,9000,9999)
replace START_YR_PREV_WIFE=. if START_YR_PREV_WIFE==0
tabstat START_YR_PREV_WIFE, by(survey_yr)
replace START_YR_PREV_WIFE=1900+START_YR_PREV_WIFE if START_YR_PREV_WIFE<100

sum START_YR_EMPLOYER_WIFE, detail
replace START_YR_EMPLOYER_WIFE=. if inrange(START_YR_EMPLOYER_WIFE,9000,9999)
replace START_YR_EMPLOYER_WIFE=. if START_YR_EMPLOYER_WIFE==0

gen start_yr_employer_wife=.
replace start_yr_employer_wife = START_YR_CURRENT_WIFE if inrange(survey_yr,1988,2001) & START_YR_CURRENT_WIFE!=.
replace start_yr_employer_wife = START_YR_PREV_WIFE if inrange(survey_yr,1988,2001) & START_YR_PREV_WIFE!=.
replace start_yr_employer_wife = START_YR_EMPLOYER_WIFE if inrange(survey_yr,2003,2023)

gen start_yr_employer_focal = .
replace start_yr_employer_focal = start_yr_employer_head if relationship_==1 
replace start_yr_employer_focal = start_yr_employer_wife if relationship_==2

gen yrs_employer_focal = .
replace yrs_employer_focal=YRS_CURRENT_EMPLOY_HEAD if relationship_==1
replace yrs_employer_focal=YRS_CURRENT_EMPLOY_WIFE if relationship_==2

// trying to make an indicator of t-1 employment status
browse survey_yr earnings_t1_focal weekly_hrs_t1_focal earnings_t2_focal weekly_hrs_t2_focal employed_t2_focal

gen has_hours_t1=.
replace has_hours_t1=0 if weekly_hrs_t1_focal==0
replace has_hours_t1=1 if weekly_hrs_t1_focal>0 & weekly_hrs_t1_focal!=.

gen has_earnings_t1=.
replace has_earnings_t1=0 if earnings_t1_focal==0
replace has_earnings_t1=1 if earnings_t1_focal>0 & earnings_t1_focal!=.

tab has_hours_t1 has_earnings_t1, m // sometimes has hours but not earnings and vice versa, which is v. confusing. i guess hours is my primary interest, so use that

gen employed_t1_hrs_focal=.
replace employed_t1_hrs_focal=0 if weekly_hrs_t1_focal==0
replace employed_t1_hrs_focal=1 if weekly_hrs_t1_focal>0 & weekly_hrs_t1_focal!=.

tab employed_t1_hrs_focal employed_t1_earn_focal, m

gen has_hours_t2=.
replace has_hours_t2=0 if weekly_hrs_t2_focal==0
replace has_hours_t2=1 if weekly_hrs_t2_focal>0 & weekly_hrs_t2_focal!=.

gen has_earnings_t2=.
replace has_earnings_t2=0 if earnings_t2_focal==0
replace has_earnings_t2=1 if earnings_t2_focal>0 & earnings_t2_focal!=.

tab has_hours_t2 has_earnings_t2, m // t-2 has way more missing, but is a lot better
tab employed_t2_focal has_hours_t2, m
tab employed_t2_focal has_earnings_t2, m

// gen new_in_hh
gen new_in_hh=.
replace new_in_hh = NEW_HEAD_YEAR if relationship_==1
replace new_in_hh = NEW_WIFE_YEAR if relationship_==2
browse unique_id survey_yr relationship_ new_in_hh NEW_HEAD_YEAR NEW_WIFE_YEAR

// drop variables that aren't core (aka were used to create main variables)
drop LABOR_INCOME_T1_WIFE_ WAGES_T1_WIFE_ LABOR_INCOME_T1_HEAD WAGES_T1_HEAD WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_  ANNUAL_HOURS_T1_INDV EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ employ_head employ1_head employ2_head employ3_head employ_wife employ1_wife employ2_wife employ3_wife HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1 EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_  educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 START_YR_EMPLOYER_HEAD START_YR_CURRENT_HEAD START_YR_PREV_HEAD YRS_CURRENT_EMPLOY_HEAD START_YR_EMPLOYER_WIFE START_YR_CURRENT_WIFE START_YR_PREV_WIFE YRS_CURRENT_EMPLOY_WIFE total_housework_weekly RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_ race_1_head_rec race_2_head_rec race_3_head_rec race_4_head_rec race_1_wife_rec race_2_wife_rec race_3_wife_rec race_4_wife_rec BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_  BIRTHS_T1_OFUMS_ BIRTHS_T2_HEAD_ BIRTHS_T2_WIFE_ BIRTHS_T2_BOTH_  BIRTHS_T2_OFUMS_ DISABILITY_HEAD DISABILITY_WIFE DISABLE_HOWMUCH_HEAD DISABLE_HOWMUCH_WIFE SR_HEALTH_HEAD SR_HEALTH_WIFE SR_HEALTH_INDV SR_HEALTH_OTHER YR_RETIRED_HEAD YR_RETIRED_WIFE HOUSE_STATUS_  RESPONDENT_

save "$temp/inidividual_vars_imputation_long.dta", replace

********************************************************************************
**# now reshape back to wide to fill in the off years where possible (with t-2 data)
********************************************************************************
use "$temp/inidividual_vars_imputation_long.dta", clear

drop *_head* *_HEAD* *_wife* *_WIFE* *_INDV* *_indv* educ_completed wave MOVED_ MOVED_MONTH_  SPLITOFF_MONTH_ FAMILY_ID_SO_ MOVED_sp_ edulevelrp_match edulevelmaxrp_match edulevelsp_match edulevelmaxsp_match father_educ_focal mother_educ_focal family_structure_focal OFUM*_ID_ OFUM*_REL_ father_check mother_check

bysort unique_id: egen birth_yr_all = min(birth_yr)
drop birth_yr

reshape wide in_sample_ relationship_ MARITAL_PAIRS_ weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal max_educ_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ TOTAL_INCOME_T2_FAMILY_ raceth_fixed_focal raceth_focal childcare_focal adultcare_focal has_hours_t1 has_earnings_t1 has_hours_t2 has_earnings_t2 employed_t1_hrs_focal employed_t1_earn_focal SPLITOFF_ COMPOSITION_CHANGE_ NUM_IN_HH_ MOVED_YEAR_ SPLITOFF_YEAR_ DATA_RECORD_TYPE_ hh_status_ in_sample_sp_ relationship_sp_ MARITAL_PAIRS_sp_  MOVED_YEAR_sp_  SPLITOFF_sp_  SPLITOFF_YEAR_sp_ hh_status_sp_ moved_focal moved_sp partnered partnered_sp any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh under18 edulevel_match edulevelmax_match new_in_hh employment_status_focal disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal empstat_retired_focal num_65up_hh age65up house_status_all moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO REGION_ parent_in_ofum num_parent_in_hh father_in_hh mother_in_hh father_fam_id father_in_sample father_moved father_change_yr mother_fam_id mother_in_sample mother_moved mother_change_yr ///
, i(unique_id partner_id rel_start_all min_dur max_dur rel_end_all last_yr_observed ended SEX) j(survey_yr)  // birth_yr FIRST_BIRTH_YR ever_parent_focal num_births_focal lives_with_parent lives_with_grandparent 

// misstable summarize weekly_hrs_t1_focal*, all
// misstable summarize weekly_hrs_t2_focal*, all
// misstable summarize housework_focal*, all
misstable summarize weekly_hrs_t2_focal1999 weekly_hrs_t2_focal2001 // okay so I fixed this
misstable summarize *focal*, all

// weekly hours
browse unique_id weekly_hrs_t1_focal* weekly_hrs_t2_focal*
// gen weekly_hrs_t1_focal1998=weekly_hrs_t2_focal1999 // so, t-2 for 1999 becomes t-1 for 1998

forvalues y=1998(2)2022{
	local z=`y'+1
	gen weekly_hrs_t1_focal`y'=weekly_hrs_t2_focal`z'
}

browse weekly_hrs_t1_focal1998 weekly_hrs_t1_focal1999 weekly_hrs_t1_focal2000 weekly_hrs_t2_focal1999 weekly_hrs_t2_focal2001

// earnings
forvalues y=1998(2)2022{
	local z=`y'+1
	gen earnings_t1_focal`y'=earnings_t2_focal`z'
}

browse weekly_hrs_t1_focal1998 earnings_t1_focal1998 weekly_hrs_t2_focal1999 earnings_t2_focal1999

// family income
forvalues y=1998(2)2022{
	local z=`y'+1
	gen long TOTAL_INCOME_T1_FAMILY_`y'=TOTAL_INCOME_T2_FAMILY_`z'
}

browse TOTAL_INCOME_T1_FAMILY_1998 TOTAL_INCOME_T1_FAMILY_1999 TOTAL_INCOME_T2_FAMILY_1999

// employment status - this won't really work because one is t, not t-1... so I actually shouldn't need t-2, because I have t? so it needs to be t and t-1?
browse employed_focal2001 employed_focal2003 employed_t1_hrs_focal2001 employed_t1_hrs_focal2003 employed_t2_focal2001 employed_t2_focal2003
tab employed_focal2001 employed_t2_focal2003  // so these should match...but they really don't...
tab employed_focal1996 employed_t1_hrs_focal1997 // thsee should match -- also don't
tab employed_focal1996 employed_t1_earn_focal1997 // thsee should match -- also don't. also think it could be based on point in time? v. annual status
// should I update employment below to be based on hours / earnings? and not even use the status variables?

forvalues y=1998(2)2022{
	local z=`y'+1
	gen employed_focal`y'=employed_t1_hrs_focal`z'
}

// age of youngest child
browse unique_id AGE_YOUNG_CHILD_*

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	gen AGE_YOUNG_CHILD_`y'=.
	replace AGE_YOUNG_CHILD_`y' = AGE_YOUNG_CHILD_`x' + 1 if AGE_YOUNG_CHILD_`x' == AGE_YOUNG_CHILD_`z' - 2
}

// can I use births to fill in number of children?
label values NUM_CHILDREN_* .
browse unique_id NUM_CHILDREN_* any_births_t1_focal* any_births_t1_hh* any_births_t2_focal* any_births_t2_hh*
// browse unique_id NUM_CHILDREN_1985 NUM_CHILDREN_1986 NUM_CHILDREN_1987 NUM_CHILDREN_1988 any_births_t1_focal1985 any_births_t1_focal1986 any_births_t1_focal1987 any_births_t1_focal1988 any_births_t1_hh1985 any_births_t1_hh1986 any_births_t1_hh1987 any_births_t1_hh1988 any_births_t2_focal1985 any_births_t2_focal1986 any_births_t2_focal1987 any_births_t2_focal1988 any_births_t2_hh1985 any_births_t2_hh1986 any_births_t2_hh1987 any_births_t2_hh1988 // there isn't really good correspondence here between number of children and births - many time the # of kids increments with no recorded births - possibly bc not nec. in sample?

	// try merging on my other file
	merge m:1 unique_id using "$created_data/hh_birth_history_file_byUNIQUE.dta", keepusing(hh_births_1985 hh_births_1986 hh_births_1987 hh_births_1988 hh_births_1989 hh_births_199* hh_births_2* individ_birth_1985 ///
	individ_birth_1986 individ_birth_1987 individ_birth_1988 individ_birth_1989 individ_birth_199* individ_birth_2*)

	drop if _merge==2
	drop _merge
	
	// browse unique_id NUM_CHILDREN_1989 NUM_CHILDREN_1990 NUM_CHILDREN_1991 NUM_CHILDREN_1992 any_births_t1_hh1989 any_births_t1_hh1990 any_births_t1_hh1991 any_births_t1_hh1992 hh_births_1989 hh_births_1990 hh_births_1991 hh_births_1992 ///
	// AGE_YOUNG_CHILD_1989 AGE_YOUNG_CHILD_1990 AGE_YOUNG_CHILD_1991 AGE_YOUNG_CHILD_1992 any_births_t1_focal1989 any_births_t1_focal1990 any_births_t1_focal1991 any_births_t1_focal1992 individ_birth_1989 individ_birth_1990 individ_birth_1991  ///
	// individ_birth_1992 any_births_t2_focal1989 any_births_t2_focal1990 any_births_t2_focal1991 any_births_t2_focal1992 any_births_t2_hh1989 any_births_t2_hh1990 any_births_t2_hh1991 any_births_t2_hh1992
	
	// browse unique_id NUM_CHILDREN_1999 NUM_CHILDREN_2001 NUM_CHILDREN_2003 NUM_CHILDREN_2005 any_births_t1_hh1999 any_births_t1_hh2001 any_births_t1_hh2003 any_births_t1_hh2005 any_births_t2_hh1999 any_births_t2_hh2001 any_births_t2_hh2003 ///
	// any_births_t2_hh2005 hh_births_1999 hh_births_2000 hh_births_2001 hh_births_2002 hh_births_2003 hh_births_2004
	
	tab any_births_t1_hh1996 hh_births_1995 // in theory, these should match

// just doing to attempt to fill in off years where there is a CHANGE in between
// otherwise, I will not because this doesn't make a lot of sense.

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	gen NUM_CHILDREN_`y'=.
	replace NUM_CHILDREN_`y' = NUM_CHILDREN_`x' if NUM_CHILDREN_`z' == NUM_CHILDREN_`x' // if num of children is same between the waves, record same
	replace NUM_CHILDREN_`y' = NUM_CHILDREN_`z' if NUM_CHILDREN_`z' == NUM_CHILDREN_`x' + 1 & (any_births_t1_hh`z'==1 | any_births_t2_hh`z'==1 | hh_births_`y' !=0) // so if seems to have occurred in off year, record it there
	replace NUM_CHILDREN_`y' = NUM_CHILDREN_`x' if NUM_CHILDREN_`z' == NUM_CHILDREN_`x' + 1 & (hh_births_`y'==0 & hh_births_`z'!=0) // if the birth occurs in time z specifically (this is confusing because there is not t birth measure from the survey)
}

browse unique_id NUM_CHILDREN_1999 NUM_CHILDREN_2000 NUM_CHILDREN_2001 NUM_CHILDREN_2002 NUM_CHILDREN_2003 any_births_t1_hh1999 any_births_t1_hh2001 any_births_t1_hh2003 any_births_t1_hh2005 any_births_t2_hh1999 any_births_t2_hh2001 any_births_t2_hh2003 any_births_t2_hh2005 hh_births_1999 hh_births_2000 hh_births_2001 hh_births_2002 hh_births_2003 hh_births_2004

// let's try to fill in education for the years where the surrounding years are the same
browse unique_id educ_focal*

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	gen educ_focal`y'=.
	replace educ_focal`y' = educ_focal`x' if educ_focal`x' == educ_focal`z'
}


// do the same for disability status
browse unique_id disabled_focal* disabled_scale_focal* sr_health_focal* // think really can only do this for main disabled; others change quite a bit

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	gen disabled_focal`y'=.
	replace disabled_focal`y' = disabled_focal`x' if disabled_focal`x' == disabled_focal`z'
}


// and religion
browse unique_id religion_focal*
forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	capture gen religion_focal`y'=.
	replace religion_focal`y' = religion_focal`x' if religion_focal`x' == religion_focal`z'
	label values religion_focal`y' religion 
}


// fill in region based on mover status
browse unique_id REGION_* moved_in_lastyr* moved_yr_lastyr*
label define region 1 "Northeast" 2 "North Central" 3 "South" 4 "West" 5 "Alaska, Hawaii" 6 "Foreign"

forvalues y=1998(2)2022{
	local z=`y'+1
	capture gen REGION_`y'=.
	replace REGION_`y' = REGION_`z' if moved_in_lastyr`z' == 0
	replace REGION_`y' = REGION_`z' if moved_in_lastyr`z' == 1 & moved_yr_lastyr`z'!=`y'
	label values REGION_`y' region
}

// Other residence things
browse unique_id house_status_all*

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	capture gen house_status_all`y'=.
	replace house_status_all`y' = house_status_all`x' if house_status_all`x' == house_status_all`z'
	label values house_status_all`y' house 
}

browse unique_id lives_family_focal*

forvalues y=1998(2)2022{
	local z=`y'+1
	capture gen lives_family_focal`y'=.
	replace lives_family_focal`y' = lives_family_focal`z' if moved_in_lastyr`z' == 0
	replace lives_family_focal`y' = lives_family_focal`z' if moved_in_lastyr`z' == 1 & moved_yr_lastyr`z'!=`y'
	label values lives_family_focal`y' lives_family
}

browse unique_id father_in_hh1995 father_in_hh1996 father_in_hh1997 father_in_hh1999 father_in_hh2001 father_in_hh2003 father_moved1995 father_moved1996 father_moved1997 father_moved1999 father_moved2001 father_moved2003 father_change_yr1995 father_change_yr1996 father_change_yr1997 father_change_yr1999 father_change_yr2001 father_change_yr2003

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	local w=`y'-2
	gen father_in_hh`y'=.
	// stayed same
	replace father_in_hh`y' = father_in_hh`x' if father_in_hh`z' == father_in_hh`x' & father_moved`z'==0
	// moved in
	replace father_in_hh`y' = father_in_hh`z' if father_in_hh`z' == father_in_hh`x' + 1 & (father_moved`z'==1 & father_change_yr`z'==`y')
	replace father_in_hh`y' = father_in_hh`x' if father_in_hh`z' == father_in_hh`x' + 1 & (father_moved`z'==1 & father_change_yr`z'==`z')
	// moved out
	replace father_in_hh`y' = father_in_hh`z' if father_in_hh`z' == father_in_hh`x' - 1 & (inlist(father_moved`z',2,3,5) & (father_change_yr`z'==`y' | father_change_yr`z'==`w'))
	replace father_in_hh`y' = father_in_hh`x' if father_in_hh`z' == father_in_hh`x' - 1 & (inlist(father_moved`z',2,3,5) & father_change_yr`z'==`z')
}

browse unique_id father_in_hh1995 father_in_hh1996 father_in_hh1997 father_in_hh1998 father_in_hh1999 father_in_hh2000 father_in_hh2001 father_in_hh2002 father_in_hh2003 father_moved1995 father_moved1996 father_moved1997 father_moved1999 father_moved2001 father_moved2003 father_change_yr1995 father_change_yr1996 father_change_yr1997 father_change_yr1999 father_change_yr2001 father_change_yr2003

forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	local w=`y'-2
	gen mother_in_hh`y'=.
	// stayed same
	replace mother_in_hh`y' = mother_in_hh`x' if mother_in_hh`z' == mother_in_hh`x' & mother_moved`z'==0
	// moved in
	replace mother_in_hh`y' = mother_in_hh`z' if mother_in_hh`z' == mother_in_hh`x' + 1 & (mother_moved`z'==1 & mother_change_yr`z'==`y')
	replace mother_in_hh`y' = mother_in_hh`x' if mother_in_hh`z' == mother_in_hh`x' + 1 & (mother_moved`z'==1 & mother_change_yr`z'==`z')
	// moved out
	replace mother_in_hh`y' = mother_in_hh`z' if mother_in_hh`z' == mother_in_hh`x' - 1 & (inlist(mother_moved`z',2,3,5) & (mother_change_yr`z'==`y' | mother_change_yr`z'==`w'))
	replace mother_in_hh`y' = mother_in_hh`x' if mother_in_hh`z' == mother_in_hh`x' - 1 & (inlist(mother_moved`z',2,3,5) & mother_change_yr`z'==`z')
}

// update respondent info with non-survey year as category
forvalues y=1998(2)2022{
	capture gen RESPONDENT_WHO_`y'=9
	label values RESPONDENT_WHO_`y' resp
}

// while here: get constant year of retirement?
browse unique_id yr_retired_focal* empstat_retired_focal* // for some people this changes quite a lot...
egen max_yr_retired_focal = rowmax(yr_retired_focal*) // decide which to use later..
recode yr_retired_focal* (0 = 9999) // the below doesn't work without this because will return 0. don't want to set 0 to missing because want to distinguish true missing
egen min_yr_retired_focal = rowmin(yr_retired_focal*)
recode yr_retired_focal* (9999 = 0)
replace min_yr_retired_focal = 0 if min_yr_retired_focal==9999
// browse unique_id min_yr_retired_focal max_yr_retired_focal yr_retired_focal* empstat_retired_focal*

// also need to realign the t-1 variables
forvalues y=1985/2023{
	local x=`y'-1
	gen long TOTAL_INCOME_T_FAMILY`x' = TOTAL_INCOME_T1_FAMILY_`y'
	gen weekly_hrs_t_focal`x' = weekly_hrs_t1_focal`y'
	gen earnings_t_focal`x' = earnings_t1_focal`y'
}

browse weekly_hrs_t_focal1996 weekly_hrs_t_focal1997 weekly_hrs_t1_focal1996 weekly_hrs_t1_focal1997 weekly_hrs_t1_focal1998
tab weekly_hrs_t_focal2001 employed_focal2001, m // so there are people with hours but are employed 0
tab weekly_hrs_t_focal2000 employed_focal2000, m // the t-1 ones are better, because I think they are based off of each other.

// try to get rolling count of not in sample to use for later and trying to figure out permanent attrition
gen yrs_non_sample1985=.
replace yrs_non_sample1985=0 if in_sample_1985==1
replace yrs_non_sample1985=1 if in_sample_1985==0

forvalues y=1986/1997{
	local x=`y'-1
	gen yrs_non_sample`y'=.
	replace yrs_non_sample`y' = yrs_non_sample`x' + 1 if in_sample_`y'==0 // add 1 to prev yr if not in sample this year
	replace yrs_non_sample`y' = yrs_non_sample`x' if in_sample_`y'==1 // add 1 to prev yr if not in sample this year
}

forvalues y=1999(2)2023{
	local x=`y'-2
	gen yrs_non_sample`y'=.
	replace yrs_non_sample`y' = yrs_non_sample`x' + 1 if in_sample_`y'==0 // add 1 to prev yr if not in sample this year
	replace yrs_non_sample`y' = yrs_non_sample`x' if in_sample_`y'==1 // add 1 to prev yr if not in sample this year
}

forvalues y=1998(2)2022{
	local z=`y'+1
	gen yrs_non_sample`y'=.
	replace yrs_non_sample`y' = yrs_non_sample`z' 
	replace yrs_non_sample`y' = yrs_non_sample`z' 
}

forvalues y=1998(2)2022{
	local z=`y'+1
	gen in_sample_`y'=.
	replace in_sample_`y' = in_sample_`z' 
	replace in_sample_`y' = in_sample_`z' 
}

browse in_sample_1986 in_sample_1987 in_sample_1988 in_sample_2001 in_sample_2003 yrs_non_sample1986 yrs_non_sample1987 yrs_non_sample1988 yrs_non_sample2001 yrs_non_sample2003

// before reshaping, get last race observed to use for later
egen last_race_focal=rowlast(raceth_focal*) // tie break with last reported
browse last_race_focal raceth_focal*
label values last_race_focal raceth

// and first observed educational
egen first_educ_focal=rowfirst(educ_focal*)
browse first_educ_focal educ_focal*
label values first_educ_focal educ


********************************************************************************
* BACK to long so can recenter on duration and fill in some missings
********************************************************************************
reshape long in_sample_ relationship_ MARITAL_PAIRS_ weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal max_educ_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ TOTAL_INCOME_T2_FAMILY_ raceth_fixed_focal raceth_focal childcare_focal adultcare_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY has_hours_t1 has_earnings_t1 has_hours_t2 has_earnings_t2 employed_t1_hrs_focal employed_t1_earn_focal SPLITOFF_ COMPOSITION_CHANGE_ NUM_IN_HH_ MOVED_YEAR_ SPLITOFF_YEAR_ DATA_RECORD_TYPE_ hh_status_ in_sample_sp_ relationship_sp_ MARITAL_PAIRS_sp_  MOVED_YEAR_sp_  SPLITOFF_sp_  SPLITOFF_YEAR_sp_ hh_status_sp_ moved_focal moved_sp partnered partnered_sp yrs_non_sample any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh under18 edulevel_match edulevelmax_match new_in_hh individ_birth_ hh_births_ employment_status_focal disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal empstat_retired_focal num_65up_hh age65up house_status_all moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO_ REGION_ parent_in_ofum num_parent_in_hh father_in_hh mother_in_hh father_fam_id father_in_sample father_moved father_change_yr mother_fam_id mother_in_sample mother_moved mother_change_yr ///
, i(unique_id partner_id rel_start_all min_dur max_dur rel_end_all last_yr_observed ended SEX) j(survey_yr) // lives_with_parent lives_with_grandparent 

browse unique_id survey_yr rel_start_all rel_end_all min_dur max_dur relationship_ in_sample_ weekly_hrs_t_focal weekly_hrs_t1_focal weekly_hrs_t2_focal housework_focal
browse unique_id survey_yr in_sample_ TOTAL_INCOME_T_FAMILY TOTAL_INCOME_T1_FAMILY house_status_all REGION_ // because family income is at HH level, when not in sample, it is actually filled in, same with house status and region

foreach var in weekly_hrs_t1_focal weekly_hrs_t_focal earnings_t1_focal earnings_t_focal housework_focal employed_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal adultcare_focal childcare_focal raceth_focal employed_t1_hrs_focal employed_t1_earn_focal disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal empstat_retired_focal employment_status_focal religion_focal{
	replace `var'=. if in_sample_==0 // oh lol, I also did this here...
}

gen duration = survey_yr - rel_start_all
browse unique_id partner_id survey_yr rel_start_all duration last_yr_observed

browse unique_id survey_yr rel_start_all duration min_dur max_dur relationship_ in_sample_ weekly_hrs_t1_focal weekly_hrs_t2_focal housework_focal

browse unique_id survey_yr age_focal birth_yr
replace age_focal = survey_yr - birth_yr if age_focal==.

drop raceth_fixed_focal
bysort unique_id: egen raceth_fixed_focal = median(raceth_focal) // majority
tab raceth_fixed_focal, m
browse unique_id survey_yr last_race_focal raceth_focal raceth_fixed_focal
replace raceth_fixed_focal=last_race_focal if inlist(raceth_fixed_focal,1.5,2.5,3.5,4.5) // tie break with last observed (from above)
replace raceth_fixed_focal=last_race_focal if raceth_fixed_focal==. // if any other gaps, use last observed

*******************************************************************
* try to fill in partnership status using various history measures
*******************************************************************
browse unique_id partner_id survey_yr rel_start_all rel_end_all partnered partnered_sp MARITAL_PAIRS_
gen partnered_imp = partnered
replace partnered_imp = 1 if survey_yr>= rel_start_all & survey_yr <=rel_end_all
replace partnered_imp = .x if partnered_imp==. // using this, because partnered will be real missing if attrit / not in smaple, so need to distinguish real missing frm those i can fill in
// browse unique_id partner_id survey_yr rel_start_all rel_end_all partnered partnered_imp partnered_sp

gen change_yr=.
replace change_yr = MOVED_YEAR_ if MOVED_YEAR_ >0 & MOVED_YEAR_ <9000
replace change_yr = SPLITOFF_YEAR_ if SPLITOFF_YEAR_ >0 & SPLITOFF_YEAR_ <9000

gen permanent_attrit=0
replace permanent_attrit=1 if PERMANENT_ATTRITION==1 // attrited
replace permanent_attrit=2 if inlist(PERMANENT_ATTRITION,2,3) // marked as died
label define perm 0 "no" 1 "attrited" 2 "died"
label values permanent_attrit perm

fre ANY_ATTRITION
tab ANY_ATTRITION permanent_attrit, m

gen lt_attrit=0
replace lt_attrit = 1 if inlist(ANY_ATTRITION, 1,3,5)
tab lt_attrit permanent_attrit, m
tab lt_attrit has_psid_gene, m
tab permanent_attrit has_psid_gene, m

merge m:1 unique_id using "$created_data/psid_composition_history.dta", keepusing(rel*_start rel*_end marr*_start marr*_end coh*_start coh*_end hh*_start hh*_end mh_yr_married* mh_yr_end* mh_status* first_survey_yr last_survey_yr in_marital_history)
drop if _merge==2
drop _merge

forvalues r=1/5{
	replace rel`r'_end = rel_end_all if rel`r'_start==rel_start_all 
}

// browse unique_id survey_yr rel_start_all rel_end_all rel1_start rel1_end rel2_start rel2_end rel3_start rel3_end

rename first_survey_yr first_survey_yr_focal
rename last_survey_yr last_survey_yr_focal

replace in_sample_ = 0 if survey_yr > last_survey_yr & in_sample_==.

tab in_marital_history, m // okay so almost everyone (think bc i restricted to later relationships)

gen in_mar_est = 0
forvalues r=1/13{
	capture replace in_mar_est = 1 if survey_yr>=mh_yr_married`r' & survey_yr <=mh_yr_end`r' // just capturing if any of the rel variables indicate a relationship
}

browse unique_id partner_id survey_yr partnered partnered_imp rel_start_all rel_end_all in_mar_est mh_yr_married1 mh_yr_end1 mh_status1 mh_yr_married2 mh_yr_end2 mh_status2 mh_yr_married3 mh_yr_end3 mh_status3 MARITAL_PAIRS_ MARITAL_PAIRS_sp
replace partnered_imp = 1 if in_mar_est==1 // so, fill in based on marital history, which is collected by PSID so we assume is accurate

/*
gen in_rel_est = 0
forvalues r=1/5{
	replace in_rel_est = 1 if survey_yr>=rel`r'_start & survey_yr <=rel`r'_end
}
tab in_mar_est in_rel_est, m
*/

gen in_coh_est = 0 // bc, I know marital history, it's really just about the cohabitation history that i don't know, so don't use FULL rel indicator, just do COHAB one
forvalues r=1/3{
	replace in_coh_est = 1 if survey_yr>=coh`r'_start & survey_yr <=coh`r'_end
}

browse unique_id partner_id survey_yr partnered partnered_imp in_sample_ in_mar_est in_coh_est rel_start_all rel_end_all coh1_start coh1_end coh2_start coh2_end coh3_start coh3_end rel1_start rel1_end rel2_start rel2_end hh1_start hh2_start hh1_end hh2_end //  if in_rel_est==1 & in_mar_est==0

replace partnered_imp = 1 if in_coh_est==1 // these seem pretty accurate, so fill in

// now, we need to distinguish between not in rel - bc we have dates and know they aren't AND attrition so we don't actually know. well, from marital history, we def know if not married. I think problem is - do we know if not cohabiting?

gen in_rel_est = 0
replace in_rel_est = 1 if in_mar_est==1 | in_coh_est==1

tab in_rel_est in_sample_, m
browse unique_id partner_id survey_yr last_survey_yr partnered_imp in_sample_ in_rel_est in_mar_est in_coh_est
replace partnered_imp = 0 if in_sample_==1 & in_rel_est==0

browse unique_id partner_id survey_yr last_survey_yr partnered_imp in_sample_ in_rel_est rel_start_all rel_end_all mh_yr_married1 mh_yr_end1 mh_yr_married2 mh_yr_end2 hh1_start hh2_start hh1_end hh2_end rel1_start rel1_end rel2_start rel2_end permanent_attrit lt_attrit yrs_non_sample last_survey_yr YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST  moved_focal change_yr

tab partnered_imp in_sample_, m // so all the missing are now those not in sample. so, those are actually missing, because while I know not married, bc should be in history, it''s possible they were cohabiting?
tab partnered_imp, m
tab partnered partnered_imp, m

// can I identify relationship order based on start date match?
// browse unique_id rel_start_all rel*_start rel*_end marr*_start marr*_end coh*_start coh*_end
gen current_rel_number=.

forvalues m=1/13{
	capture replace current_rel_number = `m' if rel_start_all==mh_yr_married`m' & rel_type_constant==1 // do this first for MARRIAGES because provided by PSID so should be accurate
}

forvalues r=1/5{
	replace current_rel_number = `r' if current_rel_number==. & (rel_start_all==rel`r'_start | rel_start_all==(rel`r'_start - 1)) // this is meant to cover all relationship types
}

forvalues c=1/3{
	replace current_rel_number = `c' if current_rel_number==. & (rel_start_all==coh`c'_start | rel_start_all==(coh`c'_start - 1))
}

forvalues m=1/5{
	replace current_rel_number = `m' if current_rel_number==. & (rel_start_all==marr`m'_start | rel_start_all==(marr`m'_start - 1))
}

tab current_rel_number, m
browse unique_id survey_yr rel_start_all current_rel_number rel*_start rel*_end mh_yr_married* coh*_start coh*_end marr*_start marr*_end 
browse unique_id survey_yr rel_start_all current_rel_number rel*_start rel*_end mh_yr_married* coh*_start coh*_end marr*_start marr*_end  if current_rel_number==.

// some of the missing it's obvious that it's not the FIRST relationship. so if missing, set to 1 if no other relationships recorded, otherwise, try to do based on count of relationships that started before this one?
rename current_rel_number current_rel_number_v0

gen current_rel_number = current_rel_number_v0
replace current_rel_number = 1 if current_rel_number_v0 == . & rel1_start==. & rel1_end==. & mh_yr_married1==. & coh1_start==. & coh1_end==. & marr1_start==. & marr1_end==. // if no identified first relationships, consider this first

gen rel_counter=0
forvalues r=1/5{
	replace rel_counter = rel_counter + 1 if rel`r'_start < rel_start_all // this is meant to cover all relationship types
}

tab rel_counter current_rel_number,m // good validation also. except not really aligned 
browse unique_id survey_yr rel_start_all rel_counter current_rel_number rel*_start rel*_end mh_yr_married* coh*_start coh*_end marr*_start marr*_end  if current_rel_number==.

replace current_rel_number = rel_counter + 1 if current_rel_number==.

// then do marriage order - jic
capture label define rel_type_constant 1 "Married" 2 "Cohab" 3 "Transitioned"
label values rel_type_constant rel_type_constant
tab rel_type_constant, m 

browse unique_id rel_type_constant transition_yr_est mh_yr_married* marr*_start

gen current_marr_number=.

forvalues m=1/13{
	capture replace current_marr_number = `m' if inlist(rel_type_constant,1,3) & rel_start_all==mh_yr_married`m' // Provided by PSID so should be accurate
}

forvalues m=1/13{
	capture replace current_marr_number = `m' if current_marr_number==. & inlist(rel_type_constant,1,3) & transition_yr_est==mh_yr_married`m' // Provided by PSID so should be accurate
}

forvalues m=1/5{
	replace current_marr_number = `m' if current_marr_number==. & inlist(rel_type_constant,1,3) & (rel_start_all==marr`m'_start | rel_start_all==(marr`m'_start - 1)) // then do created jic
}

forvalues m=1/5{
	replace current_marr_number = `m' if current_marr_number==. & inlist(rel_type_constant,1,3) & (transition_yr_est==marr`m'_start | transition_yr_est==(marr`m'_start - 1)) // then do created jic
}

tab current_marr_number, m
tab current_marr_number rel_type_constant, m
tab current_rel_number current_marr_number, m
tab current_rel_number if current_marr_number==. & inlist(rel_type_constant,1,3), m

gen marr_counter=0
forvalues m=1/13{
	capture replace marr_counter = marr_counter + 1 if mh_yr_married`m' < rel_start_all & rel_type_constant == 1
	capture replace marr_counter = marr_counter + 1 if mh_yr_married`m' < transition_yr_est & rel_type_constant == 3
}

tab marr_counter, m
tab marr_counter current_marr_number, m
browse unique_id marr_counter current_marr_number rel_start_all transition_yr_est mh_yr_married*

browse unique_id rel_type_constant rel_start_all marr_counter current_marr_number current_rel_number transition_yr_est mh_yr_married* marr*_start if current_marr_number==. & inlist(rel_type_constant,1,3)

replace current_marr_number = marr_counter + 1 if current_marr_number==. & inlist(rel_type_constant,1,3)

// browse unique_id survey_yr rel_start_all rel_counter current_marr_number current_rel_number rel*_start rel*_end mh_yr_married* coh*_start coh*_end marr*_start marr*_end  if current_rel_number!=current_marr_number & inlist(rel_type_constant,1,3)

// then do cohab order - jic
gen current_coh_number=.

forvalues c=1/3{
	capture replace current_coh_number = `c' if rel_type_constant==2 & (rel_start_all==coh`c'_start | rel_start_all==(coh`c'_start - 1))
}

forvalues r=1/5{
	replace current_coh_number = `r' if current_coh_number==. & rel_type_constant==2 & (rel_start_all==rel`r'_start | rel_start_all==(rel`r'_start - 1))
}

gen coh_counter=0
forvalues c=1/3{
	capture replace coh_counter = coh_counter + 1 if coh`c'_start < rel_start_all & rel_type_constant == 2
}

tab current_coh_number rel_type_constant, m
browse unique_id rel_start_all current_coh_number current_rel_number coh*_start rel*_start mh_yr_married* if rel_type_constant==2 & current_coh_number==.

replace current_coh_number = coh_counter + 1 if current_coh_number==. & rel_type_constant == 2

browse unique_id rel_start_all rel_type_constant current_rel_number current_marr_number current_coh_number
tab rel_type_constant if current_marr_number > current_rel_number & current_marr_number!=., m  // okay, the transition to cohab got a little funky
// browse unique_id rel_start_all rel_type_constant current_rel_number current_marr_number current_coh_number rel*_start mh_yr_married* if current_marr_number > current_rel_number & current_marr_number!=.

egen current_rel_number_main = rowmax(current_rel_number current_marr_number current_coh_number)

*******************************************************************
* Now child info (number and age)
*******************************************************************

// try to fill in number of children that are missing - same thing, this will be easier for OFF years v. many missing years
sort unique_id survey_yr
browse unique_id FAMILY_INTERVIEW_NUM_  in_sample_

// add on full history birth history for respondent
merge m:1 unique_id using "$created_data/birth_history_wide.dta" // now get person specific births
drop if _merge==2
drop _merge 

label values  NUM_CHILDREN_ NUM_IN_HH_ NUM_BIRTHS .

gen rolling_births=0
forvalues y=1968/2023{
	forvalues c=1/20{
		replace rolling_births = rolling_births + 1 if survey_yr==`y' & inrange(cah_child_birth_yr`c',1900,`y') 
	}	
}

gen had_birth=0
forvalues y=1968/2023{
	forvalues c=1/20{
		replace had_birth = 1 if survey_yr==`y' & cah_child_birth_yr`c'==`y'
	}	
}

browse unique_id survey_yr NUM_BIRTHS rolling_births had_birth cah_child_birth_yr*

tab hh_births_
gen hh_births_est = hh_births_
replace hh_births_est = hh_births_ / 2 if inlist(hh_births_,2,4,6,8)
tab hh_births_est, m
tab had_birth individ_birth_, m

gen increment_birth = .
replace increment_birth = 0 if had_birth==0 & hh_births_est==0
replace increment_birth = 1 if had_birth==1
replace increment_birth = 1 if hh_births_est>=1 & hh_births_est!=. & increment_birth==.
tab increment_birth, m

browse unique_id partner_id survey_yr NUM_CHILDREN_ AGE_YOUNG_CHILD_ rolling_births increment_birth had_birth individ_birth_ hh_births_est cah_child_birth_yr*

gen num_children_imp_focal = NUM_CHILDREN_
replace num_children_imp_focal = num_children_imp_focal[_n-1] if had_birth==0 & num_children_imp_focal==. & unique_id == unique_id[_n-1] & survey_yr == survey_yr[_n-1]+1
replace num_children_imp_focal = num_children_imp_focal[_n-1] + 1 if had_birth==1 & num_children_imp_focal==. & unique_id == unique_id[_n-1] & survey_yr == survey_yr[_n-1]+1

browse unique_id survey_yr had_birth NUM_CHILDREN_ num_children_imp_focal rolling_births
tab rolling_births num_children_imp_focal, m  // rolling births and num_children are misaligned enough that i think I can't replace. I think they are just two diff indicators...

gen num_children_imp_hh = NUM_CHILDREN_
replace num_children_imp_hh = num_children_imp_hh[_n-1] if increment_birth==0 & num_children_imp_hh==. & unique_id == unique_id[_n-1] & survey_yr == survey_yr[_n-1]+1
replace num_children_imp_hh = num_children_imp_hh[_n-1] + 1 if increment_birth==1 & num_children_imp_hh==. & unique_id == unique_id[_n-1] & survey_yr == survey_yr[_n-1]+1

tab NUM_CHILDREN_, m
tab num_children_imp_focal, m
tab num_children_imp_hh, m

tab num_children_imp_hh num_children_imp_focal, m // good alignment, just sometimes the HH has more, which makes sense
replace num_children_imp_hh = num_children_imp_focal if num_children_imp_hh==.

browse unique_id survey_yr in_sample_ num_children_imp_focal num_children_imp_hh NUM_CHILDREN_ NUM_IN_HH_ increment_birth had_birth rolling_births hh_births_est FIRST_BIRTH_YR any_births_t1_focal any_births_t1_hh cah_child_birth_yr* COMPOSITION_CHANGE

inspect NUM_CHILDREN_
inspect num_children_imp_focal
inspect num_children_imp_hh

tab num_children_imp_hh in_sample_,m 

// try to use birth history information to create better age of youngest child variable
browse unique_id survey_yr ever_parent_focal FIRST_BIRTH_YR LAST_BIRTH_YR num_children_imp_hh AGE_YOUNG_CHILD_ cah_child_birth_yr* // okay this is VERY incongruous, so actually can't use this
// instead try to fill in based on previous years (eg if 12 before missing and 14 after, then is 13?) should probably do that while wide? moving this code up

*******************************************************************
* Small other updates
*******************************************************************

// do I want to try to update education here or later?
bysort unique_id (max_educ_focal): replace max_educ_focal=max_educ_focal[1]
sort unique_id survey_yr
browse unique_id survey_yr relationship_ under18 duration educ_focal max_educ_focal new_in_hh
	gen educ_focal_imp = educ_focal
	*// https://www.stata.com/support/faqs/data-management/replacing-missing-values/
	by unique_id: replace educ_focal_imp = educ_focal_imp[_n-1] if educ_focal_imp==.
label value educ_focal_imp educ
tab educ_focal_imp max_educ_focal, m

replace educ_focal_imp = max_educ_focal if educ_focal_imp==. & first_educ_focal==max_educ_focal & max_educ_focal!=.
replace educ_focal_imp = first_educ_focal if educ_focal_imp==. & first_educ_focal==1
tab educ_focal_imp if dur>=-2, m

sort unique_id partner_id survey_yr
browse unique_id survey_yr relationship_ under18 duration educ_focal_imp educ_focal first_educ_focal max_educ_focal new_in_hh

// need to fix my calculated parent variable now that i filled in off years
capture gen num_parent_in_hh = .
replace num_parent_in_hh = 0 if father_in_hh==0 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==1 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==0 & mother_in_hh==1
replace num_parent_in_hh = 2 if father_in_hh==1 & mother_in_hh==1

**# Here the data is now long, by duration - only the durations I want.
tab duration, m
keep if duration >=-2 // only keeping up to 2 years prior, bc there are so many missing pre duration 0, it's not very stable.
keep if duration <=12 // up to 10/11 for now - but adding a few extra years so I can do the lookups below and still retain up to 20

replace partnered_imp=. if partnered_imp==.x

save "$created_data/individs_by_duration_long.dta", replace

// 
use "$created_data/individs_by_duration_long.dta", clear

unique unique_id partner_id
egen couple_id = group(unique_id partner_id)
drop if couple_id==.
browse couple_id unique_id partner_id duration SEX

// make it rectangular then fill in fixed characteristics for those added
fillin couple_id duration
tab duration
unique couple_id

bysort couple_id (SEX): replace SEX=SEX[1] if SEX==.
bysort couple_id (unique_id): replace unique_id=unique_id[1] if unique_id==.
bysort couple_id (partner_id): replace partner_id=partner_id[1] if partner_id==.

foreach var in rel_start_all min_dur max_dur rel_end_all last_yr_observed ended birth_yr_all raceth_fixed_focal FIRST_BIRTH_YR LAST_BIRTH_YR PSID_COHORT sample_type last_race_focal rel_type_constant  main_fam_id SAMPLE has_psid_gene first_survey_yr_focal  last_survey_yr_focal first_educ_focal max_educ_focal transition_yr_est rel_status current_rel_number_main current_rel_number current_coh_number current_marr_number mpf_focal ever_parent_focal num_births_focal min_yr_retired_focal max_yr_retired_focal father_max_educ_focal mother_max_educ_focal family_structure_cons_focal father_unique_id FATHER_YR_BORN mother_unique_id MOTHER_YR_BORN{
	bysort couple_id (`var'): replace `var'=`var'[1] if `var'==.
}

/* incorporate all of these above, these are people who going past 10 years is past 2023
browse unique_id duration birth_yr_all if inlist(unique_id,4039,4180,4201,4202,5004,5004,5183,5183,6032,6177,7032)
bysort unique_id (birth_yr_all): replace birth_yr_all = birth_yr_all[1]

browse if race_fixed_focal==.
browse unique_id duration race_fixed_focal if inlist(unique_id,4039,4180,4201,4202,5004,5004,5183,5183,6032,6177,7032)
bysort unique_id (race_fixed_focal): replace race_fixed_focal = race_fixed_focal[1]
bysort unique_id (FIRST_BIRTH_YR): replace FIRST_BIRTH_YR = FIRST_BIRTH_YR[1]
sort unique_id partner_id duration
*/

browse unique_id survey_yr birth_yr_all age_focal duration if inlist(unique_id,4039,4180,4201,4202,5004,5004,5183,5183,6032,6177,7032)
sort unique_id partner_id duration
replace survey_yr = survey_yr[_n-1]+1 if survey_yr==.

replace age_focal=survey_yr - birth_yr_all if age_focal==.

// while here, add that indicator I want of first birth relative to this rel start
browse unique_id survey_yr rel_start_all ever_parent_focal FIRST_BIRTH_YR
browse unique_id survey_yr rel_start_all ever_parent_focal FIRST_BIRTH_YR cah_child_birth_yr* if ever_parent_focal==1 & FIRST_BIRTH_YR==9999

egen first_birth_yr_calc = rowmin(cah_child_birth_yr*)
bysort unique_id: egen first_birth_yr_calc_dedup = min(first_birth_yr_calc)
tab first_birth_yr_calc_dedup if FIRST_BIRTH_YR==9999

gen first_birth_yr_alt=FIRST_BIRTH_YR
replace first_birth_yr_alt = first_birth_yr_calc_dedup if ever_parent_focal==1 & FIRST_BIRTH_YR==9999 & first_birth_yr_calc_dedup!=9999 & first_birth_yr_calc_dedup!=9998 & first_birth_yr_calc_dedup!=.

gen birth_timing_rel = rel_start_all - first_birth_yr_alt if first_birth_yr_alt!=9999
replace birth_timing_rel = 9999 if first_birth_yr_alt==9999

// and parent status based on first birth year
browse unique_id survey_yr ever_parent_focal first_birth_yr_alt
gen current_parent_status=.
replace current_parent_status = 0 if ever_parent_focal==0
replace current_parent_status = 0 if ever_parent_focal==1 & survey_yr < first_birth_yr_alt
replace current_parent_status = 1 if ever_parent_focal==1 & survey_yr >= first_birth_yr_alt

// oh yeah, want to fill in retirement status based on year retired
gen ever_retired = .
replace ever_retired = 0 if inlist(max_yr_retired_focal,0,9998,9999) & inlist(min_yr_retired_focal,0,9998,9999)
replace ever_retired = 1 if !inlist(max_yr_retired_focal,0,9998,9999) | !inlist(min_yr_retired_focal,0,9998,9999)

gen retired_est_focal=.
replace retired_est_focal = 0 if ever_retired==0
replace retired_est_focal = 0 if ever_retired==1 & survey_yr < max_yr_retired_focal
replace retired_est_focal = 1 if ever_retired==1 & survey_yr >= max_yr_retired_focal

tab ever_retired retired_est_focal , m
tab retired_est_focal empstat_retired_focal, m

browse unique_id partner_id survey_yr ever_retired min_yr_retired_focal max_yr_retired_focal retired_est_focal empstat_retired_focal if ever_retired==1

// recode respondent_who for non-survey years
replace RESPONDENT_WHO_ = 9 if RESPONDENT_WHO_==. & survey_yr>2023

// let's recode housing_status so I can attempt to use ologit
rename house_status_all house_status_all_v0

gen house_status_all = .
replace house_status_all = 0 if house_status_all_v0==3 // neither
replace house_status_all = 1 if house_status_all_v0==2 // rents
replace house_status_all = 2 if house_status_all_v0==1 // owns

label define house_status 0 "Neither" 1 "Rents" 2 "Owns"
label values house_status_all house_status

// get ready to reshape back
gen duration_rec=duration+2 // negatives won't work in reshape or with sq commands - so make -2 0

sort couple_id duration
browse couple_id duration weekly_hrs_t1_focal housework_focal _fillin

replace weekly_hrs_t1_focal=. if weekly_hrs_t1_focal>900 & weekly_hrs_t1_focal!=.
replace weekly_hrs_t_focal=. if weekly_hrs_t_focal>900 & weekly_hrs_t_focal!=.
replace TOTAL_INCOME_T_FAMILY=. if TOTAL_INCOME_T_FAMILY>9000000 & TOTAL_INCOME_T_FAMILY!=.
replace TOTAL_INCOME_T1_FAMILY_=. if TOTAL_INCOME_T1_FAMILY_>9000000 & TOTAL_INCOME_T1_FAMILY_!=.
replace earnings_t_focal=. if earnings_t_focal>9000000 & earnings_t_focal!=.
replace earnings_t1_focal=. if earnings_t1_focal>9000000 & earnings_t1_focal!=.

// just to get a better sense of the data instead of plotting by the continuous variable
gen hours_type_t1_focal=.
replace hours_type_t1_focal=0 if weekly_hrs_t1_focal==0
replace hours_type_t1_focal=1 if weekly_hrs_t1_focal>0 & weekly_hrs_t1_focal<35
replace hours_type_t1_focal=2 if weekly_hrs_t1_focal>=35 & weekly_hrs_t1_focal!=.

// sqset hours_type_t1 couple_id duration_rec
// sqindexplot, gapinclude
// sqindexplot, gapinclude by(SEX)
// sdchronogram hours_type_t1

// just to get a better sense of the data instead of plotting by the continuous variable
gen hw_hours_gp=.
replace hw_hours_gp=0 if housework_focal==0
replace hw_hours_gp=1 if housework_focal>0 & housework_focal<10
replace hw_hours_gp=2 if housework_focal>=10 & housework_focal!=.

// sqset hw_hours_gp couple_id duration_rec
// sqindexplot, gapinclude
// sqindexplot, gapinclude by(SEX)

// do patterns of missing make sense? yes
tab survey_yr hours_type_t1_focal, m
tab survey_yr hw_hours_gp, m

// expore SHAPE of data for continuous variables
histogram weekly_hrs_t1_focal, width(1)
histogram weekly_hrs_t1_focal if weekly_hrs_t1_focal>0, width(1)
histogram housework_focal, width(1)

/*
gen partnered=.
replace partnered=0 if MARITAL_PAIRS_==0
replace partnered=1 if inrange(MARITAL_PAIRS_,1,3)
*/

tabstat weekly_hrs_t_focal housework_focal childcare_focal adultcare_focal employed_focal earnings_t_focal age_focal birth_yr_all educ_focal college_focal raceth_focal raceth_fixed_focal ever_parent_focal children num_children_imp_focal num_children_imp_hh num_births_focal rolling_births FIRST_BIRTH_YR AGE_YOUNG_CHILD_ birth_timing_rel relationship_ partnered_imp current_rel_number_main mpf_focal TOTAL_INCOME_T_FAMILY sample_type num_parent_in_hh num_65up_hh disabled_focal sr_health_focal retired_est_focal, stats(mean sd p50) columns(statistics)

********************************************************************************
* reshaping wide for imputation purposes
********************************************************************************
rename transition_yr_est transition_yr

drop survey_yr duration _fillin MARITAL_PAIRS_ *_sp *_sp* cah_* mh_* rel*_start rel*_end marr*_start marr*_end coh*_start coh*_end hh*_start hh*_end MOVED_YEAR_ MOVED_YEAR_sp_ moved_focal moved_sp any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh *_est SPLITOFF* COMPOSITION_CHANGE_ NUM_IN_HH_ DATA_RECORD_TYPE_  SAMPLE_STATUS_TYPE PERMANENT_ATTRITION ANY_ATTRITION permanent_attrit lt_attrit YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST yrs_non_sample change_yr in_marital_history int_number per_num INTERVIEW_NUM_1968 individ_birth_ current_rel_number_v0  rel_counter marr_counter coh_counter NUM_BIRTHS first_birth_yr_calc first_birth_yr_calc_dedup  first_birth_yr_alt father_fam_id father_in_sample father_moved father_change_yr mother_fam_id mother_in_sample mother_moved mother_change_yr parent_in_ofum house_status_all_v0 // hh_births_pre1968

reshape wide in_sample_ hh_status_ relationship_  partnered weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal max_educ_focal educ_focal educ_focal_imp college_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ hours_type_t1_focal hw_hours_gp raceth_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY childcare_focal adultcare_focal TOTAL_INCOME_T2_FAMILY_ has_hours_t1 has_earnings_t1 has_hours_t2 has_earnings_t2 employed_t1_hrs_focal employed_t1_earn_focal partnered_imp num_children_imp_focal num_children_imp_hh rolling_births had_birth hh_births* increment_birth under18 edulevel_match edulevelmax_match new_in_hh disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal empstat_retired_focal retired_est_focal num_65up_hh age65up house_status_all moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO_ REGION_ employment_status_focal current_parent_status num_parent_in_hh father_in_hh mother_in_hh ///
, i(couple_id unique_id partner_id rel_start_all min_dur max_dur rel_end_all last_yr_observed ended SEX) j(duration_rec)

forvalues d=0/14{
	gen employed_focal_rec`d' = employed_focal`d'
	replace employed_focal_rec`d' = 1 if employed_focal`d'==0 & weekly_hrs_t_focal`d' > 0 & weekly_hrs_t_focal`d'!=. // so, put them as employed if there are hours
	replace employed_focal_rec`d' = 1 if employed_focal`d'==. & weekly_hrs_t_focal`d' > 0 & weekly_hrs_t_focal`d'!=. // so, put them as employed if there are hours
	replace employed_focal_rec`d' = 0 if employed_focal`d'==. & weekly_hrs_t_focal`d' == 0 // so, put them as unemployed if hours are 0 and employment is missing
}

tab weekly_hrs_t_focal5 employed_focal5, m
tab weekly_hrs_t_focal5 employed_focal_rec5, m

tab employed_focal5, m
tab employed_focal_rec5, m

// labels 
capture label define rel_status 1 "Intact" 3 "Widow" 4 "Divorce" 5 "Separated" 6 "Attrited"
label values rel_status rel_status

// try to fill in education if unchanged throughout observation here (now that duration is restricted)
egen min_educ = rowmin(educ_focal_imp*)
egen max_educ = rowmax(educ_focal_imp*)

browse unique_id min_educ max_educ educ_focal_imp*

forvalues d=0/14{
	replace educ_focal_imp`d' = max_educ if min_educ==max_educ & educ_focal_imp`d'==.
}

replace educ_focal_imp3 = educ_focal_imp4 if educ_focal_imp3==.
replace educ_focal_imp2 = educ_focal_imp3 if educ_focal_imp2==.
replace educ_focal_imp1 = educ_focal_imp2 if educ_focal_imp1==.
replace educ_focal_imp0 = educ_focal_imp1 if educ_focal_imp0==.

replace educ_focal_imp0 = min_educ if educ_focal_imp0==.
replace educ_focal_imp1 = min_educ if educ_focal_imp1==.
replace educ_focal_imp2 = min_educ if educ_focal_imp2==.

tab educ_focal_imp2, m
browse unique_id min_educ max_educ educ_focal_imp* if educ_focal_imp2==.

gen fixed_education=educ_focal_imp2 // duration 0

// might need to attempt to fill in more disabled status - causing problems with imputation
browse unique_id disabled_focal*
egen max_disabled_focal = rowmax(disabled_focal*) // if max disabled = 0 then means never observed as disabled. let's give them zeroes for all?
egen min_disabled_focal = rowmin(disabled_focal*) // if max disabled = 0 then means never observed as disabled. let's give them zeroes for all?
tab min_disabled_focal max_disabled_focal, m  // very few disabled whole time (will have 1 for min)

forvalues d=0/14{
	capture gen disabled_imp_focal`d' = disabled_focal`d'
	replace disabled_imp_focal`d' = 0 if disabled_imp_focal`d' ==. &  max_disabled_focal==0
	replace disabled_imp_focal`d' = 1 if disabled_imp_focal`d' ==. &  min_disabled_focal==1
}

browse unique_id max_disabled_focal  disabled_imp_focal* disabled_focal*

**# Here the data is now reshaped wide, by duration
save "$created_data/individs_by_duration_wide.dta", replace
// use "$created_data\individs_by_duration_wide.dta", clear

// make sure all of these variables exist in final file: rel_start_all rel_end_all rel_status rel_type_constant min_dur max_dur last_yr_observed ended transition_yr

********************************************************************************
* Exploration
********************************************************************************
// first, let's just get a sense of missings
unique unique_id
unique unique_id partner_id

browse unique_id partner_id couple_id weekly_hrs_t1_focal*
browse unique_id housework_focal*

misstable summarize *focal*, all // they all have missing, but some feel low?? oh I am dumb, I was reading wrong - the right column is where we HAVE data, so the ones that seem low are mostly the t2 variables, which makes sense,bc didn't exist until 1999 okay I actually feel okay about this
misstable summarize *focal0, all // -2? (first time point)
misstable summarize *focal2, all // 0
misstable summarize *focal3, all // t1
misstable summarize *focal14, all // last time point
mdesc *focal*

egen nmis_workhrs = rmiss(weekly_hrs_t1_focal*)
tab nmis_workhrs, m
browse nmis_workhrs weekly_hrs_t1_focal*

egen nmis_hwhrs = rmiss(housework_focal*)
tab nmis_hwhrs, m

forvalues y=0/14{
	replace weekly_hrs_t1_focal`y' = round(weekly_hrs_t1_focal`y',1)
}

/*
forvalues y=1/16{
	replace hours_type_t1_focal`y' = 4 if hours_type_t1_focal`y'==.
	replace hours_type_t1_focal`y' = 3 if hours_type_t1_focal`y'==0
}
*/

// attempting to export mdesc, following: https://www.statalist.org/forums/forum/general-stata-discussion/general/1643775-export-mdesc-table-to-excel

program mmdesc, rclass byable(recall)
syntax [varlist] [if] [in]
tempvar touse
mark `touse' `if' `in'
local nvars : word count `varlist' 
tempname matrix 
matrix `matrix' = J(`nvars', 3, .) 
local i = 1 
quietly foreach var of local varlist {
    count if missing(`var') & `touse' 
    matrix `matrix'[`i', 1] = r(N) 
    count if `touse'
    matrix `matrix'[`i', 2] = r(N) 
    matrix `matrix'[`i', 3] = `matrix'[`i',1] / `matrix'[`i',2] 
    local ++i  
}
matrix rownames `matrix' = `varlist'                     
matrix colnames `matrix' = Missing Total Missing/Total 
matrix list `matrix', noheader 
return matrix table = `matrix' 
end

putexcel set "$root/imputation/psid_missingtable.xlsx", replace
mmdesc FAMILY_INTERVIEW_NUM_0-disabled_imp_focal14
putexcel A1 = matrix(r(table))

// sdchronogram hours_type_t1_focal0-hours_type_t1_focal16 // this is not working; I am not sure why

********************************************************************************
**# Before I impute, want to match and explore child variables
********************************************************************************
/*
use "$created_data\individs_by_duration_long.dta", clear

keep unique_id partner_id survey_yr duration rel_start_all min_dur max_dur NUM_CHILDREN_ num_children_imp_focal num_children_imp_hh had_birth increment_birth cah_child_birth_yr*

save "$temp\kids_long.dta", replace

keep unique_id partner_id survey_yr NUM_CHILDREN_ num_children_imp_focal num_children_imp_hh had_birth increment_birth cah_child_birth_yr*
foreach var in NUM_CHILDREN_ num_children_imp_focal num_children_imp_hh had_birth increment_birth cah_child_birth_yr*{
	rename `var' `var'_sp
}

rename partner_id x
rename unique_id partner_id
rename x unique_id

save "$temp\kids_long_tomatch.dta", replace

use "$temp\kids_long.dta", clear
merge 1:1 unique_id partner_id survey_yr using "$temp\kids_long_tomatch.dta" // okay, so 485 didn't match
drop if _merge ==2
drop _merge

save "$temp\partner_kids_long.dta", replace

tab NUM_CHILDREN_ NUM_CHILDREN__sp, m // these already don't match
browse unique_id partner_id survey_yr duration rel_start_all min_dur max_dur

tab NUM_CHILDREN_ NUM_CHILDREN__sp if duration>=min_dur & duration<=max_dur, m // okay, this is much closer

// tab num_children_imp_focal num_children_imp_focal_sp, m
tab num_children_imp_focal num_children_imp_focal_sp if duration>=min_dur & duration<=max_dur, m // okay, this is much closer

// tab num_children_imp_hh num_children_imp_hh_sp, m 
tab num_children_imp_hh num_children_imp_hh_sp if duration>=min_dur & duration<=max_dur, m // okay, this is much closer - okay they are basically the same amount of accuracy. here, there are just more kids, which makes sense because it doesn't restrict to JUST the focal people, which is what the variable essentally is

tab had_birth had_birth_sp if duration>=min_dur & duration<=max_dur, m 
tab increment_birth increment_birth_sp if duration>=min_dur & duration<=max_dur, m // so these match a bit more than births above

browse unique_id partner_id survey_yr duration NUM_CHILDREN_ NUM_CHILDREN__sp num_children_imp_focal num_children_imp_focal_sp num_children_imp_hh num_children_imp_hh_sp had_birth had_birth_sp increment_birth increment_birth_sp cah_child_birth_yr1 cah_child_birth_yr1_sp
*/

********************************************************************************
**# MICT Exploration
********************************************************************************
/*
** Looking at steps in Halpin 2016
mict_prep weekly_hrs_t1_focal, id(couple_id)

// browse _mct_id _mct_t _mct_state _mct_last _mct_next // last feels off? okay last and next are created...somewhere else? they aren't created here? I am so confused...bc they are supposed to be created here...

// redefine bc they should be regress, not mlogit, but can't use augment when I do that, just fyi
// trying ologit instead of regress bc i think it's predicting non-integer numbers. OKAY if I just remove the i. from next and last is that actually fine??
// okay so that is not converging LOL

capture program drop mict_model_gap
program mict_model_gap
mi impute regress _mct_state ///
_mct_next _mct_last ///
_mct_before* _mct_after*, ///
add(1) force
end

capture program drop mict_model_initial
program mict_model_initial
mi impute regress _mct_state _mct_next _mct_after*, add(1) force
end

capture program drop mict_model_terminal
program mict_model_terminal
mi impute regress _mct_state _mct_last _mct_before*, add(1) force
end

mict_impute, maxgap(6) maxitgap(3) // this is getting stuck with integers. Because the data isn't truly categorical. trying ologit but now it is taking much longer.
// best practice for maxgap seems to be half of total time length. then maxitgap seems to be half of maxgap

// browse _mct_id _mct_t _mct_state _mct_last _mct_next _mct_lg _mct_tw _mct_initgap _mct_termgap _mct_igl _mct_tgl

browse couple_id weekly_hrs_t1_focal* _mct_iter

// let's trying removing cumulative duration. think for hours this is worse than if a true sequence state?
use "$created_data\individs_by_duration_wide.dta", clear

mict_prep weekly_hrs_t1_focal, id(couple_id)

capture program drop mict_model_gap
program mict_model_gap
mi impute regress _mct_state ///
_mct_next _mct_last, ///
add(1) force
end

capture program drop mict_model_initial
program mict_model_initial
mi impute regress _mct_state _mct_next, add(1) force
end

capture program drop mict_model_terminal
program mict_model_terminal
mi impute regress _mct_state _mct_last, add(1) force
end

mict_impute, maxgap(6) maxitgap(3) 

browse couple_id weekly_hrs_t1_focal* _mct_iter
*/