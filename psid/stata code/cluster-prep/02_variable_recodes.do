
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
* This files gets the individual level data from partners in the couples to
* recode and assign to focal person.


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
