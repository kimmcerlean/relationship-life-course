********************************************************************************
********************************************************************************
* Project: Marriage as a Gendering Institution
* Owner: Kimberly McErlean
* Started: September 2024
* File: individual_recodes
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* Trying alternative approach for the PSID that more closely aligns to UK / DE
* Instead of using head / ref as main variables, I want to create the FOCAL
* variables based on combo of individual / head / ref (in framework of life course paper)
* SO i actually recode for the whole sample, then I create and match partners.
* Not sure this will work but I am going to try it because it feels cleaner in some
* ways, especially in later years when some things asked first to individuals, then to head and wife
* Let's see if I will regret this LOLOLOL

********************************************************************************
* import data and clean up sample
********************************************************************************
use "$temp/PSID_full_long.dta", clear

sort unique_id survey_yr

replace SEQ_NUMBER_=0 if SEQ_NUMBER==. & RELATION_==0 //  1968 didn't have seq number so use another variable to get 0s for not in_sample
replace SEQ_NUMBER_=68 if survey_yr==1968 & SEQ_NUMBER==.
bysort id (SEQ_NUMBER_): egen ever_in_sample=max(SEQ_NUMBER_)
drop if ever_in_sample==0 // people with NO DATA in any year (I think there is actually not anyone)

browse unique_id main_per_id survey_yr SEQ_NUMBER_

gen year = survey_yr if SEQ_NUMBER_!=0

bysort unique_id (year): egen first_survey_yr = min(year)
bysort unique_id (year): egen last_survey_yr = max(year)

sort unique_id survey_yr
browse unique_id main_per_id survey_yr first_survey_yr last_survey_yr SEQ_NUMBER_ SAMPLE YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST PERMANENT_ATTRITION ANY_ATTRITION

keep if SEQ_NUMBER_!=0 | SAMPLE==1 // dropping non-sample years
drop if SEQ_NUMBER_==0 & survey_yr!=1968

tab first_survey_yr if SAMPLE==1
browse unique_id main_per_id survey_yr SEQ_NUMBER_ SAMPLE first_survey_yr last_survey_yr if SAMPLE==1 & first_survey_yr!=1968
replace first_survey_yr = 1968 if SAMPLE==1 & first_survey_yr==1969
drop if survey_yr==1968 & first_survey_yr!=1968

// want consecutive waves to make some things easier later
egen wave = group(survey_yr)

// indicator of PSID status
gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

// tmp save - move to other computer
// save "$created_data/PSID_long_all_recoded.dta", replace


********************************************************************************
********************************************************************************
**# RECODES
********************************************************************************
********************************************************************************
// use "$created_data/PSID_long_all_recoded.dta", clear

********************************************************************************
** Demographic and related variables
********************************************************************************
// before I go further, I think the INDIVIDUAL versions of variables have no missing? so need to update, wait maybe this is not true? this was true for life course...okay, I think the problem is, the 0s are NOT meaningful - some more like NA, but regardless, I need to be missing. I think the missing are when not asked and the 0s stand for both NO and N/A in many cases now that I dive in.
misstable summarize AGE_INDV_ YRS_EDUCATION_INDV_ ANNUAL_HOURS_T1_INDV_ TOTAL_INCOME_T1_INDV_ EMPLOYMENT_INDV_ STUDENT_T1_INDV_ BIRTH_YR_INDV_ LABOR_INCOME_T1_INDV_ NUM_JOBS_T1_INDV_ BACHELOR_YR_INDV_ STUDENT_CURRENT_INDV_ COLLEGE_INDV_ WEEKLY_HRS_T2_INDV_ LABOR_INCOME_T2_INDV_ HOUSEWORK_INDV_ WEEKS_WORKED_T2_INDV_ SR_HEALTH_INDV_ LIFE_SATISFACTION_INDV_, all showzeros

foreach var in AGE_INDV_ YRS_EDUCATION_INDV_ EMPLOYMENT_INDV_ STUDENT_T1_INDV_ NUM_JOBS_T1_INDV_ BACHELOR_YR_INDV_ STUDENT_CURRENT_INDV_ COLLEGE_INDV_ HOUSEWORK_INDV_ SR_HEALTH_INDV_ LIFE_SATISFACTION_INDV_{
	replace `var' = . if `var' == 0
}

// to check: ANNUAL_HOURS_T1_INDV_ TOTAL_INCOME_T1_INDV_ LABOR_INCOME_T1_INDV_ LABOR_INCOME_T2_INDV_
browse unique_id survey_yr  EMPLOYMENT_INDV_ ANNUAL_HOURS_T1_INDV_ TOTAL_INCOME_T1_INDV_ LABOR_INCOME_T1_INDV_ LABOR_INCOME_T2_INDV_ // let's come back to these as I go

// like some of these are hard bc 0 can be EITHER not worked OR n/a
fre WEEKS_WORKED_T2_INDV_  // this one I think fine
fre WEEKLY_HRS_T2_INDV_ // these match so I think okay

// age
tab BIRTH_YR_INDV_, m
replace BIRTH_YR_INDV_ = . if BIRTH_YR_INDV_==9999
replace AGE_INDV = . if AGE_INDV == 999

bysort unique_id: egen birth_yr = min(BIRTH_YR_INDV_)

replace birth_yr = survey_yr - AGE_INDV if birth_yr==. & AGE_INDV !=.

rename birth_yr birth_yr_helper
bysort unique_id: egen birth_yr = min(birth_yr_helper) // because of survey dates and ages, birth year can be a year off, use min
replace AGE_INDV_ = survey_yr - birth_yr if AGE_INDV_==. & birth_yr!=.

browse unique_id survey_yr birth_yr BIRTH_YR_INDV_ AGE_INDV

// sex
tab SEX, m
replace SEX=. if SEX==9

// who is who in HH (detailed info only collected for head / wife)
gen relationship = .
replace relationship = 1 if inlist(RELATION_,1,10) // head
replace relationship = 2 if inlist(RELATION_,2,20,22) // partner
replace relationship = 3 if !inlist(RELATION_,0,1,2,10,20,22)
replace relationship = . if RELATION_==0

label define relationship 1 "Head" 2 "Partner" 3 "Other"
label values relationship relationship

// sample info
gen in_sample = . // I might have removed non-sample actually
replace in_sample = 0 if SEQ_NUMBER_==0 | inrange(SEQ_NUMBER_,70,90)
replace in_sample = 1 if inrange(SEQ_NUMBER_,1,59)	| SEQ_NUMBER_==68
tab survey_yr in_sample, m

gen hh_status=.
replace hh_status=0 if SEQ_NUMBER_==0 
replace hh_status=1 if inrange(SEQ_NUMBER_,1,20) // in sample
replace hh_status=1 if SEQ_NUMBER_==68 // in sample, the 1968 people
replace hh_status=2 if inrange(SEQ_NUMBER_,51,59) // institutionalized
replace hh_status=3 if inrange(SEQ_NUMBER_,71,80) // new HH 
replace hh_status=4 if inrange(SEQ_NUMBER_,81,89) // died

label define hh_status 0 "not in sample" 1 "in sample" 2 "institutionalized" 3 "new hh" 4 "died"
label values hh_status hh_status

gen permanent_attrit=0
replace permanent_attrit=1 if PERMANENT_ATTRITION==1 // attrited
replace permanent_attrit=2 if inlist(PERMANENT_ATTRITION,2,3) // marked as died
label define perm 0 "no" 1 "attrited" 2 "died"
label values permanent_attrit perm

* respondent
replace RESPONDENT_WHO = 0 if RESPONDENT_WHO==. & in_sample==0
label define resp 0 "no sample" 1 "ref" 2 "spouse" 3 "partner" 4 "other hh member" 7 "proxy" 8 "wife, behalf of husband" 9 "non-survey year"
label values RESPONDENT_WHO resp

* get interview month
label values INTERVIEW_MONTH_HEAD_ INTERVIEW_DAY_HEAD_ .
tab survey_yr INTERVIEW_MONTH_HEAD_, m

inspect INTERVIEW_MONTH_HEAD_ if survey_yr >=1997
inspect INTERVIEW_DATE_HEAD_ if survey_yr < 1997
tab INTERVIEW_DATE_HEAD_ if inrange(survey_yr,1968,1979), m
tab INTERVIEW_DATE_HEAD_ if inrange(survey_yr,1980,1996), m

browse FAMILY_INTERVIEW_NUM_ unique_id survey_yr INTERVIEW_DATE_HEAD_ INTERVIEW_MONTH_HEAD_ INTERVIEW_DAY_HEAD_ INTERVIEW_YEAR_HEAD_

/* Interview date switched between years
// 1968-1979, it's Coded but the CODES CHANGE EVERY YEAR LOLOLOLOL
// 1980-1996: This four digit variable represents the month and day the interview was taken. The first two digits represent the month and the possible range is 03-10 (March-October) and the last two digits represent the day of the month which has a possible range of 01-31.

// then it switched to separate variables for month day year
*/


gen interview_month_created=.
replace interview_month_created = INTERVIEW_MONTH_HEAD_ if survey_yr >=1997
replace interview_month_created = 3 if survey_yr==1968 & inrange(INTERVIEW_DATE_HEAD_,1,2)
replace interview_month_created = 4 if survey_yr==1968 & inrange(INTERVIEW_DATE_HEAD_,3,4)
replace interview_month_created = 5 if survey_yr==1968 & inrange(INTERVIEW_DATE_HEAD_,5,6)
replace interview_month_created = 6 if survey_yr==1968 & inrange(INTERVIEW_DATE_HEAD_,7,8)
replace interview_month_created = 3 if survey_yr==1969 & inrange(INTERVIEW_DATE_HEAD_,1,3)
replace interview_month_created = 4 if survey_yr==1969 & inrange(INTERVIEW_DATE_HEAD_,4,6)
replace interview_month_created = 5 if survey_yr==1969 & inrange(INTERVIEW_DATE_HEAD_,7,8)
replace interview_month_created = 2 if survey_yr==1970 & INTERVIEW_DATE_HEAD_==1
replace interview_month_created = 3 if survey_yr==1970 & inrange(INTERVIEW_DATE_HEAD_,2,3)
replace interview_month_created = 4 if survey_yr==1970 & inrange(INTERVIEW_DATE_HEAD_,4,5)
replace interview_month_created = 5 if survey_yr==1970 & inrange(INTERVIEW_DATE_HEAD_,6,7)
replace interview_month_created = 6 if survey_yr==1970 & INTERVIEW_DATE_HEAD_==8
replace interview_month_created = 2 if inlist(survey_yr,1971,1972) & INTERVIEW_DATE_HEAD_==0
replace interview_month_created = 3 if inlist(survey_yr,1971,1972) & inrange(INTERVIEW_DATE_HEAD_,1,2)
replace interview_month_created = 4 if inlist(survey_yr,1971,1972) & inrange(INTERVIEW_DATE_HEAD_,3,4)
replace interview_month_created = 5 if inlist(survey_yr,1971,1972) & inrange(INTERVIEW_DATE_HEAD_,5,6)
replace interview_month_created = 6 if inlist(survey_yr,1971,1972) & INTERVIEW_DATE_HEAD_==7
replace interview_month_created = 7 if inlist(survey_yr,1971,1972) & INTERVIEW_DATE_HEAD_==8
replace interview_month_created = 3 if inrange(survey_yr,1973,1979) & inrange(INTERVIEW_DATE_HEAD_,1,2)
replace interview_month_created = 4 if inrange(survey_yr,1973,1979) & inrange(INTERVIEW_DATE_HEAD_,3,4)
replace interview_month_created = 5 if inrange(survey_yr,1973,1979) & inrange(INTERVIEW_DATE_HEAD_,5,6)
replace interview_month_created = 6 if inrange(survey_yr,1973,1979) & INTERVIEW_DATE_HEAD_==7
replace interview_month_created = 7 if inrange(survey_yr,1973,1979) & INTERVIEW_DATE_HEAD_==8
replace interview_month_created = 1 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,100,199) // this is prob not most efficient but whatever easier than substringing...
replace interview_month_created = 2 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,200,299)
replace interview_month_created = 3 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,300,399)
replace interview_month_created = 4 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,400,499)
replace interview_month_created = 5 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,500,599)
replace interview_month_created = 6 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,600,699)
replace interview_month_created = 7 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,700,799)
replace interview_month_created = 8 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,800,899)
replace interview_month_created = 9 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,900,999)
replace interview_month_created = 10 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,1000,1099)
replace interview_month_created = 11 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,1100,1199)
replace interview_month_created = 12 if inrange(survey_yr,1980,1996) & inrange(INTERVIEW_DATE_HEAD_,1200,1299)

tab interview_month_created, m
tab INTERVIEW_DATE_HEAD_ if interview_month_created==., m // 6 = wildcard in those years, 9 = NA/DK, 9999 = NA/DK/MAIL
tab INTERVIEW_MONTH_HEAD_ if interview_month_created==., m

tab INTERVIEW_MONTH_HEAD_ interview_month_created
tab INTERVIEW_DATE_HEAD_ interview_month_created

*********************************************
* Current marital status / relationship info
*********************************************
label define ER35107L 0 "n/a", modify

gen in_relationship=0
replace in_relationship=1 if inrange(MARITAL_PAIRS_,1,4)
replace in_relationship = . if in_sample==0 & MARITAL_PAIRS_==0

merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 partner_unique_id rel_num marr_num)
drop if _merge==2
tab in_relationship _merge, m
drop _merge

rename rel_num matrix_rel_num // just so I know where I got it
rename marr_num matrix_marr_num

tab MX8 in_relationship, m col // okay, yes, very few rows missing here
tab MX8 if in_relationship==1, m
inspect partner_unique_id if in_relationship==1 // okay so just the ones that are missing here (the 357) have a missing partner ID

// This is only head (so prob change the label). It's difficult to get this for focal people because they only ask last KNOWN marital status. I can ascertain based on partner / married status from the relationship list I merge on. I guess I can ascertain never married as well. widowed / divorced/ separated are a little harder becuase I need to use relationship history. I don't think this is that important right now, but just noting this...
gen cohab_est_head=0
replace cohab_est_head=1 if MARST_DEFACTO_HEAD_==1 & inlist(MARST_LEGAL_HEAD_,2,3,4,5) // will only apply after 1977
replace cohab_est_head=1 if MX8==22 & relationship==1
bysort FAMILY_INTERVIEW_NUM_ survey_yr: egen cohab_head_helper = max(cohab_est_head)
tab cohab_head_helper cohab_est_head,m

tab MX8 cohab_est_head if relationship==1
tab MX8 cohab_est_head if relationship==1 & survey_yr>=1977

tab MARST_DEFACTO_HEAD_ MX8 if relationship==1
tab MARST_DEFACTO_HEAD_ MX8 if relationship==1  & survey_yr>=1977

browse FAMILY_INTERVIEW_NUM_ unique_id survey_yr relationship MX8 cohab_est_head MARST_DEFACTO_HEAD_ MARST_LEGAL_HEAD_

gen marital_status_head =.
replace marital_status_head=1 if MARST_DEFACTO_HEAD_==1 & cohab_head_helper==0
replace marital_status_head=2 if MARST_DEFACTO_HEAD_==1 & cohab_head_helper==1
replace marital_status_head=3 if MARST_DEFACTO_HEAD_==2
replace marital_status_head=4 if MARST_DEFACTO_HEAD_==3
replace marital_status_head=5 if MARST_DEFACTO_HEAD_==4
replace marital_status_head=6 if MARST_DEFACTO_HEAD_==5

label define marital_status_updated 1 "Married (or pre77)" 2 "Partnered" 3 "Never Married" 4 "Widowed" 5 "Divorced" 6 "Separated"
label values marital_status_head marital_status_updated

tab marital_status_head MX8 if in_relationship==1, m
tab marital_status_head MX8 if relationship==1 & in_relationship==1, m

// browse unique_id survey_yr MX8 marital_status_indv MARITAL_STATUS FIRST_MARRIAGE_YR_START
gen marital_status_indv=.
replace marital_status_indv = marital_status_head if relationship==1
replace marital_status_indv = 1 if marital_status_indv==. & MX8 == 20
replace marital_status_indv = 2 if marital_status_indv==. & MX8 == 22
replace marital_status_indv = 3 if marital_status_indv==. & MARITAL_STATUS == 2 // if LAST KNOWN marital status is never married and not currently partnered, then we can assume never married at survey year (because can't switch from divorced back). this is probably only case where this is true.
replace marital_status_indv = 3 if marital_status_indv==. & survey_yr < FIRST_MARRIAGE_YR_START & FIRST_MARRIAGE_YR_START!=. & FIRST_MARRIAGE_YR_START!=9999 // if not currently partnered and before first marriage year (if have a first marriage year), can also assume never married
label values marital_status_indv marital_status_updated

browse unique_id survey_yr MX8 marital_status_indv MARITAL_STATUS FIRST_MARRIAGE_YR_START
tab marital_status_indv, m // the problem is many of these are because not in marital history either, so actaulyl will be hard to recover true marital status, but I don't need that anyway at this moment because a. i remove unpartnered and b. i use other relationship history indicators to get the info I need, so this is fine.

tab marital_status_indv MX8 if in_relationship==1, m

*********************************************
* Education
*********************************************
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
recode YRS_EDUCATION_INDV (1/11=1) (12=2) (13/15=3) (16/17=4) (98/99=.)(0=.), gen(educ_completed) // okay this is hard to use because head / wife ONLY recorded against those specific ones so they don't always have values here

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ_completed educ

browse unique_id survey_yr in_sample relationship YRS_EDUCATION_INDV educ_completed educ_head_early educ_head_1975 hs_head HS_GRAD_HEAD attended_college_head completed_college_head college_degree_head BACHELOR_YR_HEAD_ YR_EDUC_UPD_HEAD_ NEW_HEAD_ NEW_HEAD_YEAR if relationship==1 // using head right now to wrap my head around

* create final education variables - these are better
gen educ_head=.
replace educ_head=1 if hs_head==0
replace educ_head=2 if hs_head==1 & attended_college_head==0
replace educ_head=3 if hs_head==1 & attended_college_head==1 & completed_college_head==0
replace educ_head=3 if completed_college_head==1 & college_degree_head==1
replace educ_head=4 if completed_college_head==1 & college_degree_head==2

gen educ_head_est=. // this can help fill in some missing info
replace educ_head_est=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head_est=educ_head_1975 if inrange(survey_yr,1991,2023)

tab educ_head educ_head_est, m
tab educ_completed educ_head if relationship==1, m
tab educ_head_est educ_completed if educ_head==., m
replace educ_head = educ_completed if educ_head==. & educ_completed!=.
replace educ_head = educ_head_est if educ_head==. & educ_head_est!=.

browse unique_id survey_yr educ_head educ_head_est educ_completed YRS_EDUCATION_INDV  hs_head attended_college_head completed_college_head college_degree_head if relationship==1 

gen educ_wife=.
replace educ_wife=1 if hs_wife==0
replace educ_wife=2 if hs_wife==1 & attended_college_wife==0
replace educ_wife=3 if hs_wife==1 & attended_college_wife==1 & completed_college_wife==0
replace educ_wife=3 if completed_college_wife==1 & college_degree_wife==1
replace educ_wife=4 if completed_college_wife==1 & college_degree_wife==2

gen educ_wife_est=.
replace educ_wife_est=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife_est=educ_wife_1975 if inrange(survey_yr,1991,2023)
tab survey_yr educ_wife_est, m 

replace educ_wife = educ_completed if educ_wife==. & educ_completed!=.
replace educ_wife = educ_wife_est if educ_wife==. & educ_wife_est!=.

tab educ_wife educ_wife_est, m
tab educ_completed educ_wife if relationship==2, m
tab educ_wife_est educ_completed if educ_wife==., m

label values educ_head educ_wife educ_head_est educ_wife_est educ

gen college_wife=.
replace college_wife=0 if inrange(educ_wife,1,3)
replace college_wife=1 if educ_wife==4

gen college_head=.
replace college_head=0 if inrange(educ_head,1,3)
replace college_head=1 if educ_head==4
tab college_degree_head college_head, m

gen college_indv=.
replace college_indv=0 if inrange(educ_completed,1,3)
replace college_indv=1 if educ_completed==4

/*
* Do I want an indicator of school enrollment? (need to see if individual one too)
gen enrolled_head = .
replace enrolled_head = 0 if ENROLLED_HEAD_==5
replace enrolled_head = 1 if ENROLLED_HEAD_==1

gen enrolled_wife = .
replace enrolled_wife = 0 if ENROLLED_WIFE_==5
replace enrolled_wife = 1 if ENROLLED_WIFE_==1

// for individuals, it was STUDENT_T1_INDV from 1979-2009 and covered prior year. Then CURRENT is STUDENT_CURRENT_INDV, from 2013-2021
*/

*********************************************
* Race, ethnicity, immigrant status, related
*********************************************
browse unique_id survey_yr RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_
// wait race of wife not asked until 1985?! that's wild. also need to see if codes changed in between. try to fill in historical for wife if in survey in 1985 and prior.
/*
1968-1984: 1=White; 2=Negro; 3=PR or Mexican; 7=Other
1985-1989: 1=White; 2=Black; 3=Am Indian 4=Asian 7=Other; 8 =more than 2
1990-2003: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Latino; 6=Other; 7=Other
2005-2023: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Native Hawaiian/Pac Is; 7=Other

From SHELF:
Both summary measures were based on majority response across all available waves (with a small number of ties being broken by most recent report). 
Also good history on how the PSID collected race and when actually asked v. carried forward
so maybe rely on when asked and more recent, when measures more robust and self-reported (aka not by interviewer)
*/

gen race_1_head_rec=.
replace race_1_head_rec=1 if RACE_1_HEAD_==1
replace race_1_head_rec=2 if RACE_1_HEAD_==2
replace race_1_head_rec=3 if (inrange(survey_yr,1985,2023) & RACE_1_HEAD_==3)
replace race_1_head_rec=4 if (inrange(survey_yr,1985,2023) & RACE_1_HEAD_==4)
replace race_1_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==5)
replace race_1_head_rec=6 if RACE_1_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==6) | (inrange(survey_yr,2005,2023) & RACE_1_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_1_HEAD_==8)

gen race_2_head_rec=.
replace race_2_head_rec=1 if RACE_2_HEAD_==1
replace race_2_head_rec=2 if RACE_2_HEAD_==2
replace race_2_head_rec=3 if (inrange(survey_yr,1985,2023) & RACE_2_HEAD_==3)
replace race_2_head_rec=4 if (inrange(survey_yr,1985,2023) & RACE_2_HEAD_==4)
replace race_2_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==5)
replace race_2_head_rec=6 if RACE_2_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==6) | (inrange(survey_yr,2005,2023) & RACE_2_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_2_HEAD_==8)

gen race_3_head_rec=.
replace race_3_head_rec=1 if RACE_3_HEAD_==1
replace race_3_head_rec=2 if RACE_3_HEAD_==2
replace race_3_head_rec=3 if (inrange(survey_yr,1985,2023) & RACE_3_HEAD_==3)
replace race_3_head_rec=4 if (inrange(survey_yr,1985,2023) & RACE_3_HEAD_==4)
replace race_3_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==5)
replace race_3_head_rec=6 if RACE_3_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==6) | (inrange(survey_yr,2005,2023) & RACE_3_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_3_HEAD_==8)

gen race_4_head_rec=.
replace race_4_head_rec=1 if RACE_4_HEAD_==1
replace race_4_head_rec=2 if RACE_4_HEAD_==2
replace race_4_head_rec=3 if (inrange(survey_yr,1985,2023) & RACE_4_HEAD_==3)
replace race_4_head_rec=4 if (inrange(survey_yr,1985,2023) & RACE_4_HEAD_==4)
replace race_4_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==5)
replace race_4_head_rec=6 if RACE_4_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==6) | (inrange(survey_yr,2005,2023) & RACE_4_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_4_HEAD_==8)

gen race_1_wife_rec=.
replace race_1_wife_rec=1 if RACE_1_WIFE_==1
replace race_1_wife_rec=2 if RACE_1_WIFE_==2
replace race_1_wife_rec=3 if (inrange(survey_yr,1985,2023) & RACE_1_WIFE_==3)
replace race_1_wife_rec=4 if (inrange(survey_yr,1985,2023) & RACE_1_WIFE_==4)
replace race_1_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==5)
replace race_1_wife_rec=6 if RACE_1_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==6) | (inrange(survey_yr,2005,2023) & RACE_1_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_1_WIFE_==8)

gen race_2_wife_rec=.
replace race_2_wife_rec=1 if RACE_2_WIFE_==1
replace race_2_wife_rec=2 if RACE_2_WIFE_==2
replace race_2_wife_rec=3 if (inrange(survey_yr,1985,2023) & RACE_2_WIFE_==3)
replace race_2_wife_rec=4 if (inrange(survey_yr,1985,2023) & RACE_2_WIFE_==4)
replace race_2_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==5)
replace race_2_wife_rec=6 if RACE_2_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==6) | (inrange(survey_yr,2005,2023) & RACE_2_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_2_WIFE_==8)

gen race_3_wife_rec=.
replace race_3_wife_rec=1 if RACE_3_WIFE_==1
replace race_3_wife_rec=2 if RACE_3_WIFE_==2
replace race_3_wife_rec=3 if (inrange(survey_yr,1985,2023) & RACE_3_WIFE_==3)
replace race_3_wife_rec=4 if (inrange(survey_yr,1985,2023) & RACE_3_WIFE_==4)
replace race_3_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==5)
replace race_3_wife_rec=6 if RACE_3_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==6) | (inrange(survey_yr,2005,2023) & RACE_3_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_3_WIFE_==8)

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

// combined (in years where hispanicity not asked, I just assume not)
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

// can only make fixed AFTER I adjust to focal person (so I know they didn't change roles over time)

// add info on born in US - have to get from many variables I think...
label values STATE_BORN_HEAD_ STATE_BORN_WIFE_ .
tabstat BORN_US_HEAD_ WHEN_ARRIVE_US_HEAD_  REGION_GREW_UP_HEAD_ STATE_BORN_HEAD_ BORN_US_WIFE_ WHEN_ARRIVE_US_WIFE_ REGION_GREW_UP_WIFE_  STATE_BORN_WIFE_, by(survey_yr)

browse unique_id survey_yr relationship BORN_US_HEAD_ WHEN_ARRIVE_US_HEAD_  REGION_GREW_UP_HEAD_ STATE_BORN_HEAD_ BORN_US_WIFE_ WHEN_ARRIVE_US_WIFE_ REGION_GREW_UP_WIFE_  STATE_BORN_WIFE_

// so region grew up is most comprehensive. Not sure if it's perfect because not BORN (they say "About ages 6-16"). so ideally would prioritize that, but not asked until 2013. alternative is also just use indicator of immigrant sample

tab STATE_BORN_HEAD_ REGION_GREW_UP_HEAD_ // are they congruent when both exist?

tab sample_type, m
tab sample_type BORN_US_HEAD_ if relationship==1, m
tab sample_type REGION_GREW_UP_HEAD_ if relationship==1, m // this is definitely NOT congruous. like it is for the immigrant samples but there is a decent overlap with main sample and growing up in foreign country. not really in ROW view but in columb view

gen born_in_us_head = .
replace born_in_us_head = 0 if STATE_BORN_HEAD_==0 // prio these, use region as backup
replace born_in_us_head = 1 if inrange(STATE_BORN_HEAD_,1,60)
replace born_in_us_head = 0 if born_in_us_head==. & REGION_GREW_UP_HEAD_==6 
replace born_in_us_head = 1 if born_in_us_head==. & inrange(REGION_GREW_UP_HEAD_,1,5)
replace born_in_us_head = 0 if born_in_us_head==. & BORN_US_HEAD_==1
replace born_in_us_head = 1 if born_in_us_head==. & BORN_US_HEAD_==5

gen born_in_us_wife = .
replace born_in_us_wife = 0 if STATE_BORN_WIFE_==0 // prio these, use region as backup
replace born_in_us_wife = 1 if inrange(STATE_BORN_WIFE_,1,60)
replace born_in_us_wife = 0 if born_in_us_wife==. & REGION_GREW_UP_WIFE_==6 
replace born_in_us_wife = 1 if born_in_us_wife==. & inrange(REGION_GREW_UP_WIFE_,1,5)
replace born_in_us_wife = 0 if born_in_us_wife==. & BORN_US_WIFE_==1
replace born_in_us_wife = 1 if born_in_us_wife==. & BORN_US_WIFE_==5
// wife not asked as consistently for region born so when allocated to focal, can try to fill in missing (bc should, theoretically, be fixed)


*********************************************
* Current residence info
*********************************************
// region
recode REGION_ (0=.) (9=.)

// urban / rural
tab METRO_, m // 1 =metro (beale 1-3) 2= non-metro (beale 4-9)
tab URBAN_RURAL_BEALE_, m

gen urban_rural = .
replace urban_rural = 0 if METRO_==2 // rural
replace urban_rural = 0 if inrange(URBAN_RURAL_BEALE_,4,9)
replace urban_rural = 1 if METRO_==1 // urban
replace urban_rural = 1 if inrange(URBAN_RURAL_BEALE_,1,3)
replace urban_rural = 2 if METRO_==0 // foreign
replace urban_rural = 2 if URBAN_RURAL_BEALE_==0 // foreign

label define urban 0 "rural" 1 "urban" 2 "foreign"
label values urban_rural urban

// homeownership
replace HOUSE_STATUS = . if HOUSE_STATUS==9 | HOUSE_STATUS==0 

gen home_owner=.
replace home_owner = 0 if inlist(HOUSE_STATUS,5,8)
replace home_owner = 1 if HOUSE_STATUS_==1

********************************************************************************
**# DoL variables
********************************************************************************

*********************************************
* Employment
*********************************************
browse unique_id survey_yr relationship EMPLOYMENT_INDV_ EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// 1968-1975: 1 "working now" 2 "unemployed" 3 "retired / disabled" 4 "housewife" 5 "student" 6 "other"
// 1976+: 1 "working now" 2 "temp laid off" 3 "unemployed" 4 "retired" 5 "disabled" 6 "housewife" 7 "student" 8 "other" // since I restricted time, this is fine
// wife not asked until 1976?
// tabstat EMPLOYMENT_INDV_, by(survey_yr) // asked since 1979. need to figure out if asked of head and wife

* First, try to make one comprehensive detailed employment status and clean up existing variables
foreach var in EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_{
	recode `var'(0=.)(98/99=.)
}

egen num_emp_status_head=rownonmiss(EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ )
browse unique_id survey_yr  num_emp_status_head EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_
tab EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ if num_emp_status_head ==2

gen employment_status_head = . // okay just going to call this primary employment status actaully
replace employment_status_head = EMPLOY_STATUS_HEAD_ if inrange(survey_yr,1976,1993)
replace employment_status_head = EMPLOY_STATUS1_HEAD_ if inrange(survey_yr,1994,2023) //  & inlist(num_emp_status_head,0,1)
replace employment_status_head = 1 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_HEAD_==1
replace employment_status_head = 3 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_HEAD_==2
replace employment_status_head = 4 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_HEAD_==3 // this was retired / disabled, let's just put in retired; i'm dropping these years anyway
replace employment_status_head = 6 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_HEAD_==4
replace employment_status_head = 7 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_HEAD_==5
replace employment_status_head = 8 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_HEAD_==6
recode employment_status_head (22/99=.)(9=.)

gen employment_status_wife = . // okay just going to call this primary employment status actaully
replace employment_status_wife = EMPLOY_STATUS_WIFE_ if inrange(survey_yr,1976,1993)
replace employment_status_wife = EMPLOY_STATUS1_WIFE_ if inrange(survey_yr,1994,2023)
replace employment_status_wife = 1 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_WIFE_==1
replace employment_status_wife = 3 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_WIFE_==2
replace employment_status_wife = 4 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_WIFE_==3 // this was retired / disabled, let's just put in retired; i'm dropping these years anyway
replace employment_status_wife = 6 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_WIFE_==4
replace employment_status_wife = 7 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_WIFE_==5
replace employment_status_wife = 8 if inrange(survey_yr,1968,1975) & EMPLOY_STATUS_WIFE_==6
recode employment_status_wife (9/99=.)
replace employment_status_wife = . if employment_status_wife==0

label define employment_status 1 "working now" 2 "temp laid off" 3 "unemployed" 4 "retired" 5 "disabled" 6 "housewife" 7 "student" 8 "other"
label values employment_status_head employment_status_wife employment_status

replace EMPLOYMENT_INDV_ = . if EMPLOYMENT_INDV_==9 // otherwise, this coding matches above
replace EMPLOYMENT_INDV_ = . if EMPLOYMENT_INDV_==0

* Then just binary y/n employment
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

tab employment_status_head employed_head, m // some of this mismatch is because binary based on ALL THREE, other is PRIMARY work status

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

gen employed_indv=.
replace employed_indv=0 if inrange(EMPLOYMENT_INDV,2,9)
replace employed_indv=1 if EMPLOYMENT_INDV==1

*********************************************
* Hours
*********************************************
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
// not sure if 0s are TRUE or missing. I think the missing are when not asked and the 0s stand for both NO and N/A
browse unique_id survey_yr in_sample hh_status AGE_INDV_ EMPLOYMENT_INDV TYPE_TAXABLE_INCOME_ ANNUAL_HOURS_T1_INDV // so the "not working" says it is type_taxable_income 0 or 2, so let's set to missing if type_taxable_income is? but then some of type taxable income is bc none (ad I think those should stay 0s). but those 0s also combine NO income and N/A. and this doesn't work bc missing type taxable is bc not asked
// okay I actually think AGE might explain best and then those should stay 0s. let's leave

gen weekly_hrs_t1_indv = round(ANNUAL_HOURS_T1_INDV / 52,1) if ANNUAL_HOURS_T1_INDV!=9999
browse unique_id survey_yr relationship weekly_hrs_t1_indv weekly_hrs_t1_head weekly_hrs_t1_wife ANNUAL_HOURS_T1_INDV

/// adjust to time t (doing here, but this means I create focal here)
sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr relationship weekly_hrs_t1_wife WEEKLY_HRS_T2_WIFE_ weekly_hrs_t1_head WEEKLY_HRS_T2_HEAD_   WEEKLY_HRS_T2_INDV_ if survey_yr>1995

tabstat(WEEKLY_HRS_T2_INDV_ WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_), by(survey_yr)

gen weekly_hrs_t2_focal=.
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_INDV_ if inlist(survey_yr,1999,2001)
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_HEAD_ if relationship==1 & inrange(survey_yr,2003,2023)
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_WIFE_ if relationship==2 & inrange(survey_yr,2003,2023)
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_INDV if relationship==3 & inrange(survey_yr,2003,2023)

browse unique_id survey_yr relationship weekly_hrs_t2_focal WEEKLY_HRS_T2_HEAD WEEKLY_HRS_T2_WIFE WEEKLY_HRS_T2_INDV

gen weekly_hrs_t1_focal=.
replace weekly_hrs_t1_focal = weekly_hrs_t1_head if relationship==1
replace weekly_hrs_t1_focal = weekly_hrs_t1_wife if relationship==2
replace weekly_hrs_t1_focal = weekly_hrs_t1_indv if relationship==3

inspect weekly_hrs_t1_focal weekly_hrs_t2_focal if inlist(relationship,1,2)

browse unique_id partner_unique_id survey_yr relationship weekly_hrs_t1_focal weekly_hrs_t2_focal weekly_hrs_t1_wife WEEKLY_HRS_T2_WIFE_ weekly_hrs_t1_head WEEKLY_HRS_T2_HEAD_   WEEKLY_HRS_T2_INDV_ if survey_yr>1995

sort unique_id survey_yr
gen weekly_hrs_t_focal = .
replace weekly_hrs_t_focal = weekly_hrs_t1_focal[_n+1] if inrange(survey_yr,1968,1996) & wave==wave[_n+1]-1 & unique_id==unique_id[_n+1]
replace weekly_hrs_t_focal = weekly_hrs_t2_focal[_n+1] if inrange(survey_yr,1997,2021) & wave==wave[_n+1]-1 & unique_id==unique_id[_n+1]

browse unique_id partner_unique_id FAMILY_INTERVIEW_NUM_ survey_yr relationship weekly_hrs_t_focal weekly_hrs_t1_focal weekly_hrs_t2_focal

// for posterity but actually don't need this because prioritizing the focal versions anyway
 gen weekly_hrs_t_head = weekly_hrs_t_focal if relationship==1
bysort FAMILY_INTERVIEW_NUM_ survey_yr (weekly_hrs_t_head): replace weekly_hrs_t_head = weekly_hrs_t_head[1] 
gen weekly_hrs_t_wife = weekly_hrs_t_focal if relationship==2
bysort FAMILY_INTERVIEW_NUM_ survey_yr (weekly_hrs_t_wife): replace weekly_hrs_t_wife = weekly_hrs_t_wife[1] 

	// sort FAMILY_INTERVIEW_NUM_ survey_yr
	// browse unique_id partner_unique_id FAMILY_INTERVIEW_NUM_ survey_yr relation weekly_hrs_t_focal weekly_hrs_t_head weekly_hrs_t_wife

// validate against CURRENT employment status
replace weekly_hrs_t_focal = . if weekly_hrs_t_focal==999 | weekly_hrs_t_focal==998
replace weekly_hrs_t_head = . if weekly_hrs_t_head==999 | weekly_hrs_t_head==998
replace weekly_hrs_t_wife = . if weekly_hrs_t_wife==999 | weekly_hrs_t_wife==998

tab weekly_hrs_t_focal employed_indv, m

tab weekly_hrs_t_head employment_status_head, m
tab weekly_hrs_t_head employed_head, m
tabstat weekly_hrs_t_head, by(employment_status_head)

*********************************************
* Earnings and income
*********************************************
browse unique_id survey_yr AGE_INDV_ FAMILY_INTERVIEW_NUM_ TAXABLE_T1_HEAD_WIFE TOTAL_INCOME_T1_FAMILY LABOR_INCOME_T1_HEAD WAGES_ALT_T1_HEAD WAGES_T1_HEAD LABOR_INCOME_T2_HEAD LABOR_INCOME_T1_WIFE_ WAGES_T1_WIFE_  LABOR_INCOME_T2_WIFE_ LABOR_INCOME_T1_INDV_ LABOR_INCOME_T2_INDV_ 

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

sum LABOR_INCOME_T1_INDV, det

/// adjust to time t (following hours code above)
browse unique_id survey_yr relationship LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ LABOR_INCOME_T2_INDV_  TOTAL_INCOME_T2_FAMILY_

gen long earnings_t2_focal=.
replace earnings_t2_focal=LABOR_INCOME_T2_INDV_ if inrange(survey_yr,1999,2001)
replace earnings_t2_focal=LABOR_INCOME_T2_HEAD_ if relationship==1 & inrange(survey_yr,2003,2023)
replace earnings_t2_focal=LABOR_INCOME_T2_WIFE_ if relationship==2 & inrange(survey_yr,2003,2023)
replace earnings_t2_focal=LABOR_INCOME_T2_INDV_ if relationship==3 & inrange(survey_yr,2003,2023)
replace earnings_t2_focal=. if earnings_t2_focal==9999999 | earnings_t2_focal==99999999 | earnings_t2_focal== 99999998
browse unique_id survey_yr relationship earnings_t2_focal LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ LABOR_INCOME_T2_INDV_

gen long earnings_t1_focal=.
replace earnings_t1_focal=earnings_t1_head if relationship==1
replace earnings_t1_focal=earnings_t1_wife if relationship==2
replace earnings_t1_focal=LABOR_INCOME_T1_INDV if relationship==3

inspect earnings_t1_focal earnings_t2_focal if inlist(relationship,1,2)

sort unique_id survey_yr
gen long earnings_t_focal = .
replace earnings_t_focal = earnings_t1_focal[_n+1] if inrange(survey_yr,1968,1996) & wave==wave[_n+1]-1 & unique_id==unique_id[_n+1]
replace earnings_t_focal = earnings_t2_focal[_n+1] if inrange(survey_yr,1997,2021) & wave==wave[_n+1]-1 & unique_id==unique_id[_n+1]

browse unique_id survey_yr relationship earnings_t_focal earnings_t2_focal earnings_t1_focal LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ LABOR_INCOME_T2_INDV_  TOTAL_INCOME_T2_FAMILY_ if survey_yr>1995

// what about HH income (check what I use for life course, where do I clean that up). i align this later on.
tabstat TOTAL_INCOME_T1_FAMILY TOTAL_INCOME_T2_FAMILY, by(survey_yr) // so t2 income only asked 1999-2003. so, if I need to adjust, just use t-1? I don't see an individual level total t-2 that I could use to aggregate across HH members either...

sum TOTAL_INCOME_T1_FAMILY, det

*********************************************
* Unpaid labor variables
*********************************************
// housework hours - not totally sure if accurate prior to 1976 (asked annually not weekly). missing head/wife specific in 1968, 1975, 1982

label values HOUSEWORK_INDV_ .
browse unique_id survey_yr relationship HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_ TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1 // total and most HW stopped after 1974, inividual stopped 1986

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
replace housework_head = HOUSEWORK_INDV_ if relationship==1 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_head=. if inlist(housework_head,998,999)

gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)
replace housework_wife = HOUSEWORK_INDV_ if relationship==2 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_wife=. if inlist(housework_wife,998,999)

gen total_housework_weekly = TOTAL_HOUSEWORK_T1_HW / 52

browse unique_id survey_yr relationship housework_head housework_wife HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_

// not sure enough info here but I have pulled in
replace CHILDCARE_HEAD = . if inlist(CHILDCARE_HEAD,998,999)
replace CHILDCARE_WIFE = . if inlist(CHILDCARE_WIFE,998,999)
replace ADULTCARE_HEAD = . if inlist(ADULTCARE_HEAD,998,999)
replace ADULTCARE_WIFE = . if inlist(ADULTCARE_WIFE,998,999)

*********************************************
* HH / child variables
*********************************************
// merge on some necessary files
merge m:1 unique_id using "$created_data/birth_history_wide.dta"
drop if _merge==2
tab NUM_BIRTHS _merge , m // so all of the non-matches are those where this variable SAYS no birth history
drop _merge

merge m:1 FAMILY_INTERVIEW_NUM_ survey_yr using "$temp/hh_comp_lookup.dta"
drop _merge // all matched

// child info
gen children=.
replace children=0 if NUM_CHILDREN_==0
replace children=1 if NUM_CHILDREN_>=1 & NUM_CHILDREN_!=.

tab NUM_CHILDREN_ kidsu18_hh, m // these both cover 0-17 BUT the one is FAMILY maybe not HH? (see the below on HH size too)

tab kidsu6_hh, m

bysort unique_id: egen children_ever = max(NUM_CHILDREN_) // but this is OBSERVED in HH, not nec. births
replace children_ever=1 if children_ever>0

tab NUM_BIRTHS cah_any_births, m
tab NUM_BIRTHS cah_num_own_children, m // these are the same

gen ever_birth = .
replace ever_birth = 0 if NUM_BIRTHS==0
replace ever_birth = 1 if inrange(NUM_BIRTHS,1,20)

tab FIRST_BIRTH_YR, m
tab FIRST_BIRTH_YR ever_birth, m

	// browse unique_id survey_yr ever_birth FIRST_BIRTH_YR cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3
	// tab cah_child_birth_yr1 if FIRST_BIRTH_YR==9999, m

gen year_first_birth = FIRST_BIRTH_YR if ever_birth == 1 & FIRST_BIRTH_YR!=9999
replace year_first_birth = cah_child_birth_yr1 if ever_birth==1 & year_first_birth==. &  cah_child_birth_yr1 < 9000
replace year_first_birth = 9999 if ever_birth==0

tab year_first_birth ever_birth, m

sort unique_id survey_yr
	// browse unique_id survey_yr wave ever_birth NUM_CHILDREN_  NUM_BIRTHS year_first_birth
gen when_first_birth=.
replace when_first_birth = survey_yr if NUM_BIRTHS==99 & NUM_CHILDREN_ > 0 & NUM_CHILDREN_[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
bysort unique_id: egen first_birth_check = min(when_first_birth)

gen first_birth_est = .
replace first_birth_est = 1 if year_first_birth==. & ever_birth==1
replace year_first_birth = first_birth_check if ever_birth==1 & year_first_birth==. & first_birth_check!=. // okay this actually didn't fill anyone lol

// I think birth history has too many variables to leave on here for now, drop all except select few for now
drop cah_parent_sex cah_parent_birth_yr cah_parent_birth_mon cah_child_int_number* cah_child_per_num* cah_event_type* cah_real_birth* cah_parent_marst* cah_num_children* cah_child_sex* cah_child_birth_mon* cah_child_hispanicity* cah_child_race1* cah_child_race2* cah_child_race3* cah_mom_wanted* cah_mom_timing* cah_dad_wanted* cah_dad_timing* cah_birth_order*

// validate these against any existing (e.g. age youngest/ oldest etc.)
replace AGE_YOUNG_CHILD_ = . if AGE_YOUNG_CHILD_ == 0 // newborns stupidly coded as 1 (up to 2nd birthday)
replace AGE_OLDEST_CHILD_ = . if AGE_OLDEST_CHILD_ == 0
label values AGE_OLDEST_CHILD_ NUM_CHILDREN_ .

tabstat age_youngest_child AGE_YOUNG_CHILD_ age_oldest_child AGE_OLDEST_CHILD_ // one thing is I do youngest and oldest up to 17, for oldest, they can be older than 17
tabstat age_oldest_child AGE_OLDEST_CHILD_ if AGE_OLDEST_CHILD_ < 18
browse unique_id survey_yr NUM_CHILDREN_ kidsu18_hh age_youngest_child AGE_YOUNG_CHILD_ age_oldest_child AGE_OLDEST_CHILD_

// total HH size
label values NUM_IN_HH_ NUM_NONFU_IN_HH_ . 
tab NUM_IN_HH_, m
tab NUM_NONFU_IN_HH_, m
replace NUM_NONFU_IN_HH_ = . if NUM_NONFU_IN_HH_==99
tab all_in_hh, m

egen NUM_IN_HH = rowtotal(NUM_IN_HH_ NUM_NONFU_IN_HH_), missing
tab NUM_IN_HH, m
tab NUM_IN_HH all_in_hh, m
tab NUM_IN_HH all_in_hh if NUM_NONFU_IN_HH_!=., m

browse unique_id survey_yr all_in_hh NUM_IN_HH NUM_NONFU_IN_HH_ NUM_IN_HH_

// num of people 65+
tab num_65up_hh, m

********************************************************************************
* Miscellaneous variables
********************************************************************************

*********************************************
* Religion
*********************************************
/* head was 1970-1977, 1979-2021. wife was 1976, 1985-2021
Okay, but some weird things with how asked: 
In 1979, when this question was reinstated in the questionnaire, values were not brought forward for families with unchanged Heads since 1977.
For those cases with the same Heads from 1977 through the present, please use 1977 religious preference, V5617
So, most missings after 1977 can be interpreted as no new head, so use 1977 value? Is this another that might help if I edit once I have the variables assigned to the focal person?
Okay, but I *think* starting in 1985, was asked to everyone again? Because number of 0s goes down and the note is gone. okay, carried forward again starting 1986.
So carry through 1977-1984 if in sample and same head / partner?

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
2019-2021 - 0: Inapp (no partner), 1: None, 2: Atheist, 3: Agnostic, 4: Roman Catholic, 5: Greek Orthodox, 6: Baptist, 7: Episcopalian, 8: Jehovah's Witness, 9: Lutheran, 10: Methodist, 11: Pentecostal, 12: Presbyterian, 13: Protestant unspecified, 14: Christian, unspecified, 15: Christian, non-denominational, 16: Jewish, 17: Muslim, 18: Buddhist, 19: Other non-christian, 20: Other protestant, 21: LDS, 22: Unitarian, 23: Christian Science, 24: Adventist, 25: Amish, 26: Quaker, 27: Church of God, 28: United Church of Christ, 29: Reformed, 30: Disciples of Christ, 31: Churches of Christ, 97: Other, 98: DK, 99: NA
-- lol so DENOMINATION ends in 2017 and is integrated BACK to this question lord and the codes change AGAIN.

Denomination
1994-2017 - 0: None, atheist, agnostic, not Protestant OR no spouse (this is a lot in one), 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 11: LDS, 12: Jehovah's witness, 14: Christian, 15: Unitarian, 16: Christian Science, 17: Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 98: DK, 99: NA
-- so, I think aligns with how asked 1985-1993. I think if I combine the two I actually get all the same codes 0-25 (that's why some are missing)
*/

tabstat RELIGION_WIFE_ RELIGION_HEAD_, by(survey_yr) // just to get a sense of when asked to start.
label values RELIGION_WIFE_ RELIGION_HEAD_ . // these values are v wrong

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

tab religion_wife, m // one problem here is that this is also ZERO if no wife, but think this will get fixed once I assign to focal
tab RELIGION_WIFE_ religion_wife, m 

*********************************************
* Life status variables
*********************************************
// disability status
tab DISABILITY_HEAD, m
tab DISABLE_HOWMUCH_HEAD, m

tab DISABILITY_HEAD DISABLE_HOWMUCH_HEAD, m

gen disabled_head=.
replace disabled_head=0 if DISABILITY_HEAD==5
replace disabled_head=1 if DISABILITY_HEAD==1

gen disabled_scale_head=.
replace disabled_scale_head=0 if disabled_head==0 // not disabled
replace disabled_scale_head=0 if DISABILITY_HEAD==1 & DISABLE_HOWMUCH_HEAD==7 // not at all - so putting as 0 
replace disabled_scale_head=1 if DISABILITY_HEAD==1 & inlist(DISABLE_HOWMUCH_HEAD,4,5) // just a little
replace disabled_scale_head=2 if DISABILITY_HEAD==1 & DISABLE_HOWMUCH_HEAD==3 // somewhat
replace disabled_scale_head=3 if DISABILITY_HEAD==1 & inlist(DISABLE_HOWMUCH_HEAD,1,2) // a lot

gen disabled_wife=.
replace disabled_wife=0 if DISABILITY_WIFE==5
replace disabled_wife=1 if DISABILITY_WIFE==1

gen disabled_scale_wife=.
replace disabled_scale_wife=0 if DISABILITY_WIFE==5 // not disabled
replace disabled_scale_wife=0 if DISABILITY_WIFE==1 & DISABLE_HOWMUCH_WIFE==7 // not at all - so putting as 0 
replace disabled_scale_wife=1 if DISABILITY_WIFE==1 & inlist(DISABLE_HOWMUCH_WIFE,4,5) // just a little
replace disabled_scale_wife=2 if DISABILITY_WIFE==1 & DISABLE_HOWMUCH_WIFE==3 // somewhat
replace disabled_scale_wife=3 if DISABILITY_WIFE==1 & inlist(DISABLE_HOWMUCH_WIFE,1,2) // a lot

label define dis_scale 0 "Not at all" 1 "A little" 2 "Somewhat" 3 "A lot"
label values disabled_scale_head disabled_scale_wife dis_scale

// tab disabled_head disabled_scale_head, m
// tab disabled_wife disabled_scale_wife, m

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

// add life satisfactio
tab LIFE_SATISFACTION_INDV_, m
replace LIFE_SATISFACTION_INDV_ = . if inlist(LIFE_SATISFACTION_INDV_,0,8,9)

tab LIFE_SATISFACTION_INDV_ RESPONDENT_WHO_, m // this is confusing bc in family level file, so think it's just about repsondent but ONLY if respondent Head or Wife?
gen life_satisfaction_head = LIFE_SATISFACTION_INDV_ if RESPONDENT_WHO_==1
gen life_satisfaction_wife = LIFE_SATISFACTION_INDV_ if inlist(RESPONDENT_WHO_,2,3)

label values life_satisfaction_head life_satisfaction_wife ER82027L 

*********************************************
* Family background variables
*********************************************
// okay think want to try to make these as fixed as possible (max education of parents) BUT can't do that until assign to focal since head / wife can change over time
browse unique_id survey_yr relationship FATHER_EDUC_HEAD MOTHER_EDUC_HEAD FATHER_EDUC_WIFE MOTHER_EDUC_WIFE FAMILY_STRUCTURE_HEAD FAMILY_STRUCTURE_WIFE LIVES_FAMILY_HEAD LIVES_FAMILY_WIFE FAMILY_AREA_HEAD FAMILY_AREA_WIFE

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
foreach var in LIVES_FAMILY_HEAD LIVES_FAMILY_WIFE FAMILY_AREA_HEAD FAMILY_AREA_WIFE{
	recode `var' (9=.)(0=.)
}


// need to get rid of some variables; this file is v. large. either variables not using at all or detailed input variables I no longer need
drop  RENT_COST_V1_ MORTGAGE_COST_ HOUSE_VALUE_ VEHICLE_OWN_ FOOD_STAMPS_ HRLY_RATE_CURRENT_HEAD_ TRANSFER_INCOME_ WELFARE_JOINT_ TYPE_TAXABLE_INCOME_ SALARY_TYPE_HEAD_ WORK_MONEY_WIFE_ SALARY_TYPE_WIFE_ HRLY_RATE_CURRENT_WIFE_ WORK_MONEY_HEAD_ OTHER_ASSETS_ STOCKS_MF_ WEALTH_NO_EQUITY_ WEALTH_EQUITY_ VEHICLE_VALUE_ RELATION_TO_HEAD_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_3_HEAD_ RACE_4_HEAD_ RACE_4_WIFE_ WELFARE_HEAD_1_ WELFARE_WIFE_1_ WAGES_CURRENT_HEAD_ WAGES_CURRENT_WIFE_ RENT_COST_V2_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_ WELFARE_HEAD_2_ WELFARE_WIFE_2_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ WAGES_ALT_T1_HEAD_ WAGES_ALT_T1_WIFE_ TOTAL_HOUSING_ BANK_ASSETS_ LABOR_INC_J1_T1_WIFE_ LABOR_INC_J2_T1_HEAD_ LABOR_INC_J3_T1_HEAD_ LABOR_INC_J4_T1_HEAD_ LABOR_INC_J2_T1_WIFE_ LABOR_INC_J3_T1_WIFE_ LABOR_INC_J4_T1_WIFE_ DIVIDENDS_JOINT_ INTEREST_JOINT_ BACHELOR_YR_INDV_ BACHELOR_YR_WIFE_ ENROLLED_WIFE_ BACHELOR_YR_HEAD_ ENROLLED_HEAD_ WAGES2_T1_WIFE_ CURRENTLY_WORK_HEAD_ CURRENTLY_WORK_WIFE_ EMPLOY_STATUS_T2_HEAD_ EMPLOY_STATUS_T2_WIFE_ START_YR_EMPLOYER_HEAD_ START_YR_EMPLOYER_WIFE_ START_YR_CURRENT_HEAD_ START_YR_CURRENT_WIFE_ START_YR_PREV_HEAD_ START_YR_PREV_WIFE_ YRS_CURRENT_EMPLOY_HEAD_ YRS_CURRENT_EMPLOY_WIFE_ WEEKS_WORKED_T2_INDV_ HS_GRAD_HEAD_ ATTENDED_COLLEGE_HEAD_ HIGHEST_DEGREE_HEAD_ HS_GRAD_WIFE_ ATTENDED_COLLEGE_WIFE_ HIGHEST_DEGREE_WIFE_ WHERE_EDUC_HEAD_ FOREIGN_DEG_HEAD_ WHERE_EDUC_WIFE_ FOREIGN_DEG_WIFE_ YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_ OFUM1_ID_ OFUM2_ID_ OFUM3_ID_ OFUM4_ID_ OFUM1_REL_ OFUM2_REL_ OFUM3_REL_ OFUM4_REL_ cohab_est_head  cohab_head_helper  hs_head hs_wife attended_college_head attended_college_wife completed_college_head COLLEGE_HEAD_ completed_college_wife COLLEGE_WIFE_ completed_college_indv COLLEGE_INDV_ college_degree_head college_degree_wife NEW_HEAD_YEAR NEW_WIFE_YEAR EDUC1_WIFE_ EDUC1_HEAD_ EDUC_WIFE_ EDUC_HEAD_ educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ_head_est educ_wife_est WHEN_ARRIVE_US_WIFE_ WHEN_ARRIVE_US_HEAD_ METRO_ URBAN_RURAL_BEALE_ EMPLOY_STATUS_HEAD_ EMPLOY_STATUS_WIFE_ employ_head employ1_head employ2_head employ3_head employ_wife employ1_wife employ2_wife employ3_wife ANNUAL_HOURS_T1_INDV TOTAL_HOUSEWORK_T1_HW total_housework_weekly MOST_HOUSEWORK_T1 when_first_birth first_birth_est all kidsu18 kidsu6 age65up RELIGION_HEAD_ DENOMINATION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_ DISABILITY_HEAD DISABLE_HOWMUCH_HEAD DISABILITY_WIFE DISABLE_HOWMUCH_WIFE SR_HEALTH_INDV SR_HEALTH_OTHER LIFE_SATISFACTION_INDV_ FAMILY_STRUCTURE_HEAD FAMILY_STRUCTURE_WIFE SEX_WIFE_ TYPE_OF_INCOME_ TOTAL_MONEY_INCOME_ RELEASE_NUM2_ FAMILY_COMPOSITION_ AGE_HEAD_ AGE_WIFE_ SEX_HEAD_ RELEASE_ INTERVIEW_NUM_ WIDOW_LENGTH_HEAD_ HRLY_RATE_T1_HEAD_ HRLY_RATE_T1_WIFE_ RELEASE_NUM_ partner_id RESEPONDENT_WIFE_

// tmp save: save "$created_data/PSID_long_all_recoded.dta", replace

/* confirm level of some variables (e.g. some copied to all in HH)

sort FAMILY_INTERVIEW_NUM_ survey_yr
browse FAMILY_INTERVIEW_NUM_ survey_yr unique_id STRATUM CLUSTER

sort unique_id survey_yr
*/

********************************************************************************
********************************************************************************
**# Create FOCAL versions now of all variables
********************************************************************************
********************************************************************************

********************************************************************************
* Demographic
********************************************************************************
// Demos that already exist: relationship, in_sample, hh_status, in_relationship, sample_type

// Sex: use as SEX

// Age & DOB
gen age_focal = AGE_INDV

	// use dob as birth_yr
	
// respondent indicator
fre RESPONDENT_WHO_
recode RESPONDENT_ (9=.)(0=.)

gen is_respondent_focal=.
replace is_respondent_focal =0 if RESPONDENT_==5
replace is_respondent_focal =0 if is_respondent_focal==. & relationship==1 & inrange(RESPONDENT_WHO_,2,9)
replace is_respondent_focal =0 if is_respondent_focal==. & relationship==2 & RESPONDENT_WHO_==1
replace is_respondent_focal =0 if is_respondent_focal==. & relationship==2 & inrange(RESPONDENT_WHO_,4,9)
replace is_respondent_focal =0 if is_respondent_focal==. & relationship==3 & inrange(RESPONDENT_WHO_,1,3)
replace is_respondent_focal =1 if RESPONDENT_==1
replace is_respondent_focal =1 if is_respondent_focal==. & relationship==1 & RESPONDENT_WHO_==1
replace is_respondent_focal =1 if is_respondent_focal==. & relationship==2 & inlist(RESPONDENT_WHO_,2,3)

// browse unique_id survey_yr relationship is_respondent_focal RESPONDENT_WHO_ RESPONDENT_
//  tab RESPONDENT_WHO_ if relationship==1 & is_respondent_focal==. , m // duh i needed to fill in the 0s.

// marital status
gen marital_status_focal = marital_status_indv
label values marital_status_focal marital_status_updated

// Education
tab educ_head educ_completed if relationship==1
tab educ_wife educ_completed if relationship==2

gen educ_focal=.
replace educ_focal = educ_completed if educ_completed!=. // let's prioritize indiivdual levels of education because that is asked annually even when head / ref is not, and sometimes updated for head / ref
replace educ_focal=educ_head if relationship==1 & educ_completed==. // then fill in otherwise here
replace educ_focal=educ_wife if relationship==2 & educ_completed==.
replace educ_focal=educ_completed if relationship==3

bysort unique_id: egen max_educ_focal = max(educ_focal)
label values educ_focal max_educ_focal educ

browse unique_id survey_yr relationship educ_focal max_educ_focal educ_completed educ_head educ_wife YRS_EDUCATION_INDV_

tab educ_focal in_sample, m
tab educ_focal relationship, m
tab max_educ_focal relationship, m

gen college_focal=.
replace college_focal = 0 if inrange(educ_focal,1,3)
replace college_focal = 1 if educ_focal==4

// race
gen raceth_focal=.
replace raceth_focal=raceth_head if relationship==1
replace raceth_focal=raceth_wife if relationship==2

bysort unique_id: egen raceth_focal_fixed = median(raceth_focal) // majority
tab raceth_focal_fixed, m
gen last_race_focal=raceth_focal if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_focal): replace last_race_focal = last_race_focal[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_focal raceth_focal_fixed last_race_focal
replace raceth_focal_fixed=last_race_focal if inlist(raceth_focal_fixed,1.5,2.5,3.5,4.5)
replace raceth_focal_fixed=last_race_focal if raceth_focal_fixed==.

label values raceth_focal raceth_focal_fixed raceth

// born in US or not
gen born_in_us_focal=.
replace born_in_us_focal=born_in_us_head if relationship==1
replace born_in_us_focal=born_in_us_wife if relationship==2

// religion
gen religion_focal=.
replace religion_focal=religion_head if relationship==1
replace religion_focal=religion_wife if relationship==2
label values religion_focal religion

// disability status
browse unique_id survey_yr relationship disabled_head disabled_scale_head disabled_wife disabled_scale_wife

gen disabled_focal=.
replace disabled_focal=disabled_head if relationship==1
replace disabled_focal=disabled_wife if relationship==2

gen disabled_scale_focal=.
replace disabled_scale_focal=disabled_scale_head if relationship==1
replace disabled_scale_focal=disabled_scale_wife if relationship==2
label values disabled_scale_focal dis_scale

// self-rated health
gen sr_health_focal=.
replace sr_health_focal=SR_HEALTH_HEAD_ if relationship==1
replace sr_health_focal=SR_HEALTH_WIFE_ if relationship==2
replace sr_health_focal=health_indv if relationship==3

label values sr_health_focal health
tab sr_health_focal, m

// life satisfaction
gen life_satisfaction_focal=.
replace life_satisfaction_focal=life_satisfaction_head if relationship==1
replace life_satisfaction_focal=life_satisfaction_wife if relationship==2

// family background variables
gen father_educ_focal=.
replace father_educ_focal=FATHER_EDUC_HEAD_ if relationship==1
replace father_educ_focal=FATHER_EDUC_WIFE_ if relationship==2

gen mother_educ_focal=.
replace mother_educ_focal=MOTHER_EDUC_HEAD_ if relationship==1
replace mother_educ_focal=MOTHER_EDUC_WIFE_ if relationship==2

label define parent_educ 0 "none" 1 "0-5 grades" 2 "6-8 grades" 3 "9-11 grades" 4 "high school" 5 "12+" 6 "some college" 7 "BA" 8 "advanced degree"
label values mother_educ_focal father_educ_focal parent_educ

bysort unique_id: egen father_max_educ_focal = max(father_educ_focal)
bysort unique_id: egen mother_max_educ_focal = max(mother_educ_focal)
label values father_max_educ_focal mother_max_educ_focal parent_educ

gen father_college_focal=.
replace father_college_focal = 0 if inrange(father_max_educ_focal,0,6)
replace father_college_focal = 1 if inlist(father_max_educ_focal,7,8)

gen mother_college_focal=.
replace mother_college_focal = 0 if inrange(mother_max_educ_focal,0,6)
replace mother_college_focal = 1 if inlist(mother_max_educ_focal,7,8)

sort unique_id survey_yr
browse unique_id survey_yr relationship father_max_educ_focal father_educ_focal mother_max_educ_focal mother_educ_focal FATHER_EDUC_HEAD_ FATHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ MOTHER_EDUC_WIFE_

gen family_structure_focal=.
replace family_structure_focal=family_structure_head if relationship==1
replace family_structure_focal=family_structure_wife if relationship==2

browse unique_id survey_yr last_survey_yr relationship in_sample family_structure_focal family_structure_head family_structure_wife
bysort unique_id: egen family_structure_cons_focal = min(family_structure_focal) // use min so if ever say didn't live with parents, that is prioritized

gen lives_family_focal=.
replace lives_family_focal=LIVES_FAMILY_HEAD_ if relationship==1
replace lives_family_focal=LIVES_FAMILY_WIFE_ if relationship==2
replace lives_family_focal=. if lives_family_focal==0

label define lives_family 1 "same state" 2 "same region" 3 "diff region"
label values lives_family_focal lives_family

// do I want to create a family area variable?
gen family_area_size_focal=.
replace family_area_size_focal=FAMILY_AREA_HEAD_ if relationship==1
replace family_area_size_focal=FAMILY_AREA_WIFE_ if relationship==2

label values family_area_size_focal ER84988L 

// births - based on PSID variables NOT birth history. can add that later
browse unique_id survey_yr BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ BIRTHS_T1_OFUMS_ BIRTHS_T2_HEAD_ BIRTHS_T2_WIFE_ BIRTHS_T2_BOTH_ BIRTHS_T2_OFUMS_

gen any_births_t1_focal=.
replace any_births_t1_focal = 0 if relationship==1 & BIRTHS_T1_HEAD_==0 & BIRTHS_T1_BOTH_==0
replace any_births_t1_focal = 1 if relationship==1 & (inrange(BIRTHS_T1_HEAD_,1,3) |  inrange(BIRTHS_T1_BOTH_,1,3))
replace any_births_t1_focal = 0 if relationship==2 & BIRTHS_T1_WIFE_==0 & BIRTHS_T1_BOTH_==0
replace any_births_t1_focal = 1 if relationship==2 & (inrange(BIRTHS_T1_WIFE_,1,3) | inrange(BIRTHS_T1_BOTH_,1,3))

gen any_births_t1_hh=. // because, even if not head/  wife - if head / wife HAD a birth, that is technically a new kid in HH, even if that person didn't have the kid
replace any_births_t1_hh=0 if inlist(BIRTHS_T1_HEAD_,0,9) & inlist(BIRTHS_T1_WIFE_,0,9) & inlist(BIRTHS_T1_BOTH_,0,9) & inlist(BIRTHS_T1_OFUMS_,0,9)
replace any_births_t1_hh = 1 if inrange(BIRTHS_T1_WIFE_,1,3) | inrange(BIRTHS_T1_BOTH_,1,3) | inrange(BIRTHS_T1_WIFE_,1,3) | inrange(BIRTHS_T1_OFUMS_,1,3)

gen any_births_t2_focal=.
replace any_births_t2_focal = 0 if relationship==1 & BIRTHS_T2_HEAD_==0 & BIRTHS_T2_BOTH_==0
replace any_births_t2_focal = 1 if relationship==1 & (inrange(BIRTHS_T2_HEAD_,1,3) |  inrange(BIRTHS_T2_BOTH_,1,3))
replace any_births_t2_focal = 0 if relationship==2 & BIRTHS_T2_WIFE_==0 & BIRTHS_T2_BOTH_==0
replace any_births_t2_focal = 1 if relationship==2 & (inrange(BIRTHS_T2_WIFE_,1,3) | inrange(BIRTHS_T2_BOTH_,1,3))

gen any_births_t2_hh=. // because, even if not head/  wife - if head / wife HAD a birth, that is technically a new kid in HH, even if that person didn't have the kid
replace any_births_t2_hh=0 if inlist(BIRTHS_T2_HEAD_,0,9) & inlist(BIRTHS_T2_WIFE_,0,9) & inlist(BIRTHS_T2_BOTH_,0,9) & inlist(BIRTHS_T2_OFUMS_,0,9)
replace any_births_t2_hh = 1 if inrange(BIRTHS_T2_WIFE_,1,3) | inrange(BIRTHS_T2_BOTH_,1,3) | inrange(BIRTHS_T2_WIFE_,1,3) | inrange(BIRTHS_T2_OFUMS_,1,3)

// ever parent status
tab NUM_BIRTHS, m
tab year_first_birth NUM_BIRTHS, m

gen ever_parent_focal = .
replace ever_parent_focal = 0 if NUM_BIRTHS==0
replace ever_parent_focal = 1 if NUM_BIRTHS >=1 & NUM_BIRTHS<=20

tab ever_parent_focal cah_any_births, m // I think I want to rely on num births, so don't use the CAH one?!
tab ever_parent_focal ever_birth, m

tab year_first_birth ever_parent_focal, m // bc this ever parent one (the ever birth is the same) align with year of first birth I created nicely

gen num_births_focal = .
replace num_births_focal = 0 if NUM_BIRTHS==0
replace num_births_focal = NUM_BIRTHS if NUM_BIRTHS >=1 & NUM_BIRTHS<=20

********************************************************************************
* Division of Labor
********************************************************************************

// Many paid work created above to facilitate the year alignment
	// Weekly Hours T: weekly_hrs_t_focal
	// Annual Earnings T: earnings_t_focal
	// Weekly Hours T-1: weekly_hrs_t1_focal
	// Annual Earnings T-1: earnings_t1_focal
	// Also have t-2 versons jic (as helpers): weekly_hrs_t2_focal, earnings_t2_focal

// weekly HW hours
browse unique_id survey_yr relationship housework_head housework_wife HOUSEWORK_INDV_
gen housework_focal=.
replace housework_focal=housework_head if relationship==1
replace housework_focal=housework_wife if relationship==2
replace housework_focal=HOUSEWORK_INDV_ if relationship==3
// replace housework_focal=. if relationship==3

// weekly childcare
gen childcare_focal=.
replace childcare_focal=CHILDCARE_HEAD if relationship==1
replace childcare_focal=CHILDCARE_WIFE if relationship==2

// weekly adultcare
gen adultcare_focal=.
replace adultcare_focal=ADULTCARE_HEAD if relationship==1
replace adultcare_focal=ADULTCARE_WIFE if relationship==2

// Current employment status: detailed
browse unique_id survey_yr relationship employment_status_head employment_status_wife EMPLOYMENT_INDV_
tab EMPLOYMENT_INDV_ employment_status_head if relationship==1 // so they do match

gen employment_status_focal=.
replace employment_status_focal=employment_status_head if relationship==1
replace employment_status_focal=employment_status_wife if relationship==2
replace employment_status_focal=EMPLOYMENT_INDV_ if relationship==3

label values employment_status_focal employment_status

// Current employment status: binary
browse unique_id survey_yr relationship employed_head employed_wife employed_indv
gen employed_focal=.
replace employed_focal=employed_head if relationship==1
replace employed_focal=employed_wife if relationship==2
replace employed_focal=employed_indv if relationship==3

********************************************************************************
* Household variables (aka don't need to be focal AND should match between head / wife)
********************************************************************************
// just noting here to collect as most of these don't need to be updated: REGION_, urban_rural, TOTAL_INCOME_T1_FAMILY_, NUM_CHILDREN_, children, children_ever, age_youngest_child, age_oldest_child, NUM_IN_HH, all_in_hh, num_65up_hh, kidsu18_hh

// house status - just easier when in order
gen house_status_all = .
replace house_status_all = 1 if HOUSE_STATUS_ == 1
replace house_status_all = 2 if HOUSE_STATUS_ == 5
replace house_status_all = 3 if HOUSE_STATUS_ == 8

label define house 1 "Owns" 2 "Rents" 3 "Neither"
label values house_status_all house

tab house_status_all home_owner, m

//tmp save: save "$created_data/PSID_long_all_recoded.dta", replace

********************************************************************************
********************************************************************************
**# Compiling relationship history and current relationship info
********************************************************************************
********************************************************************************
// use "$created_data/PSID_long_all_recoded.dta", clear

merge m:1 unique_id using "$created_data/psid_master_relationship_history_wide.dta" // remember kim, before you panic, this file is restricted to ONLY PEOPLE with relationships ever
drop if _merge==2
tab in_relationship _merge, m row // a few people in relationships don't have matches, but pretty good (it's less than 1% non-match rate)
drop _merge
tab FIRST_MARRIAGE_YR_START if history_flag==., m // confirm - those missing on history flag (aka no record in rel history wide) do not have any observed marriage dates in marital history - and this is true. this validates that my history at least covers those in a relationship at some point.

browse unique_id survey_yr in_relationship marital_status_focal MARITAL_PAIRS_ MARST_DEFACTO_HEAD_ 

gen in_relationship_yr = survey_yr if in_relationship==1
sort unique_id survey_yr
gen enter_rel=0
replace enter_rel=1 if in_relationship==1 & in_relationship[_n-1]==0 & unique_id==unique_id[_n-1]
replace enter_rel=1 if in_relationship_yr==1968 // since can't transition, but call this "in_relationship 1"

gen exit_rel=0
sort id survey_yr
replace exit_rel=1 if in_relationship==1 & in_relationship[_n+1]==0 & unique_id==unique_id[_n+1]

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

gen rel_start_all=.
gen rel_end_all=.
gen rel_type_all=.
gen rel_status_all=.
gen rel_left_censored_all=.

forvalues r=1/10{
	replace rel_start_all = master_rel_start`r' if current_rel_number==`r'
	replace rel_end_all = master_rel_end`r' if current_rel_number==`r'
	replace rel_type_all = master_rel_type`r' if current_rel_number==`r'
	replace rel_status_all = master_rel_how_end`r' if current_rel_number==`r'
	replace rel_left_censored_all = master_rel_left_censored`r' if current_rel_number==`r'
}

label values rel_type_all type
label values rel_status_all how_rel_end
tab MX8 rel_type_all, m // some are off because transitioned cohab to marriage and mine only captures 1st rel - we are going to update htis the way I normally do with the min / max rel dates. but doing this at a couple-level so can attempt to fill in true start / end dates (if one has non-missing info)
tab MX8 rel_status_all, m // this is def not perfect, especially for cohab - intact not at all labelled yet

tab rel_start_all rel_left_censored_all, m
tab rel_start_all rel_left_censored_all if MX8==20, m
tab rel_start_all rel_left_censored_all if MX8==22, m // am I going to lose all of my cohabitors??
tab rel_start_all rel_left_censored_all if MX8==22 & has_psid_gene==0, m // okay OR is it really just that it's because I haven't matched and cleaned to partners when the one with the PSID gene will MASSIVELY help 
tab rel_start_all rel_left_censored_all if MX8==22 & has_psid_gene==1, m // like this is actually fine except for 1968 which I remove
	// I think it's these samples really then still causing issues on above: tab sample_type has_psid_gene

browse unique_id survey_yr in_relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all rel_status_all rel_left_censored_all enter_rel exit_rel master_rel_start1 master_rel_end1 master_rel_type1 master_rel_how_end1 master_rel_left_censored1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_how_end2 master_rel_left_censored2 master_rel_start3 master_rel_end3 master_rel_type3 master_rel_left_censored4

// attempt to fill in 9999s / missings using transition info created above. ALSO if intact, I can set to last observed survey year, right? (or do I leave as 9999?)
tab rel_start_all MX8, m
tab rel_end_all MX8, m col
tab rel_end_all rel_status_all if in_relationship==1, m col // okay actually this IS the main culprit of the 9999s

gen rel_start_est = survey_yr if enter_rel==1
bysort unique_id partner_unique_id (rel_start_est): replace rel_start_est=rel_start_est[1] if partner_unique_id!=.

gen rel_end_est = survey_yr if exit_rel==1
bysort unique_id partner_unique_id (rel_end_est): replace rel_end_est=rel_end_est[1] if partner_unique_id!=.

sort unique_id survey_yr
browse unique_id survey_yr in_relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all enter_rel exit_rel rel_start_est rel_end_est
browse unique_id survey_yr in_relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all enter_rel exit_rel rel_start_est rel_end_est if (rel_start_all==. | rel_end_all==. | rel_end_all==9999) & MX8!=.

gen current_rel_start = rel_start_all // retain original
replace current_rel_start = rel_start_est if current_rel_start==. & rel_start_est!=.

gen current_rel_end = rel_end_all // retain original
replace current_rel_end = rel_end_est if (current_rel_end==. | current_rel_end==9999) & rel_end_est!=.

browse unique_id survey_yr in_relationship MX8 partner_unique_id current_rel_start current_rel_end rel_start_all rel_end_all rel_type_all enter_rel exit_rel rel_start_est rel_end_est

tab current_rel_start MX8, m
tab current_rel_end MX8, m col

bysort unique_id partner_unique_id: egen first_couple_yr = min(survey_yr) if partner_unique_id!=. // possibly can help figure out attrition?
bysort unique_id partner_unique_id: egen last_couple_yr = max(survey_yr) if partner_unique_id!=.

tab current_rel_end rel_status_all, m // also could all 9999s be proxy for attrit / ongoing? that is true for marital history, not sure otherwise

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr history_flag MX8 rel_status_all current_rel_start current_rel_end first_survey_yr last_survey_yr first_couple_yr last_couple_yr

gen current_rel_status_est = rel_status_all
replace current_rel_status_est = 0 if current_rel_status_est==. & last_survey_yr == last_couple_yr & MX8!=.
replace current_rel_status_est = 1 if current_rel_status_est==. & last_survey_yr != last_couple_yr & MX8!=. // breakup if person continues in survey past in_relationship end
label values current_rel_status_est how_rel_end

browse unique_id partner_unique_id survey_yr history_flag MX8 current_rel_status_est rel_status_all current_rel_start current_rel_end first_survey_yr last_survey_yr first_couple_yr last_couple_yr
tab current_rel_end current_rel_status_est, m

********************************************************************************
* Important info on transitions between cohab and marriage
********************************************************************************
sort unique_id survey_yr
browse unique_id survey_yr wave partner_unique_id MX8 // check: 4008, 2848180, 5693181. was going to see if I can fix these disappear years, but I struggle because here, I am not restricting to partners, so I don't get a continuous view, so it's maybe more confusing to do the mismatch here? and just fix it later? like I was thinking I replace the below, but they have missing data rows sometimes so it might not always be continuous. but...do we think they are like...broken up??

gen marr_trans=0
replace marr_trans=1 if MX8 == 20 & MX8[_n-1]==22 & unique_id==unique_id[_n-1] & partner_unique_id==partner_unique_id[_n-1] & wave==wave[_n-1]+1

bysort unique_id partner_unique_id: egen ever_transition = max(marr_trans)

gen first_rel_type = MX8 if survey_yr==first_couple_yr
gen last_rel_type = MX8 if survey_yr==last_couple_yr

bysort unique_id partner_unique_id (first_rel_type): replace first_rel_type = first_rel_type[1]
bysort unique_id partner_unique_id (last_rel_type): replace last_rel_type = last_rel_type[1]

tab first_rel_type last_rel_type, m
tab first_rel_type last_rel_type if in_relationship==1, m 

gen transition_est = 0
replace transition_est = 1 if first_rel_type==22 & last_rel_type==20

tab transition_est ever_transition, m
// browse unique_id survey_yr wave partner_unique_id first_couple_yr last_couple_yr MX8 first_rel_type last_rel_type transition_est ever_transition if transition_est != ever_transition // I actually think this transition_est solves two problems: adds the people who disappear between being partnered and married AND removes the people who weirdly switch between spouse and partnered throughout....I guess a key question is - the transition_est that are 1s and ever transition not i think are real and okay - the oppposite, though, how do I handle? I do think MOST are resolved with start date - a lot of it is confusion with the PSID recording cohab weird in early years. so leave to sort out as I clean rest of files

gen transition_year = survey_yr if marr_trans==1
bysort unique_id partner_unique_id (transition_year): replace transition_year=transition_year[1] if partner_unique_id!=. 

sort unique_id survey_yr
browse unique_id survey_yr wave partner_unique_id MX8  marr_trans ever_transition transition_year

rename MX8 current_rel_type
rename rel_left_censored_all current_rel_left_censored // want to follow format

// tmp save; save "$created_data/PSID_long_all_recoded.dta", replace

// in next file, once I have partner info, I will a. see if I can recover missings and / or left censored info, b. harmonize across partners, and c. then standardize start and end if they transition to marriage

********************************************************************************
** let's add a few small individual things before I close / save this file.
********************************************************************************
// add first divorce year. start with version created from marital history. this info theoretically exists for head / wives also...oh there is also first SEpARATION year for all. okay let's check congruence (but also these are contingent on being in marital history so they might all match)
merge m:1 unique_id using "$temp/marital_history_wide.dta", keepusing(mh_first_divorce_year mh_first_separation_year mh_first_dissolution_year)
drop if _merge==2
tab history_flag _merge, m
drop _merge

label define first_marr_status 1 "intact" 3 "widow" 4 "divorce" 5 "separated" 7 "other" 8 "dk" 9 "n/a"
label values FIRST_MARRIAGE_STATUS first_marr_status

browse unique_id survey_yr relationship mh_first_divorce_year mh_first_separation_year mh_first_dissolution_year FIRST_SEPARATE_YR FIRST_MARRIAGE_YR_END FIRST_MARRIAGE_STATUS FIRST_DIVORCE_YR_HEAD_ FIRST_DIVORCE_YR_WIFE_
// tab FIRST_SEPARATE_YR mh_first_separation_year if FIRST_SEPARATE_YR!=9999, m

// birth history info?! need to update this with partner ID info. I pulled this in above

// let's do a check of the variables I will probably use in analysis (at least psm) so I can figure out any missings and if properly recoded
misstable summarize employment_status_focal employed_focal weekly_hrs_t_focal weekly_hrs_t1_focal weekly_hrs_t2_focal earnings_t_focal earnings_t1_focal earnings_t2_focal TOTAL_INCOME_T1_FAMILY_ housework_focal childcare_focal adultcare_focal num_births_focal ever_parent_focal year_first_birth any_births_t1_focal any_births_t2_focal age_youngest_child NUM_CHILDREN_ kidsu18_hh children num_65up_hh NUM_IN_HH all_in_hh educ_focal max_educ_focal college_focal raceth_focal raceth_focal_fixed born_in_us_focal age_oldest_child children_ever REGION_ urban_rural home_owner house_status_all religion_focal disabled_focal disabled_scale_focal sr_health_focal life_satisfaction_focal father_educ_focal father_max_educ_focal father_college_focal mother_educ_focal mother_max_educ_focal mother_college_focal family_structure_focal family_structure_cons_focal lives_family_focal family_area_size_focal SEX age_focal birth_yr sample_type RESPONDENT_WHO_ is_respondent_focal in_sample hh_status in_relationship relationship marital_status_focal current_rel_number current_rel_start current_rel_end current_rel_status_est current_rel_type mh_first_divorce_year mh_first_dissolution_year , all showzeros

// per above, anything need to be recoded (weird missing, weird min / max, etc.) - okay everything seems fine for now...

save "$created_data/PSID_long_all_recoded.dta", replace // this file not yet restricted in any way (except for sample years)

********************************************************************************
* Reshape wide for couple matching
********************************************************************************
// Think I want a WIDE version of this file to use to fill in info for eligible individuals - think WIDE at year level if first needed to facilitate the t-1/t-2 fill-in for the off survey years. so will start with couple list, merge on this wide file, fill in the t-2 info THEN reshape to long and go from there...I hope this works (this is loosely the life course framework but the life course framework starts with FULL wide not the sample restricted. so making wide, i will add indicator of no-sample year)

use "$created_data/PSID_long_all_recoded.dta", clear

drop BIRTH_YR_INDV_ FIRST_MARRIAGE_YR_WIFE_ EVER_MARRIED_HEAD_ NUM_MARRIED_HEAD_ FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_END_HEAD_ FIRST_WIDOW_YR_HEAD_ FIRST_DIVORCE_YR_HEAD_ FIRST_SEPARATED_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ LAST_WIDOW_YR_HEAD_ LAST_DIVORCE_YR_HEAD_ LAST_SEPARATED_YR_HEAD_ NUM_MARRIED_WIFE_ FIRST_MARRIAGE_END_WIFE_ FIRST_WIDOW_YR_WIFE_ FIRST_DIVORCE_YR_WIFE_ FIRST_SEPARATED_YR_WIFE_ LAST_MARRIAGE_YR_WIFE_ LAST_WIDOW_YR_WIFE_ LAST_DIVORCE_YR_WIFE_ LAST_SEPARATED_YR_WIFE_ FATHER_1988_ID_HEAD MOTHER_1988_ID_HEAD FATHER_1988_ID_WIFE MOTHER_1988_ID_WIFE HEALTH_INSURANCE_FAM_ ever_in_sample year wave birth_yr_helper race_1_head_rec race_2_head_rec race_3_head_rec race_4_head_rec race_1_wife_rec race_2_wife_rec race_3_wife_rec race_4_wife_rec num_emp_status_head int_number per_num first_birth_check in_relationship_yr enter_rel exit_rel rel_start_all rel_end_all rel_type_all rel_status_all rel_start_est rel_end_est first_couple_yr last_couple_yr marr_trans ever_transition transition_year transition_est first_rel_type  last_rel_type

// since I restrict to 1980s for relationships, I can actually drop survey years prior to 1980 to make file smaller? (add buffer for now)
drop if survey_yr < 1980

reshape wide RELATION_ AGE_INDV_ MARITAL_PAIRS_ YRS_EDUCATION_INDV_ AGE_YOUNG_CHILD_ RESPONDENT_WHO_ FAMILY_INTERVIEW_NUM_ FATHER_EDUC_HEAD_ REGION_ NUM_CHILDREN_ TOTAL_INCOME_T1_FAMILY_ SEQ_NUMBER_ RESPONDENT_ AGE_OLDEST_CHILD_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ EMPLOYMENT_INDV_ LABOR_INCOME_T1_INDV_ HOUSEWORK_INDV_ CHILDCARE_HEAD_ CHILDCARE_WIFE_ ADULTCARE_HEAD_ ADULTCARE_WIFE_ SR_HEALTH_HEAD_ SR_HEALTH_WIFE_ FAMILY_AREA_WIFE_ FAMILY_AREA_HEAD_ LIVES_FAMILY_HEAD_ LIVES_FAMILY_WIFE_ relationship in_sample hh_status in_relationship current_rel_type partner_unique_id matrix_rel_num matrix_marr_num marital_status_head marital_status_indv educ_completed educ_head educ_wife college_wife college_head college_indv race_wife race_head hispanic_head hispanic_wife raceth_head raceth_wife born_in_us_head born_in_us_wife urban_rural home_owner employment_status_head employment_status_wife employed_head employed_wife employed_indv weekly_hrs_t1_wife weekly_hrs_t1_head weekly_hrs_t1_indv weekly_hrs_t2_focal weekly_hrs_t1_focal weekly_hrs_t_focal weekly_hrs_t_head weekly_hrs_t_wife earnings_t1_wife earnings_t1_head earnings_t2_focal earnings_t1_focal earnings_t_focal housework_head housework_wife kidsu18_hh kidsu6_hh num_65up_hh age_oldest_child all_in_hh age_youngest_child children NUM_IN_HH religion_head religion_wife disabled_head disabled_scale_head disabled_wife disabled_scale_wife health_indv life_satisfaction_head life_satisfaction_wife family_structure_head family_structure_wife age_focal is_respondent_focal marital_status_focal educ_focal college_focal raceth_focal born_in_us_focal religion_focal disabled_focal disabled_scale_focal sr_health_focal life_satisfaction_focal father_educ_focal mother_educ_focal family_structure_focal lives_family_focal family_area_size_focal any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh housework_focal childcare_focal adultcare_focal employment_status_focal employed_focal house_status_all current_rel_number current_rel_start current_rel_end current_rel_status_est current_rel_left_censored X1968_PERSON_NUM_ MOVED_ MARST_DEFACTO_HEAD_ WAGES_T1_HEAD_ CORE_WEIGHT_ ANNUAL_HOURS_T1_HEAD_ ANNUAL_HOURS_T1_WIFE_ LABOR_INCOME_T1_HEAD_ LABOR_INCOME_T1_WIFE_ TAXABLE_T1_HEAD_WIFE_ WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ HOUSE_STATUS_ POVERTY_THRESHOLD_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_HEAD_ CHILDCARE_COSTS_ NEW_WIFE_ TOTAL_INCOME_T1_INDV_ MARST_LEGAL_HEAD_ STUDENT_T1_INDV_ COUPLE_STATUS_HEAD_ STATE_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ BIRTHS_T1_OFUMS_ BIRTHS_T2_BOTH_ BIRTHS_T2_HEAD_ BIRTHS_T2_OFUMS_ BIRTHS_T2_WIFE_ WAGES_T1_WIFE_ WEEKLY_HRS_T1_HEAD_ WEEKLY_HRS_T1_WIFE_ COR_IMM_WT_ ETHNIC_WIFE_ ETHNIC_HEAD_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_ CDS_ELIGIBLE_ LABOR_INC_J1_T1_HEAD_ TOTAL_WEEKS_T1_HEAD_ ANNUAL_HOURS2_T1_HEAD_ TOTAL_WEEKS_T1_WIFE_ ANNUAL_HOURS2_T1_WIFE_ WEEKLY_HRS_T2_HEAD_ NUM_JOBS_T1_INDV_ STUDENT_CURRENT_INDV_ WEEKLY_HRS_T2_WIFE_ LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ WEEKLY_HRS_T2_INDV_ LABOR_INCOME_T2_INDV_ HISPANICITY_WIFE_ HISPANICITY_HEAD_ TOTAL_INCOME_T2_FAMILY_ FOLLOW_STATUS_ NUM_IN_HH_ NUM_NONFU_IN_HH_ MOVED_YEAR_ MOVED_MONTH_ SPLITOFF_YEAR_ SPLITOFF_MONTH_ DATA_RECORD_TYPE_ SPLITOFF_ DISABLE_WORK_HEAD_ DISABLE_WORK_WIFE_ YR_RETIRED_HEAD_ YR_RETIRED_WIFE_ MOVED_LASTSPRING_HEAD_ MOVED_SPRING_MO_HEAD_ MOVED_SPRING_YR_HEAD_ REGION_GREW_UP_HEAD_ REGION_GREW_UP_WIFE_ STATE_BORN_HEAD_ STATE_BORN_WIFE_ BORN_US_HEAD_ BORN_US_WIFE_ INTERVIEW_DATE_HEAD_ INTERVIEW_MONTH_HEAD_ INTERVIEW_DAY_HEAD_ INTERVIEW_YEAR_HEAD_ interview_month_created, ///
 i(unique_id) j(survey_yr)

browse unique_id first_survey_yr last_survey_yr in_sample* SEQ_*

save "$created_data/PSID_wide_all_recoded.dta", replace
