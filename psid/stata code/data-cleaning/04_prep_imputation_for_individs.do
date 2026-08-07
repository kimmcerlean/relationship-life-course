
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
* This files takes the recoded data and does some final prep / filling in 
* of data to prepare file for imputation (in next step)

********************************************************************************
**# Get data and shape to wide to fill in the off years where possible (with t-2 data)
********************************************************************************
use "$temp/inidividual_vars_imputation_long.dta", clear

drop *_head* *_HEAD* *_wife* *_WIFE* *_INDV* *_indv* educ_completed wave MOVED_ MOVED_MONTH_  SPLITOFF_MONTH_ FAMILY_ID_SO_ MOVED_sp_ edulevelrp_match edulevelmaxrp_match edulevelsp_match edulevelmaxsp_match father_educ_focal mother_educ_focal family_structure_focal OFUM*_ID_ OFUM*_REL_ father_check mother_check year // cah_mom_wanted* cah_mom_timing* cah_dad_wanted* cah_dad_timing* cah_child_hispanicity* cah_child_race1* cah_child_race2* cah_child_race3* cah_child_birth_mon* cah_birth_order* cah_child_int_number* cah_child_per_num* cah_parent_marst* cah_child_sex* cah_event_type* cah_num_children*

bysort unique_id: egen birth_yr_all = min(birth_yr)
drop birth_yr

reshape wide in_sample_ RELATION_ relationship_ relationship_type_ MARITAL_PAIRS_ weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal  age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ TOTAL_INCOME_T2_FAMILY_ raceth_focal childcare_focal adultcare_focal has_hours_t1 has_earnings_t1 has_hours_t2 has_earnings_t2 employed_t1_hrs_focal employed_t1_earn_focal SPLITOFF_ COMPOSITION_CHANGE_ NUM_IN_HH_ MOVED_YEAR_ SPLITOFF_YEAR_ DATA_RECORD_TYPE_ hh_status_ in_sample_sp_ relationship_sp_ relationship_type_sp_ MARITAL_PAIRS_sp_  MOVED_YEAR_sp_  SPLITOFF_sp_  SPLITOFF_YEAR_sp_ hh_status_sp_ moved_focal moved_sp partnered partnered_sp any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh under18 edulevel_match edulevelmax_match new_in_hh employment_status_focal disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal empstat_retired_focal num_65up_hh age65up house_status_all moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO is_respondent_focal REGION_ parent_in_ofum num_parent_in_hh father_in_hh mother_in_hh father_fam_id father_in_sample father_moved father_change_yr mother_fam_id mother_in_sample mother_moved mother_change_yr is_first_yr_cohabitor has_first_yr_cohabitor rel_type marital_status_focal ///
, i(unique_id partner_unique_id eligible_rel_start_year min_dur max_dur eligible_rel_end_year ended SEX) j(survey_yr)  // birth_yr FIRST_BIRTH_YR ever_parent_focal num_births_focal lives_with_parent lives_with_grandparent  raceth_fixed_focal max_educ_focal

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

// but then I should replace age of youngest child in even years with 9999 if no kids?
tab AGE_YOUNG_CHILD_1999 NUM_CHILDREN_1999, m
tab AGE_YOUNG_CHILD_1998 NUM_CHILDREN_1998, m

forvalues y=1998(2)2022{
	replace AGE_YOUNG_CHILD_`y' = 9999 if NUM_CHILDREN_`y'==0
}

browse unique_id NUM_CHILDREN_1999 NUM_CHILDREN_2000 NUM_CHILDREN_2001 NUM_CHILDREN_2002 NUM_CHILDREN_2003 any_births_t1_hh1999 any_births_t1_hh2001 any_births_t1_hh2003 any_births_t1_hh2005 any_births_t2_hh1999 any_births_t2_hh2001 any_births_t2_hh2003 any_births_t2_hh2005 hh_births_1999 hh_births_2000 hh_births_2001 hh_births_2002 hh_births_2003 hh_births_2004

// let's try to fill in education for the years where the surrounding years are the same (bc education can't change that way)
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
tab RESPONDENT_WHO_1999, m
tab is_respondent_focal1999, m
tab RESPONDENT_WHO_1999 is_respondent_focal1999, m
tab RESPONDENT_WHO_1999 relationship_1999, m
tab relationship_1999 is_respondent_focal1999, m

forvalues y=1998(2)2022{
	capture gen RESPONDENT_WHO_`y'=9
	label values RESPONDENT_WHO_`y' resp
}

label define is_respondent 0 "No" 1 "Yes" 9 "Off-year or non-sample"

forvalues y=1998(2)2022{
	capture gen is_respondent_focal`y'=9
	label values is_respondent_focal`y' is_respondent
}

forvalues y=1997(2)2023{
	replace is_respondent_focal`y'=9 if is_respondent_focal`y'==.
	label values is_respondent_focal`y' is_respondent
}

tab in_sample_1999 is_respondent_focal1999, m

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

// before reshaping, get first observed educational
egen first_educ_focal=rowfirst(educ_focal*)
browse first_educ_focal educ_focal*
label values first_educ_focal educ

// do I try to recover race again actually one more time while wide?
egen last_race_focal_check=rowlast(raceth_focal*) // tie break with last reported
browse last_race_focal last_race_focal_check raceth_focal*
label values last_race_focal_check raceth
tab last_race_focal last_race_focal_check, m
tab raceth_fixed_focal last_race_focal_check, m
replace raceth_fixed_focal=last_race_focal_check if raceth_fixed_focal==. & last_race_focal_check!=. // okay actually barely recovered any

********************************************************************************
********************************************************************************
* CHECKPOINT: BACK to long so can recenter on duration and fill in some missings
********************************************************************************
********************************************************************************
reshape long in_sample_ RELATION_  relationship_ relationship_type_ MARITAL_PAIRS_ weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal  age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ TOTAL_INCOME_T2_FAMILY_ raceth_focal childcare_focal adultcare_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY has_hours_t1 has_earnings_t1 has_hours_t2 has_earnings_t2 employed_t1_hrs_focal employed_t1_earn_focal SPLITOFF_ COMPOSITION_CHANGE_ NUM_IN_HH_ MOVED_YEAR_ SPLITOFF_YEAR_ DATA_RECORD_TYPE_ hh_status_ in_sample_sp_ relationship_sp_ relationship_type_sp_  MARITAL_PAIRS_sp_  MOVED_YEAR_sp_  SPLITOFF_sp_  SPLITOFF_YEAR_sp_ hh_status_sp_ moved_focal moved_sp partnered partnered_sp yrs_non_sample any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh under18 edulevel_match edulevelmax_match new_in_hh individ_birth_ hh_births_ employment_status_focal disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal empstat_retired_focal num_65up_hh age65up house_status_all moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO_ is_respondent_focal  REGION_ parent_in_ofum num_parent_in_hh father_in_hh mother_in_hh father_fam_id father_in_sample father_moved father_change_yr mother_fam_id mother_in_sample mother_moved mother_change_yr is_first_yr_cohabitor has_first_yr_cohabitor rel_type marital_status_focal ///
, i(unique_id partner_unique_id eligible_rel_start_year min_dur max_dur eligible_rel_end_year ended SEX) j(survey_yr) // lives_with_parent lives_with_grandparent max_educ_focal raceth_fixed_focal

browse unique_id survey_yr eligible_rel_start_year eligible_rel_end_year min_dur max_dur relationship_ in_sample_ weekly_hrs_t_focal weekly_hrs_t1_focal weekly_hrs_t2_focal housework_focal earnings_t1_focal earnings_t_focal employed_focal employment_status_focal
browse unique_id survey_yr in_sample_ TOTAL_INCOME_T_FAMILY TOTAL_INCOME_T1_FAMILY house_status_all REGION_ // because family income is at HH level, when not in sample, it is actually filled in, same with house status and region

// small missing on in_sample - think these are all 1984 actually
browse unique_id survey_yr first_survey_year last_survey_year in_sample_
replace in_sample_ = 0 if survey_yr > last_survey_year & in_sample_==.
replace in_sample_ = 0 if survey_yr < first_survey_year & in_sample_==.

inspect weekly_hrs_t1_focal weekly_hrs_t_focal weekly_hrs_t2_focal earnings_t1_focal earnings_t_focal housework_focal if in_sample_==0 // the t might be okay sometimes for not in sample bc filled in from t+1? 

tab weekly_hrs_t2_focal if in_sample_==0 & weekly_hrs_t_focal==0, m // are they real zeroes or not? okay actually not real
tab weekly_hrs_t1_focal if in_sample_==0 & weekly_hrs_t_focal==0, m // are they real zeroes or not?
tab earnings_t2_focal if in_sample_==0 & earnings_t_focal==0, m // are they real zeroes or not? okay actually not real
tab earnings_t1_focal if in_sample_==0 & earnings_t_focal==0, m

foreach var in weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal adultcare_focal childcare_focal raceth_focal employed_t1_hrs_focal employed_t1_earn_focal disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal empstat_retired_focal employment_status_focal religion_focal{
	replace `var'=. if in_sample_==0 // oh lol, I also did this here...
}

foreach var in weekly_hrs_t_focal earnings_t_focal{
	replace `var'=. if in_sample_==0 & `var'==0 // leave the positives beacuse should be recoverable, but 0s seem not real per above
}

gen duration = survey_yr - eligible_rel_start_year
browse unique_id partner_unique_id survey_yr eligible_rel_start_year duration min_dur max_dur first_couple_year last_couple_year weekly_hrs_t_focal housework_focal

browse unique_id survey_yr eligible_rel_start_year duration min_dur max_dur relationship_ in_sample_ weekly_hrs_t1_focal weekly_hrs_t2_focal housework_focal

browse unique_id survey_yr age_focal birth_yr
replace age_focal = survey_yr - birth_yr if age_focal==.

*******************************************************************
* try to fill in partnership status using various history measures
*******************************************************************
// Try to fill in based on current partnership and then relationship history
label values MARITAL_PAIRS_ .
browse unique_id partner_unique_id survey_yr rel_type eligible_rel_start_year eligible_rel_end_year partnered partnered_sp MARITAL_PAIRS_

// first, create variables I want to fill in
	gen partnered_imp = partnered
	replace partnered_imp = 1 if survey_yr>= eligible_rel_start_year & survey_yr <=eligible_rel_end_year
	replace partnered_imp = 1 if duration>= min_dur & duration <=max_dur // should be covered above but jic
	// replace partnered_imp = .x if partnered_imp==. // using this, because partnered will be real missing if attrit / not in smaple, so need to distinguish real missing frm those i can fill in
	// browse unique_id partner_unique_id survey_yr eligible_rel_start_year eligible_rel_end_year partnered partnered_imp partnered_sp

	gen marst_imp = marital_status_focal 
	label values marst_imp marital_status_updated

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

merge m:1 unique_id using "$created_data/psid_master_relationship_history_wide.dta"
drop if _merge==2
tab ever_married _merge, row
drop _merge

// now fill in missing years based on relationship history
browse unique_id survey_yr in_sample_ first_survey_year last_survey_year last_couple_year partnered_imp marst_imp marital_status_focal rel_type master_rel_start1 master_rel_end1 master_rel_type1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_start3 master_rel_end3 master_rel_type3

gen in_rel_flag = 0 // this is how I did below previously (to distinguish from the top row that fills in existing variable)

/*
okay i was trying to recover more, but let's do it the conservative way

tab master_rel_end1, m // I forget the intact are labeled as 9999 but this doesn't mean like STILL partnered if attrited. how do I update these? and it's not necessarily current rel so I can't say for sure based on last couple year. i guess that is the last rel, so if they are in that rel, it's irrelevant bc I WILL restrict to last observed duration later on (I mean some minor cases are missing, but if break up and start new, they try to get end date)

forvalues y=1/9{
	local z=`y'+1 // if `y' <= 9
		
	replace partnered_imp = 1 if partnered_imp==. & survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & survey_yr <= last_couple_year // don't know beyond this point because not collected. think this is particularly problematic bc many end dates are 9999. I am struggling with their last year or last couple year, but it's not working with their last survey year bc people observed and not coupled
	
	replace in_rel_flag = 1 if survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & survey_yr <= last_survey_year
	
	replace marst_imp = 1 if marst_imp==. & survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & master_rel_type`y'==20 & survey_yr <= last_couple_year  & (survey_yr < master_rel_start`z') // marriage. 9999s also causing problems in this way, so need to also make sure end doesn't overlap with next relationship
	replace marst_imp = 2 if marst_imp==. & survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & master_rel_type`y'==22 & survey_yr <= last_couple_year & (survey_yr < master_rel_start`z')  // cohab
	
}

	replace partnered_imp = 1 if partnered_imp==. & survey_yr >= master_rel_start10 & survey_yr <= master_rel_end10 & survey_yr <= last_couple_year
	replace in_rel_flag = 1 if survey_yr >= master_rel_start10 & survey_yr <= master_rel_end10 & survey_yr <= last_survey_year
	replace marst_imp = 1 if marst_imp==. & survey_yr >= master_rel_start10 & survey_yr <= master_rel_end10 & master_rel_type10==20 & survey_yr <= last_couple_year
	replace marst_imp = 2 if marst_imp==. & survey_yr >= master_rel_start10 & survey_yr <= master_rel_end10 & master_rel_type10==22 & survey_yr <= last_couple_year
*/


forvalues y=1/10{
	replace partnered_imp = 1 if partnered_imp==. & survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & master_rel_end`y'<9000
	
	replace in_rel_flag = 1 if survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & (master_rel_end`y'<9000 | (master_rel_end`y'==9999 & survey_yr <= last_survey_year))
	
	replace marst_imp = 1 if marst_imp==. & survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & master_rel_end`y'<9000 & master_rel_type`y'==20 // marriage
	replace marst_imp = 2 if marst_imp==. & survey_yr >= master_rel_start`y' & survey_yr <= master_rel_end`y' & master_rel_end`y'<9000 & master_rel_type`y'==22 // cohab

}

browse unique_id partner_unique_id in_sample_ marst_imp duration min_dur max_dur rel_type rel_type_constant
tab marst_imp if survey_yr>= eligible_rel_start_year & survey_yr <=eligible_rel_end_year , m
	replace marst_imp = 1 if marst_imp==. & survey_yr>= eligible_rel_start_year & survey_yr <=eligible_rel_end_year & rel_type_constant==1
	replace marst_imp = 2 if marst_imp==. & survey_yr>= eligible_rel_start_year & survey_yr <=eligible_rel_end_year & rel_type_constant==2
	replace marst_imp = 2 if rel_type==22 & marst_imp!=2
	
replace partnered_imp=0 if partnered_imp==. & survey_yr < master_rel_start1 & master_rel_start1!=. // not partnered if prior to first rel date
replace marst_imp=3 if marst_imp==. & survey_yr < master_rel_start1 & master_rel_start1!=. // & master_rel_type1==20 // do I need to restrict on marriage? bc I combine histories, if it's before your first relationship, you are never married? because it you were married and then cohab, then marriage would be first relationshop/ if cohab then marriage, you are still never married at this point...

tab partnered_imp in_rel_flag, m
tab partnered_imp in_rel_flag if in_sample_==1, m
replace partnered_imp = 0 if partnered_imp ==. & in_sample_==1 & in_rel_flag==0

// can I fill in POST rel info? well, if intact, that doesn't work if they dropped out (bc we don't know when actually ended). also we generally don't know after they dropped out because could enter another rel
browse unique_id survey_yr marst_imp marital_status_focal first_survey_year last_survey_year last_couple_year eligible_rel_start_year eligible_rel_end_year eligible_rel_status master_rel_start1 master_rel_end1 master_rel_how_end1 master_rel_start2 master_rel_end2 master_rel_how_end2 master_rel_start3 master_rel_end3 master_rel_how_end3

egen last_rel_yr = rowmax(master_rel_end*) // most are ofc 9999

gen how_last_rel_end=.
forvalues y=1/10{
	replace how_last_rel_end = master_rel_how_end`y' if last_rel_yr == master_rel_end`y' & master_rel_end`y'!=. & master_rel_end`y'!=9999 // this ONLY works if not 9999, BUT i think this SHOULDn'T BE 9999 if it actually ended. and I am really only trying to recover this if ended
}

label values how_last_rel_end how_rel_end
tab how_last_rel_end, m //
tab last_rel_yr how_last_rel_end, m // so yes, if NOT 9999, I can recover this.

/* I think with the 9999s and then with cohab, it might not actually be separated, it would be never married if never married, it is too complicated to estimate and I should just impute? I only use this for imputation, this litetally is not used outside of that because i drop years outside of current relationship and this isn't used in getting rel type, so I would rather be more conservative and impute when I don't know than make bad guesses, because end dates of cohab are also so uncertain because it is based on observations that I am taking already estimated data and using it to estimate more data and that is what imputation does MORE SYSTEMATICALLY
replace marst_imp=6 if marst_imp==. & survey_yr > last_rel_yr & how_last_rel_end==1 & last_rel_yr!=9999 & survey_yr <= last_survey_year // let's just put as separated, I think that is fine
replace marst_imp=4 if marst_imp==. & survey_yr > last_rel_yr & how_last_rel_end==2 & last_rel_yr!=9999 & survey_yr <= last_survey_year // widowhood
replace partnered_imp=0 if partnered_imp==. & survey_yr > last_rel_yr & inlist(how_last_rel_end,1,2) & last_rel_yr!=9999 & survey_yr <= last_survey_year
*/

tab marst_imp partnered_imp, m // okay, mostly but not 100% makes sense, and there are some where partnered filled in but marst not (because easier I think to generally recover if partnered, but not which kind)
tab marital_status_focal marst_imp, m
tab marst_imp ever_married ,m

replace partnered_imp = 0 if partnered_imp==. & inrange(marst_imp,3,6)
replace partnered_imp = 1 if partnered_imp==. & inlist(marst_imp,1,2) // this doesn't really affect much but a few rogue ones

browse unique_id survey_yr in_sample_ eligible_rel_start_year eligible_rel_end_year first_survey_year last_survey_year last_couple_year partnered_imp partnered marst_imp marital_status_focal rel_type master_rel_start1 master_rel_end1 master_rel_type1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_start3 master_rel_end3 master_rel_type3 last_rel_yr how_last_rel_end

tab partnered_imp in_sample_, m 
tab marst_imp in_sample_, m 

tab marst_imp if duration==0, m
tab partnered_imp if duration==0, m

tab marst_imp in_sample_ if inrange(duration,-2,12), m
tab partnered_imp in_sample_ if inrange(duration,-2,12), m

// can I identify relationship order based on start date match? (Okay, just noting - this was OLD CODE because I didn't create a rel number previously. In the new way I created history, I did this in step 2). I at first thought this old code was sorting all observed relationships, but it's just calcualting the number using ELIGIBLE rel start year. Leaving this here to remember this, but removing code. NOTE: you will have to replace variable in imputation: replace current_rel_number_main with eligible_rel_no
tab eligible_rel_no, m // has no missing

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
tab NUM_BIRTHS rolling_births

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

browse unique_id partner_unique_id survey_yr NUM_CHILDREN_ AGE_YOUNG_CHILD_ rolling_births increment_birth had_birth individ_birth_ hh_births_est cah_child_birth_yr*

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

tab num_children_imp_hh num_children_imp_focal, m // good alignment, just sometimes the HH has more, which makes sense (because it's kids in HH)
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
* Small other variable updates to either fill in or make fixed
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

sort unique_id partner_unique_id survey_yr
browse unique_id survey_yr relationship_ under18 duration educ_focal_imp educ_focal first_educ_focal max_educ_focal new_in_hh

// need to fix my calculated parent variable now that i filled in off years
capture gen num_parent_in_hh = .
replace num_parent_in_hh = 0 if father_in_hh==0 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==1 & mother_in_hh==0
replace num_parent_in_hh = 1 if father_in_hh==0 & mother_in_hh==1
replace num_parent_in_hh = 2 if father_in_hh==1 & mother_in_hh==1

********************************************************************************
********************************************************************************
**# CHECKPOINT: Here the data is now long, by duration - only the durations I want.
* I then do the FILLIN to rectangularize
********************************************************************************
********************************************************************************

tab duration, m
keep if duration >=-2 // only keeping up to 2 years prior, bc there are so many missing pre duration 0, it's not very stable.
keep if duration <=12 // up to 10/11 for now - but adding a few extra years so I can do the lookups below and still retain up to 20

// replace partnered_imp=. if partnered_imp==.x

save "$created_data/individs_by_duration_long.dta", replace

// 
use "$created_data/individs_by_duration_long.dta", clear

unique unique_id partner_unique_id
egen couple_id = group(unique_id partner_unique_id)
unique couple_id
unique eligible_couple_id
drop if couple_id==.
browse couple_id unique_id partner_unique_id duration SEX

// make it rectangular then fill in fixed characteristics for those added
fillin couple_id duration
tab duration
unique couple_id, by(duration)

********************************************************************************
* Variables to create or fill in for filled in years
********************************************************************************

* Fill in fixed variables
foreach var in unique_id partner_unique_id SEX couple_id eligible_couple eligible_partner eligible_couple_id eligible_rel_start_year rel_start_all_unadj min_dur max_dur eligible_rel_end_year rel_end_all_9999 eligible_rel_status eligible_rel_no  eligible_rel_lc_flag eligible_rel_miss_flag eligible_rel_est_flag current_rel_left_censored current_rel_left_censored_sp both_end_missing exclusion_couples eligible_rel_max_relation eligible_rel_ever_fyc num_rel first_survey_year last_survey_year first_couple_year last_couple_year ended eligible_transition_status eligible_transition_year birth_yr_all raceth_fixed_focal FIRST_BIRTH_YR LAST_BIRTH_YR PSID_COHORT sample_type last_race_focal last_race_focal_check rel_type_constant  main_fam_id SAMPLE has_psid_gene SAMPLE_STATUS_TYPE PERMANENT_ATTRITION ANY_ATTRITION YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST NUM_BIRTHS SEX_sp SAMPLE_sp has_psid_gene_sp permanent_attrit lt_attrit first_educ_focal max_educ_focal mpf_focal ever_parent_focal num_births_focal min_yr_retired_focal max_yr_retired_focal father_max_educ_focal mother_max_educ_focal family_structure_cons_focal father_unique_id FATHER_YR_BORN mother_unique_id MOTHER_YR_BORN master_rel_start1 master_rel_end1 master_rel_type1 master_rel_how_end1 master_rel_left_censored1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_how_end2 master_rel_left_censored2 master_rel_start3 master_rel_end3 master_rel_type3 master_rel_how_end3 master_rel_left_censored3 master_rel_start4 master_rel_end4 master_rel_type4 master_rel_how_end4 master_rel_left_censored4 master_rel_start5 master_rel_end5 master_rel_type5 master_rel_how_end5 master_rel_left_censored5 master_rel_start6 master_rel_end6 master_rel_type6 master_rel_how_end6 master_rel_left_censored6 master_rel_start7 master_rel_end7 master_rel_type7 master_rel_how_end7 master_rel_left_censored7 master_rel_start8 master_rel_end8 master_rel_type8 master_rel_how_end8 master_rel_left_censored8 master_rel_start9 master_rel_end9 master_rel_type9 master_rel_how_end9 master_rel_left_censored9 master_rel_start10 master_rel_end10 master_rel_type10 master_rel_how_end10 master_rel_left_censored10 ever_married MARITAL_STATUS FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_END FIRST_MARRIAGE_STATUS FIRST_SEPARATE_YR NUM_MARRIED RECENT_MARRIAGE_YR_START RECENT_MARRIAGE_STATUS RECENT_MARRIAGE_YR_END RECENT_SEPARATE_YR entered_in_rel history_flag last_rel_yr how_last_rel_end cah_child_birth_yr1 cah_child_birth_yr2 cah_child_birth_yr3 cah_child_birth_yr4 cah_child_birth_yr5 cah_child_birth_yr6 cah_child_birth_yr7 cah_child_birth_yr8 cah_child_birth_yr9 cah_child_birth_yr10 cah_child_birth_yr11 cah_child_birth_yr12 cah_child_birth_yr13 cah_child_birth_yr14 cah_child_birth_yr15 cah_child_birth_yr16 cah_child_birth_yr17 cah_child_birth_yr18 cah_child_birth_yr19 cah_child_birth_yr20 cah_unique_id_child1 cah_unique_id_child2 cah_unique_id_child3 cah_unique_id_child4 cah_unique_id_child5 cah_unique_id_child6 cah_unique_id_child7 cah_unique_id_child8 cah_unique_id_child9 cah_unique_id_child10 cah_unique_id_child11 cah_unique_id_child12 cah_unique_id_child13 cah_unique_id_child14 cah_unique_id_child15 cah_unique_id_child16 cah_unique_id_child17 cah_unique_id_child18 cah_unique_id_child19 cah_unique_id_child20 cah_other_parent_id1 cah_other_parent_id2 cah_other_parent_id3 cah_other_parent_id4 cah_other_parent_id5 cah_other_parent_id6 cah_other_parent_id7 cah_other_parent_id8 cah_other_parent_id9 cah_other_parent_id10 cah_other_parent_id11 cah_other_parent_id12 cah_other_parent_id13 cah_other_parent_id14 cah_other_parent_id15 cah_other_parent_id16 cah_other_parent_id17 cah_other_parent_id18 cah_other_parent_id19 cah_other_parent_id20 cah_any_births cah_num_birth_partners cah_any_mpf{
	bysort couple_id (`var'): replace `var'=`var'[1] if `var'==.
}

* Some variables to create
browse unique_id survey_yr eligible_rel_start_year birth_yr_all age_focal duration if inlist(unique_id,4039,4180,4201,4202,5004,5004,5183,5183,6032,6177,7032)  // illustrate below post-last-survey problem
sort unique_id partner_unique_id duration

// replace survey_yr = survey_yr[_n-1]+1 if survey_yr==.
replace survey_yr = eligible_rel_start_year + duration if survey_yr==. // what i use elsewhere
replace age_focal=survey_yr - birth_yr_all if age_focal==.

// while here, add that indicator I want of first birth relative to this rel start
browse unique_id survey_yr eligible_rel_start_year ever_parent_focal FIRST_BIRTH_YR
browse unique_id survey_yr eligible_rel_start_year ever_parent_focal FIRST_BIRTH_YR cah_child_birth_yr* if ever_parent_focal==1 & FIRST_BIRTH_YR==9999

egen first_birth_yr_calc = rowmin(cah_child_birth_yr*)
bysort unique_id: egen first_birth_yr_calc_dedup = min(first_birth_yr_calc)
tab first_birth_yr_calc_dedup if FIRST_BIRTH_YR==9999

gen first_birth_yr_alt=FIRST_BIRTH_YR
replace first_birth_yr_alt = first_birth_yr_calc_dedup if ever_parent_focal==1 & FIRST_BIRTH_YR==9999 & first_birth_yr_calc_dedup!=9999 & first_birth_yr_calc_dedup!=9998 & first_birth_yr_calc_dedup!=.

gen birth_timing_rel = eligible_rel_start_year - first_birth_yr_alt if first_birth_yr_alt!=9999
replace birth_timing_rel = 9999 if first_birth_yr_alt==9999

// and parent status based on first birth year
browse unique_id survey_yr ever_parent_focal first_birth_yr_alt
tab first_birth_yr_alt ever_parent_focal, m

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

browse unique_id partner_unique_id survey_yr ever_retired min_yr_retired_focal max_yr_retired_focal retired_est_focal empstat_retired_focal if ever_retired==1

// recode respondent_who for non-survey years
tab RESPONDENT_WHO_, m
replace RESPONDENT_WHO_ = 9 if RESPONDENT_WHO_==. & survey_yr>2023

tab is_respondent_focal, m
replace is_respondent_focal = 9 if is_respondent_focal==. | survey_yr>2023

tab RESPONDENT_WHO_ is_respondent_focal, m // I guess bc Respondent WHo is HH level is sometimes you get this info when the person is non-sample

// let's recode housing_status so I can attempt to use ologit
rename house_status_all house_status_all_v0

gen house_status_all = .
replace house_status_all = 0 if house_status_all_v0==3 // neither
replace house_status_all = 1 if house_status_all_v0==2 // rents
replace house_status_all = 2 if house_status_all_v0==1 // owns

label define house_status 0 "Neither" 1 "Rents" 2 "Owns"
label values house_status_all house_status

// also - create binary home ownership because I think "neither" are causing problems...
tab house_status_all, m

gen home_owner=.
replace home_owner=0 if inlist(house_status_all,0,1)
replace home_owner=1 if house_status_all==2

// update employment status based on weekly hours at t (except, I use a more detailed indicator of this later but let's retain for posterity)
tab weekly_hrs_t_focal employed_focal, m
gen employed_focal_rec = employed_focal

replace employed_focal_rec = 1 if employed_focal==0 & weekly_hrs_t_focal > 0 & weekly_hrs_t_focal < 900 & weekly_hrs_t_focal!=. // so, put them as employed if there are hours
replace employed_focal_rec = 1 if employed_focal==. & weekly_hrs_t_focal > 0 & weekly_hrs_t_focal < 900 & weekly_hrs_t_focal!=. // so, put them as employed if there are hours
replace employed_focal_rec = 0 if employed_focal==. & weekly_hrs_t_focal == 0 // so, put them as unemployed if hours are 0 and employment is missing

// get fixed education - either whatever it is if remains, at start of relationship, or earliest observed
tab educ_focal, m
tab educ_focal_imp, m
tab educ_focal educ_focal_imp, m

quietly unique educ_focal_imp if educ_focal_imp!=., by(unique_id) gen(educ_change)
bysort unique_id (educ_change): replace educ_change=educ_change[1]

tab educ_change, m

tab educ_focal_imp if duration == min_dur, m

gen fixed_education = educ_focal_imp if educ_change==1
replace fixed_education = educ_focal_imp if fixed_education==. & duration == min_dur
bysort unique_id (fixed_education): replace fixed_education=fixed_education[1]

tab fixed_education, m // this didn't solve the problem - is it because some people only have missing (is that the 0s I think?)
label values fixed_education educ
tab educ_change fixed_education, m // yes mostly

forvalues d=-1/12{
	replace fixed_education = educ_focal_imp if duration == `d' & fixed_education==.
	bysort unique_id (fixed_education): replace fixed_education=fixed_education[1]
}

sort unique_id survey_yr
tab fixed_education, m

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
histogram weekly_hrs_t_focal, width(1)
histogram weekly_hrs_t_focal if weekly_hrs_t_focal>0, width(1)
histogram housework_focal, width(1)

/*
gen partnered=.
replace partnered=0 if MARITAL_PAIRS_==0
replace partnered=1 if inrange(MARITAL_PAIRS_,1,3)
*/

tabstat weekly_hrs_t_focal housework_focal childcare_focal adultcare_focal employed_focal earnings_t_focal age_focal birth_yr_all educ_focal college_focal fixed_education home_owner raceth_focal raceth_fixed_focal ever_parent_focal children num_children_imp_focal num_children_imp_hh num_births_focal rolling_births FIRST_BIRTH_YR AGE_YOUNG_CHILD_ birth_timing_rel relationship_ partnered_imp marst_imp eligible_rel_no mpf_focal TOTAL_INCOME_T_FAMILY sample_type num_parent_in_hh num_65up_hh disabled_focal sr_health_focal retired_est_focal, stats(mean sd p50) columns(statistics)

********************************************************************************
********************************************************************************
**# CHECKPOINT: reshaping wide for imputation purposes
********************************************************************************
********************************************************************************
// rename transition_yr_est transition_yr

drop survey_yr duration _fillin MARITAL_PAIRS_ *_sp *_sp* cah_* master_* MOVED_YEAR_ MOVED_YEAR_sp_ moved_focal moved_sp any_births_t1_focal any_births_t1_hh any_births_t2_focal any_births_t2_hh *_est SPLITOFF* COMPOSITION_CHANGE_ NUM_IN_HH_ DATA_RECORD_TYPE_  SAMPLE_STATUS_TYPE PERMANENT_ATTRITION ANY_ATTRITION permanent_attrit lt_attrit YR_NONRESPONSE_RECENT YR_NONRESPONSE_FIRST yrs_non_sample change_yr int_number per_num INTERVIEW_NUM_1968 individ_birth_ NUM_BIRTHS first_birth_yr_calc first_birth_yr_calc_dedup  first_birth_yr_alt father_fam_id father_in_sample father_moved father_change_yr mother_fam_id mother_in_sample mother_moved mother_change_yr parent_in_ofum house_status_all_v0 MARITAL_STATUS FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_END FIRST_MARRIAGE_STATUS FIRST_SEPARATE_YR NUM_MARRIED RECENT_MARRIAGE_YR_START RECENT_MARRIAGE_STATUS RECENT_MARRIAGE_YR_END RECENT_SEPARATE_YR partner_id // hh_births_pre1968

reshape wide in_sample_ hh_status_ relationship_  partnered weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal employed_focal_rec max_educ_focal educ_focal educ_focal_imp college_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ hours_type_t1_focal hw_hours_gp raceth_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY childcare_focal adultcare_focal TOTAL_INCOME_T2_FAMILY_ has_hours_t1 has_earnings_t1 has_hours_t2 has_earnings_t2 employed_t1_hrs_focal employed_t1_earn_focal partnered_imp marst_imp num_children_imp_focal num_children_imp_hh rolling_births had_birth hh_births* increment_birth under18 edulevel_match edulevelmax_match new_in_hh disabled_focal empstat_disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal empstat_retired_focal retired_est_focal num_65up_hh age65up house_status_all home_owner moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO_ REGION_ employment_status_focal current_parent_status num_parent_in_hh father_in_hh mother_in_hh RELATION_ relationship_type_ is_respondent_focal is_first_yr_cohabitor has_first_yr_cohabitor rel_type marital_status_focal in_rel_flag ///
, i(couple_id unique_id partner_unique_id eligible_rel_start_year min_dur max_dur eligible_rel_end_year ended SEX) j(duration_rec)

// might need to attempt to fill in more disabled status - causing problems with imputation. 
// Okay note 8/7/26. commenting this out for now. going to see if this works this go round (i litearlly had to completely remove the first time). If it still doesn't work, I will give up, but let's see if more sample helps this

/*
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
*/

**# FINAL CHECKPOINT: Here the data is now reshaped wide, by duration
save "$created_data/individs_by_duration_wide.dta", replace
// use "$created_data\individs_by_duration_wide.dta", clear

// make sure all of these variables exist in final file: eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no rel_type_constant min_dur max_dur last_couple_year ended eligible_transition_year eligible_transition_status eligible_rel_lc_flag eligible_rel_miss_flag eligible_rel_est_flag eligible_rel_max_relation eligible_rel_ever_fyc

********************************************************************************
* Exploration
********************************************************************************
// first, let's just get a sense of missings
unique unique_id
unique unique_id partner_unique_id

browse unique_id partner_unique_id couple_id weekly_hrs_t_focal*
browse unique_id housework_focal*

misstable summarize *focal*, all // they all have missing, but some feel low?? oh I am dumb, I was reading wrong - the right column is where we HAVE data, so the ones that seem low are mostly the t2 variables, which makes sense,bc didn't exist until 1999 okay I actually feel okay about this
misstable summarize *focal0, all // -2? (first time point)
misstable summarize *focal2, all // 0
misstable summarize *focal3, all // t1
misstable summarize *focal14, all // last time point
mdesc *focal*

egen nmis_workhrs = rmiss(weekly_hrs_t_focal*)
tab nmis_workhrs, m
browse nmis_workhrs weekly_hrs_t_focal*

egen nmis_hwhrs = rmiss(housework_focal*)
tab nmis_hwhrs, m

forvalues y=0/14{
	replace weekly_hrs_t_focal`y' = round(weekly_hrs_t_focal`y',1)
}

// I recovered people who were not just heads and wives, but I wonder if the people who were NEVER heads or wives are going to cause problems? because they might have too many missings to impute?
tab eligible_rel_max_relation, m

tab nmis_workhrs eligible_rel_max_relation,m col
tab nmis_hwhrs eligible_rel_max_relation,m col // okay let's TRY to impute with them, but I honestly might need to drop. it's VERY HIGH missing
s
/*
forvalues y=1/16{
	replace hours_type_t_focal`y' = 4 if hours_type_t_focal`y'==.
	replace hours_type_t_focal`y' = 3 if hours_type_t_focal`y'==0
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
mmdesc in_sample_0-fixed_education
putexcel A1 = matrix(r(table))

// sdchronogram hours_type_t1_focal0-hours_type_t1_focal16 // this is not working; I am not sure why

********************************************************************************
**# Before I impute, want to match and explore child variables
********************************************************************************
/*
use "$created_data\individs_by_duration_long.dta", clear

keep unique_id partner_unique_id survey_yr duration eligible_rel_start_year min_dur max_dur NUM_CHILDREN_ num_children_imp_focal num_children_imp_hh had_birth increment_birth cah_child_birth_yr*

save "$temp\kids_long.dta", replace

keep unique_id partner_unique_id survey_yr NUM_CHILDREN_ num_children_imp_focal num_children_imp_hh had_birth increment_birth cah_child_birth_yr*
foreach var in NUM_CHILDREN_ num_children_imp_focal num_children_imp_hh had_birth increment_birth cah_child_birth_yr*{
	rename `var' `var'_sp
}

rename partner_unique_id x
rename unique_id partner_unique_id
rename x unique_id

save "$temp\kids_long_tomatch.dta", replace

use "$temp\kids_long.dta", clear
merge 1:1 unique_id partner_unique_id survey_yr using "$temp\kids_long_tomatch.dta" // okay, so 485 didn't match
drop if _merge ==2
drop _merge

save "$temp\partner_kids_long.dta", replace

tab NUM_CHILDREN_ NUM_CHILDREN__sp, m // these already don't match
browse unique_id partner_unique_id survey_yr duration eligible_rel_start_year min_dur max_dur

tab NUM_CHILDREN_ NUM_CHILDREN__sp if duration>=min_dur & duration<=max_dur, m // okay, this is much closer

// tab num_children_imp_focal num_children_imp_focal_sp, m
tab num_children_imp_focal num_children_imp_focal_sp if duration>=min_dur & duration<=max_dur, m // okay, this is much closer

// tab num_children_imp_hh num_children_imp_hh_sp, m 
tab num_children_imp_hh num_children_imp_hh_sp if duration>=min_dur & duration<=max_dur, m // okay, this is much closer - okay they are basically the same amount of accuracy. here, there are just more kids, which makes sense because it doesn't restrict to JUST the focal people, which is what the variable essentally is

tab had_birth had_birth_sp if duration>=min_dur & duration<=max_dur, m 
tab increment_birth increment_birth_sp if duration>=min_dur & duration<=max_dur, m // so these match a bit more than births above

browse unique_id partner_unique_id survey_yr duration NUM_CHILDREN_ NUM_CHILDREN__sp num_children_imp_focal num_children_imp_focal_sp num_children_imp_hh num_children_imp_hh_sp had_birth had_birth_sp increment_birth increment_birth_sp cah_child_birth_yr1 cah_child_birth_yr1_sp
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