********************************************************************************
* Recodes needed for education
********************************************************************************
/// truncated - to match main

use "$created_data/ukhls_couples_wide_truncated.dta", clear

// this is what I do in describe clusters step of life course

tab hiqual_fixed, m

gen education_man=hiqual_fixed if SEX==1
replace education_man=hiqual_fixed_sp if SEX==2
replace education_man = 6 if education_man == 9 // so they are consecutive

gen education_woman=hiqual_fixed if SEX==2
replace education_woman=hiqual_fixed_sp if SEX==1
replace education_woman = 6 if education_woman == 9 

capture label define hiqual_x 1 "Degree" 2 "Other Higher Degree" 3 "A level" 4 "GCSE" 5 "Other qual" 6 "No qual"
label values education_man education_woman hiqual_x

tab education_man education_woman, m

gen couple_educ_type=.
replace couple_educ_type = 1 if inrange(education_man,2,6) & inrange(education_woman,2,6)
replace couple_educ_type = 2 if education_man==1 & inrange(education_woman,2,6)
replace couple_educ_type = 3 if inrange(education_man,2,6) & education_woman==1
replace couple_educ_type = 4 if education_man==1 & education_woman==1

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values couple_educ_type educ_type

tab couple_educ_type, m

gen one_college=.
replace one_college = 0 if couple_educ_type==1 
replace one_college = 1 if inrange(couple_educ_type,2,4)

// also create indicators needed to get life course parental status
// first birth timing relative to relationship start
browse pidp eligible_partner SEX eligible_rel_start_year yr_first_birth_woman yr_first_birth_man birth_timing_rel birth_timing_rel_sp _mi_m // I imputed first birth, so I think I need to recalculate first birth timing? 
	// also, see notes from PSID, but I had done before as relationship - first birth and I want the opposite (first birth - relationship)

mi passive: gen first_birth_timing_man = yr_first_birth_man - eligible_rel_start_year
mi passive: replace first_birth_timing_man = . if yr_first_birth_man==9999

mi passive: gen first_birth_timing_woman = yr_first_birth_woman - eligible_rel_start_year
mi passive: replace first_birth_timing_woman = . if yr_first_birth_woman==9999 // need to account for this better, but don't want the 9999 skewing.

// browse pidp eligible_partner SEX eligible_rel_start_year yr_first_birth_woman yr_first_birth_man birth_timing_rel birth_timing_rel_sp _mi_m first_birth_timing_woman first_birth_timing_man

// also create a binary of pre / post? I think so yeah
mi passive: gen first_birth_pre_rel_man = .
mi passive: replace first_birth_pre_rel_man = 0 if first_birth_timing_man >=0 & first_birth_timing_man!=.
mi passive: replace first_birth_pre_rel_man = 0 if yr_first_birth_man==9999 // can actually put 9999 here because is, theoretically, 0 if no births
mi passive: replace first_birth_pre_rel_man = 1 if first_birth_timing_man <0 & first_birth_timing_man!=.

tab first_birth_timing_man first_birth_pre_rel_man, m
tab yr_first_birth_man first_birth_pre_rel_man, m

mi passive: gen first_birth_pre_rel_woman = .
mi passive: replace first_birth_pre_rel_woman = 0 if first_birth_timing_woman >=0 & first_birth_timing_woman!=.
mi passive: replace first_birth_pre_rel_woman = 0 if yr_first_birth_woman==9999 // can actually put 9999 here because is, theoretically, 0 if no births
mi passive: replace first_birth_pre_rel_woman = 1 if first_birth_timing_woman <0 & first_birth_timing_woman!=.

tab first_birth_timing_woman first_birth_pre_rel_woman, m
tab yr_first_birth_woman first_birth_pre_rel_woman, m
tab first_birth_pre_rel_man first_birth_pre_rel_woman

mi passive: gen either_birth_pre_rel = .
mi passive: replace either_birth_pre_rel = 0 if first_birth_pre_rel_man==0 & first_birth_pre_rel_woman==0
mi passive: replace either_birth_pre_rel = 1 if first_birth_pre_rel_man==1 | first_birth_pre_rel_woman==1
tab either_birth_pre_rel, m

// Then alt versions
* Childfree v. Have Child at rel start (v. based on timing of first birth)
mi passive: gen parent_status_t1=.
mi passive: replace parent_status_t1=0 if inlist(family_type_end1,1,5)
mi passive: replace parent_status_t1=1 if inlist(family_type_end1,2,3,4,6,7,8)

tab family_type_end1 parent_status_t1
tab parent_status_t1 either_birth_pre_rel // see these are NOT congruent.
tab first_birth_timing_woman if either_birth_pre_rel==1 & parent_status_t1== 0 // but this makes more sense here than in US. think the US just odd.

* More detailed - childfree, had child at start, had child over life course. Again based on PRESENCE of children, not parental status [this will likely raise concerns, though]
browse pid family_type_end* couple_num_children_gp_end*

forvalues d=1/11{
	mi passive: gen have_children`d' = .
	mi passive: replace have_children`d' = 0 if couple_num_children_gp_end`d'==0
	mi passive: replace have_children`d' = 1 if inrange(couple_num_children_gp_end`d',1,3)
}

browse pid couple_num_children_gp_end* have_children*
tab couple_num_children_gp_end1 have_children1

mi passive: egen num_children_check = rowtotal(have_children*) // this is obviously not real, moreso to see who remains childfree v. who ever has kids
	// browse unique_id num_children_check couple_num_children_gp_end* have_children*
tab couple_num_children_gp_end1 num_children_check
tab couple_num_children_gp_end10 num_children_check

tab num_children_check parent_status_t1

mi passive: gen parent_info = . 
mi passive: replace parent_info = 0 if parent_status_t1== 0 & num_children_check== 0 // always no children
mi passive: replace parent_info = 1 if parent_status_t1== 0 & inrange(num_children_check,1,15) // transition to children
mi passive: replace parent_info = 2 if parent_status_t1== 1 // always children (or...using this as child at start, moreso to distiguish CF at start - always or not)

label define parent_info 0 "Always CF" 1 "Become Parent" 2 "Always Parent"
label values parent_info parent_info

tab parent_info, m

save "$temp/ukhls_couples_wide_truncated_educ.dta", replace

/// complete - might be needed for this to be effective

use "$created_data/ukhls_couples_imputed_wide_complete.dta", clear

tab hiqual_fixed, m

gen education_man=hiqual_fixed if SEX==1
replace education_man=hiqual_fixed_sp if SEX==2
replace education_man = 6 if education_man == 9 // so they are consecutive

gen education_woman=hiqual_fixed if SEX==2
replace education_woman=hiqual_fixed_sp if SEX==1
replace education_woman = 6 if education_woman == 9 

capture label define hiqual_x 1 "Degree" 2 "Other Higher Degree" 3 "A level" 4 "GCSE" 5 "Other qual" 6 "No qual"
label values education_man education_woman hiqual_x

tab education_man education_woman, m

gen couple_educ_type=.
replace couple_educ_type = 1 if inrange(education_man,2,6) & inrange(education_woman,2,6)
replace couple_educ_type = 2 if education_man==1 & inrange(education_woman,2,6)
replace couple_educ_type = 3 if inrange(education_man,2,6) & education_woman==1
replace couple_educ_type = 4 if education_man==1 & education_woman==1

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values couple_educ_type educ_type

tab couple_educ_type, m

gen one_college=.
replace one_college = 0 if couple_educ_type==1 
replace one_college = 1 if inrange(couple_educ_type,2,4)

save "$temp/ukhls_couples_imputed_wide_complete_tmp.dta", replace
