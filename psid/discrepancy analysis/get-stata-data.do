********************************************************************************
* Recodes needed for education
********************************************************************************
/// truncated - to match main

use "$created_data/psid_couples_wide_truncated.dta", clear

label values fixed_educ* educ

tab fixed_education fixed_education_sp, m

* need gendered versions kim
gen education_woman=fixed_education if SEX==2
replace education_woman=fixed_education_sp if SEX==1

gen education_man=fixed_education if SEX==1
replace education_man=fixed_education_sp if SEX==2

label values education_woman education_man educ
tab education_woman education_man, m

gen couple_educ_type = .
replace couple_educ_type = 1 if inrange(education_woman,1,3) & inrange(education_man,1,3)
replace couple_educ_type = 2 if inrange(education_woman,1,3) & education_man==4
replace couple_educ_type = 3 if education_woman==4 & inrange(education_man,1,3)
replace couple_educ_type = 4 if education_woman==4 & education_man==4

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values couple_educ_type educ_type

gen one_college=.
replace one_college = 0 if couple_educ_type==1 
replace one_college = 1 if inrange(couple_educ_type,2,4)

* Childfree v. Have Child at rel start (v. based on timing of first birth)
mi passive: gen parent_status_t1=.
mi passive: replace parent_status_t1=0 if inlist(family_type_end1,1,5)
mi passive: replace parent_status_t1=1 if inlist(family_type_end1,2,3,4,6,7,8)

tab family_type_end1 parent_status_t1
tab parent_status_t1 either_birth_pre_rel // see these are NOT congruent.
tab first_birth_timing_woman if either_birth_pre_rel==1 & parent_status_t1== 0

* More detailed - childfree, had child at start, had child over life course. Again based on PRESENCE of children, not parental status [this will likely raise concerns, though]
browse unique_id family_type_end* couple_num_children_gp_end*

forvalues d=1/11{
	mi passive: gen have_children`d' = .
	mi passive: replace have_children`d' = 0 if couple_num_children_gp_end`d'==0
	mi passive: replace have_children`d' = 1 if inrange(couple_num_children_gp_end`d',1,3)
}

browse unique_id couple_num_children_gp_end* have_children*
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

save "$temp/psid_couples_wide_truncated_educ.dta", replace

/// complete - might be needed for this to be effective

use "$created_data/psid_couples_imputed_wide_complete.dta", clear

label values fixed_educ* educ

tab fixed_education fixed_education_sp, m

* need gendered versions kim wtf
gen education_woman=fixed_education if SEX==2
replace education_woman=fixed_education_sp if SEX==1

gen education_man=fixed_education if SEX==1
replace education_man=fixed_education_sp if SEX==2

label values education_woman education_man educ
tab education_woman education_man, m

gen couple_educ_type = .
replace couple_educ_type = 1 if inrange(education_woman,1,3) & inrange(education_man,1,3)
replace couple_educ_type = 2 if inrange(education_woman,1,3) & education_man==4
replace couple_educ_type = 3 if education_woman==4 & inrange(education_man,1,3)
replace couple_educ_type = 4 if education_woman==4 & education_man==4

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values couple_educ_type educ_type

gen one_college=.
replace one_college = 0 if couple_educ_type==1 
replace one_college = 1 if inrange(couple_educ_type,2,4)

save "$temp/psid_couples_imputed_wide_complete_educ.dta", replace
