********************************************************************************
* Recodes needed for education
********************************************************************************
/// truncated - to match main

use "$created_data/gsoep_couples_wide_truncated.dta", clear

tab edu4_fixed_woman edu4_fixed_man, m

gen couple_educ_type = .
replace couple_educ_type = 1 if inrange(edu4_fixed_woman,1,3) & inrange(edu4_fixed_man,1,3)
replace couple_educ_type = 2 if inrange(edu4_fixed_woman,1,3) & edu4_fixed_man==4
replace couple_educ_type = 3 if edu4_fixed_woman==4 & inrange(edu4_fixed_man,1,3)
replace couple_educ_type = 4 if edu4_fixed_woman==4 & edu4_fixed_man==4

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values couple_educ_type educ_type

gen one_college=.
replace one_college = 0 if couple_educ_type==1 
replace one_college = 1 if inrange(couple_educ_type,2,4)

// okay, want to briefly look at the pre-marital birth indicators. Currently create in step 1 but let's temprarily move here
tab first_birth_year, m // no missing, this is not imputed (there were small amt missing I believe and dropped)
browse pid eligible_partner SEX eligible_rel_start_year _mi_m first_birth_year first_birth_year_sp birth_timing_rel birth_timing_rel_sp
	
gen first_birth_timing_man = first_birth_year_sp - eligible_rel_start_year
replace first_birth_timing_man = . if first_birth_year_sp==9999

gen first_birth_timing_woman = first_birth_year - eligible_rel_start_year
replace first_birth_timing_woman = . if first_birth_year==9999

// create a binary of pre / post
gen first_birth_pre_rel_man = .
replace first_birth_pre_rel_man = 0 if first_birth_timing_man >=0 & first_birth_timing_man!=.
replace first_birth_pre_rel_man = 0 if first_birth_year_sp==9999 // can actually put 9999 here because is, theoretically, 0 if no births
replace first_birth_pre_rel_man = 1 if first_birth_timing_man <0 & first_birth_timing_man!=.

tab first_birth_timing_man first_birth_pre_rel_man, m
tab first_birth_year_sp first_birth_pre_rel_man, m

gen first_birth_pre_rel_woman = .
replace first_birth_pre_rel_woman = 0 if first_birth_timing_woman >=0 & first_birth_timing_woman!=.
replace first_birth_pre_rel_woman = 0 if first_birth_year==9999 // can actually put 9999 here because is, theoretically, 0 if no births
replace first_birth_pre_rel_woman = 1 if first_birth_timing_woman <0 & first_birth_timing_woman!=.

tab first_birth_timing_woman first_birth_pre_rel_woman, m
tab first_birth_year first_birth_pre_rel_woman, m

tab first_birth_pre_rel_man first_birth_pre_rel_woman
gen either_birth_pre_rel = .
replace either_birth_pre_rel = 0 if first_birth_pre_rel_man==0 & first_birth_pre_rel_woman==0
replace either_birth_pre_rel = 1 if first_birth_pre_rel_man==1 | first_birth_pre_rel_woman==1
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

save "$temp/gsoep_couples_wide_truncated_tmp.dta", replace

/// complete - might be needed for this to be effective

use "$created_data/gsoep_couples_imputed_wide_complete.dta", clear

tab edu4_fixed_woman edu4_fixed_man, m

gen couple_educ_type = .
replace couple_educ_type = 1 if inrange(edu4_fixed_woman,1,3) & inrange(edu4_fixed_man,1,3)
replace couple_educ_type = 2 if inrange(edu4_fixed_woman,1,3) & edu4_fixed_man==4
replace couple_educ_type = 3 if edu4_fixed_woman==4 & inrange(edu4_fixed_man,1,3)
replace couple_educ_type = 4 if edu4_fixed_woman==4 & edu4_fixed_man==4

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values couple_educ_type educ_type

gen one_college=.
replace one_college = 0 if couple_educ_type==1 
replace one_college = 1 if inrange(couple_educ_type,2,4)

save "$temp/gsoep_couples_imputed_wide_complete_tmp.dta", replace
