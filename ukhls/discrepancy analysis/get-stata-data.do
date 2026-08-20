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

save "$temp/ukhls_couples_wide_truncated_tmp.dta", replace

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
