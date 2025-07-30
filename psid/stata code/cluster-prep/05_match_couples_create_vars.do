
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: match_couples_create_vars
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes the imputed data from step 3 at an individual level and
* matches the corresponding imputed partner data.
* It also creates couple-level variables.

use "$created_data/psid_individs_imputed_long_bysex", clear

mi rename family_structure_cons_focal family_structure_focal // too long

********************************************************************************
* Match couples
********************************************************************************
// browse unique_id partner_id duration_rec _mi_miss _mi_m _mi_id imputed

// just keep necessary variables

local partnervars "weekly_hrs_t_focal housework_focal employment_status_focal earnings_t_focal age_young_child num_children_imp_hh partnered_imp family_income_t relationship in_sample hh_status FIRST_BIRTH_YR LAST_BIRTH_YR age_focal birth_yr_all SEX raceth_fixed_focal sample_type SAMPLE has_psid_gene fixed_education max_educ_focal childcare_focal adultcare_focal new_in_hh rolling_births had_birth first_survey_yr_focal last_survey_yr_focal imputed num_65up_hh num_parent_in_hh REGION lives_family_focal house_status_all religion_focal disabled_focal sr_health_focal retired_est_focal father_max_educ_focal mother_max_educ_focal family_structure_focal ever_parent_focal current_parent_status birth_timing_rel current_rel_number_main RESPONDENT_WHO mpf_focal"

keep unique_id partner_id rel_start_all rel_end_all duration_rec _mi_miss _mi_m _mi_id  `partnervars'
mi rename partner_id x
mi rename unique_id partner_id
mi rename x unique_id // sp swap unique and partner to match (bc need to know it's the same couple / duration). I guess I could merge on rel_start_all as well

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	mi rename `var' `var'_sp
}

mi update

save "$temp/partner_data_imputed.dta", replace
unique unique_id partner_id
browse unique_id partner_id rel_start_all rel_end_all duration_rec weekly_hrs_t_focal if inlist(unique_id, 16032, 16176, 18037, 18197) | inlist(partner_id, 16032, 16176, 18037, 18197)

// match on partner id and duration_rec
use "$created_data/psid_individs_imputed_long_bysex", clear
mi rename family_structure_cons_focal family_structure_focal // too long

// browse unique_id partner_id rel_start_all rel_end_all duration_rec weekly_hrs_t_focal if inlist(unique_id, 16032, 16176, 18037, 18197) | inlist(partner_id, 16032, 16176, 18037, 18197)

mi merge 1:1 unique_id partner_id duration_rec using "$temp/partner_data_imputed.dta", keep(match) //gen(howmatch) // rel_start_all rel_end_all 

// browse unique_id partner_id rel_start_all rel_end_all duration_rec weekly_hrs_t_focal weekly_hrs_t_focal_sp if howmatch!=3
// browse unique_id partner_id rel_start_all rel_end_all duration_rec howmatch weekly_hrs_t_focal weekly_hrs_t_focal_sp if inlist(unique_id, 16032, 18037, 5579003) | inlist(partner_id, 16032, 18037, 5579003)

mi update

save "$created_data/psid_couples_imputed_long.dta", replace
unique unique_id partner_id

label values duration_rec .
browse unique_id partner_id duration_rec SEX SEX_sp weekly_hrs_t_focal weekly_hrs_t_focal_sp _mi_m

drop if SEX==2 & SEX_sp==2
drop if SEX==1 & SEX_sp==1

// only imputed durations 1-10 (aka 2-12). so let's restrict to that here (used to occur later in file)
gen duration = duration_rec - 2
browse duration duration_rec

drop if duration < 0 | duration > 10
drop duration_rec

mi update

********************************************************************************
**# Create variables
********************************************************************************
// capture drop weekly_hrs_woman weekly_hrs_man housework_woman housework_man partnered_woman partnered_man num_children_woman num_children_man
// mi update

// first, let's make gendered versions of each variable
*paid work
mi passive: gen weekly_hrs_woman=weekly_hrs_t_focal if SEX==2
mi passive: replace weekly_hrs_woman=weekly_hrs_t_focal_sp if SEX==1

mi passive: gen weekly_hrs_man=weekly_hrs_t_focal if SEX==1
mi passive: replace weekly_hrs_man=weekly_hrs_t_focal_sp if SEX==2

*detailed employment status
mi passive: gen employment_status_woman=employment_status_focal if SEX==2
mi passive: replace employment_status_woman=employment_status_focal_sp if SEX==1

mi passive: gen employment_status_man=employment_status_focal if SEX==1
mi passive: replace employment_status_man=employment_status_focal_sp if SEX==2.

*annual earnings
mi passive: gen annual_earnings_woman=earnings_t_focal if SEX==2
mi passive: replace annual_earnings_woman=earnings_t_focal_sp if SEX==1

mi passive: gen annual_earnings_man=earnings_t_focal if SEX==1
mi passive: replace annual_earnings_man=earnings_t_focal_sp if SEX==2

*unpaid work
mi passive: gen housework_woman=housework_focal if SEX==2
mi passive: replace housework_woman=housework_focal_sp if SEX==1

mi passive: gen housework_man=housework_focal if SEX==1
mi passive: replace housework_man=housework_focal_sp if SEX==2

*relationship status
tab partnered_imp partnered_imp_sp 

mi passive: gen partnered_woman=partnered_imp if SEX==2
mi passive: replace partnered_woman=partnered_imp_sp if SEX==1

mi passive: gen partnered_man=partnered_imp if SEX==1
mi passive: replace partnered_man=partnered_imp_sp if SEX==2

gen rel_no_woman=current_rel_number_main if SEX==2
replace rel_no_woman=current_rel_number_main_sp if SEX==1

gen rel_no_man=current_rel_number_main if SEX==1
replace rel_no_man=current_rel_number_main_sp if SEX==2

*number of children
tab num_children_imp_hh num_children_imp_hh_sp if partnered_imp==1 & partnered_imp_sp==1 // hmm - so they don't always have the same number of children...
tab num_children_imp_hh num_children_imp_hh_sp if partnered_imp==1 & partnered_imp_sp==1 & imputed==0 // much closer for non-imputed, so some of this is the imputation
tab NUM_CHILDREN_ num_children_imp_hh, m

mi passive: gen num_children_woman=num_children_imp_hh if SEX==2
mi passive: replace num_children_woman=num_children_imp_hh_sp if SEX==1

mi passive: gen num_children_man=num_children_imp_hh if SEX==1
mi passive: replace num_children_man=num_children_imp_hh_sp if SEX==2

mi passive: gen age_youngest_woman=age_young_child if SEX==2
mi passive: replace age_youngest_woman=age_young_child_sp if SEX==1

mi passive: gen age_youngest_man=age_young_child if SEX==1
mi passive: replace age_youngest_man=age_young_child_sp if SEX==2

* Year of first birth // not imputed so don't need mi passive
gen yr_first_birth_woman=FIRST_BIRTH_YR if SEX==2
replace yr_first_birth_woman=FIRST_BIRTH_YR_sp if SEX==1

gen yr_first_birth_man=FIRST_BIRTH_YR if SEX==1
replace yr_first_birth_man=FIRST_BIRTH_YR_sp if SEX==2

* Current parent status // not imputed so don't need mi passive
gen is_parent_woman=current_parent_status if SEX==2
replace is_parent_woman=current_parent_status_sp if SEX==1

gen is_parent_man=current_parent_status if SEX==1
replace is_parent_man=current_parent_status_sp if SEX==2

* Ever parent status // not imputed so don't need mi passive
gen ever_parent_woman=ever_parent_focal if SEX==2
replace ever_parent_woman=ever_parent_focal_sp if SEX==1

gen ever_parent_man=ever_parent_focal if SEX==1
replace ever_parent_man=ever_parent_focal_sp if SEX==2

// some demographics
* Region
mi passive: gen region_woman=REGION_ if SEX==2
mi passive: replace region_woman=REGION_sp if SEX==1

mi passive: gen region_man=REGION_ if SEX==1
mi passive: replace region_man=REGION_sp if SEX==2

* Housing status
mi passive: gen housing_woman=house_status_all if SEX==2
mi passive: replace housing_woman=house_status_all_sp if SEX==1

mi passive: gen housing_man=house_status_all if SEX==1
mi passive: replace housing_man=house_status_all_sp if SEX==2

* Religion
mi passive: gen religion_woman=religion_focal if SEX==2
mi passive: replace religion_woman=religion_focal_sp if SEX==1

mi passive: gen religion_man=religion_focal if SEX==1
mi passive: replace religion_man=religion_focal_sp if SEX==2

*Disability status
mi passive: gen disabled_woman=disabled_focal if SEX==2
mi passive: replace disabled_woman=disabled_focal_sp if SEX==1

mi passive: gen disabled_man=disabled_focal if SEX==1
mi passive: replace disabled_man=disabled_focal_sp if SEX==2

* Self-rated health
mi passive: gen sr_health_woman=sr_health_focal if SEX==2
mi passive: replace sr_health_woman=sr_health_focal_sp if SEX==1

mi passive: gen sr_health_man=sr_health_focal if SEX==1
mi passive: replace sr_health_man=sr_health_focal_sp if SEX==2

* Retirement status (based on year)
mi passive: gen retired_woman=retired_est_focal if SEX==2
mi passive: replace retired_woman=retired_est_focal_sp if SEX==1

mi passive: gen retired_man=retired_est_focal if SEX==1
mi passive: replace retired_man=retired_est_focal_sp if SEX==2

* Father education
mi passive: gen father_educ_woman=father_max_educ_focal if SEX==2
mi passive: replace father_educ_woman=father_max_educ_focal_sp if SEX==1

mi passive: gen father_educ_man=father_max_educ_focal if SEX==1
mi passive: replace father_educ_man=father_max_educ_focal_sp if SEX==2

* Mother education
mi passive: gen mother_educ_woman=mother_max_educ_focal if SEX==2
mi passive: replace mother_educ_woman=mother_max_educ_focal_sp if SEX==1

mi passive: gen mother_educ_man=mother_max_educ_focal if SEX==1
mi passive: replace mother_educ_man=mother_max_educ_focal_sp if SEX==2

* Family structure growing up
mi passive: gen family_structure_woman=family_structure_focal if SEX==2
mi passive: replace family_structure_woman=family_structure_focal_sp if SEX==1

mi passive: gen family_structure_man=family_structure_focal if SEX==1
mi passive: replace family_structure_man=family_structure_focal_sp if SEX==2

* Lives where grew up
mi passive: gen lives_near_fam_woman=lives_family_focal if SEX==2
mi passive: replace lives_near_fam_woman=lives_family_focal_sp if SEX==1

mi passive: gen lives_near_fam_man=lives_family_focal if SEX==1
mi passive: replace lives_near_fam_man=lives_family_focal_sp if SEX==2

* Year of own birth // not imputed so don't need mi passive
gen dob_woman=birth_yr_all if SEX==2
replace dob_woman=birth_yr_all_sp if SEX==1

gen dob_man=birth_yr_all if SEX==1
replace dob_man=birth_yr_all_sp if SEX==2

// Stata assert command to check new variables created from imputed  
foreach var in weekly_hrs_woman weekly_hrs_man employment_status_woman employment_status_man annual_earnings_woman annual_earnings_man housework_woman housework_man partnered_woman partnered_man rel_no_woman rel_no_man num_children_woman num_children_man age_youngest_woman age_youngest_man yr_first_birth_woman yr_first_birth_man is_parent_woman is_parent_man ever_parent_woman ever_parent_man region_woman region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man father_educ_woman father_educ_man mother_educ_woman mother_educ_man family_structure_woman family_structure_man lives_near_fam_woman lives_near_fam_man dob_woman dob_man{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

**# Bookmark #1
// temp save 1
save "$created_data/psid_couples_imputed_long_recoded.dta", replace

// paid work
mi passive: gen ft_pt_woman = .
mi passive: replace ft_pt_woman = 0 if weekly_hrs_woman==0 // not working
mi passive: replace ft_pt_woman = 1 if weekly_hrs_woman > 0 & weekly_hrs_woman < 35 // PT
mi passive: replace ft_pt_woman = 2 if weekly_hrs_woman >=35 & weekly_hrs_woman < 150 // FT

mi passive: gen overwork_woman=. // Cha and Weeden = 50 hrs, Cha 2010 = 50 and 60, Munsch = 60
mi passive: replace overwork_woman = 0 if weekly_hrs_woman >= 0 & weekly_hrs_woman < 50
mi passive: replace overwork_woman = 1 if weekly_hrs_woman >=50 & weekly_hrs_woman < 150 

mi passive: gen ft_pt_man = .
mi passive: replace ft_pt_man = 0 if weekly_hrs_man==0 // not working
mi passive: replace ft_pt_man = 1 if weekly_hrs_man > 0 & weekly_hrs_man < 35 // PT
mi passive: replace ft_pt_man = 2 if weekly_hrs_man >=35 & weekly_hrs_man < 150 // FT

mi passive: gen overwork_man=. // Cha and Weeden = 50 hrs, Cha 2010 = 50 and 60, Munsch = 60
mi passive: replace overwork_man = 0 if weekly_hrs_man >= 0 & weekly_hrs_man < 50
mi passive: replace overwork_man = 1 if weekly_hrs_man >=50 & weekly_hrs_man < 150 

label define ft_pt 0 "Not working" 1 "PT" 2 "FT"
label values ft_pt_woman ft_pt_man ft_pt

tab ft_pt_woman overwork_woman
tab ft_pt_man overwork_man

mi estimate: proportion ft_pt_woman ft_pt_man

histogram weekly_hrs_woman if ft_pt_woman==1 
sum weekly_hrs_woman if ft_pt_woman==1, det
histogram weekly_hrs_man if ft_pt_man==1 
sum weekly_hrs_man if ft_pt_man==1, det

twoway (histogram weekly_hrs_woman if ft_pt_woman==1, width(1) color(pink%30)) (histogram weekly_hrs_man if ft_pt_man==1, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours among PT Workers")

* more detailed breakdown and men's and women's work
sum weekly_hrs_woman if ft_pt_woman==1, det
mi passive: gen ft_pt_det_woman = .
mi passive: replace ft_pt_det_woman = 0 if weekly_hrs_woman==0 // not working
mi passive: replace ft_pt_det_woman = 1 if weekly_hrs_woman > 0 & weekly_hrs_woman < 20 // PT: low - either do median using r(p50) or use 20 and cite K&Z? doing the latter for now
mi passive: replace ft_pt_det_woman = 2 if weekly_hrs_woman >= 20 & weekly_hrs_woman < 35 // PT: high
mi passive: replace ft_pt_det_woman = 3 if weekly_hrs_woman >=35 & weekly_hrs_woman < 50 // FT: normal
mi passive: replace ft_pt_det_woman = 4 if weekly_hrs_woman >=50 & weekly_hrs_woman < 150 // FT: overwork

mi passive: gen ft_pt_det_man = .
mi passive: replace ft_pt_det_man = 0 if weekly_hrs_man==0 // not working
mi passive: replace ft_pt_det_man = 1 if weekly_hrs_man > 0 & weekly_hrs_man < 20 // PT: low - either do median using r(p50) or use 20 and cite K&Z?
mi passive: replace ft_pt_det_man = 2 if weekly_hrs_man >= 20 & weekly_hrs_man < 35 // PT: high
mi passive: replace ft_pt_det_man = 3 if weekly_hrs_man >=35 & weekly_hrs_man < 50 // FT: normal
mi passive: replace ft_pt_det_man = 4 if weekly_hrs_man >=50 & weekly_hrs_man < 150 // FT: overwork

label define ft_pt_det 0 "not working" 1 "PT < 20hrs" 2 "PT 20-35" 3 "FT: Normal" 4 "FT: OW"
label values ft_pt_det_woman ft_pt_det_man ft_pt_det

mi estimate: proportion ft_pt_det_woman ft_pt_det_man

* couple-level version
tab ft_pt_woman ft_pt_man

mi passive: gen couple_work=.
mi passive: replace couple_work = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work = 3 if ft_pt_man == 2 & ft_pt_woman == 2
mi passive: replace couple_work = 4 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work = 4 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work = 5 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work = 5 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work = 5 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work = 5 if ft_pt_man == 1 & ft_pt_woman == 0

label define couple_work 1 "male bw" 2 "1.5 male bw" 3 "dual FT" 4 "female bw" 5 "under work"
label values couple_work couple_work

mi estimate: proportion couple_work

* with overwork
mi passive: gen couple_work_ow_detailed=.
mi passive: replace couple_work_ow_detailed = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work_ow_detailed = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work_ow_detailed = 3 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==0
mi passive: replace couple_work_ow_detailed = 4 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==1 & overwork_woman==0
mi passive: replace couple_work_ow_detailed = 5 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==1
mi passive: replace couple_work_ow_detailed = 6 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==1 & overwork_woman==1
mi passive: replace couple_work_ow_detailed = 7 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work_ow_detailed = 7 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 1 & ft_pt_woman == 0

label define couple_work_ow_detailed 1 "male bw" 2 "1.5 male bw" 3 "dual FT: no OW" 4 "dual FT: his OW" 5 "dual FT: her OW" 6 "dual FT: both OW" /// 
7 "female bw" 8 "under work"
label values couple_work_ow_detailed couple_work_ow_detailed

mi estimate: proportion couple_work_ow_detailed

// rename couple_work_ow couple_work_ow_detailed
// rename couple_work_ow_end couple_work_ow_detailed_end

* Consolidated overwork version (new - 5/21/25)
mi passive: gen couple_work_ow=.
mi passive: replace couple_work_ow = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work_ow = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 3 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==0
mi passive: replace couple_work_ow = 4 if ft_pt_man == 2 & ft_pt_woman == 2 & (overwork_man==1 | overwork_woman==1)
mi passive: replace couple_work_ow = 5 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work_ow = 5 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work_ow = 6 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 6 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work_ow = 6 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 6 if ft_pt_man == 1 & ft_pt_woman == 0

label define couple_work_ow 1 "male bw" 2 "1.5 male bw" 3 "dual FT: no OW" 4 "dual FT: any OW" 5 "female bw" 6 "under work"
label values couple_work_ow couple_work_ow

mi estimate: proportion couple_work_ow_detailed couple_work_ow

// unpaid work
mi passive: egen couple_hw_total = rowtotal(housework_woman housework_man)
mi passive: gen woman_hw_share = housework_woman / couple_hw_total // this does have missing I think is couple HW total is 0

sum housework_woman, det
sum housework_woman if housework_woman!=0, det
mi passive: egen hw_terc_woman = cut(housework_woman) if housework_woman!=0, group(3) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_terc_woman
tabstat housework_woman, by(hw_terc_woman)

mi passive: egen hw_hilow_woman = cut(housework_woman) if housework_woman!=0, group(2) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_hilow_woman
tabstat housework_woman, by(hw_hilow_woman)

mi passive: egen hw_hilow_man = cut(housework_man) if housework_man!=0, group(2) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_hilow_man
tabstat housework_man, by(hw_hilow_man)

sum housework_man, det
sum woman_hw_share, det

mi passive: gen couple_hw=.
mi passive: replace couple_hw = 1 if woman_hw_share==1
mi passive: replace couple_hw = 2 if woman_hw_share > 0.60 & woman_hw_share < 1
mi passive: replace couple_hw = 3 if woman_hw_share >= 0.40 & woman_hw_share <= 0.60
mi passive: replace couple_hw = 4 if woman_hw_share < 0.40
mi passive: replace couple_hw = 3 if housework_woman==0 & housework_man==0  // neither is so small, just put in equal

label define couple_hw 1 "Woman All" 2 "Woman Most" 3 "Equal" 4 "Man Most" 5 "Neither HW"
label values couple_hw couple_hw

mi estimate: proportion couple_hw

** investigating equal HW
histogram couple_hw_total if couple_hw==3 & couple_hw_total < 100
sum couple_hw_total if couple_hw==3, det
histogram housework_woman if couple_hw==3 & housework_woman < 50
sum housework_woman if couple_hw==3, det
histogram housework_man if couple_hw==3 & housework_man < 50
sum housework_man if couple_hw==3, det

twoway (histogram housework_woman if couple_hw==3 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_man if couple_hw==3 & housework_man < 50, width(1) color(red%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** investigating she does all HW
histogram housework_woman if couple_hw==1 & housework_woman < 50
sum housework_woman if couple_hw==1, det
sum housework_woman, det

// alt cutpoints: within she does all
	mi passive: egen hw_hilow_woman_gp1 = cut(housework_woman) if housework_woman!=0 & couple_hw==1, group(2)
	tab hw_hilow_woman_gp1 if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman_gp1)
	tab hw_hilow_woman if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman)
		
** investigating she does most HW
histogram housework_woman if couple_hw==2 & housework_woman < 50
sum housework_woman if couple_hw==2, det

// alt cutpoints: within she does most
	mi passive: egen hw_hilow_woman_gp2 = cut(housework_woman) if housework_woman!=0 & couple_hw==2, group(3)
	tab hw_hilow_woman_gp2 if couple_hw==2
	tabstat housework_woman, by(hw_hilow_woman_gp2)
	tab hw_terc_woman if couple_hw==2
	tabstat housework_woman, by(hw_terc_woman)
	
** investigating she does most OR all HW
histogram housework_woman if inlist(couple_hw,1,2) & housework_woman < 50
sum housework_woman if inlist(couple_hw,1,2), det

// alt cutpoints: within she does most OR all
	mi passive: egen hw_hilow_woman_combo = cut(housework_woman) if housework_woman!=0 & inlist(couple_hw,1,2), group(2)
	tab hw_hilow_woman_combo if inlist(couple_hw,1,2)
	tabstat housework_woman, by(hw_hilow_woman_combo)

twoway (histogram housework_woman if couple_hw==1 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_woman if couple_hw==2 & housework_woman < 50, width(1) color(red%30)), legend(order(1 "She does all" 2 "She does most") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** should I lookat if he does most? bc that is essentially same size (actually larger) than woman all
histogram housework_man if couple_hw==4 & housework_man < 50
sum housework_man if couple_hw==4, det

	mi passive: egen hw_hilow_man_gp4 = cut(housework_man) if housework_man!=0 & couple_hw==4, group(2)
	tab hw_hilow_man_gp4  if couple_hw==4
	tabstat housework_man, by(hw_hilow_man_gp4)
	tab hw_hilow_man  if couple_hw==4
	tabstat housework_man, by(hw_hilow_man)

* adding consideration of how many hours she does - this is based on TOTAL distribution of HW, not within each bucket
mi passive: gen couple_hw_hrs=.
mi passive: replace couple_hw_hrs = 1 if couple_hw==1 & hw_hilow_woman==1
mi passive: replace couple_hw_hrs = 2 if couple_hw==1 & hw_hilow_woman==0
mi passive: replace couple_hw_hrs = 3 if couple_hw==2 & hw_terc_woman==2
mi passive: replace couple_hw_hrs = 4 if couple_hw==2 & hw_terc_woman==1
mi passive: replace couple_hw_hrs = 5 if couple_hw==2 & hw_terc_woman==0
mi passive: replace couple_hw_hrs = 6 if couple_hw==3 & couple_hw_total > =20 & couple_hw_total < 500
mi passive: replace couple_hw_hrs = 7 if couple_hw==3 & couple_hw_total < 20
mi passive: replace couple_hw_hrs = 8 if couple_hw==4 & hw_hilow_man==1
mi passive: replace couple_hw_hrs = 9 if couple_hw==4 & hw_hilow_man==0
mi passive: replace couple_hw_hrs = 7 if housework_woman==0 & housework_man==0 // neither is so small, just put in equal low

label define couple_hw_hrs 1 "Woman All: High" 2 "Woman All: Low" 3 "Woman Most: High" 4 "Woman Most: Med" 5 "Woman Most: Low" 6 "Equal: High" 7 "Equal: Low" 8 "Man Most: High" 9 "Man Most: Low" // rationale for splitting women most into three and the others into two is that it is the largest group (about 52%)
label values couple_hw_hrs couple_hw_hrs

// mi estimate: proportion couple_hw_hrs couple_hw

* Now adding consideration of how many hours - based on distribution of hours WITHIN each bucket of HW
mi passive: gen couple_hw_hrs_alt=.
mi passive: replace couple_hw_hrs_alt = 1 if couple_hw==1 & hw_hilow_woman_gp1==1
mi passive: replace couple_hw_hrs_alt = 2 if couple_hw==1 & hw_hilow_woman_gp1==0
mi passive: replace couple_hw_hrs_alt = 3 if couple_hw==2 & hw_hilow_woman_gp2==2
mi passive: replace couple_hw_hrs_alt = 4 if couple_hw==2 & hw_hilow_woman_gp2==1
mi passive: replace couple_hw_hrs_alt = 5 if couple_hw==2 & hw_hilow_woman_gp2==0
mi passive: replace couple_hw_hrs_alt = 6 if couple_hw==3 & couple_hw_total > =20 & couple_hw_total < 500
mi passive: replace couple_hw_hrs_alt = 7 if couple_hw==3 & couple_hw_total < 20
mi passive: replace couple_hw_hrs_alt = 8 if couple_hw==4 & hw_hilow_man_gp4==1
mi passive: replace couple_hw_hrs_alt = 9 if couple_hw==4 & hw_hilow_man_gp4==0
mi passive: replace couple_hw_hrs_alt = 7 if housework_woman==0 & housework_man==0 // neither is so small, just put in equal low

label values couple_hw_hrs_alt couple_hw_hrs

mi estimate: proportion couple_hw couple_hw_hrs couple_hw_hrs_alt 

* Consolidated housework + hours variable
mi passive: gen couple_hw_hrs_combo=.
mi passive: replace couple_hw_hrs_combo = 1 if inlist(couple_hw,1,2) & hw_hilow_woman_combo==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_combo = 2 if inlist(couple_hw,1,2) & hw_hilow_woman_combo==0 
mi passive: replace couple_hw_hrs_combo = 3 if couple_hw==3 & couple_hw_total > =20 & couple_hw_total < 500 // 20 is the median
mi passive: replace couple_hw_hrs_combo = 4 if couple_hw==3 & couple_hw_total < 20
mi passive: replace couple_hw_hrs_combo = 5 if couple_hw==4
mi passive: replace couple_hw_hrs_combo = 4 if housework_woman==0 & housework_man==0  // neither is so small, just put in equal low

label define couple_hw_hrs_combo 1 "Woman Most: High" 2 "Woman Most: Low" 3 "Equal: High" 4 "Equal: Low" 5 "Man Most: All"
label values couple_hw_hrs_combo couple_hw_hrs_combo

mi estimate: proportion couple_hw couple_hw_hrs_alt couple_hw_hrs_combo

//	capture drop couple_hw_hrs_end
//	mi update
//	mi passive: gen couple_hw_hrs_end = couple_hw_hrs
//	mi passive: replace couple_hw_hrs_end = 99 if rel_type==0

// family channel
* relationship type
mi passive: gen dur_transitioned=.
mi passive: replace dur_transitioned = transition_yr - rel_start_all

browse unique_id partner_id duration min_dur max_dur rel_start_all transition_yr dur_transitioned

label define rel_status 1 "Intact" 3 "Widow" 4 "Divorce" 5 "Separated" 6 "Attrited"
label values rel_status rel_status

mi passive: gen rel_type=.
mi passive: replace rel_type = 1 if rel_type_constant== 1
mi passive: replace rel_type = 1 if rel_type_constant== 3 & duration >= dur_transitioned
mi passive: replace rel_type = 2 if rel_type_constant== 2
mi passive: replace rel_type = 2 if rel_type_constant== 3 & duration < dur_transitioned
mi passive: replace rel_type = 3 if duration > max_dur & rel_status==1 // intact but past end of relationship
mi passive: replace rel_type = 3 if duration > max_dur & rel_status==6 & in_sample!=1 & in_sample_sp!=1 // attrited and both partners not in sample
mi passive: replace rel_type = 4 if duration > max_dur & inlist(rel_status,3,4,5) // observed end
mi passive: replace rel_type = 4 if duration > max_dur & rel_status==6 & (in_sample==1 & in_sample_sp!=1) // marked as attrit, but one still in sample, so presume broken up (largely cohab where it's less clear)
mi passive: replace rel_type = 4 if duration > max_dur & rel_status==6 & (in_sample!=1 & in_sample_sp==1) // marked as attrit, but one still in sample, so presume broken up (largely cohab where it's less clear)
// mi passive: replace rel_type = 0 if duration > max_dur
// mi passive: replace rel_type = 0 if duration < min_dur

// label define rel_type 0 "Not together" 1 "Married" 2 "Cohab"
label define rel_type 1 "Married" 2 "Cohab" 3 "Attrited" 4 "Broke Up"
label values rel_type rel_type

tab rel_type, m
mi estimate: proportion rel_type

tab rel_type rel_status, row
tab rel_type duration if _mi_m!=0, m

browse unique_id partner_id duration rel_start_all rel_end_all min_dur max_dur rel_type dur_transitioned transition_yr rel_status last_yr_observed last_survey_yr_focal* in_sample in_sample_sp weekly_hrs_woman weekly_hrs_man 

* number of children
tab num_children_woman num_children_man if inlist(rel_type,1,2) & duration>=0, m
mi passive: egen couple_num_children = rowmax(num_children_woman num_children_man)
tab couple_num_children, m

// okay, decided we will use women's number of children
mi passive: gen couple_num_children_gp=.
mi passive: replace couple_num_children_gp = 0 if num_children_woman==0
mi passive: replace couple_num_children_gp = 1 if num_children_woman==1
mi passive: replace couple_num_children_gp = 2 if num_children_woman==2
mi passive: replace couple_num_children_gp = 3 if num_children_woman>=3 & num_children_woman < 15

tab num_children_woman couple_num_children_gp

mi estimate: proportion couple_num_children_gp

tab rel_type couple_num_children_gp

* combined
mi passive: gen family_type=.
mi passive: replace family_type=0 if inlist(rel_type,3,4)
mi passive: replace family_type=1 if rel_type==1 & couple_num_children_gp==0
mi passive: replace family_type=2 if rel_type==1 & couple_num_children_gp==1
mi passive: replace family_type=3 if rel_type==1 & couple_num_children_gp==2
mi passive: replace family_type=4 if rel_type==1 & couple_num_children_gp==3
mi passive: replace family_type=5 if rel_type==2 & couple_num_children_gp==0
mi passive: replace family_type=6 if rel_type==2 & couple_num_children_gp==1
mi passive: replace family_type=7 if rel_type==2 & couple_num_children_gp==2
mi passive: replace family_type=8 if rel_type==2 & couple_num_children_gp==3

label define family_type 0 "Not together" 1 "Married, 0 Ch" 2 "Married, 1 Ch" 3 "Married, 2 Ch" 4 "Married, 3+ Ch" ///
						5 "Cohab, 0 Ch" 6 "Cohab, 1 Ch" 7 "Cohab, 2 Ch" 8 "Cohab, 3+ Ch"
label values family_type family_type

mi estimate: proportion family_type

tab family_type rel_type
tab family_type duration, m
tab family_type duration if _mi_m!=0, m // just validating that everyone has a family type at dur 0
tab family_type duration if _mi_m!=0 & max_dur >0 , m

browse unique_id partner_id duration rel_start_all rel_end_all min_dur max_dur family_type rel_type rel_status couple_num_children_gp in_sample in_sample_sp

**# Bookmark #2
// temp save 2
save "$created_data/psid_couples_imputed_long_recoded.dta", replace

// use "$created_data/psid_couples_imputed_long_recoded.dta", clear

// check
inspect woman_hw_share if couple_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect hw_terc_woman if housework_woman == 0 & imputed==1 // I only did for women with hW hours. so these missings also make sense

foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow couple_hw_total couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children couple_num_children_gp rel_type family_type{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

// designate that relationship dissolved and create versions of all variables that stop at this point
foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man couple_work couple_work_ow_detailed couple_work_ow couple_hw couple_hw_hrs couple_hw_hrs_alt couple_hw_hrs_combo couple_num_children_gp family_type{
	capture drop `var'_end
	mi update
	mi passive: gen `var'_end = `var'
	mi passive: replace `var'_end = 98 if rel_type==4 // dissolve
	mi passive: replace `var'_end = 99 if rel_type==3 // attrit
}

foreach var in ft_pt_woman_end overwork_woman_end ft_pt_man_end ft_pt_det_woman_end ft_pt_det_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end couple_num_children_gp_end family_type_end{
	assert `var' !=. if _mi_m!=0
}

label values ft_pt_man_end ft_pt_woman_end ft_pt
label values ft_pt_det_man_end ft_pt_det_woman_end ft_pt_det
label values couple_work_end couple_work
label values couple_work_ow_detailed_end couple_work_ow_detailed
label values couple_work_ow_end couple_work_ow
label values couple_hw_end couple_hw
label values couple_hw_hrs_end couple_hw_hrs
label values couple_hw_hrs_alt_end couple_hw_hrs
label values couple_hw_hrs_combo_end couple_hw_hrs_combo
label values family_type_end family_type

// cross-tabs to explore to figure out potential new variables
tab ft_pt_man_end ft_pt_woman_end, cell
tab ft_pt_det_man_end ft_pt_det_woman_end, cell

// final update and save

mi update

save "$created_data/psid_couples_imputed_long_recoded.dta", replace

// mi estimate: proportion couple_hw_hrs_end couple_hw_end if duration >=0 & duration <=10

histogram weekly_hrs_woman if couple_work_ow_end==8
histogram weekly_hrs_man if couple_work_ow_end==8

tab ft_pt_det_man_end ft_pt_det_woman_end if couple_work_ow_end==8, cell

********************************************************************************
**# Deduplicate
********************************************************************************
use "$created_data/psid_couples_imputed_long_recoded.dta", clear

// need to get rid of one record per couple; currently deduplicated
browse unique_id partner_id couple_id duration FAMILY_INTERVIEW_NUM_ main_fam_id if inlist(duration,0,1) // ah, okay, will not always have fam interview number because some of these are off years and imputed. Use fam interview number at either time 0 / 1 and use that to indicate same couple? will main fam id work? or only when in same HH? because there are many couples that come from same 1968 id...
inspect FAMILY_INTERVIEW_NUM_ if duration==0
inspect FAMILY_INTERVIEW_NUM_ if duration==1

mi passive: gen fam_id =  FAMILY_INTERVIEW_NUM_ if duration==1
mi passive: replace fam_id = FAMILY_INTERVIEW_NUM_ if fam_id==. & duration==0
bysort unique_id partner_id (fam_id): replace fam_id = fam_id[1]
sort unique_id partner_id imputed _mi_m duration
inspect fam_id

browse unique_id partner_id couple_id duration fam_id FAMILY_INTERVIEW_NUM_ main_fam_id
unique unique_id partner_id
unique fam_id
unique unique_id partner_id fam_id

bysort fam_id duration _mi_m : egen per_id = rank(couple_id)
tab per_id, m
bysort fam_id duration _mi_m : egen num_couples = max(per_id)

sort unique_id partner_id imputed _mi_m duration
browse unique_id partner_id couple_id duration fam_id per_id FAMILY_INTERVIEW_NUM_ main_fam_id if imputed==0
browse unique_id partner_id couple_id duration fam_id per_id FAMILY_INTERVIEW_NUM_ main_fam_id if imputed==0 & num_couples > 4
browse unique_id partner_id couple_id duration fam_id per_id FAMILY_INTERVIEW_NUM_ main_fam_id if imputed==0 & inlist(fam_id, 1602,6443,6978)
browse unique_id partner_id couple_id duration fam_id per_id FAMILY_INTERVIEW_NUM_ main_fam_id if imputed==0 & fam_id==3448 // 7255 3448

keep if inlist(per_id,1,3,5,7,9,11,13)
unique unique_id partner_id //4363, was 8714 (this was previous -10 sample) // 5828, was 11644 (with previous -3 sample + 2023 data)
unique unique_id partner_id, by(rel_start_all) // this makes sense as well

mi update

save "$created_data/psid_couples_imputed_long_deduped.dta", replace

********************************************************************************
**# Quick descriptives for full sample while long
********************************************************************************

// descriptives at all durations
desctable i.ft_pt_woman_end i.overwork_woman_end  i.ft_pt_det_woman_end i.ft_pt_man_end i.overwork_man_end i.ft_pt_det_man_end i.couple_work_end i.couple_work_ow_end i.couple_work_ow_detailed_end i.couple_hw_end i.couple_hw_hrs_end i.couple_hw_hrs_alt_end i.couple_hw_hrs_combo_end i.rel_type i.couple_num_children_gp_end i.family_type_end, filename("$results/psid_mi_desc") stats(mimean)
// desctable i.ft_pt_woman i.overwork_woman i.ft_pt_man i.overwork_man i.couple_work i.couple_work_ow i.couple_hw i.couple_hw_hrs i.rel_type i.couple_num_children_gp i.family_type, filename("$results/mi_desc_all") stats(mimean)  // modify - okay can't use modify but want to see if this replaces the previous or adds a new sheet. okay it replaces the previous oops

mi estimate: proportion couple_work_ow_end family_type_end // validate that this matches. it does

// should I just loop through durations while long? should I confirm the numbers are the same either way? - so here, try to loop through durations
** Note 6/16/25: have not done this yet for updated sequences (with more rel start dates included)** (takes a while - possibly send to HPC)
forvalues d=0/10{
	desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_man_end i.overwork_man_end i.couple_work_end i.couple_work_ow_end i.couple_hw_end i.couple_hw_hrs_end i.couple_hw_hrs_alt_end i.rel_type i.couple_num_children_gp_end i.family_type_end if duration==`d', filename("$results/mi_desc_`d'") stats(mimean) decimals(4)
}

// mi xeq: proportion couple_hw_end if duration==5 // troubleshooting bc this is where the code stalled. I think this is because some have "neither HW" and some don't. okay, yes that is the problem

mi estimate: proportion couple_work_ow_end family_type_end if duration==0
mi estimate: proportion couple_work_ow_end family_type_end if duration==5

/* oh, wait, I think I can actually just group by duration?? ah, no you cannot do that with mi. i knew this
desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_man_end i.overwork_man_end i.couple_work_end i.couple_work_ow_end i.couple_hw_end i.couple_hw_hrs_end i.rel_type i.couple_num_children_gp_end i.family_type_end, filename("$results/mi_desc_dur") stats(mimean) group(duration)
*/

// I think duration needs to start at 1 to work in r
gen duration_v0 = duration
replace duration = duration + 1

// use "$created_data/psid_couples_imputed_long_deduped.dta", clear
// keep ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end rel_type couple_num_children_gp_end family_type_end unique_id partner_id rel_start_all rel_end_all duration  min_dur max_dur last_yr_observed ended _mi_miss _mi_id _mi_m SEX in_sample hh_status relationship housework_focal age_focal weekly_hrs_t_focal earnings_t_focal family_income_t partnered_imp educ_focal_imp num_children_imp_hh weekly_hrs_woman weekly_hrs_man housework_woman housework_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man rel_status rel_type_constant transition_yr FIRST_BIRTH_YR sample_type has_psid_gene birth_yr_all raceth_fixed_focal fixed_education SEX_sp in_sample_sp hh_status_sp relationship_sp housework_focal_sp age_focal_sp weekly_hrs_t_focal_sp earnings_t_focal_sp family_income_t_sp partnered_imp_sp num_children_imp_hh_sp  FIRST_BIRTH_YR_sp sample_type_sp has_psid_gene_sp birth_yr_all_sp raceth_fixed_focal_sp fixed_education_sp couple_work_ow_detailed couple_work_ow couple_hw_hrs_combo // think I need to keep the base variables the passive variables I created are based off of, otherwise, they are reset back to missing I think, which causes problems when I reshape.

drop FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ TOTAL_INCOME_T1_FAMILY_ TOTAL_INCOME_T2_FAMILY_ under18 edulevel_match edulevelmax_match children moved_in_lastyr moved_mo_lastyr moved_yr_lastyr partnered weekly_hrs_t1_focal earnings_t1_focal employed_t1_earn_focal childcare_focal adultcare_focal employed_focal educ_focal max_educ_focal college_focal raceth_focal empstat_disabled_focal empstat_retired_focal age65up weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal has_hours_t1 has_earnings_t1 employed_t1_hrs_focal has_hours_t2 has_earnings_t2 new_in_hh hh_births_ had_birth increment_birth num_children_imp_focal hours_type_t1_focal hw_hours_gp main_fam_id SAMPLE last_race_focal first_educ_focal first_survey_yr_focal last_survey_yr_focal employed_focal_rec min_educ max_educ max_disabled_focal min_disabled_focal nmis_age imputed childcare_focal_sp adultcare_focal_sp max_educ_focal_sp new_in_hh_sp had_birth_sp SAMPLE_sp first_survey_yr_focal_sp last_survey_yr_focal_sp imputed_sp couple_hw_total woman_hw_share hw_terc_woman hw_hilow_woman hw_hilow_man hw_hilow_woman_gp1 hw_hilow_woman_gp2 hw_hilow_woman_combo hw_hilow_man_gp4 _Unique fam_id per_id num_couples duration_v0

mi update

********************************************************************************
**# Reshape back to wide to see the data by duration and compare to long estimates
********************************************************************************
mi reshape wide ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end rel_type couple_num_children_gp_end family_type_end in_sample hh_status relationship housework_focal age_focal weekly_hrs_t_focal earnings_t_focal family_income_t partnered_imp educ_focal_imp num_children_imp_hh weekly_hrs_woman weekly_hrs_man housework_woman housework_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man  in_sample_sp hh_status_sp relationship_sp housework_focal_sp age_focal_sp weekly_hrs_t_focal_sp earnings_t_focal_sp family_income_t_sp partnered_imp_sp num_children_imp_hh_sp couple_work_ow_detailed couple_work_ow couple_hw_hrs_combo age_young_child RESPONDENT_WHO_ REGION_ employment_status_focal religion_focal lives_family_focal disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal father_in_hh mother_in_hh num_parent_in_hh num_65up_hh rolling_births current_parent_status retired_est_focal house_status_all disabled_imp_focal age_young_child_sp RESPONDENT_WHO_sp REGION_sp employment_status_focal_sp religion_focal_sp lives_family_focal_sp disabled_focal_sp sr_health_focal_sp num_parent_in_hh_sp num_65up_hh_sp rolling_births_sp current_parent_status_sp retired_est_focal_sp house_status_all_sp employment_status_woman employment_status_man annual_earnings_woman annual_earnings_man age_youngest_woman age_youngest_man is_parent_woman is_parent_man region_woman region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man lives_near_fam_woman lives_near_fam_man couple_work couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children couple_num_children_gp family_type ft_pt_det_woman_end ft_pt_det_man_end ///
, i(unique_id partner_id rel_start_all rel_end_all) j(duration) // SEX SEX_sp rel_status rel_type_constant transition_yr FIRST_BIRTH_YR FIRST_BIRTH_YR_sp sample_type sample_type_sp has_psid_gene has_psid_gene_sp birth_yr_all birth_yr_all_sp raceth_fixed_focal raceth_fixed_focal_sp fixed_education fixed_education_sp


tab _mi_miss, m // see what happens if I reshape but DON'T convert
tab _mi_m, m
browse unique unique_id partner_id _mi_id _mi_miss _mi_m couple_work_end* couple_work_ow_end* couple_hw_hrs_combo_end*
browse unique unique_id partner_id _mi_id _mi_miss _mi_m couple_work_end* couple_work_ow_end* couple_hw_hrs_combo_end* if inrange(_mi_id,1,10)

foreach var in couple_work_ow_end couple_hw_hrs_combo_end family_type_end{
	forvalues d=1/11{
		inspect `var'`d' if inrange(_mi_m,1,10)
		assert `var'`d' !=. if inrange(_mi_m,1,10)
	}
}

unique unique_id partner_id // so now there are 4363 uniques and 11 observations for each (base + 10 imputations). 5828 with updated sample

// create indicator of complete sequences
gen complete_seq = .
replace complete_seq = 0 if max_dur < 9
replace complete_seq = 1 if max_dur >= 9 // so 9 is really 10 (because 0-9 = 1-10)

tab rel_type10 if complete_seq==1, m
browse unique_id partner_id min_dur max_dur complete_seq rel_type* // okay, so max_dur is based on start of 0, but I changed it, so now need it to be +1

// then create indicator of length of complete sequence
gen sequence_length_alt=max_dur + 1
replace sequence_length_alt = 10 if sequence_length_alt > 10

tab sequence_length_alt complete_seq

gen sequence_length = .
forvalues d=1/10{
	replace sequence_length = `d' if sequence_length==. & inlist(rel_type`d',3,4) // first instance of rel type not married / cohab
}

replace sequence_length = sequence_length - 1 

tab sequence_length complete_seq, m
tab sequence_length sequence_length_alt, m

replace sequence_length = sequence_length_alt if sequence_length==. & complete_seq==0
replace sequence_length = 10 if complete_seq==1

// note how ended (attrit or dissolve)
tab rel_status complete_seq, m
browse unique_id partner_id min_dur max_dur rel_status complete_seq rel_type*

gen status_end = .
forvalues d=1/10{
	local e = `d' + 1
	replace status_end = rel_type`e' if sequence_length == `d'
}

label values status_end rel_type
tab status_end complete_seq, m

browse unique_id partner_id min_dur max_dur sequence_length status_end rel_status rel_type* couple_work_end* if complete_seq==0 & inlist(status_end,1,2)
tab rel_type10 if complete_seq==0 & inlist(status_end,1,2)
tab couple_work_end10 if complete_seq==0 & inlist(status_end,1,2), m

replace complete_seq = 1 if complete_seq == 0 & inlist(status_end,1,2)

gen how_end=.
replace how_end = 0 if complete_seq==1 // intact
replace how_end = 1 if complete_seq==0 & status_end==4 // dissolved
replace how_end = 2 if complete_seq==0 & status_end==3 // attrit

label define how_end 0 "Intact" 1 "Dissolved" 2 "Attrited"
label values how_end how_end

tab how_end complete_seq, m

mi update

save "$created_data/psid_couples_imputed_wide.dta", replace 

********************************************************************************
* A few small additional extracts needed
********************************************************************************
// just complete sequences
keep if complete_seq==1

mi update

// tab couple_hw_hrs_combo_end10 if _mi_m!=0, m
// tab couple_work_ow_end10 if _mi_m!=0, m
// tab family_type_end10 if _mi_m!=0, m

save "$created_data/psid_couples_imputed_wide_complete.dta", replace 

// truncated data (so not attrit or dissolve - set to missing instead)
use "$created_data/psid_couples_imputed_wide.dta", clear

browse unique_id partner_id complete_seq sequence_length couple_work_ow_end* couple_hw_hrs_combo_end* family_type_end*
fre couple_work_ow_end5
fre couple_hw_hrs_combo_end5
fre family_type_end5

forvalues d=1/11{
	capture gen couple_work_ow_trunc`d' = couple_work_ow_end`d'
	replace couple_work_ow_trunc`d' = . if inlist(couple_work_ow_end`d',98,99)
	label values couple_work_ow_trunc`d' couple_work_ow
	capture gen couple_hw_hrs_combo_trunc`d' = couple_hw_hrs_combo_end`d'
	replace couple_hw_hrs_combo_trunc`d' = . if inlist(couple_hw_hrs_combo_end`d',98,99)
	label values couple_hw_hrs_combo_trunc`d' couple_hw_hrs_combo
	capture gen family_type_trunc`d' = family_type_end`d'
	replace family_type_trunc`d' = . if inlist(family_type_end`d',98,99)
	label values family_type_trunc`d' family_type
}

fre couple_work_ow_trunc5
fre couple_hw_hrs_combo_trunc5
fre family_type_trunc5

save "$created_data/psid_couples_wide_truncated.dta", replace 

/*
// don't need this; this is when I didn't know how to get clusters on to data

// non imputed data to append to R files later
use "$created_data/psid_couples_imputed_wide.dta", clear

gen couple_id = _mi_id

mi extract 0, clear

gen _mi_m = 0
rename couple_id _mi_id

save "$created_data/psid_couples_base.dta", replace
*/

/*
// fully wide data (not actually sure we need this)
use "$created_data/psid_couples_imputed_wide.dta", clear

mi convert wide, clear

save "$created_data/psid_couples_imputed_fully_wide.dta", replace // this seems to mess up some observations, so I might have done something wrong in the reshape. will revisit this, but I think getting via long format is fine for now. I wonder if this is because of the category things? okay it was because I dropped the imputed variables the passive ones were based off of; I fixed this.

unique unique_id partner_id // so, I think to what Lea said - the imputations are also wide, need to be long
tab _mi_miss, m

browse unique_id partner_id min_dur max_dur rel_type* *rel_*
browse unique_id partner_id couple_work_end1 _*_couple_work_end1 _mi_miss

mi estimate: proportion rel_type0 couple_work_ow_end0 family_type_end0 // okay NOW there are the right number of couples AND they match what I did when long
mi estimate: proportion rel_type5 couple_work_ow_end5 family_type_end5 // same here
mi estimate: proportion rel_type0 rel_type1 rel_type2 rel_type3 rel_type4 rel_type5 rel_type6 rel_type7 rel_type8 rel_type9 rel_type10 // ensure all have the right number of people now aka 4363
mi estimate: proportion couple_work_ow_end0 couple_work_ow_end1 couple_work_ow_end2 couple_work_ow_end3 couple_work_ow_end4 couple_work_ow_end5 couple_work_ow_end6 couple_work_ow_end7 couple_work_ow_end8 couple_work_ow_end9 couple_work_ow_end10 // ensure all have the right number of people now aka 4363 - wanted to do with created / imputed var, not just a constant one (rel type is constant)
*/
