
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: match_couples_create_vars
********************************************************************************
********************************************************************************

set maxvar 10000

cd "/home/kmcerlea/stage/Life Course"

********************************************************************************
* Description
********************************************************************************
* This files takes the imputed data from step 3 at an individual level and
* matches the corresponding imputed partner data.
* It also creates couple-level variables.
* This is the HPC version
/*
use "created data/stata/psid_couples_imputed_long.dta", clear

// unique unique_id partner_id

label values duration_rec .
// browse unique_id partner_id duration_rec SEX SEX_sp weekly_hrs_t_focal weekly_hrs_t_focal_sp _mi_m

drop if SEX==2 & SEX_sp==2
drop if SEX==1 & SEX_sp==1

// only imputed durations 1-10 (aka 2-12). so let's restrict to that here (used to occur later in file)
gen duration = duration_rec - 2
// browse duration duration_rec

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

*Disability status // duh I didn't actually impute this I am so dumb
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

**# Bookmark #1
// temp save 1
save "created data/stata/psid_couples_imputed_long_recoded.dta", replace

// Stata assert command to check new variables created from imputed  
// disabled_woman disabled_man -- will not be true bc I did not impute
foreach var in weekly_hrs_woman weekly_hrs_man employment_status_woman employment_status_man annual_earnings_woman annual_earnings_man housework_woman housework_man partnered_woman partnered_man rel_no_woman rel_no_man num_children_woman num_children_man age_youngest_woman age_youngest_man yr_first_birth_woman yr_first_birth_man is_parent_woman is_parent_man ever_parent_woman ever_parent_man region_woman region_man housing_woman housing_man religion_woman religion_man sr_health_woman sr_health_man retired_woman retired_man father_educ_woman father_educ_man mother_educ_woman mother_educ_man family_structure_woman family_structure_man lives_near_fam_woman lives_near_fam_man dob_woman dob_man{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 


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

// histogram weekly_hrs_woman if ft_pt_woman==1 
sum weekly_hrs_woman if ft_pt_woman==1, det
// histogram weekly_hrs_man if ft_pt_man==1 
sum weekly_hrs_man if ft_pt_man==1, det

// twoway (histogram weekly_hrs_woman if ft_pt_woman==1, width(1) color(pink%30)) (histogram weekly_hrs_man if ft_pt_man==1, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours among PT Workers")

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
// histogram couple_hw_total if couple_hw==3 & couple_hw_total < 100
sum couple_hw_total if couple_hw==3, det
// histogram housework_woman if couple_hw==3 & housework_woman < 50
sum housework_woman if couple_hw==3, det
// histogram housework_man if couple_hw==3 & housework_man < 50
sum housework_man if couple_hw==3, det

// twoway (histogram housework_woman if couple_hw==3 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_man if couple_hw==3 & housework_man < 50, width(1) color(red%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** investigating she does all HW
// histogram housework_woman if couple_hw==1 & housework_woman < 50
sum housework_woman if couple_hw==1, det
sum housework_woman, det

// alt cutpoints: within she does all
	mi passive: egen hw_hilow_woman_gp1 = cut(housework_woman) if housework_woman!=0 & couple_hw==1, group(2)
	tab hw_hilow_woman_gp1 if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman_gp1)
	tab hw_hilow_woman if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman)
		
** investigating she does most HW
// histogram housework_woman if couple_hw==2 & housework_woman < 50
sum housework_woman if couple_hw==2, det

// alt cutpoints: within she does most
	mi passive: egen hw_hilow_woman_gp2 = cut(housework_woman) if housework_woman!=0 & couple_hw==2, group(3)
	tab hw_hilow_woman_gp2 if couple_hw==2
	tabstat housework_woman, by(hw_hilow_woman_gp2)
	tab hw_terc_woman if couple_hw==2
	tabstat housework_woman, by(hw_terc_woman)
	
** investigating she does most OR all HW
// histogram housework_woman if inlist(couple_hw,1,2) & housework_woman < 50
sum housework_woman if inlist(couple_hw,1,2), det

// alt cutpoints: within she does most OR all
	mi passive: egen hw_hilow_woman_combo = cut(housework_woman) if housework_woman!=0 & inlist(couple_hw,1,2), group(2)
	tab hw_hilow_woman_combo if inlist(couple_hw,1,2)
	tabstat housework_woman, by(hw_hilow_woman_combo)

// twoway (histogram housework_woman if couple_hw==1 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_woman if couple_hw==2 & housework_woman < 50, width(1) color(red%30)), legend(order(1 "She does all" 2 "She does most") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** should I lookat if he does most? bc that is essentially same size (actually larger) than woman all
// histogram housework_man if couple_hw==4 & housework_man < 50
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


**# Bookmark #2
// temp save 2 - I am so dumb it failed here because one of these labels already existed...
save "created data/stata/psid_couples_imputed_long_recoded.dta", replace

// family channel
* relationship type
mi passive: gen dur_transitioned=.
mi passive: replace dur_transitioned = transition_yr - rel_start_all

// browse unique_id partner_id duration min_dur max_dur rel_start_all transition_yr dur_transitioned

capture label define rel_status 1 "Intact" 3 "Widow" 4 "Divorce" 5 "Separated" 6 "Attrited"
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
capture label define rel_type 1 "Married" 2 "Cohab" 3 "Attrited" 4 "Broke Up"
label values rel_type rel_type

tab rel_type, m
mi estimate: proportion rel_type

tab rel_type rel_status, row

// browse unique_id partner_id duration rel_start_all rel_end_all min_dur max_dur rel_type dur_transitioned transition_yr rel_status last_yr_observed last_survey_yr_focal* in_sample in_sample_sp weekly_hrs_woman weekly_hrs_man 

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

capture label define family_type 0 "Not together" 1 "Married, 0 Ch" 2 "Married, 1 Ch" 3 "Married, 2 Ch" 4 "Married, 3+ Ch" ///
						5 "Cohab, 0 Ch" 6 "Cohab, 1 Ch" 7 "Cohab, 2 Ch" 8 "Cohab, 3+ Ch"
label values family_type family_type

mi estimate: proportion family_type

tab family_type rel_type

// browse unique_id partner_id duration rel_start_all rel_end_all min_dur max_dur family_type rel_type rel_status couple_num_children_gp in_sample in_sample_sp

**# Bookmark #3
// temp save 3
save "created data/stata/psid_couples_imputed_long_recoded.dta", replace
*/

use "created data/stata/psid_couples_imputed_long_recoded.dta", clear

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
// tab ft_pt_man_end ft_pt_woman_end, cell
// tab ft_pt_det_man_end ft_pt_det_woman_end, cell

// final update and save

mi update

save "created data/stata/psid_couples_imputed_long_recoded.dta", replace

