
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: describe_sequences
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file pulls some basic descriptive information about couples' sequences,
* namely to explore egalitarianism across the life course

use "$created_data/psid_couples_wide_truncated.dta", clear

********************************************************************************
* Restrict to sample used in R
********************************************************************************
// minimum sequence length of 3
unique unique_id partner_unique_id if sequence_length>=3 // confirm matches: 5973
keep if sequence_length>=3

// Only using first 5 imputations
mi set m -= (6,7,8,9,10)

********************************************************************************
* Pull descriptives
********************************************************************************
// explore descriptives to characterize egalitarianism at each duration
tab division_of_labor_trunc1 egalitarian_trunc1

forvalues d=1/10{
	tab egalitarian_trunc`d'
	tab division_of_labor_trunc`d'
}

desctable i.division_of_labor_trunc1 i.division_of_labor_trunc2 i.division_of_labor_trunc3 i.division_of_labor_trunc4 i.division_of_labor_trunc5 i.division_of_labor_trunc6 i.division_of_labor_trunc7 i.division_of_labor_trunc8 i.division_of_labor_trunc9 i.division_of_labor_trunc10 i.egalitarian_trunc1 i.egalitarian_trunc2 i.egalitarian_trunc3 i.egalitarian_trunc4 i.egalitarian_trunc5 i.egalitarian_trunc6 i.egalitarian_trunc7 i.egalitarian_trunc8 i.egalitarian_trunc9 i.egalitarian_trunc10 i.dual_work_trunc1 i.dual_work_trunc2 i.dual_work_trunc3 i.dual_work_trunc4 i.dual_work_trunc5 i.dual_work_trunc6 i.dual_work_trunc7 i.dual_work_trunc8 i.dual_work_trunc9 i.dual_work_trunc10 i.dual_ft_trunc1 i.dual_ft_trunc2 i.dual_ft_trunc3 i.dual_ft_trunc4 i.dual_ft_trunc5 i.dual_ft_trunc6 i.dual_ft_trunc7 i.dual_ft_trunc8 i.dual_ft_trunc9 i.dual_ft_trunc10 i.hw_egal_trunc1 i.hw_egal_trunc2 i.hw_egal_trunc3 i.hw_egal_trunc4 i.hw_egal_trunc5 i.hw_egal_trunc6 i.hw_egal_trunc7 i.hw_egal_trunc8 i.hw_egal_trunc9 i.hw_egal_trunc10 i.hw_mod_egal_trunc1 i.hw_mod_egal_trunc2 i.hw_mod_egal_trunc3 i.hw_mod_egal_trunc4 i.hw_mod_egal_trunc5 i.hw_mod_egal_trunc6 i.hw_mod_egal_trunc7 i.hw_mod_egal_trunc8 i.hw_mod_egal_trunc9 i.hw_mod_egal_trunc10, filename("$results/psid_egal_x_duration") stats(mimean)

mi estimate: mean egalitarian_trunc1 
mi estimate: mean egalitarian_trunc5 
mi estimate: proportion egalitarian_trunc1 
mi estimate: proportion egalitarian_trunc5 
mi estimate: proportion division_of_labor_trunc1 
mi estimate: proportion division_of_labor_trunc5 
mi estimate: proportion egal_dol_yn_trunc1 
mi estimate: proportion egal_dol_yn_trunc5 
mi estimate: mean dual_work_trunc5
mi estimate: mean hw_mod_egal_trunc5

// Now, can I count the total # of states spent egal?
// browse unique_id partner_unique_id sequence_length egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

egen true_egal = rowtotal(egal_dol_yn_trunc1 egal_dol_yn_trunc2 egal_dol_yn_trunc3 egal_dol_yn_trunc4 egal_dol_yn_trunc5 egal_dol_yn_trunc6 egal_dol_yn_trunc7 egal_dol_yn_trunc8 egal_dol_yn_trunc9 egal_dol_yn_trunc10 ), missing
egen modified_egal = rowtotal(egalitarian_trunc1 egalitarian_trunc2 egalitarian_trunc3 egalitarian_trunc4 egalitarian_trunc5 egalitarian_trunc6 egalitarian_trunc7 egalitarian_trunc8 egalitarian_trunc9 egalitarian_trunc10), missing
egen dual_work = rowtotal(dual_work_trunc1 dual_work_trunc2 dual_work_trunc3 dual_work_trunc4 dual_work_trunc5 dual_work_trunc6 dual_work_trunc7 dual_work_trunc8 dual_work_trunc9 dual_work_trunc10), missing
egen dual_ft = rowtotal(dual_ft_trunc1 dual_ft_trunc2 dual_ft_trunc3 dual_ft_trunc4 dual_ft_trunc5 dual_ft_trunc6 dual_ft_trunc7 dual_ft_trunc8 dual_ft_trunc9 dual_ft_trunc10), missing
egen hw_mod_egal = rowtotal(hw_mod_egal_trunc1 hw_mod_egal_trunc2 hw_mod_egal_trunc3 hw_mod_egal_trunc4 hw_mod_egal_trunc5 hw_mod_egal_trunc6 hw_mod_egal_trunc7 hw_mod_egal_trunc8 hw_mod_egal_trunc9 hw_mod_egal_trunc10), missing
egen hw_egal = rowtotal(hw_egal_trunc1 hw_egal_trunc2 hw_egal_trunc3 hw_egal_trunc4 hw_egal_trunc5 hw_egal_trunc6 hw_egal_trunc7 hw_egal_trunc8 hw_egal_trunc9 hw_egal_trunc10), missing

tab true_egal
tab modified_egal
tab dual_work
tab dual_ft
tab hw_mod_egal
tab hw_egal

// then turn to percentages
gen true_egal_percent = true_egal / sequence_length
gen modified_egal_percent = modified_egal / sequence_length
gen dual_work_percent = dual_work / sequence_length
gen dual_ft_percent = dual_ft / sequence_length
gen hw_mod_egal_percent = hw_mod_egal / sequence_length
gen hw_egal_percent = hw_egal / sequence_length

browse unique_id partner_unique_id sequence_length true_egal true_egal_percent modified_egal modified_egal_percent egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

tabstat true_egal_percent modified_egal_percent dual_work_percent dual_ft_percent hw_mod_egal_percent hw_egal_percent, stats(mean median)

// Then flag if all states are in "egal" state
gen true_egal_flag = .
replace true_egal_flag = 0 if true_egal!=sequence_length & true_egal!=.
replace true_egal_flag = 1 if true_egal==sequence_length & true_egal!=.

tab true_egal_flag
tab true_egal_percent // 1s above match 100% here

gen modified_egal_flag = .
replace modified_egal_flag = 0 if modified_egal!=sequence_length & modified_egal!=.
replace modified_egal_flag = 1 if modified_egal==sequence_length & modified_egal!=.

tab modified_egal_flag
tab modified_egal_percent

foreach var in dual_work dual_ft hw_mod_egal hw_egal{
	gen `var'_flag = .
	replace `var'_flag = 0 if `var'!=sequence_length & `var'!=.
	replace `var'_flag = 1 if `var'==sequence_length & `var'!=.
}

tabstat dual_work_flag dual_ft_flag hw_mod_egal_flag hw_egal_flag

// STATS TO USE (just pasting in above excel, which probably isn't great....)
tab sequence_length true_egal
tab sequence_length true_egal, row nofreq
tab sequence_length modified_egal
tab sequence_length modified_egal, row nofreq

tabstat true_egal true_egal_flag true_egal_percent modified_egal modified_egal_flag modified_egal_percent, by(sequence_length)
tabstat true_egal_percent modified_egal_percent

// exploring putexcel
putexcel set "$results/psid_stata_egal_lifecourse.xlsx", replace

putexcel A1 = "Egalitarian Category"
putexcel B1 = "Sequence Length"
putexcel C1 = "Average # of Durations in Egal State"
putexcel D1 = "% with all observed durations in egal"
putexcel E1 = "Average % of durations in egal"
putexcel A2 = "True Egalitarian (Equal both)"
putexcel A11 = "Modified Egalitarian (Dual Full-time, equal or he does more HW)"
putexcel A20 = "Dual FT or Dual PT"
putexcel A29 = "Just Dual FT"
putexcel A38 = "Egalitarian OR He Does More Housework"
putexcel A47 = "Just Egalitarian Housework"

// putexcel B2 = ("3")  B3 = ("4")
putexcel B10 = ("Total") B19 = ("Total")  B28 = ("Total") B37 = ("Total") B46 = ("Total") B55 = ("Total")

* This is probably NOT the most efficient, but...it works..
// True Egalitarian
forvalues l=3/10{
	local row = `l' - 1
	putexcel B`row' = (`l')
	mean true_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean true_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean true_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean true_egal
matrix m`l'= e(b)
putexcel C10 = matrix(m`l'), nformat(#.#)

mean true_egal_flag
matrix m`l'= e(b)
putexcel D10 = matrix(m`l'), nformat(#.#%)

mean true_egal_percent
matrix m`l'= e(b)
putexcel E10 = matrix(m`l'), nformat(#.#%)
	
// Modified Egalitarian
forvalues l=3/10{
	local row = `l' + 8
	putexcel B`row' = (`l')
	mean modified_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean modified_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean modified_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean modified_egal
matrix m`l'= e(b)
putexcel C19 = matrix(m`l'), nformat(#.#)

mean modified_egal_flag
matrix m`l'= e(b)
putexcel D19 = matrix(m`l'), nformat(#.#%)

mean modified_egal_percent
matrix m`l'= e(b)
putexcel E19 = matrix(m`l'), nformat(#.#%)

// Dual FT + PT
forvalues l=3/10{
	local row = `l' + 17
	putexcel B`row' = (`l')
	mean dual_work if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean dual_work_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean dual_work_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean dual_work
matrix m`l'= e(b)
putexcel C28 = matrix(m`l'), nformat(#.#)

mean dual_work_flag
matrix m`l'= e(b)
putexcel D28 = matrix(m`l'), nformat(#.#%)

mean dual_work_percent
matrix m`l'= e(b)
putexcel E28 = matrix(m`l'), nformat(#.#%)

// Dual FT Only
forvalues l=3/10{
	local row = `l' + 26
	putexcel B`row' = (`l')
	mean dual_ft if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean dual_ft_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean dual_ft_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean dual_ft
matrix m`l'= e(b)
putexcel C37 = matrix(m`l'), nformat(#.#)

mean dual_ft_flag
matrix m`l'= e(b)
putexcel D37 = matrix(m`l'), nformat(#.#%)

mean dual_ft_percent
matrix m`l'= e(b)
putexcel E37 = matrix(m`l'), nformat(#.#%)

// Egal or He Does More Housework
forvalues l=3/10{
	local row = `l' + 35
	putexcel B`row' = (`l')
	mean hw_mod_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean hw_mod_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean hw_mod_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean hw_mod_egal
matrix m`l'= e(b)
putexcel C46 = matrix(m`l'), nformat(#.#)

mean hw_mod_egal_flag
matrix m`l'= e(b)
putexcel D46 = matrix(m`l'), nformat(#.#%)

mean hw_mod_egal_percent
matrix m`l'= e(b)
putexcel E46 = matrix(m`l'), nformat(#.#%)

// Egal HW Only
forvalues l=3/10{
	local row = `l' + 44
	putexcel B`row' = (`l')
	mean hw_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean hw_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean hw_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean hw_egal
matrix m`l'= e(b)
putexcel C55 = matrix(m`l'), nformat(#.#)

mean hw_egal_flag
matrix m`l'= e(b)
putexcel D55 = matrix(m`l'), nformat(#.#%)

mean hw_egal_percent
matrix m`l'= e(b)
putexcel E55 = matrix(m`l'), nformat(#.#%)

tabstat true_egal modified_egal dual_work dual_ft hw_mod_egal hw_egal
tabstat true_egal_percent modified_egal_percent dual_work_percent dual_ft_percent hw_mod_egal_percent hw_egal_percent
tabstat true_egal_flag modified_egal_flag dual_work_flag dual_ft_flag hw_mod_egal_flag hw_egal_flag

********************************************************************************
**# Ideal is actually do R steps FIRST - get this output, then use for above and do all in one go
********************************************************************************
* One maybe challenge because R doesn't use MI framework, but Stata does. so we remove MI = 0 from R.
* Whereas in Stata, I retain. should I .... handle this differently?
* FOr these purposes, should I remove MI = 0? Right now it's JUST 1-5. if I ever try to do anything with MI, i think it will be unhappy. let's leave as separate for NOW, though this isn't a great model....

use "$created_data/psid_wide_truncated_Rdurs.dta", clear

tab _mi_m

egen true_egal = rowtotal(egal_dol_yn_trunc1 egal_dol_yn_trunc2 egal_dol_yn_trunc3 egal_dol_yn_trunc4 egal_dol_yn_trunc5 egal_dol_yn_trunc6 egal_dol_yn_trunc7 egal_dol_yn_trunc8 egal_dol_yn_trunc9 egal_dol_yn_trunc10 ), missing
egen modified_egal = rowtotal(egalitarian_trunc1 egalitarian_trunc2 egalitarian_trunc3 egalitarian_trunc4 egalitarian_trunc5 egalitarian_trunc6 egalitarian_trunc7 egalitarian_trunc8 egalitarian_trunc9 egalitarian_trunc10), missing
egen dual_work = rowtotal(dual_work_trunc1 dual_work_trunc2 dual_work_trunc3 dual_work_trunc4 dual_work_trunc5 dual_work_trunc6 dual_work_trunc7 dual_work_trunc8 dual_work_trunc9 dual_work_trunc10), missing
egen dual_ft = rowtotal(dual_ft_trunc1 dual_ft_trunc2 dual_ft_trunc3 dual_ft_trunc4 dual_ft_trunc5 dual_ft_trunc6 dual_ft_trunc7 dual_ft_trunc8 dual_ft_trunc9 dual_ft_trunc10), missing
egen hw_mod_egal = rowtotal(hw_mod_egal_trunc1 hw_mod_egal_trunc2 hw_mod_egal_trunc3 hw_mod_egal_trunc4 hw_mod_egal_trunc5 hw_mod_egal_trunc6 hw_mod_egal_trunc7 hw_mod_egal_trunc8 hw_mod_egal_trunc9 hw_mod_egal_trunc10), missing
egen hw_egal = rowtotal(hw_egal_trunc1 hw_egal_trunc2 hw_egal_trunc3 hw_egal_trunc4 hw_egal_trunc5 hw_egal_trunc6 hw_egal_trunc7 hw_egal_trunc8 hw_egal_trunc9 hw_egal_trunc10), missing

browse max_dur_mod_egal modified_egal egalitarian_trunc* mod_egal_spell_*
browse max_dur_dol_egal true_egal division_of_labor_trunc* dol_egal_spell_*
browse max_dur_dual_work dual_work dual_work_trunc* dual_work_spell_*

tab max_dur_mod_egal // this is MAX CONSECUTIVE
tab modified_egal // this is TOTAL, not consecutive - BUT 0s should match
tab sequence_length max_dur_mod_egal // good sense check

tab max_dur_dol_egal // this is MAX CONSECUTIVE
tab true_egal  // this is TOTAL, not consecutive - BUT 0s should match. so makes sense THIS is higher
tab sequence_length max_dur_dol_egal

tab max_dur_dual_work
tab dual_work
tab sequence_length max_dur_dual_work

tab max_dur_dual_ft
tab dual_ft

tab max_dur_hw_mod_egal
tab hw_mod_egal

tab max_dur_hw_egal
tab hw_egal

tabstat max_dur_mod_egal max_dur_dol_egal modified_egal true_egal
tabstat max_dur_mod_egal max_dur_dol_egal, by(sequence_length) // this is ALL
tabstat max_dur_mod_egal max_dur_dol_egal max_dur_dual_work max_dur_dual_ft max_dur_hw_mod_egal max_dur_hw_egal, by(sequence_length) // All

tabstat max_dur_mod_egal if max_dur_mod_egal!=0, by(sequence_length) // if at least ONE SPELL egal
tabstat max_dur_dol_egal if max_dur_dol_egal!=0, by(sequence_length) 
tabstat max_dur_dual_work if max_dur_dual_work!=0, by(sequence_length) 
tabstat max_dur_dual_ft if max_dur_dual_ft!=0, by(sequence_length) 
tabstat max_dur_hw_mod_egal if max_dur_hw_mod_egal!=0, by(sequence_length) 
tabstat max_dur_hw_egal if max_dur_hw_egal!=0, by(sequence_length) 

// also want to figure out how to do this JUST for people who experienced state. think that is where some of above helpful because can use code that exists (like at least one in the rowtotal I do). actually, I can just do if MAX DUR > 0? but then again will be helpful to compare that that matches what I did above...

********************************************************************************
**# Is there something with a SURVIVAL analysis?
********************************************************************************

use "$created_data/psid_couples_wide_truncated.dta", clear 

mi extract 1

// minimum sequence length of 3
unique unique_id partner_unique_id if sequence_length>=3 // confirm matches: 5973
keep if sequence_length>=3

reshape long ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end rel_type couple_num_children_gp_end family_type_end in_sample hh_status relationship housework_focal age_focal weekly_hrs_t_focal earnings_t_focal family_income_t partnered_imp educ_focal_imp num_children_imp_hh weekly_hrs_woman weekly_hrs_man housework_woman housework_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man  in_sample_sp hh_status_sp relationship_sp housework_focal_sp age_focal_sp weekly_hrs_t_focal_sp earnings_t_focal_sp family_income_t_sp partnered_imp_sp marst_imp marst_imp_sp marst_woman marst_man num_children_imp_hh_sp couple_work_ow_detailed couple_work_ow couple_hw_hrs_combo age_young_child RESPONDENT_WHO_ REGION_ employment_status_focal religion_focal lives_family_focal disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal father_in_hh mother_in_hh num_parent_in_hh num_65up_hh rolling_births current_parent_status retired_est_focal house_status_all age_young_child_sp RESPONDENT_WHO_sp REGION_sp employment_status_focal_sp religion_focal_sp lives_family_focal_sp disabled_focal_sp sr_health_focal_sp num_65up_hh_sp rolling_births_sp current_parent_status_sp retired_est_focal_sp house_status_all_sp home_owner home_owner_sp employment_status_woman employment_status_man annual_earnings_woman annual_earnings_man age_youngest_woman age_youngest_man is_parent_woman is_parent_man region_woman region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man lives_near_fam_woman lives_near_fam_man couple_work couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children couple_num_children_gp family_type ft_pt_det_woman_end ft_pt_det_man_end is_respondent_focal is_respondent_focal_sp any_parent_in_hh any_parent_in_hh_sp RELATION_ relationship_type_ is_first_yr_cohabitor has_first_yr_cohabitor current_rel_type marital_status_focal in_rel_flag survey_yr dual_work_end division_of_labor_end egalitarian_end egal_dol_yn_end dual_ft_end hw_egal_end hw_mod_egal_end couple_hw_hrs_4cat couple_hw_hrs_4cat_end family_type_5cat family_type_5cat_end  couple_work_3cat_end couple_hw_3cat_end detailed_dol_end couple_work_ow_trunc couple_work_trunc couple_hw_hrs_combo_trunc couple_hw_hrs_4cat_trunc family_type_trunc family_type_5cat_trunc division_of_labor_trunc egalitarian_trunc egal_dol_yn_trunc dual_work_trunc dual_ft_trunc hw_egal_trunc hw_mod_egal_trunc couple_work_3cat_trunc couple_hw_3cat_trunc detailed_dol_trunc ///
, i(unique_id partner_unique_id eligible_rel_start_year eligible_rel_end_year) j(duration) 

unique unique_id partner_unique_id
tab sequence_length
tab duration

drop if duration==11

browse unique_id partner_unique_id duration sequence_length detailed_dol_trunc couple_hw_3cat_trunc couple_work_3cat_trunc egalitarian_trunc egal_dol_yn_trunc
	// egal_dol_yn_trunc = true
	// egalitarian_trunc = modified aka more liberal def

tab egalitarian_trunc
gen egal_failure = .
replace egal_failure = 1 if egalitarian_trunc==0
replace egal_failure = 0 if egalitarian_trunc==1

tab egalitarian_trunc, m
tab egal_failure, m

// I think I might need to DROP the durations post sequence length too actually
drop if duration > sequence_length

// so time would be duration
// failure needs to be egal reverse coded?
stset duration, id(couple_id) failure(egal_failure)
sts list
sts graph

sts list, failure
sts graph, failure

tab egal_failure if duration==1 // yes so NOW time 1 matches, then the rest are contingent on making it here
tab duration egal_failure, row

sts list, saving("$results/psid_survival", replace) 

sts graph, xlabel(0(1)10) ylabel(0(.1)1)
graph export "$results/psid_survival.png", replace
sts graph, saving("$results/psid_survival.gph", replace) xlabel(0(1)10) ylabel(0(.1)1)

// Can i stack and make one figure?
use "$root/results/PSID/psid_survival", clear

gen country = .
replace country = 1

label define country 1 "US" 2 "UK" 3 "DE"
label values country country 

append using "$root/results/UKHLS/ukhls_survival"
replace country = 2 if country == .

append using "$root/results/GSOEP/gsoep_survival"
replace country = 3 if country == .

browse

/*
insobs 1, after(30)
replace time = 0 if time==.
replace country = 1 if country==.
replace survivor = 1 if survivor==.

insobs 1, after(31)
replace time = 0 if time==.
replace country = 2 if country==.
replace survivor = 1 if survivor==.

insobs 1, after(32)
replace time = 0 if time==.
replace country = 3 if country==.
replace survivor = 1 if survivor==.
*/

twoway (line survivor time if country==1) (line survivor time if country==2) (line survivor time if country==3)

graph combine "$root/results/GSOEP/gsoep_survival.gph" "$root/results/PSID/psid_survival.gph" "$root/results/UKHLS/ukhls_survival.gph", rows(1)