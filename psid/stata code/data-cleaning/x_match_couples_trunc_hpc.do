
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

* I ran the bulk of this in HPC, then did a few variables interactively, so this code
* JUST does the final part where people are recoded to 98/99 outside of the bounds
* of their relationship [the code WITHOUT trunc in name is the full code, for ease, just restricted to nec. part here]

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

