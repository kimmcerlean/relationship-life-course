
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: describe_clusters_complete
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes the clusters created in R and imports to stata
* Creates some needed variables and then gets preliminary descriptives
* This is restricted to just complete sequences (aka duration of 10)

********************************************************************************
* First, clean up the cluster file, so just has variables I want
********************************************************************************
use "$temp/PSID_clusters_complete_sequences.dta", clear

egen couple_id = group(unique_id partner_id)
// rename _mi_id couple_id
unique couple_id _mi_id
unique unique_id partner_id

gen mim = _mi_m

keep couple_id unique_id partner_id mim mc7_factor
unique couple_id, by(mc7_factor)
tab mc7_factor

gen cluster=.
replace cluster=1 if mc7_factor==4
replace cluster=2 if mc7_factor==1
replace cluster=3 if mc7_factor==7
replace cluster=4 if mc7_factor==5
replace cluster=5 if mc7_factor==2
replace cluster=6 if mc7_factor==3
replace cluster=7 if mc7_factor==6

unique couple_id, by(cluster)
tab cluster, m

save "$created_data/PSID_clusters_complete.dta", replace

********************************************************************************
* Now, merge clusters onto the original Stata imputed data
********************************************************************************
use "$created_data/psid_couples_imputed_wide_complete.dta", clear

gen mim = _mi_m
egen couple_id = group (unique_id partner_id)

merge 1:1 couple_id mim using "$created_data/PSID_clusters_complete.dta"

tab _mi_m _merge, m // confirm that this is mi 0 - it is
drop mim
drop _merge

mi update

********************************************************************************
* Figure out the data structure
********************************************************************************
browse unique_id partner_id _mi_m cluster
// there are no sequence objects here. Are there supposed to be? Or we just care about cluster membership?

// for now, very crude labels
capture label define cluster 1 "Many children" 2 "Overwork with Late Transition to Parenthood" 3 "Continuous Employment with Less Children Later" ///
 4 "Transition to Neotraditional" 5 "Traditional" 6 "Continuous Employment with More Children" 7 "Childfree Continuous Employment"
label values cluster cluster

tab cluster, m

********************************************************************************
* Any variables still need to be created
********************************************************************************
// couple education: this was fixed, not imputed, so don't need to use mi passive
gen education_man=fixed_education if SEX==1
replace education_man=fixed_education_sp if SEX==2

gen education_woman=fixed_education if SEX==2
replace education_woman=fixed_education_sp if SEX==1

capture label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education_man education_woman fixed_education* educ

gen couple_educ_type=.
replace couple_educ_type = 1 if inrange(education_man,1,3) & inrange(education_woman,1,3)
replace couple_educ_type = 2 if inrange(education_man,1,3) & education_woman==4
replace couple_educ_type = 3 if education_man==4 & inrange(education_woman,1,3)
replace couple_educ_type = 4 if education_man==4 & education_woman==4

capture label define couple_educ_type 1 "Neither College" 2 "Her College" 3 "Him College" 4 "Both College"
label values couple_educ_type couple_educ_type
tab couple_educ_type, m // does this feel very uneducated?
tab _mi_m couple_educ_type, row

// gendered race variables
tab _mi_m raceth_fixed_focal, m

gen raceth_man=raceth_fixed_focal if SEX==1
replace raceth_man=raceth_fixed_focal_sp if SEX==2

gen raceth_woman=raceth_fixed_focal if SEX==2
replace raceth_woman=raceth_fixed_focal_sp if SEX==1

capture label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
labe values raceth_man raceth_woman raceth_fixed_focal raceth_fixed_focal_sp raceth

gen raceth_man_gp = raceth_man
replace raceth_man_gp = 4 if raceth_man_gp ==5

gen raceth_woman_gp = raceth_woman
replace raceth_woman_gp = 4 if raceth_woman_gp ==5

label define raceth_gp 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Other"
label values raceth_woman_gp raceth_man_gp raceth_gp

gen same_race=.
replace same_race=0 if raceth_man!=raceth_woman
replace same_race=1 if raceth_man==raceth_woman

// make some sort of birth cohort? can also use age to describe within cluster (but categorical will be easier for between cluster)
// well, age is hard because time-varying. so could just use at time 0
gen birth_yr_man=birth_yr_all if SEX==1
replace birth_yr_man=birth_yr_all_sp if SEX==2

gen birth_yr_woman=birth_yr_all if SEX==2
replace birth_yr_woman=birth_yr_all_sp if SEX==1

gen bcohort_man = .
replace bcohort_man = 1 if birth_yr_man < 1960
replace bcohort_man = 2 if birth_yr_man >= 1960 & birth_yr_man < 1970
replace bcohort_man = 3 if birth_yr_man >= 1970 & birth_yr_man < 1980
replace bcohort_man = 4 if birth_yr_man >= 1980 & birth_yr_man < 2000

gen bcohort_woman = .
replace bcohort_woman = 1 if birth_yr_woman < 1960
replace bcohort_woman = 2 if birth_yr_woman >= 1960 & birth_yr_woman < 1970
replace bcohort_woman = 3 if birth_yr_woman >= 1970 & birth_yr_woman < 1980
replace bcohort_woman = 4 if birth_yr_woman >= 1980 & birth_yr_woman < 2000

capture label define bcohort 1 "Pre-1960s" 2 "1960s" 3 "1970s" 4 "1980s+"
label values bcohort_man bcohort_woman bcohort

gen age_man1 = age_focal1  if SEX==1
replace age_man1 = age_focal_sp1 if SEX==2

gen age_woman1=age_focal1 if SEX==2
replace age_woman1=age_focal_sp1 if SEX==1

gen age_gp_woman1 = .
replace age_gp_woman1 = 1 if age_woman1 < = 24
replace age_gp_woman1 = 2 if age_woman1 > 24 & age_woman1 < = 34
replace age_gp_woman1 = 3 if age_woman1 > 34 & age_woman1 < = 1000

label define age_gp 1 "18-24" 2 "25-34" 3 "35+"
label values age_gp_woman1 age_gp

// relationship start as well
gen rel_cohort=.
replace rel_cohort = 1 if rel_start_all >=1990 & rel_start_all<2000
replace rel_cohort = 2 if rel_start_all >=2000 & rel_start_all<2012
tab rel_cohort, m

// income or earnings (perhaps earnings is a bit endogenous to employment, but could work)
// these are imputed so need mi passive
mi passive: egen couple_earnings_t1 = rowtotal(earnings_t_focal1 earnings_t_focal_sp1)

browse unique_id partner_id family_income_t1 family_income_t_sp1 couple_earnings_t1 earnings_t_focal1 earnings_t_focal_sp1
sum couple_earnings_t1, detail

mi passive: egen couple_earnings_quart1 = cut(couple_earnings_t1), group(4) // if couple_earnings_t1!=0 // have to not do this because some imputations / clusters have no observations
tab couple_earnings_quart1, m
// mi passive: replace couple_earnings_quart1 = couple_earnings_quart1 + 1
// mi passive: replace couple_earnings_quart1 = 0 if couple_earnings_t1==0

tabstat couple_earnings_t1, by(couple_earnings_quart1)
tab _mi_m couple_earnings_quart1

mi update
// mi register regular education_man education_woman couple_educ_type raceth_man raceth_woman birth_yr_man birth_yr_woman bcohort_man bcohort_woman age_man1 age_woman1 // do I need to register?

save "$created_data/PSID_complete_clusters_analysis.dta", replace

********************************************************************************
**# Within cluster descriptives
********************************************************************************

// tab education_man, gen(educ_man)
// mi estimate: proportion education_man
// mi estimate: mean educ_man1 educ_man2 educ_man3 educ_man4

mi estimate: proportion couple_educ_type raceth_woman // validate that this matches
mi estimate: mean couple_earnings_t1

mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if cluster==1
mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if cluster==4
mi estimate, esampvaryok: mean couple_earnings_t1  if cluster==1
tabstat couple_earnings_t1, by(cluster)

*Descriptive table by cluster
putexcel set "$tables/descriptives_by_cluster_complete.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Many Children"
putexcel D1 = "2: OW and Late Parenthood"
putexcel E1 = "3: Continuous Work and Less Children"
putexcel F1 = "4: Trans to Neotraditional"
putexcel G1 = "5: Traditional"
putexcel H1 = "6: Continuous Work and More Children"
putexcel I1 = "7: Continuous Work and Childfree"

putexcel A2 = "Educ Man: LTHS"
putexcel A3 = "Educ Man: HS"
putexcel A4 = "Educ Man: Some College"
putexcel A5 = "Educ Man: College+"
putexcel A6 = "Educ Woman: LTHS"
putexcel A7 = "Educ Woman: HS"
putexcel A8 = "Educ Woman: Some College"
putexcel A9 = "Educ Woman: College+"
putexcel A10 = "Couple Educ: Neither College"
putexcel A11 = "Couple Educ: Her College"
putexcel A12 = "Couple Educ: Him College"
putexcel A13 = "Couple Educ: Both College"
putexcel A14 = "Race Man: NH White"
putexcel A15 = "Race Man: Black"
putexcel A16 = "Race Man: Hispanic"
putexcel A17 = "Race Man: NH Other"
putexcel A18 = "Race Woman: NH White"
putexcel A19 = "Race Woman: Black"
putexcel A20 = "Race Woman: Hispanic"
putexcel A21 = "Race Woman: NH Other"
putexcel A22 = "Age Man"
putexcel A23 = "Age Woman"
putexcel A24 = "Birth Cohort Man: Pre 1960s"
putexcel A25 = "Birth Cohort Man: 1960s"
putexcel A26 = "Birth Cohort Man: 1970s"
putexcel A27 = "Birth Cohort Man: 1980s+"
putexcel A28 = "Birth Cohort Woman: Pre 1960s"
putexcel A29 = "Birth Cohort Woman: 1960s"
putexcel A30 = "Birth Cohort Woman: 1970s"
putexcel A31 = "Birth Cohort Woman: 1980s+"
putexcel A32 = "Couple Earnings: Q1"
putexcel A33 = "Couple Earnings: Q2"
putexcel A34 = "Couple Earnings: Q3"
putexcel A35 = "Couple Earnings: Q4"
putexcel A36 = "Couple Earnings"
putexcel A37 = "Rel Cohort: 1990-1999"
putexcel A38 = "Rel Cohort: 2000+"

// full sample
forvalues e=1/4{
   capture gen educ_man`e' = education_man==`e'
   mi estimate: mean educ_man`e'
   matrix m`e' = e(b_mi)
   local m`e' = m`e'[1,1]
   local row = 1+`e'
   putexcel B`row' = `m`e'', nformat(##.#%)
}

forvalues e=1/4{
   capture gen educ_woman`e' = education_woman==`e'
   mi estimate: mean educ_woman`e'
   matrix w`e' = e(b_mi)
   local w`e' = w`e'[1,1]
   local row = 5+`e'
   putexcel B`row' = `w`e'', nformat(##.#%)
}

forvalues e=1/4{
   capture gen couple_educ`e' = couple_educ_type==`e'
   mi estimate: mean couple_educ`e'
   matrix c`e' = e(b_mi)
   local c`e' = c`e'[1,1]
   local row = 9+`e'
   putexcel B`row' = `c`e'', nformat(##.#%)
}

forvalues r=1/4{
   capture gen race_man`r' = raceth_man_gp==`r'
   mi estimate: mean race_man`r'
   matrix r`r' = e(b_mi)
   local r`r' = r`r'[1,1]
   local row = 13+`r'
   putexcel B`row' = `r`r'', nformat(##.#%)
}

forvalues r=1/4{
   capture gen race_woman`r' = raceth_woman_gp==`r'
   mi estimate: mean race_woman`r'
   matrix rw`r' = e(b_mi)
   local rw`r' = rw`r'[1,1]
   local row = 17+`r'
   putexcel B`row' = `rw`r'', nformat(##.#%)
}

mi estimate: mean age_man1
matrix a = e(b_mi)
local a = a[1,1]
putexcel B22 = `a', nformat(##.#)

mi estimate: mean age_woman1
matrix aw = e(b_mi)
local aw = aw[1,1]
putexcel B23 = `aw', nformat(##.#)
   
forvalues b=1/4{
   capture gen bc_man`b' = bcohort_man==`b'
   mi estimate: mean bc_man`b'
   matrix b`b' = e(b_mi)
   local b`b' = b`b'[1,1]
   local row = 23+`b'
   putexcel B`row' = `b`b'', nformat(##.#%)
}

forvalues b=1/4{
   capture gen bc_woman`b' = bcohort_woman==`b'
   mi estimate: mean bc_woman`b'
   matrix bw`b' = e(b_mi)
   local bw`b' = bw`b'[1,1]
   local row = 27+`b'
   putexcel B`row' = `bw`b'', nformat(##.#%)
}

forvalues ce=0/3{
   capture mi passive: gen earn_quart`ce' = couple_earnings_quart1==`ce'
   mi estimate: mean earn_quart`ce'
   matrix ce`ce' = e(b_mi)
   local ce`ce' = ce`ce'[1,1]
   local row = 32+`ce'
   putexcel B`row' = `ce`ce'', nformat(##.#%)
}

mi estimate: mean couple_earnings_t1
matrix ce = e(b_mi)
local ce = ce[1,1]
putexcel B36 = `ce', nformat(#####)

forvalues rc=1/2{
   capture gen relcoh`rc' = rel_cohort==`rc'
   mi estimate: mean relcoh`rc'
   matrix rc`rc' = e(b_mi)
   local rc`rc' = rc`rc'[1,1]
   local row = 36+`rc'
   putexcel B`row' = `rc`rc'', nformat(##.#%)
}


**# // by cluster
local col1 "C D E F G H I"

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean educ_man`e' if cluster==`c'
	   matrix m`e' = e(b_mi)
	   local m`e' = m`e'[1,1]
	   local row = 1+`e'
	   putexcel `col'`row' = `m`e'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean educ_woman`e' if cluster==`c'
	   matrix w`e' = e(b_mi)
	   local w`e' = w`e'[1,1]
	   local row = 5+`e'
	   putexcel `col'`row' = `w`e'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean couple_educ`e' if cluster==`c'
	   matrix c`e' = e(b_mi)
	   local c`e' = c`e'[1,1]
	   local row = 9+`e'
	   putexcel `col'`row' = `c`e'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues r=1/4{
	   mi estimate, esampvaryok: mean race_man`r' if cluster==`c'
	   matrix r`r' = e(b_mi)
	   local r`r' = r`r'[1,1]
	   local row = 13+`r'
	   putexcel `col'`row' = `r`r'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues r=1/4{
	   mi estimate, esampvaryok: mean race_woman`r'  if cluster==`c'
	   matrix rw`r' = e(b_mi)
	   local rw`r' = rw`r'[1,1]
	   local row = 17+`r'
	   putexcel `col'`row' = `rw`r'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean age_man1 if cluster==`c'
	matrix a = e(b_mi)
	local a = a[1,1]
	putexcel `col'22 = `a', nformat(##.#)
}


forvalues c=1/7{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean age_woman1 if cluster==`c'
	matrix aw = e(b_mi)
	local aw = aw[1,1]
	putexcel `col'23 = `aw', nformat(##.#)
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues b=1/4{
	   mi estimate, esampvaryok: mean bc_man`b' if cluster==`c'
	   matrix b`b' = e(b_mi)
	   local b`b' = b`b'[1,1]
	   local row = 23+`b'
	   putexcel `col'`row' = `b`b'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues b=1/4{
	   mi estimate, esampvaryok: mean bc_woman`b' if cluster==`c'
	   matrix bw`b' = e(b_mi)
	   local bw`b' = bw`b'[1,1]
	   local row = 27+`b'
	   putexcel `col'`row' = `bw`b'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues ce=0/3{
	   mi estimate, esampvaryok: mean earn_quart`ce' if cluster==`c'
	   matrix ce`ce' = e(b_mi)
	   local ce`ce' = ce`ce'[1,1]
	   local row = 32+`ce'
	   putexcel `col'`row' = `ce`ce'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean couple_earnings_t1 if cluster==`c'
	matrix ce = e(b_mi)
	local ce = ce[1,1]
	putexcel `col'36 = `ce', nformat(#####)
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues rc=1/2{
	   mi estimate, esampvaryok: mean relcoh`rc' if cluster==`c'
	   matrix rc`rc' = e(b_mi)
	   local rc`rc' = rc`rc'[1,1]
	   local row = 36+`rc'
	   putexcel `col'`row' = `rc`rc'', nformat(##.#%)
	}
}


********************************************************************************
**# Describe sequences
********************************************************************************
// I think the data need to be LONG here (to get average across durations)

mi reshape long ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end rel_type couple_num_children_gp_end family_type_end in_sample hh_status relationship housework_focal age_focal weekly_hrs_t_focal earnings_t_focal family_income_t partnered_imp educ_focal_imp num_children_imp_hh weekly_hrs_woman weekly_hrs_man housework_woman housework_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man  in_sample_sp hh_status_sp relationship_sp housework_focal_sp age_focal_sp weekly_hrs_t_focal_sp earnings_t_focal_sp family_income_t_sp partnered_imp_sp num_children_imp_hh_sp, i(unique_id partner_id rel_start_all rel_end_all) j(duration) 

tab _mi_m cluster, m

drop if duration==11

mi passive: gen work_seq = couple_work_ow_end
mi passive: gen hw_seq = couple_hw_hrs_alt_end
mi passive: gen fam_seq = family_type_end

label values work_seq couple_work_ow
label values hw_seq couple_hw_hrs
label values fam_seq family_type

tab work_seq, m
tab hw_seq, m
tab fam_seq, m

putexcel set "$tables/cluster_composition_complete.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Many Children"
putexcel D1 = "2: OW and Late Parenthood"
putexcel E1 = "3: Continuous Work and Less Children"
putexcel F1 = "4: Trans to Neotraditional"
putexcel G1 = "5: Traditional"
putexcel H1 = "6: Continuous Work and More Children"
putexcel I1 = "7: Continuous Work and Childfree"

putexcel A2 = "Work"
putexcel A3 = "Male BW"
putexcel A4 = "1.5 Male BW"
putexcel A5 = "Dual FT: no OW"
putexcel A6 = "Dual FT: his OW"
putexcel A7 = "Dual FT: her OW"
putexcel A8 = "Dual FT: both OW"
putexcel A9 = "Female BW"
putexcel A10 = "Underwork"

putexcel A11 = "Housework"
putexcel A12 = "Woman All: High"
putexcel A13 = "Woman All: Low"
putexcel A14 = "Woman Most: High"
putexcel A15 = "Woman Most: Med"
putexcel A16 = "Woman Most: Low"
putexcel A17 = "Equal: High"
putexcel A18 = "Equal: Low"
putexcel A19 = "Man Most: High"
putexcel A20 = "Man Most: Low"

putexcel A21 = "Family"
putexcel A22 = "Married, 0 Ch"
putexcel A23 = "Married, 1 Ch"
putexcel A24 = "Married, 2 Ch"
putexcel A25 = "Married, 3+ Ch"
putexcel A26 = "Cohab, 0 Ch"
putexcel A27 = "Cohab, 1 Ch"
putexcel A28 = "Cohab, 2 Ch"
putexcel A29 = "Cohab, 3+ Ch"

// full sample
forvalues w=1/8{
   capture mi passive: gen work_seq`w' = work_seq==`w'
   mi estimate: mean work_seq`w'
   matrix w`w' = e(b_mi)
   local w`w' = w`w'[1,1]
   local row = 2+`w'
   putexcel B`row' = `w`w'', nformat(##.#%)
}

forvalues h=1/9{
   capture mi passive: gen hw_seq`h' = hw_seq==`h'
   mi estimate: mean hw_seq`h'
   matrix h`h' = e(b_mi)
   local h`h' = h`h'[1,1]
   local row = 11+`h'
   putexcel B`row' = `h`h'', nformat(##.#%)
}

forvalues f=1/8{
   capture mi passive: gen fam_seq`f' = fam_seq==`f'
   mi estimate: mean fam_seq`f'
   matrix f`f' = e(b_mi)
   local f`f' = f`f'[1,1]
   local row = 21+`f'
   putexcel B`row' = `f`f'', nformat(##.#%)
}

// by cluster
local col1 "C D E F G H I"

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues w=1/8{
	   mi estimate, esampvaryok: mean work_seq`w' if cluster==`c'
	   matrix w`w' = e(b_mi)
	   local w`w' = w`w'[1,1]
	   local row = 2+`w'
	   putexcel `col'`row' = `w`w'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues h=1/9{
	   mi estimate, esampvaryok: mean hw_seq`h' if cluster==`c'
	   matrix h`h' = e(b_mi)
	   local h`h' = h`h'[1,1]
	   local row = 11+`h'
	   putexcel `col'`row' = `h`h'', nformat(##.#%)
	}
}

forvalues c=1/7{
	local col: word `c' of `col1'
	forvalues f=1/8{
	   mi estimate, esampvaryok: mean fam_seq`f' if cluster==`c'
	   matrix f`f' = e(b_mi)
	   local f`f' = f`f'[1,1]
	   local row = 21+`f'
	   putexcel `col'`row' = `f`f'', nformat(##.#%)
	}
}
