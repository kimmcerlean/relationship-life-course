
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: describe_clusters
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes the clusters created in R and imports to stata
* Creates some needed variables and then gets preliminary descriptives

********************************************************************************
* First, clean up the cluster file, so just has variables I want
********************************************************************************
use "$temp/PSID_clusters.dta", clear

// append using "$created_data/psid_couples_base.dta"
// browse unique_id partner_id _mi_m _mi_id
// tab _mi_miss, m
// drop _mi_miss

egen couple_id = group(unique_id partner_id)
// rename _mi_id couple_id
unique couple_id _mi_id
unique unique_id partner_id

gen mim = _mi_m

keep couple_id unique_id partner_id mim mc_ward_det_4cl mc_ward_det_5cl mc4_factor mc5_factor

save "$created_data/PSID_clusters.dta", replace

// so if I do flong (and rename _mi_m to m so it's created again), Stata turns _mi_m of 1 into 0 and all of my passive variables are messed up and become same across imputations - so this is problematic
// if I do wide, the desctable doesn't seem to recognize that these are imputed data, but it does retain diffs across imputations, there is just not a 0.

/* jic I need this:
// mi import flong, m(m) id(couple_id) // so when I do this, Stata then turns m of 1 to _mi_m of 0
mi import wide // if I get rid of _mi_m, not created
// mi convert flong // no, this really doesn't work, now _mi_m is all 0

mi register imputed housework_focal* weekly_hrs_t_focal* earnings_t_focal* family_income_t* partnered_imp* num_children_imp_hh* // housework_focal_sp* weekly_hrs_t_focal_sp* earnings_t_focal_sp* family_income_t_sp* partnered_imp_sp* num_children_imp_hh_sp*

mi register passive weekly_hrs_woman* weekly_hrs_man* housework_woman* housework_man* partnered_woman* partnered_man* num_children_woman* num_children_man* ft_pt_woman* overwork_woman* ft_pt_man* overwork_man* ft_pt_det_woman* ft_pt_det_man* rel_type* couple_work_ow_end* couple_hw_end* couple_hw_hrs_end* couple_hw_hrs_alt_end* couple_num_children_gp_end* family_type_end* couple_work_end* // ft_pt_woman_end* overwork_woman_end* ft_pt_man_end* overwork_man_end*

mi register regular rel_start_all rel_end_all age_focal* SEX rel_status rel_type_constant FIRST_BIRTH_YR sample_type  birth_yr_all raceth_fixed_focal fixed_education SEX_sp FIRST_BIRTH_YR_sp sample_type_sp birth_yr_all_sp raceth_fixed_focal_sp fixed_education_sp // age_focal_sp* min_dur max_dur last_yr_observed ended has_psid_gene has_psid_gene_sp 
*/

********************************************************************************
* Now, merge clusters onto the original Stata imputed data
********************************************************************************
use "$created_data/psid_couples_imputed_wide.dta", clear

gen mim = _mi_m
egen couple_id = group (unique_id partner_id)

// mi merge 1:1 couple_id mim using "$created_data/PSID_clusters.dta", gen(howmatch) // keep(match) 
merge 1:1 couple_id mim using "$created_data/PSID_clusters.dta"

tab _mi_m _merge, m // confirm that this is mi 0 - it is
drop mim
drop _merge

mi update

********************************************************************************
* Figure out the data structure
********************************************************************************
browse unique_id partner_id _mi_m mc_ward_det_4cl mc_ward_det_5cl mc4_factor mc5_factor
// there are no sequence objects here. Are there supposed to be? Or we just care about cluster membership?

tab mc_ward_det_4cl, m
tab mc_ward_det_5cl, m
tab mc4_factor, m
tab mc5_factor, m // these match the charts I created but, is it problematic that no one in imputation 0 is assigned to a cluster? or is that fine? do I give them their own cluster maybe?
tab mc5_factor mc_ward_det_5cl // so the two different cluster options lead to two different arrangements...

// for now, very crude labels
capture label define mc5_factor 1 "traditional" 2 "attrition" 3 "dissolution" 4 "egal with kids" 5 "egal-ish no kids"
label values mc5_factor mc5_factor

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

// income or earnings (perhaps earnings is a bit endogenous to employment, but could work)
// these are imputed so need mi passive
mi passive: egen couple_earnings_t1 = rowtotal(earnings_t_focal1 earnings_t_focal_sp1)

browse unique_id partner_id family_income_t1 family_income_t_sp1 couple_earnings_t1 earnings_t_focal1 earnings_t_focal_sp1
sum couple_earnings_t1, detail

mi passive: egen couple_earnings_quart1 = cut(couple_earnings_t1) if couple_earnings_t1!=0, group(4)
tab couple_earnings_quart1, m
mi passive: replace couple_earnings_quart1 = couple_earnings_quart1 + 1
mi passive: replace couple_earnings_quart1 = 0 if couple_earnings_t1==0

tabstat couple_earnings_t1, by(couple_earnings_quart1)
tab _mi_m couple_earnings_quart1

mi update
// mi register regular education_man education_woman couple_educ_type raceth_man raceth_woman birth_yr_man birth_yr_woman bcohort_man bcohort_woman age_man1 age_woman1 // do I need to register?

save "$created_data/PSID_clusters_analysis.dta", replace

********************************************************************************
**# Within cluster descriptives
********************************************************************************
*Descriptive table for entire sample
desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1, filename("$tables/desc_sample_all") stats(mimean) 

mi estimate: proportion couple_educ_type raceth_woman // validate that this matches
mi estimate: mean couple_earnings_t1

mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==1
mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==4
mi estimate, esampvaryok: mean couple_earnings_t1  if mc5_factor==1
tabstat couple_earnings_t1, by(mc5_factor)


// tab education_man, gen(educ_man)
// mi estimate: proportion education_man
// mi estimate: mean educ_man1 educ_man2 educ_man3 educ_man4

*Descriptive table by cluster
putexcel set "$tables/descriptives_by_cluster.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Traditional"
putexcel D1 = "2: Attrition"
putexcel E1 = "3: Dissolution"
putexcel F1 = "4: Egal with Kids"
putexcel G1 = "5: Egal-ish No Kids"
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
putexcel A17 = "Race Man: NH Asian"
putexcel A18 = "Race Man: NH Other"
putexcel A19 = "Race Woman: NH White"
putexcel A20 = "Race Woman: Black"
putexcel A21 = "Race Woman: Hispanic"
putexcel A22 = "Race Woman: NH Asian"
putexcel A23 = "Race Woman: NH Other"
putexcel A24 = "Age Man"
putexcel A25 = "Age Woman"
putexcel A26 = "Birth Cohort Man: Pre 1960s"
putexcel A27 = "Birth Cohort Man: 1960s"
putexcel A28 = "Birth Cohort Man: 1970s"
putexcel A29 = "Birth Cohort Man: 1980s+"
putexcel A30 = "Birth Cohort Woman: Pre 1960s"
putexcel A31 = "Birth Cohort Woman: 1960s"
putexcel A32 = "Birth Cohort Woman: 1970s"
putexcel A33 = "Birth Cohort Woman: 1980s+"
putexcel A34 = "Couple Earnings: $0"
putexcel A35 = "Couple Earnings: Q1"
putexcel A36 = "Couple Earnings: Q2"
putexcel A37 = "Couple Earnings: Q3"
putexcel A38 = "Couple Earnings: Q4"
putexcel A39 = "Couple Earnings"

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

forvalues r=1/5{
   capture gen race_man`r' = raceth_man==`r'
   mi estimate: mean race_man`r'
   matrix r`r' = e(b_mi)
   local r`r' = r`r'[1,1]
   local row = 13+`r'
   putexcel B`row' = `r`r'', nformat(##.#%)
}

forvalues r=1/5{
   capture gen race_woman`r' = raceth_woman==`r'
   mi estimate: mean race_woman`r'
   matrix rw`r' = e(b_mi)
   local rw`r' = rw`r'[1,1]
   local row = 18+`r'
   putexcel B`row' = `rw`r'', nformat(##.#%)
}

mi estimate: mean age_man1
matrix a = e(b_mi)
local a = a[1,1]
putexcel B24 = `a', nformat(##.#)

mi estimate: mean age_woman1
matrix aw = e(b_mi)
local aw = aw[1,1]
putexcel B25 = `aw', nformat(##.#)
   
forvalues b=1/4{
   capture gen bc_man`b' = bcohort_man==`b'
   mi estimate: mean bc_man`b'
   matrix b`b' = e(b_mi)
   local b`b' = b`b'[1,1]
   local row = 25+`b'
   putexcel B`row' = `b`b'', nformat(##.#%)
}

forvalues b=1/4{
   capture gen bc_woman`b' = bcohort_woman==`b'
   mi estimate: mean bc_woman`b'
   matrix bw`b' = e(b_mi)
   local bw`b' = bw`b'[1,1]
   local row = 29+`b'
   putexcel B`row' = `bw`b'', nformat(##.#%)
}

forvalues ce=0/4{
   capture mi passive: gen earn_quart`ce' = couple_earnings_quart1==`ce'
   mi estimate: mean earn_quart`ce'
   matrix ce`ce' = e(b_mi)
   local ce`ce' = ce`ce'[1,1]
   local row = 34+`ce'
   putexcel B`row' = `ce`ce'', nformat(##.#%)
}

mi estimate: mean couple_earnings_t1
matrix ce = e(b_mi)
local ce = ce[1,1]
putexcel B39 = `ce', nformat(#####)


**# // by cluster
local col1 "C D E F G"

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean educ_man`e' if mc5_factor==`c'
	   matrix m`e' = e(b_mi)
	   local m`e' = m`e'[1,1]
	   local row = 1+`e'
	   putexcel `col'`row' = `m`e'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean educ_woman`e' if mc5_factor==`c'
	   matrix w`e' = e(b_mi)
	   local w`e' = w`e'[1,1]
	   local row = 5+`e'
	   putexcel `col'`row' = `w`e'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean couple_educ`e' if mc5_factor==`c'
	   matrix c`e' = e(b_mi)
	   local c`e' = c`e'[1,1]
	   local row = 9+`e'
	   putexcel `col'`row' = `c`e'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues r=1/5{
	   mi estimate, esampvaryok: mean race_man`r' if mc5_factor==`c'
	   matrix r`r' = e(b_mi)
	   local r`r' = r`r'[1,1]
	   local row = 13+`r'
	   putexcel `col'`row' = `r`r'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues r=1/5{
	   mi estimate, esampvaryok: mean race_woman`r'  if mc5_factor==`c'
	   matrix rw`r' = e(b_mi)
	   local rw`r' = rw`r'[1,1]
	   local row = 18+`r'
	   putexcel `col'`row' = `rw`r'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean age_man1 if mc5_factor==`c'
	matrix a = e(b_mi)
	local a = a[1,1]
	putexcel `col'24 = `a', nformat(##.#)
}


forvalues c=1/5{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean age_woman1 if mc5_factor==`c'
	matrix aw = e(b_mi)
	local aw = aw[1,1]
	putexcel `col'25 = `aw', nformat(##.#)
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues b=1/4{
	   mi estimate, esampvaryok: mean bc_man`b' if mc5_factor==`c'
	   matrix b`b' = e(b_mi)
	   local b`b' = b`b'[1,1]
	   local row = 25+`b'
	   putexcel `col'`row' = `b`b'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues b=1/4{
	   mi estimate, esampvaryok: mean bc_woman`b' if mc5_factor==`c'
	   matrix bw`b' = e(b_mi)
	   local bw`b' = bw`b'[1,1]
	   local row = 29+`b'
	   putexcel `col'`row' = `bw`b'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues ce=0/4{
	   mi estimate, esampvaryok: mean earn_quart`ce' if mc5_factor==`c'
	   matrix ce`ce' = e(b_mi)
	   local ce`ce' = ce`ce'[1,1]
	   local row = 34+`ce'
	   putexcel `col'`row' = `ce`ce'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean couple_earnings_t1 if mc5_factor==`c'
	matrix ce = e(b_mi)
	local ce = ce[1,1]
	putexcel `col'39 = `ce', nformat(#####)
}

********************************************************************************
/* Doesn't work because of no mi_m==0
*Descriptive table by cluster
forvalues c=1/5{
	
	desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1 if mc5_factor==`c' ///
	, filename("$tables/desc_cluster_`c'") 	stats(mimean) decimals(4) // esampvaryok
	
}

// (system variable _mi_id updated because of changed number of obs)
// (7588 m>0 obs dropped because of dropped obs in m=0)

mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==1
mi estimate, esampvaryok: mean couple_earnings_t1  if mc5_factor==1
tabstat couple_earnings_t1, by(mc5_factor)

preserve
keep if mc5_factor==1
mi estimate: proportion couple_educ_type raceth_woman 
mi estimate: mean couple_earnings_t1
restore
*/
