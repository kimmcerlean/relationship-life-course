
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
use "$temp/UKHLS_clusters.dta", clear

// egen couple_id = group(pidp eligible_partner)
// browse couple_id pidp eligible_partner _mi_id // okay these don't match (made couple id earlier in the process)
unique couple_id _mi_id
unique pidp eligible_partner

gen mim = _mi_m

keep couple_id pidp eligible_partner mim mc_ward_det_4cl mc_ward_det_5cl mc4_factor mc5_factor

save "$created_data/UKHLS_clusters.dta", replace

********************************************************************************
* Now, merge clusters onto the original Stata imputed data
********************************************************************************
use "$created_data/ukhls_couples_imputed_wide.dta", clear
mi set m -= (6,7,8,9,10) // only did first 5 imputations for now

mi update

gen mim = _mi_m

// mi merge 1:1 couple_id mim using "$created_data/PSID_clusters.dta", gen(howmatch) // keep(match) 
merge 1:1 couple_id mim using "$created_data/UKHLS_clusters.dta"

tab _mi_m _merge, m // confirm that this is mi 0 - it is
drop mim
drop _merge

mi update

********************************************************************************
* Figure out the data structure
********************************************************************************
browse pidp eligible_partner _mi_m mc_ward_det_4cl mc_ward_det_5cl mc4_factor mc5_factor
// there are no sequence objects here. Are there supposed to be? Or we just care about cluster membership?

tab mc_ward_det_4cl, m
tab mc_ward_det_5cl, m
tab mc4_factor, m
tab mc5_factor, m // these match the charts I created but, is it problematic that no one in imputation 0 is assigned to a cluster? or is that fine? do I give them their own cluster maybe?
tab mc5_factor mc_ward_det_5cl // so the two different cluster options lead to two different arrangements...

// for now, very crude labels
capture label define mc5_factor 1 "attrition" 2 "underemployed no kids" 3 "traditonal" 4 "egalitarian" 5 "dissolution"
label values mc5_factor mc5_factor

********************************************************************************
* Any variables still need to be created
********************************************************************************
// couple education: this was fixed, not imputed, so don't need to use mi passive
gen education_man=hiqual_fixed if SEX==1
replace education_man=hiqual_fixed_sp if SEX==2
replace education_man = 6 if education_man == 9 // so they are consecutive

gen education_woman=hiqual_fixed if SEX==2
replace education_woman=hiqual_fixed_sp if SEX==1
replace education_woman = 6 if education_woman == 9 

capture label define hiqual 1 "Degree" 2 "Other Higher Degree" 3 "A level" 4 "GCSE" 5 "Other qual" 6 "No qual"
label values education_man education_woman hiqual

gen couple_educ_type=.
replace couple_educ_type = 1 if inrange(education_man,2,6) & inrange(education_woman,2,6)
replace couple_educ_type = 2 if inrange(education_man,2,6) & education_woman==1
replace couple_educ_type = 3 if education_man==1 & inrange(education_woman,2,6)
replace couple_educ_type = 4 if education_man==1 & education_woman==1

capture label define couple_educ_type 1 "Neither College" 2 "Her College" 3 "Him College" 4 "Both College"
label values couple_educ_type couple_educ_type
tab couple_educ_type, m // does this feel very uneducated?
tab _mi_m couple_educ_type, row

// gendered race variables
tab xw_ethn_dv _mi_m , m

gen ethn_man=xw_ethn_dv if SEX==1
replace ethn_man=xw_ethn_dv_sp if SEX==2

gen ethn_woman=xw_ethn_dv if SEX==2
replace ethn_woman=xw_ethn_dv_sp if SEX==1

gen race_man = .
replace race_man = 0 if inrange(ethn_man,5,97) 
replace race_man = 1 if inrange(ethn_man,1,4) 

gen race_woman = .
replace race_woman = 0 if inrange(ethn_woman,5,97) 
replace race_woman = 1 if inrange(ethn_woman,1,4) 

gen same_race=.
replace same_race=0 if race_man!=race_woman
replace same_race=1 if race_man==race_woman

// country?
recode gor_dv1 (1/9=1)(10=2)(11=3)(12=4), gen(country1)
label values gor_dv1 country_all

label values country1 country

// make some sort of birth cohort? can also use age to describe within cluster (but categorical will be easier for between cluster)
// well, age is hard because time-varying. so could just use at time 0
gen birth_yr_man=dob if SEX==1
replace birth_yr_man=dob_sp if SEX==2

gen birth_yr_woman=dob if SEX==2
replace birth_yr_woman=dob_sp if SEX==1

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

gen age_man1 = age_all1  if SEX==1
replace age_man1 = age_all_sp1 if SEX==2

gen age_woman1=age_all1 if SEX==2
replace age_woman1=age_all_sp1 if SEX==1

gen age_gp_woman1 = .
replace age_gp_woman1 = 1 if age_woman1 < = 24
replace age_gp_woman1 = 2 if age_woman1 > 24 & age_woman1 < = 34
replace age_gp_woman1 = 3 if age_woman1 > 34 & age_woman1 < = 1000

label define age_gp 1 "18-24" 2 "25-34" 3 "35+"
label values age_gp_woman1 age_gp

// relationship start as well
gen rel_cohort=.
replace rel_cohort = 1 if eligible_rel_start_year >=1900 & eligible_rel_start_year<2000
replace rel_cohort = 2 if eligible_rel_start_year >=2000 & eligible_rel_start_year<2005
replace rel_cohort = 3 if eligible_rel_start_year >=2005 & eligible_rel_start_year<2020
tab rel_cohort, m

label define rel_cohort 1 "1990s" 2 "2000-2005" 3 "2005+"
label values rel_cohort rel_cohort

// income or earnings (perhaps earnings is a bit endogenous to employment, but could work)
// these are imputed so need mi passive
mi passive: egen couple_earnings_t1 = rowtotal(fimnlabgrs_dv1 fimnlabgrs_dv_sp1), missing

browse pidp eligible_partner couple_earnings_t1 fimnlabgrs_dv1 fimnlabgrs_dv_sp1 fihhmngrs_dv1 fihhmngrs_dv_sp1 
sum couple_earnings_t1, detail

mi passive: egen couple_earnings_quart1 = cut(couple_earnings_t1) if couple_earnings_t1>0 & couple_earnings_t1!=., group(4)
tab couple_earnings_quart1, m
mi passive: replace couple_earnings_quart1 = couple_earnings_quart1 + 1
mi passive: replace couple_earnings_quart1 = 0 if couple_earnings_t1<=0

tabstat couple_earnings_t1, by(couple_earnings_quart1)
tab _mi_m couple_earnings_quart1

mi update

save "$created_data/UKHLS_clusters_analysis.dta", replace

********************************************************************************
**# Within cluster descriptives
********************************************************************************
*Descriptive table for entire sample
// just doing as part of below
// desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1, filename("$tables/desc_sample_all") stats(mimean) 

//  validate that these match when done below
mi estimate: proportion couple_educ_type country1
mi estimate: mean couple_earnings_t1

mi estimate, esampvaryok: proportion couple_educ_type country1 if mc5_factor==1
mi estimate, esampvaryok: proportion couple_educ_type country1 if mc5_factor==4
mi estimate, esampvaryok: mean couple_earnings_t1  if mc5_factor==1
tabstat couple_earnings_t1, by(mc5_factor)


// tab education_man, gen(educ_man)
// mi estimate: proportion education_man
// mi estimate: mean educ_man1 educ_man2 educ_man3 educ_man4

*Descriptive table by cluster
putexcel set "$tables/UKHLS_descriptives_by_cluster.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Attrition"
putexcel D1 = "2: Underemployed No Kids"
putexcel E1 = "3: Traditonal"
putexcel F1 = "4: Egalitarian"
putexcel G1 = "5: Dissolution"
putexcel A2 = "Educ Man: Degree"
putexcel A3 = "Educ Man: Other Higher Degree"
putexcel A4 = "Educ Man: A Level"
putexcel A5 = "Educ Man: GCSE"
putexcel A6 = "Educ Man: Other Qual"
putexcel A7 = "Educ Man: No Qual"
putexcel A8 = "Educ Woman: Degree"
putexcel A9 = "Educ Woman: Other Higher Degree"
putexcel A10 = "Educ Woman: A Level"
putexcel A11 = "Educ Woman: GCSE"
putexcel A12 = "Educ Woman: Other Qual"
putexcel A13 = "Educ Woman: No Qual"
putexcel A14 = "Couple Educ: Neither College"
putexcel A15 = "Couple Educ: Her College"
putexcel A16 = "Couple Educ: Him College"
putexcel A17 = "Couple Educ: Both College"
putexcel A18 = "Race Man: Non-White"
putexcel A19 = "Race Man: White"
putexcel A20 = "Race Woman: Non-White"
putexcel A21 = "Race Woman: White"
putexcel A22 = "Country: England"
putexcel A23 = "Country: Wales"
putexcel A24 = "Country: Scotland"
putexcel A25 = "Country: N. Ireland"
putexcel A26 = "Age Man"
putexcel A27 = "Age Woman"
putexcel A28 = "Birth Cohort Man: Pre 1960s"
putexcel A29 = "Birth Cohort Man: 1960s"
putexcel A30 = "Birth Cohort Man: 1970s"
putexcel A31 = "Birth Cohort Man: 1980s+"
putexcel A32 = "Birth Cohort Woman: Pre 1960s"
putexcel A33 = "Birth Cohort Woman: 1960s"
putexcel A34 = "Birth Cohort Woman: 1970s"
putexcel A35 = "Birth Cohort Woman: 1980s+"
putexcel A36 = "Couple Earnings: $0"
putexcel A37 = "Couple Earnings: Q1"
putexcel A38 = "Couple Earnings: Q2"
putexcel A39 = "Couple Earnings: Q3"
putexcel A40 = "Couple Earnings: Q4"
putexcel A41 = "Couple Earnings"
putexcel A42 = "Rel Cohort: 1990-1999"
putexcel A43 = "Rel Cohort: 2000-2005"
putexcel A44 = "Rel Cohort: 2005+"

// full sample
forvalues e=1/6{
   capture gen educ_man`e' = education_man==`e'
   mi estimate: mean educ_man`e'
   matrix m`e' = e(b_mi)
   local m`e' = m`e'[1,1]
   local row = 1+`e'
   putexcel B`row' = `m`e'', nformat(##.#%)
}

forvalues e=1/6{
   capture gen educ_woman`e' = education_woman==`e'
   mi estimate: mean educ_woman`e'
   matrix w`e' = e(b_mi)
   local w`e' = w`e'[1,1]
   local row = 7+`e'
   putexcel B`row' = `w`e'', nformat(##.#%)
}

forvalues e=1/4{
   capture gen couple_educ`e' = couple_educ_type==`e'
   mi estimate: mean couple_educ`e'
   matrix c`e' = e(b_mi)
   local c`e' = c`e'[1,1]
   local row = 13+`e'
   putexcel B`row' = `c`e'', nformat(##.#%)
}

forvalues r=0/1{
   capture gen race_man`r' = race_man==`r'
   mi estimate: mean race_man`r'
   matrix r`r' = e(b_mi)
   local r`r' = r`r'[1,1]
   local row = 18+`r'
   putexcel B`row' = `r`r'', nformat(##.#%)
}

forvalues r=0/1{
   capture gen race_woman`r' = race_woman==`r'
   mi estimate: mean race_woman`r'
   matrix rw`r' = e(b_mi)
   local rw`r' = rw`r'[1,1]
   local row = 20+`r'
   putexcel B`row' = `rw`r'', nformat(##.#%)
}

forvalues c=1/4{
   capture gen country_x`c' = country1==`c'
   mi estimate: mean country_x`c'
   matrix c`c' = e(b_mi)
   local c`c' = c`c'[1,1]
   local row = 21+`c'
   putexcel B`row' = `c`c'', nformat(##.#%)
}

mi estimate: mean age_man1
matrix a = e(b_mi)
local a = a[1,1]
putexcel B26 = `a', nformat(##.#)

mi estimate: mean age_woman1
matrix aw = e(b_mi)
local aw = aw[1,1]
putexcel B27 = `aw', nformat(##.#)
   
forvalues b=1/4{
   capture gen bc_man`b' = bcohort_man==`b'
   mi estimate: mean bc_man`b'
   matrix b`b' = e(b_mi)
   local b`b' = b`b'[1,1]
   local row = 27+`b'
   putexcel B`row' = `b`b'', nformat(##.#%)
}

forvalues b=1/4{
   capture gen bc_woman`b' = bcohort_woman==`b'
   mi estimate: mean bc_woman`b'
   matrix bw`b' = e(b_mi)
   local bw`b' = bw`b'[1,1]
   local row = 31+`b'
   putexcel B`row' = `bw`b'', nformat(##.#%)
}

forvalues ce=0/4{
   capture mi passive: gen earn_quart`ce' = couple_earnings_quart1==`ce'
   mi estimate: mean earn_quart`ce'
   matrix ce`ce' = e(b_mi)
   local ce`ce' = ce`ce'[1,1]
   local row = 36+`ce'
   putexcel B`row' = `ce`ce'', nformat(##.#%)
}

mi estimate: mean couple_earnings_t1
matrix ce = e(b_mi)
local ce = ce[1,1]
putexcel B41 = `ce', nformat(#####)

forvalues rc=1/3{
   capture gen relcoh`rc' = rel_cohort==`rc'
   mi estimate: mean relcoh`rc'
   matrix rc`rc' = e(b_mi)
   local rc`rc' = rc`rc'[1,1]
   local row = 41+`rc'
   putexcel B`row' = `rc`rc'', nformat(##.#%)
}


**# // by cluster
local col1 "C D E F G"

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues e=1/6{
	   mi estimate, esampvaryok: mean educ_man`e' if mc5_factor==`c'
	   matrix m`e' = e(b_mi)
	   local m`e' = m`e'[1,1]
	   local row = 1+`e'
	   putexcel `col'`row' = `m`e'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues e=1/6{
	   mi estimate, esampvaryok: mean educ_woman`e' if mc5_factor==`c'
	   matrix w`e' = e(b_mi)
	   local w`e' = w`e'[1,1]
	   local row = 7+`e'
	   putexcel `col'`row' = `w`e'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues e=1/4{
	   mi estimate, esampvaryok: mean couple_educ`e' if mc5_factor==`c'
	   matrix c`e' = e(b_mi)
	   local c`e' = c`e'[1,1]
	   local row = 13+`e'
	   putexcel `col'`row' = `c`e'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues r=0/1{
	   mi estimate, esampvaryok: mean race_man`r' if mc5_factor==`c'
	   matrix r`r' = e(b_mi)
	   local r`r' = r`r'[1,1]
	   local row = 18+`r'
	   putexcel `col'`row' = `r`r'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues r=0/1{
	   mi estimate, esampvaryok: mean race_woman`r'  if mc5_factor==`c'
	   matrix rw`r' = e(b_mi)
	   local rw`r' = rw`r'[1,1]
	   local row = 20+`r'
	   putexcel `col'`row' = `rw`r'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues co=1/4{
	   mi estimate, esampvaryok: mean country_x`co' if mc5_factor==`c'
	   matrix co`co' = e(b_mi)
	   local co`co' = co`co'[1,1]
	   local row = 21+`co'
	   putexcel `col'`row' = `co`co'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean age_man1 if mc5_factor==`c'
	matrix a = e(b_mi)
	local a = a[1,1]
	putexcel `col'26 = `a', nformat(##.#)
}


forvalues c=1/5{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean age_woman1 if mc5_factor==`c'
	matrix aw = e(b_mi)
	local aw = aw[1,1]
	putexcel `col'27 = `aw', nformat(##.#)
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues b=1/4{
	   mi estimate, esampvaryok: mean bc_man`b' if mc5_factor==`c'
	   matrix b`b' = e(b_mi)
	   local b`b' = b`b'[1,1]
	   local row = 27+`b'
	   putexcel `col'`row' = `b`b'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues b=1/4{
	   mi estimate, esampvaryok: mean bc_woman`b' if mc5_factor==`c'
	   matrix bw`b' = e(b_mi)
	   local bw`b' = bw`b'[1,1]
	   local row = 31+`b'
	   putexcel `col'`row' = `bw`b'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues ce=0/4{
	   mi estimate, esampvaryok: mean earn_quart`ce' if mc5_factor==`c'
	   matrix ce`ce' = e(b_mi)
	   local ce`ce' = ce`ce'[1,1]
	   local row = 36+`ce'
	   putexcel `col'`row' = `ce`ce'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	mi estimate, esampvaryok: mean couple_earnings_t1 if mc5_factor==`c'
	matrix ce = e(b_mi)
	local ce = ce[1,1]
	putexcel `col'41 = `ce', nformat(#####)
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues rc=1/3{
	   mi estimate, esampvaryok: mean relcoh`rc' if mc5_factor==`c'
	   matrix rc`rc' = e(b_mi)
	   local rc`rc' = rc`rc'[1,1]
	   local row = 41+`rc'
	   putexcel `col'`row' = `rc`rc'', nformat(##.#%)
	}
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

********************************************************************************
**# Describe sequences
********************************************************************************
// I think the data need to be LONG here (to get average across durations)
// use "$created_data/PSID_clusters_analysis.dta", clear

// replace mc5_factor=0 if mc5_factor==.

mi reshape long age_all fihhmngrs_dv gor_dv nkids_dv jbstat aidhh aidxhh aidhrs howlng work_hours jbhrs fimnlabgrs_dv nchild_dv hiqual_dv country_all employed total_hours age_youngest_child partnered_imp marital_status_imp aidhrs_rec int_year orig_record age_all_sp fihhmngrs_dv_sp gor_dv_sp nkids_dv_sp jbstat_sp aidhrs_sp howlng_sp work_hours_sp jbhrs_sp fimnlabgrs_dv_sp employed_sp total_hours_sp age_youngest_child_sp partnered_imp_sp marital_status_imp_sp aidhrs_rec_sp weekly_hrs_woman weekly_hrs_man housework_woman housework_man marital_status_woman marital_status_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man couple_work couple_work_ow  couple_hw_total woman_hw_share hw_terc_woman hw_hilow_woman hw_hilow_man couple_hw hw_hilow_woman_gp1 hw_hilow_woman_gp2 hw_hilow_man_gp4 couple_hw_hrs couple_hw_hrs_alt rel_type couple_num_children couple_num_children_gp family_type ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end ft_pt_det_woman_end ft_pt_det_man_end couple_work_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_num_children_gp_end family_type_end ///
, i(pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status) j(duration)


tab _mi_m mc5_factor, m

mi passive: gen work_seq = couple_work_ow_end
mi passive: replace work_seq = 9 if couple_work_ow_end==98
mi passive: replace work_seq = 10 if couple_work_ow_end==99

mi passive: gen hw_seq = couple_hw_hrs_alt_end
mi passive: replace hw_seq = 10 if couple_hw_hrs_alt_end==98
mi passive: replace hw_seq = 11 if couple_hw_hrs_alt_end==99

mi passive: gen fam_seq = family_type_end
mi passive: replace fam_seq = 9 if family_type_end==98
mi passive: replace fam_seq = 10 if family_type_end==99

label values work_seq couple_work_ow
label values hw_seq couple_hw_hrs
label values fam_seq family_type

putexcel set "$tables/UKHLS_cluster_composition.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Attrition"
putexcel D1 = "2: Underemployed No Kids"
putexcel E1 = "3: Traditional"
putexcel F1 = "4: Egalitarian"
putexcel G1 = "5: Dissolution"

putexcel A2 = "Work"
putexcel A3 = "Male BW"
putexcel A4 = "1.5 Male BW"
putexcel A5 = "Dual FT: no OW"
putexcel A6 = "Dual FT: his OW"
putexcel A7 = "Dual FT: her OW"
putexcel A8 = "Dual FT: both OW"
putexcel A9 = "Female BW"
putexcel A10 = "Underwork"
putexcel A11 = "Dissolved"
putexcel A12 = "Attrited"

putexcel A14 = "Housework"
putexcel A15 = "Woman All: High"
putexcel A16 = "Woman All: Low"
putexcel A17 = "Woman Most: High"
putexcel A18 = "Woman Most: Med"
putexcel A19 = "Woman Most: Low"
putexcel A20 = "Equal: High"
putexcel A21 = "Equal: Low"
putexcel A22 = "Man Most: High"
putexcel A23 = "Man Most: Low"
putexcel A24 = "Dissolved"
putexcel A25 = "Attrited"

putexcel A27 = "Family"
putexcel A28 = "Married, 0 Ch"
putexcel A29 = "Married, 1 Ch"
putexcel A30 = "Married, 2 Ch"
putexcel A31 = "Married, 3+ Ch"
putexcel A32 = "Cohab, 0 Ch"
putexcel A33 = "Cohab, 1 Ch"
putexcel A34 = "Cohab, 2 Ch"
putexcel A35 = "Cohab, 3+ Ch"
putexcel A36 = "Dissolved"
putexcel A37 = "Attrited"

// full sample
forvalues w=1/10{
   capture mi passive: gen work_seq`w' = work_seq==`w'
   mi estimate: mean work_seq`w'
   matrix w`w' = e(b_mi)
   local w`w' = w`w'[1,1]
   local row = 2+`w'
   putexcel B`row' = `w`w'', nformat(##.#%)
}

forvalues h=1/11{
   capture mi passive: gen hw_seq`h' = hw_seq==`h'
   mi estimate: mean hw_seq`h'
   matrix h`h' = e(b_mi)
   local h`h' = h`h'[1,1]
   local row = 14+`h'
   putexcel B`row' = `h`h'', nformat(##.#%)
}

forvalues f=1/10{
   capture mi passive: gen fam_seq`f' = fam_seq==`f'
   mi estimate: mean fam_seq`f'
   matrix f`f' = e(b_mi)
   local f`f' = f`f'[1,1]
   local row = 27+`f'
   putexcel B`row' = `f`f'', nformat(##.#%)
}

// by cluster
local col1 "C D E F G"

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues w=1/10{
	   mi estimate, esampvaryok: mean work_seq`w' if mc5_factor==`c'
	   matrix w`w' = e(b_mi)
	   local w`w' = w`w'[1,1]
	   local row = 2+`w'
	   putexcel `col'`row' = `w`w'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues h=1/11{
	   mi estimate, esampvaryok: mean hw_seq`h' if mc5_factor==`c'
	   matrix h`h' = e(b_mi)
	   local h`h' = h`h'[1,1]
	   local row = 14+`h'
	   putexcel `col'`row' = `h`h'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues f=1/10{
	   mi estimate, esampvaryok: mean fam_seq`f' if mc5_factor==`c'
	   matrix f`f' = e(b_mi)
	   local f`f' = f`f'[1,1]
	   local row = 27+`f'
	   putexcel `col'`row' = `f`f'', nformat(##.#%)
	}
}
