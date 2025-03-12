
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

********************************************************************************
* Within cluster descriptives
********************************************************************************
*Descriptive table for entire sample
desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1, filename("$tables/desc_sample_all") stats(mimean) 

mi estimate: proportion couple_educ_type raceth_woman // validate that this matches

*Descriptive table by cluster
forvalues c=1/5{
	preserve
	keep if mc5_factor == `c'
	
	desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1 if mc5_factor==`c' ///
	, filename("$tables/desc_cluster_`c'") 	stats(mimean) decimals(4)
	
	restore
}


********************************************************************************
* Between cluster descriptives
********************************************************************************
tab couple_educ_type mc5_factor, row // how to do this with mi? Is this just where we have to use regression?
// yes, the below with no controls matches this exactly

global controls "i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 c.couple_earnings_t1"

mi estimate: mlogit mc5_factor i.couple_educ_type, cluster(couple_id) baseoutcome(1)
mimrgns couple_educ_type, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post // predict(pr) // pwcompare
outreg2 using "$models/test.xls", stats(coef se ci_low ci_high) sideway label(proper) ctitle(no) replace

mi estimate, saving("$models/couple_educ", replace) post: mlogit mc5_factor i.couple_educ_type $controls, cluster(couple_id) baseoutcome(1)
mimrgns couple_educ_type, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post // predict(pr) // pwcompare
outreg2 using "$models/test.xls", stats(coef se ci_low ci_high) sideway label(insert) ctitle(controls) append
