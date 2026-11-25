
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
use "$temp/PSID_clusters_truncated_sequences.dta", clear

// append using "$created_data/psid_couples_base.dta"
// browse unique_id partner_unique_id _mi_m _mi_id
// tab _mi_miss, m
// drop _mi_miss

drop couple_id
egen couple_id = group(unique_id partner_unique_id)
// rename _mi_id couple_id
unique couple_id _mi_id
unique unique_id partner_unique_id

gen mim = _mi_m

keep couple_id unique_id partner_unique_id mim mc3_factor mc4_factor mc5_factor mc6_factor mc7_factor mc8_factor // old (non-truncated): mc_ward_det_4cl mc_ward_det_5cl mc4_factor mc5_factor

save "$created_data/PSID_truncated_clusters.dta", replace

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
use "$created_data/psid_couples_wide_truncated.dta", clear

mi set m -= (6,7,8,9,10) // only did first 5 imputations for now
// unique unique_id partner_unique_id if sequence_length>=3
keep if sequence_length>=3 // also only keep relationships longer than 3 years now (for truncated sequences)

mi update

gen mim = _mi_m
drop couple_id
egen couple_id = group (unique_id partner_unique_id)

// mi merge 1:1 couple_id mim using "$created_data/PSID_clusters.dta", gen(howmatch) // keep(match) 
merge 1:1 couple_id mim using "$created_data/PSID_truncated_clusters.dta"

tab _mi_m _merge, m // confirm that this is mi 0 - it is
drop mim
drop _merge

mi update

********************************************************************************
* Figure out the data structure
********************************************************************************
browse unique_id partner_unique_id _mi_m mc3_factor mc4_factor mc5_factor mc6_factor mc7_factor mc8_factor couple_work_trunc1 couple_hw_hrs_4cat_trunc1 family_type_5cat_trunc1

tab mc5_factor, m

// for now, very crude labels
capture label define mc5_factor 1 "egal cohab to 2nd shift marriage" 2 "stable second shift" ///
3 "traditionalization among direct marriers" 4 "stable trad married couples w children" 5 "egal marriage to 2nd shift marriage"

label values mc5_factor mc5_factor

tab mc5_factor couple_work_trunc4, row
tab mc5_factor couple_hw_hrs_4cat_trunc4, row
tab mc5_factor family_type_5cat_trunc4

********************************************************************************
* Any variables still need to be created
********************************************************************************
**Current couple-SES
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

// income or earnings (perhaps earnings is a bit endogenous to employment, but could work)
// these are imputed so need mi passive
mi passive: egen couple_earnings_t1 = rowtotal(earnings_t_focal1 earnings_t_focal_sp1)

browse unique_id partner_unique_id family_income_t1 family_income_t_sp1 couple_earnings_t1 earnings_t_focal1 earnings_t_focal_sp1
sum couple_earnings_t1, detail

mi passive: egen couple_earnings_quart1 = cut(couple_earnings_t1) if couple_earnings_t1!=0, group(4)
tab couple_earnings_quart1, m
mi passive: replace couple_earnings_quart1 = couple_earnings_quart1 + 1
mi passive: replace couple_earnings_quart1 = 0 if couple_earnings_t1==0

tabstat couple_earnings_t1, by(couple_earnings_quart1)
tab _mi_m couple_earnings_quart1

// do I want the more detailed employment status for those non-working? let's come back to this...
labe values employment_status_man* employment_status_woman* employment_status
tab employment_status_man1
tab employment_status_woman1

// what about home ownership status? in theory should match across partners, esp at time1?
label values housing_woman* housing_man* .
tab housing_woman1 housing_man1, m // even here close to 100%, but not perfect
tab housing_woman1 housing_man1 if _mi_m==0, m // okay yes nearly 100%

// gendered race variables
tab _mi_m raceth_fixed_focal, m

gen raceth_man=raceth_fixed_focal if SEX==1
replace raceth_man=raceth_fixed_focal_sp if SEX==2

gen raceth_woman=raceth_fixed_focal if SEX==2
replace raceth_woman=raceth_fixed_focal_sp if SEX==1

capture label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
labe values raceth_man raceth_woman raceth_fixed_focal raceth_fixed_focal_sp raceth

gen same_race=.
replace same_race=0 if raceth_man!=raceth_woman
replace same_race=1 if raceth_man==raceth_woman

// let's use sample type to create if in immigrant sample as well (these are fixed also)
gen sample_type_man=sample_type if SEX==1
replace sample_type_man=sample_type_sp if SEX==2

gen sample_type_woman=sample_type if SEX==2
replace sample_type_woman=sample_type_sp if SEX==1

label values sample_type_woman sample_type_man sample_type

gen imm_sample_man = .
replace imm_sample_man = 0 if inlist(sample_type_man,1,5)
replace imm_sample_man = 1 if inlist(sample_type_man,2,3,4,6,7)

gen imm_sample_woman = .
replace imm_sample_woman = 0 if inlist(sample_type_woman,1,5)
replace imm_sample_woman = 1 if inlist(sample_type_woman,2,3,4,6,7)

tab raceth_woman imm_sample_woman

// current residence
label values region_woman* region_man* ER85772L
tab region_woman1 region_man1, m // even here close to 100%, but not perfect
tab region_woman1 region_man1 if _mi_m==0, m // okay yes nearly 100%, so can just use woman

	// going to make alaska / hawaii / foreign as "other"
	mi passive: gen region_gp_woman1 = region_woman1
	mi passive: replace region_gp_woman1 = 5 if inlist(region_woman1,5,6)

	label define region_gp 1 "northeast" 2 "north central" 3 "south" 4 "west" 5 "other"
	label values region_gp_woman1 region_gp

* Relationship info
// age at rel start
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

// relationship order - created earier (step 5 of prep), but want to create a binary indicator of whether in first rel
tab rel_no_woman, m
tab rel_no_man, m

gen in_first_rel_woman = 0
replace in_first_rel_woman = 1 if rel_no_woman==1

gen in_first_rel_man = 0
replace in_first_rel_man = 1 if rel_no_man==1

// first birth timing relative to relationship start
	// created in earlier step

// create a binary of pre / post
	// browse unique_id partner_unique_id SEX rel_start_all first_birth_timing_woman yr_first_birth_woman first_birth_timing_man yr_first_birth_man'
	// also moved this up earlier
	
tab first_birth_timing_man first_birth_pre_rel_man, m
tab yr_first_birth_man first_birth_pre_rel_man, m
tab first_birth_timing_woman first_birth_pre_rel_woman, m
tab yr_first_birth_woman first_birth_pre_rel_woman, m

// age of oldest child...wait, if I do this at relationship start, this is actually essentially the same? so I might not need to create this? Okay duh, this is why the POSITIVE version was useful - because that is essentially what that was...okay, let's just leave for now, we can revisit coding if needed later
browse unique_id partner_unique_id SEX rel_start_all yr_first_birth_woman first_birth_timing_woman yr_first_birth_man first_birth_timing_man family_type1

// age of youngest child - this is just in the HH, prob useful descriptively but we don't 100% know with this variable if it's their kid together
tab age_youngest_woman1 age_youngest_man1, m
tab age_youngest_woman1 age_youngest_man1 if _mi_m!=0, m // okay these are less congruent, but I think following what I do for # of kids, use the woman's

// mpf status
gen mpf_man=mpf_focal if SEX==1
replace mpf_man=mpf_focal_sp if SEX==2

gen mpf_woman=mpf_focal if SEX==2
replace mpf_woman=mpf_focal_sp if SEX==1

tab mpf_woman mpf_man, m

* Family background
// okay the gendered versions were created in step 5, but I think I need to clean them a bit here?
label values father_educ_woman father_educ_man mother_educ_woman mother_educ_man parent_educ

tab father_educ_woman
recode father_educ_woman (0/3=1)(4=2)(5/6=3)(7/8=4), gen(father_educ_gp_woman)
recode father_educ_man (0/3=1)(4=2)(5/6=3)(7/8=4), gen(father_educ_gp_man)
recode mother_educ_woman (0/3=1)(4=2)(5/6=3)(7/8=4), gen(mother_educ_gp_woman)
recode mother_educ_man (0/3=1)(4=2)(5/6=3)(7/8=4), gen(mother_educ_gp_man)

recode father_educ_woman (0/6=0)(7/8=1), gen(father_college_woman)
recode father_educ_man (0/6=0)(7/8=1), gen(father_college_man)
recode mother_educ_woman (0/6=0)(7/8=1), gen(mother_college_woman)
recode mother_educ_man (0/6=0)(7/8=1), gen(mother_college_man)

label values father_educ_gp_woman father_educ_gp_man mother_educ_gp_woman mother_educ_gp_man educ

tab family_structure_man // is 1 two-parent bio? yes (example var: V1193)
// question: Were you living with both your natural parents most of the time until you were age 16?

* Social context
// make some sort of birth cohort? can also use age to describe within cluster (but categorical will be easier for between cluster)
gen bcohort_man = .
replace bcohort_man = 1 if dob_man < 1960
replace bcohort_man = 2 if dob_man >= 1960 & dob_man < 1970
replace bcohort_man = 3 if dob_man >= 1970 & dob_man < 1980
replace bcohort_man = 4 if dob_man >= 1980 & dob_man < 1990
replace bcohort_man = 5 if dob_man >= 1990 & dob_man < 2005

gen bcohort_woman = .
replace bcohort_woman = 1 if dob_woman < 1960
replace bcohort_woman = 2 if dob_woman >= 1960 & dob_woman < 1970
replace bcohort_woman = 3 if dob_woman >= 1970 & dob_woman < 1980
replace bcohort_woman = 4 if dob_woman >= 1980 & dob_woman < 1990
replace bcohort_woman = 5 if dob_woman >= 1990 & dob_woman < 2005

capture label define bcohort 1 "Pre-1960s" 2 "1960s" 3 "1970s" 4 "1980s" 5 "1990s+"
label values bcohort_man bcohort_woman bcohort

// relationship start as well
gen rel_cohort=.
replace rel_cohort = 1 if rel_start_all >=1990 & rel_start_all<2000
replace rel_cohort = 2 if rel_start_all >=2000 & rel_start_all<2010
replace rel_cohort = 3 if rel_start_all >=2010 & rel_start_all<2025
tab rel_cohort, m

label define rel_cohort 1 "1990s" 2 "2000s" 3 "2010s"
label values rel_cohort rel_cohort

// religion - need to figure out how to group these...
label values religion_man* religion_woman* religion 
tab religion_man1

mi passive: gen religion_gp_man1 = .
mi passive: replace religion_gp_man1 = 0 if inlist(religion_man1,0,1,2)
mi passive: replace religion_gp_man1 = 1 if religion_man1==3
mi passive: replace religion_gp_man1 = 2 if inlist(religion_man1,6,7,9,10,11,12,13,14)
mi passive: replace religion_gp_man1 = 2 if inrange(religion_man1,22,29)
mi passive: replace religion_gp_man1 = 3 if inlist(religion_man1,5,8,15,19,20,21)
mi passive: replace religion_gp_man1 = 4 if inlist(religion_man1,4,16,17,18,30)

tab religion_man1 religion_gp_man1, m

mi passive: gen religion_gp_woman1 = .
mi passive: replace religion_gp_woman1 = 0 if inlist(religion_woman1,0,1,2)
mi passive: replace religion_gp_woman1 = 1 if religion_woman1==3
mi passive: replace religion_gp_woman1 = 2 if inlist(religion_woman1,6,7,9,10,11,12,13,14)
mi passive: replace religion_gp_woman1 = 2 if inrange(religion_woman1,22,29)
mi passive: replace religion_gp_woman1 = 3 if inlist(religion_woman1,5,8,15,19,20,21)
mi passive: replace religion_gp_woman1 = 4 if inlist(religion_woman1,4,16,17,18,30)

tab religion_woman1 religion_gp_woman1, m

capture label define religion_gp 0 "No Religion" 1 "Catholic" 2 "Protestant" 3 "Other Christian" 4 "Other non-Christian"
label values religion_gp_man1 religion_gp_woman1 religion_gp

tab religion_gp_man1
tab religion_gp_woman1

** Other caregiving obligations (potentially)
// disability status. okay these were created in step 5 and are fine
tab disabled_man1
tab disabled_woman1
tab employment_status_woman1 disabled_woman1

// parents in HH
// this is created in very first step 1 using info about parents unique IDs to know if they are in the HH, then in variable recodes, I update to say if one or both parents is in HH. so these are likely NOT congruous across partners (bc this is just based on individual's parents)
// Update November 2026: I had to update this so it's ANY parent in HH, not number to get the imputation to work

tab any_parent_in_hh1 any_parent_in_hh_sp1, m

tab any_parent_in_hh1 // okay time 1 is not great, because I think I am sometimes capturing like a move out of the HH??
tab any_parent_in_hh2 // bc it's much less at time 2..

tab any_parent_in_hh1 any_parent_in_hh2
tab rel_type1 any_parent_in_hh1, row

egen years_parent_in_hh = rowtotal(any_parent_in_hh1 any_parent_in_hh2 any_parent_in_hh3 any_parent_in_hh4 any_parent_in_hh5 any_parent_in_hh6 any_parent_in_hh7 any_parent_in_hh8 any_parent_in_hh9 any_parent_in_hh10)
browse unique_id years_parent_in_hh any_parent_in_hh1 any_parent_in_hh2 any_parent_in_hh3 any_parent_in_hh4 any_parent_in_hh5 any_parent_in_hh6 any_parent_in_hh7 any_parent_in_hh8 any_parent_in_hh9 any_parent_in_hh10

egen years_parent_in_hh_sp = rowtotal(any_parent_in_hh_sp1 any_parent_in_hh_sp2 any_parent_in_hh_sp3 any_parent_in_hh_sp4 any_parent_in_hh_sp5 any_parent_in_hh_sp6 any_parent_in_hh_sp7 any_parent_in_hh_sp8 any_parent_in_hh_sp9 any_parent_in_hh_sp10)

gen years_parent_in_hh_man=years_parent_in_hh if SEX==1
replace years_parent_in_hh_man=years_parent_in_hh_sp if SEX==2

gen years_parent_in_hh_woman=years_parent_in_hh if SEX==2
replace years_parent_in_hh_woman=years_parent_in_hh_sp if SEX==1

tab years_parent_in_hh_woman years_parent_in_hh_man
browse unique_id years_parent_in_hh_woman years_parent_in_hh_man

egen max_years_parent_in_hh = rowmax(years_parent_in_hh_woman years_parent_in_hh_man) // new Nov 2026 (might be better)

// lives near childhood home (proxy POSSIBLY for proximity to parents). let's leave as this detailed 3 category now, but might revisit later (like same state y/n maybe?)
label values lives_near_fam_man* lives_near_fam_woman* lives_family
tab lives_near_fam_man1
tab lives_near_fam_man2
tab lives_near_fam_man1 lives_near_fam_man2
tab lives_near_fam_woman1
tab lives_near_fam_man1 lives_near_fam_woman1

// 65+ in HH - should be same across partners?
// again, might time 1 and 2 be diff?
tab num_65up_hh1 num_65up_hh_sp1, m
tab num_65up_hh1 num_65up_hh_sp1 if _mi_m!=0, m
tab num_65up_hh2 num_65up_hh_sp2 if _mi_m!=0, m

tab num_65up_hh1 num_65up_hh2, m

	// could take an average?
	browse unique_id num_65up_hh*
	egen num_65up_hh_avg = rowmean(num_65up_hh1 num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10) // okay - one thing, does this need to be an average ONLY while they are in relationship? gah-  so need to account for relationship LENGTH.

// self-rated health
label values sr_health_man* sr_health_woman* health
browse unique_id sr_health_man* // does this tend to stay consistent or change (e.g. should I use at time 1?)
browse unique_id sr_health_woman* // does this tend to stay consistent or change (e.g. should I use at time 1?)
	// two things: a. these change quite a bit. and b. right now: excellent = 1 and poor = 5. if I want a mean, I feel like it should be the other way? (so higher = better?)
	
label define health_rev 1 "Poor" 2 "Fair" 3 "Good" 4 "Very Good" 5 "Excellent"
	// let's recode first, then create average of man and woman (separately) - then decide how those compare to time1
	forvalues d=1/10{
		foreach var in sr_health_man sr_health_woman{
			recode `var'`d' (1=5)(2=4)(3=3)(4=2)(5=1), gen(`var'_rev`d')
			label values `var'_rev`d' health_rev
		}
	}

tab sr_health_man1 sr_health_man_rev1

egen sr_health_avg3_man = rowmean(sr_health_man_rev1 sr_health_man_rev2 sr_health_man_rev3) // for now, let's restrict to the 3 years minimum so comparable for all? okay this is become probably less apealling as metric?
egen sr_health_avg10_man = rowmean(sr_health_man_rev1 sr_health_man_rev2 sr_health_man_rev3 sr_health_man_rev4 sr_health_man_rev5 sr_health_man_rev6 sr_health_man_rev7 sr_health_man_rev8 sr_health_man_rev9 sr_health_man_rev10)

tabstat sr_health_man_rev1 sr_health_man_rev10 sr_health_avg3_man sr_health_avg10_man // so seems like health declines, because 1 and 10 different, and then average for 10 more different. because this is sketchy anyway, let's just use at time 1

// update and save
mi update
// mi register regular education_man education_woman couple_educ_type raceth_man raceth_woman birth_yr_man birth_yr_woman bcohort_man bcohort_woman age_man1 age_woman1 // do I need to register?

save "$created_data/PSID_truncated_clusters_analysis.dta", replace

********************************************************************************
**# Within cluster descriptives
********************************************************************************
/* Just doing all of this below now
*Descriptive table for entire sample
desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1, filename("$tables/desc_sample_all") stats(mimean) 

mi estimate: proportion couple_educ_type raceth_woman // validate that this matches
mi estimate: mean couple_earnings_t1

mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==1
mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==4
mi estimate, esampvaryok: mean couple_earnings_t1  if mc5_factor==1
tabstat couple_earnings_t1, by(mc5_factor)

// double check the right way to do this: non-imputed
tab couple_educ_type, gen(couple_educ)
mi estimate: proportion couple_educ_type
mi estimate: mean couple_educ1 couple_educ2 couple_educ3 couple_educ4

drop couple_educ1 couple_educ2 couple_educ3 couple_educ4

forvalues e=1/4{
	capture gen couple_educ`e' = couple_educ_type==`e'
}
mi estimate: mean couple_educ1 couple_educ2 couple_educ3 couple_educ4

drop couple_educ1 couple_educ2 couple_educ3 couple_educ4

// double check the right way to do this: imputed
tab religion_gp_woman1, gen(relig_woman)
mi estimate: proportion religion_gp_woman1
mi estimate: mean relig_woman1 relig_woman2 relig_woman3 relig_woman4 relig_woman5

/*
--------------------------------------------------------------
             |       Mean   Std. err.     [95% conf. interval]
-------------+------------------------------------------------
relig_woman1 |   .1849733   .0072445      .1700955    .1998512
relig_woman2 |   .2149413   .0061749      .2028242    .2270584
relig_woman3 |   .4292423   .0077872      .4138872    .4445973
relig_woman4 |   .1457844   .0059349      .1339325    .1576363
relig_woman5 |   .0250587   .0028608       .019217    .0309004
--------------------------------------------------------------
*/

drop relig_woman1 relig_woman2 relig_woman3 relig_woman4 relig_woman5

forvalues r=0/4{
	mi passive: gen relig_woman`r' = religion_gp_woman1==`r'
}

mi estimate: mean relig_woman0 relig_woman1 relig_woman2 relig_woman3 relig_woman4 // wait okay, so these do match? okay so I don't need to do this a complicated way? this makes things so much easier...

/*

             |       Mean   Std. err.     [95% conf. interval]
-------------+------------------------------------------------
relig_woman0 |   .1849733   .0072445      .1700955    .1998512
relig_woman1 |   .2149413   .0061749      .2028242    .2270584
relig_woman2 |   .4292423   .0077872      .4138872    .4445973
relig_woman3 |   .1457844   .0059349      .1339325    .1576363
relig_woman4 |   .0250587   .0028608       .019217    .0309004
--------------------------------------------------------------
*/

drop relig_woman0 relig_woman1 relig_woman2 relig_woman3 relig_woman4
*/

// going to try creating the variables here, because I think I can just do the basic tab, per above
tab couple_educ_type, gen(couple_educ)
// tab housing_woman1, gen(house_status)
tab raceth_man, gen(race_man)
tab raceth_woman, gen(race_woman)
tab region_gp_woman1, gen(region_gp)
tab rel_cohort, gen(rel_cohort)
tab bcohort_man, gen(bc_man)
tab bcohort_woman, gen(bc_woman)
tab religion_gp_man1, gen(relig_man)
tab religion_gp_woman1, gen(relig_woman)
tab lives_near_fam_man1, gen(lives_man)
tab lives_near_fam_woman1, gen(lives_woman)

*Descriptive table by cluster
putexcel set "$tables/stata_descriptives_by_cluster.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "(MC1, C3) egal cohab to 2nd shift marriage"
putexcel D1 = "(MC2, C1) stable second shift"
putexcel E1 = "(MC3, C5) traditionalization among direct marriers"
putexcel F1 = "(MC4, C2) stable trad married couples w children"
putexcel G1 = "(MC5, C4) egal marriage to 2nd shift marriage"

putexcel A2 = "Couple Educ: Neither College"
putexcel A3 = "Couple Educ: Her College"
putexcel A4 = "Couple Educ: Him College"
putexcel A5 = "Couple Educ: Both College"
putexcel A6 = "Couple Earnings (T1)"
putexcel A7 = "Homeowner T1 (YN)"


putexcel A8 = "Race Man: NH White"
putexcel A9 = "Race Man: Black"
putexcel A10 = "Race Man: Hispanic"
putexcel A11 = "Race Man: NH Asian"
putexcel A12 = "Race Man: NH Other"
putexcel A13 = "Race Woman: NH White"
putexcel A14 = "Race Woman: Black"
putexcel A15 = "Race Woman: Hispanic"
putexcel A16 = "Race Woman: NH Asian"
putexcel A17 = "Race Woman: NH Other"
putexcel A18 = "In Immigrant Sample Man"
putexcel A19 = "In Immigrant Sample Woman"
putexcel A20 = "Region: Northeast"
putexcel A21 = "Region: North Central"
putexcel A22 = "Region: South"
putexcel A23 = "Region: West"
// putexcel A24 = "Region: Other"
putexcel A24 = "Age Man (T1)"
putexcel A25 = "Age Woman (T1)"
putexcel A26 = "Relationship Number Man"
putexcel A27 = "Relationship Number Woman"
putexcel A28 = "In First Rel YN: Man"
putexcel A29 = "In First Rel YN: Woman"
putexcel A30 = "First Birth Timing Man"
putexcel A31 = "First Birth Timing Woman"
putexcel A32 = "First Birth Pre Rel YN: Man"
putexcel A33 = "First Birth Pre Rel YN: Woman"
putexcel A34 = "Age Youngest Child in HH"
putexcel A35 = "Ever Parent Man"
putexcel A36 = "Ever Parent Woman"
putexcel A37 = "MPF Y/N Man"
putexcel A38 = "MPF Y/N Woman"
putexcel A39 = "Man's Father has College Degree"
putexcel A40 = "Man's Mother has College Degree"
putexcel A41 = "Woman's Father has College Degree"
putexcel A42 = "Woman's Mother has College Degree"
putexcel A43 = "Man Lived with Bio Parents"
putexcel A44 = "Woman Lived with Bio Parents"
putexcel A45 = "Rel Cohort: 1990-1999"
putexcel A46 = "Rel Cohort: 2000-2009"
putexcel A47 = "Rel Cohort: 2010+"
putexcel A48 = "Birth Cohort Man: Pre 1960s"
putexcel A49 = "Birth Cohort Man: 1960s"
putexcel A50 = "Birth Cohort Man: 1970s"
putexcel A51 = "Birth Cohort Man: 1980s"
putexcel A52 = "Birth Cohort Man: 1990s+"
putexcel A53 = "Birth Cohort Woman: Pre 1960s"
putexcel A54 = "Birth Cohort Woman: 1960s"
putexcel A55 = "Birth Cohort Woman: 1970s"
putexcel A56 = "Birth Cohort Woman: 1980s"
putexcel A57 = "Birth Cohort Woman: 1990s+"
putexcel A58 = "Religion Man: None"
putexcel A59 = "Religion Man: Catholic"
putexcel A60 = "Religion Man: Protestant"
putexcel A61 = "Religion Man: Other Christian"
putexcel A62 = "Religion Man: Other Non-Christian"
putexcel A63 = "Religion Woman: None"
putexcel A64 = "Religion Woman: Catholic"
putexcel A65 = "Religion Woman: Protestant"
putexcel A66 = "Religion Woman: Other Christian"
putexcel A67 = "Religion Woman: Other Non-Christian"
putexcel A68 = "Disabled Man Y/N"
putexcel A69 = "Disabled Woman Y/N"
putexcel A70 = "Man's Parents in HH (# Years)"
putexcel A71 = "Woman's Parents in HH (# Years)"
putexcel A72 = "Max # Years Any Parent in HH"
putexcel A73 = "Where Live Man: Same State"
putexcel A74 = "Where Live Man: Same Region"
putexcel A75 = "Where Live Man: Diff Region"
putexcel A76 = "Where Live Woman: Same State"
putexcel A77 = "Where Live Woman: Same Region"
putexcel A78 = "Where Live Woman: Diff Region"
putexcel A79 = "Number People 65+ in HH (average)"
putexcel A80 = "Self-rated health Man (Avg T1)"
putexcel A81 = "Self-rated health Woman (Avg T1)"

local desc_vars "couple_educ1 couple_educ2 couple_educ3 couple_educ4 couple_earnings_t1 housing_woman1 race_man1 race_man2 race_man3 race_man4 race_man5 race_woman1 race_woman2 race_woman3 race_woman4 race_woman5 imm_sample_man imm_sample_woman region_gp1 region_gp2 region_gp3 region_gp4 age_man1 age_woman1 rel_no_man rel_no_woman in_first_rel_man in_first_rel_woman first_birth_timing_man first_birth_timing_woman first_birth_pre_rel_man first_birth_pre_rel_woman age_youngest_woman1 ever_parent_man ever_parent_woman mpf_man mpf_woman father_college_man mother_college_man father_college_woman mother_college_woman family_structure_man family_structure_woman rel_cohort1 rel_cohort2 rel_cohort3 bc_man1 bc_man2 bc_man3 bc_man4 bc_man5 bc_woman1 bc_woman2 bc_woman3 bc_woman4 bc_woman5 relig_man1 relig_man2 relig_man3 relig_man4 relig_man5 relig_woman1 relig_woman2 relig_woman3 relig_woman4 relig_woman5 disabled_man1 disabled_woman1 years_parent_in_hh_man years_parent_in_hh_woman max_years_parent_in_hh lives_man1 lives_man2 lives_man3 lives_woman1 lives_woman2 lives_woman3 num_65up_hh_avg sr_health_man_rev1 sr_health_woman_rev1" // region_gp5 - this is "other" and it's too small in some cluster + imputation combos

// full sample
forvalues w=1/80{
	local row=`w'+1
	local var: word `w' of `desc_vars'
	mi estimate: mean `var'	
	matrix t`var'= e(b_mi)
	local t`var' = t`var'[1,1]
	putexcel B`row' = `t`var'', nformat(#.#%)
}


// by cluster
local col1 "C D E F G"

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues w=1/80{
		local row=`w'+1
		local var: word `w' of `desc_vars'
		mi estimate, esampvaryok: mean `var' if mc5_factor==`c'
		matrix c`var'= e(b_mi)
		local c`var' = c`var'[1,1]
		putexcel `col'`row' = `c`var'', nformat(#.#%)
	}
}

// small checks
mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==1
mi estimate, esampvaryok: proportion couple_educ_type raceth_woman if mc5_factor==5
mi estimate, esampvaryok: mean couple_earnings_t1 sr_health_man_rev1 father_college_woman if mc5_factor==1
mi estimate, esampvaryok: mean couple_earnings_t1 sr_health_man_rev1 father_college_woman if mc5_factor==5
tabstat couple_earnings_t1, by(mc5_factor)
tabstat sequence_length, by(mc5_factor)

// tabstat in_first_rel_man in_first_rel_woman first_birth_pre_rel_man first_birth_pre_rel_woman // added these later (11/7 - afer adding for UK / DE)
// tabstat in_first_rel_man in_first_rel_woman first_birth_pre_rel_man first_birth_pre_rel_woman, by(mc5_factor)

// stopped here 9/5 (above is done - updated the variables so need to fix the reshape below)
// okay back 10/20/25 - actually very few variables to add, because most of the created were fixed. so just truncated (x3) + sr health vars
// per below - decide if want to create a fixed version of any at dur 1 to use (some of the created variables this happens automatically because of how I created - like age at time 1)? though i suppose i could also do that below just restricting to dur 1 instead of creating fixed variables?

********************************************************************************
**# Describe sequences
********************************************************************************
**Not yet repulled November 2026**

// I think the data need to be LONG here (to get average across durations)
use "$created_data/PSID_truncated_clusters_analysis.dta", clear

// replace mc5_factor=0 if mc5_factor==.

/*
should I get some t1 variables to use as fixed when I reshape?? oh maybe this happened automatically? I think this only happens automatically if there is JUST a dur 1 version, but if it exists at all durations, will get reshaped? I guess, unless I don't include in reshape, but then that won't work for diff reasons...
// religion_gp_man1 religion_gp_woman1 couple_earnings_t1
// unique unique_id partner_unique_id 
// unique unique_id partner_unique_id couple_earnings_t1
// unique unique_id partner_unique_id couple_earnings_t1 if _mi_m!=0
// browse unique_id partner_unique_id duration couple_earnings_t1 age_man1 _mi_m // right - if imputed, will match across durations, but not nec. across imputations
*/

mi reshape long ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end rel_type couple_num_children_gp_end family_type_end in_sample hh_status relationship housework_focal age_focal weekly_hrs_t_focal earnings_t_focal family_income_t partnered_imp educ_focal_imp num_children_imp_hh weekly_hrs_woman weekly_hrs_man housework_woman housework_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man  in_sample_sp hh_status_sp relationship_sp housework_focal_sp age_focal_sp weekly_hrs_t_focal_sp earnings_t_focal_sp family_income_t_sp partnered_imp_sp  marst_imp marst_imp_sp  marst_woman marst_man num_children_imp_hh_sp couple_work_ow_detailed couple_work_ow couple_hw_hrs_combo age_young_child RESPONDENT_WHO_ REGION_ employment_status_focal religion_focal lives_family_focal disabled_focal disabled_scale_focal sr_health_focal yr_retired_focal father_in_hh mother_in_hh num_parent_in_hh num_65up_hh rolling_births current_parent_status retired_est_focal house_status_all disabled_imp_focal age_young_child_sp RESPONDENT_WHO_sp REGION_sp employment_status_focal_sp religion_focal_sp lives_family_focal_sp disabled_focal_sp sr_health_focal_sp num_parent_in_hh_sp num_65up_hh_sp rolling_births_sp current_parent_status_sp retired_est_focal_sp house_status_all_sp employment_status_woman employment_status_man annual_earnings_woman annual_earnings_man age_youngest_woman age_youngest_man is_parent_woman is_parent_man region_woman region_man housing_woman housing_man home_owner home_owner_sp  religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man lives_near_fam_woman lives_near_fam_man couple_work couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children couple_num_children_gp family_type ft_pt_det_woman_end ft_pt_det_man_end couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc sr_health_man_rev sr_health_woman_rev is_respondent_focal is_respondent_focal_sp RELATION_ any_parent_in_hh any_parent_in_hh_sp relationship_type_ is_first_yr_cohabitor has_first_yr_cohabitor current_rel_type marital_status_focal in_rel_flag survey_yr dual_work_end division_of_labor_end egalitarian_end egal_dol_yn_end dual_ft_end hw_egal_end hw_mod_egal_end couple_hw_hrs_4cat couple_hw_hrs_4cat_end family_type_5cat family_type_5cat_end  couple_work_3cat_end couple_hw_3cat_end detailed_dol_end couple_work_trunc couple_hw_hrs_4cat_trunc family_type_5cat_trunc division_of_labor_trunc egalitarian_trunc egal_dol_yn_trunc dual_work_trunc dual_ft_trunc hw_egal_trunc hw_mod_egal_trunc couple_work_3cat_trunc couple_hw_3cat_trunc detailed_dol_trunc, i(unique_id partner_unique_id rel_start_all rel_end_all) j(duration) 

tab _mi_m mc5_factor, m

mi estimate: proportion family_type_5cat_trunc couple_hw_hrs_4cat_trunc couple_work_trunc if duration <=10
mi estimate: proportion couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc if duration <=10 // this is possibly where those current Excel summary stats came from duh (yes, this is the case - so let's confirm these match once I update the below; not yet updated for truncated clusters)

// the below actually didnt work because of the missing, so actually HAVE to do the tab way.
   // capture mi passive: gen work_seq`w' = couple_work_ow_trunc==`w'
   // mi estimate: mean work_seq`w'
tab couple_work_trunc, gen(work_seq)
tab couple_hw_hrs_4cat_trunc, gen(hw_seq)
tab family_type_5cat_trunc, gen(fam_seq)
mi estimate: mean work_seq1 work_seq2 if duration<=10

putexcel set "$tables/stata_cluster_composition.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "(MC1, C3) egal cohab to 2nd shift marriage"
putexcel D1 = "(MC2, C1) stable second shift"
putexcel E1 = "(MC3, C5) traditionalization among direct marriers"
putexcel F1 = "(MC4, C2) stable trad married couples w children"
putexcel G1 = "(MC5, C4) egal marriage to 2nd shift marriage"

putexcel A2 = "Work"
putexcel A3 = "Male BW"
putexcel A4 = "1.5 Male BW"
putexcel A5 = "Dual FT: any OW"
putexcel A6 = "Female BW"
putexcel A7 = "Underwork"

putexcel A9 = "Housework"
putexcel A10 = "Woman Most: High"
putexcel A11 = "Woman Most: Low"
putexcel A12 = "Equal: All"
putexcel A13 = "Man Most: All"

putexcel A15 = "Family"
putexcel A16 = "Married, 0 Ch"
putexcel A17 = "Married, 1 Ch"
putexcel A18 = "Married, 2+ Ch"
putexcel A19 = "Cohab, 0 Ch"
putexcel A20 = "Cohab, 1+ Ch"

// full sample
forvalues w=1/5{
   // capture mi passive: gen work_seq`w' = couple_work_ow_trunc==`w'
   mi estimate: mean work_seq`w' if duration<=10
   matrix w`w' = e(b_mi)
   local w`w' = w`w'[1,1]
   local row = 2+`w'
   putexcel B`row' = `w`w'', nformat(##.#%)
}

forvalues h=1/4{
   // capture mi passive: gen hw_seq`h' = couple_hw_hrs_combo_trunc==`h'
   mi estimate: mean hw_seq`h' if duration<=10
   matrix h`h' = e(b_mi)
   local h`h' = h`h'[1,1]
   local row = 9+`h'
   putexcel B`row' = `h`h'', nformat(##.#%)
}

forvalues f=1/5{
   // capture mi passive: gen fam_seq`f' = family_type_trunc==`f'
   mi estimate: mean fam_seq`f' if duration<=10
   matrix f`f' = e(b_mi)
   local f`f' = f`f'[1,1]
   local row = 15+`f'
   putexcel B`row' = `f`f'', nformat(##.#%)
}

// by cluster
local col1 "C D E F G"

// by cluster
forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues w=1/5{
	   mi estimate, esampvaryok: mean work_seq`w' if mc5_factor==`c' & duration<=10
	   matrix w`w' = e(b_mi)
	   local w`w' = w`w'[1,1]
	   local row = 2+`w'
	   putexcel `col'`row' = `w`w'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues h=1/4{
	   mi estimate, esampvaryok: mean hw_seq`h' if mc5_factor==`c'  & duration<=10
	   matrix h`h' = e(b_mi)
	   local h`h' = h`h'[1,1]
	   local row = 9+`h'
	   putexcel `col'`row' = `h`h'', nformat(##.#%)
	}
}

forvalues c=1/5{
	local col: word `c' of `col1'
	forvalues f=1/5{
	   mi estimate, esampvaryok: mean fam_seq`f' if mc5_factor==`c'  & duration<=10
	   matrix f`f' = e(b_mi)
	   local f`f' = f`f'[1,1]
	   local row = 15+`f'
	   putexcel `col'`row' = `f`f'', nformat(##.#%)
	}
}

// check
mi estimate, esampvaryok: proportion couple_work_trunc couple_hw_hrs_4cat_trunc family_type_5cat_trunc if duration <=10 & mc5_factor==2
mi estimate, esampvaryok: proportion couple_work_trunc couple_hw_hrs_4cat_trunc family_type_5cat_trunc if duration <=10 & mc5_factor==5