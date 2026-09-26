
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
use "$temp/UKHLS_clusters_truncated_sequences.dta", clear

// egen couple_id = group(pidp eligible_partner)
// browse couple_id pidp eligible_partner _mi_id // okay these don't match (made couple id earlier in the process)
unique couple_id _mi_id
unique pidp eligible_partner

gen mim = _mi_m

keep couple_id pidp eligible_partner mim mc5_factor mc6_factor mc7_factor mc8_factor

save "$created_data/UKHLS_truncated_clusters.dta", replace

********************************************************************************
* Now, merge clusters onto the original Stata imputed data
********************************************************************************
use "$created_data/ukhls_couples_wide_truncated.dta", clear
mi set m -= (6,7,8,9,10) // only did first 5 imputations for now
keep if sequence_length>=3 // also only keep relationships longer than 3 years now (for truncated sequences)

mi update

gen mim = _mi_m

// mi merge 1:1 couple_id mim using "$created_data/PSID_clusters.dta", gen(howmatch) // keep(match) 
merge 1:1 couple_id mim using "$created_data/UKHLS_truncated_clusters.dta"
drop if _merge==2

tab _mi_m _merge, m // confirm that this is mi 0 - it is
drop mim
drop _merge

mi update

********************************************************************************
* Figure out the data structure
********************************************************************************
browse pidp eligible_partner _mi_m mc6_factor mc7_factor mc8_factor

tab mc8_factor, m

// for now, very crude labels
capture label define mc8_factor 1 "cf cohab + 2nd shift" 2 "trans to neotrad" 3 "ow + CF cohab" 4 "fam diversity + trad work" ///
5 "cf cohab + counter-trad hw" 6 "neotrad marriage" 7 "underwork + over-HW" 8 "neotrad cohab"

label values mc8_factor mc8_factor

********************************************************************************
* Any variables still need to be created
********************************************************************************
**Current couple-SES
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

// what about home ownership status? in theory should match across partners, esp at time1?
label values housing_woman* housing_man* a_tenure_dv
tab housing_woman1 housing_man1, m // even here close to 100%, but not perfect
tab housing_woman1 housing_man1 if _mi_m==0, m // okay yes nearly 100%. but we'll use the woman's

mi passive: gen housing_gp_woman1=.
mi passive: replace housing_gp_woman1 = 1 if housing_woman1== 1 // owned outright
mi passive: replace housing_gp_woman1 = 2 if housing_woman1== 2 // owned w mortgage
mi passive: replace housing_gp_woman1 = 3 if inrange(housing_woman1,3,7) // rented
mi passive: replace housing_gp_woman1 = 4 if housing_woman1== 8 // other

label define housing_gp_woman 1 "owned outright" 2 "owned w mortgage" 3 "rented" 4 "other"
label values housing_gp_woman1 housing_gp_woman
tab housing_woman1 housing_gp_woman1, m

// gendered race variables
tab xw_ethn_dv _mi_m , m

gen ethn_man=xw_ethn_dv if SEX==1
replace ethn_man=xw_ethn_dv_sp if SEX==2

gen ethn_woman=xw_ethn_dv if SEX==2
replace ethn_woman=xw_ethn_dv_sp if SEX==1

label values ethn_woman ethn_man ethn_dv

gen race_man = .
replace race_man = 0 if inrange(ethn_man,5,97) 
replace race_man = 1 if inrange(ethn_man,1,4) 

gen race_woman = .
replace race_woman = 0 if inrange(ethn_woman,5,97) 
replace race_woman = 1 if inrange(ethn_woman,1,4) 

gen same_race=.
replace same_race=0 if race_man!=race_woman
replace same_race=1 if race_man==race_woman

// let's use sample type to create if in immigrant sample as well (these are fixed also)
gen sample_type_man=xw_memorig if SEX==1
replace sample_type_man=xw_memorig_sp if SEX==2

gen sample_type_woman=xw_memorig if SEX==2
replace sample_type_woman=xw_memorig_sp if SEX==1

label values sample_type_woman sample_type_man  memorig // https://iserredex.essex.ac.uk/support/issues/475 (the ECHP aren't well-documented; wanted to confirm they are not immigrants - they are not)

gen imm_sample_man = .
replace imm_sample_man = 0 if inrange(sample_type_man,1,6)
replace imm_sample_man = 0 if inrange(sample_type_man,14,16)
replace imm_sample_man = 1 if inlist(sample_type_man,7,8)

gen imm_sample_woman = .
replace imm_sample_woman = 0 if inrange(sample_type_woman,1,6)
replace imm_sample_woman = 0 if inrange(sample_type_woman,14,16)
replace imm_sample_woman = 1 if inlist(sample_type_woman,7,8)

tab sample_type_woman imm_sample_woman

// country of residence
label values region_woman* region_man* a_gor_dv
tab region_woman1 region_man1,  m // even here close to 100%, but not perfect
tab region_woman1 region_man1 if _mi_m==0, m // okay yes nearly 100%. but we'll use the woman's
tab gor_dv1 country_all1, m // wait which of these did I impute?? the gor_dv, so yeah, need to recode

mi passive: gen country_woman1 = .
mi passive: replace country_woman1 = 1 if inrange(region_woman1,1,9)
mi passive: replace country_woman1 = 2 if region_woman1==10
mi passive: replace country_woman1 = 3 if region_woman1==11
mi passive: replace country_woman1 = 4 if region_woman1==12

label values country_woman1 country
tab region_woman1 country_woman1, m

* Relationship info
// age at rel start
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

// relationship order - created in step f: rel_no_woman rel_no_man
// okay actually want to create a binary indicator of whether in first rel
tab rel_no_woman, m
tab rel_no_man, m

gen in_first_rel_woman = 0
replace in_first_rel_woman = 1 if rel_no_woman==1

gen in_first_rel_man = 0
replace in_first_rel_man = 1 if rel_no_man==1

// first birth timing relative to relationship start
browse pidp eligible_partner SEX eligible_rel_start_year yr_first_birth_woman yr_first_birth_man birth_timing_rel birth_timing_rel_sp _mi_m // I imputed first birth, so I think I need to recalculate first birth timing? 
	// also, see notes from PSID, but I had done before as relationship - first birth and I want the opposite (first birth - relationship)

mi passive: gen first_birth_timing_man = yr_first_birth_man - eligible_rel_start_year
mi passive: replace first_birth_timing_man = . if yr_first_birth_man==9999

mi passive: gen first_birth_timing_woman = yr_first_birth_woman - eligible_rel_start_year
mi passive: replace first_birth_timing_woman = . if yr_first_birth_woman==9999 // need to account for this better, but don't want the 9999 skewing.

// browse pidp eligible_partner SEX eligible_rel_start_year yr_first_birth_woman yr_first_birth_man birth_timing_rel birth_timing_rel_sp _mi_m first_birth_timing_woman first_birth_timing_man

// also create a binary of pre / post? I think so yeah
mi passive: gen first_birth_pre_rel_man = .
mi passive: replace first_birth_pre_rel_man = 0 if first_birth_timing_man >=0 & first_birth_timing_man!=.
mi passive: replace first_birth_pre_rel_man = 0 if yr_first_birth_man==9999 // can actually put 9999 here because is, theoretically, 0 if no births
mi passive: replace first_birth_pre_rel_man = 1 if first_birth_timing_man <0 & first_birth_timing_man!=.

tab first_birth_timing_man first_birth_pre_rel_man, m
tab yr_first_birth_man first_birth_pre_rel_man, m

mi passive: gen first_birth_pre_rel_woman = .
mi passive: replace first_birth_pre_rel_woman = 0 if first_birth_timing_woman >=0 & first_birth_timing_woman!=.
mi passive: replace first_birth_pre_rel_woman = 0 if yr_first_birth_woman==9999 // can actually put 9999 here because is, theoretically, 0 if no births
mi passive: replace first_birth_pre_rel_woman = 1 if first_birth_timing_woman <0 & first_birth_timing_woman!=.

tab first_birth_timing_woman first_birth_pre_rel_woman, m
tab yr_first_birth_woman first_birth_pre_rel_woman, m

// age of youngest child - this is just in the HH, prob useful descriptively but we don't 100% know with this variable if it's their kid together
mi passive: gen age_youngest_woman1=age_youngest_child1 if SEX==2
mi passive: replace age_youngest_woman1=age_youngest_child_sp1 if SEX==1

mi passive: gen age_youngest_man1=age_youngest_child1 if SEX==1
mi passive: replace age_youngest_man1=age_youngest_child_sp1 if SEX==2

tab age_youngest_woman1 age_youngest_man1, m
tab age_youngest_child1 age_youngest_child_sp1, m
tab age_youngest_woman1 age_youngest_man1 if _mi_m!=0, m // okay these are less congruent, but I think following what I do for # of kids, use the woman's

// ever parent status
mi passive: gen ever_parent_woman=ever_parent if SEX==2
mi passive: replace ever_parent_woman=ever_parent_sp if SEX==1

mi passive: gen ever_parent_man=ever_parent if SEX==1
mi passive: replace ever_parent_man=ever_parent_sp if SEX==2

// mpf status - lol I lost this variable somewhere along the way....
	// because of the way I created, I could merge on? is this risky lol. let's do this for now at least (10/20/2025)
	// tmp save before we do this (code to save is below)
	
	mi merge m:1 pidp using "$temp/mpf_lookup.dta", keep(master match) // see file c (created in file b)
	drop num_bio_kids
	tab SEX
	rename any_mpf mpf_woman // since I only kept women, this work
	tab mpf_woman, m
	
	mi merge m:1 eligible_partner using "$temp/mpf_lookup.dta", keep(master match) // gen(howmatch)
	drop num_bio_kids
	rename any_mpf mpf_man
	tab mpf_man, m
	
tab ever_parent_woman mpf_woman, m // okay so the 0s match, which is a relief...
tab ever_parent_man mpf_man, m
replace mpf_man = 0 if ever_parent_man==0 & mpf_man==. // if ever parent == 0 , can't have mpf

tab mpf_woman mpf_man, m
// let's tmp save again because that was a lot

* Family background
// created many of these in step f, but want to make sure cleaned and such. also I believe I had to impute some of these...
label values father_educ_woman father_educ_man paedqf 
label values mother_educ_woman mother_educ_man maedqf 

tab father_educ_woman, m // father_educ
tab mother_educ_woman, m // mother_educ
tab family_structure_woman, m // family_structure. I think this is just two-parent y/n
tab family_structure14_det family_structure_woman

// struggling with how to classify the above, so let's just do university v not (think this is what i end up using for PSID anyway). think difficult to compare across countries otherwise
mi passive: gen father_college_woman = .
mi passive: replace father_college_woman = 0 if inrange(father_educ_woman,1,4)
mi passive: replace father_college_woman = 0 if father_educ_woman==97
mi passive: replace father_college_woman = 1 if father_educ_woman==5

mi passive: gen father_college_man = .
mi passive: replace father_college_man = 0 if inrange(father_educ_man,1,4)
mi passive: replace father_college_man = 0 if father_educ_man==97
mi passive: replace father_college_man = 1 if father_educ_man==5

mi passive: gen mother_college_woman = .
mi passive: replace mother_college_woman = 0 if inrange(mother_educ_woman,1,4)
mi passive: replace mother_college_woman = 0 if mother_educ_woman==97
mi passive: replace mother_college_woman = 1 if mother_educ_woman==5

mi passive: gen mother_college_man = .
mi passive: replace mother_college_man = 0 if inrange(mother_educ_man,1,4)
mi passive: replace mother_college_man = 0 if mother_educ_man==97
mi passive: replace mother_college_man = 1 if mother_educ_man==5

* Social context
// make some sort of birth cohort? can also use age to describe within cluster (but categorical will be easier for between cluster)
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

// relationship start as well
gen rel_cohort=.
replace rel_cohort = 1 if eligible_rel_start_year >=1900 & eligible_rel_start_year<2000
replace rel_cohort = 2 if eligible_rel_start_year >=2000 & eligible_rel_start_year<2010
replace rel_cohort = 3 if eligible_rel_start_year >=2010 & eligible_rel_start_year<2025
tab rel_cohort, m

label define rel_cohort 1 "1990s" 2 "2000s" 3 "2010s"
label values rel_cohort rel_cohort

// tmp save

// religion
label values religion_woman* religion_man* master_religion
fre religion_woman1
tab religion_woman1 country_woman1

mi passive: gen religion_gp_woman1 = .
mi passive: replace religion_gp_woman1 = 0 if religion_woman1==1
mi passive: replace religion_gp_woman1 = 1 if religion_woman1==2
mi passive: replace religion_gp_woman1 = 2 if inlist(religion_woman1,4,5,17)
mi passive: replace religion_gp_woman1 = 3 if religion_woman1==3
mi passive: replace religion_gp_woman1 = 4 if inlist(religion_woman1,6,7,8,9,10,11,18,19,20)
mi passive: replace religion_gp_woman1 = 5 if inlist(religion_woman1,12,13,14,15,16,21)

tab religion_woman1 religion_gp_woman1, m

mi passive: gen religion_gp_man1 = .
mi passive: replace religion_gp_man1 = 0 if religion_man1==1
mi passive: replace religion_gp_man1 = 1 if religion_man1==2
mi passive: replace religion_gp_man1 = 2 if inlist(religion_man1,4,5,17)
mi passive: replace religion_gp_man1 = 3 if religion_man1==3
mi passive: replace religion_gp_man1 = 4 if inlist(religion_man1,6,7,8,9,10,11,18,19,20)
mi passive: replace religion_gp_man1 = 5 if inlist(religion_man1,12,13,14,15,16,21)

tab religion_man1 religion_gp_man1, m

capture label define religion_gp 0 "No Religion" 1 "Anglican" 2 "Presbyterian" 3 "Catholic" 4 "Other Christian" 5 "Other non-Christian"
label values religion_gp_man1 religion_gp_woman1 religion_gp

** Other caregiving obligations (potentially)
// disability status. okay these were created in step f and are fine
tab disabled_woman1, m
tab disabled_man1, m

// parents in HH
tab num_parents_hh1, m
tab num_parents_hh_sp1, m

tab num_parents_hh2, m
tab num_parents_hh_sp2, m

browse pidp eligible_partner sequence_length num_parents_hh1 num_parents_hh2 num_parents_hh3 num_parents_hh4 num_parents_hh5 num_parents_hh6 num_parents_hh7 num_parents_hh8 num_parents_hh9 num_parents_hh10

egen years_parent_in_hh = anycount(num_parents_hh1 num_parents_hh2 num_parents_hh3 num_parents_hh4 num_parents_hh5 num_parents_hh6 num_parents_hh7 num_parents_hh8 num_parents_hh9 num_parents_hh10), values(1 2)
	// should I add actually divide by sequence length??
	gen pct_parent_in_hh = years_parent_in_hh / sequence_length
	replace pct_parent_in_hh = 1 if pct_parent_in_hh>1 & pct_parent_in_hh!=.
// browse pidp eligible_partner sequence_length years_parent_in_hh pct_parent_in_hh num_parents_hh1 num_parents_hh2 num_parents_hh3 num_parents_hh4 num_parents_hh5 num_parents_hh6 num_parents_hh7 num_parents_hh8 num_parents_hh9 num_parents_hh10
mean years_parent_in_hh pct_parent_in_hh

egen years_parent_in_hh_sp = anycount(num_parents_hh_sp1 num_parents_hh_sp2 num_parents_hh_sp3 num_parents_hh_sp4 num_parents_hh_sp5 num_parents_hh_sp6 num_parents_hh_sp7 num_parents_hh_sp8 num_parents_hh_sp9 num_parents_hh_sp10), values(1 2)
	gen pct_parent_in_hh_sp = years_parent_in_hh_sp / sequence_length
	replace pct_parent_in_hh_sp = 1 if pct_parent_in_hh_sp>1 & pct_parent_in_hh_sp!=.
	
gen pct_parent_woman=pct_parent_in_hh if SEX==2
replace pct_parent_woman=pct_parent_in_hh_sp if SEX==1

gen pct_parent_man=pct_parent_in_hh if SEX==1
replace pct_parent_man=pct_parent_in_hh_sp if SEX==2

// people 65+ in HH
tab npens_dv1 npens_dv_sp1, m
tab npens_dv1 npens_dv_sp1 if _mi_m!=0, m

browse pidp eligible_partner sequence_length npens_dv1 npens_dv2 npens_dv3 npens_dv4 npens_dv5 npens_dv6 npens_dv7 npens_dv8 npens_dv9 npens_dv10
egen num_65up_hh_avg = rowmean(npens_dv1 npens_dv2 npens_dv3 npens_dv4 npens_dv5 npens_dv6 npens_dv7 npens_dv8 npens_dv9 npens_dv10) // don't love this, but this is what I did for PSID so leave for now... (basically averages presence across 10 years, so sort of a proportion out of 100, except if 2 people instead of 1, then can be over 1 since it's not y/n, it's a count)

// any aid provided / how much
tab any_aid1 if _mi_m!=0, m // okay i did not impute this. duh bc I imputed below. so think can code this as Y/N per below... so few people provide care that I think for now, YN works, but could possibly just pull the amount at time 0? just hard to figure out how to average this across durations because it's also not a true continuous; it's bucketed. So could use midpoint, but that feels flawed
tab any_aid_sp1 if _mi_m!=0, m
browse pidp eligible_partner sequence_length any_aid1 any_aid2 any_aid3 any_aid4 any_aid5 any_aid6 any_aid7 any_aid8 any_aid9 any_aid10
browse pidp eligible_partner sequence_length aid_hours1 aid_hours2 aid_hours3 aid_hours4 aid_hours5 aid_hours6 aid_hours7 aid_hours8 aid_hours9 aid_hours10

tab aid_hours1  if _mi_m!=0, m
tab aid_hours_sp1  if _mi_m!=0, m

forvalues d=1/11{
	mi passive: gen aid_yn_woman`d'=.
	mi passive: replace aid_yn_woman`d' = 0 if aid_hours`d'==0 // since I just kept women, these are all women
	mi passive: replace aid_yn_woman`d' = 1 if inrange(aid_hours`d',1,7)
	
	mi passive: gen aid_yn_man`d'=.
	mi passive: replace aid_yn_man`d' = 0 if aid_hours_sp`d'==0
	mi passive: replace aid_yn_man`d' = 1 if inrange(aid_hours_sp`d',1,7)
}

tab any_aid1  if _mi_m!=0, m
tab aid_yn_woman1  if _mi_m!=0, m
tab aid_hours1 aid_yn_woman1  if _mi_m!=0, m

browse pidp eligible_partner sequence_length aid_yn_woman1 any_aid1 aid_hours1 aid_yn_woman2 any_aid2 aid_hours2 aid_yn_woman3 any_aid3 aid_hours3 aid_yn_woman4 any_aid4 aid_hours4
browse pidp eligible_partner sequence_length aid_yn_woman1 aid_yn_woman2 aid_yn_woman3 aid_yn_woman4 aid_yn_woman5 aid_yn_woman6 aid_yn_woman7 aid_yn_woman8 aid_yn_woman9 aid_yn_woman10

egen years_aid_woman = anycount(aid_yn_woman1 aid_yn_woman2 aid_yn_woman3 aid_yn_woman4 aid_yn_woman5 aid_yn_woman6 aid_yn_woman7 aid_yn_woman8 aid_yn_woman9 aid_yn_woman10), values(1)
	// should I add actually divide by sequence length??
	gen pct_aid_woman = years_aid_woman / sequence_length
	replace pct_aid_woman = 1 if pct_aid_woman>1 & pct_aid_woman!=.
	
egen years_aid_man = anycount(aid_yn_man1 aid_yn_man2 aid_yn_man3 aid_yn_man4 aid_yn_man5 aid_yn_man6 aid_yn_man7 aid_yn_man8 aid_yn_man9 aid_yn_man10), values(1)
	// should I add actually divide by sequence length??
	gen pct_aid_man = years_aid_man / sequence_length
	replace pct_aid_man = 1 if pct_aid_man>1 & pct_aid_man!=.
	
// tabstat years_aid_woman pct_aid_woman years_aid_man pct_aid_man

// there are too many categories in aid hours, because not all have data in all clusters at the top, so let's recode.
tab aid_hours1
mi passive: gen aid_hours_gp1 = .
mi passive: replace aid_hours_gp1 = 0 if aid_hours1==0
mi passive: replace aid_hours_gp1 = 1 if inrange(aid_hours1,1,2) // < 10 hrs
mi passive: replace aid_hours_gp1 = 2 if inrange(aid_hours1,3,7) // > 10 hrs. is this even useful anymore??

mi passive: gen aid_hours_gp_sp1 = .
mi passive: replace aid_hours_gp_sp1 = 0 if aid_hours_sp1==0
mi passive: replace aid_hours_gp_sp1 = 1 if inrange(aid_hours_sp1,1,2) // < 10 hrs
mi passive: replace aid_hours_gp_sp1 = 2 if inrange(aid_hours_sp1,3,7) // > 10 hrs. is this even useful anymore??

label define aid_hours_gp 0 "None" 1 "< 10 hours" 2 "10+ hours"
label values aid_hours_gp1 aid_hours_gp_sp1 aid_hours_gp

// self-rated health
label values sr_health_woman* sr_health_man* health 
tab sr_health_woman1, m
tab sr_health_man1, m
browse pidp eligible_partner sr_health_woman*

// need to recode because I want higher to be better (that is how I recoded for PSID)
label define health_rev 1 "Very Poor" 2 "Poor" 3 "Fair" 4 "Good" 5 "Very Good" 6 "Excellent"

forvalues d=1/11{
	foreach var in sr_health_man sr_health_woman{
		recode `var'`d' (1=6)(2=5)(3=4)(4=3)(5=2)(6=1), gen(`var'_rev`d')
		label values `var'_rev`d' health_rev
	}
}

tab sr_health_woman1 sr_health_woman_rev1
tabstat sr_health_woman1 sr_health_woman_rev1 sr_health_man1 sr_health_man_rev1

**# update and save recodes
mi update

save "$created_data/UKHLS_truncated_clusters_analysis.dta", replace

********************************************************************************
**# Within cluster descriptives
********************************************************************************
mi xtset, clear // think these are frm downloaded data, but some commands not working
mi stset, clear

*Descriptive table for entire sample
// just doing as part of below
// desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman i.couple_earnings_quart1 c.couple_earnings_t1, filename("$tables/desc_sample_all") stats(mimean) 

tab couple_educ_type, gen(educ_type)
mi estimate: proportion couple_educ_type
mi estimate: mean educ_type1 educ_type2 educ_type3 educ_type4

// create variables needed from binary
// tab couple_educ_type, gen(educ_type) // did above to check
tab housing_gp_woman1, gen(house_status)
tab country_woman1, gen(country)
tab rel_cohort, gen(rel_cohort)
tab bcohort_man, gen(bc_man)
tab bcohort_woman, gen(bc_woman)
tab religion_gp_man1, gen(relig_man)
tab religion_gp_woman1, gen(relig_woman)
tab aid_hours_gp1, gen(aid_time)
tab aid_hours_gp_sp1, gen(aid_time_sp)

*Descriptive table by cluster
putexcel set "$tables/UKHLS_stata_descriptives_by_cluster.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: cf cohab + 2nd shift"
putexcel D1 = "2: trans to neotrad"
putexcel E1 = "3: ow + CF cohab"
putexcel F1 = "4: fam diversity + trad work"
putexcel G1 = "5: cf cohab + counter-trad hw"
putexcel H1 = "6: neotrad marriage"
putexcel I1 = "7: underwork + over-HW"
putexcel J1 = "8: neotrad cohab"

putexcel A2 = "Couple Educ: Neither College"
putexcel A3 = "Couple Educ: Her College"
putexcel A4 = "Couple Educ: Him College"
putexcel A5 = "Couple Educ: Both College"
putexcel A6 = "Couple Earnings (T1)"
putexcel A7 = "Housing Status T1: Owns Outright"
putexcel A8 = "Housing Status T1: Owns with Mortgage"
putexcel A9 = "Housing Status T1: Rents"
putexcel A10 = "Race Man: White Y/N"
putexcel A11 = "Race Woman: White Y/N"
putexcel A12 = "In Immigrant Sample Man"
putexcel A13 = "In Immigrant Sample Woman"
putexcel A14 = "Country: England"
putexcel A15 = "Country: Wales"
putexcel A16 = "Country: Scotland"
putexcel A17 = "Country: N. Ireland"
putexcel A18 = "Age Man (T1)"
putexcel A19 = "Age Woman (T1)"
putexcel A20 = "Relationship Number Man"
putexcel A21 = "Relationship Number Woman"
putexcel A22 = "In First Rel YN: Man"
putexcel A23 = "In First Rel YN: Woman"
putexcel A24 = "First Birth Timing Man"
putexcel A25 = "First Birth Timing Woman"
putexcel A26 = "First Birth Pre Rel YN: Man"
putexcel A27 = "First Birth Pre Rel YN: Woman"
putexcel A28 = "Age Youngest Child in HH"
putexcel A29 = "Ever Parent Man"
putexcel A30 = "Ever Parent Woman"
putexcel A31 = "MPF Y/N Man"
putexcel A32 = "MPF Y/N Woman"
putexcel A33 = "Man's Father has College Degree"
putexcel A34 = "Man's Mother has College Degree"
putexcel A35 = "Woman's Father has College Degree"
putexcel A36 = "Woman's Mother has College Degree"
putexcel A37 = "Man Lived with Bio Parents"
putexcel A38 = "Woman Lived with Bio Parents"
putexcel A39 = "Rel Cohort: 1990-1999"
putexcel A40 = "Rel Cohort: 2000-2009"
putexcel A41 = "Rel Cohort: 2010+"
putexcel A42 = "Birth Cohort Man: Pre 1960s"
putexcel A43 = "Birth Cohort Man: 1960s"
putexcel A44 = "Birth Cohort Man: 1970s"
putexcel A45 = "Birth Cohort Man: 1980s"
putexcel A46 = "Birth Cohort Woman: Pre 1960s"
putexcel A47 = "Birth Cohort Woman: 1960s"
putexcel A48 = "Birth Cohort Woman: 1970s"
putexcel A49 = "Birth Cohort Woman: 1980s"
putexcel A50 = "Religion Man: None"
putexcel A51 = "Religion Man: Anglican"
putexcel A52 = "Religion Man: Presbyterian"
putexcel A53 = "Religion Man: Catholic"
putexcel A54 = "Religion Man: Other Christian"
putexcel A55 = "Religion Man: Other Non-Christian"
putexcel A56 = "Religion Woman: None"
putexcel A57 = "Religion Woman: Anglican"
putexcel A58 = "Religion Woman: Presbyterian"
putexcel A59 = "Religion Woman: Catholic"
putexcel A60 = "Religion Woman: Other Christian"
putexcel A61 = "Religion Woman: Other Non-Christian"
putexcel A62 = "Disabled Man Y/N"
putexcel A63 = "Disabled Woman Y/N"
putexcel A64 = "Man's Parents in HH (% Years)"
putexcel A65 = "Woman's Parents in HH (% Years)"
putexcel A66 = "Number People 65+ in HH (average)"
putexcel A67 = "Man Provides Aid (% Years)"
putexcel A68 = "Woman Provides Aid (% Years)"
putexcel A69 = "How Much Care: Woman (T1): None"
putexcel A70 = "How Much Care: Woman (T1): < 10 Hrs"
putexcel A71 = "How Much Care: Woman (T1): 10+ Hrs"
putexcel A72 = "How Much Care: Man (T1): None"
putexcel A73 = "How Much Care: Man (T1): < 10 Hrs"
putexcel A74 = "How Much Care: Man (T1): 10+ Hrs"
putexcel A75 = "Self-rated health Man (Avg T1)"
putexcel A76 = "Self-rated health Woman (Avg T1)"

local desc_vars "educ_type1 educ_type2 educ_type3 educ_type4 couple_earnings_t1 house_status1 house_status2 house_status3 race_man race_woman imm_sample_man imm_sample_woman country1 country2 country3 country4 age_man1 age_woman1 rel_no_man rel_no_woman in_first_rel_man in_first_rel_woman first_birth_timing_man first_birth_timing_woman first_birth_pre_rel_man first_birth_pre_rel_woman age_youngest_woman1 ever_parent_man ever_parent_woman mpf_man mpf_woman father_college_man mother_college_man father_college_woman mother_college_woman family_structure_man family_structure_woman rel_cohort1 rel_cohort2 rel_cohort3 bc_man1 bc_man2 bc_man3 bc_man4 bc_woman1 bc_woman2 bc_woman3 bc_woman4 relig_man1 relig_man2 relig_man3 relig_man4 relig_man5 relig_man6 relig_woman1 relig_woman2 relig_woman3 relig_woman4 relig_woman5 relig_woman6 disabled_man1 disabled_woman1 pct_parent_man pct_parent_woman num_65up_hh_avg pct_aid_man  pct_aid_woman aid_time1 aid_time2 aid_time3 aid_time_sp1 aid_time_sp2 aid_time_sp3 sr_health_man_rev1 sr_health_woman_rev1"

// full sample
forvalues w=1/75{
	local row=`w'+1
	local var: word `w' of `desc_vars'
	mi estimate, esampvaryok: mean `var' // bc of how I coded time between first birth and relationship (and bc first birth had to be imputed), these can vary across samples, so had to add this here
	matrix t`var'= e(b_mi)
	local t`var' = t`var'[1,1]
	putexcel B`row' = `t`var'', nformat(#.#%)
}

// by cluster
local col1 "C D E F G H I J"

forvalues c=1/8{
	local col: word `c' of `col1'
	forvalues w=1/75{
		local row=`w'+1
		local var: word `w' of `desc_vars'
		mi estimate, esampvaryok: mean `var' if mc8_factor==`c'
		matrix c`var'= e(b_mi)
		local c`var' = c`var'[1,1]
		putexcel `col'`row' = `c`var'', nformat(#.#%)
	}
}

//  validate that these match when done
mi estimate: proportion couple_educ_type country_woman1
mi estimate: proportion aid_hours_gp1 aid_hours_gp_sp1
mi estimate: mean couple_earnings_t1 educ_type1 sr_health_man_rev1 father_college_woman rel_no_woman

mi estimate, esampvaryok: proportion couple_educ_type country_woman1 if mc8_factor==1
mi estimate, esampvaryok: proportion couple_educ_type country_woman1 if mc8_factor==4
mi estimate, esampvaryok: mean couple_earnings_t1 educ_type1 sr_health_man_rev1 father_college_woman rel_no_woman if mc8_factor==1
mi estimate, esampvaryok: mean couple_earnings_t1 educ_type1 sr_health_man_rev1 father_college_woman rel_no_woman if mc8_factor==7
tabstat couple_earnings_t1, by(mc8_factor)

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
use "$created_data/UKHLS_truncated_clusters_analysis.dta", clear

mi reshape long age_all fihhmngrs_dv gor_dv nkids_dv jbstat aidhh aidxhh aidhrs howlng work_hours jbhrs fimnlabgrs_dv nchild_dv hiqual_dv country_all employed total_hours age_youngest_child partnered_imp marital_status_imp int_year orig_record age_all_sp fihhmngrs_dv_sp gor_dv_sp nkids_dv_sp jbstat_sp aidhrs_sp howlng_sp work_hours_sp jbhrs_sp fimnlabgrs_dv_sp employed_sp total_hours_sp age_youngest_child_sp partnered_imp_sp marital_status_imp_sp weekly_hrs_woman weekly_hrs_man housework_woman housework_man marital_status_woman marital_status_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man couple_work couple_work_ow couple_work_ow_detailed  couple_hw_total woman_hw_share hw_terc_woman hw_hilow_woman hw_hilow_man couple_hw hw_hilow_woman_gp1 hw_hilow_woman_gp2 hw_hilow_man_gp4 hw_hilow_woman_combo couple_hw_hrs couple_hw_hrs_alt couple_hw_hrs_combo rel_type couple_num_children couple_num_children_gp family_type ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end ft_pt_det_woman_end ft_pt_det_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end couple_num_children_gp_end family_type_end npens_dv tenure_dv jshrs employment_status disabled_est sr_health aid_hours num_parents_hh master_religion respondent_info npens_dv_sp tenure_dv_sp employment_status_sp disabled_est_sp sr_health_sp aid_hours_sp num_parents_hh_sp master_religion_sp respondent_info_sp employment_status_woman employment_status_man monthly_earnings_woman monthly_earnings_man carework_woman carework_man region_woman region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man any_aid any_aid_sp current_parent_status current_parent_status_sp couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc aid_yn_woman aid_yn_man sr_health_man_rev sr_health_woman_rev ///
, i(pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status) j(duration)

mi estimate: proportion couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc if duration <=10

tab _mi_m mc8_factor, m

tab couple_work_ow_trunc, gen(work_seq)
tab couple_hw_hrs_combo_trunc, gen(hw_seq)
tab family_type_trunc, gen(fam_seq)
mi estimate: mean work_seq1 work_seq2 if duration<=10
mi estimate: proportion couple_work_ow_trunc if duration<=10

putexcel set "$tables/UKHLS_stata_cluster_composition.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: cf cohab + 2nd shift"
putexcel D1 = "2: trans to neotrad"
putexcel E1 = "3: ow + CF cohab"
putexcel F1 = "4: fam diversity + trad work"
putexcel G1 = "5: cf cohab + counter-trad hw"
putexcel H1 = "6: neotrad marriage"
putexcel I1 = "7: underwork + over-HW"
putexcel J1 = "8: neotrad cohab"

putexcel A2 = "Work"
putexcel A3 = "Male BW"
putexcel A4 = "1.5 Male BW"
putexcel A5 = "Dual FT: no OW"
putexcel A6 = "Dual FT: any OW"
putexcel A7 = "Female BW"
putexcel A8 = "Underwork"

putexcel A10 = "Housework"
putexcel A11 = "Woman Most: High"
putexcel A12 = "Woman Most: Low"
putexcel A13 = "Equal: High"
putexcel A14 = "Equal: Low"
putexcel A15 = "Man Most: All"

putexcel A17 = "Family"
putexcel A18 = "Married, 0 Ch"
putexcel A19 = "Married, 1 Ch"
putexcel A20 = "Married, 2 Ch"
putexcel A21 = "Married, 3+ Ch"
putexcel A22 = "Cohab, 0 Ch"
putexcel A23 = "Cohab, 1 Ch"
putexcel A24 = "Cohab, 2 Ch"
putexcel A25 = "Cohab, 3+ Ch"

// full sample
forvalues w=1/6{
	// capture mi passive: gen work_seq`w' = work_seq==`w'
   mi estimate: mean work_seq`w' if duration<=10
   matrix w`w' = e(b_mi)
   local w`w' = w`w'[1,1]
   local row = 2+`w'
   putexcel B`row' = `w`w'', nformat(##.#%)
}

forvalues h=1/5{
   // capture mi passive: gen hw_seq`h' = couple_hw_hrs_combo_trunc==`h'
   mi estimate: mean hw_seq`h' if duration<=10
   matrix h`h' = e(b_mi)
   local h`h' = h`h'[1,1]
   local row = 10+`h'
   putexcel B`row' = `h`h'', nformat(##.#%)
}

forvalues f=1/8{
   // capture mi passive: gen fam_seq`f' = family_type_trunc==`f'
   mi estimate: mean fam_seq`f' if duration<=10
   matrix f`f' = e(b_mi)
   local f`f' = f`f'[1,1]
   local row = 17+`f'
   putexcel B`row' = `f`f'', nformat(##.#%)
}

// by cluster
local col1 "C D E F G H I J"

forvalues c=1/8{
	local col: word `c' of `col1'
	forvalues w=1/6{
	   mi estimate, esampvaryok: mean work_seq`w' if mc8_factor==`c' & duration<=10
	   matrix w`w' = e(b_mi)
	   local w`w' = w`w'[1,1]
	   local row = 2+`w'
	   putexcel `col'`row' = `w`w'', nformat(##.#%)
	}
}

forvalues c=1/8{
	local col: word `c' of `col1'
	forvalues h=1/5{
	   mi estimate, esampvaryok: mean hw_seq`h' if mc8_factor==`c'  & duration<=10
	   matrix h`h' = e(b_mi)
	   local h`h' = h`h'[1,1]
	   local row = 10+`h'
	   putexcel `col'`row' = `h`h'', nformat(##.#%)
	}
}

forvalues c=1/8{
	local col: word `c' of `col1'
	forvalues f=1/8{
	   capture mi estimate, esampvaryok: mean fam_seq`f' if mc8_factor==`c'  & duration<=10
	   matrix f`f' = e(b_mi)
	   local f`f' = f`f'[1,1]
	   local row = 17+`f'
	   putexcel `col'`row' = `f`f'', nformat(##.#%)
	}
}

// okay, this isn't working: mi estimate, esampvaryok: mean fam_seq7 if mc8_factor==2 & duration<=10 // in imputation 2, there are 0 people in cohab 2 children
// tab family_type_trunc mc8_factor if _mi_m==2
// will capture work? does that mess up the loop or will it keep going? does running these two lines of code together simulate? let's add a capture
capture mi estimate, esampvaryok: mean fam_seq7 if mc8_factor==2  & duration<=10
mi estimate, esampvaryok: mean fam_seq8 if mc8_factor==2  & duration<=10
// I think one problem is that it might just use the last stored matrix? so it might population with fam seq 6 instead of 7 (yes, this is exactly what happens...)? so can just check. or can fill in using the below code, i think it will work if I do as proportion, it is just not working this way because I am using the binary variables? so I think I can recover the info. just this isn't very replicable...

// check
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc if duration <=10 & mc8_factor==2 // okay this actually doesn't work either...so..I actually cannot fill it in? so, it just will be 0%. This is just descriptive anyway, it is fine.
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_combo_trunc if duration <=10 & mc8_factor==2
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc if duration <=10 & mc8_factor==5
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_combo_trunc family_type_trunc if duration <=10 & mc8_factor==7
