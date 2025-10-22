
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
use "$temp/GSOEP_clusters_truncated_sequences.dta", clear

drop couple_id
egen couple_id = group(pid eligible_partner)
// rename _mi_id couple_id
unique couple_id _mi_id
unique pid eligible_partner

gen mim = _mi_m

keep couple_id pid eligible_partner mim mc6_factor mc7_factor mc8_factor mc9_factor // old (non-truncated): mc_ward_det_4cl mc_ward_det_5cl mc4_factor mc5_factor

save "$created_data/GSOEP_truncated_clusters.dta", replace

********************************************************************************
* Now, merge clusters onto the original Stata imputed data
********************************************************************************
use "$created_data/gsoep_couples_wide_truncated.dta", clear

	// tab psample_pl
	// tab psample_pl born_germany_woman, m row
	// tab psample_pl born_germany_woman, m col
	

fre psample_pl // oh duh, I am using the file that I input into R, so these shouldn't be here anyway. 
fre sample_type

/* this happened already in the file I am using above
gen sample_type = .
replace sample_type = 1 if inrange(psample_pl,1,14)
replace sample_type = 1 if inrange(psample_pl,20,23)
replace sample_type = 2 if inlist(psample_pl,15,16,25,26)
replace sample_type = 3 if inlist(psample_pl,17,18,19,24)

label define sample_type 1 "core" 2 "migrant" 3 "refugee"
label values sample_type sample_type

tab sample_type sample_type_sp, m  // these should be perfectly matched (bc HH) but wanted to create both jic to follow step 7
*/
	
	// tab couple_work_ow1 sample_type, col

mi set m -= (6,7,8,9,10) // only did first 5 imputations for now
// unique pid eligible_partner if sequence_length>=3
keep if sequence_length>=3 // also only keep relationships longer than 3 years now (for truncated sequences)
drop if sample_type==3 // I am dumb - I am using the file without already

mi update

gen mim = _mi_m
drop couple_id
egen couple_id = group (pid eligible_partner)

merge 1:1 couple_id mim using "$created_data/GSOEP_truncated_clusters.dta"

tab _mi_m _merge, m // confirm that this is mi 0 - it is
drop if _merge==2

drop mim
drop _merge

mi update

********************************************************************************
* Figure out the data structure
********************************************************************************
browse unique_id partner_id _mi_m mc6_factor mc7_factor mc8_factor mc9_factor
// there are no sequence objects here. Are there supposed to be? Or we just care about cluster membership?

tab mc6_factor, m
tab mc7_factor, m
tab mc8_factor, m
tab mc9_factor, m

// for now, very crude labels
capture label define mc9_factor 1 "early trans to trad" 2 "egal cohab" 3 "neotrad cohab to marr" 4 "trad marriage" ///
5 "cf cohab + 2nd shift" 6 "late trans to neotrad" 7 "OW + CF cohab" 8 "trad cohab with kids" 9 "UW cohab"
label values mc9_factor mc9_factor

********************************************************************************
* Any variables still need to be created
********************************************************************************
**Current couple-SES
// couple education: this was fixed, not imputed, so don't need to use mi passive
// I created these in step 7
fre edu4_fixed_woman
fre edu4_fixed_man

gen couple_educ_type=.
replace couple_educ_type = 1 if inrange(edu4_fixed_man,1,3) & inrange(edu4_fixed_woman,1,3)
replace couple_educ_type = 2 if inrange(edu4_fixed_man,1,3) & edu4_fixed_woman==4
replace couple_educ_type = 3 if edu4_fixed_man==4 & inrange(edu4_fixed_woman,1,3)
replace couple_educ_type = 4 if edu4_fixed_man==4 & edu4_fixed_woman==4

capture label define couple_educ_type 1 "Neither College" 2 "Her College" 3 "Him College" 4 "Both College"
label values couple_educ_type couple_educ_type
tab couple_educ_type, m
tab _mi_m couple_educ_type, row

// earnings. these are imputed so need mi passive. why did i impute both lol.
// need to check, monthly are last month but annual are last year - did I recenter?? (yeah, I did this all the way in step 4)
browse pid eligible_partner _mi_m monthly_earnings_woman* annual_earnings_woman* 
browse pid eligible_partner _mi_m monthly_earnings_man* annual_earnings_man*

pwcorr monthly_earnings_woman1 annual_earnings_woman1

mi passive: egen couple_earnings_t1 = rowtotal(annual_earnings_woman1 annual_earnings_man1) // more people have earnings annually than monthly, which makes sense. also this is just meant to be directional so it's okay
sum couple_earnings_t1, detail

// home ownership status
tab housing_woman1 housing_man1 if _mi_m==0
tab housing_woman1 housing_man1 // okay this had a lot of missing I think and  so it's really quite incongruous across partners...
tab housing_woman2 housing_man2 // okay 2 is much better also? Possibly bc time 1 capturing like time in previous HH? I actually think I should use this one.

gen home_owner=.
replace home_owner=0 if inrange(housing_woman2,1,3) // main / sub tenant both imply rent (and sub tenant quite smalL) and then shared small, so think binary is fine here
replace home_owner=1 if housing_woman2==4

// race/ethnicity
/*
for Germany, let's do whichever I imputed (the country born or nationality - can't remember) and then also where born: E v. W Germany v abroad. Will do current residence E v. W and federal state below
where_born_state // imputed as fixed. In Step 7, I also created the E v. W passive version as well from this
born_in_germany // not imputed.
global_region_born // not imputed

born_germany_woman born_germany_man
global_region_born_woman global_region_born_man
federal_state_born_woman federal_state_born_man
ew_born_woman ew_born_man
*/

tab federal_state_born_woman ew_born_woman if _mi_m!=0, m // these perfectly match (bc ew born created off of this)
tab federal_state_born_woman born_germany_woman if _mi_m!=0, m // basically congruous (like 23 don't match)
tab global_region_born_woman born_germany_woman if _mi_m!=0, m
tab global_region_born_woman ew_born_woman if _mi_m!=0, m
// okay so I can just use these as is actually

tab ew_born_woman ew_born_man, m
tab born_germany_woman born_germany_man, m row

gen ethn_homogamy = .
replace ethn_homogamy = 1 if born_germany_woman==1 & born_germany_man==1 // both born Germany
replace ethn_homogamy = 2 if born_germany_woman==0 & born_germany_man==1 // one German / other not
replace ethn_homogamy = 2 if born_germany_woman==1 & born_germany_man==0 // one German / other not
replace ethn_homogamy = 3 if born_germany_woman==0 & born_germany_man==0 // both born abroad

label define ethn_homogamy 1 "Both German" 2 "Mixed" 3 "Both Abroad"
label values ethn_homogamy ethn_homogamy

// I actually think these where born are too detailed and will cause problems
tab global_region_born_man mc9_factor

gen region_born_gp_man=.
replace region_born_gp_man = 0 if global_region_born_man==0
replace region_born_gp_man = 1 if global_region_born_man==3
replace region_born_gp_man = 2 if inlist(global_region_born_man,4,9)
replace region_born_gp_man = 3 if inlist(global_region_born_man,5,6,7)
replace region_born_gp_man = 4 if inlist(global_region_born_man,1,2,8)

gen region_born_gp_woman=.
replace region_born_gp_woman = 0 if global_region_born_woman==0
replace region_born_gp_woman = 1 if global_region_born_woman==3
replace region_born_gp_woman = 2 if inlist(global_region_born_woman,4,9)
replace region_born_gp_woman = 3 if inlist(global_region_born_woman,5,6,7)
replace region_born_gp_woman = 4 if inlist(global_region_born_woman,1,2,8)

label define region_born_gp 0 "Germany" 1 "SE Europe" 2 "Africa and Middle East" 3 "Asia" 4 "Other"
label values region_born_gp_man region_born_gp_woman region_born_gp

tab global_region_born_man  region_born_gp_man, m
tab region_born_gp_man mc9_factor
tab region_born_gp_woman mc9_factor

// also denote which sample (created previously to drop refugees) - but let's see if this corresponds to above
tab sample_type sample_type_sp, m // these match so I can just pick 1

tab born_germany_woman sample_type, row // for those born in Germany, they are mostly core, but those not are mixed core v. migrant
gen imm_sample_woman = .
replace imm_sample_woman = 0 if sample_type==1
replace imm_sample_woman = 1 if sample_type==2

// current residence
tab where_ew_woman1 where_ew_man1 if _mi_m!=0, m row
tab where_ew_woman2 where_ew_man2 if _mi_m!=0, m row // again, t2 matches much better than t1
tab where_ew_woman1 where_ew_woman2, row
tab where_ew_man1 where_ew_man2, row // men move like...a touch less, so use his

tab federal_state_woman1 federal_state_man1, m
tab federal_state_woman2 federal_state_man2, m // okay, I am actually going to use time 2 for all of these
tab urban_region_woman1 urban_region_man1, m
tab urban_region_woman2 urban_region_man2, m

* Relationship info
// age at rel start
gen age_man1 = age1 if SEX==1 // I only kept women but this is fine
replace age_man1 = age_sp1 if SEX==2

gen age_woman1=age1 if SEX==2
replace age_woman1=age_sp1 if SEX==1

	// temp save

// relationship order - created in step 7
// okay actually want to create a binary indicator of whether in first rel
tab rel_no_woman, m
tab rel_no_man, m

gen in_first_rel_woman = 0
replace in_first_rel_woman = 1 if rel_no_woman==1

gen in_first_rel_man = 0
replace in_first_rel_man = 1 if rel_no_man==1

// first birth timing relative to relationship start
tab first_birth_year, m // no missing, this is not imputed (there were small amt missing I believe and dropped)
browse pid eligible_partner SEX eligible_rel_start_year _mi_m first_birth_year first_birth_year_sp birth_timing_rel birth_timing_rel_sp
	
	// also, see notes from PSID, but I had done before as relationship - first birth and I want the opposite (first birth - relationship)
	// there is only women, so I can do this this way (as just main v spouse without gendered versions)
	gen first_birth_timing_man = first_birth_year_sp - eligible_rel_start_year
	replace first_birth_timing_man = . if first_birth_year_sp==9999

	gen first_birth_timing_woman = first_birth_year - eligible_rel_start_year
	replace first_birth_timing_woman = . if first_birth_year==9999 // need to account for this better, but don't want the 9999 skewing.

// browse pid eligible_partner SEX eligible_rel_start_year _mi_m first_birth_year first_birth_year_sp birth_timing_rel birth_timing_rel_sp first_birth_timing_woman first_birth_timing_man

tab first_birth_timing_woman, m 

// also create a binary of pre / post? I think so yeah
gen first_birth_pre_rel_man = .
replace first_birth_pre_rel_man = 0 if first_birth_timing_man >=0 & first_birth_timing_man!=.
replace first_birth_pre_rel_man = 0 if first_birth_year_sp==9999 // can actually put 9999 here because is, theoretically, 0 if no births
replace first_birth_pre_rel_man = 1 if first_birth_timing_man <0 & first_birth_timing_man!=.

tab first_birth_timing_man first_birth_pre_rel_man, m
tab first_birth_year_sp first_birth_pre_rel_man, m

gen first_birth_pre_rel_woman = .
replace first_birth_pre_rel_woman = 0 if first_birth_timing_woman >=0 & first_birth_timing_woman!=.
replace first_birth_pre_rel_woman = 0 if first_birth_year==9999 // can actually put 9999 here because is, theoretically, 0 if no births
replace first_birth_pre_rel_woman = 1 if first_birth_timing_woman <0 & first_birth_timing_woman!=.

tab first_birth_timing_woman first_birth_pre_rel_woman, m
tab first_birth_year first_birth_pre_rel_woman, m

// age of youngest child - this is just in the HH, prob useful descriptively but we don't 100% know with this variable if it's their kid together
tab age_youngest_woman1 age_youngest_man1, m
tab age_youngest_woman1 age_youngest_man1 if _mi_m!=0, m // okay these are less congruent, but I think following what I do for # of kids, use the woman's

// ever parent status
gen ever_parent_woman=ever_parent if SEX==2
replace ever_parent_woman=ever_parent_sp if SEX==1

tab first_birth_year ever_parent_woman, m 

gen ever_parent_man=ever_parent if SEX==1
replace ever_parent_man=ever_parent_sp if SEX==2

// mpf status
gen mpf_man=any_mpf if SEX==1
replace mpf_man=any_mpf_sp if SEX==2

gen mpf_woman=any_mpf if SEX==2
replace mpf_woman=any_mpf_sp if SEX==1

tab mpf_woman mpf_man, m

* Family background - I created a lot of these in step 7, not only gendered versions, but also the final variables to use based on underlying variables
label values father_educ_woman father_educ_man mother_educ_woman mother_educ_man edu3 
tab father_educ_woman, m // so "high" is 5-8. think can call that university? or just use this as is here?
	// https://ec.europa.eu/eurostat/statistics-explained/index.php?title=International_Standard_Classification_of_Education_(ISCED)

// let's create these for now to match the others, but can revisit
mi passive: gen father_college_woman = .
mi passive: replace father_college_woman = 0 if inrange(father_educ_woman,1,2)
mi passive: replace father_college_woman = 1 if father_educ_woman==3

mi passive: gen father_college_man = .
mi passive: replace father_college_man = 0 if inrange(father_educ_man,1,2)
mi passive: replace father_college_man = 1 if father_educ_man==3

mi passive: gen mother_college_woman = .
mi passive: replace mother_college_woman = 0 if inrange(mother_educ_woman,1,2)
mi passive: replace mother_college_woman = 1 if mother_educ_woman==3

mi passive: gen mother_college_man = .
mi passive: replace mother_college_man = 0 if inrange(mother_educ_man,1,2)
mi passive: replace mother_college_man = 1 if mother_educ_man==3

// who_lived_with - not imputed
// yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other - imputed
tab who_lived_with_woman if _mi_m!=0, m // this is based on summing the years lived across both / mom / dad / other (bc imputed) - then at least 50% bio. okay this still has missing, I think because some people don't have 50% anyone?
browse pid who_lived_with_woman bio_pct_woman mom_pct_woman other_pct_woman dad_pct_woman _mi_m

tab who_lived_with_man if _mi_m!=0, m
tab yrs_bio_parent_woman who_lived_with_woman, m // I guess if I want this to match PSID/UK, it'll just be bio YN binary, so could just put all of these missing in 0, because they are people with less than 8 years with bio parents (aka less than 50%)

mi passive: gen family_structure_man = .
mi passive: replace family_structure_man = 0 if inrange(who_lived_with_man,2,4)
mi passive: replace family_structure_man = 0 if who_lived_with_man==. & _mi_m!=0 // these are people where not more than 50% bio parent, but split across other categories
mi passive: replace family_structure_man = 1 if who_lived_with_man==1
tab who_lived_with_man family_structure_man if _mi_m!=0, m

mi passive: gen family_structure_woman = .
mi passive: replace family_structure_woman = 0 if inrange(who_lived_with_woman,2,4)
mi passive: replace family_structure_woman = 0 if who_lived_with_woman==. & _mi_m!=0 // these are people where not more than 50% bio parent, but split across other categories
mi passive: replace family_structure_woman = 1 if who_lived_with_woman==1
tab who_lived_with_woman family_structure_woman if _mi_m!=0, m

* Social context
// make some sort of birth cohort? can also use age to describe within cluster (but categorical will be easier for between cluster)
// birthyr_pl birthyr_pl_sp
gen bcohort_man = .
replace bcohort_man = 1 if birthyr_pl_sp < 1960
replace bcohort_man = 2 if birthyr_pl_sp >= 1960 & birthyr_pl_sp < 1970
replace bcohort_man = 3 if birthyr_pl_sp >= 1970 & birthyr_pl_sp < 1980
replace bcohort_man = 4 if birthyr_pl_sp >= 1980 & birthyr_pl_sp < 1990
replace bcohort_man = 5 if birthyr_pl_sp >= 1990 & birthyr_pl_sp < 2010

gen bcohort_woman = .
replace bcohort_woman = 1 if birthyr_pl < 1960
replace bcohort_woman = 2 if birthyr_pl >= 1960 & birthyr_pl < 1970
replace bcohort_woman = 3 if birthyr_pl >= 1970 & birthyr_pl < 1980
replace bcohort_woman = 4 if birthyr_pl >= 1980 & birthyr_pl < 1990
replace bcohort_woman = 5 if birthyr_pl >= 1990 & birthyr_pl < 2010

capture label define bcohort 1 "Pre-1960s" 2 "1960s" 3 "1970s" 4 "1980s" 5 "1990s+"
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

// religion - this was imputed (I think quite a bit of missing)
tab religion_woman1 if _mi_m!=0, m
tab religion_man1 if _mi_m!=0, m

mi passive: gen religion_gp_woman1 = .
mi passive: replace religion_gp_woman1 = 0 if religion_woman1==6
mi passive: replace religion_gp_woman1 = 1 if religion_woman1==1
mi passive: replace religion_gp_woman1 = 2 if religion_woman1==2
mi passive: replace religion_gp_woman1 = 3 if inlist(religion_woman1,3,7)
mi passive: replace religion_gp_woman1 = 4 if inlist(religion_woman1,4,5,8,9,10)

tab religion_woman1 religion_gp_woman1, m

mi passive: gen religion_gp_man1 = .
mi passive: replace religion_gp_man1 = 0 if religion_man1==6
mi passive: replace religion_gp_man1 = 1 if religion_man1==1
mi passive: replace religion_gp_man1 = 2 if religion_man1==2
mi passive: replace religion_gp_man1 = 3 if inlist(religion_man1,3,7)
mi passive: replace religion_gp_man1 = 4 if inlist(religion_man1,4,5,8,9,10)

tab religion_man1 religion_gp_man1, m

capture label define religion_gp 0 "Non-Denom" 1 "Catholic" 2 "Protestant" 3 "Other Christian" 4 "Other non-Christian"
label values religion_gp_man1 religion_gp_woman1 religion_gp

** Other caregiving obligations (potentially)
// disability status. okay these were created in step 7 and are fine
tab disabled_woman1 if _mi_m!=0, m
tab disabled_man1 if _mi_m!=0, m

// parents in HH
tab any_parent_in_hh1, m // yeah time 1 continues to be so off for germany, I think bc some still live with family of origin
tab any_parent_in_hh2, m
tab any_parent_in_hh_sp1, m
tab any_parent_in_hh_sp2, m

browse pid eligible_partner sequence_length any_parent_in_hh1 any_parent_in_hh2 any_parent_in_hh3 any_parent_in_hh4 any_parent_in_hh5 any_parent_in_hh6 any_parent_in_hh7 any_parent_in_hh8 any_parent_in_hh9 any_parent_in_hh10

egen years_parent_in_hh = anycount(any_parent_in_hh2 any_parent_in_hh3 any_parent_in_hh4 any_parent_in_hh5 any_parent_in_hh6 any_parent_in_hh7 any_parent_in_hh8 any_parent_in_hh9 any_parent_in_hh10), values(1 2) // I am not including 1 because I think will skew. so need to divide by sequence length - 1 omg
	// should I add actually divide by sequence length??
	gen pct_parent_in_hh = years_parent_in_hh / (sequence_length - 1)
	replace pct_parent_in_hh = 1 if pct_parent_in_hh>1 & pct_parent_in_hh!=.
// browse pid eligible_partner sequence_length years_parent_in_hh pct_parent_in_hh any_parent_in_hh2 any_parent_in_hh3 any_parent_in_hh4 any_parent_in_hh5 any_parent_in_hh6 any_parent_in_hh7 any_parent_in_hh8 any_parent_in_hh9 any_parent_in_hh10
mean years_parent_in_hh pct_parent_in_hh

egen years_parent_in_hh_sp = anycount(any_parent_in_hh_sp2 any_parent_in_hh_sp3 any_parent_in_hh_sp4 any_parent_in_hh_sp5 any_parent_in_hh_sp6 any_parent_in_hh_sp7 any_parent_in_hh_sp8 any_parent_in_hh_sp9 any_parent_in_hh_sp10), values(1 2)
	gen pct_parent_in_hh_sp = years_parent_in_hh_sp / (sequence_length - 1)
	replace pct_parent_in_hh_sp = 1 if pct_parent_in_hh_sp>1 & pct_parent_in_hh_sp!=.
	
gen pct_parent_woman=pct_parent_in_hh if SEX==2
replace pct_parent_woman=pct_parent_in_hh_sp if SEX==1

gen pct_parent_man=pct_parent_in_hh if SEX==1
replace pct_parent_man=pct_parent_in_hh_sp if SEX==2

// people 65+ in HH
tab num_65up_hh1, m
tab num_65up_hh_sp1, m
tab num_65up_hh1 num_65up_hh_sp1, m
tab num_65up_hh1 num_65up_hh_sp1 if _mi_m!=0, m
tab num_65up_hh2 num_65up_hh_sp2 if _mi_m!=0, m

browse pid eligible_partner sequence_length num_65up_hh1 num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10
egen num_65up_hh_avg = rowmean(num_65up_hh1 num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10) // don't love this, but this is what I did for PSID so leave for now... (basically averages presence across 10 years, so sort of a proportion out of 100)

// other aid / caregiving responsibilities. remember - i did *not* impute childcare, so can't use that.
// this is just that someone in HH requires aid: aid_in_hh_hl. I created gendered versions in step 7
tab aid_in_hh_woman1 aid_in_hh_man1
tab aid_in_hh_woman2 aid_in_hh_man2 // again match better at t2, not t1 (bc it's HH level so should match...)

// browse pid eligible_partner sequence_length aid_in_hh_woman1 aid_in_hh_woman2 aid_in_hh_woman3 aid_in_hh_woman4 aid_in_hh_woman5 aid_in_hh_woman6 aid_in_hh_woman7 aid_in_hh_woman8 aid_in_hh_woman9 aid_in_hh_woman10
egen years_aid_hh = anycount(aid_in_hh_woman1 aid_in_hh_woman2 aid_in_hh_woman3 aid_in_hh_woman4 aid_in_hh_woman5 aid_in_hh_woman6 aid_in_hh_woman7 aid_in_hh_woman8 aid_in_hh_woman9 aid_in_hh_woman10), values(1)
	// should I add actually divide by sequence length??
	gen pct_aid_hh = years_aid_hh / sequence_length
	replace pct_aid_hh = 1 if pct_aid_hh>1 & pct_aid_hh!=.
	
tabstat years_aid_hh pct_aid_hh sequence_length

// self-rated health
label values sr_health_woman* sr_health_man* srh5
tab sr_health_woman1, m
tab sr_health_man1, m

// need to recode because I want higher to be better (that is how I recoded for PSID)
label define health_rev 1 "Bad" 2 "Poor" 3 "Satisfactory" 4 "Good" 5 "Very Good"

forvalues d=1/11{
	foreach var in sr_health_man sr_health_woman{
		recode `var'`d' (1=5)(2=4)(3=3)(4=2)(5=1), gen(`var'_rev`d')
		label values `var'_rev`d' health_rev
	}
}

tab sr_health_woman1 sr_health_woman_rev1
tabstat sr_health_woman1 sr_health_woman_rev1 sr_health_man1 sr_health_man_rev1

**# Save
mi update

save "$created_data/GSOEP_truncated_clusters_analysis.dta", replace

********************************************************************************
**# Within cluster descriptives
********************************************************************************
*Descriptive table for entire sample
// desctable i.education_man i.education_woman i.couple_educ_type i.raceth_man i.raceth_woman c.age_man1 c.age_woman1 i.bcohort_man i.bcohort_woman // i.couple_earnings_quart1 c.couple_earnings_t1, filename("$tables/desc_sample_all") stats(mimean) 

// test
tab couple_educ_type, gen(educ_type)
mi estimate: proportion couple_educ_type
mi estimate: mean educ_type1 educ_type2 educ_type3 educ_type4

// create variables needed from binary
// tab couple_educ_type, gen(educ_type) // did above to check
tab ew_born_man, gen(ew_born_man)
tab ew_born_woman, gen(ew_born_woman)
tab ethn_homogamy, gen(ethn_hom)
tab region_born_gp_man, gen(where_born_man)
tab region_born_gp_woman, gen(where_born_woman)
tab where_ew_woman2, gen(where_ew)
tab rel_cohort, gen(rel_cohort)
tab bcohort_man, gen(bc_man)
tab bcohort_woman, gen(bc_woman)
tab religion_gp_man1, gen(relig_man)
tab religion_gp_woman1, gen(relig_woman)

*Descriptive table by cluster
putexcel set "$tables/SOEP_stata_descriptives_by_cluster.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Early Trans to Trad"
putexcel D1 = "2: Egal Cohab"
putexcel E1 = "3: Neotrad Cohab to Marr"
putexcel F1 = "4: Trad Marriage"
putexcel G1 = "5: CF Cohab + 2nd Shift"
putexcel H1 = "6: Late Trans to Neotrad"
putexcel I1 = "7: OW + CF Cohab"
putexcel J1 = "8: Trad Cohab + Kids"
putexcel K1 = "9: UW Cohab"

putexcel A2 = "Couple Educ: Neither College"
putexcel A3 = "Couple Educ: Her College"
putexcel A4 = "Couple Educ: Him College"
putexcel A5 = "Couple Educ: Both College"
putexcel A6 = "Couple Earnings (T1)"
putexcel A7 = "Home Owner T1"
putexcel A8 = "Where Born Man: Abroad"
putexcel A9 = "Where Born Man: West Germany"
putexcel A10 = "Where Born Man: East Germany"
putexcel A11 = "Where Born Woman: Abroad"
putexcel A12 = "Where Born Woman: West Germany"
putexcel A13 = "Where Born Woman: East Germany"
putexcel A14 = "Ethnic Homogamy: Both German"
putexcel A15 = "Ethnic Homogamy: Mixed"
putexcel A16 = "Ethnic Homogamy: Both Abroad"
putexcel A17 = "Where Born Man: Germany"
putexcel A18 = "Where Born Man: S/E Europe"
putexcel A19 = "Where Born Man: Africa and Middle East"
putexcel A20 = "Where Born Man: Asia"
putexcel A21 = "Where Born Man: Other"
putexcel A22 = "Where Born Woman: Germany"
putexcel A23 = "Where Born Woman: S/E Europe"
putexcel A24 = "Where Born Woman: Africa and Middle East"
putexcel A25 = "Where Born Woman: Asia"
putexcel A26 = "Where Born Woman: Other"
putexcel A27 = "In Immigrant Sample"
putexcel A28 = "Where Currently Live: West Germany"
putexcel A29 = "Where Currently Live: East Germany"
putexcel A30 = "Where Currently Live: Urban YN"
putexcel A31 = "Age Man (T1)"
putexcel A32 = "Age Woman (T1)"
putexcel A33 = "Relationship Number Man"
putexcel A34 = "Relationship Number Woman"
putexcel A35 = "In First Rel YN: Man"
putexcel A36 = "In First Rel YN: Woman"
putexcel A37 = "First Birth Timing Man"
putexcel A38 = "First Birth Timing Woman"
putexcel A39 = "First Birth Pre Rel YN: Man"
putexcel A40 = "First Birth Pre Rel YN: Woman"
putexcel A41 = "Age Youngest Child in HH"
putexcel A42 = "Ever Parent Man"
putexcel A43 = "Ever Parent Woman"
putexcel A44 = "MPF Y/N Man"
putexcel A45 = "MPF Y/N Woman"
putexcel A46 = "Man's Father has College Degree"
putexcel A47 = "Man's Mother has College Degree"
putexcel A48 = "Woman's Father has College Degree"
putexcel A49 = "Woman's Mother has College Degree"
putexcel A50 = "Man Lived with Bio Parents (50%+ of Childhood)"
putexcel A51 = "Woman Lived with Bio Parents (50%+ of Childhood)"
putexcel A52 = "Rel Cohort: 1990-1999"
putexcel A53 = "Rel Cohort: 2000-2009"
putexcel A54 = "Rel Cohort: 2010+"
putexcel A55 = "Birth Cohort Man: Pre 1960s"
putexcel A56 = "Birth Cohort Man: 1960s"
putexcel A57 = "Birth Cohort Man: 1970s"
putexcel A58 = "Birth Cohort Man: 1980s"
putexcel A59 = "Birth Cohort Man: 1990s+"
putexcel A60 = "Birth Cohort Woman: Pre 1960s"
putexcel A61 = "Birth Cohort Woman: 1960s"
putexcel A62 = "Birth Cohort Woman: 1970s"
putexcel A63 = "Birth Cohort Woman: 1980s"
putexcel A64 = "Birth Cohort Woman: 1990s+"
putexcel A65 = "Religion Man: Non-Denom"
putexcel A66 = "Religion Man: Catholic"
putexcel A67 = "Religion Man: Protestant"
putexcel A68 = "Religion Man: Other Christian"
putexcel A69 = "Religion Man: Other Non-Christian"
putexcel A70 = "Religion Woman: Non-Denom"
putexcel A71 = "Religion Woman: Catholic"
putexcel A72 = "Religion Woman: Protestant"
putexcel A73 = "Religion Woman: Other Christian"
putexcel A74 = "Religion Woman: Other Non-Christian"
putexcel A75 = "Disabled Man Y/N"
putexcel A76 = "Disabled Woman Y/N"
putexcel A77 = "Man's Parents in HH (% Years)"
putexcel A78 = "Woman's Parents in HH (% Years)"
putexcel A79 = "Number People 65+ in HH (average)"
putexcel A80 = "Someone in HH Needs Aid (% Years)"
putexcel A81 = "Self-rated health Man (Avg T1)"
putexcel A82 = "Self-rated health Woman (Avg T1)"

local desc_vars "educ_type1 educ_type2 educ_type3 educ_type4 couple_earnings_t1 home_owner ew_born_man1 ew_born_man2 ew_born_man3 ew_born_woman1 ew_born_woman2 ew_born_woman3 ethn_hom1 ethn_hom2 ethn_hom3 where_born_man1 where_born_man2 where_born_man3 where_born_man4 where_born_man5 where_born_woman1 where_born_woman2 where_born_woman3 where_born_woman4 where_born_woman5 imm_sample_woman where_ew1 where_ew2 urban_region_woman2 age_man1 age_woman1 rel_no_man rel_no_woman in_first_rel_man in_first_rel_woman first_birth_timing_man first_birth_timing_woman first_birth_pre_rel_man first_birth_pre_rel_woman age_youngest_woman1 ever_parent_man ever_parent_woman mpf_man mpf_woman father_college_man mother_college_man father_college_woman mother_college_woman family_structure_man family_structure_woman rel_cohort1 rel_cohort2 rel_cohort3 bc_man1 bc_man2 bc_man3 bc_man4 bc_man5 bc_woman1 bc_woman2 bc_woman3 bc_woman4 bc_woman5 relig_man1 relig_man2 relig_man3 relig_man4 relig_man5 relig_woman1 relig_woman2 relig_woman3 relig_woman4 relig_woman5 disabled_man1 disabled_woman1 pct_parent_man pct_parent_woman num_65up_hh_avg pct_aid_hh sr_health_man_rev1 sr_health_woman_rev1"

// full sample
forvalues w=1/81{
	local row=`w'+1
	local var: word `w' of `desc_vars'
	mi estimate: mean `var'
	matrix t`var'= e(b_mi)
	local t`var' = t`var'[1,1]
	putexcel B`row' = `t`var'', nformat(#.#%)
}


// by cluster
log using "$results/cluster_descriptives.log", replace
local col1 "C D E F G H I J K"

forvalues c=1/9{
	local col: word `c' of `col1'
	forvalues w=1/81{
		local row=`w'+1
		local var: word `w' of `desc_vars'
		capture noisily mi estimate, esampvaryok: mean `var' if mc9_factor==`c'
		matrix c`var'= e(b_mi)
		local c`var' = c`var'[1,1]
		putexcel `col'`row' = `c`var'', nformat(#.#%)
	}
}

log close

// the global region born women is failing for cluster 3, born in Africa, even though I recoded
tab region_born_gp_woman mc9_factor
mi estimate, esampvaryok: mean where_born_woman3 if mc9_factor==3
capture mi estimate, esampvaryok: mean where_born_woman3 if mc9_factor==3
capture noisily mi estimate, esampvaryok: mean where_born_woman3 if mc9_factor==3
// could add a capture to the above. is that problematic, though, because I won't know where else fails?
// could use capture noisily I think, and then a log file? so that I can go through and check once it's done? if any errors get thrown?
// clusters 1 and 2 finished so I will just start with cluster 3 to rerun for now (bc this takes so long)

// check
mi estimate: proportion couple_educ_type region_born_gp_man religion_gp_woman1 // validate that this matches
mi estimate: mean couple_earnings_t1 educ_type1 sr_health_man_rev1 father_college_woman rel_no_woman

mi estimate, esampvaryok: proportion couple_educ_type region_born_gp_man religion_gp_woman1 if mc9_factor==1
mi estimate, esampvaryok: proportion couple_educ_type region_born_gp_man religion_gp_woman1 if mc9_factor==4
mi estimate, esampvaryok: mean couple_earnings_t1 educ_type1 sr_health_man_rev1 father_college_woman rel_no_woman  if mc9_factor==2
mi estimate, esampvaryok: mean couple_earnings_t1 educ_type1 sr_health_man_rev1 father_college_woman rel_no_woman  if mc9_factor==7
tabstat couple_earnings_t1, by(mc9_factor)

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
use "$created_data/GSOEP_truncated_clusters_analysis.dta", clear

mi reshape long employment self_reported_health disability_yn religious_affiliation errands_sundays housework_saturdays housework_sundays childcare_saturdays childcare_sundays repair_saturdays errands_weekdays housework_weekdays childcare_weekdays repair_weekdays errands_saturdays aid_in_hh_hl kidsu18_hh num_65up_hh age_youngest_child age nationality_region federal_state housing_status weekly_work_hrs gross_income_lm net_income_lm hh_income_net_monthly earnings_gross_t_cnef hh_gross_income_t_cnef repair_sundays any_outside_help any_parent_in_hh current_parent_status marst_imp retired_yn full_status_pl duplicate_record urban_region employment_sp self_reported_health_sp disability_yn_sp religious_affiliation_sp errands_sundays_sp housework_saturdays_sp housework_sundays_sp childcare_saturdays_sp childcare_sundays_sp repair_saturdays_sp errands_weekdays_sp housework_weekdays_sp childcare_weekdays_sp repair_weekdays_sp errands_saturdays_sp nationality_pb_sp aid_in_hh_hl_sp kidsu18_hh_sp num_65up_hh_sp age_youngest_child_sp age_sp nationality_region_sp federal_state_sp housing_status_sp weekly_work_hrs_sp gross_income_lm_sp net_income_lm_sp hh_income_net_monthly_sp earnings_gross_t_cnef_sp hh_gross_income_t_cnef_sp repair_sundays_sp any_outside_help_sp any_parent_in_hh_sp current_parent_status_sp marst_imp_sp retired_yn_sp full_status_pl_sp urban_region_sp weekly_hrs_woman weekly_hrs_man employment_status_woman employment_status_man monthly_earnings_woman monthly_earnings_man annual_earnings_woman annual_earnings_man housework_weekdays_woman housework_weekdays_man housework_saturdays_woman housework_saturdays_man housework_sundays_woman housework_sundays_man repair_weekdays_woman repair_weekdays_man repair_saturdays_woman repair_saturdays_man repair_sundays_woman repair_sundays_man errands_weekdays_woman errands_weekdays_man errands_saturdays_woman errands_saturdays_man errands_sundays_woman errands_sundays_man aid_in_hh_woman aid_in_hh_man marital_status_woman marital_status_man partnered_woman partnered_man num_children_woman num_children_man age_youngest_woman age_youngest_man federal_state_woman federal_state_man where_ew_woman where_ew_man urban_region_woman urban_region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow_detailed couple_work_ow couple_weekday_hw_total woman_weekday_hw_share couple_hw_weekday housework_weekdays_5_woman housework_weekly_est_woman housework_weekdays_5_man housework_weekly_est_man couple_weekly_hw_total woman_weekly_hw_share couple_hw_weekly housework_combined_woman housework_combined_man couple_combined_hw_total woman_combined_hw_share couple_hw_combined syear rel_type couple_num_children couple_num_children_gp family_type hw_weekly_hilow_woman hw_weekly_hilow_equal couple_hw_hrs_weekly hw_combined_hilow_equal hw_combined_hilow_woman couple_hw_hrs_combined hw_weekday_hilow_woman hw_weekday_hilow_equal hw_weekday_hilow_test hw_weekday_equal couple_hw_hrs_weekday ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_weekday_end couple_hw_hrs_weekday_end couple_hw_weekly_end couple_hw_hrs_weekly_end couple_hw_combined_end couple_hw_hrs_combined_end couple_num_children_gp_end family_type_end couple_work_ow_trunc couple_hw_hrs_weekday_trunc couple_hw_hrs_weekly_trunc couple_hw_hrs_combined_trunc family_type_trunc sr_health_man_rev sr_health_woman_rev, i(pid eligible_partner eligible_rel_start_year eligible_rel_end_year) j(duration) 

mi estimate: proportion couple_work_ow_trunc couple_hw_hrs_weekly_trunc family_type_trunc if duration <=10
mi estimate: proportion couple_work_ow_trunc couple_hw_hrs_weekly_trunc family_type_trunc if duration ==10

tab _mi_m mc9_factor, m

tab couple_work_ow_trunc, gen(work_seq)
tab couple_hw_hrs_weekly_trunc, gen(hw_seq)
tab family_type_trunc, gen(fam_seq)
mi estimate: mean work_seq1 work_seq2 if duration<=10
mi estimate: proportion couple_work_ow_trunc if duration<=10

putexcel set "$tables/SOEP_stata_cluster_composition.xlsx", replace
putexcel B1 = "Full Sample"
putexcel C1 = "1: Early Trans to Trad"
putexcel D1 = "2: Egal Cohab"
putexcel E1 = "3: Neotrad Cohab to Marr"
putexcel F1 = "4: Trad Marriage"
putexcel G1 = "5: CF Cohab + 2nd Shift"
putexcel H1 = "6: Late Trans to Neotrad"
putexcel I1 = "7: OW + CF Cohab"
putexcel J1 = "8: Trad Cohab + Kids"
putexcel K1 = "9: UW Cohab"

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
local col1 "C D E F G H I J K"

forvalues c=1/9{
	local col: word `c' of `col1'
	forvalues w=1/6{
	   mi estimate, esampvaryok: mean work_seq`w' if mc9_factor==`c' & duration<=10
	   matrix w`w' = e(b_mi)
	   local w`w' = w`w'[1,1]
	   local row = 2+`w'
	   putexcel `col'`row' = `w`w'', nformat(##.#%)
	}
}

forvalues c=1/9{
	local col: word `c' of `col1'
	forvalues h=1/5{
	   mi estimate, esampvaryok: mean hw_seq`h' if mc9_factor==`c'  & duration<=10
	   matrix h`h' = e(b_mi)
	   local h`h' = h`h'[1,1]
	   local row = 10+`h'
	   putexcel `col'`row' = `h`h'', nformat(##.#%)
	}
}

forvalues c=1/9{
	local col: word `c' of `col1'
	forvalues f=1/8{
	   capture noisily mi estimate, esampvaryok: mean fam_seq`f' if mc9_factor==`c'  & duration<=10 // didn't do capture with noisily, so there are a few I need to check; think I ran into similar issues like I did with UK
	   matrix f`f' = e(b_mi)
	   local f`f' = f`f'[1,1]
	   local row = 17+`f'
	   putexcel `col'`row' = `f`f'', nformat(##.#%)
	}
}


// check
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_weekly_trunc family_type_trunc if duration <=10 & mc9_factor==1
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_weekly_trunc family_type_trunc if duration <=10 & mc9_factor==4
mi estimate, esampvaryok: proportion couple_work_ow_trunc couple_hw_hrs_weekly_trunc family_type_trunc if duration <=10 & mc9_factor==9
