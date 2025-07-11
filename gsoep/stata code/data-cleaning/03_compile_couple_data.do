********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: compile_couple_data.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file first uses the couple list created in step 2 as a filter for which
* records to keep from ppathl.
* That file then forms the base file, onto which all other cleaned gsoep
* data-sets are merged.
* from there (maybe in next step), we do individual recodes
* (Will follow UKHLS file d rows 96+ for reference)

********************************************************************************
* Start with base data and merge on eligible couples
* to create a filter to get the actual sample file to use
********************************************************************************
/*
use "$created_data/ppathl_partnership_history.dta", clear
unique pid // 130434 5217360 - this is rectangularized so everyone has 40 years of records

use "$temp/ppathl_partner_match_cleaned.dta", clear
unique pid // 88288 598217 - this is restricted to partners
*/

use "$temp/ppathl_cleaned.dta", clear
unique pid // 198137 1446266 - this is NOT rectangularized, but is the most simple version of this file and includes people never interviewed.
// think we want to start here, get couples, tHEN rectangularize like I did before to then restrict to the -2 to 12

merge m:1 pid partner_id_pl using "$created_data/gsoep_couple_list.dta" //, keepusing(num_rel eligible_couple eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_no eligible_rel_status eligible_partner)
// following UK, merge for the specific couple, then create a MAX indicator of whether that person is ever eligible

drop if _merge==2
drop _merge
 
bysort pid: egen ever_eligible = max(eligible_couple)
bysort pid: egen max_eligible_rels = max(num_rel)
replace ever_eligible = 0 if ever_eligible==.
tab ever_eligible eligible_couple, m
tab partnered_pl ever_eligible, m

sort pid syear
browse pid partnered_pl partner_id_pl syear ever_eligible max_eligible_rels num_rel eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_couple_id status_pl // if max_eligible_rels > 1

keep if ever_eligible==1

// need to copy info to all rows - for people with one relationship, this is easy, we just copy to all rows
foreach var in eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no ever_transition transition_year min_dur max_dur first_couple_year last_couple_year{
	bysort pid (`var'): replace `var' = `var'[1] if max_eligible_rels==1
}

// this is harder for those with multiple - I think the clock should restart after every breakup so single to end of rel 1 = rel 1; single to end of rel 2 = rel 2, etc...how do I do this. okay BUT I guess i have to fill in their last few years with the prior relationship if they don't end in a relationship...
// I guess the question is where I do want these - like longer durations post relationship or pre-duration? This literally only matters for imputation because I do not actually retain these non relationship years in the analysis...

gsort pid -syear 

foreach var in eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no ever_transition transition_year min_dur max_dur first_couple_year last_couple_year{
	replace `var' = `var'[_n-1] if `var'==. & `var'[_n-1]!=. & pid==pid[_n-1] & max_eligible_rels > 1
	// sort pid syear
}

sort pid syear // yes do this to get those remaining years
foreach var in eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no ever_transition transition_year min_dur max_dur first_couple_year last_couple_year{
	replace `var' = `var'[_n-1] if `var'==. & `var'[_n-1]!=. & pid==pid[_n-1] & max_eligible_rels > 1
}

assert eligible_rel_start_year!=.
assert eligible_rel_end_year!=.
// assert eligible_rel_status!=. // this is false - I think because not complete for everyone (this was true in UKHLS as well)
assert eligible_partner!=.
// assert eligible_rel_no!=. // this is false - I think because not complete for everyone  (this was true in UKHLS as well)

// now generate a relationship duration so I know which years to keep
generate relative_duration = syear - eligible_rel_start_year

browse pid partnered_pl partner_id_pl syear relative_duration max_eligible_rels eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_couple_id status_pl

// keep a few durations around 0 to 10
keep if relative_duration >=-2
keep if relative_duration <=12

save "$temp/gsoep_couple_sample_base.dta", replace // here we save the file of couples for which we will now merge the rest of the data onto.

unique pid // 28032
unique pid eligible_partner // 29531

********************************************************************************
**# Now we start merging on all of the gsoep files
********************************************************************************

/* list of files
// pid syear
"$temp/pl_cleaned.dta" (01)
"$temp/pgen_cleaned.dta" (01)
"$temp/pbrutto_cleaned.dta" (01)
"$temp/pequiv_cleaned.dta" (01)
"$temp/biol_cleaned.dta" (01)

// just pid
"$temp/bioparen_cleaned.dta" (01)

// spell data (pid)
"$created_data/consolidated_rel_history.dta" (01a)
"$temp/biobirth_cleaned.dta" (01a)
"$temp/mpf_lookup.dta" (01a)

// hid syear
"$temp/hl_cleaned.dta" (01)
"$temp/hgen_cleaned.dta" (01)
"$temp/hbrutto_cleaned.dta" (01)
"$temp/hh_comp_lookup.dta" (01a)
*/

// first just merge basic individual characteristics
// bc this is ppathl and includes dropout / non-sample years
// (which I did on purpose bc that is exactly the data we want to impute)
// there will be quite a bit of non-matches, but validating that the sample years do have matches
use "$temp/gsoep_couple_sample_base.dta", clear

merge 1:1 pid hid cid syear using "$temp/pl_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // confirm that the non-matches are non-sample
drop _merge

merge 1:1 pid hid cid syear using "$temp/pgen_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // confirm that the non-matches are non-sample
drop _merge

merge 1:1 pid hid cid syear using "$temp/pbrutto_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // pbrutto actully also contains info from non-sample years, so match rate is much higher (<1% non-match - but still all dropouts)
drop _merge

merge 1:1 pid hid cid syear using "$temp/pequiv_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // some no int years actually also match (unclear if there is valid data though)
drop _merge

merge 1:1 pid hid cid syear using "$temp/biol_cleaned.dta" // this file is chaos and really actually only matches one year -- will merge on as is for now, but might need to revisit  this file and its use and uniqueness. ideal is to use bioparen anyway (bc that is just pid)
drop if _merge==2
gen biol_int_year = 0
replace biol_int_year = 1 if _merge==3 // will flag which year is their biol year just for reference
drop _merge

// files just on pid
merge m:1 pid using "$temp/bioparen_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m row // this file is tricky bc we get coverage on non-sample years if ever did this, but some poeple never do the bio interview / have data here (they also like didn't update this data briefly) so the sample coverage is not 100% (but it's close - it's 98%)...and general coverage is 90% bc of the uniqueness
drop _merge

merge m:1 pid using "$created_data/consolidated_rel_history.dta"
drop if _merge==2 // 100% match rate (which makes sense based on how I created this file)
drop _merge

merge m:1 pid using "$temp/biobirth_cleaned.dta"
drop if _merge==2 // 100% match rate also (this actually surprises me. think possibly bc of couple start date restriction? BUT just because matched does not also mean these people have birth history
tab biovalid_bh, m // yes - so some have estimated birth history info
drop _merge

merge m:1 pid using "$temp/mpf_lookup.dta"
drop if _merge==2 // 100% coverage
drop _merge

// then HH characteristics
merge m:1 hid syear using "$temp/hl_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // the good thing here is we get data on no int years as long as one person in HH responded
drop _merge

merge m:1 hid syear using "$temp/hgen_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // the good thing here is we get data on no int years as long as one person in HH responded
drop _merge

merge m:1 hid syear using "$temp/hbrutto_cleaned.dta"
drop if _merge==2
tab status_pl _merge, m // bc hbrutto also contains non sample info AND it's HH level, much higher coverage  (literally less than .05% have no record)
drop _merge

merge m:1 hid syear using "$temp/hh_comp_lookup.dta"
drop if _merge==2 // same here as above bc it's HH info
drop _merge

// confirm still matches above. it does
unique pid // 28032
unique pid eligible_partner // 29531

save "$temp/gsoep_couple_data_compiled.dta", replace
