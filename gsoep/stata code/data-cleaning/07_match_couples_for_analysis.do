********************************************************************************
********************************************************************************
* Project: Relationship Life Course Analysis
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: match_couples_for_analysis
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes the imputed data from step 6 at an individual level and
* matches the corresponding imputed partner data.
* It also creates gendered and couple-level variables.

use "$created_data/gsoep_individs_imputed_long_bysex", clear

********************************************************************************
* Match couples
********************************************************************************
// from step 5 - but this only works if all code run together
gen relative_duration = duration - 2

// browse pid eligible_partner relative_duration _mi_miss _mi_m _mi_id imputed

global keep_fixed "sex_pl birthyr_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl psample_pl lastyr_survey_pl country_born_pl eligible_rel_no ever_transition transition_year ever_int sumkids any_mpf born_in_germany where_born_ew where_born_state where_1989_ew global_region_born mother_educ father_educ yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other who_lived_with first_birth_year last_birth_year edu4_fixed isced97_fixed yrs_educ_fixed nat_region_fixed nationality_fixed ever_parent birth_timing_rel"

global keep_time "where_germany_pl survey_status_pl status_pl employment self_reported_health disability_yn disability_amount religious_affiliation errands_sundays housework_saturdays housework_sundays childcare_saturdays childcare_sundays repair_saturdays errands_weekdays housework_weekdays childcare_weekdays repair_weekdays errands_saturdays emplst_pg isced97_pg yrs_educ_pg nationality_pb survey_status_pb earnings_gross_t_cnef hh_gross_income_t_cnef hh_net_income_t_cnef live_fam_bp aid_in_hh_hl housing_status federal_state kidsu18_hh num_65up_hh age_youngest_child urban_region age edu4 nationality_region home_owner marst_defacto partnered_total religion_est employed_binary weekly_work_hrs gross_income_lm net_income_lm hh_income_net_monthly repair_sundays any_outside_help num_parent_in_hh any_parent_in_hh current_parent_status in_rel_year marst_imp partnered_imp retired_yn full_status_pl"

// just keep necessary variables - i think this doesn't work if I try to keep variables that weren't imputed
local partnervars $keep_fixed $keep_time
display "`partnervars'"

keep pid eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_couple_id relative_duration min_dur max_dur first_couple_year last_couple_year _mi_miss _mi_m _mi_id  `partnervars'

mi rename eligible_partner x
mi rename pid eligible_partner
mi rename x pid // swap unique and partner to match (bc need to know it's the same couple / duration). I guess I could merge on rel_start_date as well

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	mi rename `var' `var'_sp
}

// not sure what happened here...okay this is actually fine now
bysort pid eligible_partner: egen rowcount = count(relative_duration) if _mi_m==0
tab rowcount, m

mi update

save "$temp/gsoep_partner_data_imputed.dta", replace

unique pid eligible_partner 

// match on partner id and duration
use "$created_data/gsoep_individs_imputed_long_bysex", clear
gen relative_duration = duration - 2

mi merge 1:1 pid eligible_partner relative_duration using "$temp/gsoep_partner_data_imputed.dta", keep(match) // gen(howmatch) 
// unique pidp eligible_partner, by(howmatch) // this is like 80% match - I feel like that is the case for the UKHLS as well, so this is fine...
// browse pid eligible_partner eligible_couple_id if howmatch==1
// have also confirmed going back to multiple ppathl files - the non-matches are people with partners who never had an interview, I can find them in ppathl, but they are all ever_int==0 (or at the least the sample I chose to investigate - sO I imagine this is true for all)

unique pid eligible_partner
unique eligible_couple_id

browse pid eligible_partner relative_duration full_status_pl full_status_pl_sp sex_pl sex_pl_sp weekly_work_hrs weekly_work_hrs_sp housework_weekdays housework_weekdays_sp _mi_m

tab sex_pl sex_pl_sp, m
drop if sex_pl==sex_pl_sp

mi rename sex_pl SEX // to match PSID
mi rename sex_pl_sp SEX_sp

mi update

save "$created_data/gsoep_couples_imputed_long.dta", replace

********************************************************************************
**# Create gendered variables
********************************************************************************
// label define sex 1 "Male" 2 "Female"
label values SEX SEX_sp sex

// first, let's make gendered versions of each variable
*paid work
mi passive: gen weekly_hrs_woman=weekly_work_hrs if SEX==2
mi passive: replace weekly_hrs_woman=weekly_work_hrs_sp if SEX==1

mi passive: gen weekly_hrs_man=weekly_work_hrs if SEX==1
mi passive: replace weekly_hrs_man=weekly_work_hrs_sp if SEX==2

*detailed employment status
mi passive: gen employment_status_woman=employment if SEX==2
mi passive: replace employment_status_woman=employment_sp if SEX==1

mi passive: gen employment_status_man=employment if SEX==1
mi passive: replace employment_status_man=employment_sp if SEX==2

label values employment_status_woman employment_status_man employment

*monthly earnings
mi passive: gen monthly_earnings_woman=gross_income_lm if SEX==2
mi passive: replace monthly_earnings_woman=gross_income_lm_sp if SEX==1

mi passive: gen monthly_earnings_man=gross_income_lm if SEX==1
mi passive: replace monthly_earnings_man=gross_income_lm_sp if SEX==2

*annual earnings
mi passive: gen annual_earnings_woman=earnings_gross_t_cnef if SEX==2
mi passive: replace annual_earnings_woman=earnings_gross_t_cnef_sp if SEX==1

mi passive: gen annual_earnings_man=earnings_gross_t_cnef if SEX==1
mi passive: replace annual_earnings_man=earnings_gross_t_cnef_sp if SEX==2

*unpaid work 
foreach var in housework_weekdays housework_saturdays housework_sundays repair_weekdays repair_saturdays repair_sundays errands_weekdays errands_saturdays errands_sundays{
	mi passive: gen `var'_woman=`var' if SEX==2
	mi passive: replace `var'_woman=`var'_sp if SEX==1

	mi passive: gen `var'_man=`var' if SEX==1
	mi passive: replace `var'_man=`var'_sp if SEX==2
}

*someone in HH needs aid // in theory should match when partnered, but perhaps not when not partnered
mi passive: gen aid_in_hh_woman=aid_in_hh_hl if SEX==2
mi passive: replace aid_in_hh_woman=aid_in_hh_hl_sp if SEX==1

mi passive: gen aid_in_hh_man=aid_in_hh_hl if SEX==1
mi passive: replace aid_in_hh_man=aid_in_hh_hl_sp if SEX==2

tab aid_in_hh_woman aid_in_hh_man, m

*relationship status
tab marst_imp partnered_imp, m col

mi passive: gen marital_status_woman=marst_imp if SEX==2
mi passive: replace marital_status_woman=marst_imp_sp if SEX==1

mi passive: gen marital_status_man=marst_imp if SEX==1
mi passive: replace marital_status_man=marst_imp_sp if SEX==2

label values marital_status_woman marital_status_man marst_defacto

mi passive: gen partnered_woman=.
mi passive: replace partnered_woman = 0 if inlist(marital_status_woman,3,4,5)
mi passive: replace partnered_woman = 1 if inlist(marital_status_woman,1,2)

mi passive: gen partnered_man=.
mi passive: replace partnered_man = 0 if inlist(marital_status_man,3,4,5)
mi passive: replace partnered_man = 1 if inlist(marital_status_man,1,2)

tab partnered_woman partnered_man, m
tab partnered_woman partnered_man if imputed==1, m // well these could also be partnerships that occurred outside of focal partnership

gen rel_no_woman=eligible_rel_no if SEX==2 // not imputed
replace rel_no_woman=eligible_rel_no_sp if SEX==1

gen rel_no_man=eligible_rel_no if SEX==1
replace rel_no_man=eligible_rel_no_sp if SEX==2

*number of children
tab kidsu18_hh kidsu18_hh_sp, m
tab kidsu18_hh kidsu18_hh_sp if imputed==1
tab kidsu18_hh kidsu18_hh_sp if imputed==0 // match generally better here

mi passive: gen num_children_woman=kidsu18_hh if SEX==2
mi passive: replace num_children_woman=kidsu18_hh_sp if SEX==1

mi passive: gen num_children_man=kidsu18_hh if SEX==1
mi passive: replace num_children_man=kidsu18_hh_sp if SEX==2

* Age of youngest child
mi passive: gen age_youngest_woman=age_youngest_child if SEX==2
mi passive: replace age_youngest_woman=age_youngest_child_sp if SEX==1

mi passive: gen age_youngest_man=age_youngest_child if SEX==1
mi passive: replace age_youngest_man=age_youngest_child_sp if SEX==2

// some demographics
* Education
gen edu4_fixed_woman=edu4_fixed if SEX==2 // not imputed
replace edu4_fixed_woman=edu4_fixed_sp if SEX==1

gen edu4_fixed_man=edu4_fixed if SEX==1
replace edu4_fixed_man=edu4_fixed_sp if SEX==2

label values edu4_fixed_woman edu4_fixed_man edu4

* Federal State of Residence
tab federal_state where_germany_pl, m

mi passive: gen federal_state_woman=federal_state if SEX==2
mi passive: replace federal_state_woman=federal_state_sp if SEX==1

mi passive: gen federal_state_man=federal_state if SEX==1
mi passive: replace federal_state_man=federal_state_sp if SEX==2

label values federal_state_woman federal_state_man federal_state

* Residence: East / West
mi passive: gen where_ew_woman=.
mi passive: replace where_ew_woman = 1 if inrange(federal_state_woman,1,11) // west
mi passive: replace where_ew_woman = 2 if inrange(federal_state_woman,12,17) // east

mi passive: gen where_ew_man=.
mi passive: replace where_ew_man = 1 if inrange(federal_state_man,1,11)
mi passive: replace where_ew_man = 2 if inrange(federal_state_man,12,17)

label values where_ew_woman where_ew_man sampreg

* Urban v. Rural Residence
mi passive: gen urban_region_woman=urban_region if SEX==2
mi passive: replace urban_region_woman=urban_region_sp if SEX==1

mi passive: gen urban_region_man=urban_region if SEX==1
mi passive: replace urban_region_man=urban_region_sp if SEX==2

* Housing status
mi passive: gen housing_woman=housing_status if SEX==2
mi passive: replace housing_woman=housing_status_sp if SEX==1

mi passive: gen housing_man=housing_status if SEX==1
mi passive: replace housing_man=housing_status_sp if SEX==2

label values housing_woman housing_man housing_status

* Religion
mi passive: gen religion_woman=religious_affiliation if SEX==2
mi passive: replace religion_woman=religious_affiliation_sp if SEX==1

mi passive: gen religion_man=religious_affiliation if SEX==1
mi passive: replace religion_man=religious_affiliation_sp if SEX==2

label values religion_woman religion_man religion

*Disability status
mi passive: gen disabled_woman=disability_yn if SEX==2
mi passive: replace disabled_woman=disability_yn_sp if SEX==1

mi passive: gen disabled_man=disability_yn if SEX==1
mi passive: replace disabled_man=disability_yn_sp if SEX==2

* Self-rated health
mi passive: gen sr_health_woman=self_reported_health if SEX==2
mi passive: replace sr_health_woman=self_reported_health_sp if SEX==1

mi passive: gen sr_health_man=self_reported_health if SEX==1
mi passive: replace sr_health_man=self_reported_health_sp if SEX==2

label values self_reported_health sr_health_man srh5

* Retirement status
mi passive: gen retired_woman=retired_yn if SEX==2
mi passive: replace retired_woman=retired_yn_sp if SEX==1

mi passive: gen retired_man=retired_yn if SEX==1
mi passive: replace retired_man=retired_yn_sp if SEX==2

* Born in Germany
gen born_germany_woman=born_in_germany if SEX==2 // not imputed
replace born_germany_woman=born_in_germany_sp if SEX==1

gen born_germany_man=born_in_germany if SEX==1
replace born_germany_man=born_in_germany_sp if SEX==2

* Region of Birth
gen global_region_born_woman=global_region_born if SEX==2 // not imputed
replace global_region_born_woman=global_region_born_sp if SEX==1

gen global_region_born_man=global_region_born if SEX==1
replace global_region_born_man=global_region_born_sp if SEX==2

label values global_region_born_woman global_region_born_man cob

* Federal State of Birth
tab where_born_state where_born_ew if _mi_m==0
mi passive: gen federal_state_born_woman=where_born_state if SEX==2
mi passive: replace federal_state_born_woman=where_born_state_sp if SEX==1

mi passive: gen federal_state_born_man=where_born_state if SEX==1
mi passive: replace federal_state_born_man=where_born_state_sp if SEX==2

label values federal_state_born_woman federal_state_born_man birthregion

* East / West at Birth
mi passive: gen ew_born_woman=.
mi passive: replace ew_born_woman = 0 if federal_state_born_woman==0 // abroad
mi passive: replace ew_born_woman = 1 if inrange(federal_state_born_woman,1,10) // west
mi passive: replace ew_born_woman = 2 if inrange(federal_state_born_woman,11,16) // east

mi passive: gen ew_born_man=.
mi passive: replace ew_born_man = 0 if federal_state_born_man==0 // abroad
mi passive: replace ew_born_man = 1 if inrange(federal_state_born_man,1,10)
mi passive: replace ew_born_man = 2 if inrange(federal_state_born_man,11,16)

label values ew_born_woman ew_born_man where_born_ew

* Father education
mi passive: gen father_educ_woman=father_educ if SEX==2
mi passive: replace father_educ_woman=father_educ_sp if SEX==1

mi passive: gen father_educ_man=father_educ if SEX==1
mi passive: replace father_educ_man=father_educ_sp if SEX==2

* Mother education
mi passive: gen mother_educ_woman=mother_educ if SEX==2
mi passive: replace mother_educ_woman=mother_educ_sp if SEX==1

mi passive: gen mother_educ_man=mother_educ if SEX==1
mi passive: replace mother_educ_man=mother_educ_sp if SEX==2

* Family structure growing up
// first, create continuous indicators
foreach var in yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other{
	mi passive: gen `var'_woman=`var' if SEX==2
	mi passive: replace `var'_woman=`var'_sp if SEX==1

	mi passive: gen `var'_man=`var' if SEX==1
	mi passive: replace `var'_man=`var'_sp if SEX==2
}

// then create 1 indicator
// test:
// egen yrs_live_accounting = rowtotal(yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other), missing
egen yrs_live_accounting_woman = rowtotal(yrs_bio_parent_woman yrs_live_mom_woman yrs_live_dad_woman yrs_live_other_woman), missing // this probably won't add up to 15 because of imputation...
egen yrs_live_accounting_man = rowtotal(yrs_bio_parent_man yrs_live_mom_man yrs_live_dad_man yrs_live_other_man), missing // this probably won't add up to 15 because of imputation...

mi passive: gen bio_pct_woman = yrs_bio_parent_woman / yrs_live_accounting_woman
mi passive: gen mom_pct_woman = yrs_live_mom_woman / yrs_live_accounting_woman
mi passive: gen dad_pct_woman = yrs_live_dad_woman / yrs_live_accounting_woman
mi passive: gen other_pct_woman = yrs_live_other_woman / yrs_live_accounting_woman

mi passive: gen bio_pct_man = yrs_bio_parent_man / yrs_live_accounting_man
mi passive: gen mom_pct_man = yrs_live_mom_man / yrs_live_accounting_man
mi passive: gen dad_pct_man = yrs_live_dad_man / yrs_live_accounting_man
mi passive: gen other_pct_man = yrs_live_other_man / yrs_live_accounting_man

mi passive: gen who_lived_with_woman = .
mi passive: replace who_lived_with_woman = 1 if who_lived_with_woman==. & bio_pct_woman>.5000000 & bio_pct_woman!=. // using greater than 50% for now
mi passive: replace who_lived_with_woman = 2 if who_lived_with_woman==. & mom_pct_woman>.5000000 & mom_pct_woman!=.
mi passive: replace who_lived_with_woman = 3 if who_lived_with_woman==. & dad_pct_woman>.5000000 & dad_pct_woman!=.
mi passive: replace who_lived_with_woman = 4 if who_lived_with_woman==. & other_pct_woman>.5000000 & other_pct_woman!=.

mi passive: gen who_lived_with_man = .
mi passive: replace who_lived_with_man = 1 if who_lived_with_man==. & bio_pct_man>.5000000 & bio_pct_man!=. // using greater than 50%
mi passive: replace who_lived_with_man = 2 if who_lived_with_man==. & mom_pct_man>.5000000 & mom_pct_man!=.
mi passive: replace who_lived_with_man = 3 if who_lived_with_man==. & dad_pct_man>.5000000 & dad_pct_man!=.
mi passive: replace who_lived_with_man = 4 if who_lived_with_man==. & other_pct_man>.5000000 & other_pct_man!=.

label define who_lived_with 1 "Both Parents" 2 "Mom" 3 "Dad" 4 "Other"
label values who_lived_with_woman who_lived_with_man who_lived_with

tab who_lived_with_woman, m
tab who_lived_with_man, m
tab yrs_bio_parent_woman who_lived_with_woman, m

**# Bookmark #1
// temp save
save "$created_data/gsoep_couples_imputed_long_recoded.dta", replace

// Stata assert command to check new variables created from imputed  
foreach var in weekly_hrs_woman weekly_hrs_man employment_status_woman employment_status_man monthly_earnings_woman monthly_earnings_man annual_earnings_woman annual_earnings_man housework_weekdays_woman housework_saturdays_woman housework_sundays_woman repair_weekdays_woman repair_saturdays_woman repair_sundays_woman errands_weekdays_woman errands_saturdays_woman errands_sundays_woman housework_weekdays_man housework_saturdays_man housework_sundays_man repair_weekdays_man repair_saturdays_man repair_sundays_man errands_weekdays_man errands_saturdays_man errands_sundays_man aid_in_hh_woman aid_in_hh_man marital_status_woman marital_status_man partnered_woman partnered_man rel_no_woman rel_no_man num_children_woman num_children_man age_youngest_woman age_youngest_man edu4_fixed_woman edu4_fixed_man federal_state_woman federal_state_man where_ew_woman where_ew_man urban_region_woman urban_region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man born_germany_woman born_germany_man global_region_born_woman global_region_born_man federal_state_born_woman federal_state_born_man ew_born_woman ew_born_man father_educ_woman father_educ_man mother_educ_woman mother_educ_man{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

********************************************************************************
**# Create couple-level variables
********************************************************************************

********************
* Paid work
********************
mi passive: gen ft_pt_woman = .
mi passive: replace ft_pt_woman = 0 if weekly_hrs_woman==0 // not working
mi passive: replace ft_pt_woman = 1 if weekly_hrs_woman > 0 & weekly_hrs_woman < 35 // PT
mi passive: replace ft_pt_woman = 2 if weekly_hrs_woman >=35 & weekly_hrs_woman < 200 // FT

mi passive: gen overwork_woman=. // Cha and Weeden = 50 hrs, Cha 2010 = 50 and 60, Munsch = 60
mi passive: replace overwork_woman = 0 if weekly_hrs_woman >= 0 & weekly_hrs_woman < 50
mi passive: replace overwork_woman = 1 if weekly_hrs_woman >=50 & weekly_hrs_woman < 200 

mi passive: gen ft_pt_man = .
mi passive: replace ft_pt_man = 0 if weekly_hrs_man==0 // not working
mi passive: replace ft_pt_man = 1 if weekly_hrs_man > 0 & weekly_hrs_man < 35 // PT
mi passive: replace ft_pt_man = 2 if weekly_hrs_man >=35 & weekly_hrs_man < 200 // FT

mi passive: gen overwork_man=. // Cha and Weeden = 50 hrs, Cha 2010 = 50 and 60, Munsch = 60
mi passive: replace overwork_man = 0 if weekly_hrs_man >= 0 & weekly_hrs_man < 50
mi passive: replace overwork_man = 1 if weekly_hrs_man >=50 & weekly_hrs_man < 200 

label define ft_pt 0 "Not working" 1 "PT" 2 "FT"
label values ft_pt_woman ft_pt_man ft_pt

tab ft_pt_woman overwork_woman
tab ft_pt_man overwork_man

mi estimate: proportion ft_pt_woman ft_pt_man

histogram weekly_hrs_woman if ft_pt_woman==1 
sum weekly_hrs_woman if ft_pt_woman==1, det
histogram weekly_hrs_man if ft_pt_man==1 
sum weekly_hrs_man if ft_pt_man==1, det

twoway (histogram weekly_hrs_woman if ft_pt_woman==1, width(1) color(pink%30)) (histogram weekly_hrs_man if ft_pt_man==1, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours among PT Workers")

* more detailed breakdown and men's and women's work
/* not using for now
sum weekly_hrs_woman if ft_pt_woman==1, det
mi passive: gen ft_pt_det_woman = .
mi passive: replace ft_pt_det_woman = 0 if weekly_hrs_woman==0 // not working
mi passive: replace ft_pt_det_woman = 1 if weekly_hrs_woman > 0 & weekly_hrs_woman < 20 // PT: low - either do median using r(p50) or use 20 and cite K&Z? doing the latter for now
mi passive: replace ft_pt_det_woman = 2 if weekly_hrs_woman >= 20 & weekly_hrs_woman < 35 // PT: high
mi passive: replace ft_pt_det_woman = 3 if weekly_hrs_woman >=35 & weekly_hrs_woman < 50 // FT: normal
mi passive: replace ft_pt_det_woman = 4 if weekly_hrs_woman >=50 & weekly_hrs_woman < 200 // FT: overwork

mi passive: gen ft_pt_det_man = .
mi passive: replace ft_pt_det_man = 0 if weekly_hrs_man==0 // not working
mi passive: replace ft_pt_det_man = 1 if weekly_hrs_man > 0 & weekly_hrs_man < 20 // PT: low - either do median using r(p50) or use 20 and cite K&Z?
mi passive: replace ft_pt_det_man = 2 if weekly_hrs_man >= 20 & weekly_hrs_man < 35 // PT: high
mi passive: replace ft_pt_det_man = 3 if weekly_hrs_man >=35 & weekly_hrs_man < 50 // FT: normal
mi passive: replace ft_pt_det_man = 4 if weekly_hrs_man >=50 & weekly_hrs_man < 200 // FT: overwork

label define ft_pt_det 0 "not working" 1 "PT < 20hrs" 2 "PT 20-35" 3 "FT: Normal" 4 "FT: OW"
label values ft_pt_det_woman ft_pt_det_man ft_pt_det

mi estimate: proportion ft_pt_det_woman ft_pt_det_man
mi estimate: proportion ft_pt_det_woman ft_pt_det_man if age_all >=18 & age_all <=60
tab ft_pt_det_woman if age_all >=18 & age_all <=60 & imputed==0 // it's still quite high even in non-imputed data. is this right?! okay it was also quite high in descrptives for relative density...
*/

* couple-level version
tab ft_pt_woman ft_pt_man

mi passive: gen couple_work=.
mi passive: replace couple_work = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work = 3 if ft_pt_man == 2 & ft_pt_woman == 2
mi passive: replace couple_work = 4 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work = 4 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work = 5 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work = 5 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work = 5 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work = 5 if ft_pt_man == 1 & ft_pt_woman == 0

label define couple_work 1 "male bw" 2 "1.5 male bw" 3 "dual FT" 4 "female bw" 5 "under work"
label values couple_work couple_work

// mi estimate: proportion couple_work

* with overwork
mi passive: gen couple_work_ow_detailed=.
mi passive: replace couple_work_ow_detailed = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work_ow_detailed = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work_ow_detailed = 3 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==0
mi passive: replace couple_work_ow_detailed = 4 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==1 & overwork_woman==0
mi passive: replace couple_work_ow_detailed = 5 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==1
mi passive: replace couple_work_ow_detailed = 6 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==1 & overwork_woman==1
mi passive: replace couple_work_ow_detailed = 7 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work_ow_detailed = 7 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work_ow_detailed = 8 if ft_pt_man == 1 & ft_pt_woman == 0

label define couple_work_ow_detailed 1 "male bw" 2 "1.5 male bw" 3 "dual FT: no OW" 4 "dual FT: his OW" 5 "dual FT: her OW" 6 "dual FT: both OW" /// 
7 "female bw" 8 "under work"
label values couple_work_ow_detailed couple_work_ow_detailed

mi estimate: proportion couple_work_ow_detailed

* Consolidated overwork version (new - 5/21/25)
mi passive: gen couple_work_ow=.
mi passive: replace couple_work_ow = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work_ow = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 3 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==0
mi passive: replace couple_work_ow = 4 if ft_pt_man == 2 & ft_pt_woman == 2 & (overwork_man==1 | overwork_woman==1)
mi passive: replace couple_work_ow = 5 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work_ow = 5 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work_ow = 6 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 6 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work_ow = 6 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 6 if ft_pt_man == 1 & ft_pt_woman == 0

capture label drop couple_work_ow
label define couple_work_ow 1 "male bw" 2 "1.5 male bw" 3 "dual FT: no OW" 4 "dual FT: any OW" 5 "female bw" 6 "under work"
label values couple_work_ow couple_work_ow

mi estimate: proportion couple_work_ow_detailed couple_work_ow

********************
* Unpaid work
********************
// Let's create three versions for now (while I am doing all of this)
	// 1 just using weekday housework (housework_weekdays_woman housework_weekdays_man)
	// 2 using housework estimated for the week based on weekday / weekends (so it's weekly) (housework_saturdays_woman housework_saturdays_man housework_sundays_woman housework_sundays_man)
	// 3 using weekday housework + repair work (repair_weekdays_woman repair_weekdays_man)
	
*****
*1. Start with weekday
mi passive: egen couple_weekday_hw_total = rowtotal(housework_weekdays_woman housework_weekdays_man)
mi passive: gen woman_weekday_hw_share = housework_weekdays_woman / couple_weekday_hw_total // this does have missing I think is couple HW total is 0

sum housework_weekdays_woman, det
sum housework_weekdays_woman if housework_weekdays_woman!=0, det
sum housework_weekdays_man, det
sum woman_weekday_hw_share, det

/*
mi passive: egen hw_terc_woman = cut(housework_weekdays_woman) if housework_weekdays_woman!=0, group(3) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_terc_woman
tabstat housework_weekdays_woman, by(hw_terc_woman)

mi passive: egen hw_hilow_woman = cut(housework_weekdays_woman) if housework_weekdays_woman!=0, group(2) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_hilow_woman
tabstat housework_weekdays_woman, by(hw_hilow_woman)

mi passive: egen hw_hilow_man = cut(housework_weekdays_man) if housework_weekdays_man!=0, group(2) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_hilow_man
tabstat housework_weekdays_man, by(hw_hilow_man)
*/

mi passive: gen couple_hw_weekday=.
mi passive: replace couple_hw_weekday = 1 if woman_weekday_hw_share==1
mi passive: replace couple_hw_weekday = 2 if woman_weekday_hw_share > 0.60 & woman_weekday_hw_share < 1
mi passive: replace couple_hw_weekday = 3 if woman_weekday_hw_share >= 0.40 & woman_weekday_hw_share <= 0.60
mi passive: replace couple_hw_weekday = 4 if woman_weekday_hw_share < 0.40
mi passive: replace couple_hw_weekday = 3 if housework_weekdays_woman==0 & housework_weekdays_man==0  // neither is so small, just put in equal

label define couple_hw 1 "Woman All" 2 "Woman Most" 3 "Equal" 4 "Man Most" 5 "Neither HW"
label values couple_hw_weekday couple_hw

mi estimate: proportion couple_hw_weekday

	// Cutpoints: within equal HW
	sum couple_weekday_hw_total, detail
	sum couple_weekday_hw_total if couple_weekday_hw_total!=0, detail
	sum couple_weekday_hw_total if couple_weekday_hw_total!=0 & 
	- okay so , detail
	sum couple_weekday_hw_total if couple_hw_weekday==3, detail // I put the 0s in here, so actually, I think I should *not* remove 0s
	mi passive: egen hw_weekday_hilow_equal = cut(couple_weekday_hw_total) if couple_hw_weekday==3, group(2) // couple_weekday_hw_total!=0 & 
	// is this not working because it's impossible to split into two groups? Now, it is just splitting 0 into low. why isn't it splitting at like...3
	tab couple_weekday_hw_total if couple_hw_weekday==3 // so this is why it's not splitting - bc 70% are 2 or below. It is impossible to split at 2
	tab hw_weekday_hilow_equal if couple_hw_weekday==3
	tabstat couple_weekday_hw_total, by(hw_weekday_hilow_equal) stats(mean min max)
	mi passive: egen hw_weekday_hilow_test = cut(couple_weekday_hw_total), group(2) // couple_weekday_hw_total!=0 & 
	tab hw_weekday_hilow_test if couple_hw_weekday==3, m
	tab hw_weekday_hilow_test, m // okay but the frequencies are essentially reversed for everyone v. the equal housework... should I just split at 2? is that what this is doing?
	tabstat couple_weekday_hw_total, by(hw_weekday_hilow_test) stats(mean min max) // wait this doesn't make any sense?, but the max is 2.5 in first group
	
	// let's try to split at 2
	mi passive: gen hw_weekday_equal = 0 if couple_weekday_hw_total <=2
	mi passive: replace hw_weekday_equal = 1 if couple_weekday_hw_total > 2 & couple_weekday_hw_total < 50
	tab hw_weekday_equal if couple_hw_weekday==3, m
	tab hw_weekday_equal, m
	tab hw_weekday_hilow_test  hw_weekday_equal if couple_hw_weekday==3
	
	twoway (histogram couple_weekday_hw_total if hw_weekday_equal==0, width(1) color(gray%30)) ///
	(histogram couple_weekday_hw_total if hw_weekday_equal==1, width(1) color(pink%30)) ///
	, legend(order(1 "Low" 2 "High") rows(1) position(6)) xtitle("Core HW: Weekdays, Couple Total")
	
	twoway (histogram couple_weekday_hw_total if hw_weekday_hilow_test==0, width(1) color(gray%30)) ///
	(histogram couple_weekday_hw_total if hw_weekday_hilow_test==1, width(1) color(pink%30)) ///
	, legend(order(1 "Low" 2 "High") rows(1) position(6)) xtitle("Core HW: Weekdays, Couple Total") // yeah these overlap in a way they should not...
	
	// Cutpoints: within she does most OR all
	sum housework_weekdays_woman, detail
	sum housework_weekdays_woman if housework_weekdays_woman!=0, detail
	mi passive: egen hw_weekday_hilow_woman = cut(housework_weekdays_woman) if housework_weekdays_woman!=0 & inlist(couple_hw_weekday,1,2), group(2)
	tab hw_weekday_hilow_woman if inlist(couple_hw_weekday,1,2)
	tabstat housework_weekdays_woman, by(hw_weekday_hilow_woman)


/* we are not using these detailed ones anymore - since I am creating many housework variables here, I am not going to create for now
* adding consideration of how many hours she does - this is based on TOTAL distribution of HW, not within each bucket
mi passive: gen couple_hw_hrs=.
mi passive: replace couple_hw_hrs = 1 if couple_hw==1 & hw_hilow_woman==1
mi passive: replace couple_hw_hrs = 2 if couple_hw==1 & hw_hilow_woman==0
mi passive: replace couple_hw_hrs = 3 if couple_hw==2 & hw_terc_woman==2
mi passive: replace couple_hw_hrs = 4 if couple_hw==2 & hw_terc_woman==1
mi passive: replace couple_hw_hrs = 5 if couple_hw==2 & hw_terc_woman==0
mi passive: replace couple_hw_hrs = 6 if couple_hw==3 & couple_weekday_hw_total > =20 & couple_weekday_hw_total < 500
mi passive: replace couple_hw_hrs = 7 if couple_hw==3 & couple_weekday_hw_total < 20
mi passive: replace couple_hw_hrs = 8 if couple_hw==4 & hw_hilow_man==1
mi passive: replace couple_hw_hrs = 9 if couple_hw==4 & hw_hilow_man==0
mi passive: replace couple_hw_hrs = 7 if housework_weekdays_woman==0 & housework_weekdays_man==0 // neither is so small, just put in equal low

label define couple_hw_hrs 1 "Woman All: High" 2 "Woman All: Low" 3 "Woman Most: High" 4 "Woman Most: Med" 5 "Woman Most: Low" 6 "Equal: High" 7 "Equal: Low" 8 "Man Most: High" 9 "Man Most: Low" // rationale for splitting women most into three and the others into two is that it is the largest group (about 52%)
label values couple_hw_hrs couple_hw_hrs

// mi estimate: proportion couple_hw_hrs couple_hw

* Now adding consideration of how many hours - based on distribution of hours WITHIN each bucket of HW
mi passive: gen couple_hw_hrs_alt=.
mi passive: replace couple_hw_hrs_alt = 1 if couple_hw==1 & hw_hilow_woman_gp1==1
mi passive: replace couple_hw_hrs_alt = 2 if couple_hw==1 & hw_hilow_woman_gp1==0
mi passive: replace couple_hw_hrs_alt = 3 if couple_hw==2 & hw_hilow_woman_gp2==2
mi passive: replace couple_hw_hrs_alt = 4 if couple_hw==2 & hw_hilow_woman_gp2==1
mi passive: replace couple_hw_hrs_alt = 5 if couple_hw==2 & hw_hilow_woman_gp2==0
mi passive: replace couple_hw_hrs_alt = 6 if couple_hw==3 & couple_weekday_hw_total > =20 & couple_weekday_hw_total < 500
mi passive: replace couple_hw_hrs_alt = 7 if couple_hw==3 & couple_weekday_hw_total < 20
mi passive: replace couple_hw_hrs_alt = 8 if couple_hw==4 & hw_hilow_man_gp4==1
mi passive: replace couple_hw_hrs_alt = 9 if couple_hw==4 & hw_hilow_man_gp4==0
mi passive: replace couple_hw_hrs_alt = 7 if housework_weekdays_woman==0 & housework_weekdays_man==0 // neither is so small, just put in equal low

label values couple_hw_hrs_alt couple_hw_hrs

mi estimate: proportion couple_hw couple_hw_hrs couple_hw_hrs_alt 
*/

* Consolidated housework + hours variable
mi passive: gen couple_hw_hrs_weekday=.
mi passive: replace couple_hw_hrs_weekday = 1 if inlist(couple_hw_weekday,1,2) & hw_weekday_hilow_woman==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_weekday = 2 if inlist(couple_hw_weekday,1,2) & hw_weekday_hilow_woman==0 
mi passive: replace couple_hw_hrs_weekday = 3 if couple_hw_weekday==3 & hw_weekday_equal==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_weekday = 4 if couple_hw_weekday==3 & hw_weekday_equal==0 
mi passive: replace couple_hw_hrs_weekday = 5 if couple_hw_weekday==4
mi passive: replace couple_hw_hrs_weekday = 4 if housework_weekdays_woman==0 & housework_weekdays_man==0  // neither is so small, just put in equal low

capture label define couple_hw_hrs_combo 1 "Woman Most: High" 2 "Woman Most: Low" 3 "Equal: High" 4 "Equal: Low" 5 "Man Most: All"
label values couple_hw_hrs_weekday couple_hw_hrs_combo

mi estimate: proportion couple_hw_weekday couple_hw_hrs_weekday

*****
*2. Now attempt to estimate weekly
	// mi passive: gen housework_weekdays_5 = housework_weekdays * 5
	// mi passive: egen housework_weekly_est = rowtotal(housework_weekdays_5 housework_saturdays housework_sundays), missing
	// inspect housework_weekly_est if _mi_m!=0
	// sum housework_weekly_est, det
	// browse pid relative_duration _mi_m housework_weekly_est housework_weekdays housework_saturdays housework_sundays

mi passive: gen housework_weekdays_5_woman = housework_weekdays_woman * 5
mi passive: egen housework_weekly_est_woman = rowtotal(housework_weekdays_5_woman housework_saturdays_woman housework_sundays_woman), missing
mi passive: gen housework_weekdays_5_man = housework_weekdays_man * 5
mi passive: egen housework_weekly_est_man = rowtotal(housework_weekdays_5_man housework_saturdays_man housework_sundays_man), missing

mi passive: egen couple_weekly_hw_total = rowtotal(housework_weekly_est_woman housework_weekly_est_man)
mi passive: gen woman_weekly_hw_share = housework_weekly_est_woman / couple_weekly_hw_total // this does have missing I think is couple HW total is 0

sum housework_weekly_est_woman, det
sum housework_weekly_est_woman if housework_weekly_est_woman!=0, det
sum housework_weekly_est_man, det
sum woman_weekly_hw_share, det

mi passive: gen couple_hw_weekly=.
mi passive: replace couple_hw_weekly = 1 if woman_weekly_hw_share==1
mi passive: replace couple_hw_weekly = 2 if woman_weekly_hw_share > 0.60 & woman_weekly_hw_share < 1
mi passive: replace couple_hw_weekly = 3 if woman_weekly_hw_share >= 0.40 & woman_weekly_hw_share <= 0.60
mi passive: replace couple_hw_weekly = 4 if woman_weekly_hw_share < 0.40
mi passive: replace couple_hw_weekly = 3 if housework_weekly_est_woman==0 & housework_weekly_est_man==0  // neither is so small, just put in equal

capture label define couple_hw 1 "Woman All" 2 "Woman Most" 3 "Equal" 4 "Man Most" 5 "Neither HW"
label values couple_hw_weekly couple_hw

mi estimate: proportion couple_hw_weekly

	// Cutpoints: within equal HW
	sum couple_weekly_hw_total, detail
	sum couple_weekly_hw_total if couple_weekly_hw_total!=0, detail
	mi passive: egen hw_weekly_hilow_equal = cut(couple_weekly_hw_total) if couple_hw_weekly==3, group(2) // need to think about if I should keep 0 here as well, because 0 hours of housework also goes in category 3 // couple_weekly_hw_total!=0 -- yeah remove this, because 0s included in the PSID / UKHLS versions
	tab hw_weekly_hilow_equal if couple_hw_weekly==3
	tabstat couple_weekly_hw_total, by(hw_weekly_hilow_equal)
	
	// Cutpoints: within she does most OR all
	sum housework_weekly_est_woman, detail
	sum housework_weekly_est_woman if housework_weekly_est_woman!=0, detail
	mi passive: egen hw_weekly_hilow_woman = cut(housework_weekly_est_woman) if housework_weekly_est_woman!=0 & inlist(couple_hw_weekly,1,2), group(2)
	tab hw_weekly_hilow_woman if inlist(couple_hw_weekly,1,2)
	tabstat housework_weekly_est_woman, by(hw_weekly_hilow_woman)

* Consolidated housework + hours variable
mi passive: gen couple_hw_hrs_weekly=.
mi passive: replace couple_hw_hrs_weekly = 1 if inlist(couple_hw_weekly,1,2) & hw_weekly_hilow_woman==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_weekly = 2 if inlist(couple_hw_weekly,1,2) & hw_weekly_hilow_woman==0 
mi passive: replace couple_hw_hrs_weekly = 3 if couple_hw_weekly==3 & hw_weekly_hilow_equal==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_weekly = 4 if couple_hw_weekly==3 & hw_weekly_hilow_equal==0 
mi passive: replace couple_hw_hrs_weekly = 5 if couple_hw_weekly==4
mi passive: replace couple_hw_hrs_weekly = 4 if housework_weekly_est_woman==0 & housework_weekly_est_man==0  // neither is so small, just put in equal low

capture label define couple_hw_hrs_combo 1 "Woman Most: High" 2 "Woman Most: Low" 3 "Equal: High" 4 "Equal: Low" 5 "Man Most: All"
label values couple_hw_hrs_weekly couple_hw_hrs_combo

mi estimate: proportion couple_hw_weekly couple_hw_hrs_weekly

*****
*3. Now see what happens if I use housework + repairs (just as a test)
mi passive: egen housework_combined_woman = rowtotal(housework_weekdays_woman repair_weekdays_woman), missing
mi passive: egen housework_combined_man = rowtotal(housework_weekdays_man repair_weekdays_man), missing

mi passive: egen couple_combined_hw_total = rowtotal(housework_combined_woman housework_combined_man)
mi passive: gen woman_combined_hw_share = housework_combined_woman / couple_combined_hw_total // this does have missing I think is couple HW total is 0

sum housework_combined_woman, det
sum housework_combined_woman if housework_combined_woman!=0, det
sum housework_combined_man, det
sum woman_combined_hw_share, det

mi passive: gen couple_hw_combined=.
mi passive: replace couple_hw_combined = 1 if woman_combined_hw_share==1
mi passive: replace couple_hw_combined = 2 if woman_combined_hw_share > 0.60 & woman_combined_hw_share < 1
mi passive: replace couple_hw_combined = 3 if woman_combined_hw_share >= 0.40 & woman_combined_hw_share <= 0.60
mi passive: replace couple_hw_combined = 4 if woman_combined_hw_share < 0.40
mi passive: replace couple_hw_combined = 3 if housework_combined_woman==0 & housework_combined_man==0  // neither is so small, just put in equal

capture label define couple_hw 1 "Woman All" 2 "Woman Most" 3 "Equal" 4 "Man Most" 5 "Neither HW"
label values couple_hw_combined couple_hw

mi estimate: proportion couple_hw_combined

	// Cutpoints: within equal HW
	sum couple_combined_hw_total, detail
	sum couple_combined_hw_total if couple_combined_hw_total!=0, detail
	mi passive: egen hw_combined_hilow_equal = cut(couple_combined_hw_total) if couple_hw_combined==3, group(2) // couple_combined_hw_total!=0 
	tab hw_combined_hilow_equal if couple_hw_combined==3
	tabstat couple_combined_hw_total, by(hw_combined_hilow_equal)
	
	// Cutpoints: within she does most OR all
	sum housework_combined_woman, detail
	sum housework_combined_woman if housework_combined_woman!=0, detail
	mi passive: egen hw_combined_hilow_woman = cut(housework_combined_woman) if housework_combined_woman!=0 & inlist(couple_hw_combined,1,2), group(2)
	tab hw_combined_hilow_woman if inlist(couple_hw_combined,1,2)
	tabstat housework_combined_woman, by(hw_combined_hilow_woman)

* Consolidated housework + hours variable
mi passive: gen couple_hw_hrs_combined=.
mi passive: replace couple_hw_hrs_combined = 1 if inlist(couple_hw_combined,1,2) & hw_combined_hilow_woman==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_combined = 2 if inlist(couple_hw_combined,1,2) & hw_combined_hilow_woman==0 
mi passive: replace couple_hw_hrs_combined = 3 if couple_hw_combined==3 & hw_combined_hilow_equal==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_combined = 4 if couple_hw_combined==3 & hw_combined_hilow_equal==0 
mi passive: replace couple_hw_hrs_combined = 5 if couple_hw_combined==4
mi passive: replace couple_hw_hrs_combined = 4 if housework_combined_woman==0 & housework_combined_man==0  // neither is so small, just put in equal low

capture label define couple_hw_hrs_combo 1 "Woman Most: High" 2 "Woman Most: Low" 3 "Equal: High" 4 "Equal: Low" 5 "Man Most: All"
label values couple_hw_hrs_combined couple_hw_hrs_combo

mi estimate: proportion couple_hw_combined couple_hw_hrs_combined

/* I am doing this sort of out of order, but want to pull some basic housework descriptives:
1. Summary measures using my three different definitions
sum housework_weekdays_woman, det
sum housework_weekdays_man, det
sum couple_weekday_hw_total, det
sum woman_weekday_hw_share, det

sum housework_weekly_est_woman, det
sum housework_weekly_est_man, det
sum couple_weekly_hw_total, det
sum woman_weekly_hw_share, det

sum housework_combined_woman, det
sum housework_combined_man, det
sum couple_combined_hw_total, det
sum woman_combined_hw_share, det

// actually this is better duh
tabstat housework_weekdays_woman housework_weekdays_man couple_weekday_hw_total woman_weekday_hw_share ///
housework_weekly_est_woman housework_weekly_est_man couple_weekly_hw_total woman_weekly_hw_share ///
housework_combined_woman housework_combined_man couple_combined_hw_total woman_combined_hw_share ///
, stats(mean min p25 p50 p75 max) columns(statistics)

tabstat housework_saturdays_woman housework_saturdays_man housework_sundays_woman housework_sundays_man ///
, stats(mean min p25 p50 p75 max) columns(statistics)

2. proportions
mi estimate: proportion couple_hw_weekday couple_hw_weekly couple_hw_combined
mi estimate: proportion couple_hw_weekday couple_hw_weekly couple_hw_combined if relative_duration>=0 & relative_duration<=10

3. Histograms at overall level
histogram housework_weekdays_woman, width(1)
histogram housework_weekdays_man, width(1)
histogram couple_weekday_hw_total if couple_weekday_hw_total<=20, width(1)
twoway (histogram housework_weekdays_woman if housework_weekdays_woman<=15, width(1) color(pink%30)) (histogram housework_weekdays_man if housework_weekdays_man<=15, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Core Housework: Weekdays")

histogram housework_weekly_est_woman, width(5)
histogram housework_weekly_est_man, width(5)
histogram couple_weekly_hw_total if couple_weekly_hw_total<=100, width(2)
twoway (histogram housework_weekly_est_woman if housework_weekly_est_woman<=60, width(2) color(pink%30)) (histogram housework_weekly_est_man if housework_weekly_est_man<=60, width(2) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Core Housework: Weekly")

histogram housework_combined_woman, width(1)
histogram housework_combined_man, width(1)
histogram couple_combined_hw_total if couple_combined_hw_total<=20, width(1)
twoway (histogram housework_combined_woman if housework_combined_woman<=15, width(1) color(pink%30)) (histogram housework_combined_man if housework_combined_man<=15, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Core HW + Repairs: Weekdays")

4. Histograms by who does most
histogram housework_weekdays_woman if inlist(couple_hw_weekday,1,2) & housework_weekdays_woman<=15, width(1)
histogram couple_weekday_hw_total if couple_hw_weekday==3 & couple_weekday_hw_total<=15, width(1)

histogram housework_weekly_est_woman if inlist(couple_hw_weekly,1,2) & housework_weekly_est_woman<=80, width(2)
histogram couple_weekly_hw_total if couple_hw_weekly==3 & couple_weekly_hw_total<=80, width(2)

histogram housework_combined_woman if inlist(couple_hw_combined,1,2) & housework_combined_woman<=15, width(1)
histogram couple_combined_hw_total if couple_hw_combined==3 & couple_combined_hw_total<=15, width(1)

*/

**# Bookmark #1
// temp save
save "$created_data/gsoep_couples_imputed_long_recoded.dta", replace

********************
* Family
********************
* relationship type
// wait why did I drop off syear somewhere
tab transition_year ever_transition, m

gen syear = eligible_rel_start_year + relative_duration // okay this matches other file though, so it's fine
browse pid eligible_partner relative_duration syear marst_imp eligible_rel_start_year ever_transition transition_year min_dur max_dur

mi passive: gen dur_transitioned=.
mi passive: replace dur_transitioned = transition_year - eligible_rel_start_year if ever_transition==1

tab dur_transitioned ever_transition, m

// browse pid eligible_partner syear relative_duration marst_imp ever_transition transition_year dur_transitioned eligible_rel_start_year eligible_rel_end_year eligible_rel_status if ever_transition == 0 & dur_transitioned!=. // these are people I mistakenly captured before / after the start of their relationship. have adjusted code above so that only get a duration transiton if ever_transition == 1 (those are proper)
browse pid eligible_partner syear relative_duration marst_defacto marst_imp ever_transition transition_year dur_transitioned eligible_rel_start_year eligible_rel_end_year eligible_rel_status

tab marst_defacto if syear >= transition_year & syear < = eligible_rel_end_year & imputed==0, m
tab marst_imp if syear >= transition_year & syear < = eligible_rel_end_year, m // so generally already fine
tab marst_imp if relative_duration >= dur_transitioned & relative_duration < = max_dur, m // so generally already fine
tab marst_imp if syear < transition_year & syear > = eligible_rel_start_year & ever_transition==1, m 
tab marst_imp if duration < dur_transitioned & relative_duration >= min_dur & ever_transition==1, m 

// browse pid eligible_partner marst_defacto marst_imp syear relative_duration min_dur max_dur eligible_rel_start_year eligible_rel_end_year eligible_rel_status
tab marst_imp if relative_duration > min_dur & relative_duration < max_dur , m
tab marst_imp if relative_duration >= min_dur & relative_duration <= max_dur , m

tab marst_defacto if relative_duration==min_dur, m
gen rel_type_min_dur = marst_defacto if relative_duration==min_dur
	// unique pid eligible_partner if rel_type_min_dur!=.
	// unique pid eligible_partner rel_type_min_dur if rel_type_min_dur!=.
bysort pid eligible_partner: egen master_rel_type = max(rel_type_min_dur)
label values master_rel_type marst_defacto
tab master_rel_type, m
tab master_rel_type if ever_transition==1

sort pid eligible_partner  _mi_m relative_duration

	// tab marst_imp if ever_transition==0 & relative_duration >= min_dur & relative_duration <= max_dur
	// tab master_rel_type if ever_transition==0 & relative_duration >= min_dur & relative_duration <= max_dur
	// tab master_rel_type marst_imp if ever_transition==0 & relative_duration >= min_dur & relative_duration <= max_dur
	// browse pid eligible_partner _mi_m relative_duration ever_transition master_rel_type marst_imp marst_defacto min_dur max_dur
	
mi passive: gen rel_type=.
mi passive: replace rel_type = 0 if inlist(relative_duration,-2,-1) // pre-rel
mi passive: replace rel_type = 1 if ever_transition==0 & marst_imp==1 & relative_duration >= min_dur & relative_duration <= max_dur // married, never transitioned
mi passive: replace rel_type = 1 if ever_transition==1 & relative_duration >= dur_transitioned & relative_duration <= max_dur // married, post transition
// mi passive: replace rel_type = 1 if rel_type==. & ever_transition==0 & relative_duration==0 & marst_imp==1
mi passive: replace rel_type = 1 if rel_type==. & ever_transition==0 & master_rel_type==1 & relative_duration >= min_dur & relative_duration <= max_dur // never transitioned, rel type at start==married
mi passive: replace rel_type = 1 if rel_type==. & ever_transition==0 & relative_duration==0 & master_rel_type==1 // people whose rel started mid-way through yr 0 always cause problems..
mi passive: replace rel_type = 2 if ever_transition==0 & marst_imp==2 & relative_duration >= min_dur & relative_duration <= max_dur // partnered, never transitioned
mi passive: replace rel_type = 2 if ever_transition==1 & relative_duration < dur_transitioned & relative_duration >= min_dur // pre transition
mi passive: replace rel_type = 2 if ever_transition==1 & rel_type==. & relative_duration==0 // if transitioned, had to be cohab at start
// mi passive: replace rel_type = 2 if rel_type==. & relative_duration==0 & marst_imp==2
mi passive: replace rel_type = 2 if rel_type==. & ever_transition==0 & master_rel_type==2 & relative_duration >= min_dur & relative_duration <= max_dur // never transitioned, rel type at start==partnered
mi passive: replace rel_type = 2 if rel_type==. & relative_duration==0 & master_rel_type==2
mi passive: replace rel_type = 3 if relative_duration > max_dur & eligible_rel_status==0 // intact but past end of relationship
mi passive: replace rel_type = 3 if relative_duration > max_dur & eligible_rel_status==. // estimated attrition - doesn't actually matter if we know how it ended, because I am dropping anyway post end of rel
mi passive: replace rel_type = 4 if relative_duration > max_dur & inlist(eligible_rel_status,1,2,3) // past end of relationship and designated ended - including widowhood here for these purposes - bc it is an observed end...

tab rel_type imputed, m
tab rel_type if _mi_m!=0, m
	// tab rel_type if relative_duration >= min_dur & relative_duration <= max_dur, m
	// tab rel_type if relative_duration < min_dur, m
	// tab rel_type if relative_duration > max_dur, m
	// tab rel_type if ever_transition==1 & relative_duration >= dur_transitioned, m
	// tab rel_type if ever_transition==1 & relative_duration < dur_transitioned, m

// browse pid eligible_partner _mi_m relative_duration ever_transition dur_transitioned rel_type master_rel_type marst_imp marst_defacto min_dur max_dur

label define rel_type 0 "Pre-Relationship" 1 "Married" 2 "Cohab" 3 "Attrited" 4 "Broke Up"
label values rel_type rel_type

tab rel_type, m
mi estimate: proportion rel_type

tab rel_type eligible_rel_status, row
tab rel_type relative_duration if _mi_m!=0, m

* number of children
tab num_children_woman num_children_man if inlist(rel_type,1,2) & relative_duration>=0, m
mi passive: egen couple_num_children = rowmax(num_children_woman num_children_man)
tab couple_num_children, m

// okay, decided we will use women's number of children
mi passive: gen couple_num_children_gp=.
mi passive: replace couple_num_children_gp = 0 if num_children_woman==0
mi passive: replace couple_num_children_gp = 1 if num_children_woman==1
mi passive: replace couple_num_children_gp = 2 if num_children_woman==2
mi passive: replace couple_num_children_gp = 3 if num_children_woman>=3 & num_children_woman < 15

tab num_children_woman couple_num_children_gp

mi estimate: proportion couple_num_children_gp

tab rel_type couple_num_children_gp

* combined
mi passive: gen family_type=.
mi passive: replace family_type=0 if inlist(rel_type,0,3,4)
mi passive: replace family_type=1 if rel_type==1 & couple_num_children_gp==0
mi passive: replace family_type=2 if rel_type==1 & couple_num_children_gp==1
mi passive: replace family_type=3 if rel_type==1 & couple_num_children_gp==2
mi passive: replace family_type=4 if rel_type==1 & couple_num_children_gp==3
mi passive: replace family_type=5 if rel_type==2 & couple_num_children_gp==0
mi passive: replace family_type=6 if rel_type==2 & couple_num_children_gp==1
mi passive: replace family_type=7 if rel_type==2 & couple_num_children_gp==2
mi passive: replace family_type=8 if rel_type==2 & couple_num_children_gp==3

label define family_type 0 "Not together" 1 "Married, 0 Ch" 2 "Married, 1 Ch" 3 "Married, 2 Ch" 4 "Married, 3+ Ch" ///
						5 "Cohab, 0 Ch" 6 "Cohab, 1 Ch" 7 "Cohab, 2 Ch" 8 "Cohab, 3+ Ch"
label values family_type family_type

mi estimate: proportion family_type

tab family_type rel_type
tab family_type relative_duration
tab family_type relative_duration if _mi_m!=0, m

browse pid eligible_partner _mi_m relative_duration dur_transitioned eligible_rel_start_year eligible_rel_end_year min_dur max_dur family_type rel_type marst_imp couple_num_children_gp eligible_rel_status 

**# Bookmark #1
// temp save
save "$created_data/gsoep_couples_imputed_long_recoded.dta", replace

********************
* Create truncated versions
********************
// use "$created_data/gsoep_couples_imputed_long_recoded.dta", clear
// check
inspect woman_weekday_hw_share if couple_weekday_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect woman_weekly_hw_share if couple_weekly_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect woman_combined_hw_share if couple_combined_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below

inspect hw_weekday_hilow_woman if housework_weekdays_woman == 0 & imputed==1 // I only did for women with hW hours. so these missings also make sense
inspect hw_weekly_hilow_woman if housework_weekly_est_woman == 0 & imputed==1 // I only did for women with hW hours. so these missings also make sense
inspect hw_combined_hilow_woman if housework_combined_woman == 0 & imputed==1 // I only did for women with hW hours. so these missings also make sense

foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow_detailed couple_work_ow couple_hw_weekday couple_hw_hrs_weekday couple_hw_weekly couple_hw_hrs_weekly couple_hw_combined couple_hw_hrs_combined couple_num_children_gp rel_type family_type{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

// designate that relationship dissolved and create versions of all variables that stop at this point
foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow_detailed couple_work_ow couple_hw_weekday couple_hw_hrs_weekday couple_hw_weekly couple_hw_hrs_weekly couple_hw_combined couple_hw_hrs_combined couple_num_children_gp family_type{
	capture drop `var'_end
	mi update
	mi passive: gen `var'_end = `var'
	mi passive: replace `var'_end = 98 if rel_type==4 // dissolve
	mi passive: replace `var'_end = 99 if rel_type==3 // attrit
}

// save "$created_data/gsoep_couples_imputed_long_recoded.dta", replace

foreach var in ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_weekday_end couple_hw_hrs_weekday_end couple_hw_weekly_end couple_hw_hrs_weekly_end couple_hw_combined_end couple_hw_hrs_combined_end couple_num_children_gp_end family_type_end{
	assert `var' !=. if _mi_m!=0
}

label values ft_pt_man_end ft_pt_woman_end ft_pt
label values couple_work_end couple_work
label values couple_work_ow_detailed_end couple_work_ow_detailed
label values couple_work_ow_end couple_work_ow
label values couple_hw_weekday_end couple_hw
label values couple_hw_hrs_weekday_end couple_hw_hrs_combo
label values couple_hw_weekly_end couple_hw
label values couple_hw_hrs_weekly_end couple_hw_hrs_combo
label values couple_hw_combined_end couple_hw
label values couple_hw_hrs_combined_end couple_hw_hrs_combo
label values family_type_end family_type

// final update and save

mi update

save "$created_data/gsoep_couples_imputed_long_recoded.dta", replace

// mi estimate: proportion couple_hw_hrs_end couple_hw_end if duration >=0 & duration <=10

********************************************************************************
**# Deduplicate
********************************************************************************
drop if relative_duration < 0 | relative_duration > 10
drop duration

mi update

// need to get rid of one record per couple; currently duplicated - I *think* there is where my handy dandy couple id can come in? or can just do by sex like I did UKHLS. should I do this *again*? because the per id has ties that don't make sense...
unique pid eligible_partner
unique eligible_couple_id
inspect eligible_couple_id

// alternatively - do I like not even need to create this and just keep who is partner 1?
gen long partner_1 = cond(pid < eligible_partner, pid, eligible_partner) // omg was whole problem here this whole time - needed to do LONG because some getting truncated? I am dumb...
gen long partner_2 = cond(pid < eligible_partner, eligible_partner, pid)
egen long couple_id = group(partner_1 partner_2)
unique couple_id

browse couple_id pid eligible_partner relative_duration partner_1  partner_2 eligible_couple_id

unique pid eligible_partner, by(SEX)
unique couple_id, by(SEX)

tab SEX SEX_sp, m

bysort couple_id relative_duration _mi_m : egen per_id = rank(pid)
tab per_id, m

sort pid eligible_partner imputed _mi_m relative_duration
// browse pid eligible_partner eligible_couple_id SEX relative_duration per_id
// browse pid eligible_partner eligible_couple_id SEX relative_duration per_id if !inlist(per_id,1,2)
// browse couple_id pid eligible_partner relative_duration partner_1  partner_2 eligible_couple_id if !inlist(per_id,1,2)

// does it matter which we use? sex or per id? no they literally all match
tab couple_work if imputed==1
tab couple_work per_id if imputed==1, col
tab couple_work SEX if imputed==1, col

tab couple_hw_hrs_weekday if imputed==1
tab couple_hw_hrs_weekday per_id if imputed==1, col
tab couple_hw_hrs_weekday SEX if imputed==1, col

tab family_type if imputed==1
tab family_type per_id if imputed==1, col
tab family_type SEX if imputed==1, col

tab per_id SEX // I guess let's just keep women? that's what I do for UK (but PSID does per id) the outcomes are all the same

// keep if per_id==1
unique pid eligible_partner // 20856
unique couple_id // 10428
keep if SEX==2
unique pid eligible_partner // 10428

// need to do age restrictions (18-60)
// keep if age_all>=18 & age_all<=60 // wait, if I drop these now, won't be rectangular anymore...
gen age_flag = 0
replace age_flag = 1 if (age>=18 & age<=60) & (age_sp>=18 & age_sp<=60) 

bysort pid eligible_partner _mi_m: egen age_eligible=total(age_flag)
tab age_eligible, m // 0 means never within age range

sort pid eligible_partner imputed _mi_m relative_duration
drop if age_eligible==0

mi update

unique pid eligible_partner // now 9776

save "$created_data/gsoep_couples_imputed_long_deduped.dta", replace

********************************************************************************
**# Quick descriptives for full sample while long
********************************************************************************
tab couple_work_ow_end imputed, col
tab couple_work_end imputed, col

// descriptives at all durations
desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_man_end i.overwork_man_end i.couple_work_end i.couple_work_ow_end i.couple_work_ow_detailed_end i.couple_hw_weekday_end i.couple_hw_hrs_weekday_end i.couple_hw_weekly_end i.couple_hw_hrs_weekly_end i.couple_hw_combined_end i.couple_hw_hrs_combined_end i.rel_type i.couple_num_children_gp_end i.family_type_end, filename("$results/gsoep_mi_desc") stats(mimean)
// desctable i.ft_pt_woman i.overwork_woman i.ft_pt_man i.overwork_man i.couple_work i.couple_work_ow i.couple_hw i.couple_hw_hrs i.rel_type i.couple_num_children_gp i.family_type, filename("$results/mi_desc_all") stats(mimean)  // modify - okay can't use modify but want to see if this replaces the previous or adds a new sheet. okay it replaces the previous oops

mi estimate: proportion couple_work_ow_end couple_hw_weekday_end family_type_end // validate that this matches. it does

// should I just loop through durations while long? should I confirm the numbers are the same either way? - so here, try to loop through durations
** Note 8/6/25: have not done this yet** (takes a while - possibly send to HPC - but also not actually sure needed because this is what we can get in R much more easily (and is really the whole point of the sequences...)
/* Commenting out for now so not accidentally run and slow things down
forvalues d=0/10{
	desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_man_end i.overwork_man_end i.couple_work_end i.couple_work_ow_end i.couple_work_ow_detailed_end i.couple_hw_weekday_end i.couple_hw_hrs_weekday_end i.couple_hw_weekly_end i.couple_hw_hrs_weekly_end i.couple_hw_combined_end i.couple_hw_hrs_combined_end i.rel_type i.couple_num_children_gp_end i.family_type_end if duration==`d', filename("$results/gsoep_mi_desc_`d'") stats(mimean) decimals(4)
}

mi estimate: proportion couple_work_ow_end couple_hw_weekday_end family_type_end if duration==0
mi estimate: proportion couple_work_ow_end couple_hw_weekday_end family_type_end if duration==5

/* oh, wait, I think I can actually just group by duration?? ah, no you cannot do that with mi. i knew this
desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_man_end i.overwork_man_end i.couple_work_end i.couple_work_ow_end i.couple_hw_end i.couple_hw_hrs_end i.rel_type i.couple_num_children_gp_end i.family_type_end, filename("$results/mi_desc_dur") stats(mimean) group(duration)
*/
*/

// I think duration needs to start at 1 to work in r
gen duration = relative_duration + 1

local regular_vars : char _dta[_mi_rvars]
display "`regular_vars'"
local imputed_vars : char _dta[_mi_ivars]
display "`imputed_vars'"
local passive_vars : char _dta[_mi_pvars]
display "`passive_vars'"

drop eligible_couple_id couple_id_unique where_germany_pl survey_status_pl status_pl disability_amount emplst_pg isced97_pg yrs_educ_pg nationality_pb survey_status_pb live_fam_bp region_type edu4 home_owner marst_defacto partnered_total religion_est employed_binary hh_net_income_t_cnef num_parent_in_hh fillin in_rel_year partnered_imp ever_int where_born_ew where_1989_ew who_lived_with last_birth_year num_elig_partners retired_year1 retired_first_obs nmis_age nmis_parent nmis_status imputed relative_duration where_germany_pl_sp survey_status_pl_sp status_pl_sp disability_amount_sp emplst_pg_sp isced97_pg_sp yrs_educ_pg_sp survey_status_pb_sp live_fam_bp_sp edu4_sp home_owner_sp marst_defacto_sp partnered_total_sp religion_est_sp employed_binary_sp hh_net_income_t_cnef_sp num_parent_in_hh_sp in_rel_year_sp partnered_imp_sp ever_transition_sp transition_year_sp ever_int_sp where_born_ew_sp where_1989_ew_sp who_lived_with_sp last_birth_year_sp rowcount rel_type_min_dur partner_1 partner_2 _Unique per_id age_flag age_eligible

mi update

********************************************************************************
**# Reshape back to wide to see the data by duration and compare to long estimates
********************************************************************************

mi reshape wide employment self_reported_health disability_yn religious_affiliation errands_sundays housework_saturdays housework_sundays childcare_saturdays childcare_sundays repair_saturdays errands_weekdays housework_weekdays childcare_weekdays repair_weekdays errands_saturdays aid_in_hh_hl kidsu18_hh num_65up_hh age_youngest_child age nationality_region federal_state housing_status weekly_work_hrs gross_income_lm net_income_lm hh_income_net_monthly earnings_gross_t_cnef hh_gross_income_t_cnef repair_sundays any_outside_help any_parent_in_hh current_parent_status marst_imp retired_yn full_status_pl duplicate_record urban_region employment_sp self_reported_health_sp disability_yn_sp religious_affiliation_sp errands_sundays_sp housework_saturdays_sp housework_sundays_sp childcare_saturdays_sp childcare_sundays_sp repair_saturdays_sp errands_weekdays_sp housework_weekdays_sp childcare_weekdays_sp repair_weekdays_sp errands_saturdays_sp nationality_pb_sp aid_in_hh_hl_sp kidsu18_hh_sp num_65up_hh_sp age_youngest_child_sp age_sp nationality_region_sp federal_state_sp housing_status_sp weekly_work_hrs_sp gross_income_lm_sp net_income_lm_sp hh_income_net_monthly_sp earnings_gross_t_cnef_sp hh_gross_income_t_cnef_sp repair_sundays_sp any_outside_help_sp any_parent_in_hh_sp current_parent_status_sp marst_imp_sp retired_yn_sp full_status_pl_sp urban_region_sp weekly_hrs_woman weekly_hrs_man employment_status_woman employment_status_man monthly_earnings_woman monthly_earnings_man annual_earnings_woman annual_earnings_man housework_weekdays_woman housework_weekdays_man housework_saturdays_woman housework_saturdays_man housework_sundays_woman housework_sundays_man repair_weekdays_woman repair_weekdays_man repair_saturdays_woman repair_saturdays_man repair_sundays_woman repair_sundays_man errands_weekdays_woman errands_weekdays_man errands_saturdays_woman errands_saturdays_man errands_sundays_woman errands_sundays_man aid_in_hh_woman aid_in_hh_man marital_status_woman marital_status_man partnered_woman partnered_man num_children_woman num_children_man age_youngest_woman age_youngest_man federal_state_woman federal_state_man where_ew_woman where_ew_man urban_region_woman urban_region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow_detailed couple_work_ow couple_weekday_hw_total woman_weekday_hw_share couple_hw_weekday housework_weekdays_5_woman housework_weekly_est_woman housework_weekdays_5_man housework_weekly_est_man couple_weekly_hw_total woman_weekly_hw_share couple_hw_weekly housework_combined_woman housework_combined_man couple_combined_hw_total woman_combined_hw_share couple_hw_combined syear rel_type couple_num_children couple_num_children_gp family_type hw_weekly_hilow_woman hw_weekly_hilow_equal couple_hw_hrs_weekly hw_combined_hilow_equal hw_combined_hilow_woman couple_hw_hrs_combined hw_weekday_hilow_woman hw_weekday_hilow_equal hw_weekday_hilow_test hw_weekday_equal couple_hw_hrs_weekday ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_weekday_end couple_hw_hrs_weekday_end couple_hw_weekly_end couple_hw_hrs_weekly_end couple_hw_combined_end couple_hw_hrs_combined_end couple_num_children_gp_end family_type_end  ///
, i(pid eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status) j(duration)

tab _mi_miss, m // see what happens if I reshape but DON'T convert
tab _mi_m, m

browse pid eligible_partner max_dur _mi_m couple_work_end* couple_work_ow_end* couple_hw_weekday_end*
browse pid eligible_partner max_dur _mi_m couple_work_end* couple_work_ow_end* couple_hw_weekday_end* if inrange(_mi_m,1,10)

unique pid eligible_partner // so now there are 9776 uniques and 11 observations for each (base + 10 imputations) - so 107536 observations
unique pid eligible_partner, by(_mi_m)

// create indicator of complete sequences
gen complete_seq = .
replace complete_seq = 0 if max_dur < 9
replace complete_seq = 1 if max_dur >= 9 // so 9 is really 10 (because 0-9 = 1-10) // I feel like the # of complete sequences here is much lower than in other countries?

tab rel_type10 if complete_seq==1, m
browse pid eligible_partner min_dur max_dur complete_seq rel_type* // okay, so max_dur is based on start of 0, but I changed it, so now need it to be +1

// then create indicator of length of complete sequence
gen sequence_length_alt=max_dur + 1
replace sequence_length_alt = 10 if sequence_length_alt > 10

tab sequence_length_alt complete_seq, m

gen sequence_length = .
forvalues d=1/10{
	replace sequence_length = `d' if sequence_length==. & inlist(rel_type`d',3,4)
}

replace sequence_length = sequence_length - 1 

tab sequence_length complete_seq, m
tab sequence_length sequence_length_alt, m // these match
replace sequence_length = 10 if complete_seq==1

// note how ended (attrit or dissolve)
tab eligible_rel_status complete_seq, m
browse pid eligible_partner min_dur max_dur sequence_length eligible_rel_status complete_seq rel_type*

gen status_end = .
forvalues d=1/10{
	local e = `d' + 1
	replace status_end = rel_type`e' if sequence_length == `d'
}

label values status_end rel_type
tab status_end complete_seq, m

browse pid eligible_partner  min_dur max_dur sequence_length status_end eligible_rel_status rel_type* couple_work_end* if complete_seq==0 // & inlist(status_end,1,2)

gen how_end=.
replace how_end = 0 if complete_seq==1 // intact
replace how_end = 1 if complete_seq==0 & status_end==4 // dissolved
replace how_end = 2 if complete_seq==0 & status_end==3 // attrit

label define how_end_seq 0 "Intact" 1 "Dissolved" 2 "Attrited"
label values how_end how_end_seq

tab how_end complete_seq, m

mi update

save "$created_data/gsoep_couples_imputed_wide.dta", replace 

********************************************************************************
* A few small additional extracts needed
********************************************************************************
// just complete sequences
use "$created_data/gsoep_couples_imputed_wide.dta", clear

keep if complete_seq==1

tab couple_hw_weekday_end10 if _mi_m!=0, m
tab couple_work_ow_end10 if _mi_m!=0, m
tab family_type_end10 if _mi_m!=0, m

mi update

unique couple_id // 2372

save "$created_data/gsoep_couples_imputed_wide_complete.dta", replace 

**# THIS IS OUR MAIN ANALYSIS FILE
// truncated data (so not attrit or dissolve - set to missing instead)
use "$created_data/gsoep_couples_imputed_wide.dta", clear

browse pid eligible_partner complete_seq sequence_length couple_work_ow_end* couple_hw_hrs_weekday_end* family_type_end*
fre couple_work_ow_end5
fre couple_hw_hrs_weekday_end
fre family_type_end5

tab couple_hw_hrs_weekday_end1 couple_hw_hrs_weekly_end1

forvalues d=1/11{
	capture gen couple_work_ow_trunc`d' = couple_work_ow_end`d'
	replace couple_work_ow_trunc`d' = . if inlist(couple_work_ow_end`d',98,99)
	label values couple_work_ow_trunc`d' couple_work_ow
	
	capture gen couple_hw_hrs_weekday_trunc`d' = couple_hw_hrs_weekday_end`d'
	replace couple_hw_hrs_weekday_trunc`d' = . if inlist(couple_hw_hrs_weekday_end`d',98,99)
	label values couple_hw_hrs_weekday_trunc`d' couple_hw_hrs_combo
	
	capture gen couple_hw_hrs_weekly_trunc`d' = couple_hw_hrs_weekly_end`d'
	replace couple_hw_hrs_weekly_trunc`d' = . if inlist(couple_hw_hrs_weekly_end`d',98,99)
	label values couple_hw_hrs_weekly_trunc`d' couple_hw_hrs_combo
	
	capture gen couple_hw_hrs_combined_trunc`d' = couple_hw_hrs_combined_end`d'
	replace couple_hw_hrs_combined_trunc`d' = . if inlist(couple_hw_hrs_combined_end`d',98,99)
	label values couple_hw_hrs_combined_trunc`d' couple_hw_hrs_combo
	
	capture gen family_type_trunc`d' = family_type_end`d'
	replace family_type_trunc`d' = . if inlist(family_type_end`d',98,99)
	label values family_type_trunc`d' family_type
}

fre couple_work_ow_trunc5
fre couple_hw_hrs_weekday_trunc5
fre family_type_trunc5

// should not have any missing at time 1
tab couple_work_ow_trunc1 if _mi_m!=0, m
tab couple_hw_hrs_weekday_trunc1 if _mi_m!=0, m
tab couple_hw_hrs_weekly_trunc1 if _mi_m!=0, m
tab couple_hw_hrs_combined_trunc1 if _mi_m!=0, m
tab family_type_trunc1 if _mi_m!=0, m

unique pid eligible_partner

// browse pid eligible_partner complete_seq sequence_length couple_work_ow_trunc* couple_hw_hrs_weekday_trunc* family_type_trunc* if _mi_m!=0

// we are dropping those in the refugee over-samples. I am going to drop those here (but not from main file above jic) because feels easier
// other sample things (sequence length >=3) happen in R

gen sample_type = .
replace sample_type = 1 if inrange(psample_pl,1,14)
replace sample_type = 1 if inrange(psample_pl,20,23)
replace sample_type = 2 if inlist(psample_pl,15,16,25,26)
replace sample_type = 3 if inlist(psample_pl,17,18,19,24)

gen sample_type_sp = .
replace sample_type_sp = 1 if inrange(psample_pl_sp,1,14)
replace sample_type_sp = 1 if inrange(psample_pl_sp,20,23)
replace sample_type_sp = 2 if inlist(psample_pl_sp,15,16,25,26)
replace sample_type_sp = 3 if inlist(psample_pl_sp,17,18,19,24)

label define sample_type 1 "core" 2 "migrant" 3 "refugee"
label values sample_type sample_type_sp sample_type

tab sample_type, m
// oh - are refugees typically partnered to each other? Like prob drop if either is a refugee?
tab sample_type sample_type_sp, m // wait - does it make sense it's perfectly matched? is this because it's based on HH? so if in same HH, in same sample? I guess this means that SOEP respondents never marry other SOEP respondents (that is what this implies - that the partner is ONLY a SOEP respondent as a result of being a partner...)
// browse pid eligible_partner sample_type sample_type_sp psample_pl psample_pl_sp full_status_pl1 full_status_pl_sp1

tab born_germany_woman born_germany_man, m // because this isn't congruous
tab born_germany_woman born_germany_man if sample_type!=3, m 
tab born_germany_woman born_germany_man if sample_type==3, m // but then basically 100% overlap in both not being born in Germany. Okay, I think this is fine.

drop if sample_type==3 | sample_type_sp==3

save "$created_data/gsoep_couples_wide_truncated.dta", replace 

********************************************************************************
**# Troubleshooting / QA area
********************************************************************************
use "$created_data/gsoep_couples_imputed_long_deduped.dta", clear

// Underwork - is this real? and is it because of non-employment or PT employment?
tab couple_work imputed, col // higher among imputed, but that isn't that surprising because imputed always have more people with no hours (across surveys)
tab couple_work_end imputed, col 
tab couple_work rel_type, col // there is more attrition in the GSOEP - is this why?
// so underwork is quite prevalent among the attrited, but no more than among the married...and actually, since we're ignoring attrition in the clusters, this shouldn't be driven by this...
tab psample_pl couple_work, row // is any of this due to NOT WEIGHTING the data? like - could some of this be driven by oversamples of some populations??
tab born_germany_woman couple_work, row // okay - this is very much driven by migration samples. who...may actually be overrepresented?
tab born_germany_man couple_work, row
tab couple_work_end born_germany_woman , col // is this also why attrition so high? I think also yes
tab born_germany_woman rel_type, row
tab where_ew_woman born_germany_woman, row
tab syear born_germany_woman, row // okay yeah drops in 2013 (some migrants added), then again in 2016...by end, really high proportion of those born outside of Germany...
// this is from panel data.org BUT the caveat is that this covers all years of the SOEP, so will be biased towards less migrants, since we restrict to later years
// display 321547 / (321547 + 1124719) // 22.2%
 
tabstat weekly_hrs_woman weekly_hrs_man, by(couple_work) stats(mean sd p50 min max) columns(statistics)
tabstat weekly_hrs_woman weekly_hrs_man, by(rel_type) stats(mean sd p50 min max) columns(statistics) // trying to also compare basic statistics to prior research as well
tab ft_pt_woman ft_pt_man if couple_work==5, cell // so 70% are both not working, only <6% are both PT
tabstat age age_sp, by(couple_work) stats(mean p25 p50 p75 min max) // so they are older, but not wildly so
tabstat age_youngest_woman age_youngest_man num_children_woman, by(couple_work) stats(mean p50 min max) // they have the most # of children - but seems crazy that they would both not work...
twoway (histogram weekly_hrs_woman if ft_pt_woman==1, width(1) color(pink%30)) (histogram weekly_hrs_man if ft_pt_man==1, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours among PT Workers")

twoway (histogram weekly_hrs_woman if weekly_hrs_woman<100, width(1) color(pink%30)) (histogram weekly_hrs_man if weekly_hrs_man<100, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours") // Does this feel like too many 0s for men?

twoway (histogram weekly_hrs_woman if couple_work==5, width(1) color(pink%30)) (histogram weekly_hrs_man if couple_work==5, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours among those in Underwork Group") // yes - dominated by those not working AND both not working

// browse pid eligible_partner relative_duration couple_work ft_pt_woman ft_pt_man weekly_hrs_woman weekly_hrs_man _mi_m

// Let's compare all created variables
tab ft_pt_man imputed, col
tab ft_pt_woman imputed, col
tab couple_work imputed, col
tab couple_work_ow imputed, col
tab couple_hw_hrs_weekday imputed, col // this is interesting - when imputed, way less woman most high and way more woman most low...this is maybe because the weekday is so volatile because such low numbers of hours?
tab couple_hw_hrs_weekly imputed, col // here there are less women most high, but only like 5% less (more like 20% above), so this validates my feelings about the housework measure
tab couple_hw_hrs_combined imputed, col
