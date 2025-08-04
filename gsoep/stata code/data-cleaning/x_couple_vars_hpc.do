
set maxvar 10000

cd "/home/kmcerlea/stage/Life Course"

/*
use "created data/stata/gsoep_couples_imputed_long.dta", clear

********************************************************************************
**# Create variables
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

// for reference, in HPC, took 2 hrs and 50 minutes to get here - so we're looking at like another 5-6 hours...

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
save "created data/stata/gsoep_couples_imputed_long_recoded.dta", replace

// Stata assert command to check new variables created from imputed  
foreach var in weekly_hrs_woman weekly_hrs_man employment_status_woman employment_status_man monthly_earnings_woman monthly_earnings_man annual_earnings_woman annual_earnings_man housework_weekdays_woman housework_saturdays_woman housework_sundays_woman repair_weekdays_woman repair_saturdays_woman repair_sundays_woman errands_weekdays_woman errands_saturdays_woman errands_sundays_woman housework_weekdays_man housework_saturdays_man housework_sundays_man repair_weekdays_man repair_saturdays_man repair_sundays_man errands_weekdays_man errands_saturdays_man errands_sundays_man aid_in_hh_woman aid_in_hh_man marital_status_woman marital_status_man partnered_woman partnered_man rel_no_woman rel_no_man num_children_woman num_children_man age_youngest_woman age_youngest_man edu4_fixed_woman edu4_fixed_man federal_state_woman federal_state_man where_ew_woman where_ew_man urban_region_woman urban_region_man housing_woman housing_man religion_woman religion_man disabled_woman disabled_man sr_health_woman sr_health_man retired_woman retired_man born_germany_woman born_germany_man global_region_born_woman global_region_born_man federal_state_born_woman federal_state_born_man ew_born_woman ew_born_man father_educ_woman father_educ_man mother_educ_woman mother_educ_man{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 
*/

use "created data/stata/gsoep_couples_imputed_long_recoded.dta", clear
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

// histogram weekly_hrs_woman if ft_pt_woman==1 
sum weekly_hrs_woman if ft_pt_woman==1, det
// histogram weekly_hrs_man if ft_pt_man==1 
sum weekly_hrs_man if ft_pt_man==1, det

// twoway (histogram weekly_hrs_woman if ft_pt_woman==1, width(1) color(pink%30)) (histogram weekly_hrs_man if ft_pt_man==1, width(1) color(blue%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Average Work Hours among PT Workers")

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
	mi passive: egen hw_weekday_hilow_equal = cut(couple_weekday_hw_total) if couple_weekday_hw_total!=0 & couple_hw_weekday==3, group(2)
	tab hw_weekday_hilow_equal if couple_hw_weekday==3
	tabstat couple_weekday_hw_total, by(hw_weekday_hilow_equal)
	
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
mi passive: replace couple_hw_hrs_weekday = 3 if couple_hw_weekday==3 & hw_weekday_hilow_equal==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_weekday = 4 if couple_hw_weekday==3 & hw_weekday_hilow_equal==0 
mi passive: replace couple_hw_hrs_weekday = 5 if couple_hw_weekday==4
mi passive: replace couple_hw_hrs_weekday = 4 if housework_weekdays_woman==0 & housework_weekdays_man==0  // neither is so small, just put in equal low

label define couple_hw_hrs_combo 1 "Woman Most: High" 2 "Woman Most: Low" 3 "Equal: High" 4 "Equal: Low" 5 "Man Most: All"
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
mi passive: gen woman_weekly_hw_share = housework_weekly_est_man / couple_weekly_hw_total // this does have missing I think is couple HW total is 0

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
	mi passive: egen hw_weekly_hilow_equal = cut(couple_weekly_hw_total) if couple_weekly_hw_total!=0 & couple_hw_weekly==3, group(2)
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
	mi passive: egen hw_combined_hilow_equal = cut(couple_combined_hw_total) if couple_combined_hw_total!=0 & couple_hw_combined==3, group(2)
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

**# Bookmark #1
// temp save
save "created data/stata/gsoep_couples_imputed_long_recoded.dta", replace

********************
* Family
********************
* relationship type
// wait why did I drop off syear somewhere
tab transition_year ever_transition, m

gen syear = eligible_rel_start_year + relative_duration // okay this matches other file though, so it's fine
// browse pid eligible_partner relative_duration syear marst_imp eligible_rel_start_year ever_transition transition_year min_dur max_dur

mi passive: gen dur_transitioned=.
mi passive: replace dur_transitioned = transition_year - eligible_rel_start_year if ever_transition==1

tab dur_transitioned ever_transition, m

// browse pid eligible_partner syear relative_duration marst_imp ever_transition transition_year dur_transitioned eligible_rel_start_year eligible_rel_end_year eligible_rel_status if ever_transition == 0 & dur_transitioned!=. // these are people I mistakenly captured before / after the start of their relationship. have adjusted code above so that only get a duration transiton if ever_transition == 1 (those are proper)
// browse pid eligible_partner syear relative_duration marst_defacto marst_imp ever_transition transition_year dur_transitioned eligible_rel_start_year eligible_rel_end_year eligible_rel_status

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

// browse pid eligible_partner _mi_m relative_duration dur_transitioned eligible_rel_start_year eligible_rel_end_year min_dur max_dur family_type rel_type marst_imp couple_num_children_gp eligible_rel_status 

**# Bookmark #1
// temp save
save "created data/stata/gsoep_couples_imputed_long_recoded.dta", replace

********************
* Create truncated versions
********************
// check
inspect woman_weekday_hw_share if couple_weekday_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect woman_weekly_hw_share if couple_weekday_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect woman_combined_hw_share if couple_weekday_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below

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

save "created data/stata/gsoep_couples_imputed_long_recoded.dta", replace

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

save "created data/stata/gsoep_couples_imputed_long_recoded.dta", replace
