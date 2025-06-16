
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: imputation_for_individs
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files uses the wide data and examines multiple methods of imputing missing data

********************************************************************************
* Stata MI Impute
********************************************************************************
use "$created_data/individs_by_duration_wide.dta", clear
capture label define rel_status 1 "Intact" 3 "Widow" 4 "Divorce" 5 "Separated" 6 "Attrited"
label values rel_status rel_status

browse if inlist(unique_id, 16032, 16176)
browse unique_id partner_id rel_start_all rel_end_all ended rel_status last_yr_observed min_dur max_dur partnered_imp*
browse unique_id partner_id RESPONDENT_WHO_*

misstable summarize FIRST_BIRTH_YR age_focal* birth_yr_all SEX raceth_fixed_focal fixed_education sample_type rel_start_all birth_timing_rel rel_type_constant current_rel_number_main current_parent_status* RESPONDENT_WHO_* retired_est_focal*, all

egen nmis_workhrs = rmiss(weekly_hrs_t1_focal*)
tab nmis_workhrs, m

egen nmis_hwhrs = rmiss(housework_focal*)
tab nmis_hwhrs, m

egen nmis_employ = rmiss(employed_focal*)
tab nmis_employ, m

egen nmis_age = rmiss(age_focal*)
tab nmis_age, m

drop if nmis_age==17 // for now, just so this is actually complete
drop if birth_yr_all==. // for now, just so this is actually complete
drop if raceth_fixed_focal==. // for now, just so this is actually complete
drop if fixed_education==.

forvalues d=0/14{
	drop if RESPONDENT_WHO_`d'==.
}

/*
********************************************************************************
********************************************************************************
**# * Just Employment Hours and HW for now to make it work
********************************************************************************
********************************************************************************

********************************************************************************
* Total sample
********************************************************************************
mi set wide
mi register imputed weekly_hrs_t_focal* housework_focal* employed_focal* earnings_t_focal* educ_focal* college_focal* children* NUM_CHILDREN_* AGE_YOUNG_CHILD_* relationship_* partnered* TOTAL_INCOME_T_FAMILY* num_children_imp* partnered_imp*
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all SEX raceth_fixed_focal sample_type rel_type_constant

#delimit ;

mi impute chained

/* Employment hours */
(pmm, knn(5) include (                weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16 housework_focal0)) weekly_hrs_t_focal0
(pmm, knn(5) include (               weekly_hrs_t_focal0 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16  housework_focal1)) weekly_hrs_t_focal1
(pmm, knn(5) include (              weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16   housework_focal2)) weekly_hrs_t_focal2
(pmm, knn(5) include (             weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16    housework_focal3)) weekly_hrs_t_focal3
(pmm, knn(5) include (            weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16     housework_focal4)) weekly_hrs_t_focal4
(pmm, knn(5) include (           weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16      housework_focal5)) weekly_hrs_t_focal5
(pmm, knn(5) include (          weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16       housework_focal6)) weekly_hrs_t_focal6
(pmm, knn(5) include (         weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16        housework_focal7)) weekly_hrs_t_focal7
(pmm, knn(5) include (        weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16         housework_focal8)) weekly_hrs_t_focal8
(pmm, knn(5) include (       weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16          housework_focal9)) weekly_hrs_t_focal9
(pmm, knn(5) include (      weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16           housework_focal10)) weekly_hrs_t_focal10
(pmm, knn(5) include (     weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16            housework_focal11)) weekly_hrs_t_focal11
(pmm, knn(5) include (    weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16             housework_focal12)) weekly_hrs_t_focal12
(pmm, knn(5) include (   weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16              housework_focal13)) weekly_hrs_t_focal13
(pmm, knn(5) include (  weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal15 weekly_hrs_t_focal16               housework_focal14)) weekly_hrs_t_focal14
(pmm, knn(5) include ( weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal16                housework_focal15)) weekly_hrs_t_focal15
(pmm, knn(5) include (weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15                 housework_focal16)) weekly_hrs_t_focal16


/* Housework hours */
(pmm, knn(5) include (                housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16 weekly_hrs_t_focal0)) housework_focal0
(pmm, knn(5) include (               housework_focal0 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16  weekly_hrs_t_focal1)) housework_focal1
(pmm, knn(5) include (              housework_focal0 housework_focal1 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16   weekly_hrs_t_focal2)) housework_focal2
(pmm, knn(5) include (             housework_focal0 housework_focal1 housework_focal2 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16    weekly_hrs_t_focal3)) housework_focal3
(pmm, knn(5) include (            housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16     weekly_hrs_t_focal4)) housework_focal4
(pmm, knn(5) include (           housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16      weekly_hrs_t_focal5)) housework_focal5
(pmm, knn(5) include (          housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16       weekly_hrs_t_focal6)) housework_focal6
(pmm, knn(5) include (         housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16        weekly_hrs_t_focal7)) housework_focal7
(pmm, knn(5) include (        housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16         weekly_hrs_t_focal8)) housework_focal8
(pmm, knn(5) include (       housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16          weekly_hrs_t_focal9)) housework_focal9
(pmm, knn(5) include (      housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16           weekly_hrs_t_focal10)) housework_focal10
(pmm, knn(5) include (     housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16            weekly_hrs_t_focal11)) housework_focal11
(pmm, knn(5) include (    housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal13 housework_focal14 housework_focal15 housework_focal16             weekly_hrs_t_focal12)) housework_focal12
(pmm, knn(5) include (   housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal14 housework_focal15 housework_focal16              weekly_hrs_t_focal13)) housework_focal13
(pmm, knn(5) include (  housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal15 housework_focal16               weekly_hrs_t_focal14)) housework_focal14
(pmm, knn(5) include ( housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal16                weekly_hrs_t_focal15)) housework_focal15
(pmm, knn(5) include (housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15                 weekly_hrs_t_focal16)) housework_focal16

= i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all i.SEX i.raceth_fixed_focal i.sample_type, chaindots add(10) rseed(12345) noimputed // dryrun // force augment noisily

;
#delimit cr

save "$created_data/psid_individs_imputed_wide", replace

// reshape back to long to look at descriptives
mi reshape long in_sample_ relationship_  partnered weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ hours_type_t1_focal hw_hours_gp raceth_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY childcare_focal adultcare_focal TOTAL_INCOME_T2_FAMILY_ ///
, i(couple_id unique_id partner_id rel_start_all min_dur max_dur rel_end_yr last_yr_observed ended SEX) j(duration_rec)

mi convert flong

browse couple_id unique_id partner_id duration_rec weekly_hrs_t_focal housework_focal _mi_miss _mi_m _mi_id
gen imputed=0
replace imputed=1 if inrange(_mi_m,1,10)

inspect weekly_hrs_t_focal if imputed==0
inspect weekly_hrs_t_focal if imputed==1

inspect housework_focal if imputed==0
inspect housework_focal if imputed==1

// mi register regular n

save "$created_data/psid_individs_imputed_long", replace

********************************************************************************
*  Let's look at some descriptives
********************************************************************************
tabstat weekly_hrs_t_focal housework_focal, by(imputed) stats(mean sd p50)

preserve

collapse (mean) weekly_hrs_t_focal housework_focal, by(duration_rec imputed)

twoway (line weekly_hrs_t_focal duration_rec if imputed==0) (line weekly_hrs_t_focal duration_rec if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0) (line housework_focal duration_rec if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

restore

********************************************************************************
**# * Let's try by sex
********************************************************************************
use "$created_data\individs_by_duration_wide.dta", clear

egen nmis_age = rmiss(age_focal*)
tab nmis_age, m

drop if nmis_age==17 // for now, just so this is actually complete
drop if birth_yr_all==. // for now, just so this is actually complete
drop if raceth_fixed_focal==. // for now, just so this is actually complete

mi set wide
mi register imputed weekly_hrs_t_focal* housework_focal* employed_focal* earnings_t_focal* educ_focal* college_focal* children* NUM_CHILDREN_* AGE_YOUNG_CHILD_* relationship_* partnered* TOTAL_INCOME_T_FAMILY* num_children_imp* partnered_imp*
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all SEX raceth_fixed_focal sample_type rel_type_constant

#delimit ;

mi impute chained

/* Employment hours */
(pmm, knn(5) include (                weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16 housework_focal0)) weekly_hrs_t_focal0
(pmm, knn(5) include (               weekly_hrs_t_focal0 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16  housework_focal1)) weekly_hrs_t_focal1
(pmm, knn(5) include (              weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16   housework_focal2)) weekly_hrs_t_focal2
(pmm, knn(5) include (             weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16    housework_focal3)) weekly_hrs_t_focal3
(pmm, knn(5) include (            weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16     housework_focal4)) weekly_hrs_t_focal4
(pmm, knn(5) include (           weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16      housework_focal5)) weekly_hrs_t_focal5
(pmm, knn(5) include (          weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16       housework_focal6)) weekly_hrs_t_focal6
(pmm, knn(5) include (         weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16        housework_focal7)) weekly_hrs_t_focal7
(pmm, knn(5) include (        weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16         housework_focal8)) weekly_hrs_t_focal8
(pmm, knn(5) include (       weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16          housework_focal9)) weekly_hrs_t_focal9
(pmm, knn(5) include (      weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16           housework_focal10)) weekly_hrs_t_focal10
(pmm, knn(5) include (     weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16            housework_focal11)) weekly_hrs_t_focal11
(pmm, knn(5) include (    weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16             housework_focal12)) weekly_hrs_t_focal12
(pmm, knn(5) include (   weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal14 weekly_hrs_t_focal15 weekly_hrs_t_focal16              housework_focal13)) weekly_hrs_t_focal13
(pmm, knn(5) include (  weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal15 weekly_hrs_t_focal16               housework_focal14)) weekly_hrs_t_focal14
(pmm, knn(5) include ( weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal16                housework_focal15)) weekly_hrs_t_focal15
(pmm, knn(5) include (weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 weekly_hrs_t_focal15                 housework_focal16)) weekly_hrs_t_focal16


/* Housework hours */
(pmm, knn(5) include (                housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16 weekly_hrs_t_focal0)) housework_focal0
(pmm, knn(5) include (               housework_focal0 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16  weekly_hrs_t_focal1)) housework_focal1
(pmm, knn(5) include (              housework_focal0 housework_focal1 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16   weekly_hrs_t_focal2)) housework_focal2
(pmm, knn(5) include (             housework_focal0 housework_focal1 housework_focal2 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16    weekly_hrs_t_focal3)) housework_focal3
(pmm, knn(5) include (            housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16     weekly_hrs_t_focal4)) housework_focal4
(pmm, knn(5) include (           housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16      weekly_hrs_t_focal5)) housework_focal5
(pmm, knn(5) include (          housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16       weekly_hrs_t_focal6)) housework_focal6
(pmm, knn(5) include (         housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16        weekly_hrs_t_focal7)) housework_focal7
(pmm, knn(5) include (        housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16         weekly_hrs_t_focal8)) housework_focal8
(pmm, knn(5) include (       housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16          weekly_hrs_t_focal9)) housework_focal9
(pmm, knn(5) include (      housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16           weekly_hrs_t_focal10)) housework_focal10
(pmm, knn(5) include (     housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16            weekly_hrs_t_focal11)) housework_focal11
(pmm, knn(5) include (    housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal13 housework_focal14 housework_focal15 housework_focal16             weekly_hrs_t_focal12)) housework_focal12
(pmm, knn(5) include (   housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal14 housework_focal15 housework_focal16              weekly_hrs_t_focal13)) housework_focal13
(pmm, knn(5) include (  housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal15 housework_focal16               weekly_hrs_t_focal14)) housework_focal14
(pmm, knn(5) include ( housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal16                weekly_hrs_t_focal15)) housework_focal15
(pmm, knn(5) include (housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15                 weekly_hrs_t_focal16)) housework_focal16

= i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all i.raceth_fixed_focal i.sample_type, chaindots add(10) rseed(12345) noimputed by(SEX) // dryrun force augment noisily

;
#delimit cr

save "$created_data/psid_individs_imputed_wide_bysex", replace

// reshape back to long to look at descriptives
mi reshape long in_sample_ relationship_  partnered weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ hours_type_t1_focal hw_hours_gp raceth_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY childcare_focal adultcare_focal TOTAL_INCOME_T2_FAMILY_ ///
, i(couple_id unique_id partner_id rel_start_all min_dur max_dur rel_end_yr last_yr_observed ended SEX) j(duration_rec)

mi convert flong

browse couple_id unique_id partner_id SEX duration_rec weekly_hrs_t_focal housework_focal _mi_miss _mi_m _mi_id
gen imputed=0
replace imputed=1 if inrange(_mi_m,1,10)

inspect weekly_hrs_t_focal if imputed==0
inspect weekly_hrs_t_focal if imputed==1

inspect housework_focal if imputed==0
inspect housework_focal if imputed==1

// mi register regular n

save "$created_data/psid_individs_imputed_long_bysex", replace

********************************************************************************
*  Let's look at some descriptives
********************************************************************************
tabstat weekly_hrs_t_focal housework_focal, by(imputed) stats(mean sd p50)
tabstat weekly_hrs_t_focal housework_focal if SEX==1, by(imputed) stats(mean sd p50)
tabstat weekly_hrs_t_focal housework_focal if SEX==2, by(imputed) stats(mean sd p50)

preserve

collapse (mean) weekly_hrs_t_focal housework_focal, by(duration_rec imputed)

twoway (line weekly_hrs_t_focal duration_rec if imputed==0) (line weekly_hrs_t_focal duration_rec if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0) (line housework_focal duration_rec if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

restore

preserve

collapse (mean) weekly_hrs_t_focal housework_focal, by(SEX duration_rec imputed)

// men
twoway (line weekly_hrs_t_focal duration_rec if imputed==0 & SEX==1) (line weekly_hrs_t_focal duration_rec if imputed==1 & SEX==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0 & SEX==1) (line housework_focal duration_rec if imputed==1 & SEX==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

// women - okay so it is women where the disparities are primarily. is it bc of EMPLOYMENT STATUS?! need to do conditional on that? let's see if it improves with other predictors, bc employment status not currently included
twoway (line weekly_hrs_t_focal duration_rec if imputed==0 & SEX==2) (line weekly_hrs_t_focal duration_rec if imputed==1 & SEX==2), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0 & SEX==2) (line housework_focal duration_rec if imputed==1 & SEX==2), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

restore


********************************************************************************
********************************************************************************
**# * Attempting to add all variables
********************************************************************************
********************************************************************************

********************************************************************************
* Total sample
********************************************************************************
use "$created_data\individs_by_duration_wide.dta", clear

egen nmis_age = rmiss(age_focal*)
tab nmis_age, m

drop if nmis_age==17 // for now, just so this is actually complete
drop if birth_yr_all==. // for now, just so this is actually complete
drop if raceth_fixed_focal==. // for now, just so this is actually complete

mi set wide
mi register imputed weekly_hrs_t_focal* housework_focal* employed_focal* earnings_t_focal* educ_focal* college_focal* children* NUM_CHILDREN_* AGE_YOUNG_CHILD_* relationship_* partnered* TOTAL_INCOME_T_FAMILY* num_children_imp* partnered_imp*
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all SEX raceth_fixed_focal sample_type rel_type_constant

// start imputation
#delimit ;

mi impute chained

/* Employment hours */
(pmm, knn(5) include (              weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14 housework_focal0 i.employed_focal0 earnings_t_focal0 i.educ_focal0 num_children_imp0 AGE_YOUNG_CHILD_0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) weekly_hrs_t_focal0
(pmm, knn(5) include (             weekly_hrs_t_focal0 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14  housework_focal1 i.employed_focal1 earnings_t_focal1 i.educ_focal1 num_children_imp1 AGE_YOUNG_CHILD_1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) weekly_hrs_t_focal1
(pmm, knn(5) include (            weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14   housework_focal2 i.employed_focal2 earnings_t_focal2 i.educ_focal2 num_children_imp2 AGE_YOUNG_CHILD_2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) weekly_hrs_t_focal2
(pmm, knn(5) include (           weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14    housework_focal3 i.employed_focal3 earnings_t_focal3 i.educ_focal3 num_children_imp3 AGE_YOUNG_CHILD_3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) weekly_hrs_t_focal3
(pmm, knn(5) include (          weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14     housework_focal4 i.employed_focal4 earnings_t_focal4 i.educ_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) weekly_hrs_t_focal4
(pmm, knn(5) include (         weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14      housework_focal5 i.employed_focal5 earnings_t_focal5 i.educ_focal5 num_children_imp5 AGE_YOUNG_CHILD_5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) weekly_hrs_t_focal5
(pmm, knn(5) include (        weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14       housework_focal6 i.employed_focal6 earnings_t_focal6 i.educ_focal6 num_children_imp6 AGE_YOUNG_CHILD_6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) weekly_hrs_t_focal6
(pmm, knn(5) include (       weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14        housework_focal7 i.employed_focal7 earnings_t_focal7 i.educ_focal7 num_children_imp7 AGE_YOUNG_CHILD_7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) weekly_hrs_t_focal7
(pmm, knn(5) include (      weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14         housework_focal8 i.employed_focal8 earnings_t_focal8 i.educ_focal8 num_children_imp8 AGE_YOUNG_CHILD_8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) weekly_hrs_t_focal8
(pmm, knn(5) include (     weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14          housework_focal9 i.employed_focal9 earnings_t_focal9 i.educ_focal9 num_children_imp9 AGE_YOUNG_CHILD_9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) weekly_hrs_t_focal9
(pmm, knn(5) include (    weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14           housework_focal10 i.employed_focal10 earnings_t_focal10 i.educ_focal10 num_children_imp10 AGE_YOUNG_CHILD_10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) weekly_hrs_t_focal10
(pmm, knn(5) include (   weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal12 weekly_hrs_t_focal13 weekly_hrs_t_focal14            housework_focal11 i.employed_focal11 earnings_t_focal11 i.educ_focal11 num_children_imp11 AGE_YOUNG_CHILD_11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) weekly_hrs_t_focal11
(pmm, knn(5) include (  weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal13 weekly_hrs_t_focal14             housework_focal12 i.employed_focal12 earnings_t_focal12 i.educ_focal12 num_children_imp12 AGE_YOUNG_CHILD_12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) weekly_hrs_t_focal12
(pmm, knn(5) include ( weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal14              housework_focal13 i.employed_focal13 earnings_t_focal13 i.educ_focal13 num_children_imp13 AGE_YOUNG_CHILD_13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) weekly_hrs_t_focal13
(pmm, knn(5) include (weekly_hrs_t_focal0 weekly_hrs_t_focal1 weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 weekly_hrs_t_focal13               housework_focal14 i.employed_focal14 earnings_t_focal14 i.educ_focal14 num_children_imp14 AGE_YOUNG_CHILD_14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) weekly_hrs_t_focal14


/* Housework Hours */
(pmm, knn(5) include (              housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 weekly_hrs_t_focal0 i.employed_focal0 earnings_t_focal0 i.educ_focal0 num_children_imp0 AGE_YOUNG_CHILD_0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) housework_focal0
(pmm, knn(5) include (             housework_focal0 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14  weekly_hrs_t_focal1 i.employed_focal1 earnings_t_focal1 i.educ_focal1 num_children_imp1 AGE_YOUNG_CHILD_1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) housework_focal1
(pmm, knn(5) include (            housework_focal0 housework_focal1 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14   weekly_hrs_t_focal2 i.employed_focal2 earnings_t_focal2 i.educ_focal2 num_children_imp2 AGE_YOUNG_CHILD_2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) housework_focal2
(pmm, knn(5) include (           housework_focal0 housework_focal1 housework_focal2 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14    weekly_hrs_t_focal3 i.employed_focal3 earnings_t_focal3 i.educ_focal3 num_children_imp3 AGE_YOUNG_CHILD_3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) housework_focal3
(pmm, knn(5) include (          housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14     weekly_hrs_t_focal4 i.employed_focal4 earnings_t_focal4 i.educ_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) housework_focal4
(pmm, knn(5) include (         housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14      weekly_hrs_t_focal5 i.employed_focal5 earnings_t_focal5 i.educ_focal5 num_children_imp5 AGE_YOUNG_CHILD_5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) housework_focal5
(pmm, knn(5) include (        housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14       weekly_hrs_t_focal6 i.employed_focal6 earnings_t_focal6 i.educ_focal6 num_children_imp6 AGE_YOUNG_CHILD_6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) housework_focal6
(pmm, knn(5) include (       housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14        weekly_hrs_t_focal7 i.employed_focal7 earnings_t_focal7 i.educ_focal7 num_children_imp7 AGE_YOUNG_CHILD_7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) housework_focal7
(pmm, knn(5) include (      housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14         weekly_hrs_t_focal8 i.employed_focal8 earnings_t_focal8 i.educ_focal8 num_children_imp8 AGE_YOUNG_CHILD_8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) housework_focal8
(pmm, knn(5) include (     housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14          weekly_hrs_t_focal9 i.employed_focal9 earnings_t_focal9 i.educ_focal9 num_children_imp9 AGE_YOUNG_CHILD_9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) housework_focal9
(pmm, knn(5) include (    housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal11 housework_focal12 housework_focal13 housework_focal14           weekly_hrs_t_focal10 i.employed_focal10 earnings_t_focal10 i.educ_focal10 num_children_imp10 AGE_YOUNG_CHILD_10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) housework_focal10
(pmm, knn(5) include (   housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal12 housework_focal13 housework_focal14            weekly_hrs_t_focal11 i.employed_focal11 earnings_t_focal11 i.educ_focal11 num_children_imp11 AGE_YOUNG_CHILD_11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) housework_focal11
(pmm, knn(5) include (  housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal13 housework_focal14             weekly_hrs_t_focal12 i.employed_focal12 earnings_t_focal12 i.educ_focal12 num_children_imp12 AGE_YOUNG_CHILD_12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) housework_focal12
(pmm, knn(5) include ( housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal14              weekly_hrs_t_focal13 i.employed_focal13 earnings_t_focal13 i.educ_focal13 num_children_imp13 AGE_YOUNG_CHILD_13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) housework_focal13
(pmm, knn(5) include (housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13               weekly_hrs_t_focal14 i.employed_focal14 earnings_t_focal14 i.educ_focal14 num_children_imp14 AGE_YOUNG_CHILD_14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) housework_focal14


/* Employment Status */
(logit, include (              i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14  housework_focal0  i.educ_focal0 num_children_imp0 AGE_YOUNG_CHILD_0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) employed_focal0
(logit, include (             i.employed_focal0 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14  weekly_hrs_t_focal0 housework_focal1 earnings_t_focal0 i.educ_focal1 num_children_imp1 AGE_YOUNG_CHILD_1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) employed_focal1
(logit, include (            i.employed_focal0 i.employed_focal1 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14   weekly_hrs_t_focal1 housework_focal2 earnings_t_focal1 i.educ_focal2 num_children_imp2 AGE_YOUNG_CHILD_2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) employed_focal2
(logit, include (           i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14    weekly_hrs_t_focal2 housework_focal3 earnings_t_focal2 i.educ_focal3 num_children_imp3 AGE_YOUNG_CHILD_3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) employed_focal3
(logit, include (          i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14     weekly_hrs_t_focal3 housework_focal4 earnings_t_focal3 i.educ_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) employed_focal4
(logit, include (         i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14      weekly_hrs_t_focal4 housework_focal5 earnings_t_focal4 i.educ_focal5 num_children_imp5 AGE_YOUNG_CHILD_5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) employed_focal5
(logit, include (        i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14       weekly_hrs_t_focal5 housework_focal6 earnings_t_focal5 i.educ_focal6 num_children_imp6 AGE_YOUNG_CHILD_6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) employed_focal6
(logit, include (       i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14        weekly_hrs_t_focal6 housework_focal7 earnings_t_focal6 i.educ_focal7 num_children_imp7 AGE_YOUNG_CHILD_7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) employed_focal7
(logit, include (      i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14         weekly_hrs_t_focal7 housework_focal8 earnings_t_focal7 i.educ_focal8 num_children_imp8 AGE_YOUNG_CHILD_8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) employed_focal8
(logit, include (     i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14          weekly_hrs_t_focal8 housework_focal9 earnings_t_focal8 i.educ_focal9 num_children_imp9 AGE_YOUNG_CHILD_9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) employed_focal9
(logit, include (    i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14           weekly_hrs_t_focal9 housework_focal10 earnings_t_focal9 i.educ_focal10 num_children_imp10 AGE_YOUNG_CHILD_10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) employed_focal10
(logit, include (   i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal12 i.employed_focal13 i.employed_focal14            weekly_hrs_t_focal10 housework_focal11 earnings_t_focal10 i.educ_focal11 num_children_imp11 AGE_YOUNG_CHILD_11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) employed_focal11
(logit, include (  i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal13 i.employed_focal14             weekly_hrs_t_focal11 housework_focal12 earnings_t_focal11 i.educ_focal12 num_children_imp12 AGE_YOUNG_CHILD_12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) employed_focal12
(logit, include ( i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal14              weekly_hrs_t_focal12 housework_focal13 earnings_t_focal12 i.educ_focal13 num_children_imp13 AGE_YOUNG_CHILD_13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) employed_focal13
(logit, include (i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13               weekly_hrs_t_focal13 housework_focal14 earnings_t_focal13 i.educ_focal14 num_children_imp14 AGE_YOUNG_CHILD_14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) employed_focal14


/* Annual Earnings */
(pmm, knn(5) include (              earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14 weekly_hrs_t_focal0 i.employed_focal0 housework_focal0 i.educ_focal0 num_children_imp0 AGE_YOUNG_CHILD_0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) earnings_t_focal0
(pmm, knn(5) include (             earnings_t_focal0 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14  weekly_hrs_t_focal1 i.employed_focal1 housework_focal1 i.educ_focal1 num_children_imp1 AGE_YOUNG_CHILD_1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) earnings_t_focal1
(pmm, knn(5) include (            earnings_t_focal0 earnings_t_focal1 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14   weekly_hrs_t_focal2 i.employed_focal2 housework_focal2 i.educ_focal2 num_children_imp2 AGE_YOUNG_CHILD_2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) earnings_t_focal2
(pmm, knn(5) include (           earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14    weekly_hrs_t_focal3 i.employed_focal3 housework_focal3 i.educ_focal3 num_children_imp3 AGE_YOUNG_CHILD_3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) earnings_t_focal3
(pmm, knn(5) include (          earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 i.educ_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) earnings_t_focal4
(pmm, knn(5) include (         earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14      weekly_hrs_t_focal5 i.employed_focal5 housework_focal5 i.educ_focal5 num_children_imp5 AGE_YOUNG_CHILD_5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) earnings_t_focal5
(pmm, knn(5) include (        earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14       weekly_hrs_t_focal6 i.employed_focal6 housework_focal6 i.educ_focal6 num_children_imp6 AGE_YOUNG_CHILD_6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) earnings_t_focal6
(pmm, knn(5) include (       earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14        weekly_hrs_t_focal7 i.employed_focal7 housework_focal7 i.educ_focal7 num_children_imp7 AGE_YOUNG_CHILD_7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) earnings_t_focal7
(pmm, knn(5) include (      earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14         weekly_hrs_t_focal8 i.employed_focal8 housework_focal8 i.educ_focal8 num_children_imp8 AGE_YOUNG_CHILD_8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) earnings_t_focal8
(pmm, knn(5) include (     earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14          weekly_hrs_t_focal9 i.employed_focal9 housework_focal9 i.educ_focal9 num_children_imp9 AGE_YOUNG_CHILD_9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) earnings_t_focal9
(pmm, knn(5) include (    earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14           weekly_hrs_t_focal10 i.employed_focal10 housework_focal10 i.educ_focal10 num_children_imp10 AGE_YOUNG_CHILD_10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) earnings_t_focal10
(pmm, knn(5) include (   earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal12 earnings_t_focal13 earnings_t_focal14            weekly_hrs_t_focal11 i.employed_focal11 housework_focal11 i.educ_focal11 num_children_imp11 AGE_YOUNG_CHILD_11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) earnings_t_focal11
(pmm, knn(5) include (  earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal13 earnings_t_focal14             weekly_hrs_t_focal12 i.employed_focal12 housework_focal12 i.educ_focal12 num_children_imp12 AGE_YOUNG_CHILD_12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) earnings_t_focal12
(pmm, knn(5) include ( earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal14              weekly_hrs_t_focal13 i.employed_focal13 housework_focal13 i.educ_focal13 num_children_imp13 AGE_YOUNG_CHILD_13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) earnings_t_focal13
(pmm, knn(5) include (earnings_t_focal0 earnings_t_focal1 earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 earnings_t_focal13               weekly_hrs_t_focal14 i.employed_focal14 housework_focal14 i.educ_focal14 num_children_imp14 AGE_YOUNG_CHILD_14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) earnings_t_focal14


/* Educational Attainment */
(ologit, include (              i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14 weekly_hrs_t_focal0 i.employed_focal0 housework_focal0 earnings_t_focal0 num_children_imp0 AGE_YOUNG_CHILD_0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) educ_focal0
(ologit, include (             i.educ_focal0 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14  weekly_hrs_t_focal1 i.employed_focal1 housework_focal1 earnings_t_focal1 num_children_imp1 AGE_YOUNG_CHILD_1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) educ_focal1
(ologit, include (            i.educ_focal0 i.educ_focal1 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14   weekly_hrs_t_focal2 i.employed_focal2 housework_focal2 earnings_t_focal2 num_children_imp2 AGE_YOUNG_CHILD_2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) educ_focal2
(ologit, include (           i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14    weekly_hrs_t_focal3 i.employed_focal3 housework_focal3 earnings_t_focal3 num_children_imp3 AGE_YOUNG_CHILD_3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) educ_focal3
(ologit, include (          i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 earnings_t_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) educ_focal4
(ologit, include (         i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14      weekly_hrs_t_focal5 i.employed_focal5 housework_focal5 earnings_t_focal5 num_children_imp5 AGE_YOUNG_CHILD_5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) educ_focal5
(ologit, include (        i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14       weekly_hrs_t_focal6 i.employed_focal6 housework_focal6 earnings_t_focal6 num_children_imp6 AGE_YOUNG_CHILD_6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) educ_focal6
(ologit, include (       i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14        weekly_hrs_t_focal7 i.employed_focal7 housework_focal7 earnings_t_focal7 num_children_imp7 AGE_YOUNG_CHILD_7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) educ_focal7
(ologit, include (      i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14         weekly_hrs_t_focal8 i.employed_focal8 housework_focal8 earnings_t_focal8 num_children_imp8 AGE_YOUNG_CHILD_8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) educ_focal8
(ologit, include (     i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14          weekly_hrs_t_focal9 i.employed_focal9 housework_focal9 earnings_t_focal9 num_children_imp9 AGE_YOUNG_CHILD_9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) educ_focal9
(ologit, include (    i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14           weekly_hrs_t_focal10 i.employed_focal10 housework_focal10 earnings_t_focal10 num_children_imp10 AGE_YOUNG_CHILD_10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) educ_focal10
(ologit, include (   i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal12 i.educ_focal13 i.educ_focal14            weekly_hrs_t_focal11 i.employed_focal11 housework_focal11 earnings_t_focal11 num_children_imp11 AGE_YOUNG_CHILD_11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) educ_focal11
(ologit, include (  i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal13 i.educ_focal14             weekly_hrs_t_focal12 i.employed_focal12 housework_focal12 earnings_t_focal12 num_children_imp12 AGE_YOUNG_CHILD_12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) educ_focal12
(ologit, include ( i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal14              weekly_hrs_t_focal13 i.employed_focal13 housework_focal13 earnings_t_focal13 num_children_imp13 AGE_YOUNG_CHILD_13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) educ_focal13
(ologit, include (i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal4 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13               weekly_hrs_t_focal14 i.employed_focal14 housework_focal14 earnings_t_focal14 num_children_imp14 AGE_YOUNG_CHILD_14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) educ_focal14


/* Number of Children */
(pmm, knn(5) include (              num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14 weekly_hrs_t_focal0 i.employed_focal0 housework_focal0 i.educ_focal0 earnings_t_focal0 AGE_YOUNG_CHILD_0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) num_children_imp0
(pmm, knn(5) include (             num_children_imp0 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14  weekly_hrs_t_focal1 i.employed_focal1 housework_focal1 i.educ_focal1 earnings_t_focal1 AGE_YOUNG_CHILD_1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) num_children_imp1
(pmm, knn(5) include (            num_children_imp0 num_children_imp1 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14   weekly_hrs_t_focal2 i.employed_focal2 housework_focal2 i.educ_focal2 earnings_t_focal2 AGE_YOUNG_CHILD_2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) num_children_imp2
(pmm, knn(5) include (           num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14    weekly_hrs_t_focal3 i.employed_focal3 housework_focal3 i.educ_focal3 earnings_t_focal3 AGE_YOUNG_CHILD_3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) num_children_imp3
(pmm, knn(5) include (          num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 i.educ_focal4 earnings_t_focal4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) num_children_imp4
(pmm, knn(5) include (         num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14      weekly_hrs_t_focal5 i.employed_focal5 housework_focal5 i.educ_focal5 earnings_t_focal5 AGE_YOUNG_CHILD_5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) num_children_imp5
(pmm, knn(5) include (        num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14       weekly_hrs_t_focal6 i.employed_focal6 housework_focal6 i.educ_focal6 earnings_t_focal6 AGE_YOUNG_CHILD_6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) num_children_imp6
(pmm, knn(5) include (       num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14        weekly_hrs_t_focal7 i.employed_focal7 housework_focal7 i.educ_focal7 earnings_t_focal7 AGE_YOUNG_CHILD_7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) num_children_imp7
(pmm, knn(5) include (      num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14         weekly_hrs_t_focal8 i.employed_focal8 housework_focal8 i.educ_focal8 earnings_t_focal8 AGE_YOUNG_CHILD_8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) num_children_imp8
(pmm, knn(5) include (     num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14          weekly_hrs_t_focal9 i.employed_focal9 housework_focal9 i.educ_focal9 earnings_t_focal9 AGE_YOUNG_CHILD_9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) num_children_imp9
(pmm, knn(5) include (    num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp11 num_children_imp12 num_children_imp13 num_children_imp14           weekly_hrs_t_focal10 i.employed_focal10 housework_focal10 i.educ_focal10 earnings_t_focal10 AGE_YOUNG_CHILD_10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) num_children_imp10
(pmm, knn(5) include (   num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp12 num_children_imp13 num_children_imp14            weekly_hrs_t_focal11 i.employed_focal11 housework_focal11 i.educ_focal11 earnings_t_focal11 AGE_YOUNG_CHILD_11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) num_children_imp11
(pmm, knn(5) include (  num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp13 num_children_imp14             weekly_hrs_t_focal12 i.employed_focal12 housework_focal12 i.educ_focal12 earnings_t_focal12 AGE_YOUNG_CHILD_12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) num_children_imp12
(pmm, knn(5) include ( num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp14              weekly_hrs_t_focal13 i.employed_focal13 housework_focal13 i.educ_focal13 earnings_t_focal13 AGE_YOUNG_CHILD_13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) num_children_imp13
(pmm, knn(5) include (num_children_imp0 num_children_imp1 num_children_imp2 num_children_imp3 num_children_imp4 num_children_imp5 num_children_imp6 num_children_imp7 num_children_imp8 num_children_imp9 num_children_imp10 num_children_imp11 num_children_imp12 num_children_imp13               weekly_hrs_t_focal14 i.employed_focal14 housework_focal14 i.educ_focal14 earnings_t_focal14 AGE_YOUNG_CHILD_14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) num_children_imp14


/* Age of Youngest Child */
(pmm, knn(5) include (              AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14 weekly_hrs_t_focal0 i.employed_focal0 housework_focal0 i.educ_focal0 earnings_t_focal0 num_children_imp0 i.partnered_imp0 TOTAL_INCOME_T_FAMILY0)) AGE_YOUNG_CHILD_0
(pmm, knn(5) include (             AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14  weekly_hrs_t_focal1 i.employed_focal1 housework_focal1 i.educ_focal1 earnings_t_focal1 num_children_imp1 i.partnered_imp1 TOTAL_INCOME_T_FAMILY1)) AGE_YOUNG_CHILD_1
(pmm, knn(5) include (            AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14   weekly_hrs_t_focal2 i.employed_focal2 housework_focal2 i.educ_focal2 earnings_t_focal2 num_children_imp2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2)) AGE_YOUNG_CHILD_2
(pmm, knn(5) include (           AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14    weekly_hrs_t_focal3 i.employed_focal3 housework_focal3 i.educ_focal3 earnings_t_focal3 num_children_imp3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3)) AGE_YOUNG_CHILD_3
(pmm, knn(5) include (          AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 i.educ_focal4 earnings_t_focal4 num_children_imp4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4)) AGE_YOUNG_CHILD_4
(pmm, knn(5) include (         AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14      weekly_hrs_t_focal5 i.employed_focal5 housework_focal5 i.educ_focal5 earnings_t_focal5 num_children_imp5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5)) AGE_YOUNG_CHILD_5
(pmm, knn(5) include (        AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14       weekly_hrs_t_focal6 i.employed_focal6 housework_focal6 i.educ_focal6 earnings_t_focal6 num_children_imp6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6)) AGE_YOUNG_CHILD_6
(pmm, knn(5) include (       AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14        weekly_hrs_t_focal7 i.employed_focal7 housework_focal7 i.educ_focal7 earnings_t_focal7 num_children_imp7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7)) AGE_YOUNG_CHILD_7
(pmm, knn(5) include (      AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14         weekly_hrs_t_focal8 i.employed_focal8 housework_focal8 i.educ_focal8 earnings_t_focal8 num_children_imp8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8)) AGE_YOUNG_CHILD_8
(pmm, knn(5) include (     AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14          weekly_hrs_t_focal9 i.employed_focal9 housework_focal9 i.educ_focal9 earnings_t_focal9 num_children_imp9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9)) AGE_YOUNG_CHILD_9
(pmm, knn(5) include (    AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14           weekly_hrs_t_focal10 i.employed_focal10 housework_focal10 i.educ_focal10 earnings_t_focal10 num_children_imp10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10)) AGE_YOUNG_CHILD_10
(pmm, knn(5) include (   AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14            weekly_hrs_t_focal11 i.employed_focal11 housework_focal11 i.educ_focal11 earnings_t_focal11 num_children_imp11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11)) AGE_YOUNG_CHILD_11
(pmm, knn(5) include (  AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_13 AGE_YOUNG_CHILD_14             weekly_hrs_t_focal12 i.employed_focal12 housework_focal12 i.educ_focal12 earnings_t_focal12 num_children_imp12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12)) AGE_YOUNG_CHILD_12
(pmm, knn(5) include ( AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_14              weekly_hrs_t_focal13 i.employed_focal13 housework_focal13 i.educ_focal13 earnings_t_focal13 num_children_imp13 i.partnered_imp13 TOTAL_INCOME_T_FAMILY13)) AGE_YOUNG_CHILD_13
(pmm, knn(5) include (AGE_YOUNG_CHILD_0 AGE_YOUNG_CHILD_1 AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 AGE_YOUNG_CHILD_13               weekly_hrs_t_focal14 i.employed_focal14 housework_focal14 i.educ_focal14 earnings_t_focal14 num_children_imp14 i.partnered_imp14 TOTAL_INCOME_T_FAMILY14)) AGE_YOUNG_CHILD_14


/* Relationship to HH Head
(mlogit, include (                i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16)) relationship_0
(mlogit, include (               i.relationship_0 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16 )) relationship_1
(mlogit, include (              i.relationship_0 i.relationship_1 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16  )) relationship_2
(mlogit, include (             i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16   )) relationship_3
(mlogit, include (            i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16    )) relationship_4
(mlogit, include (           i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16     )) relationship_5
(mlogit, include (          i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16      )) relationship_6
(mlogit, include (         i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16       )) relationship_7
(mlogit, include (        i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16        )) relationship_8
(mlogit, include (       i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16         )) relationship_9
(mlogit, include (      i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16          )) relationship_10
(mlogit, include (     i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16           )) relationship_11
(mlogit, include (    i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_13 i.relationship_14 i.relationship_15 i.relationship_16            )) relationship_12
(mlogit, include (   i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_14 i.relationship_15 i.relationship_16             )) relationship_13
(mlogit, include (  i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_15 i.relationship_16              )) relationship_14
(mlogit, include ( i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_16               )) relationship_15
(mlogit, include (i.relationship_0 i.relationship_1 i.relationship_2 i.relationship_3 i.relationship_4 i.relationship_5 i.relationship_6 i.relationship_7 i.relationship_8 i.relationship_9 i.relationship_10 i.relationship_11 i.relationship_12 i.relationship_13 i.relationship_14 i.relationship_15                )) relationship_16
*/

/* Partnership Status */
(logit, include (              i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14 weekly_hrs_t_focal0 i.employed_focal0 housework_focal0 earnings_t_focal0 num_children_imp0 AGE_YOUNG_CHILD_0 i.educ_focal0 TOTAL_INCOME_T_FAMILY0)) partnered_imp0
(logit, include (             i.partnered_imp0 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14  weekly_hrs_t_focal1 i.employed_focal1 housework_focal1 earnings_t_focal1 num_children_imp1 AGE_YOUNG_CHILD_1 i.educ_focal1 TOTAL_INCOME_T_FAMILY1)) partnered_imp1
(logit, include (            i.partnered_imp0 i.partnered_imp1 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14   weekly_hrs_t_focal2 i.employed_focal2 housework_focal2 earnings_t_focal2 num_children_imp2 AGE_YOUNG_CHILD_2 i.educ_focal2 TOTAL_INCOME_T_FAMILY2)) partnered_imp2
(logit, include (           i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14    weekly_hrs_t_focal3 i.employed_focal3 housework_focal3 earnings_t_focal3 num_children_imp3 AGE_YOUNG_CHILD_3 i.educ_focal3 TOTAL_INCOME_T_FAMILY3)) partnered_imp3
(logit, include (          i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 earnings_t_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.educ_focal4 TOTAL_INCOME_T_FAMILY4)) partnered_imp4
(logit, include (         i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14      weekly_hrs_t_focal5 i.employed_focal5 housework_focal5 earnings_t_focal5 num_children_imp5 AGE_YOUNG_CHILD_5 i.educ_focal5 TOTAL_INCOME_T_FAMILY5)) partnered_imp5
(logit, include (        i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14       weekly_hrs_t_focal6 i.employed_focal6 housework_focal6 earnings_t_focal6 num_children_imp6 AGE_YOUNG_CHILD_6 i.educ_focal6 TOTAL_INCOME_T_FAMILY6)) partnered_imp6
(logit, include (       i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14        weekly_hrs_t_focal7 i.employed_focal7 housework_focal7 earnings_t_focal7 num_children_imp7 AGE_YOUNG_CHILD_7 i.educ_focal7 TOTAL_INCOME_T_FAMILY7)) partnered_imp7
(logit, include (      i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14         weekly_hrs_t_focal8 i.employed_focal8 housework_focal8 earnings_t_focal8 num_children_imp8 AGE_YOUNG_CHILD_8 i.educ_focal8 TOTAL_INCOME_T_FAMILY8)) partnered_imp8
(logit, include (     i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14          weekly_hrs_t_focal9 i.employed_focal9 housework_focal9 earnings_t_focal9 num_children_imp9 AGE_YOUNG_CHILD_9 i.educ_focal9 TOTAL_INCOME_T_FAMILY9)) partnered_imp9
(logit, include (    i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14           weekly_hrs_t_focal10 i.employed_focal10 housework_focal10 earnings_t_focal10 num_children_imp10 AGE_YOUNG_CHILD_10 i.educ_focal10 TOTAL_INCOME_T_FAMILY10)) partnered_imp10
(logit, include (   i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp12 i.partnered_imp13 i.partnered_imp14            weekly_hrs_t_focal11 i.employed_focal11 housework_focal11 earnings_t_focal11 num_children_imp11 AGE_YOUNG_CHILD_11 i.educ_focal11 TOTAL_INCOME_T_FAMILY11)) partnered_imp11
(logit, include (  i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp13 i.partnered_imp14             weekly_hrs_t_focal12 i.employed_focal12 housework_focal12 earnings_t_focal12 num_children_imp12 AGE_YOUNG_CHILD_12 i.educ_focal12 TOTAL_INCOME_T_FAMILY12)) partnered_imp12
(logit, include ( i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp14              weekly_hrs_t_focal13 i.employed_focal13 housework_focal13 earnings_t_focal13 num_children_imp13 AGE_YOUNG_CHILD_13 i.educ_focal13 TOTAL_INCOME_T_FAMILY13)) partnered_imp13
(logit, include (i.partnered_imp0 i.partnered_imp1 i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 i.partnered_imp13               weekly_hrs_t_focal14 i.employed_focal14 housework_focal14 earnings_t_focal14 num_children_imp14 AGE_YOUNG_CHILD_14 i.educ_focal14 TOTAL_INCOME_T_FAMILY14)) partnered_imp14


/* Family Income */
(pmm, knn(5) include (              TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14 weekly_hrs_t_focal0 i.employed_focal0 housework_focal0 i.educ_focal0 earnings_t_focal0 num_children_imp0 i.partnered_imp0 AGE_YOUNG_CHILD_0)) TOTAL_INCOME_T_FAMILY0
(pmm, knn(5) include (             TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14  weekly_hrs_t_focal1 i.employed_focal1 housework_focal1 i.educ_focal1 earnings_t_focal1 num_children_imp1 i.partnered_imp1 AGE_YOUNG_CHILD_1)) TOTAL_INCOME_T_FAMILY1
(pmm, knn(5) include (            TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14   weekly_hrs_t_focal2 i.employed_focal2 housework_focal2 i.educ_focal2 earnings_t_focal2 num_children_imp2 i.partnered_imp2 AGE_YOUNG_CHILD_2)) TOTAL_INCOME_T_FAMILY2
(pmm, knn(5) include (           TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14    weekly_hrs_t_focal3 i.employed_focal3 housework_focal3 i.educ_focal3 earnings_t_focal3 num_children_imp3 i.partnered_imp3 AGE_YOUNG_CHILD_3)) TOTAL_INCOME_T_FAMILY3
(pmm, knn(5) include (          TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 i.educ_focal4 earnings_t_focal4 num_children_imp4 i.partnered_imp4 AGE_YOUNG_CHILD_4)) TOTAL_INCOME_T_FAMILY4
(pmm, knn(5) include (         TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14      weekly_hrs_t_focal5 i.employed_focal5 housework_focal5 i.educ_focal5 earnings_t_focal5 num_children_imp5 i.partnered_imp5 AGE_YOUNG_CHILD_5)) TOTAL_INCOME_T_FAMILY5
(pmm, knn(5) include (        TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14       weekly_hrs_t_focal6 i.employed_focal6 housework_focal6 i.educ_focal6 earnings_t_focal6 num_children_imp6 i.partnered_imp6 AGE_YOUNG_CHILD_6)) TOTAL_INCOME_T_FAMILY6
(pmm, knn(5) include (       TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14        weekly_hrs_t_focal7 i.employed_focal7 housework_focal7 i.educ_focal7 earnings_t_focal7 num_children_imp7 i.partnered_imp7 AGE_YOUNG_CHILD_7)) TOTAL_INCOME_T_FAMILY7
(pmm, knn(5) include (      TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14         weekly_hrs_t_focal8 i.employed_focal8 housework_focal8 i.educ_focal8 earnings_t_focal8 num_children_imp8 i.partnered_imp8 AGE_YOUNG_CHILD_8)) TOTAL_INCOME_T_FAMILY8
(pmm, knn(5) include (     TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14          weekly_hrs_t_focal9 i.employed_focal9 housework_focal9 i.educ_focal9 earnings_t_focal9 num_children_imp9 i.partnered_imp9 AGE_YOUNG_CHILD_9)) TOTAL_INCOME_T_FAMILY9
(pmm, knn(5) include (    TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14           weekly_hrs_t_focal10 i.employed_focal10 housework_focal10 i.educ_focal10 earnings_t_focal10 num_children_imp10 i.partnered_imp10 AGE_YOUNG_CHILD_10)) TOTAL_INCOME_T_FAMILY10
(pmm, knn(5) include (   TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14            weekly_hrs_t_focal11 i.employed_focal11 housework_focal11 i.educ_focal11 earnings_t_focal11 num_children_imp11 i.partnered_imp11 AGE_YOUNG_CHILD_11)) TOTAL_INCOME_T_FAMILY11
(pmm, knn(5) include (  TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY13 TOTAL_INCOME_T_FAMILY14             weekly_hrs_t_focal12 i.employed_focal12 housework_focal12 i.educ_focal12 earnings_t_focal12 num_children_imp12 i.partnered_imp12 AGE_YOUNG_CHILD_12)) TOTAL_INCOME_T_FAMILY12
(pmm, knn(5) include ( TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY14              weekly_hrs_t_focal13 i.employed_focal13 housework_focal13 i.educ_focal13 earnings_t_focal13 num_children_imp13 i.partnered_imp13 AGE_YOUNG_CHILD_13)) TOTAL_INCOME_T_FAMILY13
(pmm, knn(5) include (TOTAL_INCOME_T_FAMILY0 TOTAL_INCOME_T_FAMILY1 TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 TOTAL_INCOME_T_FAMILY13               weekly_hrs_t_focal14 i.employed_focal14 housework_focal14 i.educ_focal14 earnings_t_focal14 num_children_imp14 i.partnered_imp14 AGE_YOUNG_CHILD_14)) TOTAL_INCOME_T_FAMILY14

= birth_yr_all i.raceth_fixed_focal i.sample_type i.SEX i.rel_type_constant, chaindots add(10) rseed(12345) noimputed augment force // dryrun // noisily i.rel_start_all  FIRST_BIRTH_YR

;
#delimit cr

// = i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all i.raceth_fixed_focal i.sample_type i.SEX, chaindots add(1) rseed(12345) noimputed augment noisily force // dryrun

save "$created_data/psid_individs_imputed_wide", replace // okay, now that this worked with more variables, will replace above version

// reshape back to long to look at descriptives
mi reshape long in_sample_ relationship_  partnered weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal college_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ hours_type_t1_focal hw_hours_gp raceth_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY childcare_focal adultcare_focal TOTAL_INCOME_T2_FAMILY_ num_children_imp partnered_imp ///
, i(couple_id unique_id partner_id rel_start_all min_dur max_dur rel_end_all last_yr_observed ended SEX) j(duration_rec)

mi convert flong

browse couple_id unique_id partner_id duration_rec weekly_hrs_t_focal housework_focal _mi_miss _mi_m _mi_id
gen imputed=0
replace imputed=1 if inrange(_mi_m,1,10)

inspect weekly_hrs_t_focal if imputed==0
inspect weekly_hrs_t_focal if imputed==1

inspect housework_focal if imputed==0
inspect housework_focal if imputed==1

// mi register regular n

save "$created_data/psid_individs_imputed_long", replace

********************************************************************************
*  Let's look at some descriptives
********************************************************************************
tabstat weekly_hrs_t_focal housework_focal, by(imputed) stats(mean sd p50)

histogram weekly_hrs_t_focal, width(1)
twoway (histogram weekly_hrs_t_focal if imputed==0, width(2) color(blue%30)) (histogram weekly_hrs_t_focal if imputed==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

histogram weekly_hrs_t_focal if weekly_hrs_t1_focal>0, width(1)
histogram housework_focal, width(1)
twoway (histogram housework_focal if imputed==0, width(2) color(blue%30)) (histogram housework_focal if imputed==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

preserve

collapse (mean) weekly_hrs_t_focal housework_focal, by(duration_rec imputed)

twoway (line weekly_hrs_t_focal duration_rec if imputed==0) (line weekly_hrs_t_focal duration_rec if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0) (line housework_focal duration_rec if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

restore

// kim, this is by duration, which is useful, but also look at historgrams - so then it is proportion by working hours instead of working hours by duration.
*/

*******************************************************************************
**# By sex
********************************************************************************
use "$created_data/individs_by_duration_wide.dta", clear

// prep work - data
egen nmis_age = rmiss(age_focal*)
tab nmis_age, m

drop if nmis_age==15 // for now, just so this is actually complete
drop if birth_yr_all==. // for now, just so this is actually complete
drop if raceth_fixed_focal==. // for now, just so this is actually complete
drop if fixed_education==.

forvalues d=0/14{
	inspect RESPONDENT_WHO_`d'
	drop if RESPONDENT_WHO_`d'==.
}

misstable summarize FIRST_BIRTH_YR age_focal* birth_yr_all SEX raceth_fixed_focal fixed_education sample_type rel_start_all birth_timing_rel rel_type_constant current_rel_number_main current_parent_status* RESPONDENT_WHO_* retired_est_focal*, all

// prep work - imputation
mi set wide
mi register imputed weekly_hrs_t_focal* housework_focal* employment_status_focal* earnings_t_focal* AGE_YOUNG_CHILD_* partnered* TOTAL_INCOME_T_FAMILY* num_children_imp_hh* house_status_all* REGION_* religion_focal* sr_health_focal* num_65up_hh* num_parent_in_hh* lives_family_focal* father_max_educ_focal mother_max_educ_focal family_structure_cons_focal // disabled_focal* disabled_imp_focal* children* NUM_CHILDREN_* relationship_* - maybe I can't register them?
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all SEX raceth_fixed_focal sample_type rel_type_constant fixed_education birth_timing_rel current_rel_number_main current_parent_status* RESPONDENT_WHO_* retired_est_focal*
// swap rel_status for rel_type_constant?

// log using "$logdir\mi_help.log", replace

// start imputation
// mimpt chained

#delimit ;

mi impute chained

/* Employment hours */
(pmm, knn(5) include (           weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12 housework_focal2 i.employment_status_focal2 earnings_t_focal2 i.house_status_all2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.religion_focal2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) weekly_hrs_t_focal2
(pmm, knn(5) include (          weekly_hrs_t_focal2 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12  housework_focal3 i.employment_status_focal3 earnings_t_focal3 i.house_status_all3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.religion_focal3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) weekly_hrs_t_focal3
(pmm, knn(5) include (         weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12   housework_focal4 i.employment_status_focal4 earnings_t_focal4 i.house_status_all4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.religion_focal4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) weekly_hrs_t_focal4
(pmm, knn(5) include (        weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12    housework_focal5 i.employment_status_focal5 earnings_t_focal5 i.house_status_all5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.religion_focal5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) weekly_hrs_t_focal5
(pmm, knn(5) include (       weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12     housework_focal6 i.employment_status_focal6 earnings_t_focal6 i.house_status_all6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.religion_focal6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) weekly_hrs_t_focal6
(pmm, knn(5) include (      weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12      housework_focal7 i.employment_status_focal7 earnings_t_focal7 i.house_status_all7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.religion_focal7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) weekly_hrs_t_focal7
(pmm, knn(5) include (     weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12       housework_focal8 i.employment_status_focal8 earnings_t_focal8 i.house_status_all8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.religion_focal8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) weekly_hrs_t_focal8
(pmm, knn(5) include (    weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal10 weekly_hrs_t_focal11 weekly_hrs_t_focal12        housework_focal9 i.employment_status_focal9 earnings_t_focal9 i.house_status_all9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.religion_focal9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) weekly_hrs_t_focal9
(pmm, knn(5) include (   weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal11 weekly_hrs_t_focal12         housework_focal10 i.employment_status_focal10 earnings_t_focal10 i.house_status_all10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.religion_focal10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) weekly_hrs_t_focal10
(pmm, knn(5) include (  weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal12          housework_focal11 i.employment_status_focal11 earnings_t_focal11 i.house_status_all11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.religion_focal11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) weekly_hrs_t_focal11
(pmm, knn(5) include ( weekly_hrs_t_focal2 weekly_hrs_t_focal3 weekly_hrs_t_focal4 weekly_hrs_t_focal5 weekly_hrs_t_focal6 weekly_hrs_t_focal7 weekly_hrs_t_focal8 weekly_hrs_t_focal9 weekly_hrs_t_focal10 weekly_hrs_t_focal11           housework_focal12 i.employment_status_focal12 earnings_t_focal12 i.house_status_all12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.religion_focal12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) weekly_hrs_t_focal12


/* Housework Hours */
(pmm, knn(5) include (           housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 weekly_hrs_t_focal2 i.employment_status_focal2 earnings_t_focal2 i.house_status_all2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.religion_focal2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) housework_focal2
(pmm, knn(5) include (          housework_focal2 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12  weekly_hrs_t_focal3 i.employment_status_focal3 earnings_t_focal3 i.house_status_all3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.religion_focal3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) housework_focal3
(pmm, knn(5) include (         housework_focal2 housework_focal3 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12   weekly_hrs_t_focal4 i.employment_status_focal4 earnings_t_focal4 i.house_status_all4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.religion_focal4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) housework_focal4
(pmm, knn(5) include (        housework_focal2 housework_focal3 housework_focal4 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12    weekly_hrs_t_focal5 i.employment_status_focal5 earnings_t_focal5 i.house_status_all5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.religion_focal5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) housework_focal5
(pmm, knn(5) include (       housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12     weekly_hrs_t_focal6 i.employment_status_focal6 earnings_t_focal6 i.house_status_all6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.religion_focal6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) housework_focal6
(pmm, knn(5) include (      housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12      weekly_hrs_t_focal7 i.employment_status_focal7 earnings_t_focal7 i.house_status_all7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.religion_focal7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) housework_focal7
(pmm, knn(5) include (     housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal9 housework_focal10 housework_focal11 housework_focal12       weekly_hrs_t_focal8 i.employment_status_focal8 earnings_t_focal8 i.house_status_all8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.religion_focal8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) housework_focal8
(pmm, knn(5) include (    housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal10 housework_focal11 housework_focal12        weekly_hrs_t_focal9 i.employment_status_focal9 earnings_t_focal9 i.house_status_all9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.religion_focal9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) housework_focal9
(pmm, knn(5) include (   housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal11 housework_focal12         weekly_hrs_t_focal10 i.employment_status_focal10 earnings_t_focal10 i.house_status_all10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.religion_focal10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) housework_focal10
(pmm, knn(5) include (  housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal12          weekly_hrs_t_focal11 i.employment_status_focal11 earnings_t_focal11 i.house_status_all11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.religion_focal11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) housework_focal11
(pmm, knn(5) include ( housework_focal2 housework_focal3 housework_focal4 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11           weekly_hrs_t_focal12 i.employment_status_focal12 earnings_t_focal12 i.house_status_all12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.religion_focal12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) housework_focal12


/* Employment Status */
(pmm, knn(5) include (           i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12 weekly_hrs_t_focal2 housework_focal2 earnings_t_focal2 i.house_status_all2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.religion_focal2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) employment_status_focal2
(pmm, knn(5) include (          i.employment_status_focal2 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12  weekly_hrs_t_focal3 housework_focal3 earnings_t_focal3 i.house_status_all3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.religion_focal3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) employment_status_focal3
(pmm, knn(5) include (         i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12   weekly_hrs_t_focal4 housework_focal4 earnings_t_focal4 i.house_status_all4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.religion_focal4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) employment_status_focal4
(pmm, knn(5) include (        i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12    weekly_hrs_t_focal5 housework_focal5 earnings_t_focal5 i.house_status_all5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.religion_focal5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) employment_status_focal5
(pmm, knn(5) include (       i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12     weekly_hrs_t_focal6 housework_focal6 earnings_t_focal6 i.house_status_all6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.religion_focal6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) employment_status_focal6
(pmm, knn(5) include (      i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12      weekly_hrs_t_focal7 housework_focal7 earnings_t_focal7 i.house_status_all7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.religion_focal7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) employment_status_focal7
(pmm, knn(5) include (     i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12       weekly_hrs_t_focal8 housework_focal8 earnings_t_focal8 i.house_status_all8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.religion_focal8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) employment_status_focal8
(pmm, knn(5) include (    i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal10 i.employment_status_focal11 i.employment_status_focal12        weekly_hrs_t_focal9 housework_focal9 earnings_t_focal9 i.house_status_all9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.religion_focal9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) employment_status_focal9
(pmm, knn(5) include (   i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal11 i.employment_status_focal12         weekly_hrs_t_focal10 housework_focal10 earnings_t_focal10 i.house_status_all10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.religion_focal10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) employment_status_focal10
(pmm, knn(5) include (  i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal12          weekly_hrs_t_focal11 housework_focal11 earnings_t_focal11 i.house_status_all11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.religion_focal11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) employment_status_focal11
(pmm, knn(5) include ( i.employment_status_focal2 i.employment_status_focal3 i.employment_status_focal4 i.employment_status_focal5 i.employment_status_focal6 i.employment_status_focal7 i.employment_status_focal8 i.employment_status_focal9 i.employment_status_focal10 i.employment_status_focal11           weekly_hrs_t_focal12 housework_focal12 earnings_t_focal12 i.house_status_all12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.religion_focal12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) employment_status_focal12

/* Annual Earnings */
(pmm, knn(5) include (           earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 i.house_status_all2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.religion_focal2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) earnings_t_focal2
(pmm, knn(5) include (          earnings_t_focal2 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 i.house_status_all3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.religion_focal3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) earnings_t_focal3
(pmm, knn(5) include (         earnings_t_focal2 earnings_t_focal3 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 i.house_status_all4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.religion_focal4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) earnings_t_focal4
(pmm, knn(5) include (        earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 i.house_status_all5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.religion_focal5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) earnings_t_focal5
(pmm, knn(5) include (       earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 i.house_status_all6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.religion_focal6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) earnings_t_focal6
(pmm, knn(5) include (      earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 i.house_status_all7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.religion_focal7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) earnings_t_focal7
(pmm, knn(5) include (     earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 i.house_status_all8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.religion_focal8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) earnings_t_focal8
(pmm, knn(5) include (    earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal10 earnings_t_focal11 earnings_t_focal12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 i.house_status_all9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.religion_focal9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) earnings_t_focal9
(pmm, knn(5) include (   earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal11 earnings_t_focal12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 i.house_status_all10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.religion_focal10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) earnings_t_focal10
(pmm, knn(5) include (  earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 i.house_status_all11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.religion_focal11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) earnings_t_focal11
(pmm, knn(5) include ( earnings_t_focal2 earnings_t_focal3 earnings_t_focal4 earnings_t_focal5 earnings_t_focal6 earnings_t_focal7 earnings_t_focal8 earnings_t_focal9 earnings_t_focal10 earnings_t_focal11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 i.house_status_all12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.religion_focal12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) earnings_t_focal12


/* Educational Attainment - using time invariant now */

/* Number of Children */

(pmm, knn(5) include (           num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 i.house_status_all2 i.REGION_2 earnings_t_focal2 AGE_YOUNG_CHILD_2 i.religion_focal2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) num_children_imp_hh2
(pmm, knn(5) include (          num_children_imp_hh2 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 i.house_status_all3 i.REGION_3 earnings_t_focal3 AGE_YOUNG_CHILD_3 i.religion_focal3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) num_children_imp_hh3
(pmm, knn(5) include (         num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 i.house_status_all4 i.REGION_4 earnings_t_focal4 AGE_YOUNG_CHILD_4 i.religion_focal4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) num_children_imp_hh4
(pmm, knn(5) include (        num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 i.house_status_all5 i.REGION_5 earnings_t_focal5 AGE_YOUNG_CHILD_5 i.religion_focal5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) num_children_imp_hh5
(pmm, knn(5) include (       num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 i.house_status_all6 i.REGION_6 earnings_t_focal6 AGE_YOUNG_CHILD_6 i.religion_focal6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) num_children_imp_hh6
(pmm, knn(5) include (      num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 i.house_status_all7 i.REGION_7 earnings_t_focal7 AGE_YOUNG_CHILD_7 i.religion_focal7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) num_children_imp_hh7
(pmm, knn(5) include (     num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 i.house_status_all8 i.REGION_8 earnings_t_focal8 AGE_YOUNG_CHILD_8 i.religion_focal8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) num_children_imp_hh8
(pmm, knn(5) include (    num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh10 num_children_imp_hh11 num_children_imp_hh12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 i.house_status_all9 i.REGION_9 earnings_t_focal9 AGE_YOUNG_CHILD_9 i.religion_focal9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) num_children_imp_hh9
(pmm, knn(5) include (   num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh11 num_children_imp_hh12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 i.house_status_all10 i.REGION_10 earnings_t_focal10 AGE_YOUNG_CHILD_10 i.religion_focal10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) num_children_imp_hh10
(pmm, knn(5) include (  num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 i.house_status_all11 i.REGION_11 earnings_t_focal11 AGE_YOUNG_CHILD_11 i.religion_focal11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) num_children_imp_hh11
(pmm, knn(5) include ( num_children_imp_hh2 num_children_imp_hh3 num_children_imp_hh4 num_children_imp_hh5 num_children_imp_hh6 num_children_imp_hh7 num_children_imp_hh8 num_children_imp_hh9 num_children_imp_hh10 num_children_imp_hh11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 i.house_status_all12 i.REGION_12 earnings_t_focal12 AGE_YOUNG_CHILD_12 i.religion_focal12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) num_children_imp_hh12


/* Age of Youngest Child */
(pmm, knn(5) include (           AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 i.house_status_all2 i.REGION_2 earnings_t_focal2 num_children_imp_hh2 i.religion_focal2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) AGE_YOUNG_CHILD_2
(pmm, knn(5) include (          AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 i.house_status_all3 i.REGION_3 earnings_t_focal3 num_children_imp_hh3 i.religion_focal3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) AGE_YOUNG_CHILD_3
(pmm, knn(5) include (         AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 i.house_status_all4 i.REGION_4 earnings_t_focal4 num_children_imp_hh4 i.religion_focal4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) AGE_YOUNG_CHILD_4
(pmm, knn(5) include (        AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 i.house_status_all5 i.REGION_5 earnings_t_focal5 num_children_imp_hh5 i.religion_focal5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) AGE_YOUNG_CHILD_5
(pmm, knn(5) include (       AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 i.house_status_all6 i.REGION_6 earnings_t_focal6 num_children_imp_hh6 i.religion_focal6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) AGE_YOUNG_CHILD_6
(pmm, knn(5) include (      AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 i.house_status_all7 i.REGION_7 earnings_t_focal7 num_children_imp_hh7 i.religion_focal7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) AGE_YOUNG_CHILD_7
(pmm, knn(5) include (     AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 i.house_status_all8 i.REGION_8 earnings_t_focal8 num_children_imp_hh8 i.religion_focal8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) AGE_YOUNG_CHILD_8
(pmm, knn(5) include (    AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 i.house_status_all9 i.REGION_9 earnings_t_focal9 num_children_imp_hh9 i.religion_focal9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) AGE_YOUNG_CHILD_9
(pmm, knn(5) include (   AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_11 AGE_YOUNG_CHILD_12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 i.house_status_all10 i.REGION_10 earnings_t_focal10 num_children_imp_hh10 i.religion_focal10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) AGE_YOUNG_CHILD_10
(pmm, knn(5) include (  AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 i.house_status_all11 i.REGION_11 earnings_t_focal11 num_children_imp_hh11 i.religion_focal11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) AGE_YOUNG_CHILD_11
(pmm, knn(5) include ( AGE_YOUNG_CHILD_2 AGE_YOUNG_CHILD_3 AGE_YOUNG_CHILD_4 AGE_YOUNG_CHILD_5 AGE_YOUNG_CHILD_6 AGE_YOUNG_CHILD_7 AGE_YOUNG_CHILD_8 AGE_YOUNG_CHILD_9 AGE_YOUNG_CHILD_10 AGE_YOUNG_CHILD_11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 i.house_status_all12 i.REGION_12 earnings_t_focal12 num_children_imp_hh12 i.religion_focal12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) AGE_YOUNG_CHILD_12


/* Partnership Status */
(pmm, knn(5) include (           i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 i.house_status_all2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.religion_focal2 earnings_t_focal2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) partnered_imp2
(pmm, knn(5) include (          i.partnered_imp2 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 i.house_status_all3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.religion_focal3 earnings_t_focal3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) partnered_imp3
(pmm, knn(5) include (         i.partnered_imp2 i.partnered_imp3 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 i.house_status_all4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.religion_focal4 earnings_t_focal4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) partnered_imp4
(pmm, knn(5) include (        i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 i.house_status_all5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.religion_focal5 earnings_t_focal5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) partnered_imp5
(pmm, knn(5) include (       i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 i.house_status_all6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.religion_focal6 earnings_t_focal6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) partnered_imp6
(pmm, knn(5) include (      i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 i.house_status_all7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.religion_focal7 earnings_t_focal7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) partnered_imp7
(pmm, knn(5) include (     i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 i.house_status_all8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.religion_focal8 earnings_t_focal8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) partnered_imp8
(pmm, knn(5) include (    i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp10 i.partnered_imp11 i.partnered_imp12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 i.house_status_all9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.religion_focal9 earnings_t_focal9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) partnered_imp9
(pmm, knn(5) include (   i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp11 i.partnered_imp12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 i.house_status_all10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.religion_focal10 earnings_t_focal10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) partnered_imp10
(pmm, knn(5) include (  i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 i.house_status_all11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.religion_focal11 earnings_t_focal11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) partnered_imp11
(pmm, knn(5) include ( i.partnered_imp2 i.partnered_imp3 i.partnered_imp4 i.partnered_imp5 i.partnered_imp6 i.partnered_imp7 i.partnered_imp8 i.partnered_imp9 i.partnered_imp10 i.partnered_imp11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 i.house_status_all12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.religion_focal12 earnings_t_focal12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) partnered_imp12


/* Home Ownership*/
(pmm, knn(5) include (           i.house_status_all3 i.house_status_all4                             )) house_status_all2
(pmm, knn(5) include (          i.house_status_all2 i.house_status_all4 i.house_status_all5                             )) house_status_all3
(pmm, knn(5) include (         i.house_status_all2 i.house_status_all3 i.house_status_all5 i.house_status_all6                             )) house_status_all4
(pmm, knn(5) include (         i.house_status_all3 i.house_status_all4 i.house_status_all6 i.house_status_all7                             )) house_status_all5
(pmm, knn(5) include (         i.house_status_all4 i.house_status_all5 i.house_status_all7 i.house_status_all8                             )) house_status_all6
(pmm, knn(5) include (         i.house_status_all5 i.house_status_all6 i.house_status_all8 i.house_status_all9                             )) house_status_all7
(pmm, knn(5) include (         i.house_status_all6 i.house_status_all7 i.house_status_all9 i.house_status_all10                             )) house_status_all8
(pmm, knn(5) include (         i.house_status_all7 i.house_status_all8 i.house_status_all10 i.house_status_all11                             )) house_status_all9
(pmm, knn(5) include (         i.house_status_all8 i.house_status_all9 i.house_status_all11 i.house_status_all12                             )) house_status_all10
(pmm, knn(5) include (         i.house_status_all9 i.house_status_all10 i.house_status_all12                              ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education  i.sample_type birth_timing_rel current_rel_number_main)) house_status_all11
(pmm, knn(5) include (         i.house_status_all10 i.house_status_all11                               ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) house_status_all12


/* Region*/
(pmm, knn(5) include (           i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 earnings_t_focal2 i.religion_focal2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.house_status_all2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) REGION_2
(pmm, knn(5) include (          i.REGION_2 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 earnings_t_focal3 i.religion_focal3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.house_status_all3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) REGION_3
(pmm, knn(5) include (         i.REGION_2 i.REGION_3 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 earnings_t_focal4 i.religion_focal4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.house_status_all4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) REGION_4
(pmm, knn(5) include (        i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 earnings_t_focal5 i.religion_focal5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.house_status_all5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) REGION_5
(pmm, knn(5) include (       i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 earnings_t_focal6 i.religion_focal6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.house_status_all6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) REGION_6
(pmm, knn(5) include (      i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 earnings_t_focal7 i.religion_focal7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.house_status_all7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) REGION_7
(pmm, knn(5) include (     i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_9 i.REGION_10 i.REGION_11 i.REGION_12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 earnings_t_focal8 i.religion_focal8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.house_status_all8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) REGION_8
(pmm, knn(5) include (    i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_10 i.REGION_11 i.REGION_12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 earnings_t_focal9 i.religion_focal9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.house_status_all9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) REGION_9
(pmm, knn(5) include (   i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_11 i.REGION_12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 earnings_t_focal10 i.religion_focal10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.house_status_all10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) REGION_10
(pmm, knn(5) include (  i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 earnings_t_focal11 i.religion_focal11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.house_status_all11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) REGION_11
(pmm, knn(5) include ( i.REGION_2 i.REGION_3 i.REGION_4 i.REGION_5 i.REGION_6 i.REGION_7 i.REGION_8 i.REGION_9 i.REGION_10 i.REGION_11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 earnings_t_focal12 i.religion_focal12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.house_status_all12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) REGION_12



/* Family Income */
(pmm, knn(5) include (           TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 i.house_status_all2 i.REGION_2 earnings_t_focal2 num_children_imp_hh2 i.religion_focal2 i.partnered_imp2 AGE_YOUNG_CHILD_2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) TOTAL_INCOME_T_FAMILY2
(pmm, knn(5) include (          TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 i.house_status_all3 i.REGION_3 earnings_t_focal3 num_children_imp_hh3 i.religion_focal3 i.partnered_imp3 AGE_YOUNG_CHILD_3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) TOTAL_INCOME_T_FAMILY3
(pmm, knn(5) include (         TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 i.house_status_all4 i.REGION_4 earnings_t_focal4 num_children_imp_hh4 i.religion_focal4 i.partnered_imp4 AGE_YOUNG_CHILD_4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) TOTAL_INCOME_T_FAMILY4
(pmm, knn(5) include (        TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 i.house_status_all5 i.REGION_5 earnings_t_focal5 num_children_imp_hh5 i.religion_focal5 i.partnered_imp5 AGE_YOUNG_CHILD_5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) TOTAL_INCOME_T_FAMILY5
(pmm, knn(5) include (       TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 i.house_status_all6 i.REGION_6 earnings_t_focal6 num_children_imp_hh6 i.religion_focal6 i.partnered_imp6 AGE_YOUNG_CHILD_6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) TOTAL_INCOME_T_FAMILY6
(pmm, knn(5) include (      TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 i.house_status_all7 i.REGION_7 earnings_t_focal7 num_children_imp_hh7 i.religion_focal7 i.partnered_imp7 AGE_YOUNG_CHILD_7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) TOTAL_INCOME_T_FAMILY7
(pmm, knn(5) include (     TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 i.house_status_all8 i.REGION_8 earnings_t_focal8 num_children_imp_hh8 i.religion_focal8 i.partnered_imp8 AGE_YOUNG_CHILD_8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) TOTAL_INCOME_T_FAMILY8
(pmm, knn(5) include (    TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 i.house_status_all9 i.REGION_9 earnings_t_focal9 num_children_imp_hh9 i.religion_focal9 i.partnered_imp9 AGE_YOUNG_CHILD_9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) TOTAL_INCOME_T_FAMILY9
(pmm, knn(5) include (   TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY11 TOTAL_INCOME_T_FAMILY12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 i.house_status_all10 i.REGION_10 earnings_t_focal10 num_children_imp_hh10 i.religion_focal10 i.partnered_imp10 AGE_YOUNG_CHILD_10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) TOTAL_INCOME_T_FAMILY10
(pmm, knn(5) include (  TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 i.house_status_all11 i.REGION_11 earnings_t_focal11 num_children_imp_hh11 i.religion_focal11 i.partnered_imp11 AGE_YOUNG_CHILD_11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) TOTAL_INCOME_T_FAMILY11
(pmm, knn(5) include ( TOTAL_INCOME_T_FAMILY2 TOTAL_INCOME_T_FAMILY3 TOTAL_INCOME_T_FAMILY4 TOTAL_INCOME_T_FAMILY5 TOTAL_INCOME_T_FAMILY6 TOTAL_INCOME_T_FAMILY7 TOTAL_INCOME_T_FAMILY8 TOTAL_INCOME_T_FAMILY9 TOTAL_INCOME_T_FAMILY10 TOTAL_INCOME_T_FAMILY11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 i.house_status_all12 i.REGION_12 earnings_t_focal12 num_children_imp_hh12 i.religion_focal12 i.partnered_imp12 AGE_YOUNG_CHILD_12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) TOTAL_INCOME_T_FAMILY12


/* Religion */
(pmm, knn(5) include (           i.religion_focal3 i.religion_focal4 i.religion_focal5 i.religion_focal6 i.religion_focal7 i.religion_focal8 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 earnings_t_focal2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.house_status_all2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) religion_focal2
(pmm, knn(5) include (          i.religion_focal2 i.religion_focal4 i.religion_focal5 i.religion_focal6 i.religion_focal7 i.religion_focal8 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 earnings_t_focal3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.house_status_all3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) religion_focal3
(pmm, knn(5) include (         i.religion_focal2 i.religion_focal3 i.religion_focal5 i.religion_focal6 i.religion_focal7 i.religion_focal8 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 earnings_t_focal4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.house_status_all4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) religion_focal4
(pmm, knn(5) include (        i.religion_focal2 i.religion_focal3 i.religion_focal4 i.religion_focal6 i.religion_focal7 i.religion_focal8 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 earnings_t_focal5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.house_status_all5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) religion_focal5
(pmm, knn(5) include (       i.religion_focal2 i.religion_focal3 i.religion_focal4 i.religion_focal5 i.religion_focal7 i.religion_focal8 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 earnings_t_focal6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.house_status_all6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) religion_focal6
(pmm, knn(5) include (      i.religion_focal2 i.religion_focal3 i.religion_focal4 i.religion_focal5 i.religion_focal6 i.religion_focal8 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 earnings_t_focal7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.house_status_all7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) religion_focal7
(pmm, knn(5) include (     i.religion_focal2 i.religion_focal3 i.religion_focal4 i.religion_focal5 i.religion_focal6 i.religion_focal7 i.religion_focal9 i.religion_focal10 i.religion_focal11 i.religion_focal12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 earnings_t_focal8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.house_status_all8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) religion_focal8
(pmm, knn(5) include (    i.religion_focal2 i.religion_focal3 i.religion_focal4 i.religion_focal5 i.religion_focal6 i.religion_focal7 i.religion_focal8 i.religion_focal10 i.religion_focal11 i.religion_focal12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 earnings_t_focal9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.house_status_all9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) religion_focal9
(pmm, knn(5) include (   i.religion_focal2 i.religion_focal3 i.religion_focal4 i.religion_focal5 i.religion_focal6 i.religion_focal7 i.religion_focal8 i.religion_focal9 i.religion_focal11 i.religion_focal12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 earnings_t_focal10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.house_status_all10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) religion_focal10
(pmm, knn(5) include (         i.religion_focal9 i.religion_focal10 i.religion_focal12                              ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) religion_focal11
(pmm, knn(5) include (         i.religion_focal10 i.religion_focal11                               ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) religion_focal12


/* Disability Status */
/*
(pmm, knn(5) include (           i.disabled_imp_focal3 i.disabled_imp_focal4                             )) disabled_imp_focal2
(pmm, knn(5) include (          i.disabled_imp_focal2 i.disabled_imp_focal4 i.disabled_imp_focal5                             )) disabled_imp_focal3
(pmm, knn(5) include (         i.disabled_imp_focal2 i.disabled_imp_focal3 i.disabled_imp_focal5 i.disabled_imp_focal6                             )) disabled_imp_focal4
(pmm, knn(5) include (         i.disabled_imp_focal3 i.disabled_imp_focal4 i.disabled_imp_focal6 i.disabled_imp_focal7                             )) disabled_imp_focal5
(pmm, knn(5) include (         i.disabled_imp_focal4 i.disabled_imp_focal5 i.disabled_imp_focal7 i.disabled_imp_focal8                             )) disabled_imp_focal6
(pmm, knn(5) include (         i.disabled_imp_focal5 i.disabled_imp_focal6 i.disabled_imp_focal8 i.disabled_imp_focal9                             )) disabled_imp_focal7
(pmm, knn(5) include (         i.disabled_imp_focal6 i.disabled_imp_focal7 i.disabled_imp_focal9 i.disabled_imp_focal10                             )) disabled_imp_focal8
(pmm, knn(5) include (         i.disabled_imp_focal7 i.disabled_imp_focal8 i.disabled_imp_focal10 i.disabled_imp_focal11                             )) disabled_imp_focal9
(pmm, knn(5) include (         i.disabled_imp_focal8 i.disabled_imp_focal9 i.disabled_imp_focal11 i.disabled_imp_focal12                             ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) disabled_imp_focal10
(pmm, knn(5) include (         i.disabled_imp_focal9 i.disabled_imp_focal10 i.disabled_imp_focal12                              ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) disabled_imp_focal11
(pmm, knn(5) include (         i.disabled_imp_focal10 i.disabled_imp_focal11                               ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) disabled_imp_focal12
*/

/* Self-rated Health */
(pmm, knn(5) include (           i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 earnings_t_focal2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.house_status_all2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.religion_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) sr_health_focal2
(pmm, knn(5) include (          i.sr_health_focal2 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 earnings_t_focal3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.house_status_all3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.religion_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) sr_health_focal3
(pmm, knn(5) include (         i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 earnings_t_focal4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.house_status_all4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.religion_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) sr_health_focal4
(pmm, knn(5) include (        i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 earnings_t_focal5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.house_status_all5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.religion_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) sr_health_focal5
(pmm, knn(5) include (       i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 earnings_t_focal6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.house_status_all6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.religion_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) sr_health_focal6
(pmm, knn(5) include (      i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 earnings_t_focal7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.house_status_all7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.religion_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) sr_health_focal7
(pmm, knn(5) include (     i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 earnings_t_focal8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.house_status_all8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.religion_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) sr_health_focal8
(pmm, knn(5) include (    i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal10 i.sr_health_focal11 i.sr_health_focal12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 earnings_t_focal9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.house_status_all9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.religion_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) sr_health_focal9
(pmm, knn(5) include (   i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal11 i.sr_health_focal12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 earnings_t_focal10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.house_status_all10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.religion_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) sr_health_focal10
(pmm, knn(5) include (  i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 earnings_t_focal11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.house_status_all11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.religion_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) sr_health_focal11
(pmm, knn(5) include ( i.sr_health_focal2 i.sr_health_focal3 i.sr_health_focal4 i.sr_health_focal5 i.sr_health_focal6 i.sr_health_focal7 i.sr_health_focal8 i.sr_health_focal9 i.sr_health_focal10 i.sr_health_focal11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 earnings_t_focal12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.house_status_all12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.religion_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) sr_health_focal12


/* 65+ in HH */
(pmm, knn(5) include (           num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 earnings_t_focal2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.house_status_all2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.religion_focal2 i.num_parent_in_hh2 i.lives_family_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) num_65up_hh2
(pmm, knn(5) include (          num_65up_hh2 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 earnings_t_focal3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.house_status_all3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.religion_focal3 i.num_parent_in_hh3 i.lives_family_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) num_65up_hh3
(pmm, knn(5) include (         num_65up_hh2 num_65up_hh3 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 earnings_t_focal4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.house_status_all4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.religion_focal4 i.num_parent_in_hh4 i.lives_family_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) num_65up_hh4
(pmm, knn(5) include (        num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 earnings_t_focal5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.house_status_all5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.religion_focal5 i.num_parent_in_hh5 i.lives_family_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) num_65up_hh5
(pmm, knn(5) include (       num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 earnings_t_focal6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.house_status_all6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.religion_focal6 i.num_parent_in_hh6 i.lives_family_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) num_65up_hh6
(pmm, knn(5) include (      num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 earnings_t_focal7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.house_status_all7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.religion_focal7 i.num_parent_in_hh7 i.lives_family_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) num_65up_hh7
(pmm, knn(5) include (     num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh9 num_65up_hh10 num_65up_hh11 num_65up_hh12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 earnings_t_focal8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.house_status_all8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.religion_focal8 i.num_parent_in_hh8 i.lives_family_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) num_65up_hh8
(pmm, knn(5) include (    num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh10 num_65up_hh11 num_65up_hh12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 earnings_t_focal9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.house_status_all9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.religion_focal9 i.num_parent_in_hh9 i.lives_family_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) num_65up_hh9
(pmm, knn(5) include (   num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh11 num_65up_hh12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 earnings_t_focal10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.house_status_all10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.religion_focal10 i.num_parent_in_hh10 i.lives_family_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) num_65up_hh10
(pmm, knn(5) include (  num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 earnings_t_focal11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.house_status_all11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.religion_focal11 i.num_parent_in_hh11 i.lives_family_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) num_65up_hh11
(pmm, knn(5) include ( num_65up_hh2 num_65up_hh3 num_65up_hh4 num_65up_hh5 num_65up_hh6 num_65up_hh7 num_65up_hh8 num_65up_hh9 num_65up_hh10 num_65up_hh11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 earnings_t_focal12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.house_status_all12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.religion_focal12 i.num_parent_in_hh12 i.lives_family_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) num_65up_hh12


/* Coreside with parents */
(pmm, knn(5) include (           i.num_parent_in_hh3 i.num_parent_in_hh4                             )) num_parent_in_hh2
(pmm, knn(5) include (          i.num_parent_in_hh2 i.num_parent_in_hh4 i.num_parent_in_hh5                             )) num_parent_in_hh3
(pmm, knn(5) include (         i.num_parent_in_hh2 i.num_parent_in_hh3 i.num_parent_in_hh5 i.num_parent_in_hh6                             )) num_parent_in_hh4
(pmm, knn(5) include (         i.num_parent_in_hh3 i.num_parent_in_hh4 i.num_parent_in_hh6 i.num_parent_in_hh7                             )) num_parent_in_hh5
(pmm, knn(5) include (         i.num_parent_in_hh4 i.num_parent_in_hh5 i.num_parent_in_hh7 i.num_parent_in_hh8                             )) num_parent_in_hh6
(pmm, knn(5) include (         i.num_parent_in_hh5 i.num_parent_in_hh6 i.num_parent_in_hh8 i.num_parent_in_hh9                             )) num_parent_in_hh7
(pmm, knn(5) include (         i.num_parent_in_hh6 i.num_parent_in_hh7 i.num_parent_in_hh9 i.num_parent_in_hh10                             )) num_parent_in_hh8
(pmm, knn(5) include (          i.num_parent_in_hh8 i.num_parent_in_hh10                              ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) num_parent_in_hh9
(pmm, knn(5) include (          i.num_parent_in_hh9 i.num_parent_in_hh11                              ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) num_parent_in_hh10
(pmm, knn(5) include (          i.num_parent_in_hh10 i.num_parent_in_hh12                              ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) num_parent_in_hh11
(pmm, knn(5) include (          i.num_parent_in_hh11                               ) omit (birth_yr_all i.raceth_fixed_focal i.fixed_education i.sample_type birth_timing_rel current_rel_number_main)) num_parent_in_hh12


/* Live in same region as childhood home */
(pmm, knn(5) include (           i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12 weekly_hrs_t_focal2 i.employment_status_focal2 housework_focal2 earnings_t_focal2 i.REGION_2 num_children_imp_hh2 AGE_YOUNG_CHILD_2 i.house_status_all2 i.partnered_imp2 TOTAL_INCOME_T_FAMILY2  i.sr_health_focal2 i.num_65up_hh2 i.num_parent_in_hh2 i.religion_focal2 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status2 i.RESPONDENT_WHO_2 i.retired_est_focal2)) lives_family_focal2
(pmm, knn(5) include (          i.lives_family_focal2 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12  weekly_hrs_t_focal3 i.employment_status_focal3 housework_focal3 earnings_t_focal3 i.REGION_3 num_children_imp_hh3 AGE_YOUNG_CHILD_3 i.house_status_all3 i.partnered_imp3 TOTAL_INCOME_T_FAMILY3  i.sr_health_focal3 i.num_65up_hh3 i.num_parent_in_hh3 i.religion_focal3 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status3 i.RESPONDENT_WHO_3 i.retired_est_focal3)) lives_family_focal3
(pmm, knn(5) include (         i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12   weekly_hrs_t_focal4 i.employment_status_focal4 housework_focal4 earnings_t_focal4 i.REGION_4 num_children_imp_hh4 AGE_YOUNG_CHILD_4 i.house_status_all4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4  i.sr_health_focal4 i.num_65up_hh4 i.num_parent_in_hh4 i.religion_focal4 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status4 i.RESPONDENT_WHO_4 i.retired_est_focal4)) lives_family_focal4
(pmm, knn(5) include (        i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12    weekly_hrs_t_focal5 i.employment_status_focal5 housework_focal5 earnings_t_focal5 i.REGION_5 num_children_imp_hh5 AGE_YOUNG_CHILD_5 i.house_status_all5 i.partnered_imp5 TOTAL_INCOME_T_FAMILY5  i.sr_health_focal5 i.num_65up_hh5 i.num_parent_in_hh5 i.religion_focal5 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status5 i.RESPONDENT_WHO_5 i.retired_est_focal5)) lives_family_focal5
(pmm, knn(5) include (       i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12     weekly_hrs_t_focal6 i.employment_status_focal6 housework_focal6 earnings_t_focal6 i.REGION_6 num_children_imp_hh6 AGE_YOUNG_CHILD_6 i.house_status_all6 i.partnered_imp6 TOTAL_INCOME_T_FAMILY6  i.sr_health_focal6 i.num_65up_hh6 i.num_parent_in_hh6 i.religion_focal6 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status6 i.RESPONDENT_WHO_6 i.retired_est_focal6)) lives_family_focal6
(pmm, knn(5) include (      i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12      weekly_hrs_t_focal7 i.employment_status_focal7 housework_focal7 earnings_t_focal7 i.REGION_7 num_children_imp_hh7 AGE_YOUNG_CHILD_7 i.house_status_all7 i.partnered_imp7 TOTAL_INCOME_T_FAMILY7  i.sr_health_focal7 i.num_65up_hh7 i.num_parent_in_hh7 i.religion_focal7 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status7 i.RESPONDENT_WHO_7 i.retired_est_focal7)) lives_family_focal7
(pmm, knn(5) include (     i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12       weekly_hrs_t_focal8 i.employment_status_focal8 housework_focal8 earnings_t_focal8 i.REGION_8 num_children_imp_hh8 AGE_YOUNG_CHILD_8 i.house_status_all8 i.partnered_imp8 TOTAL_INCOME_T_FAMILY8  i.sr_health_focal8 i.num_65up_hh8 i.num_parent_in_hh8 i.religion_focal8 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status8 i.RESPONDENT_WHO_8 i.retired_est_focal8)) lives_family_focal8
(pmm, knn(5) include (    i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal10 i.lives_family_focal11 i.lives_family_focal12        weekly_hrs_t_focal9 i.employment_status_focal9 housework_focal9 earnings_t_focal9 i.REGION_9 num_children_imp_hh9 AGE_YOUNG_CHILD_9 i.house_status_all9 i.partnered_imp9 TOTAL_INCOME_T_FAMILY9  i.sr_health_focal9 i.num_65up_hh9 i.num_parent_in_hh9 i.religion_focal9 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status9 i.RESPONDENT_WHO_9 i.retired_est_focal9)) lives_family_focal9
(pmm, knn(5) include (   i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal11 i.lives_family_focal12         weekly_hrs_t_focal10 i.employment_status_focal10 housework_focal10 earnings_t_focal10 i.REGION_10 num_children_imp_hh10 AGE_YOUNG_CHILD_10 i.house_status_all10 i.partnered_imp10 TOTAL_INCOME_T_FAMILY10  i.sr_health_focal10 i.num_65up_hh10 i.num_parent_in_hh10 i.religion_focal10 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status10 i.RESPONDENT_WHO_10 i.retired_est_focal10)) lives_family_focal10
(pmm, knn(5) include (  i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal12          weekly_hrs_t_focal11 i.employment_status_focal11 housework_focal11 earnings_t_focal11 i.REGION_11 num_children_imp_hh11 AGE_YOUNG_CHILD_11 i.house_status_all11 i.partnered_imp11 TOTAL_INCOME_T_FAMILY11  i.sr_health_focal11 i.num_65up_hh11 i.num_parent_in_hh11 i.religion_focal11 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status11 i.RESPONDENT_WHO_11 i.retired_est_focal11)) lives_family_focal11
(pmm, knn(5) include ( i.lives_family_focal2 i.lives_family_focal3 i.lives_family_focal4 i.lives_family_focal5 i.lives_family_focal6 i.lives_family_focal7 i.lives_family_focal8 i.lives_family_focal9 i.lives_family_focal10 i.lives_family_focal11           weekly_hrs_t_focal12 i.employment_status_focal12 housework_focal12 earnings_t_focal12 i.REGION_12 num_children_imp_hh12 AGE_YOUNG_CHILD_12 i.house_status_all12 i.partnered_imp12 TOTAL_INCOME_T_FAMILY12  i.sr_health_focal12 i.num_65up_hh12 i.num_parent_in_hh12 i.religion_focal12 i.father_max_educ_focal i.mother_max_educ_focal i.family_structure_cons_focal i.current_parent_status12 i.RESPONDENT_WHO_12 i.retired_est_focal12)) lives_family_focal12


/* Fixed covariates - will mainly impute using the other constant variables */
(pmm, knn(5) include (                                           i.mother_max_educ_focal i.family_structure_cons_focal   )) father_max_educ_focal
(pmm, knn(5) include (                                           i.father_max_educ_focal i.family_structure_cons_focal   )) mother_max_educ_focal
(pmm, knn(5) include (                                           i.father_max_educ_focal i.mother_max_educ_focal  )) family_structure_cons_focal


= birth_yr_all i.raceth_fixed_focal i.sample_type i.fixed_education birth_timing_rel current_rel_number_main, by(SEX) chaindots add(10) rseed(12345) noimputed augment force showcommand noisily // skipnonconvergence(2)  //  dryrun i.rel_type_constant i.rel_start_all  FIRST_BIRTH_YR 

;
#delimit cr

// log close

// = i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all i.raceth_fixed_focal i.sample_type i.SEX, chaindots add(1) rseed(12345) noimputed augment noisily force // dryrun

save "$created_data/psid_individs_imputed_wide_bysex", replace // okay, now that this worked with more variables, will replace above version

// reshape back to long to look at descriptives
mi reshape long in_sample_ hh_status_ relationship_  partnered weekly_hrs_t1_focal earnings_t1_focal housework_focal employed_focal educ_focal max_educ_focal educ_focal_imp college_focal age_focal weekly_hrs_t2_focal earnings_t2_focal employed_t2_focal start_yr_employer_focal yrs_employer_focal children FAMILY_INTERVIEW_NUM_ NUM_CHILDREN_ AGE_YOUNG_CHILD_ TOTAL_INCOME_T1_FAMILY_ hours_type_t1_focal hw_hours_gp raceth_focal weekly_hrs_t_focal earnings_t_focal TOTAL_INCOME_T_FAMILY childcare_focal adultcare_focal TOTAL_INCOME_T2_FAMILY_ num_children_imp_focal num_children_imp_hh partnered_imp rolling_births had_birth increment_birth hh_births_ under18 edulevel_match edulevelmax_match new_in_hh has_hours_t1 has_hours_t2 has_earnings_t1 has_earnings_t2 employed_t1_earn_focal employed_t1_hrs_focal employed_focal_rec disabled_focal empstat_disabled_focal disabled_scale_focal disabled_imp_focal sr_health_focal yr_retired_focal empstat_retired_focal retired_est_focal num_65up_hh age65up house_status_all moved_in_lastyr moved_mo_lastyr moved_yr_lastyr religion_focal lives_family_focal RESPONDENT_WHO_ REGION_ employment_status_focal current_parent_status num_parent_in_hh father_in_hh mother_in_hh ///
, i(couple_id unique_id partner_id rel_start_all min_dur max_dur rel_end_all last_yr_observed ended SEX) j(duration_rec)

mi convert flong

browse couple_id unique_id partner_id duration_rec weekly_hrs_t_focal housework_focal _mi_miss _mi_m _mi_id
gen imputed=0
replace imputed=1 if inrange(_mi_m,1,10)

inspect weekly_hrs_t_focal if imputed==0
inspect weekly_hrs_t_focal if imputed==1

inspect housework_focal if imputed==0
inspect housework_focal if imputed==1

// recode some vars for ease
mi rename AGE_YOUNG_CHILD_ age_young_child
mi rename TOTAL_INCOME_T_FAMILY family_income_t
mi rename relationship_ relationship
mi rename in_sample_ in_sample
mi rename hh_status_ hh_status

mi update

// mi register regular n

save "$created_data/psid_individs_imputed_long_bysex", replace

********************************************************************************
********************************************************************************
**# * Imputation descriptives
********************************************************************************
********************************************************************************

tabstat weekly_hrs_t_focal housework_focal, by(imputed) stats(mean sd p50)
tabstat weekly_hrs_t_focal housework_focal if SEX==1, by(imputed) stats(mean sd p50)
tabstat weekly_hrs_t_focal housework_focal if SEX==2, by(imputed) stats(mean sd p50)

tabstat weekly_hrs_t_focal housework_focal employment_status_focal earnings_t_focal num_children_imp_hh age_young_child partnered_imp num_65up_hh num_parent_in_hh family_income_t REGION_ lives_family_focal house_status_all religion_focal disabled_focal sr_health_focal father_max_educ_focal mother_max_educ_focal family_structure_cons_focal, by(imputed) stats(mean sd p50) columns(statistics)

histogram weekly_hrs_t_focal, width(1)
twoway (histogram weekly_hrs_t_focal if imputed==0, width(2) color(blue%30)) (histogram weekly_hrs_t_focal if imputed==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Employment Hours")
twoway (histogram weekly_hrs_t_focal if imputed==0 & SEX==1, width(2) color(blue%30)) (histogram weekly_hrs_t_focal if imputed==1 & SEX==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (histogram weekly_hrs_t_focal if imputed==0 & SEX==2, width(2) color(blue%30)) (histogram weekly_hrs_t_focal if imputed==1 & SEX==2, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

histogram weekly_hrs_t_focal if weekly_hrs_t1_focal>0, width(1)
histogram housework_focal, width(1)
twoway (histogram housework_focal if imputed==0 & housework_focal <=50, width(2) color(blue%30)) (histogram housework_focal if imputed==1 & housework_focal <=50, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Housework Hours")

preserve

collapse (mean) weekly_hrs_t_focal housework_focal, by(duration_rec imputed)

twoway (line weekly_hrs_t_focal duration_rec if imputed==0 & duration_rec >=2 & duration_rec<=12) (line weekly_hrs_t_focal duration_rec if imputed==1 & duration_rec >=2 & duration_rec<=12), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) ytitle("Weekly Employment Hours") title("Avg Employment Hours by Duration") xtitle("Marital Duration") yscale(range(32 38))
twoway (line housework_focal duration_rec if imputed==0 & duration_rec >=2 & duration_rec<=12) (line housework_focal duration_rec if imputed==1 & duration_rec >=2 & duration_rec<=12), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) ytitle("Weekly Housework Hours") title("Avg Housework Hours by Duration") xtitle("Marital Duration") yscale(range(10 15))

restore

//
preserve

collapse (mean) weekly_hrs_t_focal housework_focal, by(SEX duration_rec imputed)

// men
twoway (line weekly_hrs_t_focal duration_rec if imputed==0 & SEX==1) (line weekly_hrs_t_focal duration_rec if imputed==1 & SEX==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0 & SEX==1) (line housework_focal duration_rec if imputed==1 & SEX==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

// women - okay so it is women where the disparities are primarily. is it bc of EMPLOYMENT STATUS?! need to do conditional on that? let's see if it improves with other predictors, bc employment status not currently included
twoway (line weekly_hrs_t_focal duration_rec if imputed==0 & SEX==2) (line weekly_hrs_t_focal duration_rec if imputed==1 & SEX==2), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))
twoway (line housework_focal duration_rec if imputed==0 & SEX==2) (line housework_focal duration_rec if imputed==1 & SEX==2), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6))

restore

********************************************************************************
**# Going to make a table so I can compare the categorical variables
********************************************************************************
use "$created_data/psid_individs_imputed_long_bysex", clear

keep if duration_rec >=2 & duration_rec <=12

tab employment_status_focal, gen(emp_status) // 9
tab REGION_, gen(region) // 6
tab lives_family_focal, gen(lives_fam) // 3
tab house_status_all, gen(own) // 3
tab religion_focal, gen(religion) // 31
tab sr_health_focal, gen(health) // 5 
tab father_max_educ_focal, gen(dad_educ) // 9
tab mother_max_educ_focal, gen(mom_educ) // 9

putexcel set "$results/PSID_imputation_descriptives", replace
putexcel B1:C1 = "Duration: All", merge
putexcel D1:E1 = "Duration: 0", merge
putexcel F1:G1 = "Duration: 5", merge
putexcel H1:I1 = "Duration: 10", merge
putexcel A2 = "Variable"
putexcel B2 = ("Not Imputed") D2 = ("Not Imputed") F2 = ("Not Imputed") H2 = ("Not Imputed") 
putexcel C2 = ("Imputed") E2 = ("Imputed") G2 = ("Imputed") I2 = ("Imputed")

// Means
putexcel A3 = "Paid Work Hours (Weekly)"
putexcel A4 = "Unpaid Work Hours (Weekly)"
putexcel A5 = "0 Under 18"
putexcel A6 = "1 Working"
putexcel A7 = "2 Temp laid off"
putexcel A8 = "3 Unemployed"
putexcel A9 = "4 Retired"
putexcel A10 = "5 Disabled"
putexcel A11 = "6 Housewife"
putexcel A12 = "7 Student"
putexcel A13 = "8 Other"
putexcel A14 = "Earnings (Annual)"
putexcel A15 = "Number of children"
putexcel A16 = "Age of youngest child"
putexcel A17 = "Partnered"
putexcel A18 = "Number of people aged 65+ in HH"
putexcel A19 = "Coresidence with elder parents (N, 1, or both))"
putexcel A20 = "Family income"
putexcel A21 = "1 Northeast"
putexcel A22 = "2 North Central"
putexcel A23 = "3 South"
putexcel A24 = "4 West"
putexcel A25 = "5 AK / HI"
putexcel A26 = "6 Foreign"
putexcel A27 = "1 Same state"
putexcel A28 = "2 Same region"
putexcel A29 = "3 Diff region"
putexcel A30 = "0 Neither"
putexcel A31 = "1 Rents"
putexcel A32 = "2 Owns"
putexcel A33 = "0 No religion"
putexcel A34 = "1 Atheist"
putexcel A35 = "2 Agnostic"
putexcel A36 = "3 Catholic"
putexcel A37 = "4 Jewish"
putexcel A38 = "5 Greek Orthodox"
putexcel A39 = "6 Baptist"
putexcel A40 = "7 Episcopalian"
putexcel A41 = "8 Jehovah's Witness"
putexcel A42 = "9 Lutheran"
putexcel A43 = "10 Methodist"
putexcel A44 = "11 Pentecostal"
putexcel A45 = "12 Presbyterian"
putexcel A46 = "13 Protestant unspecified"
putexcel A47 = "14 Other Protestant"
putexcel A48 = "15 Other Christian"
putexcel A49 = "16 Muslim"
putexcel A50 = "17 Buddhist"
putexcel A51 = "18 Other non-Christian"
putexcel A52 = "19 LDS"
putexcel A53 = "20 Unitarian"
putexcel A54 = "21 Christian Science"
putexcel A55 = "22 Seventh Day Adventist"
putexcel A56 = "23 Amish"
putexcel A57 = "24 Quaker"
putexcel A58 = "25 Church of God"
putexcel A59 = "26 United Church or Christ"
putexcel A60 = "27 Reformed"
putexcel A61 = "28 Disciples of Christ"
putexcel A62 = "29 Churches of Christ"
putexcel A63 = "30 Other Other"
putexcel A64 = "Disability status (Y/N)"
putexcel A65 = "1 Excellent"
putexcel A66 = "2 Very Good"
putexcel A67 = "3 Good"
putexcel A68 = "4 Fair"
putexcel A69 = "5 Poor"
putexcel A70 = "Father's educ: 0 none"
putexcel A71 = "Father's educ: 1 0-5th grade"
putexcel A72 = "Father's educ: 2 6-8th grade"
putexcel A73 = "Father's educ: 3 9-11th grade"
putexcel A74 = "Father's educ: 4 high school"
putexcel A75 = "Father's educ: 5 12+"
putexcel A76 = "Father's educ: 6 some college"
putexcel A77 = "Father's educ: 7 BA"
putexcel A78 = "Father's educ: 8 advanced degree"
putexcel A79 = "Mother's educ: 0 none"
putexcel A80 = "Mother's educ: 1 0-5th grade"
putexcel A81 = "Mother's educ: 2 6-8th grade"
putexcel A82 = "Mother's educ: 3 9-11th grade"
putexcel A83 = "Mother's educ: 4 high school"
putexcel A84 = "Mother's educ: 5 12+"
putexcel A85 = "Mother's educ: 6 some college"
putexcel A86 = "Mother's educ: 7 BA"
putexcel A87 = "Mother's educ: 8 advanced degree"
putexcel A88 = "binary indicator of living with both parents at age 16"

local desc_vars "weekly_hrs_t_focal housework_focal emp_status1 emp_status2 emp_status3 emp_status4 emp_status5 emp_status6 emp_status7 emp_status8 emp_status9 earnings_t_focal num_children_imp_hh age_young_child partnered_imp num_65up_hh num_parent_in_hh family_income_t region1 region2 region3 region4 region5 region6 lives_fam1 lives_fam2 lives_fam3 own1 own2 own3 religion1 religion2 religion3 religion4 religion5 religion6 religion7 religion8 religion9 religion10 religion11 religion12 religion13 religion14 religion15 religion16 religion17 religion18 religion19 religion20 religion21 religion22 religion23 religion24 religion25 religion26 religion27 religion28 religion29 religion30 religion31 disabled_focal health1 health2 health3 health4 health5 dad_educ1 dad_educ2 dad_educ3 dad_educ4 dad_educ5 dad_educ6 dad_educ7 dad_educ8 dad_educ9 mom_educ1 mom_educ2 mom_educ3 mom_educ4 mom_educ5 mom_educ6 mom_educ7 mom_educ8 mom_educ9 family_structure_cons_focal" // 86

** All durations
// Not imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}

** Duration 0
// Not imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0 & duration_rec==2
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1  & duration_rec==2
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

** Duration 5
// Not imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0 & duration_rec==7
	matrix t`var'= e(b)
	putexcel F`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1  & duration_rec==7
	matrix t`var'= e(b)
	putexcel G`row' = matrix(t`var'), nformat(#.#%)
}

** Duration 10
// Not imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0 & duration_rec==12
	matrix t`var'= e(b)
	putexcel H`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/86{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1  & duration_rec==12
	matrix t`var'= e(b)
	putexcel I`row' = matrix(t`var'), nformat(#.#%)
}

********************************************************************************
********************************************************************************
**# * Troubleshooting area
********************************************************************************
********************************************************************************
/*
// https://www.statalist.org/forums/forum/general-stata-discussion/general/1618081-multiple-imputation-convergence-not-achieved
// https://www.statalist.org/forums/forum/general-stata-discussion/general/1595900-mi-impute-mlogit-and-convergence-not-achieved

********************************************************************************
* Can I get this to work with just one time varying variable?
********************************************************************************

mi set wide
mi register imputed weekly_hrs_t1_focal*
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all

#delimit ;

mi impute chained

(pmm, knn(5) include (                weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16)) weekly_hrs_t1_focal0
(pmm, knn(5) include (               weekly_hrs_t1_focal0 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16 )) weekly_hrs_t1_focal1
(pmm, knn(5) include (              weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16  )) weekly_hrs_t1_focal2
(pmm, knn(5) include (             weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16   )) weekly_hrs_t1_focal3
(pmm, knn(5) include (            weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16    )) weekly_hrs_t1_focal4
(pmm, knn(5) include (           weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16     )) weekly_hrs_t1_focal5
(pmm, knn(5) include (          weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16      )) weekly_hrs_t1_focal6
(pmm, knn(5) include (         weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16       )) weekly_hrs_t1_focal7
(pmm, knn(5) include (        weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16        )) weekly_hrs_t1_focal8
(pmm, knn(5) include (       weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16         )) weekly_hrs_t1_focal9
(pmm, knn(5) include (      weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16          )) weekly_hrs_t1_focal10
(pmm, knn(5) include (     weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16           )) weekly_hrs_t1_focal11
(pmm, knn(5) include (    weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16            )) weekly_hrs_t1_focal12
(pmm, knn(5) include (   weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16             )) weekly_hrs_t1_focal13
(pmm, knn(5) include (  weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16              )) weekly_hrs_t1_focal14
(pmm, knn(5) include ( weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal16               )) weekly_hrs_t1_focal15
(pmm, knn(5) include (weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15                )) weekly_hrs_t1_focal16

= i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all, chaindots force add(1) rseed(12345) noimputed // dryrun

;
#delimit cr

/*
do I need less predictors? NO you just needed to include ALL of the weekly hours variables 0 - 16
log using "$logdir/mi_troubleshoot.log", replace

mi set wide
mi register imputed weekly_hrs_t1_focal*
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all

// for LP:
mi describe
mi misstable summarize

// run your imputation model with the dryrun option. You can probably remove the augment option for now.

#delimit ;

mi impute chained

(pmm, knn(5) include (          weekly_hrs_t1_focal0 weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8        )) weekly_hrs_t1_focal4
(pmm, knn(5) include (          weekly_hrs_t1_focal1 weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9        )) weekly_hrs_t1_focal5
(pmm, knn(5) include (          weekly_hrs_t1_focal2 weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10        )) weekly_hrs_t1_focal6
(pmm, knn(5) include (          weekly_hrs_t1_focal3 weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11        )) weekly_hrs_t1_focal7
(pmm, knn(5) include (          weekly_hrs_t1_focal4 weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12        )) weekly_hrs_t1_focal8
(pmm, knn(5) include (          weekly_hrs_t1_focal5 weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13        )) weekly_hrs_t1_focal9
(pmm, knn(5) include (          weekly_hrs_t1_focal6 weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14        )) weekly_hrs_t1_focal10
(pmm, knn(5) include (          weekly_hrs_t1_focal7 weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15        )) weekly_hrs_t1_focal11
(pmm, knn(5) include (          weekly_hrs_t1_focal8 weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal13 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16        )) weekly_hrs_t1_focal12
(pmm, knn(5) include (          weekly_hrs_t1_focal9 weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal14 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16         )) weekly_hrs_t1_focal13
(pmm, knn(5) include (          weekly_hrs_t1_focal10 weekly_hrs_t1_focal11 weekly_hrs_t1_focal12 weekly_hrs_t1_focal13 weekly_hrs_t1_focal15 weekly_hrs_t1_focal16          )) weekly_hrs_t1_focal14

= i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all, chaindots force add(10) rseed(12345) dryrun noimputed

;
#delimit cr

log close
*/

********************************************************************************
* employment status problems (bc binary) - trying with NO other time varying predictors
********************************************************************************
use "$created_data\individs_by_duration_wide.dta", clear

egen nmis_age = rmiss(age_focal*)
tab nmis_age, m

egen nmis_employ = rmiss(employed_focal*)
tab nmis_employ, m

drop if nmis_age==17 // for now, just so this is actually complete
drop if birth_yr_all==. // for now, just so this is actually complete
drop if raceth_fixed_focal==. // for now, just so this is actually complete

mi set wide
mi register imputed weekly_hrs_t_focal* housework_focal* employed_focal* earnings_t_focal* educ_focal* college_focal* children* NUM_CHILDREN_* AGE_YOUNG_CHILD_* relationship_* partnered* TOTAL_INCOME_T_FAMILY*
mi register regular FIRST_BIRTH_YR birth_yr_all rel_start_all SEX raceth_fixed_focal sample_type

// start imputation
#delimit ;

mi impute chained

(logit, include (                i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16)) employed_focal0
(logit, include (               i.employed_focal0 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16 )) employed_focal1
(logit, include (              i.employed_focal0 i.employed_focal1 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16  )) employed_focal2
(logit, include (             i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16   )) employed_focal3
(logit, include (            i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16    )) employed_focal4
(logit, include (           i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16     )) employed_focal5
(logit, include (          i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16      )) employed_focal6
(logit, include (         i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16       )) employed_focal7
(logit, include (        i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16        )) employed_focal8
(logit, include (       i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16         )) employed_focal9
(logit, include (      i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16          )) employed_focal10
(logit, include (     i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16           )) employed_focal11
(logit, include (    i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16            )) employed_focal12
(logit, include (   i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal14 i.employed_focal15 i.employed_focal16             )) employed_focal13
(logit, include (  i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal15 i.employed_focal16              )) employed_focal14
(logit, include ( i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal16               )) employed_focal15
(logit, include (i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15                )) employed_focal16

= FIRST_BIRTH_YR birth_yr_all i.raceth_fixed_focal i.sample_type i.SEX, chaindots add(1) rseed(12345) noimputed augment noisily force // dryrun rel_start_all

;
#delimit cr


// trying less forwards and lags
#delimit ;

mi impute chained

(logit, include (                i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5           )) employed_focal0
(logit, include (               i.employed_focal0 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6           )) employed_focal1
(logit, include (              i.employed_focal0 i.employed_focal1 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7           )) employed_focal2
(logit, include (             i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8           )) employed_focal3
(logit, include (            i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9           )) employed_focal4
(logit, include (           i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10           )) employed_focal5
(logit, include (           i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11           )) employed_focal6
(logit, include (           i.employed_focal2 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12           )) employed_focal7
(logit, include (           i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13           )) employed_focal8
(logit, include (           i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14           )) employed_focal9
(logit, include (           i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15           )) employed_focal10
(logit, include (           i.employed_focal6 i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16           )) employed_focal11
(logit, include (           i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal13 i.employed_focal14 i.employed_focal15 i.employed_focal16            )) employed_focal12
(logit, include (           i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal14 i.employed_focal15 i.employed_focal16             )) employed_focal13
(logit, include (           i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal15 i.employed_focal16              )) employed_focal14
(logit, include (           i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal16               )) employed_focal15
(logit, include (           i.employed_focal11 i.employed_focal12 i.employed_focal13 i.employed_focal14 i.employed_focal15                )) employed_focal16

= FIRST_BIRTH_YR birth_yr_all i.raceth_fixed_focal i.sample_type i.SEX, chaindots add(1) rseed(12345) noimputed augment noisily force // dryrun i.rel_start_all 

;
#delimit cr

********************************************************************************
* No observations
********************************************************************************

regress housework_focal4 housework_focal0 housework_focal1 housework_focal2 housework_focal3 housework_focal5 housework_focal6 housework_focal7 housework_focal8 housework_focal9 housework_focal10 housework_focal11 housework_focal12 housework_focal13 housework_focal14 housework_focal15 housework_focal16 weekly_hrs_t1_focal4 employed_focal4 earnings_t1_focal4 educ_focal4 children4 NUM_CHILDREN_4 AGE_YOUNG_CHILD_4 relationship_4 partnered4 TOTAL_INCOME_T1_FAMILY_4 race_fixed_focal 

regress housework_focal4 housework_focal2 housework_focal3 housework_focal5 housework_focal6 weekly_hrs_t1_focal4 employed_focal4 earnings_t1_focal4 educ_focal4 children4 NUM_CHILDREN_4 AGE_YOUNG_CHILD_4 relationship_4 partnered4 TOTAL_INCOME_T1_FAMILY_4 race_fixed_focal 

regress housework_focal9 housework_focal7 housework_focal8 housework_focal10 housework_focal11 weekly_hrs_t1_focal4 employed_focal4 earnings_t1_focal4 educ_focal4 children4 NUM_CHILDREN_4 AGE_YOUNG_CHILD_4 relationship_4 partnered4 TOTAL_INCOME_T1_FAMILY_4 race_fixed_focal 

********************************************************************************
* Not converging (not surprised...) and sometimes no observations?
********************************************************************************
// convergence issues - okay it says convergence issues, but when I run it here, it says no observations... is that bc this manual attempt isn't taking into account previously imputed data?
logit employed_focal4 i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal5 i.employed_focal6 /// 
                     i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 ///
                     i.employed_focal14 i.employed_focal15 i.employed_focal16 weekly_hrs_t_focal4 housework_focal4 earnings_t_focal4 i.FIRST_BIRTH_YR ///
                     i.birth_yr_all i.rel_start_all i.raceth_fixed_focal i.sample_type i.SEX 

logit employed_focal8 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal9 ///
                     i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all ///
                     i.raceth_fixed_focal i.sample_type i.SEX 

logit employed_focal8 i.employed_focal3 i.employed_focal4 i.employed_focal5 i.employed_focal6 i.employed_focal7 i.employed_focal9 ///
                     i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 i.FIRST_BIRTH_YR i.birth_yr_all i.rel_start_all ///
                     i.raceth_fixed_focal i.sample_type i.SEX  if nmis_employ<=10
					 
logit employed_focal4 i.employed_focal0 i.employed_focal1 i.employed_focal2 i.employed_focal3 i.employed_focal5 i.employed_focal6 /// 
                     i.employed_focal7 i.employed_focal8 i.employed_focal9 i.employed_focal10 i.employed_focal11 i.employed_focal12 i.employed_focal13 ///
                     i.employed_focal14 i.employed_focal15 i.employed_focal16 weekly_hrs_t_focal4 housework_focal4 earnings_t_focal4 birth_yr_all ///
                     i.raceth_fixed_focal i.sample_type i.SEX 
					 
ologit educ_focal4 i.educ_focal0 i.educ_focal1 i.educ_focal2 i.educ_focal3 i.educ_focal5 i.educ_focal6 i.educ_focal7 i.educ_focal8 i.educ_focal9 i.educ_focal10 i.educ_focal11 i.educ_focal12 i.educ_focal13 i.educ_focal14     weekly_hrs_t_focal4 i.employed_focal4 housework_focal4 earnings_t_focal4 num_children_imp4 AGE_YOUNG_CHILD_4 i.partnered_imp4 TOTAL_INCOME_T_FAMILY4

forvalues e=0/14{
	display "duration=`e'"
	tab educ_focal_imp`e', m
}

*/