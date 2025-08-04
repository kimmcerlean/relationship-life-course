
set maxvar 10000

cd "/home/kmcerlea/stage/Life Course"

use "created data/stata/gsoep_individs_imputed_wide_bysex.dta", clear

global keep_time "where_germany_pl survey_status_pl status_pl employment self_reported_health disability_yn disability_amount religious_affiliation errands_sundays housework_saturdays housework_sundays childcare_saturdays childcare_sundays repair_saturdays errands_weekdays housework_weekdays childcare_weekdays repair_weekdays errands_saturdays emplst_pg isced97_pg yrs_educ_pg nationality_pb survey_status_pb earnings_gross_t_cnef hh_gross_income_t_cnef hh_net_income_t_cnef live_fam_bp aid_in_hh_hl housing_status federal_state kidsu18_hh num_65up_hh age_youngest_child region_type age edu4 nationality_region home_owner marst_defacto partnered_total religion_est employed_binary weekly_work_hrs gross_income_lm net_income_lm hh_income_net_monthly repair_sundays any_outside_help num_parent_in_hh any_parent_in_hh current_parent_status in_rel_year marst_imp partnered_imp retired_yn full_status_pl" // hh_gross_income_py_cnef hh_net_income_py_cnef earnings_gross_py_cnef 

mi reshape long $keep_time fillin duplicate_record urban_region, i(pid eligible_partner eligible_couple_id couple_id_unique) j(duration)

mi convert flong

// browse couple_id pid eligible_partner full_status_pl weekly_work_hrs housework_weekdays housework_saturdays _mi_miss _mi_m _mi_id
gen imputed=0
replace imputed=1 if inrange(_mi_m,1,10)

inspect weekly_work_hrs if imputed==0
inspect weekly_work_hrs if imputed==1

inspect housework_weekdays housework_saturdays if imputed==0
inspect housework_weekdays housework_saturdays if imputed==1

mi update

// mi register regular n

save "created data/stata/gsoep_individs_imputed_long_bysex.dta", replace

********************************************************************************
**# Some comparisons
********************************************************************************

tabstat weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays, by(imputed) stats(mean sd p50)
tabstat weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays if sex_pl==1, by(imputed) stats(mean sd p50)
tabstat weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays if sex_pl==2, by(imputed) stats(mean sd p50)

tabstat weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays repair_saturdays repair_sundays errands_weekdays errands_saturdays errands_sundays aid_in_hh_hl employment gross_income_lm earnings_gross_t_cnef kidsu18_hh age_youngest_child marst_imp num_65up_hh any_parent_in_hh hh_gross_income_t_cnef federal_state urban_region housing_status religious_affiliation disability_yn self_reported_health retired_yn where_born_state father_educ mother_educ yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other, by(imputed) stats(mean sd p50) columns(statistics)


********************************************************************************
**# Going to make a table so I can compare the categorical variables
********************************************************************************

gen relative_duration = duration - 2
keep if relative_duration >=0 & relative_duration <=10

tab employment, gen(emp) // 13
tab marst_imp, gen(marst) // 5
tab federal_state, gen(state_res) // 17
tab housing_status, gen(house) // 4
tab religious_affiliation, gen(religion) // 11
tab self_reported_health, gen(health) // 5 
tab where_born_state, gen(state_birth) // 17
tab father_educ, gen(dad_educ) // 3
tab mother_educ, gen(mom_educ) // 3

putexcel set "results/GSOEP/GSOEP_imputation_descriptives.xlsx", replace
putexcel B1:C1 = "Duration: All", merge
putexcel D1:E1 = "Duration: 0", merge
putexcel F1:G1 = "Duration: 5", merge
putexcel H1:I1 = "Duration: 10", merge
putexcel A2 = "Variable"
putexcel B2 = ("Not Imputed") D2 = ("Not Imputed") F2 = ("Not Imputed") H2 = ("Not Imputed") 
putexcel C2 = ("Imputed") E2 = ("Imputed") G2 = ("Imputed") I2 = ("Imputed")

// Means
putexcel A3 = "Paid Work Hours (Weekly)"
putexcel A4 = "Housework (Weekdays)"
putexcel A5 = "Housework (Saturdays)"
putexcel A6 = "Housework (Sundays)"
putexcel A7 = "Repair Work (Weekdays)"
putexcel A8 = "Repair Work (Saturdays)"
putexcel A9 = "Repair Work (Sundays)"
putexcel A10 = "Errands (Weekdays)"
putexcel A11 = "Errands (Saturdays)"
putexcel A12 = "Errands (Sundays)"
putexcel A13 = "Someone in HH requires care"
putexcel A14 = "1 FT"
putexcel A15 = "2 PT"
putexcel A16 = "3 Training"
putexcel A17 = "4 Marginally employed"
putexcel A18 = "5 PT Retired"
putexcel A19 = "6 Military (vol)"
putexcel A20 = "7 Volunteering"
putexcel A21 = "8 Disabled"
putexcel A22 = "9 Not employed"
putexcel A23 = "10 Internship / ST work"
putexcel A24 = "11 Military (comp)"
putexcel A25 = "12 Short-time work"
putexcel A26 = "13 Retired"
putexcel A27 = "Earnings (Monthly)"
putexcel A28 = "Earnings (Annual)"
putexcel A29 = "Number of children"
putexcel A30 = "Age of youngest child"
putexcel A31 = "1 Married"
putexcel A32 = "2 Partnered"
putexcel A33 = "3 Never Married"
putexcel A34 = "4 Divorced"
putexcel A35 = "5 Widowed"
putexcel A36 = "Number of people aged 65+ in HH"
putexcel A37 = "Coresidence with parents (YN)"
putexcel A38 = "Family income"
putexcel A39 = "Live: 1 Schleswig-Holstein"
putexcel A40 = "Live: 2 Hamburg"
putexcel A41 = "Live: 3 Niedersachsen"
putexcel A42 = "Live: 4 Bremen"
putexcel A43 = "Live: 5 Nordrhein-Westfalen"
putexcel A44 = "Live: 6 Hessen"
putexcel A45 = "Live: 7 Rheinland-Pfalz,Saarland"
putexcel A46 = "Live: 8 Baden-Wuerttemberg"
putexcel A47 = "Live: 9 Bayern"
putexcel A48 = "Live: 10 Saarland"
putexcel A49 = "Live: 11 West Berlin"
putexcel A50 = "Live: 12 East Berlin"
putexcel A51 = "Live: 13 Brandenburg"
putexcel A52 = "Live: 14 Mecklenburg-Vorpommern"
putexcel A53 = "Live: 15 Sachsen"
putexcel A54 = "Live: 16 Sachsen-Anhalt"
putexcel A55 = "Live: 17 Thueringen"
putexcel A56 = "Lives in urban area"
putexcel A57 = "1 Shared Housing"
putexcel A58 = "2 Sub-tenant"
putexcel A59 = "3 Main-tenant"
putexcel A60 = "4 Owner"
putexcel A61 = "1 Catholic"
putexcel A62 = "2 Protestant"
putexcel A63 = "3 Other Christian"
putexcel A64 = "4 Islam"
putexcel A65 = "5 Other"
putexcel A66 = "6 Non-denominational"
putexcel A67 = "7 Orthodox Christian"
putexcel A68 = "8 Islam"
putexcel A69 = "9 Shiite"
putexcel A70 = "10 Alevi"
putexcel A71 = "11 Multiple"
putexcel A72 = "Disability status (Y/N)"
putexcel A73 = "Retired (Y/N)"
putexcel A74 = "1 Very Good"
putexcel A75 = "2 Good"
putexcel A76 = "3 Satisfactory"
putexcel A77 = "4 Poor"
putexcel A78 = "5 Bad"
putexcel A79 = "Born: 0 Abroad"
putexcel A80 = "Born: 1 Schleswig-Holstein"
putexcel A81 = "Born: 2 Hamburg"
putexcel A82 = "Born: 3 Niedersachsen"
putexcel A83 = "Born: 4 Bremen"
putexcel A84 = "Born: 5 Nordrhein-Westfalen"
putexcel A85 = "Born: 6 Hessen"
putexcel A86 = "Born: 7 Rheinland-Pfalz"
putexcel A87 = "Born: 8 Baden-Wuerttemberg"
putexcel A88 = "Born: 9 Bayern"
putexcel A89 = "Born: 10 Saarland"
putexcel A90 = "Born: 11 Berlin"
putexcel A91 = "Born: 12 Brandenburg"
putexcel A92 = "Born: 13 Mecklenburg-Vorpommern"
putexcel A93 = "Born: 14 Sachsen"
putexcel A94 = "Born: 15 Sachsen-Anhalt"
putexcel A95 = "Born: 16 Thueringen"
putexcel A96 = "Father's educ: 1 [0-2] Low"
putexcel A97 = "Father's educ: 2 [3-4] Medium"
putexcel A98 = "Father's educ: 3 [5-6] High"
putexcel A99 = "Mother's educ: 1 [0-2] Low"
putexcel A100 = "Mother's educ: 2 [3-4] Medium"
putexcel A101 = "Mother's educ: 2 3 [5-6] High"
putexcel A102 = "Residence up to age 15: Yrs both bio parents"
putexcel A103 = "Residence up to age 15: Yrs just bio mom"
putexcel A104 = "Residence up to age 15: Yrs just bio dad"
putexcel A105 = "Residence up to age 15: Yrs neither bio parent"

local desc_vars "weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays repair_saturdays repair_sundays errands_weekdays errands_saturdays errands_sundays aid_in_hh_hl emp1 emp2 emp3 emp4 emp5 emp6 emp7 emp8 emp9 emp10 emp11 emp12 emp13 gross_income_lm earnings_gross_t_cnef kidsu18_hh age_youngest_child marst1 marst2 marst3 marst4 marst5 num_65up_hh any_parent_in_hh hh_gross_income_t_cnef state_res1 state_res2 state_res3 state_res4 state_res5 state_res6 state_res7 state_res8 state_res9 state_res10 state_res11 state_res12 state_res13 state_res14 state_res15 state_res16 state_res17 urban_region house1 house2 house3 house4 religion1 religion2 religion3 religion4 religion5 religion6 religion7 religion8 religion9 religion10 religion11 disability_yn retired_yn health1 health2 health3 health4 health5 state_birth1 state_birth2 state_birth3 state_birth4 state_birth5 state_birth6 state_birth7 state_birth8 state_birth9 state_birth10 state_birth11 state_birth12 state_birth13 state_birth14 state_birth15 state_birth16 state_birth17 dad_educ1 dad_educ2 dad_educ3 mom_educ1 mom_educ2 mom_educ3 yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other" // 103

** All durations
// Not imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}

** Duration 0
// Not imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0 & relative_duration==0
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1  & relative_duration==0
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

** Duration 5
// Not imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0 & relative_duration==5
	matrix t`var'= e(b)
	putexcel F`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1  & relative_duration==5
	matrix t`var'= e(b)
	putexcel G`row' = matrix(t`var'), nformat(#.#%)
}

** Duration 10
// Not imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==0 & relative_duration==10
	matrix t`var'= e(b)
	putexcel H`row' = matrix(t`var'), nformat(#.#%)
}

// Imputed
forvalues w=1/103{
	local row=`w'+2
	local var: word `w' of `desc_vars'
	mean `var' if imputed==1  & relative_duration==10
	matrix t`var'= e(b)
	putexcel I`row' = matrix(t`var'), nformat(#.#%)
}
