********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: prep_imputation_for_individs.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file takes the long data from step 4 (that is mostly just data from
* when respondents are in sample, with some non-survey / dropout years
* and rectangularizes, so we can impute ALL durations
* In doing so, I create some final variables and attempt to fill in other
* missings with variables we have (e.g. relationship dates)
 
********************************************************************************
* Load data - also remove respondents NEVER in sample -- RIGHT?
********************************************************************************
use "$created_data/gsoep_couple_data_recoded.dta", clear

tab ever_int, m // these respondents will never have data upon which to base the imputation
unique pid, by(ever_int) // about 8.6%
browse pid eligible_partner syear sex_pl psample_pl born_in_germany weekly_work_hrs housework_weekdays edu4_fixed if ever_int==0 // so, we do get info from ppathl and other bio file (which I don't understand) as well as HH info, which makes sense
tab survey_status_pl if ever_int==0, m // most are 30s: Persons In Successfully Interviewed HH Without Individual Interview or 40: Person in non completed gross HH

**# Need to come back to this and decide. let's just keep recoding for now
// drop if ever_int==0

********************************************************************************
* Rectangularize
********************************************************************************
* want these uniques to match
unique pid // 28032 204584
unique pid eligible_partner // 29531 204854
unique eligible_couple_id // 15792. one problem with doing couple ID the way I did previously (so it matches regardless of who is pid and who is partner), is that this won't work for fill in, because I am imputing at individual level, so need rows for all pids - BUT because pids can contribute multiple relationships, needs to be by relationship. so create new couple id to use just for these purposes and will retain original as well

egen couple_id_unique = group(pid eligible_partner)
unique couple_id_unique

sort pid syear
browse pid syear eligible_partner eligible_couple_id couple_id_unique eligible_rel_start_year eligible_rel_end_year relative_duration

fillin couple_id_unique relative_duration

// quick checks
tab relative_duration // yes, now it perfectly aligns
unique couple_id_unique, by(relative_duration)
bysort couple_id_unique: egen rowcount = count(relative_duration)
tab rowcount, m // all should be 15

// pull through fixed variables // couple_id_unique
foreach var in pid cid eligible_couple_id eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no min_dur max_dur birthyr_pl firstyr_contact_pl firstyr_survey_pl first_couple_year lastyr_contact_pl lastyr_survey_pl last_couple_year edu4_fixed isced97_fixed yrs_educ_fixed born_in_germany where_born_ew where_born_state where_1989_ew country_born_pl global_region_born nationality_fixed nat_region_fixed any_births_bh sumkids first_birth_year last_birth_year psample_pl sex_pl ever_transition transition_year master_rel_type1 master_how_end1 master_start_yr1 master_end_yr1 master_rel_type2 master_how_end2 master_start_yr2 master_end_yr2 master_rel_type3 master_how_end3 master_start_yr3 master_end_yr3 master_rel_type4 master_how_end4 master_start_yr4 master_end_yr4 master_rel_type5 master_how_end5 master_start_yr5 master_end_yr5 master_rel_type6 master_how_end6 master_start_yr6 master_end_yr6 master_rel_type7 master_how_end7 master_start_yr7 master_end_yr7 master_rel_type8 master_how_end8 master_start_yr8 master_end_yr8 master_rel_type9 master_how_end9 master_start_yr9 master_end_yr9 master_rel_type10 master_how_end10 master_start_yr10 master_end_yr10 father_educ mother_educ who_lived_with yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other retirement_yr any_mpf num_religion edu4_change isced_change yrs_educ_change ever_int biovalid_bh{
	bysort couple_id_unique (`var'): replace `var'=`var'[1] if `var'==.
}

replace syear = eligible_rel_start_year + relative_duration if syear==.
replace age = syear - birthyr_pl if age==.

sort pid syear
browse pid eligible_partner syear age birthyr_pl partnered_total eligible_rel_start_year eligible_rel_end_year eligible_rel_no relative_duration eligible_couple_id _fillin

********************************************************************************
* Now create some variables now that rectangularized
********************************************************************************
* Current parent status
gen ever_parent = .
replace ever_parent = 0 if first_birth_year==9999
replace ever_parent = 1 if first_birth_year!=9999 & first_birth_year!=. // I think something went wrong with recent births (especially if don't actually have biobirth info)
replace ever_parent = 1 if ever_parent==. & sumkids !=0

tab biovalid_bh if ever_parent==1 & any_births_bh==0, m

tab sumkids ever_parent, m
tab sumkids any_births_bh, m
tab any_births_bh ever_parent, m

tab first_birth_year ever_parent, m
tab first_birth_year any_births_bh, m

sort pid couple_id_unique syear
browse pid syear first_birth_year ever_parent any_births_bh sumkids num_hh_0_1_cnef num_hh_2_4_cnef num_hh_5_7_cnef
browse pid syear relative_duration first_birth_year ever_parent any_births_bh sumkids num_hh_0_1_cnef num_hh_2_4_cnef num_hh_5_7_cnef if ever_parent==1 & any_births_bh==0 // I think these births are valid based on presence of young kids in HH following birth years, so I am going to consider parents
tab num_hh_0_1_cnef if ever_parent==1 & any_births_bh==0 & (syear==first_birth_year | syear==first_birth_year+1)
tab num_hh_0_1_cnef if ever_parent==1 & any_births_bh==0 & syear==first_birth_year+1

gen current_parent_status=.
replace current_parent_status = 0 if ever_parent==0
replace current_parent_status = 0 if ever_parent==1 & syear < first_birth_year & first_birth_year!=.
replace current_parent_status = 1 if ever_parent==1 & syear >= first_birth_year & first_birth_year!=.

// browse pid syear current_parent_status first_birth_year ever_parent any_births_bh sumkids

tab ever_parent current_parent_status,m 
tab first_birth_year current_parent_status, m

* Birth timing relative to relationship start
gen birth_timing_rel = eligible_rel_start_year - first_birth_year if first_birth_year!=9999 & first_birth_year!=.
replace birth_timing_rel = 9999 if first_birth_year==9999

tab birth_timing_rel ever_parent, m

// browse pid syear birth_timing_rel eligible_rel_start_year first_birth_year

* Partnership status for filled in years - based on relationship history
browse pid syear status_pl partnered_total partnered_pl marst_defacto eligible_rel_no num_rel eligible_rel_start_year eligible_rel_end_year master_start_yr1 master_end_yr1 master_rel_type1 master_start_yr2 master_end_yr2 master_rel_type2 master_start_yr3 master_end_yr3 master_rel_type3 master_start_yr4 master_end_yr4 master_rel_type4 master_start_yr5 master_end_yr5 master_rel_type5

// I feel like some of the marital statuses are not even accurate if in a relationship...Think some of this is because people are observed in relationships but I lack marital history for them?
// I clean this up post imputation. this is hard if in year of start / end, because it could be pre / post relationship in that year, so i think I need to rely first on provided info actually. also, i impute this info, but we don't even keep people outside of their relationship years (and I handle that data cleaning post imputation), so...I am not actually sure this variable does much (I just impute it because contains useful info for imputation).
gen in_rel_year = 0
replace in_rel_year = 1 if syear>=eligible_rel_start_year & syear<=eligible_rel_end_year

tab marst_defacto if in_rel_year==1, m
tab partnered_pl if in_rel_year==1, m

gen eligible_rel_type=.
forvalues r=1/10{
	replace eligible_rel_type = master_rel_type`r' if eligible_rel_start_year == master_start_yr`r' & eligible_rel_end_year == master_end_yr`r'
}
label values eligible_rel_type rel_type // this isn't working, especially because I change start / end dates for marriage / cohab (and I need to handle that transition using the transition year info I have). I do that post imputation anyway

gen marst_imp = marst_defacto 
capture label define marst_defacto 1 "Married" 2 "Partnered" 3 "Never Partnered"  4 "Divorced" 5 "Widowed"
label values marst_imp marst_defacto

gen partnered_imp = partnered_total // useful to create this because some people missing on relationship type I think? So this allows me to figure out who should have this info

// now fill in missing years based on relationship history
forvalues y=1/10{
	replace partnered_imp = 1 if partnered_imp==. & syear >= master_start_yr`y' & syear <= master_end_yr`y'
	replace marst_imp = 1 if marst_imp==. & syear >= master_start_yr`y' & syear <= master_end_yr`y' & master_rel_type`y'==2 // marriage
	replace marst_imp = 2 if marst_imp==. & syear >= master_start_yr`y' & syear <= master_end_yr`y' & master_rel_type`y'==1 // cohab
}

replace partnered_imp=0 if partnered_imp==. & syear < master_start_yr1 & master_start_yr1!=. // not partnered if prior to first rel date
replace partnered_imp=1 if partnered_imp==. & in_rel_year==1 // if no other info available, will assume partnered
replace marst_imp=3 if marst_imp==. & syear < master_start_yr1 & master_start_yr1!=. // & master_rel_type1==1 // this is never partnered so is fine to be either partner

// can I fill in POST rel info? well, if intact, that doesn't work if they dropped out (bc we don't know when actually ended)
egen last_rel_yr = rowmax(master_end_yr*)
browse pid last_rel_yr master_end_yr* master_how_end*

gen how_last_rel_end=.
forvalues y=1/10{
	replace how_last_rel_end = master_how_end`y' if last_rel_yr == master_end_yr`y' & master_end_yr`y'!=. // is there a risk of two rels ending in same year and creating a problem?
}
label values how_last_rel_end how_end
tab how_last_rel_end, m

replace marst_imp=4 if marst_imp==. & syear > last_rel_yr & inlist(how_last_rel_end,1,2) // breakup / divorce
replace marst_imp=5 if marst_imp==. & syear > last_rel_yr & how_last_rel_end==3 // widowhood
replace partnered_imp=0 if partnered_imp==. & syear > last_rel_yr & inrange(how_last_rel_end,1,3)

tab marst_imp partnered_imp, m // there are just a small amount (<1.5%) where partnered filled in but marst not, I think it's okay -- when I browse below, will see if can fill anything in. I actually think most of these are first year in rel
	tab relative_duration if partnered_imp==1 & marst_imp==. // okay yes, largely years 0/1
	tab in_rel_year if partnered_imp==1 & marst_imp==.
	sort eligible_couple_id pid syear
	browse pid syear relative_duration in_rel_year status_pl marst_imp partnered_imp
	replace marst_imp = marst_imp[_n+1] if marst_imp==. & partnered_imp==1 & in_rel_year==1 & in_rel_year[_n+1]==1 & pid==pid[_n+1] & eligible_partner==eligible_partner[_n+1]

tab marst_defacto marst_imp, m

browse pid syear status_pl marst_imp partnered_imp partnered_total partnered_pl marst_defacto in_rel_year eligible_rel_start_year eligible_rel_end_year master_start_yr1 master_end_yr1 master_rel_type1 master_start_yr2 master_end_yr2 master_rel_type2 master_start_yr3 master_end_yr3 master_rel_type3 master_start_yr4 master_end_yr4 master_rel_type4 master_start_yr5 master_end_yr5 master_rel_type5 last_rel_yr how_last_rel_end

* Current retirement status
tab retirement_yr ret_cpf, m
browse pid syear age status_pl retirement_yr ret_cpf

gen retired_yn = ret_cpf
replace retired_yn = 0 if retirement_yr!=. & syear<retirement_yr
replace retired_yn = 1 if retirement_yr!=. & syear>=retirement_yr
browse pid syear age status_pl retirement_yr retired_yn ret_cpf

* Fill in status_pl (either pre-interview, post-interview, or just non-sample) // feel like I have code for this somewhere...(was in step 1a)
browse pid syear status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl

tab status_pl if lastyr_contact_pl == syear, m

gen full_status_pl = status_pl
replace full_status_pl = 0 if syear > lastyr_contact_pl & full_status_pl==. & lastyr_contact_pl!=.
replace full_status_pl = 0 if syear > lastyr_survey_pl & full_status_pl==. & lastyr_survey_pl!=.
replace full_status_pl = -1 if syear < firstyr_contact_pl & full_status_pl==. & firstyr_contact_pl!=.
replace full_status_pl = 3 if full_status_pl==. & ever_int==0

label define full_status -1 "pre-survey" 0 "dropout" 1 "sample" 2 "youth" 3 "no int"
label values full_status_pl full_status
tab full_status_pl, m

browse pid eligible_partner hid cid syear full_status_pl status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl housework_weekdays weekly_work_hrs employment edu4

**# Save
save "$created_data/gsoep_couples_alldurs_long.dta", replace

********************************************************************************
* Let's drop unnecessary variables
********************************************************************************
use "$created_data/gsoep_couples_alldurs_long.dta", clear

global keep_fixed "sex_pl birthyr_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl psample_pl lastyr_survey_pl country_born_pl eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no ever_transition transition_year min_dur max_dur first_couple_year last_couple_year ever_int sumkids any_mpf born_in_germany where_born_ew where_born_state where_1989_ew global_region_born mother_educ father_educ yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other who_lived_with first_birth_year last_birth_year edu4_fixed isced97_fixed yrs_educ_fixed nat_region_fixed nationality_fixed ever_parent birth_timing_rel"

global keep_time "where_germany_pl survey_status_pl status_pl employment self_reported_health disability_yn disability_amount religious_affiliation errands_sundays housework_saturdays housework_sundays childcare_saturdays childcare_sundays repair_saturdays errands_weekdays housework_weekdays childcare_weekdays repair_weekdays errands_saturdays emplst_pg isced97_pg yrs_educ_pg nationality_pb survey_status_pb earnings_gross_t_cnef hh_gross_income_t_cnef hh_net_income_t_cnef live_fam_bp aid_in_hh_hl housing_status federal_state_hb kidsu18_hh num_65up_hh age_youngest_child region_type age edu4 nationality_region home_owner marst_defacto partnered_total religion_est employed_binary weekly_work_hrs gross_income_lm net_income_lm hh_income_net_monthly repair_sundays any_outside_help num_parent_in_hh any_parent_in_hh current_parent_status in_rel_year marst_imp partnered_imp retired_yn full_status_pl" // hh_gross_income_py_cnef hh_net_income_py_cnef earnings_gross_py_cnef 

global keep_id "pid eligible_couple_id eligible_partner relative_duration couple_id_unique"

keep $keep_fixed $keep_time $keep_id syear _fillin hid retirement_yr

********************************************************************************
* I think there is something to sort out with people with multiple partners
* Want to do this here, before I do the final reshape
********************************************************************************
quietly unique eligible_partner, by(pid) gen(num_elig_partners)
bysort pid (num_elig_partners): replace num_elig_partners=num_elig_partners[1]
tab num_elig_partners, m
unique pid, by(num_elig_partners)

sort pid eligible_partner syear
sort pid syear _fillin
browse pid eligible_partner syear _fillin full_status_pl status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl housework_weekdays weekly_work_hrs employment edu4 if pid==2802
browse pid eligible_partner syear relative_duration _fillin full_status_pl status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl housework_weekdays weekly_work_hrs employment edu4 if num_elig_partners > 1

// how can I first flag if they have two rows for same year, one via fillin, one not.
quietly unique _fillin, by(pid syear) gen(duplicate_record)
bysort pid syear (duplicate_record): replace duplicate_record=duplicate_record[1]
tab duplicate_record,m 
tab num_elig_partners duplicate_record, m

sort pid syear _fillin
browse pid eligible_partner syear duplicate_record relative_duration _fillin full_status_pl status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl housework_weekdays weekly_work_hrs employment edu4 edu4_fixed kidsu18_hh if num_elig_partners > 1

foreach var in $keep_fixed{
	inspect `var' if duplicate_record==2 & _fillin==0
	inspect `var' if duplicate_record==2 & _fillin==1 // so any that are missing are because they are missing generally, the fixed variables are fine here
}

foreach var in $keep_time{
	inspect `var' if duplicate_record==2 & _fillin==0
	inspect `var' if duplicate_record==2 & _fillin==1 // yes so basically all of these are missing unless newly created in this last file
}

// let's create two copies of variables to check that the below works as intended
gen hw_check = housework_weekdays
gen work_check = weekly_work_hrs

tab status_pl if duplicate_record==2 & _fillin==0, m
tab status_pl if duplicate_record==2 & _fillin==1, m

sort pid syear _fillin
foreach var in $keep_time{
	replace `var' = `var'[_n-1] if `var'==. & pid==pid[_n-1] & syear==syear[_n-1] & _fillin==1 & _fillin[_n-1]==0 & duplicate_record==2
	replace `var' = `var'[_n-1] if `var'==. & pid==pid[_n-1] & syear==syear[_n-1] & _fillin==1 & _fillin[_n-1]==1 & duplicate_record==2 // for people with more than 2 partners, some can be two fillins in a row GAH
}

browse pid eligible_partner syear num_elig_partners duplicate_record relative_duration _fillin full_status_pl status_pl housework_weekdays hw_check weekly_work_hrs work_check employment edu4 edu4_fixed kidsu18_hh if num_elig_partners > 1 & duplicate_record==2

// browse pid eligible_partner syear ever_int full_status_pl status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl housework_weekdays weekly_work_hrs employment edu4 if full_status_pl==.
// browse pid eligible_partner syear ever_int full_status_pl status_pl firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl housework_weekdays weekly_work_hrs employment edu4 edu4_fixed kidsu18_hh if inlist(pid, 17003, 101302, 136603)

replace full_status_pl = 3 if full_status_pl==. // unknown no sample status / no info

********************************************************************************
* Let's do the missing again now that rectangularized (and some new variables)
********************************************************************************
/*
preserve

local varlist "weekly_work_hrs housework_weekdays housework_saturdays housework_sundays repair_weekdays repair_saturdays repair_sundays errands_weekdays errands_saturdays errands_sundays childcare_weekdays childcare_saturdays childcare_sundays aid_in_hh_hl gross_income_lm net_income_lm earnings_gross_t_cnef employment emplst_pg employed_binary edu4 isced97_pg yrs_educ_pg edu4_fixed isced97_fixed yrs_educ_fixed ever_parent current_parent_status kidsu18_hh age_youngest_child partnered_total marst_defacto partnered_imp marst_imp hh_income_net_monthly hh_gross_income_t_cnef hh_net_income_t_cnef num_65up_hh num_parent_in_hh any_parent_in_hh born_in_germany where_born_ew where_born_state where_1989_ew country_born_pl global_region_born nationality_pb nationality_region nationality_fixed nat_region_fixed father_educ mother_educ who_lived_with yrs_bio_parent yrs_live_mom yrs_live_dad yrs_live_other where_germany_pl federal_state_hb region_type live_fam_bp housing_status home_owner religious_affiliation religion_est retired_yn retirement_yr disability_yn disability_amount self_reported_health birthyr_pl first_birth_year birth_timing_rel eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no psample_pl full_status_pl survey_status_pb sex_pl"

// can I follow this to export more easily? https://www.statalist.org/forums/forum/general-stata-discussion/general/1766742-export-results-from-misstable-sum-to-excel
frame create results str32 varname `c(obs_t)' (eq_dot gt_dot lt_dot ///
    n_distinct minimum maximum)
    
// ds, has(type numeric)
foreach var in `varlist'{
    misstable summ `var', all
    if !missing(r(N_eq_dot), r(N_gt_dot)) {
        frame post results ("`var'") (r(N_eq_dot)) (r(N_gt_dot)) (r(N_lt_dot)) ///
            (r(K_uniq)) (r(min)) (r(max))
    }
}

frame change results
label var varname "Variable"
label var eq_dot "Obs=."
label var gt_dot "Obs>."
label var lt_dot "Obs<."
label var n_distinct "Unique values"
label var minimum "Min"
label var maximum "Max"

export excel using "$root/imputation/gsoep_missingtable_post.xlsx", firstrow(varlabels) replace

restore
*/

********************************************************************************
**# * Now reshape wide and final prep for imputation
********************************************************************************
// explore shape of data before imputation
histogram weekly_work_hrs if weekly_work_hrs > 0 & weekly_work_hrs < 80, width(1)
histogram weekly_work_hrs if weekly_work_hrs >= 0 & weekly_work_hrs < 80, width(1)
histogram housework_weekdays, width(1)

// recode duration so none negative (-2 becomes 0)
gen duration = relative_duration+2
browse relative_duration duration
tab duration, m

// confirm AGAIN these are truly unique
unique pid eligible_partner $keep_fixed

// drop rogue variables I wanted above
drop syear hid retirement_yr relative_duration hw_check work_check _Unique
rename _fillin fillin

// DO THE RESHAPE
reshape wide $keep_time fillin duplicate_record, i(pid eligible_partner eligible_couple_id couple_id_unique) j(duration)

browse pid eligible_partner eligible_couple_id weekly_work_hrs* housework_weekdays* fillin*

unique pid eligible_partner
unique couple_id_unique
unique eligible_couple_id

save "$created_data/gsoep_couples_alldurs_wide.dta", replace

********************************************************************************
* Last variable updates
********************************************************************************
/* these codes are for PSID. these are also different because I mostly filled in off-years, not large gaps
// option 1: fill in based on right and left
forvalues y=1998(2)2022{
	local z=`y'+1
	local x=`y'-1
	capture gen religion_focal`y'=.
	replace religion_focal`y' = religion_focal`x' if religion_focal`x' == religion_focal`z'
	label values religion_focal`y' religion 
}

// option 2: fill in based on them never changing
egen min_educ = rowmin(educ_focal_imp*)
egen max_educ = rowmax(educ_focal_imp*)

// This is what Killewald 2016 does with PSID (i do this for PSID as well, but for reference in thinking about GSOEP):
If an individual's health status, religion, education, or student status, or the homeownership status or region of the couple, is missing in a
given year, but has consistent reports in the prior and subsequent survey waves, I impute the same value
to the intervening wave. Because health, religion, education, and region are assumed to change only
rarely, for these variables I impute valid reports in successive earlier and later survey waves, with a
preference for earlier reports in the case of ties...Finally, I multiply impute remaining
missing values.  (so does all of these BEFORE mi - which again, is my approach too)
*/

********
* Do I want to fill in religion for in-between years?
browse pid religious_affiliation* religion_est* // okay, I think religion, let's just do if the surrounding years are the same (like truly just 1 year pre / post) - this will not work for many, because in early waves, religion asked very infrequently, but there are some people with every other filled in. Do this for now, and can come back to this if I change my mind

forvalues y=1/13{
	local z=`y'+1
	local x=`y'-1
	replace religious_affiliation`y' = religious_affiliation`x' if religious_affiliation`y'==. & religious_affiliation`x' == religious_affiliation`z' // okay yes, this added very few
}

********
* Could actually also possibly do the same for retirement? (feel like if 0 pre non-response and 0 post, probably still 0? it's just post dropout that are difficult to fill in). also like - if first value is 0, then all prior are probably zero as well? Again, it's the post dropout that I probably shouldn't fill in
browse pid retired_yn*

egen retired_year1 = rowfirst(retired_yn*)

gen retired_first_obs=.
forvalues y=0/14{
	replace retired_first_obs = `y' if retired_first_obs==. & retired_yn`y'!=.
}

// so, first do the leading 0s
forvalues y=0/14{
	replace retired_yn`y' = 0 if retired_year1 == 0 & `y' < retired_first_obs & retired_first_obs!=.
}

browse pid retired_year1 retired_first_obs retired_yn*

// now non-response years (e.g. 13802 / 13803, dur11)
forvalues y=1/13{
	local z=`y'+1
	local x=`y'-1
	replace retired_yn`y' = retired_yn`x' if retired_yn`y'==. & retired_yn`x' == retired_yn`z'
}

********
* Disability / health? disability_yn disability_amount self_reported_health
browse pid disability_yn* disability_amount* // if I decide to ONLY do the yes / no here, but then want to impute amount, need to set amount to 0 if yn == 0. but, in other surveys, I only impute the yn

forvalues y=1/13{
	local z=`y'+1
	local x=`y'-1
	replace disability_yn`y' = disability_yn`x' if disability_yn`y'==. & disability_yn`x' == disability_yn`z'
	replace disability_amount`y' = disability_amount`x' if disability_amount`y'==. & disability_amount`x' == disability_amount`z'
}

browse pid self_reported_health* // I actually think this changes too much to do this, will just impute all

********
* Home ownership? housing_status, home_owner
tab housing_status3 home_owner3, m // home_owner is a recode of housing_status. I guess need to impute housing status as it is the base variable
browse pid housing_status* home_owner* 

forvalues y=1/13{
	local z=`y'+1
	local x=`y'-1
	replace housing_status`y' = housing_status`x' if housing_status`y'==. & housing_status`x' == housing_status`z'
	replace home_owner`y' = home_owner`x' if home_owner`y'==. & home_owner`x' == home_owner`z'
}

********
* Same with education (backup for fixed version?) - although both PSID AND UKHLS use education at time 0, which I already created. So, shoudld I just use that? Education, at least, can only change in one direction, unlike rest of these, so actually if min / max education same and / or surrounding years same, education has to be same
// but - I really need to fill in all the middle years, not just 1 gap (because it can't change in any direction)
browse pid edu4* // isced97_pg* yrs_educ_pg* // okay, no I am just leaving this as fixed.

********
* And nationality
browse nationality_pb* nationality_region*
// no I think these should be fixed

**# Final save pre-imputation
save "$created_data/gsoep_couples_alldurs_wide.dta", replace // same file name as above

********************************************************************************
* Final missing check
********************************************************************************
// check missings by duration, following: https://www.statalist.org/forums/forum/general-stata-discussion/general/1643775-export-mdesc-table-to-excel

program mmdesc, rclass byable(recall)
syntax [varlist] [if] [in]
tempvar touse
mark `touse' `if' `in'
local nvars : word count `varlist' 
tempname matrix 
matrix `matrix' = J(`nvars', 3, .) 
local i = 1 
quietly foreach var of local varlist {
    count if missing(`var') & `touse' 
    matrix `matrix'[`i', 1] = r(N) 
    count if `touse'
    matrix `matrix'[`i', 2] = r(N) 
    matrix `matrix'[`i', 3] = `matrix'[`i',1] / `matrix'[`i',2] 
    local ++i  
}
matrix rownames `matrix' = `varlist'                     
matrix colnames `matrix' = Missing Total Missing/Total 
matrix list `matrix', noheader 
return matrix table = `matrix' 
end

putexcel set "$root/imputation/gsoep_missingtable_bydur.xlsx", replace
mmdesc where_germany_pl0-retired_first_obs
putexcel A1 = matrix(r(table))