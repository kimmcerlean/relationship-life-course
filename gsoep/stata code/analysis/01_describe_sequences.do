
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: describe_sequences
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file pulls some basic descriptive information about couples' sequences,
* namely to explore egalitarianism across the life course

use "$created_data/gsoep_couples_wide_truncated.dta", clear

********************************************************************************
* Restrict to sample used in R
********************************************************************************
// minimum sequence length of 3
unique pid eligible_partner if sequence_length>=3 // confirm matches: 5425
keep if sequence_length>=3

// Only using first 5 imputations
mi set m -= (6,7,8,9,10)

********************************************************************************
* Pull descriptives
********************************************************************************
// explore descriptives to characterize egalitarianism at each duration
tab division_of_labor_trunc1 egalitarian_trunc1

forvalues d=1/10{
	tab egalitarian_trunc`d'
	tab division_of_labor_trunc`d'
}

desctable i.division_of_labor_trunc1 i.division_of_labor_trunc2 i.division_of_labor_trunc3 i.division_of_labor_trunc4 i.division_of_labor_trunc5 i.division_of_labor_trunc6 i.division_of_labor_trunc7 i.division_of_labor_trunc8 i.division_of_labor_trunc9 i.division_of_labor_trunc10 i.egalitarian_trunc1 i.egalitarian_trunc2 i.egalitarian_trunc3 i.egalitarian_trunc4 i.egalitarian_trunc5 i.egalitarian_trunc6 i.egalitarian_trunc7 i.egalitarian_trunc8 i.egalitarian_trunc9 i.egalitarian_trunc10, filename("$results/gsoep_egal_x_duration") stats(mimean)


mi estimate: mean egalitarian_trunc1 
mi estimate: mean egalitarian_trunc5 
mi estimate: proportion egalitarian_trunc1 
mi estimate: proportion egalitarian_trunc5 
mi estimate: proportion division_of_labor_trunc1 
mi estimate: proportion division_of_labor_trunc5 
mi estimate: proportion egal_dol_yn_trunc1 
mi estimate: proportion egal_dol_yn_trunc5 

// Now, can I count the total # of states spent egal?
// browse unique_id partner_unique_id sequence_length egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

egen true_egal = rowtotal(egal_dol_yn_trunc1 egal_dol_yn_trunc2 egal_dol_yn_trunc3 egal_dol_yn_trunc4 egal_dol_yn_trunc5 egal_dol_yn_trunc6 egal_dol_yn_trunc7 egal_dol_yn_trunc8 egal_dol_yn_trunc9 egal_dol_yn_trunc10 ), missing
egen modified_egal = rowtotal(egalitarian_trunc1 egalitarian_trunc2 egalitarian_trunc3 egalitarian_trunc4 egalitarian_trunc5 egalitarian_trunc6 egalitarian_trunc7 egalitarian_trunc8 egalitarian_trunc9 egalitarian_trunc10), missing

tab true_egal
tab modified_egal

gen true_egal_percent = true_egal / sequence_length
gen modified_egal_percent = modified_egal / sequence_length

browse  pid eligible_partner sequence_length true_egal true_egal_percent modified_egal modified_egal_percent egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

tabstat true_egal_percent modified_egal_percent, stats(mean median)

gen true_egal_flag = .
replace true_egal_flag = 0 if true_egal!=sequence_length & true_egal!=.
replace true_egal_flag = 1 if true_egal==sequence_length & true_egal!=.

tab true_egal_flag
tab true_egal_percent // 1s above match 100% here

gen modified_egal_flag = .
replace modified_egal_flag = 0 if modified_egal!=sequence_length & modified_egal!=.
replace modified_egal_flag = 1 if modified_egal==sequence_length & modified_egal!=.

tab modified_egal_flag
tab modified_egal_percent

// STATS TO USE (just pasting in above excel, which probably isn't great....)
tab sequence_length true_egal
tab sequence_length true_egal, row nofreq
tab sequence_length modified_egal
tab sequence_length modified_egal, row nofreq

tabstat true_egal true_egal_flag true_egal_percent modified_egal modified_egal_flag modified_egal_percent, by(sequence_length)
tabstat true_egal_percent modified_egal_percent

********************************************************************************
**# Ideal is actually do R steps FIRST - get this output, then use for above and do all in one go
********************************************************************************
* One maybe challenge because R doesn't use MI framework, but Stata does. so we remove MI = 0 from R.
* Whereas in Stata, I retain. should I .... handle this differently?
* FOr these purposes, should I remove MI = 0? Right now it's JUST 1-5. if I ever try to do anything with MI, i think it will be unhappy. let's leave as separate for NOW, though this isn't a great model....

use "$created_data/gsoep_wide_truncated_Rdurs.dta", clear

tab _mi_m

egen true_egal = rowtotal(egal_dol_yn_trunc1 egal_dol_yn_trunc2 egal_dol_yn_trunc3 egal_dol_yn_trunc4 egal_dol_yn_trunc5 egal_dol_yn_trunc6 egal_dol_yn_trunc7 egal_dol_yn_trunc8 egal_dol_yn_trunc9 egal_dol_yn_trunc10 ), missing
egen modified_egal = rowtotal(egalitarian_trunc1 egalitarian_trunc2 egalitarian_trunc3 egalitarian_trunc4 egalitarian_trunc5 egalitarian_trunc6 egalitarian_trunc7 egalitarian_trunc8 egalitarian_trunc9 egalitarian_trunc10), missing

browse max_dur_mod_egal modified_egal egalitarian_trunc* mod_egal_spell_*
browse max_dur_dol_egal true_egal division_of_labor_trunc* dol_egal_spell_*

tab max_dur_mod_egal // this is MAX CONSECUTIVE
tab modified_egal // this is TOTAL, not consecutive - BUT 0s should match
tab sequence_length max_dur_mod_egal // good sense check

tab max_dur_dol_egal // this is MAX CONSECUTIVE
tab true_egal  // this is TOTAL, not consecutive - BUT 0s should match. so makes sense THIS is higher
tab sequence_length max_dur_dol_egal

tabstat max_dur_mod_egal max_dur_dol_egal modified_egal true_egal
tabstat max_dur_mod_egal max_dur_dol_egal, by(sequence_length) // this is ALL
tabstat max_dur_mod_egal if max_dur_mod_egal!=0, by(sequence_length) // if at least ONE SPELL egal
tabstat max_dur_dol_egal if max_dur_dol_egal!=0, by(sequence_length) 

// also want to figure out how to do this JUST for people who experienced state. think that is where some of above helpful because can use code that exists (like at least one in the rowtotal I do). actually, I can just do if MAX DUR > 0? but then again will be helpful to compare that that matches what I did above... okay, so did all of this...