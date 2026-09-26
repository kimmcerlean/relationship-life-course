
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

use "$created_data/ukhls_couples_wide_truncated.dta", clear

********************************************************************************
* Restrict to sample used in R
********************************************************************************
// minimum sequence length of 3
unique pidp eligible_partner if sequence_length>=3 // confirm matches: 4873
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

desctable i.division_of_labor_trunc1 i.division_of_labor_trunc2 i.division_of_labor_trunc3 i.division_of_labor_trunc4 i.division_of_labor_trunc5 i.division_of_labor_trunc6 i.division_of_labor_trunc7 i.division_of_labor_trunc8 i.division_of_labor_trunc9 i.division_of_labor_trunc10 i.egalitarian_trunc1 i.egalitarian_trunc2 i.egalitarian_trunc3 i.egalitarian_trunc4 i.egalitarian_trunc5 i.egalitarian_trunc6 i.egalitarian_trunc7 i.egalitarian_trunc8 i.egalitarian_trunc9 i.egalitarian_trunc10, filename("$results/ukhls_egal_x_duration") stats(mimean)

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

browse pidp eligible_partner sequence_length true_egal true_egal_percent modified_egal modified_egal_percent egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

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