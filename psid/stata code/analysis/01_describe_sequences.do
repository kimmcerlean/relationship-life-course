
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

use "$created_data/psid_couples_wide_truncated.dta", clear

********************************************************************************
* Restrict to sample used in R
********************************************************************************
// minimum sequence length of 3
unique unique_id partner_unique_id if sequence_length>=3 // confirm matches: 5973
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

desctable i.division_of_labor_trunc1 i.division_of_labor_trunc2 i.division_of_labor_trunc3 i.division_of_labor_trunc4 i.division_of_labor_trunc5 i.division_of_labor_trunc6 i.division_of_labor_trunc7 i.division_of_labor_trunc8 i.division_of_labor_trunc9 i.division_of_labor_trunc10 i.egalitarian_trunc1 i.egalitarian_trunc2 i.egalitarian_trunc3 i.egalitarian_trunc4 i.egalitarian_trunc5 i.egalitarian_trunc6 i.egalitarian_trunc7 i.egalitarian_trunc8 i.egalitarian_trunc9 i.egalitarian_trunc10 i.dual_work_trunc1 i.dual_work_trunc2 i.dual_work_trunc3 i.dual_work_trunc4 i.dual_work_trunc5 i.dual_work_trunc6 i.dual_work_trunc7 i.dual_work_trunc8 i.dual_work_trunc9 i.dual_work_trunc10 i.dual_ft_trunc1 i.dual_ft_trunc2 i.dual_ft_trunc3 i.dual_ft_trunc4 i.dual_ft_trunc5 i.dual_ft_trunc6 i.dual_ft_trunc7 i.dual_ft_trunc8 i.dual_ft_trunc9 i.dual_ft_trunc10 i.hw_egal_trunc1 i.hw_egal_trunc2 i.hw_egal_trunc3 i.hw_egal_trunc4 i.hw_egal_trunc5 i.hw_egal_trunc6 i.hw_egal_trunc7 i.hw_egal_trunc8 i.hw_egal_trunc9 i.hw_egal_trunc10 i.hw_mod_egal_trunc1 i.hw_mod_egal_trunc2 i.hw_mod_egal_trunc3 i.hw_mod_egal_trunc4 i.hw_mod_egal_trunc5 i.hw_mod_egal_trunc6 i.hw_mod_egal_trunc7 i.hw_mod_egal_trunc8 i.hw_mod_egal_trunc9 i.hw_mod_egal_trunc10, filename("$results/psid_egal_x_duration") stats(mimean)

mi estimate: mean egalitarian_trunc1 
mi estimate: mean egalitarian_trunc5 
mi estimate: proportion egalitarian_trunc1 
mi estimate: proportion egalitarian_trunc5 
mi estimate: proportion division_of_labor_trunc1 
mi estimate: proportion division_of_labor_trunc5 
mi estimate: proportion egal_dol_yn_trunc1 
mi estimate: proportion egal_dol_yn_trunc5 
mi estimate: mean dual_work_trunc5
mi estimate: mean hw_mod_egal_trunc5

// Now, can I count the total # of states spent egal?
// browse unique_id partner_unique_id sequence_length egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

egen true_egal = rowtotal(egal_dol_yn_trunc1 egal_dol_yn_trunc2 egal_dol_yn_trunc3 egal_dol_yn_trunc4 egal_dol_yn_trunc5 egal_dol_yn_trunc6 egal_dol_yn_trunc7 egal_dol_yn_trunc8 egal_dol_yn_trunc9 egal_dol_yn_trunc10 ), missing
egen modified_egal = rowtotal(egalitarian_trunc1 egalitarian_trunc2 egalitarian_trunc3 egalitarian_trunc4 egalitarian_trunc5 egalitarian_trunc6 egalitarian_trunc7 egalitarian_trunc8 egalitarian_trunc9 egalitarian_trunc10), missing
egen dual_work = rowtotal(dual_work_trunc1 dual_work_trunc2 dual_work_trunc3 dual_work_trunc4 dual_work_trunc5 dual_work_trunc6 dual_work_trunc7 dual_work_trunc8 dual_work_trunc9 dual_work_trunc10), missing
egen dual_ft = rowtotal(dual_ft_trunc1 dual_ft_trunc2 dual_ft_trunc3 dual_ft_trunc4 dual_ft_trunc5 dual_ft_trunc6 dual_ft_trunc7 dual_ft_trunc8 dual_ft_trunc9 dual_ft_trunc10), missing
egen hw_mod_egal = rowtotal(hw_mod_egal_trunc1 hw_mod_egal_trunc2 hw_mod_egal_trunc3 hw_mod_egal_trunc4 hw_mod_egal_trunc5 hw_mod_egal_trunc6 hw_mod_egal_trunc7 hw_mod_egal_trunc8 hw_mod_egal_trunc9 hw_mod_egal_trunc10), missing
egen hw_egal = rowtotal(hw_egal_trunc1 hw_egal_trunc2 hw_egal_trunc3 hw_egal_trunc4 hw_egal_trunc5 hw_egal_trunc6 hw_egal_trunc7 hw_egal_trunc8 hw_egal_trunc9 hw_egal_trunc10), missing

tab true_egal
tab modified_egal
tab dual_work
tab dual_ft
tab hw_mod_egal
tab hw_egal

// then turn to percentages
gen true_egal_percent = true_egal / sequence_length
gen modified_egal_percent = modified_egal / sequence_length
gen dual_work_percent = dual_work / sequence_length
gen dual_ft_percent = dual_ft / sequence_length
gen hw_mod_egal_percent = hw_mod_egal / sequence_length
gen hw_egal_percent = hw_egal / sequence_length

browse unique_id partner_unique_id sequence_length true_egal true_egal_percent modified_egal modified_egal_percent egalitarian_trunc* egal_dol_yn_trunc* division_of_labor_trunc* if _mi_m!=0

tabstat true_egal_percent modified_egal_percent dual_work_percent dual_ft_percent hw_mod_egal_percent hw_egal_percent, stats(mean median)

// Then flag if all states are in "egal" state
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

foreach var in dual_work dual_ft hw_mod_egal hw_egal{
	gen `var'_flag = .
	replace `var'_flag = 0 if `var'!=sequence_length & `var'!=.
	replace `var'_flag = 1 if `var'==sequence_length & `var'!=.
}

tabstat dual_work_flag dual_ft_flag hw_mod_egal_flag hw_egal_flag

// STATS TO USE (just pasting in above excel, which probably isn't great....)
tab sequence_length true_egal
tab sequence_length true_egal, row nofreq
tab sequence_length modified_egal
tab sequence_length modified_egal, row nofreq

tabstat true_egal true_egal_flag true_egal_percent modified_egal modified_egal_flag modified_egal_percent, by(sequence_length)
tabstat true_egal_percent modified_egal_percent

// exploring putexcel
putexcel set "$results/psid_stata_egal_lifecourse.xlsx", replace

putexcel A1 = "Egalitarian Category"
putexcel B1 = "Sequence Length"
putexcel C1 = "Average # of Durations in Egal State"
putexcel D1 = "% with all observed durations in egal"
putexcel E1 = "Average % of durations in egal"
putexcel A2 = "True Egalitarian (Equal both)"
putexcel A11 = "Modified Egalitarian (Dual Full-time, equal or he does more HW)"
putexcel A20 = "Dual FT or Dual PT"
putexcel A29 = "Just Dual FT"
putexcel A38 = "Egalitarian OR He Does More Housework"
putexcel A47 = "Just Egalitarian Housework"

// putexcel B2 = ("3")  B3 = ("4")
putexcel B10 = ("Total") B19 = ("Total")  B28 = ("Total") B37 = ("Total") B46 = ("Total") B55 = ("Total")

* This is probably NOT the most efficient, but...it works..
// True Egalitarian
forvalues l=3/10{
	local row = `l' - 1
	putexcel B`row' = (`l')
	mean true_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean true_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean true_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean true_egal
matrix m`l'= e(b)
putexcel C10 = matrix(m`l'), nformat(#.#)

mean true_egal_flag
matrix m`l'= e(b)
putexcel D10 = matrix(m`l'), nformat(#.#%)

mean true_egal_percent
matrix m`l'= e(b)
putexcel E10 = matrix(m`l'), nformat(#.#%)
	
// Modified Egalitarian
forvalues l=3/10{
	local row = `l' + 8
	putexcel B`row' = (`l')
	mean modified_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean modified_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean modified_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean modified_egal
matrix m`l'= e(b)
putexcel C19 = matrix(m`l'), nformat(#.#)

mean modified_egal_flag
matrix m`l'= e(b)
putexcel D19 = matrix(m`l'), nformat(#.#%)

mean modified_egal_percent
matrix m`l'= e(b)
putexcel E19 = matrix(m`l'), nformat(#.#%)

// Dual FT + PT
forvalues l=3/10{
	local row = `l' + 17
	putexcel B`row' = (`l')
	mean dual_work if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean dual_work_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean dual_work_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean dual_work
matrix m`l'= e(b)
putexcel C28 = matrix(m`l'), nformat(#.#)

mean dual_work_flag
matrix m`l'= e(b)
putexcel D28 = matrix(m`l'), nformat(#.#%)

mean dual_work_percent
matrix m`l'= e(b)
putexcel E28 = matrix(m`l'), nformat(#.#%)

// Dual FT Only
forvalues l=3/10{
	local row = `l' + 26
	putexcel B`row' = (`l')
	mean dual_ft if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean dual_ft_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean dual_ft_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean dual_ft
matrix m`l'= e(b)
putexcel C37 = matrix(m`l'), nformat(#.#)

mean dual_ft_flag
matrix m`l'= e(b)
putexcel D37 = matrix(m`l'), nformat(#.#%)

mean dual_ft_percent
matrix m`l'= e(b)
putexcel E37 = matrix(m`l'), nformat(#.#%)

// Egal or He Does More Housework
forvalues l=3/10{
	local row = `l' + 35
	putexcel B`row' = (`l')
	mean hw_mod_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean hw_mod_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean hw_mod_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean hw_mod_egal
matrix m`l'= e(b)
putexcel C46 = matrix(m`l'), nformat(#.#)

mean hw_mod_egal_flag
matrix m`l'= e(b)
putexcel D46 = matrix(m`l'), nformat(#.#%)

mean hw_mod_egal_percent
matrix m`l'= e(b)
putexcel E46 = matrix(m`l'), nformat(#.#%)

// Egal HW Only
forvalues l=3/10{
	local row = `l' + 44
	putexcel B`row' = (`l')
	mean hw_egal if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel C`row' = matrix(m`l'), nformat(#.#)
	mean hw_egal_flag if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel D`row' = matrix(m`l'), nformat(#.#%)
	mean hw_egal_percent if sequence_length == `l'
	matrix m`l'= e(b)
	putexcel E`row' = matrix(m`l'), nformat(#.#%)
}

mean hw_egal
matrix m`l'= e(b)
putexcel C55 = matrix(m`l'), nformat(#.#)

mean hw_egal_flag
matrix m`l'= e(b)
putexcel D55 = matrix(m`l'), nformat(#.#%)

mean hw_egal_percent
matrix m`l'= e(b)
putexcel E55 = matrix(m`l'), nformat(#.#%)

tabstat true_egal modified_egal dual_work dual_ft hw_mod_egal hw_egal
tabstat true_egal_percent modified_egal_percent dual_work_percent dual_ft_percent hw_mod_egal_percent hw_egal_percent
tabstat true_egal_flag modified_egal_flag dual_work_flag dual_ft_flag hw_mod_egal_flag hw_egal_flag

********************************************************************************
**# Ideal is actually do R steps FIRST - get this output, then use for above and do all in one go
********************************************************************************
* One maybe challenge because R doesn't use MI framework, but Stata does. so we remove MI = 0 from R.
* Whereas in Stata, I retain. should I .... handle this differently?
* FOr these purposes, should I remove MI = 0? Right now it's JUST 1-5. if I ever try to do anything with MI, i think it will be unhappy. let's leave as separate for NOW, though this isn't a great model....

use "$created_data/psid_wide_truncated_Rdurs.dta", clear

tab _mi_m

egen true_egal = rowtotal(egal_dol_yn_trunc1 egal_dol_yn_trunc2 egal_dol_yn_trunc3 egal_dol_yn_trunc4 egal_dol_yn_trunc5 egal_dol_yn_trunc6 egal_dol_yn_trunc7 egal_dol_yn_trunc8 egal_dol_yn_trunc9 egal_dol_yn_trunc10 ), missing
egen modified_egal = rowtotal(egalitarian_trunc1 egalitarian_trunc2 egalitarian_trunc3 egalitarian_trunc4 egalitarian_trunc5 egalitarian_trunc6 egalitarian_trunc7 egalitarian_trunc8 egalitarian_trunc9 egalitarian_trunc10), missing
egen dual_work = rowtotal(dual_work_trunc1 dual_work_trunc2 dual_work_trunc3 dual_work_trunc4 dual_work_trunc5 dual_work_trunc6 dual_work_trunc7 dual_work_trunc8 dual_work_trunc9 dual_work_trunc10), missing
egen dual_ft = rowtotal(dual_ft_trunc1 dual_ft_trunc2 dual_ft_trunc3 dual_ft_trunc4 dual_ft_trunc5 dual_ft_trunc6 dual_ft_trunc7 dual_ft_trunc8 dual_ft_trunc9 dual_ft_trunc10), missing
egen hw_mod_egal = rowtotal(hw_mod_egal_trunc1 hw_mod_egal_trunc2 hw_mod_egal_trunc3 hw_mod_egal_trunc4 hw_mod_egal_trunc5 hw_mod_egal_trunc6 hw_mod_egal_trunc7 hw_mod_egal_trunc8 hw_mod_egal_trunc9 hw_mod_egal_trunc10), missing
egen hw_egal = rowtotal(hw_egal_trunc1 hw_egal_trunc2 hw_egal_trunc3 hw_egal_trunc4 hw_egal_trunc5 hw_egal_trunc6 hw_egal_trunc7 hw_egal_trunc8 hw_egal_trunc9 hw_egal_trunc10), missing

browse max_dur_mod_egal modified_egal egalitarian_trunc* mod_egal_spell_*
browse max_dur_dol_egal true_egal division_of_labor_trunc* dol_egal_spell_*
browse max_dur_dual_work dual_work dual_work_trunc* dual_work_spell_*

tab max_dur_mod_egal // this is MAX CONSECUTIVE
tab modified_egal // this is TOTAL, not consecutive - BUT 0s should match
tab sequence_length max_dur_mod_egal // good sense check

tab max_dur_dol_egal // this is MAX CONSECUTIVE
tab true_egal  // this is TOTAL, not consecutive - BUT 0s should match. so makes sense THIS is higher
tab sequence_length max_dur_dol_egal

tab max_dur_dual_work
tab dual_work
tab sequence_length max_dur_dual_work

tab max_dur_dual_ft
tab dual_ft

tab max_dur_hw_mod_egal
tab hw_mod_egal

tab max_dur_hw_egal
tab hw_egal

tabstat max_dur_mod_egal max_dur_dol_egal modified_egal true_egal
tabstat max_dur_mod_egal max_dur_dol_egal, by(sequence_length) // this is ALL
tabstat max_dur_mod_egal max_dur_dol_egal max_dur_dual_work max_dur_dual_ft max_dur_hw_mod_egal max_dur_hw_egal, by(sequence_length) // All

tabstat max_dur_mod_egal if max_dur_mod_egal!=0, by(sequence_length) // if at least ONE SPELL egal
tabstat max_dur_dol_egal if max_dur_dol_egal!=0, by(sequence_length) 
tabstat max_dur_dual_work if max_dur_dual_work!=0, by(sequence_length) 
tabstat max_dur_dual_ft if max_dur_dual_ft!=0, by(sequence_length) 
tabstat max_dur_hw_mod_egal if max_dur_hw_mod_egal!=0, by(sequence_length) 
tabstat max_dur_hw_egal if max_dur_hw_egal!=0, by(sequence_length) 

// also want to figure out how to do this JUST for people who experienced state. think that is where some of above helpful because can use code that exists (like at least one in the rowtotal I do). actually, I can just do if MAX DUR > 0? but then again will be helpful to compare that that matches what I did above...