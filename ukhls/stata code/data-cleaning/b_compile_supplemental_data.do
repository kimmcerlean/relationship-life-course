********************************************************************************
********************************************************************************
* Project: Relationship Life Course Analysis
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: b_individual_recodes.do
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file recodes key variables to use in imputation / subsequence analysis


********************************************************************************
********************************************************************************
* Data prep steps
********************************************************************************
********************************************************************************

********************************************************************************
* Going to try to first update spouse id for BHPS so it's pidp NOT pid
********************************************************************************
use "$UKHLS/xwaveid_bh.dta", clear

keep pidp pid
rename pid sppid_bh
rename pidp partner_pidp_bh

save "$temp/spid_lookup.dta", replace

********************************************************************************
* Prep partner history file for later
********************************************************************************
use "$UKHLS_mh/phistory_wide.dta", clear

foreach var in status* partner* starty* startm* endy* endm* divorcey* divorcem* mrgend* cohend* ongoing* ttl_spells ttl_married ttl_civil_partnership ttl_cohabit ever_married ever_civil_partnership ever_cohabit lastintdate lastinty lastintm hhorig{
	rename `var' mh_`var' // renaming for ease of finding later, especially when matching partner info
}

save "$temp/partner_history_tomatch.dta", replace

********************************************************************************
* Backup partner history files
********************************************************************************
// see list on table 4 here: https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/list-of-data-files-and-their-descriptions/
// based on below, some of these are in indresp, but not the end dates I don't think?

** Marriage

local waves = "a f"

local i=1

foreach wave in `waves' {
	local waveno=`i'
	use "$UKHLS/`wave'_marriage.dta", clear
	
	foreach var in `wave'_lmary4 `wave'_lmcby4 `wave'_lmend `wave'_lmwwy4 `wave'_lmdvy4 `wave'_lmspy4{
		recode `var' (-9=.)(-2/-1=.)
	}
	
	gen year_end = `wave'_lmwwy4 if `wave'_lmend==2
	replace year_end = `wave'_lmspy4 if `wave'_lmend==1 & `wave'_lmspy4!=. & `wave'_lmspy4!=-8
	replace year_end = `wave'_lmdvy4 if year_end==. & `wave'_lmend==1 & `wave'_lmdvy4!=. & `wave'_lmdvy4!=-8
	replace year_end = `wave'_lmspy4 if year_end==. & `wave'_lmend==97 & `wave'_lmspy4!=. & `wave'_lmspy4!=-8
	replace year_end = `wave'_lmdvy4 if year_end==. & `wave'_lmend==97 & `wave'_lmdvy4!=. & `wave'_lmdvy4!=-8
	replace year_end = `wave'_lmwwy4 if year_end==. & `wave'_lmend==97 & `wave'_lmwwy4!=. & `wave'_lmwwy4!=-8
	
	gen how_end=.
	replace how_end = 1 if inlist(`wave'_lmend,1,97) // divorce / sep
	replace how_end = 2 if `wave'_lmend==2 // widowed
	
	label define how_end 1 "Dissolved" 2 "Widowed" 3 "Ongoing"
	label values how_end how_end
	
	keep pidp `wave'_marno `wave'_lmary4 `wave'_lmcby4 year_end how_end
	
	gen wave = "`wave'"
	
	rename `wave'_* *
	
	save "$temp/marriage_`wave'", replace
	
	local ++i
}

local waves = "bb bk bl" // coding switched in between

local i=1

foreach wave in `waves' {
	local waveno=`i'
	use "$UKHLS/`wave'_marriag.dta", clear
	
	foreach var in `wave'_lmary4 `wave'_lmcby4 `wave'_lmend `wave'_lmwwy4 `wave'_lmdvy4 `wave'_lmspy4{
		recode `var' (-9=.)(-2/-1=.)
	}
		
	gen year_end = `wave'_lmwwy4 if `wave'_lmend==1
	replace year_end = `wave'_lmspy4 if inlist(`wave'_lmend,2,3) & `wave'_lmspy4!=. & `wave'_lmspy4!=-8
	replace year_end = `wave'_lmdvy4 if year_end==. & inlist(`wave'_lmend,2,3) & `wave'_lmdvy4!=. & `wave'_lmdvy4!=-8
	
	gen how_end=.
	replace how_end = 1 if inlist(`wave'_lmend,2,3) // divorce / sep
	replace how_end = 2 if `wave'_lmend==1 // widowed
	replace how_end = 3 if `wave'_lmend==4 // still married
	
	label define how_end 1 "Dissolved" 2 "Widowed" 3 "Ongoing"
	label values how_end how_end
	
	keep pidp `wave'_marno `wave'_lmary4 `wave'_lmcby4 year_end how_end
	
	gen wave = "`wave'"
	
	rename `wave'_* *
	
	save "$temp/marriage_`wave'", replace
	
	local ++i
}


use "$temp/marriage_a", clear
append using "$temp/marriage_f"
append using "$temp/marriage_bb"
append using "$temp/marriage_bk"
append using "$temp/marriage_bl"

tab year_end how_end, m col

gen wavename=.
replace wavename = 1 if wave == "a"
replace wavename = 6 if wave == "f"
replace wavename = 16 if wave == "bb"
replace wavename = 25 if wave == "bk"
replace wavename = 26 if wave == "bl"

gen year=.
replace year=2009 if wavename==1
replace year=2014 if wavename==6
replace year=1992 if wavename==16
replace year=2001 if wavename==25
replace year=2002 if wavename==26

sort pidp marno 
unique pidp
browse

rename lmary4 mh2_year_marr
rename lmcby4 mh2_year_coh
rename year_end mh2_year_end
rename how_end mh2_how_end

drop wave
reshape wide mh2_year_marr mh2_year_coh mh2_year_end mh2_how_end, j(marno) i(pidp year wavename)

save "$temp/marriage_combined_wide.dta", replace

/*
** Cohabitation: I feel like these are all integrated into indresp?
use "$UKHLS/a_cohab.dta", clear
use "$UKHLS/f_cohab.dta", clear
use "$UKHLS/bb_cohabit.dta", clear

** in indresp - make sure I have all. but like none of these are END date
cohlby1 cohlby2 cohlby3 // year started reverse order (most recent to less recent) BH 08 (would that be h?) oh but these are in the main file, just only asked in wave bh 08
cohley1 cohley2 cohley3 // year ended
curmarrby // year current cohab started (wave 14) - these are only for new entrants
lcsby4 // year began cohab spell (1 6 Bh02 BH11 BH12)
lcsey4 // year ended
currcohby
lcmary4 // year of current marriage (1 6 BH11 BH12)
lcmspy4 // year separated (1 6 BH11 BH12)
lmar1y // year of first marriage (good coverage)

** xwlsten
lcmstat // wave marital status last changed
*/

********************************************************************************
* Get variables from cross wave files?
********************************************************************************
use "$UKHLS/xwavedat.dta", clear

// why does it still feel like no good race variables
tab racel_dv xwdat_dv, m col
tab racel_bh xwdat_dv, m col // very bad coverage
tab race_bh xwdat_dv, m col // very bad coverage

local xwave "xwdat_dv sex memorig sampst racel_dv ethn_dv coh1m_dv coh1y_dv evercoh_dv lmar1m_dv lmar1y_dv evermar_dv ch1by_dv anychild_dv paedqf paju maedqf maju lvag16 lvag14 agelh"

foreach var in `xwave'{
	fre `var'
}

foreach var in `xwave'{
	recode `var' (-9/-1=.)
	rename `var' xw_`var' // renaming
}

keep pidp xw_* // to match later

save "$temp/xwave_tomatch.dta", replace 


********************************************************************************
* Attempting to figure out MPF
********************************************************************************
// first, get a list of all child ids by pidp
use "$UKHLS/xhhrel.dta", clear

browse pidp doby_dv bpx_N bpx_pidp_1 bpx_pidp_2 bpx_pidp_3 bpx_pidp_4 bcx_N bcx_pidp_1 bcx_pidp_2 bcx_pidp_3 bcx_pidp_4 bcx_pidp_5

keep pidp sex bcx_N bcx_pidp_*

reshape long bcx_pidp_, i(pidp sex bcx_N) j(child_no)
rename bcx_pidp_ bcx_pidp

drop if bcx_pidp == -8 & inrange(child_no,2,16) // so keep one record for all, even if not parent

save "$temp/parent_child_ids.dta", replace 

// now, clean up file to merge on bio parent info using child as connector
use "$UKHLS/xhhrel.dta", clear

browse pidp sex bpx_N bpx_pidp_1 bpx_sex_1 bpx_pidp_2 bpx_sex_2 bpx_pidp_3 bpx_sex_3 bpx_pidp_4 bpx_sex_4
tab bpx_pidp_1 if bpx_N==0
tab bpx_pidp_2 if bpx_N==0
tab bpx_pidp_3 if bpx_N==0
tab bpx_pidp_4 if bpx_N==0

drop if bpx_N==0

keep pidp bpx_N bpx_pidp_1 bpx_sex_1 bpx_pidp_2 bpx_sex_2 // bpx_pidp_3 bpx_sex_3 bpx_pidp_4 bpx_sex_4
rename pidp bcx_pidp

save "$temp/parent_lookup.dta", replace 

// now merge bio parent info of child onto parent / child info by pidp
use "$temp/parent_child_ids.dta", clear

merge m:1 bcx_pidp using "$temp/parent_lookup.dta"
drop if _merge==2
tab bcx_pidp if _merge==1 // basically no children
drop _merge

browse

recode bpx_sex_1 (-9/0=.)
recode bpx_sex_2 (-9/0=.)

tab bpx_sex_1 bpx_sex_2, m

gen long pidp_mom = bpx_pidp_1 if bpx_sex_1==2
replace pidp_mom = bpx_pidp_2 if bpx_sex_2==2 & pidp_mom==.

gen long pidp_dad = bpx_pidp_1 if bpx_sex_1==1
replace pidp_dad = bpx_pidp_2 if bpx_sex_2==1 & pidp_dad==.

browse pidp sex bcx_pidp pidp_mom pidp_dad bpx_pidp_1 bpx_sex_1 bpx_pidp_2 bpx_sex_2

gen is_mom=0
replace is_mom=1 if pidp == pidp_mom & pidp!=0

gen is_dad=0
replace is_dad=1 if pidp == pidp_dad & pidp!=0

tab sex is_mom, m // validate
tab sex is_dad, m
tab is_mom is_dad, m

gen which_parent=.
replace which_parent = 1 if is_mom==1
replace which_parent = 2 if is_dad==1

label define which_parent 1 "Mom" 2 "Dad"
label values which_parent which_parent

gen long other_parent_id=.
replace other_parent_id = pidp_dad if which_parent==1
replace other_parent_id = pidp_mom if which_parent==2

inspect other_parent_id if which_parent!=. // so some are 0s because just 1 parent listed
inspect bpx_pidp_2 // aka all of the negatives here

sort pidp child_no
browse pidp sex child_no bcx_pidp which_parent other_parent_id

unique other_parent_id if other_parent_id!=0 & other_parent_id!=., by(pidp) gen(num_birth_partners)
bysort pidp (num_birth_partners): replace num_birth_partners = num_birth_partners[1]
tab num_birth_partners, m
tab bpx_pidp_2 if bpx_pidp_2 < 0

gen any_mpf = .
replace any_mpf = 0 if num_birth_partners==0 | num_birth_partners==1
replace any_mpf = 1 if num_birth_partners>1 & num_birth_partners<1000

rename bcx_N num_bio_kids

preserve
collapse (max) any_mpf, by(pidp num_bio_kids)
tab num_bio_kids any_mpf, m
gen long eligible_partner = pidp

save "$temp/mpf_lookup.dta", replace 
restore