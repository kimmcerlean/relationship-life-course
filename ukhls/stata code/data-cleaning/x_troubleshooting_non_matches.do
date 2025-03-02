

********************************************************************************
*UKHLS
********************************************************************************
use "$UKHLS/xwaveid.dta", clear

browse *_ivfio if inlist(pidp,6270971, 14613284, 5300611, 6022771, 32651, 62571, 21868882, 6028891, 61244, 6029571, 78887, 2427684, 206326965, 85007, 863611, 44764482, 103371, 102044, 6562691,4905531)

local waves = "a b c d e f g h i j k l m n"
local years = "2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022"
local i=1

foreach wave in `waves' {
	local waveno=`i'
	local year: word `i' of `years'
	
	rename `wave'_ivfio y`year'_ivfio
	
	local ++i
}

gen last_wave=.
replace last_wave=2009 if lwenum_dv==1
replace last_wave=2010 if lwenum_dv==2
replace last_wave=2011 if lwenum_dv==3
replace last_wave=2012 if lwenum_dv==4
replace last_wave=2013 if lwenum_dv==5
replace last_wave=2014 if lwenum_dv==6
replace last_wave=2015 if lwenum_dv==7
replace last_wave=2016 if lwenum_dv==8
replace last_wave=2017 if lwenum_dv==9
replace last_wave=2018 if lwenum_dv==10
replace last_wave=2019 if lwenum_dv==11
replace last_wave=2020 if lwenum_dv==12
replace last_wave=2021 if lwenum_dv==13
replace last_wave=2022 if lwenum_dv==14

gen first_wave=.
replace first_wave=2009 if fwenum_dv==1
replace first_wave=2010 if fwenum_dv==2
replace first_wave=2011 if fwenum_dv==3
replace first_wave=2012 if fwenum_dv==4
replace first_wave=2013 if fwenum_dv==5
replace first_wave=2014 if fwenum_dv==6
replace first_wave=2015 if fwenum_dv==7
replace first_wave=2016 if fwenum_dv==8
replace first_wave=2017 if fwenum_dv==9
replace first_wave=2018 if fwenum_dv==10
replace first_wave=2019 if fwenum_dv==11
replace first_wave=2020 if fwenum_dv==12
replace first_wave=2021 if fwenum_dv==13
replace first_wave=2022 if fwenum_dv==14

keep pidp y*_ivfio first_wave last_wave

gen survey = 1
label define survey 1 "UKHLS" 2 "BHPS"
label values survey survey

save "$temp/xwave_ukhls.dta", replace

********************************************************************************
*BHPS
********************************************************************************
use "$UKHLS/xwaveid_bh.dta", clear

browse *_ivfio* if inlist(pidp,6270971, 14613284, 5300611, 6022771, 32651, 62571, 21868882, 6028891, 61244, 6029571, 78887, 2427684, 206326965, 85007, 863611, 44764482, 103371, 102044, 6562691,4905531)

local waves = "bb bc bd be bf bg bh bi bj bk bl bm bn bo bp bq br"
local years = "1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009"
local i=1

foreach wave in `waves' {
	local waveno=`i'
	local year: word `i' of `years'
	
	rename `wave'_ivfio y`year'_ivfio
	
	local ++i
}

rename ba_ivfio1_bh y1991_ivfio

gen last_wave=.
replace last_wave=1991 if lwenum_dv_bh==1
replace last_wave=1992 if lwenum_dv_bh==2
replace last_wave=1993 if lwenum_dv_bh==3
replace last_wave=1994 if lwenum_dv_bh==4
replace last_wave=1995 if lwenum_dv_bh==5
replace last_wave=1996 if lwenum_dv_bh==6
replace last_wave=1997 if lwenum_dv_bh==7
replace last_wave=1998 if lwenum_dv_bh==8
replace last_wave=1999 if lwenum_dv_bh==9
replace last_wave=2000 if lwenum_dv_bh==10
replace last_wave=2001 if lwenum_dv_bh==11
replace last_wave=2002 if lwenum_dv_bh==12
replace last_wave=2003 if lwenum_dv_bh==13
replace last_wave=2004 if lwenum_dv_bh==14
replace last_wave=2005 if lwenum_dv_bh==15
replace last_wave=2006 if lwenum_dv_bh==16
replace last_wave=2007 if lwenum_dv_bh==17
replace last_wave=2008 if lwenum_dv_bh==18

gen first_wave=.
replace first_wave=1991 if fwenum_dv_bh==1
replace first_wave=1992 if fwenum_dv_bh==2
replace first_wave=1993 if fwenum_dv_bh==3
replace first_wave=1994 if fwenum_dv_bh==4
replace first_wave=1995 if fwenum_dv_bh==5
replace first_wave=1996 if fwenum_dv_bh==6
replace first_wave=1997 if fwenum_dv_bh==7
replace first_wave=1998 if fwenum_dv_bh==8
replace first_wave=1999 if fwenum_dv_bh==9
replace first_wave=2000 if fwenum_dv_bh==10
replace first_wave=2001 if fwenum_dv_bh==11
replace first_wave=2002 if fwenum_dv_bh==12
replace first_wave=2003 if fwenum_dv_bh==13
replace first_wave=2004 if fwenum_dv_bh==14
replace first_wave=2005 if fwenum_dv_bh==15
replace first_wave=2006 if fwenum_dv_bh==16
replace first_wave=2007 if fwenum_dv_bh==17
replace first_wave=2008 if fwenum_dv_bh==18

keep pidp y*_ivfio first_wave last_wave

gen survey = 2
label define survey 1 "UKHLS" 2 "BHPS"
label values survey survey

save "$temp/xwave_bhps.dta", replace


/// now append
use "$temp/xwave_ukhls.dta", clear
merge 1:1 pidp  using "$temp/xwave_bhps.dta"
drop _merge

// append

tab first_wave survey, m 

browse pidp survey first_wave last_wave
gen long partner_id = pidp

unique pidp
sort pidp

save "$temp/xwave_combined.dta", replace

********************************************************************************
*Explore
********************************************************************************
use "$created_data/ukhls_couple_list.dta", clear

drop if partner_id == .
merge m:1 partner_id using "$temp/xwave_combined.dta"

drop if _merge==2
drop _merge

browse partner_id eligible_rel_start_year eligible_rel_end_year *_ivfio if inlist(partner_id,6270971, 14613284, 5300611, 6022771, 32651, 62571, 21868882, 6028891, 61244, 6029571, 78887, 2427684, 206326965, 85007, 863611, 44764482, 103371, 102044, 6562691,4905531)

gen first_year_status = .
gen last_year_status = .

forvalues y=1991/2022{
	replace first_year_status = y`y'_ivfio if eligible_rel_start_year==`y'
	replace last_year_status = y`y'_ivfio if eligible_rel_end_year==`y'
}

// okay, so many seem like valid missing?

tab first_year_status if inlist(partner_id,6270971, 14613284, 5300611, 6022771, 32651, 62571, 21868882, 6028891, 61244, 6029571, 78887, 2427684, 206326965, 85007, 863611, 44764482, 103371, 102044, 6562691,4905531,274767764,283071164, 279875170, 275027373, 307673562, 277752902,15782924,283465564,1565715647,1565742171,1632367284,1632097931,1632517572,1632138731,1632145531,1632163891,1632259091,1633992604,1634060522,1634469162,1634931562,1632621531,1635468764), m

tab last_year_status if inlist(partner_id,6270971, 14613284, 5300611, 6022771, 32651, 62571, 21868882, 6028891, 61244, 6029571, 78887, 2427684, 206326965, 85007, 863611, 44764482, 103371, 102044, 6562691,4905531,274767764,283071164, 279875170, 275027373, 307673562, 277752902,15782924,283465564,1565715647,1565742171,1632367284,1632097931,1632517572,1632138731,1632145531,1632163891,1632259091,1633992604,1634060522,1634469162,1634931562,1632621531,1635468764), m
