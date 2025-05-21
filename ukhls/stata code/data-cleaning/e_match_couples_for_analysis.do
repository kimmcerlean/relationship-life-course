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
* This files takes the imputed data from step d at an individual level and
* matches the corresponding imputed partner data.
* It also creates couple-level variables.

use "$created_data/ukhls_individs_imputed_long_bysex", clear

********************************************************************************
* Match couples
********************************************************************************
// browse pidp eligible_partner duration _mi_miss _mi_m _mi_id imputed

// just keep necessary variables - i think this doesn't work if I try to keep variables that weren't imputed
local partnervars "total_hours work_hours jbhrs howlng aidhrs aidhrs_rec employed jbstat fimnlabgrs_dv nkids_dv age_youngest_child partnered_imp marital_status_imp fihhmngrs_dv gor_dv xw_sex xw_memorig xw_sampst xw_racel_dv xw_anychild_dv xw_ethn_dv dob hiqual_fixed first_year_observed last_year_observed imputed age_all" // orig_record hiqual_dv nchild_dv partnered marital_status_defacto country_all current_rel_start_year current_rel_end_year ivfio sampst hidp psu strata int_year year aidhh aidxhh husits hubuys hufrys huiron humops huboss year_first_birth

keep pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status duration min_dur max_dur first_couple_year last_couple_year _mi_miss _mi_m _mi_id  `partnervars'

mi rename eligible_partner x
mi rename pidp eligible_partner
mi rename x pidp // swap unique and partner to match (bc need to know it's the same couple / duration). I guess I could merge on rel_start_date as well

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	mi rename `var' `var'_sp
}

// not sure what happened here...but it's like 9 couples so this is fine for now
bysort pidp eligible_partner: egen rowcount = count(duration) if _mi_m==0
drop if rowcount==30 | rowcount==45

mi update

save "$temp/ukhls_partner_data_imputed.dta", replace

unique pidp eligible_partner 

// match on partner id and duration
use "$created_data/ukhls_individs_imputed_long_bysex", clear

// not sure what happened here...but it's like 9 couples so this is fine for now
bysort couple_id: egen rowcount = count(duration) if _mi_m==0
drop if rowcount==30 | rowcount==45

mi merge 1:1 pidp eligible_partner duration using "$temp/ukhls_partner_data_imputed.dta", keep(match) // gen(howmatch)
// unique pidp eligible_partner, by(howmatch) // this feels like a terrible match rate? but I checked in the original couple list, and it was generally similar. I think the match rate for UKHLS is low (this was true for relative density as well). and I looked at the survey info files and these feel right.
// browse pidp eligible_partner couple_id if howmatch==2

unique pidp eligible_partner

browse pidp eligible_partner duration xw_sex xw_sex_sp total_hours total_hours_sp howlng howlng_sp _mi_m

tab xw_sex xw_sex_sp
drop if xw_sex==xw_sex_sp

rename xw_sex SEX // to match PSID

mi update

save "$created_data/ukhls_couples_imputed_long.dta", replace

********************************************************************************
**# Create variables
********************************************************************************
capture drop weekly_hrs_woman weekly_hrs_man housework_woman housework_man partnered_woman partnered_man num_children_woman num_children_man
mi update

// first, let's make gendered versions of each variable
*paid work
mi passive: gen weekly_hrs_woman=total_hours if SEX==2
mi passive: replace weekly_hrs_woman=total_hours_sp if SEX==1

mi passive: gen weekly_hrs_man=total_hours if SEX==1
mi passive: replace weekly_hrs_man=total_hours_sp if SEX==2

*unpaid work
mi passive: gen housework_woman=howlng if SEX==2
mi passive: replace housework_woman=howlng_sp if SEX==1

mi passive: gen housework_man=howlng if SEX==1
mi passive: replace housework_man=howlng_sp if SEX==2

*relationship status
tab marital_status_imp partnered_imp, m col

mi passive: gen marital_status_woman=marital_status_imp if SEX==2
mi passive: replace marital_status_woman=marital_status_imp_sp if SEX==1

mi passive: gen marital_status_man=marital_status_imp if SEX==1
mi passive: replace marital_status_man=marital_status_imp_sp if SEX==2

mi passive: gen partnered_woman=.
mi passive: replace partnered_woman = 0 if inlist(marital_status_woman,3,4,5,6)
mi passive: replace partnered_woman = 1 if inlist(marital_status_woman,1,2)

mi passive: gen partnered_man=.
mi passive: replace partnered_man = 0 if inlist(marital_status_man,3,4,5,6)
mi passive: replace partnered_man = 1 if inlist(marital_status_man,1,2)

tab partnered_woman partnered_man, m
tab partnered_woman partnered_man if imputed==1, m // well these could also be partnerships that occurred outside of focal partnership

*number of children
tab nkids_dv nkids_dv_sp, m
tab nkids_dv nkids_dv_sp if imputed==1
tab nkids_dv nkids_dv_sp if imputed==0 // match generally better here

mi passive: gen num_children_woman=nkids_dv if SEX==2
mi passive: replace num_children_woman=nkids_dv_sp if SEX==1

mi passive: gen num_children_man=nkids_dv if SEX==1
mi passive: replace num_children_man=nkids_dv_sp if SEX==2

// Stata assert command to check new variables created from imputed  
foreach var in weekly_hrs_woman weekly_hrs_man housework_woman housework_man marital_status_woman marital_status_man partnered_woman partnered_man num_children_woman num_children_man{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

// paid work
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

tab total_hours imputed if jbstat==2, col // is this concerning? 20% of employed have 0 hours in imputed data. Now 14% with updates

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

// rename couple_work_ow couple_work_ow_detailed
// rename couple_work_ow_end couple_work_ow_detailed_end

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

// unpaid work
mi passive: egen couple_hw_total = rowtotal(housework_woman housework_man)
mi passive: gen woman_hw_share = housework_woman / couple_hw_total // this does have missing I think is couple HW total is 0

sum housework_woman, det
sum housework_woman if housework_woman!=0, det
mi passive: egen hw_terc_woman = cut(housework_woman) if housework_woman!=0, group(3) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_terc_woman
tabstat housework_woman, by(hw_terc_woman)

mi passive: egen hw_hilow_woman = cut(housework_woman) if housework_woman!=0, group(2) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_hilow_woman
tabstat housework_woman, by(hw_hilow_woman)

mi passive: egen hw_hilow_man = cut(housework_man) if housework_man!=0, group(2) // https://groups.google.com/g/missing-data/c/sN8PeFuuA4s
tab hw_hilow_man
tabstat housework_man, by(hw_hilow_man)

sum housework_man, det
sum woman_hw_share, det

mi passive: gen couple_hw=.
mi passive: replace couple_hw = 1 if woman_hw_share==1
mi passive: replace couple_hw = 2 if woman_hw_share > 0.60 & woman_hw_share < 1
mi passive: replace couple_hw = 3 if woman_hw_share >= 0.40 & woman_hw_share <= 0.60
mi passive: replace couple_hw = 4 if woman_hw_share < 0.40
mi passive: replace couple_hw = 3 if housework_woman==0 & housework_man==0  // neither is so small, just put in equal

label define couple_hw 1 "Woman All" 2 "Woman Most" 3 "Equal" 4 "Man Most" 5 "Neither HW"
label values couple_hw couple_hw

mi estimate: proportion couple_hw

** investigating equal HW
histogram couple_hw_total if couple_hw==3 & couple_hw_total < 100
sum couple_hw_total if couple_hw==3, det
histogram housework_woman if couple_hw==3 & housework_woman < 50
sum housework_woman if couple_hw==3, det
histogram housework_man if couple_hw==3 & housework_man < 50
sum housework_man if couple_hw==3, det

twoway (histogram housework_woman if couple_hw==3 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_man if couple_hw==3 & housework_man < 50, width(1) color(red%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** investigating she does all HW
histogram housework_woman if couple_hw==1 & housework_woman < 50
sum housework_woman if couple_hw==1, det
sum housework_woman, det

// alt cutpoints: within she does all
	mi passive: egen hw_hilow_woman_gp1 = cut(housework_woman) if housework_woman!=0 & couple_hw==1, group(2)
	tab hw_hilow_woman_gp1 if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman_gp1)
	tab hw_hilow_woman if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman)
		
** investigating she does most HW
histogram housework_woman if couple_hw==2 & housework_woman < 50 // these feel more similar here than in PSID
sum housework_woman if couple_hw==2, det

// alt cutpoints: within she does most
	mi passive: egen hw_hilow_woman_gp2 = cut(housework_woman) if housework_woman!=0 & couple_hw==2, group(3)
	tab hw_hilow_woman_gp2 if couple_hw==2
	tabstat housework_woman, by(hw_hilow_woman_gp2)
	tab hw_terc_woman if couple_hw==2
	tabstat housework_woman, by(hw_terc_woman)
	
** investigating she does most OR all HW
histogram housework_woman if inlist(couple_hw,1,2) & housework_woman < 50
sum housework_woman if inlist(couple_hw,1,2), det

// alt cutpoints: within she does most OR all
	mi passive: egen hw_hilow_woman_combo = cut(housework_woman) if housework_woman!=0 & inlist(couple_hw,1,2), group(2)
	tab hw_hilow_woman_combo if inlist(couple_hw,1,2)
	tabstat housework_woman, by(hw_hilow_woman_combo)

twoway (histogram housework_woman if couple_hw==1 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_woman if couple_hw==2 & housework_woman < 50, width(1) color(red%30)), legend(order(1 "She does all" 2 "She does most") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** should I lookat if he does most? bc that is essentially same size (actually larger) than woman all
histogram housework_man if couple_hw==4 & housework_man < 50
sum housework_man if couple_hw==4, det

	mi passive: egen hw_hilow_man_gp4 = cut(housework_man) if housework_man!=0 & couple_hw==4, group(2)
	tab hw_hilow_man_gp4  if couple_hw==4
	tabstat housework_man, by(hw_hilow_man_gp4)
	tab hw_hilow_man  if couple_hw==4
	tabstat housework_man, by(hw_hilow_man)

* adding consideration of how many hours she does - this is based on TOTAL distribution of HW, not within each bucket
mi passive: gen couple_hw_hrs=.
mi passive: replace couple_hw_hrs = 1 if couple_hw==1 & hw_hilow_woman==1
mi passive: replace couple_hw_hrs = 2 if couple_hw==1 & hw_hilow_woman==0
mi passive: replace couple_hw_hrs = 3 if couple_hw==2 & hw_terc_woman==2
mi passive: replace couple_hw_hrs = 4 if couple_hw==2 & hw_terc_woman==1
mi passive: replace couple_hw_hrs = 5 if couple_hw==2 & hw_terc_woman==0
mi passive: replace couple_hw_hrs = 6 if couple_hw==3 & couple_hw_total > =20 & couple_hw_total < 500
mi passive: replace couple_hw_hrs = 7 if couple_hw==3 & couple_hw_total < 20
mi passive: replace couple_hw_hrs = 8 if couple_hw==4 & hw_hilow_man==1
mi passive: replace couple_hw_hrs = 9 if couple_hw==4 & hw_hilow_man==0
mi passive: replace couple_hw_hrs = 7 if housework_woman==0 & housework_man==0 // neither is so small, just put in equal low

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
mi passive: replace couple_hw_hrs_alt = 6 if couple_hw==3 & couple_hw_total > =20 & couple_hw_total < 500
mi passive: replace couple_hw_hrs_alt = 7 if couple_hw==3 & couple_hw_total < 20
mi passive: replace couple_hw_hrs_alt = 8 if couple_hw==4 & hw_hilow_man_gp4==1
mi passive: replace couple_hw_hrs_alt = 9 if couple_hw==4 & hw_hilow_man_gp4==0
mi passive: replace couple_hw_hrs_alt = 7 if housework_woman==0 & housework_man==0 // neither is so small, just put in equal low

label values couple_hw_hrs_alt couple_hw_hrs

mi estimate: proportion couple_hw couple_hw_hrs couple_hw_hrs_alt 

* Consolidated housework + hours variable
mi passive: gen couple_hw_hrs_combo=.
mi passive: replace couple_hw_hrs_combo = 1 if inlist(couple_hw,1,2) & hw_hilow_woman_combo==1 // 1 = high, 0 = low
mi passive: replace couple_hw_hrs_combo = 2 if inlist(couple_hw,1,2) & hw_hilow_woman_combo==0 
mi passive: replace couple_hw_hrs_combo = 3 if couple_hw==3 & couple_hw_total > =20 & couple_hw_total < 500 // 20 is the median
mi passive: replace couple_hw_hrs_combo = 4 if couple_hw==3 & couple_hw_total < 20
mi passive: replace couple_hw_hrs_combo = 5 if couple_hw==4
mi passive: replace couple_hw_hrs_combo = 4 if housework_woman==0 & housework_man==0  // neither is so small, just put in equal low

label define couple_hw_hrs_combo 1 "Woman Most: High" 2 "Woman Most: Low" 3 "Equal: High" 4 "Equal: Low" 5 "Man Most: All"
label values couple_hw_hrs_combo couple_hw_hrs_combo

mi estimate: proportion couple_hw couple_hw_hrs_alt couple_hw_hrs_combo

//	capture drop couple_hw_hrs_end
//	mi update
//	mi passive: gen couple_hw_hrs_end = couple_hw_hrs
//	mi passive: replace couple_hw_hrs_end = 99 if rel_type==0

// family channel
* relationship type
gen duration_rec = duration
replace duration = duration_rec - 2
label values duration duration_rec . 

browse pidp eligible_partner marital_status_defacto eligible_rel_start_year eligible_rel_end_year year int_year duration duration_rec

mi passive: gen dur_transitioned=.
mi passive: replace dur_transitioned = year_transitioned - eligible_rel_start_year

tab year_transitioned ever_transition, m

browse pidp eligible_partner int_year duration marital_status_imp ever_transition year_transitioned dur_transitioned eligible_rel_start_year eligible_rel_end_year eligible_rel_status duration_rec

tab marital_status_defacto if int_year >= year_transitioned & int_year < = eligible_rel_end_year & imputed==0, m
tab marital_status_imp if int_year >= year_transitioned & int_year < = eligible_rel_end_year, m // so generally already fine
tab marital_status_imp if duration >= dur_transitioned & duration < = max_dur, m // so generally already fine
tab marital_status_imp if int_year <= year_transitioned & int_year > = eligible_rel_start_year & ever_transition==1, m // some of these are married??
tab marital_status_imp if duration <= dur_transitioned & duration >= min_dur & ever_transition==1, m // some of these are married??

capture label drop status
label define status 0 "ended" 1 "ongoing" 99 "attrit"
label values eligible_rel_status status

// browse pidp eligible_partner marital_status_imp int_year duration min_dur max_dur eligible_rel_start_year eligible_rel_end_year eligible_rel_status
tab marital_status_imp if duration > min_dur & duration < max_dur , m
tab marital_status_imp if duration >= min_dur & duration <= max_dur , m

mi passive: gen rel_type=.
mi passive: replace rel_type = 0 if inlist(duration,-2,-1) // pre-rel
mi passive: replace rel_type = 1 if ever_transition==0 & marital_status_imp==1 & duration >= min_dur & duration <= max_dur
mi passive: replace rel_type = 1 if ever_transition==1 & duration >= dur_transitioned & duration <= max_dur // married, post transition
mi passive: replace rel_type = 1 if duration==0 & marital_status_imp==1
mi passive: replace rel_type = 1 if rel_type==. & duration==0 & marital_status_imp[_n+1]==1 & pidp==pidp[_n+1]
mi passive: replace rel_type = 1 if rel_type==. & marital_status_imp==1 & int_year >= eligible_rel_start_year & int_year <= eligible_rel_end_year
mi passive: replace rel_type = 2 if ever_transition==0 & marital_status_imp==2 & duration >= min_dur & duration <= max_dur
mi passive: replace rel_type = 2 if ever_transition==1 & duration <= dur_transitioned & duration >= min_dur // pre transition
mi passive: replace rel_type = 2 if duration==0 & marital_status_imp==2
mi passive: replace rel_type = 2 if rel_type==. & duration==0 & marital_status_imp[_n+1]==2 & pidp==pidp[_n+1]
mi passive: replace rel_type = 2 if rel_type==. & marital_status_imp==2 & int_year >= eligible_rel_start_year & int_year <= eligible_rel_end_year
mi passive: replace rel_type = 3 if duration > max_dur & eligible_rel_status==1 // intact but past end of relationship
mi passive: replace rel_type = 3 if duration > max_dur & eligible_rel_status==99 // estimated attrition
mi passive: replace rel_type = 3 if duration > max_dur & eligible_rel_status==. // estimated attrition
mi passive: replace rel_type = 4 if duration > max_dur & eligible_rel_status==0 // past end of relationship and designated ended
mi passive: replace rel_type = rel_type[_n-1] if rel_type==. & duration >= min_dur & duration <=max_dur & pidp==pidp[_n-1] & rel_type[_n-1]!=0
mi passive: replace rel_type = rel_type[_n+1] if rel_type==. & duration >= 0 & duration <=max_dur & pidp==pidp[_n+1]

// really struggling to fill in the last lingering relationship types...
gsort pidp eligible_partner _mi_m -duration 
// browse pidp eligible_partner duration _mi_m rel_type
mi passive: replace rel_type = rel_type[_n-1] if rel_type==. & duration <=max_dur & pidp==pidp[_n-1] & rel_type[_n-1]!=0

sort pidp eligible_partner _mi_m duration 
mi passive: replace rel_type = rel_type[_n+1] if rel_type==. & duration >= 0 & duration <=max_dur & pidp==pidp[_n+1]
mi passive: replace rel_type = rel_type[_n-1] if rel_type==. & duration >= 0 & duration <=max_dur & pidp==pidp[_n-1] & rel_type[_n-1]!=0

mi passive: replace rel_type = 1 if rel_type==. // best guess based on current info (this is also like 3 people)
mi passive: replace rel_type = rel_type[_n+1] if duration==0 & inlist(rel_type,3,4) & !inlist(rel_type[_n+1],0,3,4) & pidp==pidp[_n+1]

tab rel_type imputed, m

// tab rel_type if imputed==1, m
// browse pidp eligible_partner marital_status_imp rel_type duration min_dur max_dur int_year eligible_rel_start_year eligible_rel_end_year eligible_rel_status orig_record total_hours if inlist(pidp, 4197647, 30776922, 428992285, 837228245, 428992285) // if inlist(pidp,510699,7731844,30620522,89355405,225385325)

label define rel_type 0 "Pre-Relationship" 1 "Married" 2 "Cohab" 3 "Attrited" 4 "Broke Up"
label values rel_type rel_type

tab rel_type, m
mi estimate: proportion rel_type

tab rel_type eligible_rel_status, row
tab rel_type duration

* number of children
tab num_children_woman num_children_man if inlist(rel_type,1,2) & duration>=0, m
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
tab family_type duration

// browse pidp eligible_partner marital_status_imp rel_type if family_type_end == 0 & duration==0
// browse pidp eligible_partner marital_status_imp rel_type duration family_type_end if inlist(pidp,32143682,67551242,67626042,748332543)

browse pidp eligible_partner duration eligible_rel_start_year eligible_rel_end_year min_dur max_dur family_type rel_type marital_status_imp couple_num_children_gp eligible_rel_status 

// use "$created_data/ukhls_couples_imputed_long_recoded.dta", clear

// check
inspect woman_hw_share if couple_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect hw_terc_woman if housework_woman == 0 & imputed==1 // I only did for women with hW hours. so these missings also make sense

foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow couple_hw_total couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children couple_num_children_gp rel_type family_type{  
	inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

// designate that relationship dissolved and create versions of all variables that stop at this point
foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man couple_work couple_work_ow_detailed couple_work_ow couple_hw couple_hw_hrs couple_hw_hrs_alt couple_hw_hrs_combo couple_num_children_gp family_type{
	capture drop `var'_end
	mi update
	mi passive: gen `var'_end = `var'
	mi passive: replace `var'_end = 98 if rel_type==4 // dissolve
	mi passive: replace `var'_end = 99 if rel_type==3 // attrit
}

foreach var in ft_pt_woman_end overwork_woman_end ft_pt_man_end ft_pt_det_woman_end ft_pt_det_man_end overwork_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end couple_num_children_gp_end family_type_end{
	assert `var' !=. if _mi_m!=0
}

label values ft_pt_man_end ft_pt_woman_end ft_pt
label values ft_pt_det_man_end ft_pt_det_woman_end ft_pt_det
label values couple_work_end couple_work
label values couple_work_ow_detailed_end couple_work_ow_detailed
label values couple_work_ow_end couple_work_ow
label values couple_hw_end couple_hw
label values couple_hw_hrs_end couple_hw_hrs
label values couple_hw_hrs_alt_end couple_hw_hrs
label values couple_hw_hrs_combo_end couple_hw_hrs_combo
label values family_type_end family_type

// cross-tabs to explore to figure out potential new variables
tab ft_pt_man_end ft_pt_woman_end, cell
tab ft_pt_det_man_end ft_pt_det_woman_end, cell

// final update and save

mi update

save "$created_data/ukhls_couples_imputed_long_recoded.dta", replace

// mi estimate: proportion couple_hw_hrs_end couple_hw_end if duration >=0 & duration <=10

********************************************************************************
**# Deduplicate
********************************************************************************
drop if duration < 0 | duration > 10
drop duration_rec

mi update

// need to get rid of one record per couple; currently duplicated - but problem is hidp is missing when not in sample so need to create a constant family variable
inspect hidp if duration==0
inspect hidp if duration==1

browse pidp eligible_partner int_year duration hidp

/*
// is there a better place to do this? maybe in earlier step and carry it through? let's do this for now but return to this later?
// I guess, a second option is to just keep all men or all women? if I have all matches and I dropped diff sex couples, then in theory, dropping one gender would drop in half?
mi passive: gen long fam_id =  hidp if duration==1
mi passive: replace fam_id = hidp if fam_id==. & duration==0
mi passive: replace fam_id = hidp if fam_id==. & duration==2
mi passive: replace fam_id = hidp if fam_id==. & duration==5
mi passive: replace fam_id = hidp if fam_id==. & duration==10
bysort pidp eligible_partner (fam_id): replace fam_id = fam_id[1]
inspect fam_id

// unique pidp eligible_partner if fam_id==.

sort pidp eligible_partner imputed _mi_m duration

browse pidp eligible_partner couple_id duration fam_id hidp

unique pidp eligible_partner
unique pidp eligible_partner, by(SEX)
unique fam_id
unique pidp eligible_partner fam_id
tab SEX
rename xw_sex_sp SEX_sp

bysort fam_id duration _mi_m : egen per_id = rank(couple_id)
tab per_id, m // see I think this is not working properly
bysort fam_id duration _mi_m : egen num_couples = max(per_id)

sort pidp eligible_partner imputed _mi_m duration
browse pidp eligible_partner SEX couple_id duration fam_id hidp per_id num_couples if imputed==0
*/

// okay yes, sex should work
tab couple_work if imputed==1
tab couple_work SEX if imputed==1, col

tab family_type if imputed==1
tab family_type SEX if imputed==1, col

// keep if inlist(per_id,1,3,5,7,9,11)
unique pidp eligible_partner // 14126
keep if SEX==2
unique pidp eligible_partner // 7063, was 14126

// need to do age restrictions (18-60)
// keep if age_all>=18 & age_all<=60 // wait, if I drop these now, won't be rectangular anymore...
gen age_flag = 0
replace age_flag = 1 if (age_all>=18 & age_all<=60) & (age_all_sp>=18 & age_all_sp<=60) 

bysort pidp eligible_partner _mi_m: egen age_eligible=total(age_flag)
tab age_eligible, m // 0 means never within age range

sort pidp eligible_partner imputed _mi_m duration
drop if age_eligible==0

mi update

unique pidp eligible_partner // now 6271

save "$created_data/ukhls_couples_imputed_long_deduped.dta", replace

********************************************************************************
**# Quick descriptives for full sample while long
********************************************************************************
//
histogram weekly_hrs_woman if couple_work_ow_end==8
histogram weekly_hrs_man if couple_work_ow_end==8

tab ft_pt_det_man_end ft_pt_det_woman_end if couple_work_ow_end==8, cell
tab ft_pt_man_end ft_pt_woman_end, cell

tab couple_work_ow_end imputed, col
tab couple_work_end imputed, col

// descriptives at all durations
desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_det_woman_end i.ft_pt_man_end i.overwork_man_end i.ft_pt_det_man_end i.couple_work_end i.couple_work_ow_end i.couple_hw_end i.couple_hw_hrs_end i.couple_hw_hrs_alt_end i.rel_type i.couple_num_children_gp_end i.family_type_end, filename("$results/ukhls_mi_desc") stats(mimean)
// desctable i.ft_pt_woman i.overwork_woman i.ft_pt_man i.overwork_man i.couple_work i.couple_work_ow i.couple_hw i.couple_hw_hrs i.rel_type i.couple_num_children_gp i.family_type, filename("$results/mi_desc_all") stats(mimean)  // modify - okay can't use modify but want to see if this replaces the previous or adds a new sheet. okay it replaces the previous oops

mi estimate: proportion couple_work_ow_end family_type_end // validate that this matches. it does

// should I just loop through durations while long? should I confirm the numbers are the same either way? - so here, try to loop through durations

forvalues d=0/10{
	desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_det_woman_end i.ft_pt_man_end i.overwork_man_end i.ft_pt_det_man_end i.couple_work_end i.couple_work_ow_end i.couple_hw_end i.couple_hw_hrs_end i.couple_hw_hrs_alt_end i.rel_type i.couple_num_children_gp_end i.family_type_end if duration==`d', filename("$results/ukhls_mi_desc_`d'") stats(mimean) decimals(4)
}

// mi xeq: proportion couple_hw_end if duration==5 // troubleshooting bc this is where the code stalled. I think this is because some have "neither HW" and some don't. okay, yes that is the problem

mi estimate: proportion couple_work_ow_end family_type_end if duration==0
mi estimate: proportion couple_work_ow_end family_type_end if duration==5

/* oh, wait, I think I can actually just group by duration?? ah, no you cannot do that with mi. i knew this
desctable i.ft_pt_woman_end i.overwork_woman_end i.ft_pt_man_end i.overwork_man_end i.couple_work_end i.couple_work_ow_end i.couple_hw_end i.couple_hw_hrs_end i.rel_type i.couple_num_children_gp_end i.family_type_end, filename("$results/mi_desc_dur") stats(mimean) group(duration)
*/

// I think duration needs to start at 1 to work in r
gen duration_v0 = duration
replace duration = duration + 1

// use "$created_data/ukhls_couples_imputed_long_deduped.dta", clear

drop hidp sampst ivfio hubuys hufrys humops huiron husits huboss year marital_status_defacto partnered current_rel_start_year current_rel_end_year rowcount age_flag age_eligible duration_v0 psu strata // think I need to keep the base variables the passive variables I created are based off of, otherwise, they are reset back to missing I think, which causes problems when I reshape.

mi update

********************************************************************************
**# Reshape back to wide to see the data by duration and compare to long estimates
********************************************************************************

mi reshape wide age_all fihhmngrs_dv gor_dv nkids_dv jbstat aidhh aidxhh aidhrs howlng work_hours jbhrs fimnlabgrs_dv nchild_dv hiqual_dv country_all employed total_hours age_youngest_child partnered_imp marital_status_imp aidhrs_rec int_year orig_record age_all_sp fihhmngrs_dv_sp gor_dv_sp nkids_dv_sp jbstat_sp aidhrs_sp howlng_sp work_hours_sp jbhrs_sp fimnlabgrs_dv_sp employed_sp total_hours_sp age_youngest_child_sp partnered_imp_sp marital_status_imp_sp aidhrs_rec_sp weekly_hrs_woman weekly_hrs_man housework_woman housework_man marital_status_woman marital_status_man partnered_woman partnered_man num_children_woman num_children_man ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man couple_work couple_work_ow couple_work_ow_detailed  couple_hw_total woman_hw_share hw_terc_woman hw_hilow_woman hw_hilow_man couple_hw hw_hilow_woman_gp1 hw_hilow_woman_gp2 hw_hilow_man_gp4 hw_hilow_woman_combo couple_hw_hrs couple_hw_hrs_alt couple_hw_hrs_combo rel_type couple_num_children couple_num_children_gp family_type ft_pt_woman_end overwork_woman_end ft_pt_man_end overwork_man_end ft_pt_det_woman_end ft_pt_det_man_end couple_work_end couple_work_ow_detailed_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_hw_hrs_combo_end couple_num_children_gp_end family_type_end ///
, i(pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status) j(duration)

tab _mi_miss, m // see what happens if I reshape but DON'T convert
tab _mi_m, m

browse pidp eligible_partner _mi_id _mi_miss _mi_m couple_work_end* couple_work_ow_end* couple_hw_hrs_combo_end*
browse pidp eligible_partner _mi_id _mi_miss _mi_m couple_work_end* couple_work_ow_end* couple_hw_hrs_combo_end* if inrange(_mi_m,1,10)

unique pidp eligible_partner // so now there are 6271 uniques and 11 observations for each (base + 10 imputations) - so 68981 observations
unique pidp eligible_partner, by(_mi_m)

// create indicator of complete sequences
gen complete_seq = .
replace complete_seq = 0 if max_dur < 9
replace complete_seq = 1 if max_dur >= 9 // so 9 is really 10 (because 0-9 = 1-10)

tab rel_type10 if complete_seq==1, m
browse pidp eligible_partner min_dur max_dur complete_seq rel_type* // okay, so max_dur is based on start of 0, but I changed it, so now need it to be +1
// browse pidp eligible_partner min_dur max_dur complete_seq rel_type* if complete_seq==1 & rel_type10==3

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
replace complete_seq=0 if complete_seq==1 & sequence_length < 10
drop if sequence_length==0

tab sequence_length complete_seq, m
tab sequence_length sequence_length_alt, m

replace sequence_length = sequence_length_alt if sequence_length==. & complete_seq==0
// replace sequence_length = sequence_length_alt if sequence_length==0 & complete_seq==0
replace sequence_length = 10 if complete_seq==1

// browse pidp eligible_partner min_dur max_dur sequence_length complete_seq rel_type* if complete_seq==1 & rel_type10==3

// note how ended (attrit or dissolve)
tab eligible_rel_status complete_seq, m
browse pidp eligible_partner min_dur max_dur sequence_length eligible_rel_status complete_seq rel_type*

gen status_end = .
forvalues d=1/10{
	local e = `d' + 1
	replace status_end = rel_type`e' if sequence_length == `d'
}

label values status_end rel_type
tab status_end complete_seq, m

browse pidp eligible_partner  min_dur max_dur sequence_length status_end eligible_rel_status rel_type* couple_work_end* if complete_seq==0 // & inlist(status_end,1,2)

gen how_end=.
replace how_end = 0 if complete_seq==1 // intact
replace how_end = 1 if complete_seq==0 & status_end==4 // dissolved
replace how_end = 2 if complete_seq==0 & status_end==3 // attrit

label define how_end 0 "Intact" 1 "Dissolved" 2 "Attrited"
label values how_end how_end

tab how_end complete_seq, m

mi update

save "$created_data/ukhls_couples_imputed_wide.dta", replace 

********************************************************************************
* A few small additional extracts needed
********************************************************************************
// just complete sequences
use "$created_data/ukhls_couples_imputed_wide.dta", clear

keep if complete_seq==1

// browse pidp eligible_partner min_dur max_dur  _mi_m sequence_length couple_hw_hrs_combo_end* couple_work_ow_end* rel_type* if couple_work_ow_end1==. & _mi_m!=0
// browse pidp eligible_partner min_dur max_dur  _mi_m sequence_length couple_hw_hrs_combo_end* couple_work_ow_end* rel_type* if pidp==293086125

mi update

save "$created_data/ukhls_couples_imputed_wide_complete.dta", replace 

/*
// fully wide data (not actually sure we need this)
mi convert wide, clear

save "$created_data/ukhls_couples_imputed_fully_wide.dta", replace

unique pidp eligible_partner // so, I think to what Lea said - the imputations are also wide, need to be long. so now there are 6263 uniques AND 6263 rows
tab _mi_miss, m

browse pidp eligible_partner  min_dur max_dur rel_type* *rel_*
browse pidp eligible_partner couple_work_end1 _*_couple_work_end1 _mi_miss

mi estimate: proportion rel_type1 couple_work_ow_end1 family_type_end1 // okay NOW there are the right number of couples AND they match what I did when long
mi estimate: proportion rel_type1 rel_type2 rel_type3 rel_type4 rel_type5 rel_type6 rel_type7 rel_type8 rel_type9 rel_type10 rel_type11 // ensure all have the right number of people now aka 6263
mi estimate: proportion couple_work_ow_end1 couple_work_ow_end2 couple_work_ow_end3 couple_work_ow_end4 couple_work_ow_end5 couple_work_ow_end6 couple_work_ow_end7 couple_work_ow_end8 couple_work_ow_end9 couple_work_ow_end10 couple_work_ow_end11 // ensure all have the right number of people now aka 6263 - wanted to do with created / imputed var, not just a constant one (rel type is constant)
*/