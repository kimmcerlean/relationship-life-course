set maxvar 10000

cd "/home/kmcerlea/stage/Life Course"

use "created data/stata/ukhls_couples_imputed_long.dta", clear

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
mi passive: replace ft_pt_woman = 2 if weekly_hrs_woman >=35 & weekly_hrs_woman < 180 // FT

mi passive: gen overwork_woman=. // Cha and Weeden = 50 hrs, Cha 2010 = 50 and 60, Munsch = 60
mi passive: replace overwork_woman = 0 if weekly_hrs_woman >= 0 & weekly_hrs_woman < 50
mi passive: replace overwork_woman = 1 if weekly_hrs_woman >=50 & weekly_hrs_woman < 180 

mi passive: gen ft_pt_man = .
mi passive: replace ft_pt_man = 0 if weekly_hrs_man==0 // not working
mi passive: replace ft_pt_man = 1 if weekly_hrs_man > 0 & weekly_hrs_man < 35 // PT
mi passive: replace ft_pt_man = 2 if weekly_hrs_man >=35 & weekly_hrs_man < 180 // FT

mi passive: gen overwork_man=. // Cha and Weeden = 50 hrs, Cha 2010 = 50 and 60, Munsch = 60
mi passive: replace overwork_man = 0 if weekly_hrs_man >= 0 & weekly_hrs_man < 50
mi passive: replace overwork_man = 1 if weekly_hrs_man >=50 & weekly_hrs_man < 180 

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
sum weekly_hrs_woman if ft_pt_woman==1, det
mi passive: gen ft_pt_det_woman = .
mi passive: replace ft_pt_det_woman = 0 if weekly_hrs_woman==0 // not working
mi passive: replace ft_pt_det_woman = 1 if weekly_hrs_woman > 0 & weekly_hrs_woman < 20 // PT: low - either do median using r(p50) or use 20 and cite K&Z? doing the latter for now
mi passive: replace ft_pt_det_woman = 2 if weekly_hrs_woman >= 20 & weekly_hrs_woman < 35 // PT: high
mi passive: replace ft_pt_det_woman = 3 if weekly_hrs_woman >=35 & weekly_hrs_woman < 50 // FT: normal
mi passive: replace ft_pt_det_woman = 4 if weekly_hrs_woman >=50 & weekly_hrs_woman < 180 // FT: overwork

mi passive: gen ft_pt_det_man = .
mi passive: replace ft_pt_det_man = 0 if weekly_hrs_man==0 // not working
mi passive: replace ft_pt_det_man = 1 if weekly_hrs_man > 0 & weekly_hrs_man < 20 // PT: low - either do median using r(p50) or use 20 and cite K&Z?
mi passive: replace ft_pt_det_man = 2 if weekly_hrs_man >= 20 & weekly_hrs_man < 35 // PT: high
mi passive: replace ft_pt_det_man = 3 if weekly_hrs_man >=35 & weekly_hrs_man < 50 // FT: normal
mi passive: replace ft_pt_det_man = 4 if weekly_hrs_man >=50 & weekly_hrs_man < 180 // FT: overwork

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
mi passive: gen couple_work_ow=.
mi passive: replace couple_work_ow = 1 if ft_pt_man == 2 & ft_pt_woman == 0
mi passive: replace couple_work_ow = 2 if ft_pt_man == 2 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 3 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==0
mi passive: replace couple_work_ow = 4 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==1 & overwork_woman==0
mi passive: replace couple_work_ow = 5 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==0 & overwork_woman==1
mi passive: replace couple_work_ow = 6 if ft_pt_man == 2 & ft_pt_woman == 2 & overwork_man==1 & overwork_woman==1
mi passive: replace couple_work_ow = 7 if ft_pt_man == 0 & ft_pt_woman == 2
mi passive: replace couple_work_ow = 7 if ft_pt_man == 1 & ft_pt_woman == 2
mi passive: replace couple_work_ow = 8 if ft_pt_man == 1 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 8 if ft_pt_man == 0 & ft_pt_woman == 0
mi passive: replace couple_work_ow = 8 if ft_pt_man == 0 & ft_pt_woman == 1
mi passive: replace couple_work_ow = 8 if ft_pt_man == 1 & ft_pt_woman == 0

label define couple_work_ow 1 "male bw" 2 "1.5 male bw" 3 "dual FT: no OW" 4 "dual FT: his OW" 5 "dual FT: her OW" 6 "dual FT: both OW" /// 
7 "female bw" 8 "under work"
label values couple_work_ow couple_work_ow

mi estimate: proportion couple_work couple_work_ow

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
// histogram couple_hw_total if couple_hw==3 & couple_hw_total < 100
sum couple_hw_total if couple_hw==3, det
// histogram housework_woman if couple_hw==3 & housework_woman < 50
sum housework_woman if couple_hw==3, det
// histogram housework_man if couple_hw==3 & housework_man < 50
sum housework_man if couple_hw==3, det

// twoway (histogram housework_woman if couple_hw==3 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_man if couple_hw==3 & housework_man < 50, width(1) color(red%30)), legend(order(1 "Women" 2 "Men") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** investigating she does all HW
// histogram housework_woman if couple_hw==1 & housework_woman < 50
sum housework_woman if couple_hw==1, det
sum housework_woman, det

// alt cutpoints: within she does all
	mi passive: egen hw_hilow_woman_gp1 = cut(housework_woman) if housework_woman!=0 & couple_hw==1, group(2)
	tab hw_hilow_woman_gp1 if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman_gp1)
	tab hw_hilow_woman if couple_hw==1
	tabstat housework_woman, by(hw_hilow_woman)
		
** investigating she does most HW
// histogram housework_woman if couple_hw==2 & housework_woman < 50 // these feel more similar here than in PSID
sum housework_woman if couple_hw==2, det

// alt cutpoints: within she does most
	mi passive: egen hw_hilow_woman_gp2 = cut(housework_woman) if housework_woman!=0 & couple_hw==2, group(3)
	tab hw_hilow_woman_gp2 if couple_hw==2
	tabstat housework_woman, by(hw_hilow_woman_gp2)
	tab hw_terc_woman if couple_hw==2
	tabstat housework_woman, by(hw_terc_woman)
	
** investigating she does most OR all HW
// histogram housework_woman if inlist(couple_hw,1,2) & housework_woman < 50
sum housework_woman if inlist(couple_hw,1,2), det

// twoway (histogram housework_woman if couple_hw==1 & housework_woman < 50, width(1) color(blue%30)) (histogram housework_woman if couple_hw==2 & housework_woman < 50, width(1) color(red%30)), legend(order(1 "She does all" 2 "She does most") rows(1) position(6)) xtitle("Weekly HW Hours among Equal HW Couples")

** should I lookat if he does most? bc that is essentially same size (actually larger) than woman all
// histogram housework_man if couple_hw==4 & housework_man < 50
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

//	capture drop couple_hw_hrs_end
//	mi update
//	mi passive: gen couple_hw_hrs_end = couple_hw_hrs
//	mi passive: replace couple_hw_hrs_end = 99 if rel_type==0

// family channel
* relationship type
gen duration_rec = duration
replace duration = duration_rec - 2
label values duration duration_rec . 

mi passive: gen dur_transitioned=.
mi passive: replace dur_transitioned = year_transitioned - eligible_rel_start_year

tab year_transitioned ever_transition, m

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
mi passive: replace rel_type = rel_type[_n-1] if rel_type==. & duration >= min_dur & duration <=max_dur & pidp==pidp[_n-1]
mi passive: replace rel_type = rel_type[_n+1] if rel_type==. & duration >= 0 & duration <=max_dur & pidp==pidp[_n+1]

// tab rel_type if imputed==1, m
// browse pidp eligible_partner marital_status_imp rel_type duration min_dur max_dur int_year eligible_rel_start_year eligible_rel_end_year eligible_rel_status orig_record total_hours // if inlist(pidp,510699,7731844,30620522,89355405,225385325)

label define rel_type 0 "Pre-Relationship" 1 "Married" 2 "Cohab" 3 "Attrited" 4 "Broke Up"
label values rel_type rel_type

tab rel_type, m
mi estimate: proportion rel_type

tab rel_type eligible_rel_status, row

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

// check
inspect woman_hw_share if couple_hw_total == 0 & imputed==1 // so yes, these are missing when couple HW total is 0 because can't divide by 0, will remove from below
inspect hw_terc_woman if housework_woman == 0 & imputed==1 // I only did for women with hW hours. so these missings also make sense

foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man couple_work couple_work_ow couple_hw_total couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children couple_num_children_gp rel_type family_type{  
	// inspect `var' if _mi_m != 0  
	assert `var' != . if _mi_m != 0  
} 

// designate that relationship dissolved and create versions of all variables that stop at this point
foreach var in ft_pt_woman overwork_woman ft_pt_man overwork_man ft_pt_det_woman ft_pt_det_man couple_work couple_work_ow couple_hw couple_hw_hrs couple_hw_hrs_alt couple_num_children_gp family_type{
	capture drop `var'_end
	mi update
	mi passive: gen `var'_end = `var'
	mi passive: replace `var'_end = 98 if rel_type==4 // dissolve
	mi passive: replace `var'_end = 99 if rel_type==3 // attrit
}

foreach var in ft_pt_woman_end overwork_woman_end ft_pt_man_end ft_pt_det_woman_end ft_pt_det_man_end overwork_man_end couple_work_end couple_work_ow_end couple_hw_end couple_hw_hrs_end couple_hw_hrs_alt_end couple_num_children_gp_end family_type_end{
	assert `var' !=. if _mi_m!=0
}

label values ft_pt_man_end ft_pt_woman_end ft_pt
label values ft_pt_det_man_end ft_pt_det_woman_end ft_pt_det
label values couple_work_end couple_work
label values couple_work_ow_end couple_work_ow
label values couple_hw_end couple_hw
label values couple_hw_hrs_end couple_hw_hrs
label values couple_hw_hrs_alt_end couple_hw_hrs
label values family_type_end family_type

// cross-tabs to explore to figure out potential new variables
tab ft_pt_man_end ft_pt_woman_end, cell
tab ft_pt_det_man_end ft_pt_det_woman_end, cell

// final update and save

mi update

save "created data/stata/ukhls_couples_imputed_long_recodes.dta", replace
