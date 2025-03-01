********************************************************************************
********************************************************************************
* Project: Relationship Life Course Analysis
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: d_imputation
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file takes individuals in couples (created step c)
* and imputes missing years / variables (esp housework)

********************************************************************************
* First, a few final steps for imputation
********************************************************************************
use "$created_data/ukhls_couples_alldurs_long.dta", clear

// few final explorations and recodes
unique pidp // 18613
unique pidp if hiqual_fixed==. //  620 (3%)
unique pidp if xw_ethn_dv==. //  548
unique pidp if xw_racel_dv==. //  597

tab xw_memorig xw_sampst, m row // are these perfectly correlated? no
tab marital_status_imp partnered_imp, m // only need to use one of these

tab aidhrs, m
fre aidhrs
// recoding this so the scale makes more sense. will put varies under 20 into 10-19 and varies over 20 into 20-34
recode aidhrs (8=3)(9=4)(97=.), gen(aidhrs_rec)

label define aidhrs 0 "0" 1 "1-4hrs" 2 "5-9hrs" 3 "10-19hrs" 4 "20-24hrs" 5 "35-49hrs" 6 "50-99hrs" 7 "100+hrs"
label values aidhrs_rec aidhrs

// explore shape of data before imputation
histogram total_hours if total_hours > 0 & total_hours < 80, width(1)
histogram howlng if howlng < 50, width(1)

// recode duration so none negative (-2 becomes 0)
gen duration = relative_duration+2
browse relative_duration duration
tab duration, m

// final cleanup of some variables that shouldn't be duplicated but are...
unique pidp couple_id
unique couple_id pidp eligible_partner
unique couple_id pidp eligible_partner min_dur max_dur first_couple_year last_couple_year

sort pidp int_year
browse pidp eligible_partner int_year year eligible_rel_start_year relative_duration first_couple_year last_couple_year current_rel_start_year min_dur max_dur if min_dur==. browse pidp eligible_partner int_year year eligible_rel_start_year relative_duration first_couple_year last_couple_year current_rel_start_year min_dur max_dur if pidp==4029691
drop if min_dur==. // more than 1 partner in a year
browse pidp eligible_partner int_year year eligible_rel_start_year relative_duration first_couple_year last_couple_year current_rel_start_year min_dur max_dur if first_couple_year==.
browse pidp eligible_partner int_year year eligible_rel_start_year relative_duration first_couple_year last_couple_year current_rel_start_year min_dur max_dur if pidp==82295645

quietly unique dob_year, by(pidp) gen(dob_change)
bysort pidp (dob_change): replace dob_change=dob_change[1]
tab dob_change, m

sort pidp int_year
browse pidp int_year dob_year dob_change if dob_change>  1
by pidp: egen dob = min(dob_year)

browse couple_id pidp eligible_partner int_year orig_record min_dur max_dur first_couple_year last_couple_year

foreach var in min_dur max_dur first_couple_year last_couple_year{
	rename `var' `var'_v0
}

bysort couple_id: egen min_dur = min(min_dur_v0)
bysort couple_id: egen first_couple_year = min(first_couple_year_v0)
bysort couple_id: egen max_dur = max(max_dur_v0)
bysort couple_id: egen last_couple_year = max(last_couple_year_v0)

sort pidp int_year

********************************************************************************
* Reshape wide
********************************************************************************
local reshape_vars "total_hours jbhrs howlng aidhrs aidhrs_rec employed jbstat fimnlabgrs_dv nkids_dv age_youngest_child partnered_imp marital_status_imp fihhmngrs_dv gor_dv orig_record hiqual_dv nchild_dv partnered marital_status_defacto country_all age_all current_rel_start_year current_rel_end_year ivfio sampst hidp psu strata int_year year aidhh aidxhh husits hubuys hufrys huiron humops huboss"

keep couple_id pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status xw_sex min_dur max_dur first_year_observed first_couple_year last_year_observed last_couple_year duration xw_anychild_dv dob hiqual_fixed xw_ethn_dv xw_memorig xw_sampst xw_racel_dv year_first_birth `reshape_vars'

reshape wide `reshape_vars'  ///
, i(couple_id pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status xw_sex) ///
 j(duration)
 
browse couple_id pidp eligible_partner total_hours* howlng* orig_record*

unique couple_id
 
save "$created_data/ukhls_couples_alldurs_wide.dta", replace

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

putexcel set "$root/imputation/ukhls_missingtable.xlsx", replace
mmdesc hidp0-last_couple_year
putexcel A1 = matrix(r(table))

********************************************************************************
**# Start imputation
********************************************************************************
use "$created_data/ukhls_couples_alldurs_wide.dta", clear

foreach var in dob xw_sex hiqual_fixed xw_ethn_dv xw_memorig xw_sampst eligible_rel_start_year{
	drop if `var' == . // most of these have no missing, but so educ and race are complete
}

mi set wide
mi register imputed total_hours* jbhrs* howlng* aidhrs_rec* employed* jbstat* fimnlabgrs_dv* nkids_dv* age_youngest_child* partnered_imp* marital_status_imp* fihhmngrs_dv* gor_dv* xw_anychild_dv
mi register regular dob xw_sex hiqual_fixed xw_ethn_dv xw_memorig xw_sampst eligible_rel_start_year
mi xtset, clear
mi stset, clear

// start imputation
#delimit ;

mi impute chained

/* Employment hours */
(pmm, knn(5) include (              total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) total_hours0
(pmm, knn(5) include (             total_hours0 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) total_hours1
(pmm, knn(5) include (            total_hours0 total_hours1 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) total_hours2
(pmm, knn(5) include (           total_hours0 total_hours1 total_hours2 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) total_hours3
(pmm, knn(5) include (          total_hours0 total_hours1 total_hours2 total_hours3 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) total_hours4
(pmm, knn(5) include (         total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) total_hours5
(pmm, knn(5) include (        total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) total_hours6
(pmm, knn(5) include (       total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) total_hours7
(pmm, knn(5) include (      total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) total_hours8
(pmm, knn(5) include (     total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours10 total_hours11 total_hours12 total_hours13 total_hours14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) total_hours9
(pmm, knn(5) include (    total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours11 total_hours12 total_hours13 total_hours14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) total_hours10
(pmm, knn(5) include (   total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours12 total_hours13 total_hours14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) total_hours11
(pmm, knn(5) include (  total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours13 total_hours14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) total_hours12
(pmm, knn(5) include ( total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) total_hours13
(pmm, knn(5) include (total_hours0 total_hours1 total_hours2 total_hours3 total_hours4 total_hours5 total_hours6 total_hours7 total_hours8 total_hours9 total_hours10 total_hours11 total_hours12 total_hours13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) total_hours14

/* Employment hours (no OT) */
(pmm, knn(5) include (              jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14 total_hours0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) jbhrs0
(pmm, knn(5) include (             jbhrs0 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14  total_hours1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) jbhrs1
(pmm, knn(5) include (            jbhrs0 jbhrs1 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14   total_hours2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) jbhrs2
(pmm, knn(5) include (           jbhrs0 jbhrs1 jbhrs2 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14    total_hours3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) jbhrs3
(pmm, knn(5) include (          jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14     total_hours4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) jbhrs4
(pmm, knn(5) include (         jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14      total_hours5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) jbhrs5
(pmm, knn(5) include (        jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14       total_hours6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) jbhrs6
(pmm, knn(5) include (       jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14        total_hours7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) jbhrs7
(pmm, knn(5) include (      jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14         total_hours8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) jbhrs8
(pmm, knn(5) include (     jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs10 jbhrs11 jbhrs12 jbhrs13 jbhrs14          total_hours9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) jbhrs9
(pmm, knn(5) include (    jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs11 jbhrs12 jbhrs13 jbhrs14           total_hours10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) jbhrs10
(pmm, knn(5) include (   jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs12 jbhrs13 jbhrs14            total_hours11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) jbhrs11
(pmm, knn(5) include (  jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs13 jbhrs14             total_hours12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) jbhrs12
(pmm, knn(5) include ( jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs14              total_hours13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) jbhrs13
(pmm, knn(5) include (jbhrs0 jbhrs1 jbhrs2 jbhrs3 jbhrs4 jbhrs5 jbhrs6 jbhrs7 jbhrs8 jbhrs9 jbhrs10 jbhrs11 jbhrs12 jbhrs13               total_hours14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) jbhrs14

/* Housework hours */
(pmm, knn(5) include (              howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14 jbhrs0 total_hours0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) howlng0
(pmm, knn(5) include (             howlng0 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14  jbhrs1 total_hours1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) howlng1
(pmm, knn(5) include (            howlng0 howlng1 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14   jbhrs2 total_hours2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) howlng2
(pmm, knn(5) include (           howlng0 howlng1 howlng2 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14    jbhrs3 total_hours3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) howlng3
(pmm, knn(5) include (          howlng0 howlng1 howlng2 howlng3 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14     jbhrs4 total_hours4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) howlng4
(pmm, knn(5) include (         howlng0 howlng1 howlng2 howlng3 howlng4 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14      jbhrs5 total_hours5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) howlng5
(pmm, knn(5) include (        howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14       jbhrs6 total_hours6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) howlng6
(pmm, knn(5) include (       howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14        jbhrs7 total_hours7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) howlng7
(pmm, knn(5) include (      howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng9 howlng10 howlng11 howlng12 howlng13 howlng14         jbhrs8 total_hours8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) howlng8
(pmm, knn(5) include (     howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng10 howlng11 howlng12 howlng13 howlng14          jbhrs9 total_hours9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) howlng9
(pmm, knn(5) include (    howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng11 howlng12 howlng13 howlng14           jbhrs10 total_hours10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) howlng10
(pmm, knn(5) include (   howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng12 howlng13 howlng14            jbhrs11 total_hours11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) howlng11
(pmm, knn(5) include (  howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng13 howlng14             jbhrs12 total_hours12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) howlng12
(pmm, knn(5) include ( howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng14              jbhrs13 total_hours13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) howlng13
(pmm, knn(5) include (howlng0 howlng1 howlng2 howlng3 howlng4 howlng5 howlng6 howlng7 howlng8 howlng9 howlng10 howlng11 howlng12 howlng13               jbhrs14 total_hours14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) howlng14

/* Carework hours */
(pmm, knn(5) include (              i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14 jbhrs0 total_hours0 howlng0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) aidhrs_rec0
(pmm, knn(5) include (             i.aidhrs_rec0 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14  jbhrs1 total_hours1 howlng1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) aidhrs_rec1
(pmm, knn(5) include (            i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14   jbhrs2 total_hours2 howlng2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) aidhrs_rec2
(pmm, knn(5) include (           i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14    jbhrs3 total_hours3 howlng3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) aidhrs_rec3
(pmm, knn(5) include (          i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14     jbhrs4 total_hours4 howlng4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) aidhrs_rec4
(pmm, knn(5) include (         i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14      jbhrs5 total_hours5 howlng5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) aidhrs_rec5
(pmm, knn(5) include (        i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14       jbhrs6 total_hours6 howlng6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) aidhrs_rec6
(pmm, knn(5) include (       i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14        jbhrs7 total_hours7 howlng7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) aidhrs_rec7
(pmm, knn(5) include (      i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14         jbhrs8 total_hours8 howlng8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) aidhrs_rec8
(pmm, knn(5) include (     i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14          jbhrs9 total_hours9 howlng9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) aidhrs_rec9
(pmm, knn(5) include (    i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14           jbhrs10 total_hours10 howlng10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) aidhrs_rec10
(pmm, knn(5) include (   i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec12 i.aidhrs_rec13 i.aidhrs_rec14            jbhrs11 total_hours11 howlng11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) aidhrs_rec11
(pmm, knn(5) include (  i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec13 i.aidhrs_rec14             jbhrs12 total_hours12 howlng12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) aidhrs_rec12
(pmm, knn(5) include ( i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec14              jbhrs13 total_hours13 howlng13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) aidhrs_rec13
(pmm, knn(5) include (i.aidhrs_rec0 i.aidhrs_rec1 i.aidhrs_rec2 i.aidhrs_rec3 i.aidhrs_rec4 i.aidhrs_rec5 i.aidhrs_rec6 i.aidhrs_rec7 i.aidhrs_rec8 i.aidhrs_rec9 i.aidhrs_rec10 i.aidhrs_rec11 i.aidhrs_rec12 i.aidhrs_rec13               jbhrs14 total_hours14 howlng14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) aidhrs_rec14

/* Detailed employment status */
(pmm, knn(5) include (              i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14 jbhrs0 total_hours0 howlng0 i.aidhrs_rec0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) jbstat0
(pmm, knn(5) include (             i.jbstat0 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14  jbhrs1 total_hours1 howlng1 i.aidhrs_rec1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) jbstat1
(pmm, knn(5) include (            i.jbstat0 i.jbstat1 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14   jbhrs2 total_hours2 howlng2 i.aidhrs_rec2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) jbstat2
(pmm, knn(5) include (           i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14    jbhrs3 total_hours3 howlng3 i.aidhrs_rec3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) jbstat3
(pmm, knn(5) include (          i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14     jbhrs4 total_hours4 howlng4 i.aidhrs_rec4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) jbstat4
(pmm, knn(5) include (         i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14      jbhrs5 total_hours5 howlng5 i.aidhrs_rec5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) jbstat5
(pmm, knn(5) include (        i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14       jbhrs6 total_hours6 howlng6 i.aidhrs_rec6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) jbstat6
(pmm, knn(5) include (       i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14        jbhrs7 total_hours7 howlng7 i.aidhrs_rec7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) jbstat7
(pmm, knn(5) include (      i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14         jbhrs8 total_hours8 howlng8 i.aidhrs_rec8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) jbstat8
(pmm, knn(5) include (     i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14          jbhrs9 total_hours9 howlng9 i.aidhrs_rec9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) jbstat9
(pmm, knn(5) include (    i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat11 i.jbstat12 i.jbstat13 i.jbstat14           jbhrs10 total_hours10 howlng10 i.aidhrs_rec10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) jbstat10
(pmm, knn(5) include (   i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat12 i.jbstat13 i.jbstat14            jbhrs11 total_hours11 howlng11 i.aidhrs_rec11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) jbstat11
(pmm, knn(5) include (  i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat13 i.jbstat14             jbhrs12 total_hours12 howlng12 i.aidhrs_rec12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) jbstat12
(pmm, knn(5) include ( i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat14              jbhrs13 total_hours13 howlng13 i.aidhrs_rec13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) jbstat13
(pmm, knn(5) include (i.jbstat0 i.jbstat1 i.jbstat2 i.jbstat3 i.jbstat4 i.jbstat5 i.jbstat6 i.jbstat7 i.jbstat8 i.jbstat9 i.jbstat10 i.jbstat11 i.jbstat12 i.jbstat13               jbhrs14 total_hours14 howlng14 i.aidhrs_rec14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) jbstat14

/* Monthly earnings */
(pmm, knn(5) include (              fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 total_hours0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) fimnlabgrs_dv0
(pmm, knn(5) include (             fimnlabgrs_dv0 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 total_hours1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) fimnlabgrs_dv1
(pmm, knn(5) include (            fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 total_hours2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) fimnlabgrs_dv2
(pmm, knn(5) include (           fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 total_hours3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) fimnlabgrs_dv3
(pmm, knn(5) include (          fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 total_hours4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) fimnlabgrs_dv4
(pmm, knn(5) include (         fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 total_hours5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) fimnlabgrs_dv5
(pmm, knn(5) include (        fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 total_hours6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) fimnlabgrs_dv6
(pmm, knn(5) include (       fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 total_hours7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) fimnlabgrs_dv7
(pmm, knn(5) include (      fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 total_hours8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) fimnlabgrs_dv8
(pmm, knn(5) include (     fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 total_hours9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) fimnlabgrs_dv9
(pmm, knn(5) include (    fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 total_hours10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) fimnlabgrs_dv10
(pmm, knn(5) include (   fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv12 fimnlabgrs_dv13 fimnlabgrs_dv14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 total_hours11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) fimnlabgrs_dv11
(pmm, knn(5) include (  fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv13 fimnlabgrs_dv14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 total_hours12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) fimnlabgrs_dv12
(pmm, knn(5) include ( fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 total_hours13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) fimnlabgrs_dv13
(pmm, knn(5) include (fimnlabgrs_dv0 fimnlabgrs_dv1 fimnlabgrs_dv2 fimnlabgrs_dv3 fimnlabgrs_dv4 fimnlabgrs_dv5 fimnlabgrs_dv6 fimnlabgrs_dv7 fimnlabgrs_dv8 fimnlabgrs_dv9 fimnlabgrs_dv10 fimnlabgrs_dv11 fimnlabgrs_dv12 fimnlabgrs_dv13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 total_hours14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) fimnlabgrs_dv14

/* Number of children in HH */
(pmm, knn(5) include (              nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 total_hours0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) nkids_dv0
(pmm, knn(5) include (             nkids_dv0 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 total_hours1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) nkids_dv1
(pmm, knn(5) include (            nkids_dv0 nkids_dv1 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 total_hours2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) nkids_dv2
(pmm, knn(5) include (           nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 total_hours3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) nkids_dv3
(pmm, knn(5) include (          nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 total_hours4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) nkids_dv4
(pmm, knn(5) include (         nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 total_hours5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) nkids_dv5
(pmm, knn(5) include (        nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 total_hours6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) nkids_dv6
(pmm, knn(5) include (       nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 total_hours7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) nkids_dv7
(pmm, knn(5) include (      nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 total_hours8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) nkids_dv8
(pmm, knn(5) include (     nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 total_hours9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) nkids_dv9
(pmm, knn(5) include (    nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv11 nkids_dv12 nkids_dv13 nkids_dv14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 total_hours10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) nkids_dv10
(pmm, knn(5) include (   nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv12 nkids_dv13 nkids_dv14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 total_hours11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) nkids_dv11
(pmm, knn(5) include (  nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv13 nkids_dv14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 total_hours12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) nkids_dv12
(pmm, knn(5) include ( nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 total_hours13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) nkids_dv13
(pmm, knn(5) include (nkids_dv0 nkids_dv1 nkids_dv2 nkids_dv3 nkids_dv4 nkids_dv5 nkids_dv6 nkids_dv7 nkids_dv8 nkids_dv9 nkids_dv10 nkids_dv11 nkids_dv12 nkids_dv13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 total_hours14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) nkids_dv14

/* Age of youngest child */
(pmm, knn(5) include (              age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 total_hours0 i.marital_status_imp0 fihhmngrs_dv0 i.gor_dv0)) age_youngest_child0
(pmm, knn(5) include (             age_youngest_child0 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 total_hours1 i.marital_status_imp1 fihhmngrs_dv1 i.gor_dv1)) age_youngest_child1
(pmm, knn(5) include (            age_youngest_child0 age_youngest_child1 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 total_hours2 i.marital_status_imp2 fihhmngrs_dv2 i.gor_dv2)) age_youngest_child2
(pmm, knn(5) include (           age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 total_hours3 i.marital_status_imp3 fihhmngrs_dv3 i.gor_dv3)) age_youngest_child3
(pmm, knn(5) include (          age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 total_hours4 i.marital_status_imp4 fihhmngrs_dv4 i.gor_dv4)) age_youngest_child4
(pmm, knn(5) include (         age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 total_hours5 i.marital_status_imp5 fihhmngrs_dv5 i.gor_dv5)) age_youngest_child5
(pmm, knn(5) include (        age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 total_hours6 i.marital_status_imp6 fihhmngrs_dv6 i.gor_dv6)) age_youngest_child6
(pmm, knn(5) include (       age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 total_hours7 i.marital_status_imp7 fihhmngrs_dv7 i.gor_dv7)) age_youngest_child7
(pmm, knn(5) include (      age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 total_hours8 i.marital_status_imp8 fihhmngrs_dv8 i.gor_dv8)) age_youngest_child8
(pmm, knn(5) include (     age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 total_hours9 i.marital_status_imp9 fihhmngrs_dv9 i.gor_dv9)) age_youngest_child9
(pmm, knn(5) include (    age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child11 age_youngest_child12 age_youngest_child13 age_youngest_child14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 total_hours10 i.marital_status_imp10 fihhmngrs_dv10 i.gor_dv10)) age_youngest_child10
(pmm, knn(5) include (   age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child12 age_youngest_child13 age_youngest_child14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 total_hours11 i.marital_status_imp11 fihhmngrs_dv11 i.gor_dv11)) age_youngest_child11
(pmm, knn(5) include (  age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child13 age_youngest_child14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 total_hours12 i.marital_status_imp12 fihhmngrs_dv12 i.gor_dv12)) age_youngest_child12
(pmm, knn(5) include ( age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 total_hours13 i.marital_status_imp13 fihhmngrs_dv13 i.gor_dv13)) age_youngest_child13
(pmm, knn(5) include (age_youngest_child0 age_youngest_child1 age_youngest_child2 age_youngest_child3 age_youngest_child4 age_youngest_child5 age_youngest_child6 age_youngest_child7 age_youngest_child8 age_youngest_child9 age_youngest_child10 age_youngest_child11 age_youngest_child12 age_youngest_child13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 total_hours14 i.marital_status_imp14 fihhmngrs_dv14 i.gor_dv14)) age_youngest_child14

/* Marital status */
(pmm, knn(5) include (              i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 total_hours0 fihhmngrs_dv0 i.gor_dv0)) marital_status_imp0
(pmm, knn(5) include (             i.marital_status_imp0 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 total_hours1 fihhmngrs_dv1 i.gor_dv1)) marital_status_imp1
(pmm, knn(5) include (            i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 total_hours2 fihhmngrs_dv2 i.gor_dv2)) marital_status_imp2
(pmm, knn(5) include (           i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 total_hours3 fihhmngrs_dv3 i.gor_dv3)) marital_status_imp3
(pmm, knn(5) include (          i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 total_hours4 fihhmngrs_dv4 i.gor_dv4)) marital_status_imp4
(pmm, knn(5) include (         i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 total_hours5 fihhmngrs_dv5 i.gor_dv5)) marital_status_imp5
(pmm, knn(5) include (        i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 total_hours6 fihhmngrs_dv6 i.gor_dv6)) marital_status_imp6
(pmm, knn(5) include (       i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 total_hours7 fihhmngrs_dv7 i.gor_dv7)) marital_status_imp7
(pmm, knn(5) include (      i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 total_hours8 fihhmngrs_dv8 i.gor_dv8)) marital_status_imp8
(pmm, knn(5) include (     i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 total_hours9 fihhmngrs_dv9 i.gor_dv9)) marital_status_imp9
(pmm, knn(5) include (    i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 total_hours10 fihhmngrs_dv10 i.gor_dv10)) marital_status_imp10
(pmm, knn(5) include (   i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp12 i.marital_status_imp13 i.marital_status_imp14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 total_hours11 fihhmngrs_dv11 i.gor_dv11)) marital_status_imp11
(pmm, knn(5) include (  i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp13 i.marital_status_imp14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 total_hours12 fihhmngrs_dv12 i.gor_dv12)) marital_status_imp12
(pmm, knn(5) include ( i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 total_hours13 fihhmngrs_dv13 i.gor_dv13)) marital_status_imp13
(pmm, knn(5) include (i.marital_status_imp0 i.marital_status_imp1 i.marital_status_imp2 i.marital_status_imp3 i.marital_status_imp4 i.marital_status_imp5 i.marital_status_imp6 i.marital_status_imp7 i.marital_status_imp8 i.marital_status_imp9 i.marital_status_imp10 i.marital_status_imp11 i.marital_status_imp12 i.marital_status_imp13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 total_hours14 fihhmngrs_dv14 i.gor_dv14)) marital_status_imp14

/* Total family income */
(pmm, knn(5) include (              fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 total_hours0 i.gor_dv0)) fihhmngrs_dv0
(pmm, knn(5) include (             fihhmngrs_dv0 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 total_hours1 i.gor_dv1)) fihhmngrs_dv1
(pmm, knn(5) include (            fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 total_hours2 i.gor_dv2)) fihhmngrs_dv2
(pmm, knn(5) include (           fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 total_hours3 i.gor_dv3)) fihhmngrs_dv3
(pmm, knn(5) include (          fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 total_hours4 i.gor_dv4)) fihhmngrs_dv4
(pmm, knn(5) include (         fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 total_hours5 i.gor_dv5)) fihhmngrs_dv5
(pmm, knn(5) include (        fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 total_hours6 i.gor_dv6)) fihhmngrs_dv6
(pmm, knn(5) include (       fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 total_hours7 i.gor_dv7)) fihhmngrs_dv7
(pmm, knn(5) include (      fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 total_hours8 i.gor_dv8)) fihhmngrs_dv8
(pmm, knn(5) include (     fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 total_hours9 i.gor_dv9)) fihhmngrs_dv9
(pmm, knn(5) include (    fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 total_hours10 i.gor_dv10)) fihhmngrs_dv10
(pmm, knn(5) include (   fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv12 fihhmngrs_dv13 fihhmngrs_dv14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 total_hours11 i.gor_dv11)) fihhmngrs_dv11
(pmm, knn(5) include (  fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv13 fihhmngrs_dv14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 total_hours12 i.gor_dv12)) fihhmngrs_dv12
(pmm, knn(5) include ( fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 total_hours13 i.gor_dv13)) fihhmngrs_dv13
(pmm, knn(5) include (fihhmngrs_dv0 fihhmngrs_dv1 fihhmngrs_dv2 fihhmngrs_dv3 fihhmngrs_dv4 fihhmngrs_dv5 fihhmngrs_dv6 fihhmngrs_dv7 fihhmngrs_dv8 fihhmngrs_dv9 fihhmngrs_dv10 fihhmngrs_dv11 fihhmngrs_dv12 fihhmngrs_dv13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 total_hours14 i.gor_dv14)) fihhmngrs_dv14

/* Country (detailed) */
(pmm, knn(5) include (              i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14 jbhrs0 howlng0 i.aidhrs_rec0 i.jbstat0 fimnlabgrs_dv0 nkids_dv0 age_youngest_child0 i.marital_status_imp0 fihhmngrs_dv0 total_hours0)) gor_dv0
(pmm, knn(5) include (             i.gor_dv0 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14  jbhrs1 howlng1 i.aidhrs_rec1 i.jbstat1 fimnlabgrs_dv1 nkids_dv1 age_youngest_child1 i.marital_status_imp1 fihhmngrs_dv1 total_hours1)) gor_dv1
(pmm, knn(5) include (            i.gor_dv0 i.gor_dv1 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14   jbhrs2 howlng2 i.aidhrs_rec2 i.jbstat2 fimnlabgrs_dv2 nkids_dv2 age_youngest_child2 i.marital_status_imp2 fihhmngrs_dv2 total_hours2)) gor_dv2
(pmm, knn(5) include (           i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14    jbhrs3 howlng3 i.aidhrs_rec3 i.jbstat3 fimnlabgrs_dv3 nkids_dv3 age_youngest_child3 i.marital_status_imp3 fihhmngrs_dv3 total_hours3)) gor_dv3
(pmm, knn(5) include (          i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14     jbhrs4 howlng4 i.aidhrs_rec4 i.jbstat4 fimnlabgrs_dv4 nkids_dv4 age_youngest_child4 i.marital_status_imp4 fihhmngrs_dv4 total_hours4)) gor_dv4
(pmm, knn(5) include (         i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14      jbhrs5 howlng5 i.aidhrs_rec5 i.jbstat5 fimnlabgrs_dv5 nkids_dv5 age_youngest_child5 i.marital_status_imp5 fihhmngrs_dv5 total_hours5)) gor_dv5
(pmm, knn(5) include (        i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14       jbhrs6 howlng6 i.aidhrs_rec6 i.jbstat6 fimnlabgrs_dv6 nkids_dv6 age_youngest_child6 i.marital_status_imp6 fihhmngrs_dv6 total_hours6)) gor_dv6
(pmm, knn(5) include (       i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14        jbhrs7 howlng7 i.aidhrs_rec7 i.jbstat7 fimnlabgrs_dv7 nkids_dv7 age_youngest_child7 i.marital_status_imp7 fihhmngrs_dv7 total_hours7)) gor_dv7
(pmm, knn(5) include (      i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14         jbhrs8 howlng8 i.aidhrs_rec8 i.jbstat8 fimnlabgrs_dv8 nkids_dv8 age_youngest_child8 i.marital_status_imp8 fihhmngrs_dv8 total_hours8)) gor_dv8
(pmm, knn(5) include (     i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14          jbhrs9 howlng9 i.aidhrs_rec9 i.jbstat9 fimnlabgrs_dv9 nkids_dv9 age_youngest_child9 i.marital_status_imp9 fihhmngrs_dv9 total_hours9)) gor_dv9
(pmm, knn(5) include (    i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv11 i.gor_dv12 i.gor_dv13 i.gor_dv14           jbhrs10 howlng10 i.aidhrs_rec10 i.jbstat10 fimnlabgrs_dv10 nkids_dv10 age_youngest_child10 i.marital_status_imp10 fihhmngrs_dv10 total_hours10)) gor_dv10
(pmm, knn(5) include (   i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv12 i.gor_dv13 i.gor_dv14            jbhrs11 howlng11 i.aidhrs_rec11 i.jbstat11 fimnlabgrs_dv11 nkids_dv11 age_youngest_child11 i.marital_status_imp11 fihhmngrs_dv11 total_hours11)) gor_dv11
(pmm, knn(5) include (  i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv13 i.gor_dv14             jbhrs12 howlng12 i.aidhrs_rec12 i.jbstat12 fimnlabgrs_dv12 nkids_dv12 age_youngest_child12 i.marital_status_imp12 fihhmngrs_dv12 total_hours12)) gor_dv12
(pmm, knn(5) include ( i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv14              jbhrs13 howlng13 i.aidhrs_rec13 i.jbstat13 fimnlabgrs_dv13 nkids_dv13 age_youngest_child13 i.marital_status_imp13 fihhmngrs_dv13 total_hours13)) gor_dv13
(pmm, knn(5) include (i.gor_dv0 i.gor_dv1 i.gor_dv2 i.gor_dv3 i.gor_dv4 i.gor_dv5 i.gor_dv6 i.gor_dv7 i.gor_dv8 i.gor_dv9 i.gor_dv10 i.gor_dv11 i.gor_dv12 i.gor_dv13               jbhrs14 howlng14 i.aidhrs_rec14 i.jbstat14 fimnlabgrs_dv14 nkids_dv14 age_youngest_child14 i.marital_status_imp14 fihhmngrs_dv14 total_hours14)) gor_dv14

= dob i.xw_ethn_dv i.xw_memorig i.xw_sampst i.hiqual_fixed, by(xw_sex) chaindots add(10) rseed(12345) noimputed augment force showcommand // dryrun // noisily i.eligible_rel_start_year

;
#delimit cr

save "$created_data/ukhls_individs_imputed_wide_bysex", replace

********************************************************************************
**# Reshape back to long to look at descriptives
********************************************************************************

mi reshape long total_hours jbhrs howlng aidhrs aidhrs_rec employed jbstat fimnlabgrs_dv nkids_dv age_youngest_child partnered_imp marital_status_imp fihhmngrs_dv gor_dv orig_record hiqual_dv nchild_dv partnered marital_status_defacto country_all age_all current_rel_start_year current_rel_end_year ivfio sampst hidp psu strata int_year year aidhh aidxhh husits hubuys hufrys huiron humops huboss ///
, i(couple_id pidp eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status xw_sex) ///
 j(duration)
 
mi convert flong

browse couple_id pidp eligible_partner duration total_hours howlng _mi_miss _mi_m _mi_id

gen imputed=0
replace imputed=1 if inrange(_mi_m,1,10)

inspect total_hours if imputed==0
inspect total_hours if imputed==1

inspect howlng if imputed==0
inspect howlng if imputed==1

mi update

save "$created_data/ukhls_individs_imputed_long_bysex", replace

// explore congruence between imputed and not
tabstat total_hours jbhrs howlng, by(imputed) stats(mean sd p50)
tabstat total_hours jbhrs howlng if xw_sex==1, by(imputed) stats(mean sd p50)
tabstat total_hours jbhrs howlng if xw_sex==2, by(imputed) stats(mean sd p50)

tabstat total_hours jbhrs howlng aidhrs_rec employed jbstat fimnlabgrs_dv nkids_dv age_youngest_child partnered_imp marital_status_imp fihhmngrs_dv gor_dv age_all dob hiqual_fixed xw_ethn_dv, by(imputed), stats(mean sd p50) columns(statistics)

twoway (histogram total_hours if imputed==0, width(2) color(blue%30)) (histogram total_hours if imputed==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Employment Hours")
twoway (histogram total_hours if imputed==0 & xw_sex==1, width(2) color(blue%30)) (histogram total_hours if imputed==1 & xw_sex==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Employment Hours")
twoway (histogram total_hours if imputed==0 & xw_sex==2, width(2) color(blue%30)) (histogram total_hours if imputed==1 & xw_sex==2, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Employment Hours")
twoway (histogram total_hours if imputed==0 & total_hours > 0, width(2) color(blue%30)) (histogram total_hours if imputed==1 & total_hours > 0, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Employment Hours")

twoway (histogram howlng if imputed==0, width(2) color(blue%30)) (histogram howlng if imputed==1, width(2) color(red%30)), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) xtitle("Weekly Housework Hours")

preserve

collapse (mean) total_hours jbhrs howlng, by(duration imputed)

twoway (line total_hours duration if imputed==0) (line total_hours duration if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) ytitle("Weekly Employment Hours") title("Avg Employment Hours by Duration") xtitle("Marital Duration") //  yscale(range(30 40))

twoway (line howlng duration if imputed==0) (line howlng duration if imputed==1), legend(order(1 "Observed" 2 "Imputed") rows(1) position(6)) ytitle("Weekly Housework Hours") title("Avg Housework Hours by Duration") xtitle("Marital Duration")

restore

