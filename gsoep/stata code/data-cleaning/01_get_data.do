********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: get_data.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files gets the data from the GSOEP and reorganizes it to prep for analysis
* First, we take the variables needed from each of the various SOEP datasets
* and rename / recode missings / etc.
* Then, we merge all datasets together to create one master file
* No real recoding or cleaning besides missing values happens in this step

net install soephelp,from("https://git.soep.de/mpetrenz/soephelp/-/raw/master/") replace

/*from website: https://companion.soep.de/Data%20Structure%20of%20SOEPcore/Data%20Sets.html#
(see section on pl)
alt option to not need to load SIX GB of data
use pid hid cid syear plh0149-plh0151 using "$data/pl.dta" // so just use the variables you want (though, I probably have a longer list of variables, but still much less than the full dataset)
*/

********************************************************************************
********************************************************************************
********************************************************************************
* Individual files
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
**# PPATHL
*** individual tracking file. This file includes all respondents ever in a SOEP
*** HH, even if they didn't respond in a given year. It is recommended that you
*** use this file as your base dataset on which to merge all other variables
********************************************************************************

// use "$GSOEP/ppathl.dta", clear
// let's try this way
use pid syear hid cid sex parid partner psample sampreg netto germborn corigin gebjahr eintritt erstbefr austritt letztbef birthregion_ew using "$GSOEP/ppathl.dta", clear

label language EN

unique pid // 198137, 1446266
tab syear, m

/*Netto-codes 10-19 (and 29) define the respondents population of PGEN,
the codes 20-28 indicate children (29 is actually youth refugee sample also)
30-39 unit-non-responses in partially realized households,
and the codes 90-99 describe permanent (or temporary) dropouts.
Further differentiations point to the survey instruments (questionnaires). 
The Codes 10-39 describe the population in realized (and partially realized households).*/

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

recode netto (10/19=1)(20/29=2)(30/39=3)(40/99=0)(-3=0), gen(status)
label define status 0 "dropout" 1 "sample" 2 "youth" 3 "no int"
label values status status

sort pid syear

rename sex sex_pl
rename parid partner_id_pl
rename partner partnered_pl
rename gebjahr birthyr_pl
rename germborn born_germany_pl
rename corigin country_born_pl
rename eintritt firstyr_contact_pl
rename erstbefr firstyr_survey_pl
rename austritt lastyr_contact_pl
rename letztbef lastyr_survey_pl
rename netto survey_status_pl
rename status status_pl
rename psample psample_pl
rename sampreg where_germany_pl
rename birthregion_ew where_born_germany_pl

browse

label define partnered 0 "No partner" 1 "Spouse" 2 "Partner" 3 "Prob Spouse" ///
4 "Prob Partner" 5 "Not Clear"
label values partnered_pl partnered

tab birthyr_pl, m

tab birthyr_pl where_born_germany_pl, m // okay the year of reunification doesn't quite explain this. I guess it could be if not born in Germany?
tab born_germany_pl where_born_germany_pl, m
tab birthyr_pl where_born_germany_pl if born_germany_pl==1, m

save "$temp/ppathl_cleaned.dta", replace

inspect partner_id_pl if inrange(partnered_pl,1,4) // so all except 6 have valid id
inspect partner_id_pl if inrange(partnered_pl,1,2) // all except 2 for the stricter definition

// for use later to add partner sample status
keep pid syear sex_pl status_pl
rename pid parid // to match to partner id
gen long partner_id_pl = parid
rename status_pl status_sp
rename sex sex_sp

save "$temp/ppathl_partnerstatus.dta", replace

********************************************************************************
**# PL
*** All original individual data
********************************************************************************
use pid syear hid cid intid pab0002 pab0004 pab0005 pab0006 pab0008 pab0013 plb0022_h plb0185_v1 plb0185_v2 plb0186_h plb0193* plb0193_v1 plb0193_v2 plb0196_h plb0197 plb0471_h plb0474_h plc0013_h plc0014_h plc0015_h plc0017_h pld0038 pld0039 pld0040 pld0131_h pld0132_h pld0133 pld0134 pld0135 pld0136 pld0137 pld0138 pld0139 pld0140 pld0141 pld0142 pld0143 pld0144 pld0145 pld0149 pld0150 pld0151 pld0152 pld0153 pld0154 pld0159 ple0008 ple0040 ple0041_h plg0012_h plg0012_v1 plg0012_v2 plh0007 plh0012_h plh0173 plh0174 plh0175 plh0176 plh0178 plh0179 plh0180 plh0182 plh0258_h pli0011 pli0012_h pli0016_h pli0019_h pli0022_h pli0038_h pli0038_v1 pli0038_v2 pli0038_v3 pli0038_v4 pli0040 pli0043_h pli0044_h pli0046 pli0054 pli0055 pli0057 plk0001_v1 plk0001_v2 plk0001_v3 pli0034* pli0049_h pli0031_h using "$GSOEP/pl.dta", clear // plb0193_h

label language EN

unique pid // 125983, 820360
tab syear, m

// mvdecode _all, mv(-8/-1) // do I want to retain -2 in some way bc it designates does not apply (so helps figure out who is in sample in a given question?)
mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename pab0002 py_employed_pt
rename pab0004 py_unemployed
rename pab0005 py_retired
rename pab0006 py_matleave
rename pab0008 py_housewife
rename pab0013 py_inschool
rename plb0022_h employment
rename plb0185_v1 hours_contracted_v1
rename plb0185_v2 hours_contracted_v2
rename plb0186_h hours_per_wk
rename plb0193_v1 work_ot_v1
rename plb0193_v2 work_ot_v2
rename plb0196_h work_ot_lastmonth
rename plb0197 hours_ot
rename plb0471_h gross_salary_ly
rename plb0474_h gross_selfincome_ly
rename plc0013_h gross_income_lastmonth
rename plc0014_h net_income_lastmonth
rename plc0015_h salary_received_ly
rename plc0017_h net_wages_ly
rename pld0038 chg_newpartner
rename pld0039 chg_newpartner_month
rename pld0040 chg_newpartner_month_py
rename pld0131_h marst
rename pld0132_h partnered
rename pld0133 partner_in_hh
rename pld0134 chg_married
rename pld0135 chg_married_month
rename pld0136 chg_married_month_py
rename pld0137 chg_cohabit
rename pld0138 chg_cohabit_month
rename pld0139 chg_cohabit_month_py
rename pld0140 chg_divorced
rename pld0141 chg_divorced_month
rename pld0142 chg_divorced_month_py
rename pld0143 chg_separated
rename pld0144 chg_separated_month
rename pld0145 chg_seaprated_month_py
rename pld0149 chg_childleft
rename pld0150 chg_childleft_month
rename pld0151 chg_childleft_month_py
rename pld0152 chg_hadbirth
rename pld0153 chg_hadbirth_month
rename pld0154 chg_hadbirth_month_py
rename pld0159 nochg_composition
rename ple0008 self_reported_health
rename ple0040 disability_yn
rename ple0041_h disability_amount
rename plg0012_h currently_enrolled
rename plg0012_v1 currently_enrolled_v1
rename plg0012_v2 currently_enrolled_v2
rename plh0007 political_interest
rename plh0012_h political_affiliation
rename plh0173 satisfaction_work
rename plh0174 satisfaction_housework
rename plh0175 satisfaction_hhincome
rename plh0176 satisfaction_income
rename plh0178 satisfaction_leisure
rename plh0179 satisfaction_childcare
rename plh0180 satisfaction_family
rename plh0182 life_satisfaction
rename plh0258_h religious_affiliation
rename pli0011 errands_sundays
rename pli0012_h housework_saturdays
rename pli0016_h housework_sundays
rename pli0019_h childcare_saturdays
rename pli0022_h childcare_sundays
rename pli0038_h hours_typical_weekday
rename pli0038_v1 working_weekdays_v1
rename pli0038_v2 working_weekdays_v2
rename pli0038_v3 working_weekdays_v3
rename pli0038_v4 working_weekdays_v4
rename pli0040 errands_weekdays
rename pli0043_h housework_weekdays
rename pli0044_h childcare_weekdays
rename pli0046 support_weekdays
rename pli0054 errands_saturdays
rename pli0055 support_saturdays
rename pli0057 support_sundays
rename pli0049_h repair_weekdays
rename pli0031_h repair_saturdays
rename pli0034_v1 repair_sundays_v1
rename pli0034_v2 repair_sundays_v2
rename pli0034_v3 repair_sundays_v3
rename pli0034_v4 repair_sundays_v4
rename plk0001_v1 partnerid_v1
rename plk0001_v2 partnerid_v2
rename plk0001_v3 partnerid_v3

save "$temp/pl_cleaned.dta", replace

// tabstat childcare_weekdays housework_weekdays, by(syear) stats(mean N)
// tabstat childcare_saturdays childcare_sundays housework_saturdays housework_sundays, by(syear) stats(mean N)
// tabstat errands_weekdays errands_sundays errands_saturdays, by(syear) stats(mean N)
// tabstat support_weekdays support_saturdays support_sundays, by(syear) stats(mean N)
// tabstat repair_weekdays repair_saturdays repair_sundays_v1 repair_sundays_v2 repair_sundays_v3 repair_sundays_v4, by(syear) stats(mean N)

********************************************************************************
**# PGEN
*** Generated variables (built from pl and other files by SOEP team to facilitate
*** analysis); these are recommended over pl variables if both exist
********************************************************************************
use pid syear hid cid pgbilzeit pgcasmin pgdegree pgemplst pgfamstd pgisced11 pgisced97 pglabgro pglabnet pglfs pgnation pgpartnr pgpartz pgpbbil01 pgpbbil02 pgpsbil  pgtatzeit pguebstd pgvebzeit pgstib using "$GSOEP/pgen.dta", clear
label language EN

unique pid // 125983, 820360
tab syear, m
sort pid syear

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

browse pid syear hid cid pgfamstd pgpartz pgpartnr pgemplst pglfs pgtatzeit pglabgro pglabnet

rename pgfamstd marital_status_pg
rename pgpartz partnered_pg
rename pgpartnr partner_id_pg
rename pgbilzeit yrs_educ_pg
rename pgcasmin casmin_pg
rename pgdegree tertiary_type_pg
rename pgpbbil02 college_type_pg
rename pgpsbil last_degree_pg
rename pgpbbil01 vocational_degree_pg
rename pgisced11 isced11_pg
rename pgisced97 isced97_pg
rename pgemplst emplst_pg
rename pglfs laborforce_pg
rename pglabgro gross_labor_inc_pg
rename pglabnet net_labor_inc_pg
rename pgtatzeit work_hours_pg
rename pgvebzeit work_hours_agreed_pg
rename pguebstd overtime_pg
rename pgnation nationality_pg
rename pgstib occupation_position_pg

capture label define partnered 0 "No partner" 1 "Spouse" 2 "Partner" 3 "Prob Spouse" ///
4 "Prob Partner" 5 "Not Clear"
label values partnered_pg partnered

inspect partner_id_pg if inrange(partnered_pg,1,4) // just 2 missing

save "$temp/pgen_cleaned.dta", replace

********************************************************************************
**# PBRUTTO
*** Individual tracking file, mostly has things about survey status / interview 
*** info but does also have some demos
********************************************************************************
use pid syear hid cid befstat_h pnat_h stell_h stell_v1 stell_v2 stell_v3 stell_v4 geburt_h using "$GSOEP/pbrutto.dta", clear
label language EN

unique pid // 198138, 1443750
tab syear, m
sort pid syear

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename befstat_h survey_status_pb
rename pnat_h nationality_pb
rename stell_h relation_pb
rename stell_v1 relation_v1_pb
rename stell_v2 relation_v2_pb
rename stell_v3 relation_v3_pb
rename stell_v4 relation_v4_pb
rename geburt_h birthyr_pb

tab birthyr_pb, m

save "$temp/pbrutto_cleaned.dta", replace

********************************************************************************
**# PEQUIV
*** This is the file created for CNEF (to be comparable to other countries)
*** Because of that, some of the variables are nicely coded in a way i am used to
********************************************************************************
use pid syear hid cid d11105 d11106 d11107 d11108 d11109 l11101 h11103 h11104 h11105 h11106 h11107 h11108 h11109 m11126 m11124 using "$GSOEP/pequiv.dta", clear // d11112ll
label language EN

unique pid // 196531, 1196228
tab syear, m
sort pid syear

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename d11105 relation_cnef
rename d11106 hh_size_cnef
rename d11107 num_children_cnef
rename d11108 educ_hs_cnef
rename d11109 yrs_educ_cnef
rename l11101 federal_state_cnef
rename h11103 num_hh_0_1_cnef
rename h11104 num_hh_2_4_cnef
rename h11105 num_hh_5_7_cnef
rename h11106 num_hh_8_10_cnef
rename h11107 num_hh_11_12_cnef
rename h11108 num_hh_13_15_cnef
rename h11109 num_hh_16_18_cnef
rename m11126 self_reported_health_cnef
rename m11124 disability_yn_cnef

save "$temp/pequiv_cleaned.dta", replace

********************************************************************************
**# BIOL
*** This is the biography interview. In theory, they only get this their first
*** year in the SOEP. Many variables are time constant (like family background)
*** I *believe* for those that are not, they update using pl or other datasets
*** I need to explore this further and see if I can / need to copy
*** time constant info to other rows
********************************************************************************
use pid syear hid cid lb0059 lb0060 lb0066 lb0067 lb0068 lb0069 lb0070 lb0071 lb0072 lb0073 lb0077 lb0371 lb0372 lb0374 lb0078_v1 lb0078_v2 lb0078_v3 lb0079_v1 lb0079_v2 lb0079_v3 lb0082_h lb0083_h lb0090_h lb0091_h lb0110 lb0111 lb0285 lb0286_h lb0287_h lb0290_h lb0293_h lb0296_h lb0299_h lb0302_h lb0305_h lb0308_h lb0311 lb0312* lb0312_v1 lb0312_v2 lb0313* lb0313_v1 lb0313_v2 lb0314_h lb0315_h lb0316 lb0317_h lb0318_h lb0319 lb0320_h lb0640 lb0641 lb0650 lb0651 lb0663 lb0664 lb0675 lb0676 lb0679 lb0680 lb0795 lb0805 lb0817 lb0829 lb0833 lb0834 lb0835 lb0836 lb0964 lb0973 lb0982 lb0991 lb0992 lb0993 lb1105_v1 lb1105_v2 lb1106_v1 lb1106_v2 lb1107_v1 lb1107_v2 lb1108_v1 lb1108_v2 using "$GSOEP/biol.dta", clear
// not found: lb0312_h
label language EN

unique pid // 111701, 149324
tab syear, m

sort pid syear

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename lb0059 live_fam_bl
rename lb0060 who_lived_with_bl
rename lb0066 num_yrs_bio_bl
rename lb0067 num_yrs_singlemom_bl
rename lb0068 num_yrs_partneredmom_bl
rename lb0069 num_yrs_singledad_bl
rename lb0070 num_yrs_partnereddad_bl
rename lb0071 num_yrs_otherrel_bl
rename lb0072 num_yrs_foster_bl
rename lb0073 num_yrs_inhome_bl
rename lb0077 num_parent_in_hh_v1_bl
rename lb0371 num_parent_in_hh_v2_bl
rename lb0372 father_in_hh_bl
rename lb0374 mother_in_hh_bl
rename lb0078_v1 where_father_live_v1_bl
rename lb0078_v2 where_father_live_v2_bl
rename lb0078_v3 where_father_live_v3_bl
rename lb0079_v1 where_mother_live_v1_bl
rename lb0079_v2 where_mother_live_v2_bl
rename lb0079_v3 where_mother_live_v3_bl
rename lb0082_h yr_father_born_bl
rename lb0083_h yr_mother_born_bl
rename lb0090_h father_educ_bl
rename lb0091_h mother_educ_bl
rename lb0110 father_training_bl
rename lb0111 mother_training_bl
rename lb0285 num_children_bl
rename lb0286_h has_no_children_bl
rename lb0287_h yr_birth_child1_bl
rename lb0290_h yr_birth_child2_bl
rename lb0293_h yr_birth_child3_bl
rename lb0296_h yr_birth_child4_bl
rename lb0299_h yr_birth_child5_bl
rename lb0302_h yr_birth_child6_bl
rename lb0305_h yr_birth_child7_bl
rename lb0308_h yr_birth_child8_bl
rename lb0311 ever_married_bl
// rename lb0312_h marr1_start_bl
rename lb0312_v1 marr1_start_v1_bl
rename lb0312_v2 marr1_start_v2_bl
// rename lb0313 marr1_status_bl
rename lb0313_v1 marr1_status_v1_bl
rename lb0313_v2 marr1_status_v2_bl
rename lb0314_h marr1_end_bl
rename lb0315_h marr2_start_bl
rename lb0316 marr2_status_bl
rename lb0317_h marr2_end_bl
rename lb0318_h marr3_start_bl
rename lb0319 marr3_status_bl
rename lb0320_h marr3_end_bl

by pid: egen num_records_bl = count(syear)
tab num_records_bl, m
unique pid, by(num_records_bl) // so yes 74.6% of pids just have one record.
// So, I think this is really keyed on pid because many respondents only have 1 year in this file - they year they entered. which I bet corresponds to their sample
// The average PY is like 1.36, so...I need to figure out, for respondents with multiple years, which to use, and then I *think* this essentially becomes a m:1 match on pid?

browse pid syear num_records marr1_start_v1_bl marr1_start_v2_bl marr1_status_v1_bl marr1_status_v2_bl marr1_end_bl marr2_start_bl marr2_status_bl marr2_end_bl father_in_hh_bl where_father_live_v1_bl where_father_live_v2_bl where_father_live_v3_bl
// okay but there are still a lot of missing...

save "$temp/biol_cleaned.dta", replace

********************************************************************************
**# BIOPAREN
*** This info comes from biol, but is specifically compiled for history
*** About mother and father
*** HOWEVER, this is not always up to date up to the same info as the main survey
*** so need to figure this out. Think only updated through 2019 respondents atm
********************************************************************************
use pid cid fcurrloc fprofedu fsedu locchild1 mcurrloc mprofedu msedu bioyear living* using "$GSOEP/bioparen.dta", clear // this is JUST keyed on pid - so it is 1 row per pid, NOT long
label language EN

unique pid // 118608, 118608
// tab syear, m // right because year is also not a variable. so how do I know when updated?!
// but I guess - the info that exists shouldn't change, it might just possibly not have any NEW people?
tab bioyear, m // okay so in v39, biography surveys not updated past 2019, but now, they are updated through 2023 as well - so I think this file is actually up to date in this version! YES confirmed in the What's new
// let's compare info listed here to info listed in biol; this feels more comprehensive (and cleaned AND already at unique pid level)

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename fcurrloc where_father_live_bp
rename fprofedu father_vocational_bp
rename fsedu father_educ_bp
rename locchild1 live_fam_bp
rename mcurrloc where_mother_live_bp
rename mprofedu mother_vocational_bp
rename msedu mother_educ_bp
rename bioyear bioyear_bp
rename living1 num_yrs_bio_bp
rename living2 num_yrs_singlemom_bp
rename living3 num_yrs_partneredmom_bp
rename living4 num_yrs_singledad_bp
rename living5 num_yrs_partnereddad_bp
rename living6 num_yrs_otherrel_bp
rename living7 num_yrs_foster_bp
rename living8 num_yrs_inhome_bp

save "$temp/bioparen_cleaned.dta", replace

********************************************************************************
********************************************************************************
********************************************************************************
* Household information files
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
**# HL
*** All original HH data
********************************************************************************
use hid syear cid intid hlc0005_h hlf0001_h hlf0153_h hlf0261 hlf0291 hlf0315_h hlf0317_h hlf0320 hlk0044_v1 hlk0044_v2 hlk0044_v3 using "$GSOEP/hl.dta", clear
label language EN

unique hid // 72943, 468837
unique hid syear // so this is the level of the file

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

tab hlk0044_v1, m // children under 16 in HH - oh this is just yes / no. generally good coverage, about 3.5% missing. let's see what other HH variables I might need?
tab hlk0044_v2, m 
tab hlk0044_v3, m 

rename hlc0005_h hh_net_income_monthly_hl
rename hlf0001_h housing_status_hl
rename hlf0153_h area_live_hl
rename hlf0261 outside_help_hl
rename hlf0291 aid_in_hh_hl
rename hlf0315_h aid_outside_hh_hl
rename hlf0317_h aid_inside_hh_hl
rename hlf0320 aid_private_care_hl
rename hlk0044_v1 num_children_u16_v1_hl
rename hlk0044_v3 num_children_u16_v2_hl
rename hlk0044_v2 num_children_u17_hl // feel like they changed the age range? need to investigate this more, but probably will combine

save "$temp/hl_cleaned.dta", replace

********************************************************************************
**# HGEN
*** Cleaned up / standardized HH file
********************************************************************************
use hid syear cid hghinc hgowner hgnuts1 hgnuts1_ew hgtyp1hh hgtyp2hh using "$GSOEP/hgen.dta", clear
label language EN

unique hid // 72943, 468837 // so this matches hl (the same is NOT true for pgen and pl...okay this has ALSO been corrected in v40)

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename hghinc hh_net_income_monthly_hg
rename hgowner housing_status_hg
rename hgnuts1 federal_state_hg
rename hgnuts1_ew where_germany_hg
rename hgtyp1hh hh_type_hg
rename hgtyp2hh hh_type_det_hg

save "$temp/hgen_cleaned.dta", replace

********************************************************************************
**# HBRUTTO
*** HH Tracking file; mostly contains survey info but also has some demo variables
********************************************************************************
use hid syear cid befhpmax bula_ew bula_h hhgr using "$GSOEP/hbrutto.dta", clear
label language EN

unique hid // 79975, 565967 // this has more because contains HHs EVER surveyed.

mvdecode _all, mv(-9=.\-8=.s\-7/-6=.\-5=.s\-4/-3=.\-2=.n\-1=.) // .s = not in survey, .n is n/a, regular missing is dk, etc. -9 new as of v40

rename befhpmax person_surveyed_hb
rename bula_ew where_germany_hb
rename bula_h federal_state_hb
rename hhgr hh_size_hb

save "$temp/hbrutto_cleaned.dta", replace
