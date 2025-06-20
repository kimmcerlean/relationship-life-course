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
**# Import data (created in step a) and do some data cleaning / recoding
********************************************************************************
********************************************************************************

use "$created_data/UKHLS_long_all.dta", clear
drop if pidp==. // think these are HHs that didn't match?

// Right now 1-13 are ukhls and 14-31 are bhps, so the wave order doesn't make a lot of sense. These aren't perfect but will work for now.
// okay i added interview characteristics, so use that?
browse pidp pid wavename intdatey intdaty_dv istrtdaty // istrtdaty seems the most comprehensive. the DV one is only UKHLS
// okay, but sometimes consecutive surveys are NOT consecutive years? https://www.understandingsociety.ac.uk/documentation/mainstage/survey-timeline/

gen year=.
replace year=2009 if wavename==1
replace year=2010 if wavename==2
replace year=2011 if wavename==3
replace year=2012 if wavename==4
replace year=2013 if wavename==5
replace year=2014 if wavename==6
replace year=2015 if wavename==7
replace year=2016 if wavename==8
replace year=2017 if wavename==9
replace year=2018 if wavename==10
replace year=2019 if wavename==11
replace year=2020 if wavename==12
replace year=2021 if wavename==13
replace year=2022 if wavename==14
replace year=1991 if wavename==15
replace year=1992 if wavename==16
replace year=1993 if wavename==17
replace year=1994 if wavename==18
replace year=1995 if wavename==19
replace year=1996 if wavename==20
replace year=1997 if wavename==21
replace year=1998 if wavename==22
replace year=1999 if wavename==23
replace year=2000 if wavename==24
replace year=2001 if wavename==25
replace year=2002 if wavename==26
replace year=2003 if wavename==27
replace year=2004 if wavename==28
replace year=2005 if wavename==29
replace year=2006 if wavename==30
replace year=2007 if wavename==31
replace year=2008 if wavename==32

// key DoL variables: husits howlng hubuys hufrys huiron humops jbhrs huboss (but in less waves of the old survey)
// key other variables: scend_dv jbstat mastat mastat_dv nchild_dv marstat age age_dv doby doby_dv qfhigh_dv hiqual_dv racel_dv ethn_dv sex sex_dv
// key ID variables: pidp hidp pno
// key linking variables: ppid ppno sppid sppno

** Demographic and related variables

// first, get variables from xwave file
merge m:1 pidp using "$temp/xwave_tomatch.dta" // only 4 didn't match
drop if _merge==2
drop _merge
// browse pidp year xw_*

gen survey=.
replace survey=1 if inrange(wavename,1,14)
replace survey=2 if inrange(wavename,15,32)
label define survey 1 "UKHLS" 2 "BHPS"
label values survey survey

tab wavename survey, m
tab survey xw_xwdat_dv, m

tab xw_memorig, m

// going to see if I can fill in missing sex variables
tab sex, m
recode sex (-9/0=.)
tab sex xw_sex, m

browse pidp year sex hgsex xw_sex
replace xw_sex = sex if xw_sex==. & sex!=.
replace xw_sex = hgsex if xw_sex==. & hgsex!=.
replace xw_sex = sex_dv if xw_sex==. & sex_dv!=.

tab xw_sex, m
recode xw_sex (-9/0=.)

tab age if survey==2, m
tab age_dv if survey==1, m

tab birthy, m
tab doby if survey==2, m
tab doby_dv if survey==1, m
browse pidp survey birthy doby age doby_dv age_dv

gen age_all=.
replace age_all = age if survey==2
replace age_all = age_dv if survey==1
replace age_all = . if age_all==-9
tab age_all, m

gen dob_year=.
replace dob_year = doby if survey==2
replace dob_year = doby_dv if survey==1
replace dob_year = . if inrange(dob_year,-9,-1)
replace dob_year = istrtdaty - age_all if dob_year==.
tab dob_year, m

replace age_all = istrtdaty - dob_year if age_all == .

// are these just respondents in sample? is there a sample status?
bysort pidp: egen first_year_observed = min(istrtdaty)
bysort pidp: egen last_year_observed = max(istrtdaty)
sort pidp year

browse pidp year istrtdaty wavename ivfio sex age_all dob_year first_year_observed last_year_observed

// also need to figure out how to get college degree equivalent (see Musick et al 2020 - use the ISCED guidelines to identify bachelor's degree equivalents as the completion of tertiary education programs, excluding higher vocational programs

fre hiqual_dv // think need to use the component variable of this
tab hiqual_dv survey, m // both
replace hiqual_dv=. if inlist(hiqual_dv,-8,-9)
tab qfhigh_dv survey, m // this is only ukhls
// other educations: qfachi (bhps only) qfedhi (bhps only - details) qfhigh (not v good) qfhigh_dv (ukhls details) hiqual_dv hiqualb_dv (bhps only) isced11_dv (only UKHS, but lots of missing, which is sad bc I think it's what I nee/d?) isced (bhps, might be helpful?)

tab qfhigh_dv hiqual_dv
tab isced11_dv hiqual_dv // see what bachelors in isced is considered in hiqual // okay so bachelor's / master's in degree. some of bachelor's also in "other higher degree"
tab isced hiqual_dv // so here 5a and 6 are in degree. 5b is what is in other degree (vocational) - do I want that? I feel like Musick didn't include vocational.

gen college_degree=0
replace college_degree=1 if  hiqual_dv==1
replace college_degree=. if hiqual_dv==.

/*
Undergraduate degrees are either level 4, 5 or 6 qualifications, with postgraduate degrees sitting at level 7 or 8. In Scotland, awards are at level 9 or 10 for an undergraduate degree, and level 11 and 12 for master's and doctorates.
A bachelor's degree involves studying one, or sometimes two, subjects in detail. It's the most common undergraduate degree in the UK and is a level 6 qualification (level 9 or 10 in Scotland). 
*/

tab racel survey, m
tab racel_dv survey, m // ukhls - but one the survey says to use I think
tab race survey, m // not helpful
tab racel_bh survey, m // this doesn't feel helpful either
tab ethn_dv survey, m // okay, need to figure out a bhps race variable...
// might need to combine these two: race (BH01-12) racel_bh (BH13-18) or get racel_dv_all from xwavedat

browse survey year racel_dv race racel_bh
gen race_use = .
replace race_use = 1 if inrange(racel_dv,1,4) & survey==1
replace race_use = 1 if race==1 & survey==2 & inrange(year,1991,2002)
replace race_use = 1 if inrange(racel_bh,1,5) & survey==2 & inrange(year,2003,2008)
replace race_use = 2 if inrange(racel_dv,14,16) & survey==1
replace race_use = 2 if inrange(race,2,4) & survey==2 & inrange(year,1991,2002)
replace race_use = 2 if inrange(racel_bh,14,16) & survey==2 & inrange(year,2003,2008)
replace race_use = 3 if inrange(racel_dv,9,13) & survey==1
replace race_use = 3 if inrange(race,5,8) & survey==2 & inrange(year,1991,2002)
replace race_use = 3 if inrange(racel_bh,10,13) & survey==2 & inrange(year,2003,2008)
replace race_use = 4 if inrange(racel_dv,5,8) & survey==1
replace race_use = 4 if inrange(racel_bh,6,9) & survey==2 & inrange(year,2003,2008)
replace race_use = 5 if inrange(racel_dv,17,97) & survey==1
replace race_use = 5 if race==9 & survey==2 & inrange(year,1991,2002)
replace race_use = 5 if racel_bh==18 & survey==2 & inrange(year,2003,2008)
replace race_use = -8 if race==-8 & survey==2 & inrange(year,1991,2002)
replace race_use = -8 if racel_bh==-8 & survey==2 & inrange(year,2003,2008)

label define race_use 1 "White" 2 "Black" 3 "Asian" 4 "Mixed" 5 "Other" -8 "Get"
label values race_use race_use

browse pidp survey year race_use xw_racel_dv xw_ethn_dv

tab xw_racel_dv survey, m
tab xw_ethn_dv survey, m
tab xw_racel_dv race_use, m
tab xw_ethn_dv race_use, m
tab race_use if xw_ethn_dv==., m // okay, let's just use the ethnicity variable
tab race_use if xw_racel_dv==., m

// country
recode gor_dv (1/9=1)(10=2)(11=3)(12=4), gen(country_all)
replace country_all=. if inlist(country_all,-9,13)
label define country 1 "England" 2 "Wales" 3 "Scotland" 4 "N. Ireland"
label values country_all country

replace gor_dv = . if gor_dv==-9
label define a_gor_dv 13 "channel islands", add

// so many marital statuses STILL
tab mlstat survey, m // present legal marital status - has a lot of inapplicable, so doesn't seem right - think only for new people?
tab marstat survey, m  // legal marital status -- only for ukhls, doesn't seem to include cohabiting
tab marstat_dv survey, m // harmonized de facto - includes cohabiting, but only ukhls
tab mastat_dv survey, m // de facto - confused, because also only ukhls, so think above is better / cleaner?
tab currmstat survey, m // current legal marital status - many missings, think this wasn't added until later
tab mlstat_bh survey, m // present legal marital status - so this is bhps legal
tab mastat survey, m // marital status - okay this has cohabiting for bhps

fre mlstat_bh // 1 married, 2 sep, 3 divorced, 4 widowed, 5 never married
fre marstat // 1 never married, 2 married, 3 civil partnership, 4 separated, 5 divorced, 6 widowed, 7 sep civil, 8 divorced civil, 9 widow civil
gen marital_status_legal=. // use mlstat_bh for bhps; marstat for ukhls, but need to recode because not currently on same scale
* populate for bhps
replace marital_status_legal = 1 if survey==2 & mlstat_bh==1
replace marital_status_legal = 2 if survey==2 & mlstat_bh==2
replace marital_status_legal = 3 if survey==2 & mlstat_bh==3
replace marital_status_legal = 4 if survey==2 & mlstat_bh==4
replace marital_status_legal = 5 if survey==2 & mlstat_bh==5
* populate for ukhls
replace marital_status_legal = 1 if survey==1 & inlist(marstat,2,3) // married, including civil partner
replace marital_status_legal = 2 if survey==1 & inlist(marstat,4,7) // sep, including civil
replace marital_status_legal = 3 if survey==1 & inlist(marstat,5,8) // divorced, including civil
replace marital_status_legal = 4 if survey==1 & inlist(marstat,6,9) // widowed, including civil
replace marital_status_legal = 5 if survey==1 & marstat==1 // never married including civil
tab marital_status_legal, m // most of these missing are proxy interviews from ukhls
tab age_all marital_status_legal, m
tab mlstat_bh marital_status_legal, m
tab marstat marital_status_legal, m

label define marital_status_legal 1 "Married" 2 "Separated" 3 "Divorced" 4 "Widowed" 5 "Never Married"
label values marital_status_legal marital_status_legal

fre mastat // 1 married, 2 cohab, 3 widowed, 4 divorced, 5 separated, 6 never married
fre marstat_dv // 1 married, 2 cohab, 3 widowed, 4 divorced, 5 separated, 6 never married
gen marital_status_defacto=. // use mastat for bhps; marstat_dv for ukhls, but need to recode because not currently on same scale
* populate for bhps
replace marital_status_defacto = 1 if survey==2 & mastat==1
replace marital_status_defacto = 2 if survey==2 & mastat==2
replace marital_status_defacto = 3 if survey==2 & mastat==5
replace marital_status_defacto = 4 if survey==2 & mastat==4
replace marital_status_defacto = 5 if survey==2 & mastat==3
replace marital_status_defacto = 6 if survey==2 & mastat==6
* populate for ukhls
replace marital_status_defacto = 1 if survey==1 & marstat_dv==1
replace marital_status_defacto = 2 if survey==1 & marstat_dv==2
replace marital_status_defacto = 3 if survey==1 & marstat_dv==5
replace marital_status_defacto = 4 if survey==1 & marstat_dv==4
replace marital_status_defacto = 5 if survey==1 & marstat_dv==3
replace marital_status_defacto = 6 if survey==1 & marstat_dv==6

tab marital_status_defacto, m
tab age_all marital_status_defacto, m
tab mastat marital_status_defacto, m
tab marstat_dv marital_status_defacto, m

label define marital_status_defacto 1 "Married" 2 "Cohabiting" 3 "Separated" 4 "Divorced" 5 "Widowed" 6 "Never Married"
label values marital_status_defacto marital_status_defacto

gen partnered=.
replace partnered=0 if inrange(marital_status_defacto,3,6)
replace partnered=1 if inlist(marital_status_defacto,1,2)

inspect ppid
inspect ppid if partnered==1 // this is only ukhls
inspect ppid if partnered==1 & survey==1
inspect sppid if partnered==1 // okay this is also only ukhls and is just spouses
inspect sppid_bh if partnered==1 // okay this is bhps but includes spouses and partners
inspect sppid_bh if partnered==1 & survey==2 

merge m:1 sppid_bh using "$temp/spid_lookup.dta"
drop if _merge==2
browse survey pidp pid sppid_bh partner_pidp_bh _merge
inspect sppid_bh if partnered==1 & survey==2 
inspect partner_pidp_bh if partnered==1 & survey==2 
drop _merge

browse pidp wavename survey marital_status_defacto partnered ppid sppid sppid_bh

gen long partner_id = .
replace partner_id = ppid if survey==1
// replace partner_id = sppid_bh if survey==2
replace partner_id = partner_pidp_bh if survey==2 // okay will try to use pidp instead? I don't know if this is smart?
replace partner_id = . if partner_id <=0 // inapplicable / spouse not in HH

inspect partner_id
inspect partner_id if partnered==1

forvalues y=1991/2022{
	display `y'
	inspect partner_id if partnered==1 & year==`y'
}

sort pidp year
browse pidp wavename year survey marital_status_defacto partnered partner_id ppid partner_pidp_bh sppid_bh if partnered==1

********************************************************************************
** DoL variables
********************************************************************************
/*
attempting to QA howlng here - coverage feels low in next file, want to see if that is actually true. do before all recodes.
tab howlng if wavename==6, m
tab howlng if wavename==6 & howlng>=0, m
browse survey istrtdaty istrtdatm  howlng if wavename==6
*/

foreach var in howlng husits hubuys hufrys huiron humops huboss jbstat aidhh aidxhh jshrs j2hrs{
	recode `var' (-10/-1=.)
}

// some better employment variables
fre jbstat
gen employed=.
replace employed=0 if inrange(jbstat,3,97)
replace employed=1 if inlist(jbstat,1,2)

* also do want to retain / impute a detailed job status indicator (to get a sense of retirement / disability from that)
label define a_jbstat 14 "On shared parental leave" 15 "On adoption leave", add 

gen employment_status = jbstat
replace employment_status = 5 if inlist(jbstat,14,15)
replace employment_status = 14 if jbstat==97
label define emp_status 1 "self employed" 2 "Paid employment(ft/pt)" 3 "unemployed" 4 "retired" 5 "on parental leave" 6 "Family care or home" ///
7 "full-time student" 8 "LT sick or disabled" 9 "Govt training scheme" 10 "Unpaid, family business" 11 "On apprenticeship" 12 "On furlough" ///
13 "Temporarily laid off/short term working" 14 "other"
label values employment_status emp_status

// need to ensure self-employed hours accounted for
tab jbstat jbhrs  if jbhrs <= 0
tab jbhrs if jbstat==1, m
tab jbhrs if jbstat==2, m
tab j2hrs if jbstat==2 & jbhrs < 0, m
tab j2hrs if jbstat==2 & jbhrs > 0, m

recode jbhrs (-9=.)(-7/-1=.)
replace jbhrs = 0 if jbhrs == -8 & inrange(jbstat,3,97)
replace jbhrs = . if jbhrs == -8 & inlist(jbstat,1,2)
replace jbhrs = . if jbhrs == -8 & jbstat==.

fre jbot
recode jbot (-9=.)(-7/-1=.)
replace jbot = 0 if jbot == -8 & inrange(jbstat,3,97)
replace jbot = . if jbot == -8 & inlist(jbstat,1,2)
replace jbot = . if jbot == -8 & jbstat==.

tabstat jbhrs jbot jshrs j2hrs, by(jbstat)
inspect jshrs if jbstat==1
inspect jbhrs if jbstat==1
browse pidp year jbstat jbhrs jbot jshrs j2hrs
// browse pidp year jbstat jbhrs jbot jshrs j2hrs if jbstat==1

gen work_hours = .
replace work_hours = jbhrs if jbstat!=1 // update with this variable for all EXCEPT self employed
replace work_hours = jbhrs if jbstat==1 & jbhrs!=. // for self employed - update with jb hours if non-missing (they don't seem to overlap)
replace work_hours = jshrs if jbstat==1 & work_hours==. // then, update with self-employment hours measure
replace work_hours = jshrs if work_hours==. & jshrs!=.

egen total_hours=rowtotal(work_hours jbot), missing

tabstat jbhrs jbot work_hours total_hours jshrs j2hrs, by(jbstat)
gen has_hours=.
replace has_hours = 0 if total_hours==0
replace has_hours = 1 if total_hours > 0 & total_hours< 200

tab jbstat has_hours, m row
tab jbhrs if has_hours==., m
tab jshrs if has_hours==., m 
tab j2hrs if has_hours==., m

browse pidp year jbstat work_hours total_hours jbhrs jbot jshrs j2hrs

sum howlng, detail
sum jbhrs, detail
sum jbhrs if employed==1, detail
tab husits, m
fre husits

// earnings 
recode fimnlabgrs_dv (-9=.)(-7=.) // Total personal monthly labour income gross
recode fimnlabnet_dv (-9=.)(-1=.) // net is only asked in UKHLS
recode paynu_dv (-9=.)(-7=.)(-8=0) // usual net pay per month: current job

tabstat fimnlabgrs_dv fimnlabnet_dv paynu_dv, by(year)

// HH incomes
tabstat fihhmngrs_dv fihhmnlabnet_dv fihhmnlabgrs_dv fihhml fihhyl grpay hhneti hhyneti hhyrlg hhyrln netlab, by(year)
// fihhmngrs_dv is only one asked in all waves, though I could probably combine a few? But this is gross household income: month before interview
recode fihhmngrs_dv (-9=.)

sort hidp year
browse hidp pidp year fimnlabgrs_dv paynu_dv fihhmngrs_dv
sort pidp year

// other employment-adjacent variables
* Disability
	// bhps
	tab hlltw survey, m // just bhps (Y/N: health limits type or amount of work - most comparable to psid I think)
	recode hlltw (-9/-1=.)
	tab hlltwa survey, m // just bhps (how much health limits)
	tab hlltwa hlltw, m

	gen disabled_scale=.
	replace disabled_scale=0 if hlltw==2 // not disabled
	replace disabled_scale=0 if hlltw==1 & hlltwa==4 // not at all - so putting as 0
	replace disabled_scale=1 if hlltw==1 & hlltwa==3 // just a little
	replace disabled_scale=2 if hlltw==1 & hlltwa==2 // somewhat
	replace disabled_scale=3 if hlltw==1 & hlltwa==1 // a lot

	label define dis_scale 0 "Not at all" 1 "A little" 2 "Somewhat" 3 "A lot"
	label values disabled_scale dis_scale

	tab hlltwa disabled_scale, m
	tab disabled_scale hlltw, m

	// ukhls
	tab health survey, m // just ukhls and very few missing (Y/N: longstanding illness or disability)
	recode health (-9/-1=.)

	recode scsf3a (-10/-1=.)
	recode scsf3b (-10/-1=.)
	tab scsf3a health, m col // not super well correlated...
	tab scsf3b health, m col // not super well correlated...
	tab scsf3a scsf3b, m // sort of correlated
	
tab jbstat hlltw, m // so many people who are disabled are employed by the majority of those with jbstat of disabled have a yes
tab jbstat health, m // same here
tab jbstat disabled_scale, m
tab jbstat scsf3a, m

browse pidp year survey jbstat hlltw disabled_scale health scsf3a scsf3b
tab hlltw health, m // these never overlap

gen disabled_est = .
replace disabled_est = 0 if hlltw==2 | health==2
replace disabled_est = 1 if hlltw==1 | health==1
tab disabled_est, m

gen empstat_disabled = .
replace empstat_disabled = 0 if inrange(employment_status,1,7)
replace empstat_disabled = 0 if inrange(employment_status,9,14)
replace empstat_disabled = 1 if employment_status==8

tab empstat_disabled, m
tab empstat_disabled disabled_est, m

* SR health
browse pidp year survey hlstat sf1 scsf1 
tab hlstat survey, m // just bhps
recode sf1 (-9/-1=.)
tab sf1 survey, m // this is in both?
recode scsf1 (-9/-1=.)
tab scsf1 survey, m // just ukhls
tab sf1 scsf1, m

// For Waves 6 onwards, users will need to combine responses from sf1 and scsf1. I believe this is 2014
// ugh why are the scales diff?
// hlstat: Excellent, Good, Fair, Poor, Very Poor
// the others: Excellent, Very Good, Good, Fair, Poor

gen sr_health = .
replace sr_health = 1 if survey==2 & hlstat==1 // excellent
replace sr_health = 3 if survey==2 & hlstat==2 // good
replace sr_health = 4 if survey==2 & hlstat==3 // fair
replace sr_health = 5 if survey==2 & hlstat==4 // poor
replace sr_health = 6 if survey==2 & hlstat==5 // very poor
replace sr_health = 1 if sr_health==. & survey==2 & sf1==1 // excellent
replace sr_health = 2 if sr_health==. & survey==2 & sf1==2 // very good
replace sr_health = 3 if sr_health==. & survey==2 & sf1==3 // good
replace sr_health = 4 if sr_health==. & survey==2 & sf1==4 // fair
replace sr_health = 5 if sr_health==. & survey==2 & sf1==5 // poor
replace sr_health = 1 if survey==1 & (sf1==scsf1) & sf1==1 // excellent
replace sr_health = 2 if survey==1 & (sf1==scsf1) & sf1==2 // very good
replace sr_health = 3 if survey==1 & (sf1==scsf1) & sf1==3 // good
replace sr_health = 4 if survey==1 & (sf1==scsf1) & sf1==4 // fair
replace sr_health = 5 if survey==1 & (sf1==scsf1) & sf1==5 // poor
replace sr_health = 1 if survey==1 & ((scsf1==. & sf1==1) | (sf1==. & scsf1==1)) // excellent
replace sr_health = 2 if survey==1 & ((scsf1==. & sf1==2) | (sf1==. & scsf1==2)) // very good
replace sr_health = 3 if survey==1 & ((scsf1==. & sf1==3) | (sf1==. & scsf1==3)) // good
replace sr_health = 4 if survey==1 & ((scsf1==. & sf1==4) | (sf1==. & scsf1==4)) // fair
replace sr_health = 5 if survey==1 & ((scsf1==. & sf1==5) | (sf1==. & scsf1==5)) // poor
// let's prioritize scsf as that is self-reported not to interviewer, so maybe more accurate?
replace sr_health = 1 if survey==1 & sr_health==. & scsf1==1 // excellent
replace sr_health = 2 if survey==1 & sr_health==. & scsf1==2 // very good
replace sr_health = 3 if survey==1 & sr_health==. & scsf1==3 // good
replace sr_health = 4 if survey==1 & sr_health==. & scsf1==4 // fair
replace sr_health = 5 if survey==1 & sr_health==. & scsf1==5 // poor

label define health 1 "Excellent" 2 "Very Good" 3 "Good" 4 "Fair" 5 "Poor" 6 "Very Poor"
label values sr_health health

tab sr_health, m
tab sr_health survey, m
foreach var in hlstat sf1 scsf1{
	display "`var'"
	tab `var' if sr_health==., m
	tab sr_health `var', m
}

* Retirement Status
gen empstat_retired = .
replace empstat_retired = 0 if inrange(employment_status,1,3)
replace empstat_retired = 0 if inrange(employment_status,5,14)
replace empstat_retired = 1 if employment_status==4

tab empstat_retired, m

browse pidp year survey jbstat agexrt retdatem retdatey retchk
recode retchk (-10/-1=.) // this is missing for almost 99%
tab retchk empstat_retired, m // no correspondence
tab retdatem retchk, m // no correspondence here
tab retdatey retchk, m // no correspondence here
tab agexrt retchk, m // no correspondence here

tab retdatem empstat_retired, m // okay so some of the yes for retired have a month and most of the inapplicable are truly not retired
tab retdatey empstat_retired, m // okay so some of the yes for retired have a month and most of the inapplicable are truly not retired
tab agexrt empstat_retired, m // okay so some of the yes for retired have a month and most of the inapplicable are truly not retired

recode agexrt (-10/-9=.) (-8=0)(-7/-1=.)
recode retdatem (-10/-9=.) (-8=0)(-7/-1=.)
recode retdatey (-10/-9=.) (-8=0)(-7/-1=.)

tab agexrt, m // okay these all have like 95% missing so really not useful....
tab retdatem, m
tab retdatey, m

// housing status
tab tenure_dv, m
recode tenure_dv (-9/-1=.)

gen housing_status_alt=.
replace housing_status_alt = 1 if tenure_dv== 1 // owned outright
replace housing_status_alt = 2 if tenure_dv== 2 // owned w mortgage
replace housing_status_alt = 3 if inrange(tenure_dv,3,7) // rented
replace housing_status_alt = 4 if tenure_dv== 8 // other

label define housing_status_alt 1 "owned outright" 2 "owned w mortgage" 3 "rented" 4 "other"
label values housing_status_alt housing_status_alt

// unpaid labor variables
foreach var in hubuys hufrys huiron humops{ // coding changed starting wave 12 (and then only asked every other year)
	gen `var'_v0 = `var'
	replace `var' = 1 if inlist(wavename,12,14) & inlist(`var'_v0,1,2) // 1 = mostly self
	replace `var' = 2 if inlist(wavename,12,14) & inlist(`var'_v0,4,5) // 2 = mostly partner
	replace `var' = 3 if inlist(wavename,12,14) & inlist(`var'_v0,3) // 3 = shared
	replace `var' = 97 if inlist(wavename,12,14) & inlist(`var'_v0,6,7) // 97 = other
	replace `var'=. if `var'_v0==8
	replace `var'=. if `var'==5
	tab `var', m
	
}

/*
tab huiron, m
tab huiron if wavename==12 // okay so I think coding changed on all of these
gen huiron_v0 = huiron
replace huiron = 1 if wavename==12 & inlist(huiron_v0,1,2) // 1 = mostly self
replace huiron = 2 if wavename==12 & inlist(huiron_v0,4,5) // 2 = mostly partner
replace huiron = 3 if wavename==12 & inlist(huiron_v0,3) // 3 = shared
replace huiron = 97 if wavename==12 & inlist(huiron_v0,6,7) // 97 = other
replace huiron=. if huiron_v0==8
replace huiron=. if huiron==5
*/

tab huboss, m
replace huboss=97 if huboss==4 // other was 4 in bhps

gen howlng_flag=0
replace howlng_flag = 1 if inlist(wavename,1,2,4,6,8,10,12,14) | inrange(wavename,16,32) // when howlng (continuous HW) is asked
// 1,2,4,6,8,10,12,BH02,BH03,BH04,BH05,BH06,BH07,BH08,BH09,BH10,BH11,BH12,BH13,BH14,BH15,BH16,BH17,BH18

gen housework_flag=0
replace housework_flag = 1 if inlist(wavename,2,4,6,8,10,12,14,15) | inrange(wavename,18,32) // when HW categorical vars are asked
// 2,4,6,8,10,12,BH01,BH04,BH05,BH06,BH07,BH08,BH09,BH10,BH11,BH12,BH13,BH14,BH15,BH16,BH17,BH18

tab aidhh survey, m
replace aidhh = . if aidhh==3
tab aidxhh survey, m
tab aidhrs survey, m
recode aidhrs (-8=0)(-10/-9=.)(-7/-1=.)

gen any_aid=.
replace any_aid = 0 if aidhh==2 & aidxhh==2
replace any_aid = 1 if aidhh==1 | aidxhh==1
tab any_aid, m

tab aidhrs any_aid, m

gen aid_hours = aidhrs
replace aid_hours = 3 if aidhrs ==8 // putting under 20 into 10-19 (because right now, this scale doens't make sense)
replace aid_hours = 4 if aidhrs ==9 // putting over 20 into 20-34 (because right now, this scale doens't make sense)
replace aid_hours = . if aidhrs ==97
label values aid_hours a_aidhrs 

// child variables
foreach var in nch02_dv nch34_dv nch511_dv nch1215_dv agechy_dv nkids_dv{
	replace `var'=. if `var'==-9
}

egen nchild_015 = rowtotal(nch02_dv nch34_dv nch511_dv nch1215_dv), missing
browse nchild_dv nkids_dv nchild_015 nch02_dv nch34_dv nch511_dv nch1215_dv

tab agechy_dv nchild_dv, m
tab agechy_dv nchild_015, m
tab agechy_dv nkids_dv, m

tab xw_anychild_dv, m
tab xw_ch1by_dv xw_anychild_dv, m
tab agechy_dv xw_anychild_dv, m

gen ever_parent=.
replace ever_parent = 0 if xw_anychild_dv==2
replace ever_parent = 1 if xw_anychild_dv==1

gen year_first_birth = xw_ch1by_dv
replace year_first_birth = 9999 if xw_anychild_dv==2

gen age_youngest_child = agechy_dv
replace age_youngest_child = 9999 if agechy_dv==-8
tab age_youngest_child, m

	// why is it so difficult to get a total number of births?
	browse pidp year survey ever_parent lnprnt // this is mostly filled in 2009 and 1992. is this updated ? max #?

	// merge on that indicator of mpf I created because I also grab number of births from there as well. still not convinced this is comprehensive
	merge m:1 pidp using "$temp/mpf_lookup.dta" // okay, not that I have added those without births, it's much better
	drop if _merge==2
	drop _merge

	tab any_mpf, m
	tab ever_parent any_mpf, m
	replace any_mpf = 0 if xw_anychild_dv==2 // can't have mpf if no kids...
	// oh, okay, I think this is because I used the HH roster which is based on ever co-residence with children. It is NOT fertility history
	// so, if never in HH with child, I actually think they will be missing here. is there a fertility history (that is separate the same way marriage is?)
	
	// are these congruent
	tab num_bio_kids ever_parent, m // no, as I suspected (from previous attempt at merging, there are a bunch of zeroes for ever parent==1)
	tab lnprnt if ever_parent==1 & num_bio_kids==0
	browse pidp year survey ever_parent num_bio_kids lnprnt // these feel not very congruent. I guess because lprnt is incrementing?
	// let's return to this later, not actually sure if I need at the moment...

tab husits if partnered==1, m
tab husits if nchild_dv!=0, m
tab wavename husits if nchild_dv!=0, m // still a lot of missing..oh, well, i guess they could have child AND not be partnered DUH
tab wavename husits if nchild_dv!=0 & partnered==1, m // still a lot of missing..oh, well, i guess they could have child AND not be partnered. okay still a lot of missing, might be proxy surveys
tab hubuys housework_flag if partnered==1, m col
tab hufrys housework_flag if partnered==1, m col
tab huiron housework_flag if partnered==1, m col
tab humops housework_flag if partnered==1, m col
tab huboss housework_flag if partnered==1, m col

browse pidp hidp wavename age_all partnered marital_status_defacto husits howlng hubuys hufrys huiron humops jbhrs

// other hh comp variables
* Number of people over pensionable age (60 for women, 65 for men)
fre npens_dv
replace npens_dv = . if npens_dv == -9

* Going to see if I can attempt coresidence with parents
tab npn_dv survey, m // this is # but only ukhls (not bhps)
inspect mnspno fnspno hgbiof hgbiom if survey==1
inspect mnspno fnspno hgbiof hgbiom if survey==2
browse pidp survey year npn_dv hgbiom mnspno hgbiof fnspno

gen mom_in_hh = 0
replace mom_in_hh = 1 if hgbiom > 0 & hgbiom!=.

gen dad_in_hh = 0
replace dad_in_hh = 1 if hgbiof > 0 & hgbiof!=.

egen num_parents_hh = rowtotal(mom_in_hh dad_in_hh)
tab num_parents_hh npn_dv // generally matches for the UKHLS provided info

// attempt to figure out religion
foreach var in oprlg1 oprlg0 ff_oprlg0 oprlg0ni ff_oprlg0ni{
	display "`var'" 
	tab `var' survey, m
	tab `var' country_all, m
}

/*
oprlg1 // only one in bhps and across countries but still terrible coverage
oprlg0 // not in bhps , filled in only for e/s/w
ff_oprlg0 // not in bhps, filled in only for e/s/w
oprlg0ni // not in bhps and many missing, only filled in for those in NI, but still many missing
ff_oprlg0ni // not in bhps and many missing, only filled in for those in NI, but still many missing
*/

gen religion = oprlg1
replace religion = . if inrange(oprlg1,-10,-1) 
label values religion a_oprlg1

gen religion_esw = oprlg0
replace religion_esw = . if inrange(oprlg0,-10,-1) 
label values religion_esw a_oprlg0

gen ff_religion_esw = ff_oprlg0
replace ff_religion_esw = . if inrange(ff_oprlg0,-10,-1) 
label values ff_religion_esw b_ff_oprlg0

gen religion_ni = oprlg0ni
replace religion_ni = . if inrange(oprlg0ni,-10,-1) 
label values religion_ni a_oprlg0ni

gen ff_religion_ni = ff_oprlg0ni
replace ff_religion_ni = . if inrange(ff_oprlg0ni,-10,-1) 
label values ff_religion_ni b_ff_oprlg0ni

egen religion_values = rownonmiss(religion religion_esw ff_religion_esw religion_ni ff_religion_ni)
tab religion_values, m

browse pidp survey year country_all religion_values religion religion_esw ff_religion_esw religion_ni ff_religion_ni
browse pidp survey year country_all religion_values religion religion_esw ff_religion_esw religion_ni ff_religion_ni if religion_values==2

tab religion ff_religion_esw if religion_values==2, m

label define master_religion 1 "no religion" 2 "church of england/anglican" 3 "roman catholic" 4 "church of scotland" 5 "free church or free presbyterian church of scotland" ///
6 "episcopalian" 7 "methodist" 8 "baptist" 9 "congregational/united reform/urc" 10 "other christian" 11 "christian (no spec denom)" 12 "muslim/islam" 13 "hindu" ///
14 "jewish" 15 "sikh" 16 "buddhist" 17 "presbyterian" 18 "church of ireland" 19 "brethren" 20 "protestant (unspecified)" 21 "other"

gen master_religion=.
replace master_religion = 1 if religion_values==1 & (religion_esw==1 | ff_religion_esw==1 | religion_ni==96 | ff_religion_ni==96) // no
replace master_religion = 2 if religion_values==1 & (religion==2 | religion_esw==2 | ff_religion_esw==2) // church of england
replace master_religion = 3 if religion_values==1 & (religion==3 | religion_esw==3 | ff_religion_esw==3 | religion_ni==1 | ff_religion_ni==1) // catholic
replace master_religion = 4 if religion_values==1 & (religion==4 | religion_esw==4 | ff_religion_esw==4) // church of scotland
replace master_religion = 5 if religion_values==1 & (religion==5 | religion==22 | religion_esw==5 | ff_religion_esw==5 | religion_ni==6 | ff_religion_ni==6) // free church
replace master_religion = 6 if religion_values==1 & (religion==6 | religion_esw==6 | ff_religion_esw==6) // episcopalian
replace master_religion = 7 if religion_values==1 & (religion==7 | religion_esw==7 | ff_religion_esw==7 | religion_ni==4 | ff_religion_ni==4) // methodist
replace master_religion = 8 if religion_values==1 & (religion==8 | religion_esw==8 | ff_religion_esw==8 | religion_ni==5 | ff_religion_ni==5) // baptist
replace master_religion = 9 if religion_values==1 & (religion==9 | religion_esw==9 | ff_religion_esw==9) // congregational
replace master_religion = 10 if religion_values==1 & (religion==10 | religion==25 | religion_esw==10 | ff_religion_esw==10 | religion_ni==9 | ff_religion_ni==9) // other christian
replace master_religion = 11 if religion_values==1 & (religion==11 | religion_esw==11 | ff_religion_esw==11) // christian unspec
replace master_religion = 12 if religion_values==1 & (religion==12 | religion_esw==12 | ff_religion_esw==12 | religion_ni==13 | ff_religion_ni==13) // muslim
replace master_religion = 13 if religion_values==1 & (religion==13 | religion_esw==13 | ff_religion_esw==13) // hindu
replace master_religion = 14 if religion_values==1 & (religion==14 | religion_esw==14 | ff_religion_esw==14 | religion_ni==12 | religion==26) // jewish
replace master_religion = 15 if religion_values==1 & (religion==15 | religion_esw==15 | ff_religion_esw==15) // sikh
replace master_religion = 16 if religion_values==1 & (religion==16 | religion_esw==16 | ff_religion_esw==16 | religion_ni==10 | ff_religion_ni==10) // buddhist
replace master_religion = 17 if religion_values==1 & (religion_ni==2 | ff_religion_ni==2) // presby
replace master_religion = 18 if religion_values==1 & (religion_ni==3 | ff_religion_ni==3) // church of ireland
replace master_religion = 19 if religion_values==1 & (religion==23 | religion_ni==7 | ff_religion_ni==7) // brethren
replace master_religion = 20 if religion_values==1 & (religion==24 | religion_ni==8 | ff_religion_ni==8) // protestant
replace master_religion = 21 if religion_values==1 & (inlist(religion,17,27,97) | religion_esw==97 | ff_religion_esw==97 | religion_ni==97 | ff_religion_ni==97) // other

replace master_religion = 1 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==1 // no
replace master_religion = 2 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==2 // church of england
replace master_religion = 3 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==3 // catholic
replace master_religion = 4 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==4 // church of scotland
replace master_religion = 5 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==5 // free church
replace master_religion = 6 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==6 // episc
replace master_religion = 7 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==7 // methodist
replace master_religion = 8 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==8 // baptist
replace master_religion = 9 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==9 // congregational
replace master_religion = 10 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==10 // other christian
replace master_religion = 11 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==11 // christian unspec
replace master_religion = 12 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==12 // muslim
replace master_religion = 13 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==13 // hindu
replace master_religion = 14 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==14 // jewish
replace master_religion = 15 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==15 // sikh
replace master_religion = 16 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==16 // buddhist
replace master_religion = 21 if religion_values==2 & religion==ff_religion_esw & ff_religion_esw==97 // other
// based on the below investigation, seems like I should prioritize the ff_esw
replace master_religion = 1 if religion_values==2 & master_religion==. & ff_religion_esw==1 // no
replace master_religion = 2 if religion_values==2 & master_religion==. & ff_religion_esw==2 // church of england
replace master_religion = 3 if religion_values==2 & master_religion==. & ff_religion_esw==3 // catholic
replace master_religion = 4 if religion_values==2 & master_religion==. & ff_religion_esw==4 // church of scotland
replace master_religion = 5 if religion_values==2 & master_religion==. & ff_religion_esw==5 // free church
replace master_religion = 6 if religion_values==2 & master_religion==. & ff_religion_esw==6 // episc
replace master_religion = 7 if religion_values==2 & master_religion==. & ff_religion_esw==7 // methodist
replace master_religion = 8 if religion_values==2 & master_religion==. & ff_religion_esw==8 // baptist
replace master_religion = 9 if religion_values==2 & master_religion==. & ff_religion_esw==9 // congregational
replace master_religion = 10 if religion_values==2 & master_religion==. & ff_religion_esw==10 // other christian
replace master_religion = 11 if religion_values==2 & master_religion==. & ff_religion_esw==11 // christian unspec
replace master_religion = 12 if religion_values==2 & master_religion==. & ff_religion_esw==12 // muslim
replace master_religion = 13 if religion_values==2 & master_religion==. & ff_religion_esw==13 // hindu
replace master_religion = 14 if religion_values==2 & master_religion==. & ff_religion_esw==14 // jewish
replace master_religion = 15 if religion_values==2 & master_religion==. & ff_religion_esw==15 // sikh
replace master_religion = 16 if religion_values==2 & master_religion==. & ff_religion_esw==16 // buddhist
replace master_religion = 21 if religion_values==2 & master_religion==. & ff_religion_esw==97 // other
//
replace master_religion = 1 if religion_values==2 & master_religion==. & ff_religion_ni==96 // no
replace master_religion = 3 if religion_values==2 & master_religion==. & ff_religion_ni==1 // catholic
replace master_religion = 10 if religion_values==2 & master_religion==. & ff_religion_ni==9 // other christian
replace master_religion = 17 if religion_values==2 & master_religion==. & ff_religion_ni==2 // presbyterian
replace master_religion = 18 if religion_values==2 & master_religion==. & ff_religion_ni==3 // church of ireland
replace master_religion = 20 if religion_values==2 & master_religion==. & ff_religion_ni==8 // protestant (unspecified)
replace master_religion = 21 if religion_values==2 & master_religion==. & ff_religion_ni==97 // other

label values master_religion master_religion
tab master_religion religion_values, m

unique master_religion if master_religion!=., by(pidp) gen(num_religion)
bysort pidp (num_religion): replace num_religion = num_religion[1]
tab num_religion, m // for those who just have 1, should I fill in with that value? or just impute all?

sort pidp year
browse pidp survey year country_all religion_values num_religion master_religion religion religion_esw ff_religion_esw religion_ni ff_religion_ni // okay after all of this, do most people only have one year with religion info?

bysort pidp: egen religion_est = max(master_religion) if num_religion==1
replace religion_est = master_religion if num_religion > 1
label values religion_est master_religion

// browse pidp survey year country_all religion_values num_religion religion_est master_religion religion religion_esw ff_religion_esw religion_ni ff_religion_ni

tab master_religion num_religion,m 
tab religion_est num_religion,m 

// browse pidp survey year country_all religion_values religion religion_esw ff_religion_esw religion_ni ff_religion_ni if religion_values==2 & master_religion==. // why are these all specifically 2012, 2016, 2020
// browse pidp survey year country_all religion_values religion religion_esw ff_religion_esw religion_ni ff_religion_ni if inlist(pidp,68105411,68284931,816877207)

// family backround variables
* parent SES
tab xw_paedqf, m
tab xw_maedqf, m
tab xw_maju, m // the parent working variables (at child age 14) have much better coverage than the education
tab xw_paju, m // the parent working variables (at child age 14) have much better coverage than the education

browse pidp survey year xw_paedqf xw_paju xw_maedqf xw_maju // okay not really much to do here, think will just need to impute those with no data (using a more consolidated number of variables)
rename xw_paedqf father_educ
rename xw_maedqf mother_educ
rename xw_paju father_empstatus
rename xw_maju mother_empstatus

* family structure
tab xw_lvag16, m
tab xw_lvag14, m
tab xw_lvag14 xw_lvag16, m // so the family structure is only filled in if not living with both bio parents
tab lvag14_bh, m // this is mostly missing
tab lvag14_bh xw_lvag14, m // and no instances where the bh one gives you more info
tab xw_agelh, m
tab xw_agelh xw_lvag16, m // also generally only filled in if not living with both

browse pidp survey year xw_lvag16 xw_lvag14 xw_agelh

gen family_structure = .
replace family_structure = 1 if xw_lvag16==1
replace family_structure = 0 if xw_lvag16==2

gen family_structure14_det = .
replace family_structure14_det = xw_lvag14 if family_structure==0
replace family_structure14_det = 1 if family_structure==1
replace family_structure14_det = 8 if family_structure14_det==97
label define lvag14 8 "other", add
label values family_structure14_det lvag14

tab family_structure14_det, m
tab family_structure14_det family_structure, m

// respondent info
gen respondent_self = 0
replace respondent_self = 1 if inlist(ivfio,1,3)

tab ivfio respondent_self, m

***************************
***************************
// let's do a check of the variables I either will use for analysis or will use to impute, so I can be sure I a. properly impute and b. properly recoded
foreach var in jbhrs work_hours total_hours howlng aidhrs fimnlabgrs_dv jbstat employment_status employed hiqual_dv ever_parent nchild_dv nkids_dv agechy_dv partnered marital_status_defacto fihhmngrs_dv xw_ethn_dv country_all gor_dv dob_year year_first_birth xw_memorig xw_sampst respondent_self xw_sex tenure_dv housing_status_alt master_religion religion_est empstat_disabled disabled_est sr_health empstat_retired any_aid aid_hours npens_dv num_parents_hh any_mpf father_educ father_empstatus mother_educ mother_empstatus family_structure family_structure14_det{
	inspect `var'
}

// okay, doing this, but actually care more in next file once I have my actual sample

********************************************************************************
* Okay, let's add on marital history as well, so I can use this to get duration / relationship order
********************************************************************************
// main partner history
merge m:1 pidp using "$temp/partner_history_tomatch.dta", keepusing(mh_*)
tab marital_status_defacto _merge, row // so def some missing that shouldn't be... but not a lot (like coverage is 97-98% for those cohab / married, a little less for those dissolved)
drop if _merge==2

gen in_partner_history=0
replace in_partner_history=1 if _merge==3

drop _merge

// backup marital history
merge m:1 pidp using "$temp/marriage_combined_wide.dta", keepusing(mh2_*)
drop if _merge==2
drop _merge

	// browse pidp year mh_starty1 mh_endy1 mh_starty2 mh_endy2 mh2_year_marr1 mh2_year_end1 mh2_how_end1 mh2_year_marr4 mh2_year_end4 mh2_how_end4 mh2_year_marr2 mh2_year_marr3 // so sometimes mh2 year 4 is actually year 1 lolz (maybe most to least recent?)

// create necessary variables
sort pidp year
browse pidp year sex marital_status_defacto year partner_id mh_partner1 mh_status1 mh_starty1 mh_endy1 mh_partner2 mh_status2 mh_starty2 mh_endy2 mh_partner3 mh_status3  mh_starty3 mh_endy3 mh_partner4 mh_status4

label define status_new 1 "Marriage" 2 "Cohab"
foreach var in mh_status1 mh_status2 mh_status3 mh_status4 mh_status5 mh_status6 mh_status7 mh_status8 mh_status9 mh_status10 mh_status11 mh_status12 mh_status13 mh_status14{
	gen x_`var' = `var'
	replace `var' = 1 if inlist(`var',2,3)
	replace `var' = 2 if `var'==10
	label values `var' .
	label values `var' status_new
}

gen rel_no=. // add this to get lookup for current relationship start and end dates
forvalues r=1/14{
	replace rel_no = `r' if mh_status`r' == marital_status_defacto & mh_partner`r' == partner_id & partner_id!=.
}

// should this also be based on survey yr being between relationship start and end dates? Because of the people who are missing partner ids? But I guess they are not useful if missing partner ids? but at least to see what I can fill in because some have partner ids in main fiile but not in the marital history
gen rel_no_alt=.
forvalues r=1/14{
	replace rel_no_alt = `r' if istrtdaty >= mh_starty`r' & istrtdaty <= mh_endy`r' & mh_starty`r'!=. & mh_endy`r'!=.
}

replace rel_no = rel_no_alt if rel_no==. & partner_id!=.
replace rel_no = rel_no_alt if rel_no==. & partnered==1
tab rel_no partnered, m // this one is more congruent with partnershp status
tab rel_no_alt partnered, m

tab rel_no if inlist(marital_status_defacto,1,2), m // so about 4% missing. come back to this - can I see if any started during the survey to at least get duration? okay, doing what I did for psid and using marital history and estimating based on recorded relationships
tab year rel_no if inlist(marital_status_defacto,1,2), m row // check that updated marital history file made this consistent again by year

gen rel_counter=0
forvalues r=1/14{
	replace rel_counter = rel_counter + 1 if mh_starty`r' < istrtdaty // this is meant to cover all relationship types
}

browse pidp year marital_status_defacto rel_counter rel_no mh_starty*

browse pidp sex marital_status_defacto year partner_id rel_counter rel_no mh_partner1 mh_status1 mh_starty1 mh_endy1 mh_partner2 mh_status2 mh_starty2 mh_endy2 mh_partner3 mh_status3  mh_starty3 mh_endy3 mh_partner4 mh_status4 if partnered==1 & rel_no==.

gen rel_no_est = rel_no
replace rel_no_est = rel_counter + 1 if rel_no_est==. & partnered==1 // estimate
tab rel_no partnered, m
tab rel_no_est partnered, m

sort pidp year
// browse pidp year marital_status_defacto

// manually created relationship transitions
*enter
gen rel_start=0
replace rel_start=1 if (inlist(marital_status_defacto,1,2) & inlist(marital_status_defacto[_n-1],3,4,5,6)) & pidp==pidp[_n-1] & year==year[_n-1]+1

*exit
gen rel_end=0
replace rel_end=1 if (inlist(marital_status_defacto,3,4,5,6) & inlist(marital_status_defacto[_n-1],1,2)) & pidp==pidp[_n-1] & year==year[_n-1]+1

gen rel_end_pre=0
replace rel_end_pre=1 if (inlist(marital_status_defacto,1,2) & inlist(marital_status_defacto[_n+1],3,4,5,6)) & pidp==pidp[_n+1] & year==year[_n+1]-1

*cohab to marr
gen marr_trans=0
replace marr_trans=1 if (marital_status_defacto==1 & marital_status_defacto[_n-1]==2) & pidp==pidp[_n-1] & partner_id==partner_id[_n-1] & year==year[_n-1]+1

// browse pidp sex year marital_status_defacto partner_id rel_no rel_start rel_end rel_end_pre marr_trans mh_partner1 mh_status1 mh_starty1 mh_endy1 mh_partner2 mh_status2 mh_starty2 mh_endy2 mh_partner3 mh_status3  mh_starty3 mh_endy3 mh_partner4 mh_status4

// I am going to create individual level versions of these variables for now and then check in next file against partners to make sure they match.
// using the NON estimated rel_no because that corresponds to the info given in marital history. The ones where I estimate don't have the current partnership recorded in marital history, so I don't want to overwrite the wrong info...
gen current_rel_start_year=.
gen current_rel_start_month=.
gen current_rel_end_year=.
gen current_rel_end_month=.
gen current_rel_ongoing=.
// gen current_rel_how_end=.
gen current_rel_marr_end=.
gen current_rel_coh_end=.

forvalues r=1/14{
	replace current_rel_start_year = mh_starty`r' if rel_no==`r'
	replace current_rel_start_month = mh_startm`r' if rel_no==`r'
	replace current_rel_end_year = mh_endy`r' if rel_no==`r'
	replace current_rel_end_month = mh_endm`r' if rel_no==`r'
	replace current_rel_ongoing = mh_ongoing`r' if rel_no==`r'
	// replace current_rel_how_end = mrgend`r' if rel_no==`r' & status`r'==1 // if marriage - okay this actually won't work because the codes are different between marriage and cohab
	// replace current_rel_how_end = cohend`r' if rel_no==`r' & status`r'==2 // if cohab
	replace current_rel_marr_end = mh_mrgend`r' if rel_no==`r'
	replace current_rel_coh_end = mh_cohend`r' if rel_no==`r'
}

replace current_rel_start_year=. if current_rel_start_year==-9
replace current_rel_start_month=. if current_rel_start_month==-9
replace current_rel_end_year=. if current_rel_end_year==-9
replace current_rel_end_month=. if current_rel_end_month==-9

label values current_rel_ongoing ongoing
label values current_rel_marr_end mrgend
label values current_rel_coh_end cohend

browse pidp year marital_status_defacto partner_id rel_no current_rel_start_year current_rel_start_month current_rel_end_year current_rel_end_month current_rel_ongoing rel_start rel_end rel_end_pre current_rel_marr_end current_rel_coh_end mh_partner1 mh_status1 mh_starty1 mh_startm1 mh_endy1 mh_endm1 mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 mh_partner2 mh_status2 mh_starty2 mh_startm2 mh_endy2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2

// check start dates if I use my manually created ones - to help with later info
gen rel_start_year_est = istrtdaty if rel_start==1
bysort pidp partner_id (rel_start_year_est): replace rel_start_year_est=rel_start_year_est[1]  if partner_id!=. // if partner id is missing, this actually doesn't work well

gen rel_end_year_est = istrtdaty if rel_end_pre==1
bysort pidp partner_id (rel_end_year_est): replace rel_end_year_est=rel_end_year_est[1]  if partner_id!=. // if partner id is missing, this actually doesn't work well

sort pidp year

gen rel_start_check=.
replace rel_start_check=0 if current_rel_start_year!=rel_start_year_est & current_rel_start_year!=. & rel_start_year_est!=.
replace rel_start_check=1 if current_rel_start_year==rel_start_year_est & current_rel_start_year!=. & rel_start_year_est!=.

tab rel_start_check // okay not actually a lot of congruence (even when they are both non-missing). this is problematic if the first year observed they are already in a rel. like most are just missing...

replace current_rel_start_year = rel_start_year_est if current_rel_start_year==. & rel_start_year_est!=. & partner_id!=. & istrtdaty>=rel_start_year_est
replace current_rel_end_year = rel_end_year_est if current_rel_end_year==. & rel_end_year_est!=. & partner_id!=. & istrtdaty<=rel_end_year_est

// check how many relationships have the proper info
tab current_rel_start_year if partner_id!=. , m // about 5% missing
tab current_rel_start_year if inlist(marital_status_defacto,1,2), m // about 6% missing
tab current_rel_start_year partnered, m col
tab current_rel_end_year partnered, m col

// for those with missing, maybe if only 1 spell, use that info? as long as the status matches and the interview date is within the confines of the spell?
// this might have been covered in my "rel no alt" but let's try
browse pidp istrtdaty marital_status_defacto partner_id rel_no current_rel_start_year rel_start_year_est current_rel_end_year rel_start rel_end rel_end_pre mh_ttl_spells mh_partner1 mh_status1 mh_starty1 mh_startm1 mh_endy1 mh_endm1 mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 mh_partner2 mh_status2 mh_starty2 mh_startm2 mh_endy2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2

gen rel_no_orig=rel_no

forvalues r=1/14{
	replace current_rel_start_year = mh_starty`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_start_month = mh_startm`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_end_year = mh_endy`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_end_month = mh_endm`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_ongoing = mh_ongoing`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'

	replace rel_no=`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r' // okay yes this didn't add any
}

// can I use the dates from the main file, not marital history? or the cross-wave file?
tabstat lmar1y coh1by coh1ey lmcby41 lmcby42 lmspy41 lmspy42, by(year)

by pidp: egen first_marr_yr = max(lmar1y)
by pidp: egen first_cohab_yr = max(coh1by)
by pidp: egen first_cohab_end_yr = max(coh1ey)

forvalues c=1/7{
	by pidp: egen cohab`c'_yr = max(lmcby4`c')
	by pidp: egen cohab`c'_end_yr = max(lmspy4`c')
}

tab lmar1y survey if current_rel_start_year==., m
tab first_marr_yr survey if current_rel_start_year==., m
tab xw_lmar1y_dv survey if current_rel_start_year==., m
tab xw_coh1y_dv survey if current_rel_start_year==., m

sort pidp year
browse pidp istrtdaty marital_status_defacto partner_id rel_no mh_ttl_spells mh_ttl_married mh_ttl_cohabit xw_lmar1y_dv xw_coh1y_dv lmar1y first_marr_yr coh1by coh1ey first_cohab_yr lmcby41 lmcby42 mh_starty1 mh_endy1 mh_starty2 mh_endy2 if current_rel_start_year==. & partnered==1

replace current_rel_start_year = xw_lmar1y_dv if current_rel_start_year==. & marital_status_defacto==1 & mh_ttl_married==1 & inrange(xw_lmar1y_dv, 1900,2024) // if married and only has one marriage, use this date?
replace current_rel_start_year = first_marr_yr if current_rel_start_year==. & marital_status_defacto==1 & mh_ttl_married==1 & inrange(first_marr_yr, 1900,2024)
replace current_rel_start_year = xw_coh1y_dv if current_rel_start_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(xw_coh1y_dv, 1900,2024) 
replace current_rel_start_year = first_cohab_yr if current_rel_start_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(first_cohab_yr, 1900,2024) // same for cohab
replace current_rel_start_year = cohab1_yr if current_rel_start_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(cohab1_yr, 1900,2024) 
replace current_rel_end_year = first_cohab_end_yr if current_rel_end_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(first_cohab_end_yr, 1900,2024) 
replace current_rel_end_year = cohab1_end_yr if current_rel_end_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(cohab1_end_yr, 1900,2024)
// replace current_rel_start_year = mh_starty1 if current_rel_start_year==. & mh_ttl_spells==1 & inrange(mh_starty1, 1900,2024) & inlist(marital_status_defacto,1,2) // this is giving me errors for cohabitors sometimes
// replace current_rel_end_year = mh_endy1 if current_rel_end_year==. & mh_ttl_spells==1 & inrange(mh_endy1, 1900,2024) & inlist(marital_status_defacto,1,2)

sort pidp year
browse pidp year mh_ttl_cohabit rel_no rel_no_alt rel_no_orig current_rel_start_year current_rel_end_year rel_start rel_end rel_end_pre xw_coh1y_dv coh1by coh1ey cohab1_yr cohab2_yr cohab1_end_yr cohab2_end_yr if marital_status_defacto==2  & (current_rel_start_year==. | current_rel_end_year==.)
browse pidp year mh_ttl_married rel_no current_rel_start_year current_rel_end_year rel_start rel_end rel_end_pre xw_lmar1y_dv lmar1y mh2_year_marr1 mh2_year_end1 mh2_how_end1 mh2_year_marr4 mh2_year_end4 mh2_how_end4 mh2_year_marr3 mh2_year_end3 mh2_year_marr2 mh2_year_end2 mh_status1 mh_starty1 mh_endy1 if marital_status_defacto==1 & (current_rel_start_year==. | current_rel_end_year==.)

tab current_rel_start_year marital_status_defacto if partnered==1, m col
tab current_rel_end_year marital_status_defacto if partnered==1, m col

// browse pidp istrtdaty marital_status_defacto partner_id rel_no current_rel_start_year current_rel_end_year rel_start rel_end rel_end_pre mh_ttl_spells mh_partner1 mh_status1 mh_starty1 mh_startm1 mh_endy1 mh_endm1 xw_lmar1y_dv xw_coh1y_dv mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 first_marr_yr first_cohab_yr mh_partner2 mh_status2 mh_starty2 mh_startm2 mh_endy2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2

// what things are correlated with missing relationship start year?
gen missing_rel_start=.
replace missing_rel_start = 0 if partnered==1 & current_rel_start_year!=.
replace missing_rel_start = 1 if partnered==1 & current_rel_start_year==.

gen missing_rel_end=.
replace missing_rel_end = 0 if partnered==1 & current_rel_end_year!=.
replace missing_rel_end = 1 if partnered==1 & current_rel_end_year==.

tab sampst missing_rel_start, row // codebook says psm and tsm might have more missing, that is true (We are aware that the information about past partnership history is incomplete or missing for new entrants and those who asked someone else to complete their interview on their behalf (proxy interviews))
tab sampst missing_rel_end, row 
tab ivfio missing_rel_start, row // and proxy interviews, so that is also true (96% of full interviews have a rel start)
tab ivfio missing_rel_end, row 
tab college_degree missing_rel_start, row // quite evenly distrbuted
tab college_degree missing_rel_end, row 
tab country_all missing_rel_start, row // quite evenly distributed
tab employed missing_rel_start, row // quite evenly distributed
tab age_all missing_rel_start, row
// okay so it really is about the type of sample / interview, not really the people themselves. so I think fine to just leave as missing for now?

// final clean up of file before saving
gen int_year = istrtdaty 
replace int_year = year if istrtdaty < 0 // missing / dk, so not very helpful...

// also need to make sure the start / end dates are congruent if the couple transitioned from cohabitation to marriage gAH
bysort pidp partner_id: egen ever_transition = max(marr_trans) if partnered==1
gen year_transitioned = int_year if marr_trans==1 // note, year transition is first year marrriage. so years prior are cohab, then that year onward is marriage
bysort pidp partner_id (year_transitioned): replace year_transitioned = year_transitioned[1]

sort pidp year
browse pidp partner_id int_year marital_status_defacto marr_trans ever_transition year_transitioned if partnered==1

// some cohab dates missing so the above is messing things up for marriage to cohab (start date after rel start)
bysort pidp partner_id: egen first_couple_year = min(int_year) if partnered==1
bysort pidp partner_id: egen last_couple_year = max(int_year) if partnered==1

sort pidp year
browse pidp partner_id int_year marital_status_defacto first_couple_year last_couple_year current_rel_start_year current_rel_end_year marr_trans if ever_transition==1

// updating dates specifically for those who transition because it will mess up the codes otherwise. add a flag to denote that I did still
gen rel_end_flag=.
replace rel_end_flag=0 if current_rel_end_year!=.
replace rel_end_flag=1 if current_rel_end_year==.

gen rel_start_flag=.
replace rel_start_flag=0 if current_rel_start_year!=.
replace rel_start_flag=1 if current_rel_start_year==.

replace current_rel_start_year = first_couple_year if current_rel_start_year==. & ever_transition==1 & marital_status_defacto==2
replace current_rel_end_year = last_couple_year if current_rel_end_year==. & ever_transition==1 & marital_status_defacto==1

bysort pidp partner_id: egen rel_start_all = min(current_rel_start_year) if partnered==1
bysort pidp partner_id: egen rel_end_all = max(current_rel_end_year) if partnered==1
bysort pidp partner_id: egen status_all = max(current_rel_ongoing) if partnered==1

capture label define status 0 "ended" 1 "ongoing"
label values status_all status

sort pidp year
replace rel_end_all = rel_end_all[_n-1] if rel_end_all==. & rel_start_all == rel_start_all[_n-1] & pidp==pidp[_n-1]

browse pidp partner_id int_year marital_status_defacto first_couple_year last_couple_year rel_start_all rel_end_all status_all current_rel_start_year current_rel_end_year marr_trans ever_transition

gen date_check=0
replace date_check = 1 if rel_end_all < rel_start_all & rel_end_all!=. & rel_start_all!=.

// browse pidp partner_id int_year marital_status_defacto rel_no first_couple_year last_couple_year rel_start_all rel_end_all status_all current_rel_start_year current_rel_end_year mh_starty1 mh_endy1 mh_starty2 mh_endy2 if date_check==1

replace rel_start_all = first_couple_year if date_check==1 & rel_start_all > last_couple_year
replace rel_end_all = last_couple_year if date_check==1 & rel_end_all < first_couple_year

gen int_date_check=0 if partnered==1
replace int_date_check =1 if partnered==1 & int_year >= rel_start_all & int_year <= rel_end_all & rel_start_all!=. & rel_end_all!=.
tab int_date_check if rel_start_all!=.
tab int_date_check if rel_end_all!=.
tab int_date_check if rel_start_all!=. & rel_end_all!=.

// browse pidp partner_id int_year marital_status_defacto rel_no rel_no_est rel_start_all rel_end_all status_all current_rel_start_year current_rel_end_year mh_starty1 mh_endy1 mh_starty2 mh_endy2 first_couple_year last_couple_year if partnered==1

save "$created_data/UKHLS_long_all_recoded.dta", replace

// note: this file is still NOT restricted in any way
unique pidp // 118405, 807942 total py
unique pidp partner_id // 135496	
unique pidp, by(xw_sex) // 56297 m, 62216 w
unique pidp, by(partnered) // 59866 0, 73581 1
unique pidp partner_id if partnered==1 & current_rel_start_year==. // 8124 couples. does that feel like too many to be missing? okay now 7066
unique pidp partner_id if partnered==1 & current_rel_end_year==. //  8150 couples. does that feel like too many to be missing? okay now it is 5688. when did this change gah?!
// unique pidp partner_id if partnered==1, by(current_rel_start_year)