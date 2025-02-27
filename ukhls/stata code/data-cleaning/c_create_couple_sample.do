********************************************************************************
********************************************************************************
* Project: Relationship Life Course Analysis
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: c_create_couple_sample.do
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file restricts full UKHLS sample to individuals in eligible couples

* Eligibility


use "$created_data/UKHLS_long_all_recoded.dta", clear

// think I will create a lookup file of couples from here, then use that to match to data file and only keep if the id is flagged in couple list?
// so essentially what I did for PSID, but keep all of these recodes and do more of a flag here, don't like get the list, then recode all of the data AGAIN