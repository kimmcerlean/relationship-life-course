********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean and Léa Pessin
* Started: September 2024
* File: psid_master.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
/*
CREATED [Kim McErlean] [02.06.25]
PURPOSE [Creating the master .do file]

CHANGE LOG:

*/

clear all
macro drop _all
set more off

set seed 8675309

set maxvar 10000

net install cleanplots, from("https://tdmize.github.io/data/cleanplots") replace
set scheme cleanplots

net install desctable, from("https://tdmize.github.io/data") replace

net install mimrgns, from("http://fmwww.bc.edu/RePEc/bocode/m/") replace
net install outreg2, from("http://fmwww.bc.edu/RePEc/bocode/o") replace
ssc install mdesc, replace

/*
 INSTALL THESE FIRST
ssc install sq, replace
ssc install moremata, replace

net sj 17-3 st0486 // SADI
net install st0486, replace

net sj 16-3 st0445 // MICT
net install st0445, replace
net get st0445 // these go into working directory (the ancillary files); pwd shows current working directory
*/

// ssc install coefplot, replace

// define the reference folder for all 
   
/*Where and when are you working on this?*/
	global date: di %tdYND daily("$S_DATE", "DMY")	// YYMMDD. Update to avoid saving over previous work
	global comp "kim"	// change who is working

/*What do you want this program to do?*/
	global dataclean = 1 //==1 if want to build analysis file from raw data
	global prep = 0 //==1 if want to create and impute data for the cluster analysis in R
	global analysis = 0  //==1 if want to do the analysis of clusters created in R

/*Setting directories based on ${comp}*/
	if ("${comp}"=="kim") {
		if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global root `"C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course"' // Shared One Drive on Kim's PC
		// if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global root `"G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)"' // Kim's Personal Computer
		if `"`c(hostname)'"' == "PPRC-STATS-P01" global root `"T:/Research Projects/Relationship Life Course (with LP)"' // PRC Stats Server
		if `"`c(hostname)'"' == "PPRC-STATS-P01" global code `"T:/github/relationship-life-course/psid"'
		if `"`c(hostname)'"' == "60018D" global root `"C:/Users/kmcerlea/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course"' // Team folder on EUI Computer
		if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global code    "G:/Other computers/My Laptop/Documents/GitHub/relationship-life-course/psid" // I am actually not sure if having the code on a shared directory will work. I am a bit worried about version control if the code can be updated in OneDrive but not yet pushed to github; I am worried about accidental changes. putting code into my own folder for now
	}
			
			
	if ("${comp}"=="lea") {
		// global root	"G:\My Drive\(4) Pessin & Pojman - LCA - PSID 2017" // Lea to change
		global code    "$root/code/relationship-life-course/psid" // should match names if cloned from github
	}	

// define globals
global PSID    "$root/PSID data" /*original PSID data*/
global temp    "$root/temp data" /*intermediary processing files*/
global created_data "$root/created data" /*created data*/
global results  "$root/results/PSID"
global tables  "$root/results/PSID/tables"
global models "$root/results/PSID/models"
global logs "$root/results/logs"	
// global graphs  "$root/graphs"

cd "$code"
/*
capture log using "$results/logs/LifeCourse_${date}.log", append

/* Get raw PSID data and organize*/

	if (${dataclean}==1) {
	
	// Starting data analysis
	
		// 1. Get PSID data, rename variables and reshape to long
		do "$code/stata code/data-cleaning/01_get_data.do"
		
		// 2. Restrict to only coupled individuals
		do "$code/stata code/data-cleaning/02_create_couple_sample.do"
		
		// 3. Get relationship history for all PSID respondents, using marital history, move dates, and other variables
		do "$code/stata code/data-cleaning/03_create_cohab_history.do"
		
		// 4. Merge on relationship history and create other couple-level variables.
		// Note: most of these are not actually used anymore because of data imputation
		do "$code/stata code/data-cleaning/04_variable_recodes.do"		

}

/*Get data for life course sample specifically and clean and impute*/

	if (${prep}==1) {
	
		// 1. Use relationship variables to get eligible sample for our analysis and create unique list of individuals
		do "$code/stata code/analysis/01_get_couple_sample.do"
		
		// 2. Create all focal variables, fill in off survey years when possible, and otherwise
		// get ready for imputation
		do "$code/stata code/analysis/02_prep_imputation_for_individs.do"
		
		// 3. Actually do the imputation
		do "$code/stata code/analysis/03_imputation_for_individs.do"
		
		// 4. Take imputed variables and create couple-level variables for analysis.
		// Creates final wide couple-level file for sequence analysis
		do "$code/stata code/analysis/04_match_couples_create_vars.do"
}

/*Do sequence analysis*/

	if (${analysis}==1) {
	

		
}
*/
