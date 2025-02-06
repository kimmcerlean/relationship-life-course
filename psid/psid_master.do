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

*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots

// define the reference folder for all 
   
/*Where and when are you working on this?*/
	global date: di %tdYND daily("$S_DATE", "DMY")	// YYMMDD. Update to avoid saving over previous work
	global comp "kim"	// change who is working

/*What do you want this program to do?*/
	global databuild = 1 //==1 if want to build analysis file from raw data
	global dataclean = 0 //==1 if want to create and impute data for our analysis specifically
	global analysis = 0  //==1 if want to do the sequence analysis

/*Setting directories based on ${comp}*/
	if ("${comp}"=="kim") {
		if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global root `"G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)"' // Kim's Personal Computer
		if `"`c(hostname)'"' == "PPRC-STATS-P01" global root `"T:/Research Projects/Relationship Life Course (with LP)"' // PRC Stats Server
		if `"`c(hostname)'"' == "60018D" global root `"C:/Users/kmcerlea/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course"' // EUI Computer
			}
	if ("${comp}"=="lea") {
		// global root	"G:\My Drive\(4) Pessin & Pojman - LCA - PSID 2017" // Lea to change
		}	

// define globals
global PSID    "$root/PSID data" /*original PSID data*/
global temp    "$root/temp data" /*intermediary processing files*/
global created_data "$root/created data" /*created data*/
global results  "$root/results"
global code    "$root/code/relationship-life-course" // should match names if cloned from github
// global graphs  "$root/graphs"
// global tables  "$root/tables"

cd "$code"

capture log using "$results/LifeCourse_${date}.log", append

/* Get raw PSID data and organize*/

	if (${databuild}==1) {
	
	// Starting data analysis
	
		// 1. Transform PSID data from text to Stata	
		do "$code/J296292.do"
		

}

/*Get data for life course sample specifically and clean and impute*/

	if (${dataclean}==1) {
	

		
}

/*Do sequence analysis*/

	if (${analysis}==1) {
	

		
}
