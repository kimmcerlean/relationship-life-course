*
**|=============================================|
**|	    ####	CPF	v1.5	####				|
**|		>>>	SOEP						 		|
**|		>>	New Vars for the merged dataset		|
**|---------------------------------------------|
**|		Konrad Turek 	| 	2023				|
**|=============================================|
*

/// This is a slimmed down version of the CPF file. I deleted variables not relevant to this analysis
/// I have also added in the variables name in my file (as I renamed most of these already) as well as which base file they came from
/// Note: I don't use this directly - I am just annotating here, and will pull in the relevant code to a different file

	
*################################
*#								#
*#	Socio-demographic basic 	#
*#								#
*################################
**--------------------------------------
** Demographic
**--------------------------------------
* age
gen age=piyear - gebjahr if gebjahr>100 & piyear>100 // gebjahr is from ppathl; i renamed to birthyr_pl. piyear=syear
recode age (.=-1)
/* Compare with pequiv var
sum age d11101 if d11101>=0 & age >=0
bro pid syear piyear gebjahr d11101 age 
*/

* Birth year
clonevar yborn=gebjahr
	lab var yborn "Birth year" 

* Gender
recode sex (1=0) (2=1) (else=.), gen(female)
	lab def female 0 "Male" 1 "Female" 
	lab val female female 
	lab var female "Gender" 

*################################
*#								#
*#	Education					#
*#								#
*################################
**--------------------------------------
** Years 
**--------------------------------------
* d11109 - years ok 
gen eduy=d11109 // this is from pequiv, I renamed yrs_educ		

recode eduy (.=-1) (-2=-1)
lab var eduy "Education: years"


 *** edu3
 
/*Note:
- differences in coding between isced97 11 and casmin
- d11109 also gives different results 
*/


recode pgisced11 (0/2=1) (3 4=2) (5/8=3) , gen(edu3a) // from pgen, I renamed isced11_pg
recode pgisced97 (0/2=1) (3 4=2) (5/6=3) , gen(edu3b) // from pgen, I renamed isced97_pg
replace edu3b = 2 if intyear <2010 & pgisced97>=5 & pgcasmin==3 & d11109<12 & d11109>0 // also includes casmin_pg (from pgen)
gen edu3=edu3a
replace edu3=edu3b if intyear <2010
replace edu3=edu3b if intyear >=2010 & (edu3<0|edu3==.)

	drop edu3a edu3b
	
	lab def edu3  1 "[0-2] Low" 2 "[3-4] Medium" 3 "[5-8] High" // 2 incl Vocational
	lab val edu3 edu3
	lab var edu3 "Education: 3 levels"

	
*** edu4 -- this feels sufficient? (don't know we need to split tertiary out - like below)
recode pgisced11 (0 1=1) (2=2) (3 4=3) (5/8=4) , gen(edu4a)
recode pgisced97 (0 1=1) (2=2) (3 4=3) (5 6=4) , gen(edu4b)
replace edu4b = 3 if intyear <2010 & pgisced97>=5 & pgcasmin==3 & d11109<12 & d11109>0
gen edu4=edu4a
replace edu4=edu4b if intyear <2010
replace edu4=edu4b if intyear >=2010 & (edu4<0|edu4==.)

	drop edu4a edu4b
		
	lab def edu4  1 "[0-1] Primary" 2 "[2] Secondary lower" ///
				  3 "[3-4] Secondary upper" 4 "[5-8] Tertiary" 
	lab val edu4 edu4
	lab var edu4 "Education: 4 levels"
	
*** edu5
recode pgisced11 (0 1=1) (2=2) (3 4=3) (5 6=4) (7 8=5) , gen(edu5a)
recode pgisced97 (0 1=1) (2=2) (3 4=3) (5=4) (6=5) , gen(edu5b)
replace edu5b = 3 if intyear <2010 & pgisced97>=5 & pgcasmin==3 & d11109<12 & d11109>0
replace edu5b = 4  if intyear <2010 & pgisced97==6 & pgcasmin==8   
replace edu5b = 5  if intyear <2010 & pgisced97==6 & pgcasmin==9   
gen edu5=edu5a
replace edu5=edu5b if intyear <2010
replace edu5=edu5b if intyear >=2010 & (edu5<0|edu5==.)

	drop edu5a edu5b
		
	lab def edu5  1 "[0-1] Primary" 2 "[2] Secondary lower" ///
				  3 "[3-4] Secondary upper" ///
				  4 "[5-6] Tertiary lower(bachelore)"  ///
				  5 "[7-8] Tertiary upper (master/doctoral)"
				  
	lab val edu5 edu5
	lab var edu5 "Education: 5 levels"

* Alternative version:
/*NOTE:
- only for 2010+ 
*/	
recode pgisced11 (0 1=1) (2=2) (3 4=3) (5 6 7=4) (8=5) if intyear >=2010, gen(edu5v2)

	lab def edu5v2  1 "[0-1] Primary" 2 "[2] Secondary lower" ///
					3 "[3-4] Secondary upper" ///
					4 "[5-7] Tertiary first(bachelore/master)"  ///
					5 "[8] Tertiary second (doctoral)"
	
	lab val edu5v2 edu5v2
	lab var edu5v2 "Education: 5 levels v2"

*################################
*#								#
*#	Family and relationships	#
*#								#
*################################		
**--------------------------------------
** Primary partnership status  (from CNEF) 	 
**--------------------------------------
* Approach based on CNEF 
* Equal to CNEF's d11104  
* NOTE: 
* - categories of 'single' and 'living with partner' not fully precise and can be contradictory to other variables 
* - country differences in inclusion of having/living with partner
* - country differences in definition of ‘single’ 


recode pgfamstd (1 7=1)(2 6 8=5)(3=2)(4=4)(5=3)	/// I have this as marital_status_pg (from pgen)
				(-1=-2) (-3=-1) (-5 -8=-8), gen(marstat5)

	lab var marstat5 "Primary partnership status [5]"
	lab def marstat5				///
	1	"Married or Living with partner"	///
	2	"Single" 				///
	3	"Widowed" 				///
	4	"Divorced" 				///
	5	"Separated" 			///
	-1 "-1 MV general" -2 "-2 Item non-response" ///
	-3 "-3 Does not apply" -8 "-8 Question not asked in survey"
	lab val marstat5 marstat5
		
		
**--------------------------------------
** Formal marital status 	 
**--------------------------------------
* Formal marital status
* Only formal marital status included, no info on having/living with partner
* Never married include singles  

recode pgfamstd (1 7=1)(2 6 8=5)(3=2)(4=4)(5=3)	///
				(-1=-2) (-3=-1) (-5 -8=-8), gen(mlstat5)

	lab var mlstat5 "Formal marital status [5]"
	lab def mlstat5				///
	1	"Married/registered"	///
	2	"Never married" 		///
	3	"Widowed" 				///
	4	"Divorced" 				///
	5	"Separated" 			///
	-1 "-1 MV general" -2 "-2 Item non-response" ///
	-3 "-3 Does not apply" -8 "-8 Question not asked in survey"
	lab val mlstat5 mlstat5



**--------------------------------------
** Partnership living-status 	 
**--------------------------------------
* Includes inforamtion on marital status and whether living with partner in HH 

				
gen parstat6=-3 if partner<.				
replace parstat6=6 if (pgfamstd==2|pgfamstd==6|pgfamstd==8) & partner==0 
replace parstat6=5 if pgfamstd==4 & partner==0 
replace parstat6=4 if pgfamstd==5 & partner==0 
replace parstat6=3 if pgfamstd==3 & partner==0
replace parstat6=2 if (pgfamstd!=1 & pgfamstd!=7) & (partner>=1 & partner<=4) 
replace parstat6=1 if (pgfamstd==1|pgfamstd==7)	// pgfamstd is cleaned, so it has a priority over partner info 

		recode  pgfamstd (1 7=1)(2 6 8=6)(3=3)(4=5)(5=4)	///
						 (-1=-2) (-3=-1) (-5 -8=-8), gen(temp_parstat6)
		replace parstat6=temp_parstat6 if parstat6==-3 & temp_parstat6>0 & temp_parstat6<.


	lab var parstat6 "Partnership living-status [6]"
	lab def parstat6				///
	1	"Married/registered, with P"	///
	2	"Cohabiting (Not married, Living with P)"				///
	3	"Single, No P" 				///
	4	"Widowed, No P" 				///
	5	"Divorced, No P" 			///
	6	"Separated, No P" 			///
	-1 "-1 MV general" -2 "-2 Item non-response" ///
	-3 "-3 Does not apply" -8 "-8 Question not asked in survey"
	lab val parstat6 parstat6
	
	

**--------------------------------------
** Children , people in HH
**--------------------------------------
*  
clonevar kidsn_all= sumkids // this is from biobirth. I feel like these are two different constructs - children born v. in HH
clonevar kidsn_hh17= d11107 // this is from pequiv; I renamed num_children_cnef
// this is making me realize - they might use the CNEF version rather than the fact that they have like 3 different versions of kids in the non-cnef version (that I was going to try to figure out how to harmonize)

recode kidsn_all (1/50=1) (0=0), gen(kids_any)
 	
	lab var kids_any  "Has any children"
	lab val kids_any   yesno
	lab var kidsn_all  "Number Of Children Ever Had" 
//  	lab var kidsn_18   "Number Of Children <18 y.o." 
// 	lab var kidsn_15   "Number Of Children <15 y.o." 
 	lab var kidsn_hh17   "Number of Children in HH aged 0-17"


*** New in CPF 1.52
*
gen kidsn_hh_04  =  h11103 + h11104 // these are from pequiv and I actually did not pull in. Instead, I used the HH info to get an indicator of kids in the HH (u18 and u6), but this might be better. Okay, I have now pulled in all of the variables (labeled num_hh_#_#_cnef) - there are 7 such variables from 0-1 to 16-18
gen kidsn_hh_510 = 	h11105 + h11106
	
// 	lab var kidsn_hh_02   "Number of Children in HH aged 0-2"
// 	lab var kidsn_hh_34   "Number of Children in HH aged 3-4"
	lab var kidsn_hh_04   "Number of Children in HH aged 0-4"
	lab var kidsn_hh_510  "Number of Children in HH aged 5-10"
*
recode kidsn_hh_04 (0=0)(1/20=1), gen(kids_hh_04)
	lab var kids_hh_04   "Any children in HH aged 0-4?"
	lab val kids_hh_04   yesno


**--------------------------------------
** People in HH F14
**--------------------------------------
clonevar nphh= d11106 // hh_size_cnef = my variable
	
	lab var nphh   "Number of People in HH" 
	
**--------------------------------------
** Binary specific current partnership status (yes/no)
**--------------------------------------
*** Specific current marital statuses (probably the same as Formal marital status)
* No mater if currently living with a partner
*  Probably overlap with marstat5. If strong overlap in all countries, we can delete them. 

// 		lab var cmarr   "Currently: married"
// 		lab var cwidow 	"Currently: widowed (no mater partner)"
// 		lab var cdivor  "Currently: divorced (no mater partner)"
// 		lab var csepar  "Currently: separated (no mater partner)"
// 		lab val cmarr cwidow cdivor csepar yesno
		
*** Partner
	// pld0133 - filtered

	recode partner (0 5=0) (1/4=1), gen(livpart) // from ppathl
// 		lab var haspart "Has a partner"
		lab var livpart "Living together with partner"
		lab val   livpart  yesno


*** Single
// 		lab var csing "Single: not married and no partner"
// 		lab val csing yesno
								
*** Never married 
recode pgfamstd (3=1)(1 2 4/8=0)	///
				(-1=-2) (-3=-1) (-5 -8=-8), gen(nvmarr)

		lab var nvmarr "Never married"
		lab val nvmarr yesno					
		
		
*** Widowed
recode mlstat5 (3=1) (1 2 4 5=0), gen(widow) // created in row 166 - basically turning it into a categorical variable. I don't think I need these

		lab var widow "Widowed (current status)"
		lab val widow yesno	
		
*** Divorced
recode mlstat5 (4=1) (1 2 3 5=0), gen(divor)

		lab var divor "Divorced (current status)"
		lab val divor yesno	


*** Separated
recode mlstat5 (5=1) (1/4=0), gen(separ)

		lab var separ "Separated (current status)"
		lab val separ yesno	

								
*################################
*#								#
*#	Labour market situation		#
*#								#
*################################
 
**--------------------------------------
** hours conracted
**--------------------------------------
*pgvebzeit
clonevar whweek_ctr=pgvebzeit // from pgen: work_hours_agreed_pg
	lab var whweek_ctr "Work hours per week: conracted"

**--------------------------------------
** hours worked 
**--------------------------------------
*pgtatzeit	 plb0186_v1 plb0186_v2 e11101
/*Notes: 
- pgtatzeit is harmonized and corrected 
- e11101 is imputed (pequiv)
*/

clonevar whweek=pgtatzeit // work_hours_pg (pgen) 
clonevar whyear=e11101 // this is the pequiv version and is ANNUAL work hours (not weekly). I did not pull this in

	lab var whweek "Work hours per week: worked"
	lab var whyear "Work hours per year: worked" 


**--------------------------------------
** overtime working  
**--------------------------------------
* pguebstd plb0193 plb0196_h plb0197
// my corresponding labels
// overtime_pg (pgen)
// work_ot (pl)
// work_ot_lastmonth (pl)
// hours_ot (pl)
			
*################################
*#								#
*#	Retired						#
*#								#
*################################

**--------------------------------------
** Fully retired - identification
**--------------------------------------
*   

* create zeros (obs with any type of information)
recode  plb0022_h (1/11=0) (-5/-1=-1), gen (retf) // this is the employment status variable in pl (I recoded as employment)
replace retf=0 if pgstib>=10 & pgstib<. // this is "occupational position" from pgen. I don't currently have this variable. There are many many categories but possibly add so I can use this code?
replace retf=0 if pglfs>=1 & pglfs<. // laborforce_pg
* create MV(-1) with no information 
replace retf=-1 if   (plb0022_h<0 | plb0022_h==.) & (pgstib<=0 | pgstib==.) & (pglfs<0 | pglfs==.)

* Fill MV
		* Code below fills information for individuals who left due to retirement
		* for years after the event
		sort pid syear
		bysort pid: gen  temp_order= _n
		bysort pid: gen  temp_yret=syear if plb0304_h==6 // also don't have this variable from pl: type of termination from employment. 6 = reaching retirement age
		bysort pid: egen temp_yret2=max(temp_yret)
		bysort pid: gen  temp_ret=1 if syear>=temp_yret2
		
* Criteria for 1
replace retf=1  if (plb0022_h==9 | plb0022_h==7 | plb0022_h==5) &		/// NW or Voluntary Services (FSJ / FOEJ / BFD) or Near Retirement, Zero Working Hours
					 pglfs!=11 &								/// NW
					 temp_ret==1 & age>=50							// left job becouse of retirement (plb0304_h)
		* Can drop the temporary files 	
		drop temp_*
	
replace retf=1  if (plb0022_h==9 | plb0022_h==7 | plb0022_h==5) & pglfs!=11 & age>=65	// age>65

replace retf=1  if (plb0022_h==9 | plb0022_h==7 | plb0022_h==5) &	 pglfs!=11 &	/// 
					  (kal1e01==1 | kal2d01==1)  &		/// received pension (maybe early) // these are frm pkal and I do not yet have
					  age>=50						//  age 
	
	lab var retf "Retired fully (NW, old-age pens, 45+)"
	lab val retf yesno 	
	
	
	
**--------------------------------------
** Employment Status moved here to use retf and un_act 
**--------------------------------------
/*Notes: 
- to do based on Retirement and Unemployed
- pglfs - not sure how created 
- pglfs pgstib
- plb0022_h	 pgemplst - more less the same (pgemplst  simplified)
*/

* emplst5 // okay this basically uses same variables as above
// I also don't know that i want to recode this; might want to use as is?

* create zeros (obs with any type of information)
recode  plb0022_h (1/11=0) (-5/-1=-1), gen (emplst5)
replace emplst5=0 if pgstib>=10 & pgstib<.
replace emplst5=0 if pglfs>=1 & pglfs<.
* create MV(-1) with no information 
replace emplst5=-1 if   (plb0022_h<0 | plb0022_h==.) & (pgstib<=0 | pgstib==.) & (pglfs<0 | pglfs==.)

* Categories 
replace emplst5=4 if  plb0022_h==5 | plb0022_h==6| plb0022_h==7 | plb0022_h==9 | plb0022_h==11 
replace emplst5=4 if  (pglfs>=1 & pglfs<=10)
replace emplst5=4 if pgstib>=10 & pgstib<=13

replace emplst5=3 if retf==1
replace emplst5=3 if pgstib==13 | pglfs ==2 // Pensioner or NW-age 65 and older

replace emplst5=5 if pgstib==11 | pglfs==3

replace emplst5=2 if pgstib==12 | pglfs==6 
replace emplst5=2 if un_act==1 // only from 1994+ 

replace emplst5=1 if (plb0022_h>=1 & plb0022_h<=4) | plb0022_h==8 | plb0022_h==10
replace emplst5=1 if pglfs==11 | (pglfs==12 & pgstib!=13)
// replace emplst5=1 if pgstib>100 & pgstib<. // less reliable (more contradictory results)


	lab def emplst5	///
			1 "Employed" 			/// including leaves
			2 "Unemployed (active)"	///
			3 "Retired, disabled"	///
			4 "Not active/home"		///   
			5 "In education"		///
			-1 "MV"
	lab val emplst5 emplst5
	lab var emplst5 "Employment status [5]"
	
* emplst6
gen emplst6=emplst5
replace emplst6=6 if (pglfs==4 | pglfs==12) & /// NW-maternity leave or NW-work but past 7 days
			pgstib>13 & pgstib<.	//

	lab def emplst6	///
			1 "Employed" 			///  
			2 "Unemployed (active)"	///
			3 "Retired, disabled"	///
			4 "Not active/home"		///   
			5 "In education"		///
			6 "On leave (employed)" ///
			-1 "MV"
	lab val emplst6 emplst6
	lab var emplst6 "Employment status [6]"


**--------------------------------------
** full/part time
**--------------------------------------
*  plb0022_h e11103
/*Notes: 
*/
recode plb0022_h (1=1) (2 4=2) (3 5/12=3) (-5 .=-1), gen(fptime_r)

*
gen fptime_h=.
replace fptime_h=1 if whweek>=35 & whweek<. // these are created above
replace fptime_h=2 if whweek<35 & whweek>0
replace fptime_h=3 if whweek==0
replace fptime_h=3 if emplst5>1 & emplst5<.
replace fptime_h=whweek if whweek<0 & fptime_h==.


lab def fptime_r 1 "Full-time" 2 "Part-time/irregular" 3 "Not empl/other"
lab val fptime_r fptime_h fptime

lab var fptime_r "Employment Level (self-report)"
lab var fptime_h "Employment Level (based on hours)"

*################################
*#								#
*#	Income and wealth			#  
*#								#
*################################

**--------------------------------------
**   Work Income - detailed
**--------------------------------------
* CNEF: i11110  ijob1 ijob2
/*Note: 
- more variables in Pequiv file
*/


* whole income (jobs, benefits)

	
	*lab var inctot_yn "Individual Income (All types, year, net)"
	*lab var inctot_mn "Individual Income (All types, month, net)"

	
* all jobs 
clonevar incjobs_yg=i11110

	*lab var incjobs_yn "Individual Labor Earnings (All jobs, year, net)"
	lab var incjobs_yg "Individual Labor Earnings (All jobs, year, gross)"
	*lab var incjobs_mn "Individual Labor Earnings (All jobs, month, net)"

* main job
clonevar incjob1_yg=ijob1
clonevar incjob1_mg=pglabgro // think I want to rely on one of these (my name: gross_labor_inc_pg)  
clonevar incjob1_mn=pglabnet // think I want to rely on one of these (my name: net_labor_inc_pg)

	lab var incjob1_yg  "Salary from main job (year, gross)"
	*lab var incjob1_yn  "Salary from main job (year, net)"
	lab var incjob1_mg "Salary from main job (month, gross)"
	lab var incjob1_mn "Salary from main job (month, net)"	


*################################
*#								#
*#	Health status				#
*#								#
*################################
**--------------------------------------
**  Self-rated health 
**--------------------------------------
*      m11126    
 /*Note: 
- ple0008 has more data but better to use m11126 which was cleaned 
- m11126 - see Pequiv manual
- 1984-1991, 1993: Data not available in SOEP
- data for 1992 and since 1994
*/

// so I only have the pl version above (as self_reported_health)
// the other one is pequiv version - as they say, seems to have many more DNA.
// not sure why we wouldn't supplement possibly?

recode m11126  (-2=-3) (-1=-2) , gen(srh5)

	lab var srh5 "Self-rated health"
	lab def srh5 5 "Very bad" 4 "Bad" 3 "Satisfactory" 2 "Good" 1 "Very good"
	lab val srh5 srh5

**--------------------------------------
**  Disability 
**--------------------------------------
*   ple0040 ple0041_h m11124
// disability_yn
// disability_amount
// I didn't pull in the pequiv version

/*Note: 
- m11124=1 if >30% disability 
- check if ple0041 possible to harmonize
*/
recode ple0040 (1=1) (2=0) (-5 -8=-8) (-2=-3) (-1=-2), gen(disab)
recode m11124 (1=1) (-5=-8) (-2=-3) (-1=-2), gen(disab2c)

	lab var disab	"Disability (any)"
	lab var disab2c "Disability (min. category 2 or >30%)"
	lab val disab disab2c yesno

*################################
*#								#
*#	Parents						#
*#								#
*################################		


**--------------------------------------  
**   Parents' education
**--------------------------------------
*** Father 
   
	   
*edu3
recode fsedu (1/2 6/8=1)(3 9=2)(4 5=2) (-5=-8)(-2=-3) (0=-1), gen(fedu3) // from bioparen: father_educ_bp
recode fprofedu (26/27 30/32=3) (20 28=2) (20 23 50 51=1)	/// // from bioparen: father_vocational_bp
				(-5/0=-10) (else=-9),  gen(temp_fedu3)

replace fedu3=temp_fedu3 if (fedu3==. | fedu3<0) & (temp_fedu3>0 & temp_fedu3<.) 
replace fedu3=temp_fedu3 if temp_fedu3>fedu3 & (temp_fedu3>0 & temp_fedu3<.) 


	lab val fedu3 edu3
	lab var fedu3 "Father's education: 3 levels"

	
* edu4
recode fsedu (6/8=1)(1/2=2) (3 9=3)(4 5=3) (-5=-8)(-2=-3) (0=-1), gen(fedu4)  // from bioparen: father_educ_bp
recode fprofedu (26/27 30/32=4) (20 28=3) (50 51=1) (20 23=2)	/// // from bioparen: father_vocational_bp
				(-5/0=-10) (else=-9),  gen(temp_fedu4)

replace fedu4=temp_fedu4 if (fedu4==. | fedu4<0) & (temp_fedu4>0 & temp_fedu4<.) 
replace fedu4=temp_fedu4 if temp_fedu4>fedu4 & (temp_fedu4>0 & temp_fedu4<.) 

	lab val fedu4 edu4
	lab var fedu4 "Father's education: 4 levels"
	
drop temp_fedu*

*** Mother 
*edu3
recode msedu (1/2 6/8=1)(3 9=2)(4 5=2) (-5=-8)(-2=-3) (0=-1), gen(medu3) // same as above just mother instead of father
recode mprofedu (26/27 30/32=3) (20 28=2) (20 23 50 51=1)	///
				(-5/0=-10) (else=-9),  gen(temp_medu3)

replace medu3=temp_medu3 if (medu3==. | medu3<0) & (temp_medu3>0 & temp_medu3<.) 
replace medu3=temp_medu3 if temp_medu3>medu3 & (temp_medu3>0 & temp_medu3<.) 

	lab val medu3 edu3
	lab var medu3 "Mother's education: 3 levels"

	
* edu4
recode msedu (6/8=1)(1/2=2) (3 9=3)(4 5=3) (-5=-8)(-2=-3) (0=-1), gen(medu4)
recode mprofedu (26/27 30/32=4) (20 28=3) (50 51=1) (20 23=2)	///
				(-5/0=-10) (else=-9),  gen(temp_medu4)

replace medu4=temp_medu4 if (medu4==. | medu4<0) & (temp_medu4>0 & temp_medu4<.) 
replace medu4=temp_medu4 if temp_medu4>medu4 & (temp_medu4>0 & temp_medu4<.) 

	lab val medu4 edu4
	lab var medu4 "Mother's education: 4 levels"
	
drop temp_medu*
	
*** Fill MV based on other waves 
	foreach p in f m  			{    
	foreach e in 3 4  			{
	bysort pid: egen `p'temp`e'=max(`p'edu`e') 
	bysort pid (wave): replace `p'edu`e'=`p'temp`e' if (`p'edu`e'<0 | `p'edu`e'==.)  
	drop `p'temp`e'
	}
	}

*################################
*#								#
*#	    Ethnicity				#
*#								#
*################################	 
//Not available for SOEP

// okay, so this validates what I was thinking (this doesn't exist here)


*################################
*#								#
*#	Migration					#
*#								#
*################################	 

**--------------------------------------
**   COB respondent, father and mother
**--------------------------------------	
// NOTE:because of the extensive list of countries, a separate do-file generates the variables for the country of birth of the respondent and their parents categories by region. (see additional do-file for details)

// do "${Grd_syntax}/06_SOEP/ge_02add_labels_COB.do" 
// I am just adding here because I deleted a lot and I want all of this in one place

label define COB ///
0 "Born in Survey-Country" ///
1 "Oceania and Antarctica" ///
2 "North-West Europe" ///
3 "Southern and Eastern Europe" ///
4 "North Africa and the Middle East" ///
5 "South-East Asia" ///
6 "North-East Asia" ///
7 "Southern and Central Asia" ///
8 "Americas" ///
9 "Sub-Saharan Africa" ///
10 "Other" ///
-1 "MV general" ///
-2 "Item non-response" ///
-3 "Does not apply" ///
-8 "Question not asked in survey"

// If you want to add cob_f & cob_m, change the line below to"
// "foreach var in corigin forigin morigin {" 

foreach var in corigin   { // from ppathl: country_born_pl
	gen cob_`var'=.
			replace cob_`var'=1 if `var'==41 //Australia (includes External Territories)
		replace cob_`var'=1 if `var'==56 //New Zealand
		replace cob_`var'=1 if `var'==129 //Samoa
		replace cob_`var'=1 if `var'==137 //Micronesia
		replace cob_`var'=1 if `var'==182 //Fiji
		replace cob_`var'=2 if `var'==14 //UK
		replace cob_`var'=0 if `var'==1 //Germany
		replace cob_`var'=2 if `var'==10 //Austria
		replace cob_`var'=2 if `var'==11 //France
		replace cob_`var'=2 if `var'==13 //Denmark
		replace cob_`var'=2 if `var'==15 //Sweden
		replace cob_`var'=2 if `var'==16 //Norway
		replace cob_`var'=2 if `var'==17 //Finland
		replace cob_`var'=2 if `var'==19 //Switzerland
		replace cob_`var'=2 if `var'==70 //Iceland
		replace cob_`var'=2 if `var'==71 //Ireland
		replace cob_`var'=2 if `var'==12 //Benelux
		replace cob_`var'=2 if `var'==69 //Liechtenstein
		replace cob_`var'=2 if `var'==62 //Monaco
		replace cob_`var'=2 if `var'==7 //Former DDR
		replace cob_`var'=3 if `var'==4 //Greece
		replace cob_`var'=3 if `var'==5 //Italy
		replace cob_`var'=3 if `var'==6 //Spain
		replace cob_`var'=3 if `var'==21 //Romania
		replace cob_`var'=3 if `var'==22 //Poland
		replace cob_`var'=3 if `var'==26 //Hungary
		replace cob_`var'=3 if `var'==28 //Portugal
		replace cob_`var'=3 if `var'==29 //Bulgaria
		replace cob_`var'=3 if `var'==31 //Czech Republic
		replace cob_`var'=3 if `var'==101 //Estland
		replace cob_`var'=3 if `var'==146 //Lithuania
		replace cob_`var'=3 if `var'==140 //Kosovo-Albaner
		replace cob_`var'=3 if `var'==106 //Montenegro
		replace cob_`var'=3 if `var'==32 //Russia
		replace cob_`var'=3 if `var'==117 //Belgium
		replace cob_`var'=3 if `var'==118 //The Netherlands
		replace cob_`var'=3 if `var'==116 //Luxembourg
		replace cob_`var'=3 if `var'==73 //Moldova
		replace cob_`var'=3 if `var'==75 //Albania
		replace cob_`var'=3 if `var'==78 //Ukraine
		replace cob_`var'=3 if `var'==168 //Montenegro
		replace cob_`var'=3 if `var'==165 //Serbia
		replace cob_`var'=3 if `var'==103 //Latvia
		replace cob_`var'=3 if `var'==120 //Bosnia and Herzegovina
		replace cob_`var'=3 if `var'==122 //Slovenia
		replace cob_`var'=3 if `var'==123 //Slovakia
		replace cob_`var'=3 if `var'==121 //North Macedonia
		replace cob_`var'=3 if `var'==222 //Eastern Europe
		replace cob_`var'=3 if `var'==119 //Croatia
		replace cob_`var'=3 if `var'==112 //Malta
		replace cob_`var'=3 if `var'==3 //Former Yugoslavia
		replace cob_`var'=3 if `var'==58 //Cyprus
		replace cob_`var'=3 if `var'==153 //Free City of Danzig (now Poland)
		replace cob_`var'=3 if `var'==196 //Kosovo
		replace cob_`var'=3 if `var'==132 //Belarus
		replace cob_`var'=3 if `var'==180 //Bessarabia (Moldova)
		replace cob_`var'=3 if `var'==188 //Chechnya
		replace cob_`var'=4 if `var'==2 //Turkey
		replace cob_`var'=4 if `var'==30 //Syria
		replace cob_`var'=4 if `var'==81 //Egypt
		replace cob_`var'=4 if `var'==111 //Lybia
		replace cob_`var'=4 if `var'==67 //Marocco
		replace cob_`var'=4 if `var'==142 //Sudan
		replace cob_`var'=4 if `var'==52 //Tunisia
		replace cob_`var'=4 if `var'==161 //Bahrain
		replace cob_`var'=4 if `var'==24 //Iran
		replace cob_`var'=4 if `var'==60 //Iraq
		replace cob_`var'=4 if `var'==39 //Israel
		replace cob_`var'=4 if `var'==90 //Jordan
		replace cob_`var'=4 if `var'==126 //Kuwait
		replace cob_`var'=4 if `var'==76 //Lebanon
		replace cob_`var'=4 if `var'==136 //Oman
		replace cob_`var'=4 if `var'==46 //Saudi Arabia
		replace cob_`var'=4 if `var'==87 //United Arab Emirates
		replace cob_`var'=4 if `var'==151 //Yemen
		replace cob_`var'=4 if `var'==79 //Algeria
		replace cob_`var'=4 if `var'==193 //Qatar
		replace cob_`var'=4 if `var'==33 //Kurdistan (added)
		replace cob_`var'=4 if `var'==152 //Palestine
		replace cob_`var'=5 if `var'==181 //Myanmar
		replace cob_`var'=5 if `var'==169 //Cambodia
		replace cob_`var'=5 if `var'==100 //Laos
		replace cob_`var'=5 if `var'==44 //Thailand
		replace cob_`var'=5 if `var'==83 //Vietnam
		replace cob_`var'=5 if `var'==25 //Indonesia
		replace cob_`var'=5 if `var'==104 //Malaysia
		replace cob_`var'=5 if `var'==38 //Philippines
		replace cob_`var'=5 if `var'==93 //Singapore
		replace cob_`var'=5 if `var'==160 //Timor-Leste
		replace cob_`var'=5 if `var'==128 //Malaysia
		replace cob_`var'=6 if `var'==23 //Korea
		replace cob_`var'=6 if `var'==40 //Japan
		replace cob_`var'=6 if `var'==68 //China
		replace cob_`var'=6 if `var'==63 //Hong Kong
		replace cob_`var'=6 if `var'==145 //Mongolia
		replace cob_`var'=6 if `var'==154 //Taiwan
		replace cob_`var'=7 if `var'==50 //Bangladesh
		replace cob_`var'=7 if `var'==74 //Kazakhstan
		replace cob_`var'=7 if `var'==82 //Tajikistan
		replace cob_`var'=7 if `var'==85 //Pakistan
		replace cob_`var'=7 if `var'==97 //Uzbekistan
		replace cob_`var'=7 if `var'==43 //Afghanistan
		replace cob_`var'=7 if `var'==66 //Nepal
		replace cob_`var'=7 if `var'==65 //Sri Lanka
		replace cob_`var'=7 if `var'==163 //Maldives
		replace cob_`var'=7 if `var'==155 //Turkmenistan
		replace cob_`var'=7 if `var'==91 //Turkmenistan-UdSSR
		replace cob_`var'=7 if `var'==148 //Armenia
		replace cob_`var'=7 if `var'==141 //Georgia
		replace cob_`var'=7 if `var'==77 //Kyrgyzstan
		replace cob_`var'=7 if `var'==42 //India
		replace cob_`var'=7 if `var'==177 //Bhutan
		replace cob_`var'=7 if `var'==130 //Azerbaijan
		replace cob_`var'=8 if `var'==18 //USA
		replace cob_`var'=8 if `var'==55 //Canada
		replace cob_`var'=8 if `var'==35 //Argentina
		replace cob_`var'=8 if `var'==48 //Colombia
		replace cob_`var'=8 if `var'==61 //Brazil
		replace cob_`var'=8 if `var'==64 //Peru
		replace cob_`var'=8 if `var'==96 //Ecuador
		replace cob_`var'=8 if `var'==99 //Puerto Rico
		replace cob_`var'=8 if `var'==115 //Trinidad and Tobago
		replace cob_`var'=8 if `var'==124 //Paraguay
		replace cob_`var'=8 if `var'==133 //Uruguay
		replace cob_`var'=8 if `var'==159 //Panama
		replace cob_`var'=8 if `var'==157 //Guatemala
		replace cob_`var'=8 if `var'==167 //Honduras
		replace cob_`var'=8 if `var'==88 //El Salvador
		replace cob_`var'=8 if `var'==92 //Costa Rica
		replace cob_`var'=8 if `var'==109 //Nicaragua
		replace cob_`var'=8 if `var'==134 //Bahamas
		replace cob_`var'=8 if `var'==27 //Bolivia
		replace cob_`var'=8 if `var'==34 //Mexico
		replace cob_`var'=8 if `var'==59 //Cuba
		replace cob_`var'=8 if `var'==72 //St Lucia
		replace cob_`var'=8 if `var'==170 //Surinam
		replace cob_`var'=8 if `var'==107 //Belize
		replace cob_`var'=8 if `var'==171 //Guiana
		replace cob_`var'=8 if `var'==108 //Dominican Republic
		replace cob_`var'=8 if `var'==45 //Jamaica
		replace cob_`var'=8 if `var'==51 //Venezuela
		replace cob_`var'=8 if `var'==20 //Chile
		replace cob_`var'=8 if `var'==175 //Grenada
		replace cob_`var'=8 if `var'==114 //Haiti
		replace cob_`var'=8 if `var'==164 //Hawaii (political specification)
		replace cob_`var'=9 if `var'==102 //Angola
		replace cob_`var'=9 if `var'==110 //Kenya
		replace cob_`var'=9 if `var'==113 //Botswana
		replace cob_`var'=9 if `var'==47 //Ethiopia
		replace cob_`var'=9 if `var'==89 //Eritrea
		replace cob_`var'=9 if `var'==84 //Somalia
		replace cob_`var'=9 if `var'==86 //South Africa
		replace cob_`var'=9 if `var'==37 //Benin
		replace cob_`var'=9 if `var'==49 //Ghana
		replace cob_`var'=9 if `var'==53 //Mauritius
		replace cob_`var'=9 if `var'==54 //Nigeria
		replace cob_`var'=9 if `var'==57 //Tanzania
		replace cob_`var'=9 if `var'==125 //Guinea
		replace cob_`var'=9 if `var'==105 //Namibia
		replace cob_`var'=9 if `var'==94 //Burkina Faso
		replace cob_`var'=9 if `var'==80 //Mozambique
		replace cob_`var'=9 if `var'==158 //Sierra Leone
		replace cob_`var'=9 if `var'==36 //Cabo Verde
		replace cob_`var'=9 if `var'==135 //Uganda
		replace cob_`var'=9 if `var'==174 //Madagascar
		replace cob_`var'=9 if `var'==173 //Zimbabwe
		replace cob_`var'=9 if `var'==166 //Gambia
		replace cob_`var'=9 if `var'==162 //Senegal
		replace cob_`var'=9 if `var'==138 //Mali
		replace cob_`var'=9 if `var'==139 //Cameroon
		replace cob_`var'=9 if `var'==150 //Liberia
		replace cob_`var'=9 if `var'==95 //Zambia
		replace cob_`var'=9 if `var'==143 //Congo
		replace cob_`var'=9 if `var'==144 //Togo
		replace cob_`var'=9 if `var'==176 //Lesotho
		replace cob_`var'=9 if `var'==178 //Rwanda
		replace cob_`var'=9 if `var'==183 //Niger
		replace cob_`var'=9 if `var'==190 //Djibouti
		replace cob_`var'=9 if `var'==179 //Malawi
		replace cob_`var'=9 if `var'==127 //Côte d'Ivoire
		replace cob_`var'=9 if `var'==147 //Chad
		replace cob_`var'=9 if `var'==131 //Seychelles
		replace cob_`var'=10 if `var'==333 //unspecified
		replace cob_`var'=10 if `var'==444 //unspecified EU
		replace cob_`var'=10 if `var'==999 //Ethnic Minority
		replace cob_`var'=10 if `var'==172 //Caucasus
		replace cob_`var'=10 if `var'==156 //Africa (unspecified)
		replace cob_`var'=10 if `var'==149 //Kurdistan
		replace cob_`var'=10 if `var'==98 //Stateless
	lab val cob_`var' COB
}

rename cob_corigin cob_r
//rename cob_forigin cob_f
//rename cob_morigin cob_m

*fill if german born (respondent)
replace cob_r=0 if germborn==1 // from ppathl: born_germany_pl
replace cob_r=10 if cob_r==. & germborn ==2

*** Identify valid COB and fill across waves   // so this is if it varies across wave? I guess it might not be consistent?
// let's explore this as I clean the data
sort pid wave 


*** Generate working variables
	gen cob_rt=cob_r
 

*** Generate valid stage 1 - mode across the waves (values 1-10)
	// It takes the value of the most common valid answer between 1 and 10 
	// If there is an equal number of 2 or more answers, it returns "." - filled in next steps
	bysort pid: egen mode_cob_rt=mode(cob_rt)
	
	
*** Generate valid stage 2 - first valid answer provided (values 0-9)
	// It takes the value of the first recorded answer between 0 and 9 (so ignores 10 "other")
	// These are used to fill COB in cases: 
	//	(a) equal number of 2 or more answers (remaining MV)
	//	(b) there is a valid answer other than 10 but the mode (stage 1) returns 10
	
	by pid (wave), sort: gen temp_first_cob_rt=cob_rt if ///
			sum(inrange(cob_rt, 0,9)) == 1 &      ///
			sum(inrange(cob_rt[_n - 1],0,9)) == 0 // identify 1st valid answer in range 1-9
	bysort pid: egen first_cob_rt=max(temp_first_cob_rt) // copy across waves within pid
	drop  temp_first_cob_rt

	
*** Fill the valid COB across waves
 	replace cob_r = mode_cob_rt // stage 1 - based on mode
	replace cob_r = first_cob_rt if cob_r==. & inrange(first_cob_rt, 0,9) // stage 2 - based on the first for MV
	replace cob_r = first_cob_rt if cob_r==10 & inrange(first_cob_rt, 1,9) // stage 2 - based on the first for 10'other'
	drop cob_rt
	 
		
rename cob_r cob
	
*specify some missing
replace cob=-2 if cob==. & corigin==-1 // non response


lab val cob  COB

**-------------------------------------------------
**   Migration Background (respondent)
**-------------------------------------------------

*migr - specifies if respondent foreign-born or not.
lab def migr ///
0 "Native-born" ///
1 "Foreign-born" ///
-1 "MV general" ///
-2 "Item non-response" ///
-3 "Does not apply" ///
-8 "Question not asked in survey"

recode cob (0=0) (1/10=1) (-1=-1) (-2=-2) (-3=-3) (-8=-8), gen(migr) // this is created just above
	
*################################
*#								#
*#	    Religion			 	#
*#								#
*################################

**--------------------------------------  
** Religiosity
**--------------------------------------
//NOTE: because we do not want to assume religious affiliation to be time-constant, missing values are not filled automatically across waves. 

lab def relig ///
0 "Not religious/Atheist/Agnostic" ///
1 "Religious" ///
-1 "MV general" ///
-2 "Item non-response" ///
-3 "Does not apply" ///
-8 "Question not asked in survey"

recode plh0258 (1/5=1) (6=0) (7/11=1) (-8 -5=-8), gen(relig) // from pl: religious_affiliation
// okay so this is literally just a yes / no. this is not about affiliation. I probably want to retain the base variable with affiliation

lab val relig relig

*specify if question not asked
replace relig=-8 if relig==. & !inlist(wavey, 1990, 1991, 1997, 2003, 2007, 2011, 2015, 2019)
