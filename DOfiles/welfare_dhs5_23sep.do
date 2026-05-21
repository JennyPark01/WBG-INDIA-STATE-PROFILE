
*** Working Folder Path *** 
  if c(username) == "wb608271" { 
global path_in "C:/Users/wb608271/WBG/Nandini Krishnan - Multidimensional _Poverty/Analysis/data_constructed/dhs_2019" 
global path_out "C:/Users/wb608271/WBG/Nandini Krishnan - Welfare/Non-monetary/Aug4"
global path_ado "C:/ado"
} 


use "$path_in/Intermediate_data/ind_dhs19-21_large.dta", clear 

*** Keep selected variables for global MPI estimation ***
keep hh_id ind_id ccty ccnum cty survey year subsample strata psu weight region district area relationship sex age agec7 agec4 marital hhsize  year_interview month_interview date_interview d_cm d_nutr d_satt d_educ d_elct d_wtr d_sani d_hsg d_ckfl d_asst hh_mortality_5y hh_nutrition_uw_st hh_child_atten hh_years_edu6 electricity water_mdg toilet_mdg housing_1 cooking_mdg hh_assets2 stunting

order hh_id ind_id ccty ccnum cty survey year subsample strata psu weight region district area relationship sex age agec7 agec4 marital hhsize year_interview month_interview date_interview d_cm d_nutr d_satt d_educ d_elct d_wtr d_sani d_hsg d_ckfl d_asst hh_mortality_5y hh_nutrition_uw_st hh_child_atten hh_years_edu6 electricity water_mdg toilet_mdg housing_1 cooking_mdg hh_assets2 stunting


*** Sort, compress and save data for estimation ***
sort ind_id
compress

egen district_id = group(region district)
save "$path_out/ind_dhs19-21_pov.dta", replace 


*****************

use "$path_out/ind_dhs19-21_pov.dta", clear
gen sample_weight = weight/1000000 

decode region, gen(statename)
decode district, gen(dist_name)
replace dist_name = proper(dist_name)

/*
replace statename="AN"		if statename=="Andaman & Nicobar Islands"
replace statename="AP"		if statename=="Andhra Pradesh" 
replace statename="AR"		if statename=="Arunachal Pradesh"	
replace statename="DN" 	if statename=="Dadra & Nagar Haveli and Daman & Diu"	
// replace statename="DD" 	if statename=="Daman & Diu"
replace statename="HP"		if statename=="Himachal Pradesh"	 
replace statename="JK"		if statename=="Jammu & Kashmir"	 
replace statename="MP"		if statename=="Madhya Pradesh"	 
replace statename="Pondicherry" if statename=="Puducherry"	 
replace statename="TN"		if statename=="Tamil Nadu"	 
replace statename="UP"		if statename=="Uttar Pradesh"	 
replace statename="WB"		if statename=="West Bengal"	 

*/

replace dist_name="YSR" if dist_name=="Y.S.R."

// keep if statename=="Bihar" | statename=="Chhattisgarh" | statename=="HP" | statename=="UP" | statename=="Rajasthan" | statename=="Karnataka" | statename=="Maharashtra"

// keep if statename=="Haryana"
levelsof statename, local(levels)
di `levels'
 
** RUN for STATE LEVEL and DISTRICT LEVEL separately 
****State-level: uncomment lines 1931, 1934-35
****District level: uncomment lines 1931, 1940-43

// foreach state of local levels {
	
 	/** State level**********
		preserve
		keep if statename=="`state'"
			
	***************************/
	
	/** District level*****
	levelsof dist_name if statename=="`state'", local(dname_list) 
	foreach dist of local dname_list {
		preserve	
		keep if statename=="`state'"
		keep if dist_name=="`dist'"
	***********************************/

gen country = "India" 
gen countrycode = "IND"  

	
********************************************************************************
*** List of the 10 indicators included in the MPI ***
********************************************************************************

gen edu_1 = hh_years_edu6
gen atten_1 = hh_child_atten
gen cm_1 = hh_mortality_5y
/* change countries with no child mortality 5 year to child mortality ever*/
gen nutri_1 = hh_nutrition_uw_st
gen elec_1 = electricity
gen toilet_1 = toilet_mdg
gen water_1 = water_mdg
gen house_1 = housing_1
gen fuel_1 = cooking_mdg
gen asset_1 = hh_assets2

global est_1 edu_1 atten_1 cm_1 nutri_1 elec_1 toilet_1 water_1 house_1 fuel_1 asset_1

********************************************************************************
*** List of sample without missing values ***
********************************************************************************

foreach j of numlist 1 {
gen sample_`j' = (edu_`j'!=. & atten_`j'!=. & cm_`j'!=. & nutri_`j'!=. & elec_`j'!=. & toilet_`j'!=. & water_`j'!=. & house_`j'!=. & fuel_`j'!=. & asset_`j'!=.)

replace sample_`j' = . if subsample==0
	/* Note: If the anthropometric data was collected from a subsample of the 
	total population that was sampled, then the final analysis only includes the 
	subsample population. */ 

*** Percentage sample after dropping missing values ***
sum sample_`j' [iw = sample_weight]
gen per_sample_weighted_`j' = r(mean)

sum sample_`j'
gen per_sample_`j' = r(mean)
}
***

********************************************************************************
*** Define deprivation matrix 'g0' 
*** which takes values 1 if individual is deprived in the particular 
*** indicator according to deprivation cutoff z as defined during step 2 ***
********************************************************************************
foreach j of numlist 1 {
foreach var in ${est_`j'} {  
	gen	g0`j'_`var' = 1 if `var'==0
	replace g0`j'_`var' = 0 if `var'==1
	}
}
	
*** Raw Headcount Ratios
foreach j of numlist 1 {
foreach var in ${est_`j'}   {  
	sum	g0`j'_`var' if sample_`j'==1 [iw = sample_weight]
	gen	raw`j'_`var' = r(mean)*100
	lab var raw`j'_`var'  "Raw Headcount: Percentage of people who are deprived in `var'"
	}
}
********************************************************************************
*** Define vector 'w' of dimensional and indicator weight ***
********************************************************************************
/*If survey lacks one or more indicators, weights need to be adjusted within /
each dimension such that each dimension weighs 1/3 and the indicator weights
add up to one (100%). CHECK COUNTRY FILE*/

foreach j of numlist 1 {
// DIMENSION EDUCATION 
foreach var in edu_`j' atten_`j' {
capture drop w`j'_`var' 
	gen w`j'_`var' = 1/6
	}

// DIMENSION HEALTH
foreach var in cm_`j' nutri_`j' {
	capture drop w`j'_`var'
	gen w`j'_`var' = 1/6
	}

// DIMENSION LIVING STANDARD
foreach var in elec_`j' toilet_`j' water_`j' house_`j' fuel_`j' asset_`j' {
	
	capture drop w`j'_`var'
	gen w`j'_`var' = 1/18
	}

}
********************************************************************************
*** Generate the weighted deprivation matrix 'w' * 'g0'
********************************************************************************

foreach j of numlist 1 {
foreach var in ${est_`j'} {
	gen	w`j'_g0_`var' = w`j'_`var' * g0`j'_`var' 
	replace w`j'_g0_`var' = . if sample_`j'!=1 
	/*The estimation is based only on observations that have non-missing values 
	for all variables in varlist_pov*/
	}
}
********************************************************************************
*** Generate the vector of individual weighted deprivation count 'c'
********************************************************************************

foreach j of numlist 1 {
egen	c_vector_`j' = rowtotal(w`j'_g0_*)
replace c_vector_`j' = . if sample_`j'!=1
*drop	w_g0_*
}

********************************************************************************
*** Identification step according to poverty cutoff k (20 33.33 50) ***
********************************************************************************

foreach j of numlist 1 {
	foreach k of numlist 20 33 50 {
		gen	multidimensionally_poor_`j'_`k' = (c_vector_`j'>=`k'/100)
		replace multidimensionally_poor_`j'_`k' = . if sample_`j'!=1 
		//Takes value 1 if individual is multidimensional poor
	}
}


*** Headcount (H) ***
foreach j of numlist 1 {
foreach k in 20 33 50 {
		sum	multidimensionally_poor_`j'_`k' [iw = sample_weight] if sample_`j'==1
	gen	H_`j'_`k' = r(mean)*100
	lab var H_`j'_`k' "k=`k' Headcount ratio: % Population in multidimensional poverty (H)"
	
}
}


* mpi score for each hh member - convert to per household var
* bysort hh_id: keep if _n == 1


save "$path_out/mpi2019data.dta", replace

use "$path_out/mpi2019data.dta", clear

***************************************



********************************************************************************
*** PART 2 : GET KEY VARIABLES
********************************************************************************

use "${path_in}\Intermediate_data\ind_dhs19-21_large.dta", clear

keep hvidx hv000 hv001 hv002 hv003 hv004 hv005 hv006 hv009 hv024 hv025 hv217 hv219 hv220 hv244 hv270 hv271 hv270a hv106 hv107 hv108 hv109 hv244 sh62 hv101 sh61

gen double hh_id = hv001*10000 + hv002 
format hh_id %20.0g


clonevar relationship = hv101 
codebook relationship, tab (20)
recode relationship (1=1)(2=2)(3=3)(11=3)(4/10=4)(15/16=4)(12=5)(17=6)(98=.)
*label define lab_rel 1"head" 2"spouse" 3"child" 4"extended family" 5"not related" 6"maid"
label values relationship lab_rel
label var relationship "Relationship to the head of household"
tab hv101 relationship, miss

* Tag if head exists in household
gen byte is_head = (relationship == 1)
bysort hh_id (is_head): gen byte head_exists = sum(is_head)
bysort hh_id: replace head_exists = head_exists[_N]

* Create a filter variable: keep head if present, else one spouse
gen byte keep_obs = 0
bysort hh_id (relationship): replace keep_obs = 1 if relationship == 1
bysort hh_id (relationship): replace keep_obs = 1 if head_exists == 0 & relationship == 2 & _n == 1

* Keep only selected observations
keep if keep_obs == 1

* check if there are more than one keep_obs in hhid
bysort hh_id (keep_obs): gen n_keep = sum(keep_obs)
bysort hh_id: replace n_keep = n_keep[_N]
list hh_id if n_keep > 1



tempfile hh2019
save "$path_out\hhsubset2019.dta", replace
use "$path_out\hhsubset2019.dta", clear

merge 1:m hh_id using "$path_out\mpi2019data.dta" // 635,864 matched, 325 unmatched - coz of missing demographic variables

save "$path_out\hhsubsetmerge1.dta", replace

use "$path_out\hhsubsetmerge1.dta", clear

*************************************************************************************************
*** PART 3: Create HH demography variables
*******************************************************************************************

* Education of HH head
gen edu_group = .
replace edu_group = 1 if hv106 == 0
replace edu_group = 2 if hv106 == 1
replace edu_group = 3 if hv106 == 2
replace edu_group = 4 if hv106 == 3


label define edu_group_lbl 1 "No education / Preschool" ///
                          2 "Primary" ///
                          3 "Secondary" ///
                          4 "Higher" ///
              
label values edu_group edu_group_lbl

* Age of HH head
gen head_age = hv220
gen head_age_cat = .
replace head_age_cat = 1 if inrange(head_age, 15, 29)
replace head_age_cat = 2 if inrange(head_age, 30, 45)
replace head_age_cat = 3 if inrange(head_age, 46, 64)
replace head_age_cat = 4 if inrange(head_age, 65, 150)

label define agecat 1 "15-29" 2 "30–45" 3 "46–64" 4 "65+"
label values head_age_cat agecat


* Gender of HH head
gen head_gender = hv219
replace head_gender = 1 if head_gender == 3

* Household Size
gen hhsize_cat = .
replace hhsize_cat = 1 if hhsize == 1
replace hhsize_cat = 2 if inrange(hhsize, 2, 3)
replace hhsize_cat = 3 if inrange(hhsize, 4, 6)
replace hhsize_cat = 4 if inrange(hhsize, 7, 100)

label define hhsize_lbl 1 "1" 2 "2-3" 3 "4-6" 4 "7+"
label values hhsize_cat hhsize_lbl

* House Ownership
gen own_house = sh61 == 1 // does this household own this house or any other house?

* Agricultural Land Ownership 
gen agri_land = hv244 == 1 // owns land usable for agriculture

tempfile mpi2019_welfare
save "$path_out\hhsubsetmpi2019_all.dta", replace

use "$path_out\hhsubsetmpi2019_all.dta", clear

gen sector = area == 1 if !missing(area)
* encode hv024, gen(state_numeric)
decode(region), gen (state) 

gen str_state = ""
label variable state "State Code"


*********************************************************************************
*** SECTION 1 -  MPI 
*********************************************************************************	

/*

collapse (mean) multidimensionally_poor_1_33 ///         /* H = pop share */
                 multidimensionally_poor_1_50 ///
                 [aw = sample_weight] 

gen     level = "National"
gen     area  = "All"
gen     dem   = "All Households"                         // label only
order   level area dem 

export excel using ///
    "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2019-total.xlsx", ///
    sheet("allindia_pop") sheetreplace firstrow(variables)

*/

/***********************************************************************
  EXPORT POPULATION–WEIGHTED MPI (PERSON LEVEL)
  ----------------------------------------------------------------------
  • Run this chunk immediately after all m_poor_* variables are created
    and BEFORE you collapse to one record per household.
  • Dataset must still contain one line per person.
  • Keeps only fully-observed cases (sample_1 == 1).
***********************************************************************/
keep if sample_1 == 1                     // complete-case sample
* svyset [pw = sample_weight]             // optional, if you later need SEs


gen m_poor_1_20 =  multidimensionally_poor_1_20
gen m_poor_1_33 =  multidimensionally_poor_1_33
gen m_poor_1_50 =  multidimensionally_poor_1_50

* =============== 1. TOTAL – ALL HOUSEHOLDS ===========================
local out "$path_out\Output\pov-2019-total.xlsx"


* All-India
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight]
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "All Households"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

* All-India, Urban / Rural
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "All Households"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

* State
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "All Households"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

* State × Sector
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "All Households"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


* =============== 2. EDUCATION OF HH HEAD ============================
local out "$path_out\Output\pov-2019-edu.xlsx"

* All-India
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(edu_group)
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "Education"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

* All-India × Sector
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector edu_group)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "Education"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

* State
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state edu_group)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "Education"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

* State × Sector
preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector edu_group)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "Education"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


* =============== 3. AGE OF HH HEAD ==================================
local out "$path_out\Output\pov-2019-age.xlsx"

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(head_age_cat)
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "Age"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector head_age_cat)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "Age"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state head_age_cat)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "Age"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector head_age_cat)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "Age"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


* =============== 4. GENDER OF HH HEAD ===============================
local out "$path_out\Output\pov-2019-gen.xlsx"

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(head_gender)
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "Gender"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector head_gender)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "Gender"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state head_gender)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "Gender"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector head_gender)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "Gender"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


* =============== 5. HH SIZE =========================================
local out "$path_out\Output\pov-2019-hhsize.xlsx"

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(hhsize_cat)
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "HH Size"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector hhsize_cat)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "HH Size"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state hhsize_cat)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "HH Size"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector hhsize_cat)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "HH Size"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


* =============== 6. HOUSE OWNERSHIP =================================
local out "$path_out\Output\pov-2019-ownhouse.xlsx"

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(own_house)
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "Owns House"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

preserve
collapse (mean)  m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector own_house)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "Owns House"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state own_house)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "Owns House"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

preserve
collapse (mean) m_poor_1_20 m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector own_house)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "Owns House"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


* =============== 7. AGRICULTURAL LAND ===============================
local out "$path_out\Output\pov-2019-agriland.xlsx"

preserve
collapse (mean) m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(agri_land)
gen round = "2019"
gen level = "National"
gen area  = "All"
gen dem   = "Agri Land"
export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
restore

preserve
collapse (mean) m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(sector agri_land)
gen round = "2019"
gen level = "National"
gen area  = "urban/rural"
gen dem   = "Agri Land"
export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
restore

preserve
collapse (mean) m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state agri_land)
gen round = "2019"
gen level = "State"
gen area  = "All"
gen dem   = "Agri Land"
export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
restore

preserve
collapse (mean) m_poor_1_33 m_poor_1_50 [aw = sample_weight], by(state sector agri_land)
gen round = "2019"
gen level = "State"
gen area  = "urban/rural"
gen dem   = "Agri Land"
export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
restore


*********************************************************************************
*** SECTION 2 -  WEALTH INDEX
*********************************************************************************	


gen wealth_index = hv270
tabulate wealth_index, generate(wi_cat)

* Total - All households
local out "$path_out\Output\wealth-2019-total.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by (m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore



* Education of Household head
local out "$path_out\Output\wealth-2019-edu.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore

* Age of Household head
local out "$path_out\Output\wealth-2019-age.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
*Gender
local out "$path_out\Output\wealth-2019-gen.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
* HH size
local out "$path_out\Output\wealth-2019-hhsize.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
* House ownership
local out "$path_out\Output\wealth-2019-ownhouse.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(own_house m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector own_house m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state own_house m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector own_house m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	

	
* Agricultural Land Ownership
local out "$path_out\Output\wealth-2019-agriland.xlsx"

	* All India level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(sector agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) wi_cat* [aw=sample_weight], by(state sector agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
*****************************************************************************************
** SECTION 3: COMPONENTS
*****************************************************************************************

* Total Households
local out "$path_out\Output\components-2019-total.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by (m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore



* Total Households - 2
local out "$path_out\Output\components-2019-total-all-2.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight]
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore

	
	
	
* Education of HH head
local out "$path_out\Output\components-2019-edu.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector edu_group m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore

* Age of Household head
local out "$path_out\Output\components-2019-age.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector head_age_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
*Gender of Household head
local out "$path_out\Output\components-2019-gen.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector head_gender m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
* HH size
local out "$path_out\Output\components-2019-hhsize.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector hhsize_cat m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
* House ownership
local out "$path_out\Output\components-2019-ownhouse.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(own_house m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector own_house m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state own_house m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector own_house m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
		
* Agri Land
local out "$path_out\Output\components-2019-agriland.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "All"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "All"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector agri_land m_poor_1_33)
	gen round = "2019"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Agri Land"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	