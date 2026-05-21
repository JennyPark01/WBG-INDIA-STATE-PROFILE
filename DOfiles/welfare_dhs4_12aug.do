
/********************************************************************************

# NON MONETARY POVERTY - NFHS 2015

**********************************************************************************/

clear all 
set more off
set maxvar 10000
global year 2015-16
global y 2015


*** Working Folder Path *** 
  if c(username) == "wb608271" { 
global path_in "C:/Users/wb608271/WBG/Nandini Krishnan - Multidimensional _Poverty/Analysis/data_constructed/dhs_$y" 
global path_out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary"
global path_ado "C:/ado"
} 


use "C:\Users\wb608271\WBG\Nandini Krishnan - Intermediate_data\ind_dhs15-16_large.dta", clear

keep sh46 hhid hvidx hv000 hv001 hv002 hv003 hv004 hv005 hv009 hv024 hv025 hv219 hv220 hv101 relationship hv270 hv271 hv106 shphase hv107 hv108

gen double hh_id = hv001*10000 + hv002 
format hh_id %20.0g

*keep if hv101 == 1 // Relationship to head is self

tempfile hh2015
save "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\hhsubset2015.dta", replace

*************************************************************************************************
*Create demography variables
*******************************************************************************************

* Education
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


* Age
gen head_age = hv220
gen head_age_cat = .
replace head_age_cat = 1 if inrange(head_age, 15, 29)
replace head_age_cat = 2 if inrange(head_age, 30, 45)
replace head_age_cat = 3 if inrange(head_age, 46, 64)
replace head_age_cat = 4 if inrange(head_age, 65, 150)

label define agecat 1 "15–29" 2 "30–45" 3 "46–64" 4 "65+"
label values head_age_cat agecat

* Gender
gen head_gender = hv219

* Household Size
gen hhsize = hv009
gen hhsize_cat = .
replace hhsize_cat = 1 if hhsize == 1
replace hhsize_cat = 2 if inrange(hhsize, 2, 3)
replace hhsize_cat = 3 if inrange(hhsize, 4, 6)
replace hhsize_cat = 4 if inrange(hhsize, 7, 100) 

label define hhsize_lbl 1 "1" 2 "2-3" 3 "4-6" 4 "7+"
label values hhsize_cat hhsize_lbl

* House Ownership
gen own_house = sh46 >= 1

* merge with first part of welfare merge
merge m:1 hh_id using "C:/Users/wb608271/WBG/Nandini Krishnan - Schemes/Non-monetary (NFHS)/2015/mpi-poor/part1-mpiscore_2015-16.dta" // see below
drop _merge
save "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\hhsubsetmpi2015_all.dta", replace

use "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\hhsubsetmpi2015_all.dta", clear

/*************************************************************************************
use "$path_in/Intermediate_data/ind_dhs15-16_pov.dta", clear
gen sample_weight = weight/1000000 

decode region, gen(statename)
decode district, gen(dist_name)
replace dist_name = proper(dist_name)

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

replace dist_name="YSR" if dist_name=="Y.S.R."


	
********************************************************************************
*** List of the 10 indicators included in the MPI ***
********************************************************************************

gen edu_1 = hh_years_edu6
gen atten_1 = hh_child_atten
gen cm_1 = hh_mortality_5y
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
*** Generate the complement vector of individual weighted deprivation count 'c'
********************************************************************************

gen score_var = 1-c_vector_1
replace score_var = 0 if c_vector_1 > 1 & c_vector_1 !=. // some odd reason 2 observations with c_vector_1 are 1 but dont show up with == 1 - puzzle to solve later
*Score var variable - 1 is the bottom 20% and 5 is the top 20%.

replace c_vector_1 = 1 if c_vector_1 > 1 & c_vector_1 !=.


****************************
* Identification step according to poverty cutoff k (20 33.33 50) *
****************************

foreach j of numlist 1 {
	foreach k of numlist 20 33 50 {
		gen	m_poor_`j'_`k' = (c_vector_`j'>=`k'/100)
		replace m_poor_`j'_`k' = . if sample_`j'!=1 
		//Takes value 1 if individual is multidimensional poor
	}
}

* mpi score for each hh member - convert to per household var
bysort hh_id: keep if _n == 1

tempfile hhmpiscore
save "$path_out/part1-mpiscore_2015-16.dta", replace


*******************************************************************************************/

gen wealth_index = hv270
tabulate wealth_index, generate(wi_cat)

gen sector = area == 1 if !missing(area)
* encode hv024, gen(state_numeric)

* Create readable label
decode hv024, gen(state2)

gen state = proper(state2)

replace state = "Dadra and Nagar Haveli & Daman and Diu" if ///
    inlist(state, "Dadra And Nagar Haveli", "Daman And Diu")




/********************************************************************************
 GRAPH 1 AND 2:  POVERTY AND WEALTH INDEX
 ********************************************************************************/
 
* TOTAL - All Households
local out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2015-total.xlsx"

	* All India level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20  [aw=sample_weight]
	gen round  = "2015"
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(sector)
	gen round  = "2015"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state)
	gen round  = "2015"
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20  [aw=sample_weight], by(state sector)
	gen round  = "2015"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore



* Education of HH head

local out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2015-edu.xlsx"

	* All India level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(edu_group)
	gen round  = "2015"
	gen level = "National"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(sector edu_group)
	gen round  = "2015"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state edu_group)
	gen round  = "2015"
	gen level = "State"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state sector edu_group)
	gen round  = "2015"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore

* Age of Household head
local out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2015-age.xlsx"

	* All India level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(head_age_cat)
	gen round  = "2015"
	gen level = "National"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(sector head_age_cat)
	gen round  = "2015"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state head_age_cat)
	gen round  = "2015"
	gen level = "State"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state sector head_age_cat)
	gen round  = "2015"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
* Gender of HH head
local out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2015-gen.xlsx"

	* All India level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(head_gender)
	gen round  = "2015"
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(sector head_gender)
	gen round  = "2015"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state head_gender)
	gen round  = "2015"
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state sector head_gender)
	gen round  = "2015"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
* HH size
local out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2015-hhsize.xlsx"

	* All India level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(hhsize_cat)
	gen round  = "2015"
	gen level = "National"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(sector hhsize_cat)
	gen round  = "2015"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state hhsize_cat)
	gen round  = "2015"
	gen level = "State"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state sector hhsize_cat)
	gen round  = "2015"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
* House ownership
local out "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\Non-monetary\Aug4\Output\pov-2015-ownhouse.xlsx"

	* All India level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(own_house)
	gen round  = "2015"
	gen level = "National"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(sector own_house)
	gen round  = "2015"
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state own_house)
	gen round  = "2015"
	gen level = "State"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) m_poor_1_33 m_poor_1_50 m_poor_1_20 [aw=sample_weight], by(state sector own_house)
	gen round  = "2015"
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore	
	
	/************************************************************************************
	
setwd("C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/NFHS-Nonmonetary/2015")

read_and_tag <- function(path) {
  sheet_names <- excel_sheets(path)
  bind_rows(lapply(sheet_names, function(sheet) {
    read_excel(path, sheet = sheet) %>% mutate(SourceSheet = sheet)
  }))
}

merged_data_total <- read_and_tag("pov-2015-total.xlsx")
merged_data_edu <- read_and_tag("pov-2015-edu.xlsx")
merged_data_gender <- read_and_tag("pov-2015-gen.xlsx")
merged_data_age <- read_and_tag("pov-2015-age.xlsx")
merged_data_hhsize <- read_and_tag("pov-2015-hhsize.xlsx")
merged_data_land <- read_and_tag("pov-2015-ownhouse.xlsx")

nfhs_pov_wealth_2015 <- bind_rows(merged_data_total, merged_data_age, merged_data_edu, merged_data_gender, merged_data_hhsize, merged_data_land)

write.csv(nfhs_pov_wealth_2015, "nfhs_pov_wealth_2015.csv")

	**********************************************************************************/
	
	
/********************************************************************************
 GRAPH 3:  SOURCES OF MPI
 ********************************************************************************/
 
* TOTAL - All households

local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\NFHS-Nonmonetary\2015\components-2015-total.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by (m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore



* Education of Household head
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\NFHS-Nonmonetary\2015\components-2015-edu.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(edu_group m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector edu_group m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state edu_group m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector edu_group m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore

* Age of Household head

local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\NFHS-Nonmonetary\2015\components-2015-age.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(head_age_cat m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector head_age_cat m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state head_age_cat m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector head_age_cat m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
* Gender of Household Head
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\NFHS-Nonmonetary\2015\components-2015-gen.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(head_gender m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector head_gender m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state head_gender m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector head_gender m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
* Household Size
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\NFHS-Nonmonetary\2015\components-2015-hhsize.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(hhsize_cat m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector hhsize_cat m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state hhsize_cat m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector hhsize_cat m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
	
* House ownership

local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\NFHS-Nonmonetary\2015\components-2015-ownhouse.xlsx"

	* All India level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(own_house m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore

	* All India sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(sector own_house m_poor_1_33)
	gen round = 4
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("indiasector") sheetreplace
	restore

	* State level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state own_house m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "All"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore

	* State-sector level
	preserve
	collapse (mean) g01_* [aw=sample_weight], by(state sector own_house m_poor_1_33)
	gen round = 4
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns House"
	export excel using "`out'", firstrow(variables) sheet("statesector") sheetreplace
	restore
	
 
 /*********************************************************************************************
 
setwd("C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/NFHS-Nonmonetary/2015")

read_and_tag <- function(path) {
  sheet_names <- excel_sheets(path)
  bind_rows(lapply(sheet_names, function(sheet) {
    read_excel(path, sheet = sheet) %>% mutate(SourceSheet = sheet)
  }))
}

merged_data_total   <- read_and_tag("components-2015-total.xlsx")
merged_data_edu     <- read_and_tag("components-2015-edu.xlsx")
merged_data_gender  <- read_and_tag("components-2015-gen.xlsx")
merged_data_age     <- read_and_tag("components-2015-age.xlsx")
merged_data_hhsize  <- read_and_tag("components-2015-hhsize.xlsx")
merged_data_land    <- read_and_tag("components-2015-ownhouse.xlsx")

nfhs_components_2015 <- bind_rows(
  merged_data_total, merged_data_age, merged_data_edu,
  merged_data_gender, merged_data_hhsize, merged_data_land
)

write.csv(nfhs_components_2015, "nfhs_components_2015.csv", row.names = FALSE)

 
 ************************************************************************************************/