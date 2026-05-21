/* =============================================================================
Project: Data for state dashboard 
Dofile: Welfare data HCES 2022 data compilation
Author: Vishali Sairam
Last modified:	7 July 2025
*/
*** Working Folder Path *** 
  if c(username) == "wb608271" { 
global path_in "C:\Users\wb608271\WBG\Nishtha Kochhar - 2022-23\dta" 
global path_out "C:/Users/wb608271/WBG/Nandini Krishnan - Welfare"
global path_ado "C:/ado"
} 

*******************************************************************************************
* Open 102 HCQ data
********************************************************************************************
use "${path_in}\102_hcq.dta", clear
destring year, replace

bysort hhid: gen hhsize2 = _N // using hhsize from pov dataset

gen is_head = relation_to_head == 1 // N = 261746, same as number of HHs
* quietly egen tag = tag(hhid)
* count if tag == 1


* Gender HH head
bysort hhid (is_head): gen head_gender = sex if is_head

* Age category - HH head /// change to 15-29
gen head_age = age if is_head == 1
destring head_age, replace ignore(".,") force
gen head_age_cat = .
replace head_age_cat = 1 if inrange(head_age, 15, 29) // there are approx 170 observations with head age is 14. These will be dropped out of the data. Nishtha vetoed this.
replace head_age_cat = 2 if inrange(head_age, 30, 45)
replace head_age_cat = 3 if inrange(head_age, 46, 64)
replace head_age_cat = 4 if inrange(head_age, 65, 150)

label define agecat 1 "15-29" 2 "30–45" 3 "46–64" 4 "65+"
label values head_age_cat agecat


* Education category - HH head
gen head_educ = highest_edu if is_head == 1
gen edu_group = .
replace edu_group = 1 if inlist(highest_edu, 1, 2, 3)
replace edu_group = 2 if highest_edu == 4
replace edu_group = 3 if highest_edu == 5
replace edu_group = 4 if inlist(highest_edu, 6, 7)
replace edu_group = 5 if inlist(highest_edu, 8, 10, 11, 12)
replace edu_group = 6 if highest_edu == 13 

label define edu_group_lbl 1 "No education / Below primary" ///
                          2 "Primary completed" ///
                          3 "Middle completed" ///
                          4 "Secondary/Sr Secondary" /// 
                          5 "Diploma / Graduate" ///
                          6 "Post Graduate and above"
label values edu_group edu_group_lbl

* Subset to only household head obs and save data
preserve
keep if is_head == 1 
tempfile hhdata
save "${path_out}\hhhead-102.dta", replace
restore


*******************************************************************************************
* Open 103 HCQ data
********************************************************************************************

use "${path_in}\103_hcq.dta", clear
destring year, replace

* Obtain Land Ownership variable

gen owns_land = land_ownership == 1
tab owns_land

* Dwelling unit variable 

gen owns_dwelling = typeof_dwelling == 1
tab owns_dwelling

* Merge 103 and 102 data

merge 1:1 hhid using "${path_out}\hhhead-102.dta"
tempfile hcqdata
drop _merge
save "${path_out}\hcqdata.dta", replace 

* N = 261,746

********************************************************************************************
* Get PPP vars
******************************************************************************************

*** merging with 2017 data
merge 1:1 hhid using "${path_out}\welfare_pov_final.dta"
tempfile merge_all
drop _merge
save "${path_out}\hhmerge-hces.dta", replace


*** merging with 2021 data
use "C:\Users\wb608271\Downloads\welfare_pov_final_ppp21.dta", replace
merge 1:1 hhid using "${path_out}\hhmerge-hces.dta"
drop _merge
save "${path_out}\hhmerge-hces_all.dta", replace


******************************************************************************************
*** Obtain PPP and demographic estimates
******************************************************************************************

use "${path_out}\hhmerge-hces_all.dta", clear

* Subset 2022 dataset
keep if year == 2022

* Household size category 
gen hhsize_grp = .
replace hhsize_grp = 1 if hhsize == 1
replace hhsize_grp = 2 if inrange(hhsize, 2, 3)
replace hhsize_grp = 3 if inrange(hhsize, 4, 6)
replace hhsize_grp = 4 if inrange(hhsize, 7, 100)

label define hhsize_lbl 1 "1" 2 "2-3" 3 "4-6" 4 "7+"
label values hhsize_grp hhsize_lbl

*gen sector2 = .
*replace sector2 = 0 if sector == 1 // rural
*replace sector2 = 1 if sector == 2 // urban

replace head_gender = 1 if head_gender == 3

tabulate head_age_cat, generate(age_cat)
tabulate edu_group, generate(edu_cat)
tabulate hhsize_grp, generate(hhsize_cat)
tabulate head_gender, generate (gender_cat)


* Save file
tempfile hhmergealldem
save "${path_out}\hhmerge-hces-alldem.dta", replace


use "${path_out}\hhmerge-hces-alldem.dta", clear
***************************************************************************************
***Sources of Income
***********************************************************************************
***** Construction of income sources

gen inc_selfemp = inlist(majorincome_selfemployed, 1, 2) | (maxincome_activity == 1)
gen inc_salaried = inlist(majorincome_salaried, 3, 4) | (maxincome_activity == 2)
gen inc_casual = inlist(majorincome_casual, 5, 6) | (maxincome_activity == 3)

save "${path_out}\hhmerge-hces-alldem-income.dta", replace

save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2011\hces2022.dta", replace
	

/**** Sources of Income (Alternate Construction) (JUST IN CASE - FOR LATER - DO NOT RUN NOW)*****************
gen inc_selfemp_agri = ( majorincome_selfemployed == 1)
	gen inc_selfemp_nonagri = ( majorincome_selfemployed == 2)
	gen inc_salaried_agri = ( majorincome_salaried == 3 )
	gen inc_salaried_nonagri = ( majorincome_salaried == 4)
	gen inc_casual_agri = ( majorincome_casual == 5)
	gen inc_casual_nonagri = ( majorincome_casual == 6)

	lab var inc_selfemp_agri "Income SelfEmployment Agri"
	lab var inc_selfemp_nonagri "Income SelfEmployment Non Agriculture"
	lab var inc_salaried_agri "Income Salaried Agriculture"
	lab var inc_salaried_nonagri "Income Salaried Non Agriculture"
	lab var inc_casual_agri "Income Casual Agriculture"
	lab var inc_casual_nonagri "Income Casual Non Agriculture"


	egen total_flags = rowtotal(inc_selfemp_agri inc_selfemp_nonagri inc_casual_agri inc_casual_nonagri inc_salaried_agri inc_salaried_nonagri)
	tab total_flags

***************************************************************************************************************/


********************************************************************************************************************************************
* 2022 poverty data construction
**************************************************************************************************
use "${path_out}\hhmerge-hces-alldem-income_2022.dta", clear

**# POVERTY RATES

* Total
* Define the output file path
	local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_total.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt]
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore

	** Education levels
	
	local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_edu.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (edu_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 edu_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state edu_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state edu_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore
	
**Gender

local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_gender.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (gender_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 gender_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state gender_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state gender_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


* Age


local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_age.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (age_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 age_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state age_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state age_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Age"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


* HH size
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_hhsize.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (hhsize_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 hhsize_cat*)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state hhsize_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state hhsize_cat*)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


* Owns Dwelling
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_dwelling.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (owns_dwelling)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 owns_dwelling)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state owns_dwelling)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state owns_dwelling)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore
	
	
	
	

* Landownership
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_2022_land.xlsx"

	preserve
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (owns_land)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 owns_land)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (state owns_land)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) poor_215 poor_365 poor_685 poor_300 poor_420 poor_830 [aw = pwt], by (sector2 state owns_land)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


	
	/*******************************************************************************

library(readxl)
library(dplyr)


read_and_tag <- function(path) {
  sheet_names <- excel_sheets(path)
  bind_rows(lapply(sheet_names, function(sheet) {
    read_excel(path, sheet = sheet) %>% mutate(SourceSheet = sheet)
  }))
}

setwd("C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/HCES-Monetary/2022")

merged_data_all <- read_and_tag("hces_2022_total.xlsx")
merged_data_gender <- read_and_tag("hces_2022_gender.xlsx")
merged_data_edu <- read_and_tag("hces_2022_edu.xlsx")
merged_data_age <- read_and_tag("hces_2022_age.xlsx")
merged_data_hhsize <- read_and_tag("hces_2022_hhsize.xlsx")
merged_data_dwelling <- read_and_tag("hces_2022_dwelling.xlsx")
merged_data_land <- read_and_tag("hces_2022_land.xlsx")


hces2022_pov <- bind_rows(merged_data_all, merged_data_age, merged_data_edu, merged_data_gender, merged_data_hhsize, merged_data_dwelling, merged_data_land)

write.csv(hces2022_pov, "hces2022_pov.csv")


	*******************************************************************************************************/
	
	
***************************************************************************************************************
*** INCOME DATA
************************************************************************************************************

use "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\hhmerge-hces-alldem-income_2022.dta", clear
drop if missing(poor_420)

* Total
* Define the output file path
	local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_total.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "All Households"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


	** Education levels
	
	local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_educ.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (edu_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 edu_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state edu_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state edu_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Education"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore
	
**Gender

local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_gender.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (gender_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 gender_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state gender_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state gender_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


* Age
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_age.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (age_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 age_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state age_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state age_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Gender"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


* HH size
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_hhsize.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (hhsize_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_*[aw = pwt], by (sector2 hhsize_cat* poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state hhsize_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state hhsize_cat* poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "HH Size"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore
	
	
* Owns Dwelling
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_dwelling.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (owns_dwelling poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_*[aw = pwt], by (sector2 owns_dwelling poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state owns_dwelling poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state owns_dwelling poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns Dwelling"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore


* Landownership
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_output\HCES-Monetary\2022\hces_income_2022_land.xlsx"

	preserve
	collapse (mean) inc_* [aw = pwt], by (owns_land poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_*[aw = pwt], by (sector2 owns_land poor_420)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("sect_allindia") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (state owns_land poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("state") sheetreplace
	restore
	
	preserve
	drop if missing(sector2)
	collapse (mean) inc_* [aw = pwt], by (sector2 state owns_land poor_420)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns Land"
	export excel using "`out'", firstrow(variables) sheet("sector_state") sheetreplace
	restore

	
	/*****************************************************
  
setwd("C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/HCES-Monetary/2022")

merged_data_all <- read_and_tag("hces_income_2022_total.xlsx")
merged_data_gender <- read_and_tag("hces_income_2022_gender.xlsx")
merged_data_edu <- read_and_tag("hces_income_2022_educ.xlsx")
merged_data_age <- read_and_tag("hces_income_2022_age.xlsx")
merged_data_hhsize <- read_and_tag("hces_income_2022_hhsize.xlsx")
merged_data_dwelling <- read_and_tag("hces_income_2022_dwelling.xlsx")
merged_data_land <- read_and_tag("hces_income_2022_land.xlsx")


hces2022_income <- bind_rows(merged_data_all, merged_data_age, merged_data_edu, merged_data_gender, merged_data_hhsize, merged_data_dwelling, merged_data_land)

write.csv(hces2022_income, "hces2022_income.csv")

*****************************************************************************************/
	
	
**********************************************************************************
* ------------------ WELFAREAGG AGGREGATES -------------------------------------
******************************************************************************************	


keep if welfare_agg21_sp != . //not including HHs without welfare aggregates


preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_all_2022.xlsx", replace
restore 


*** TOTAL DEMOGRAPHICS - All Households
	
xtile nat_quint = welfare_agg21_sp [aw=pwt], nq(5)
lab var nat_quint "Quintiles national"


gen sec_quint = .
lab var sec_quint "Quintiles national + sector "
foreach sec in 0 1 {
        xtile temp_q = welfare_agg21_sp if sector2== `sec' [aw=pwt], nq(5)
        replace sec_quint = temp_q if sector2== `sec'
        drop temp_q
		
}

gen state_quint = .
lab var state_quint "Quintiles within state"
levelsof state, local(states)
foreach s of local states {
        xtile temp_q = welfare_agg21_sp if state==`s' [aw=pwt], nq(5)
        replace state_quint = temp_q if state==`s'
        drop temp_q
}

gen state_sec_quint = .
lab var state_sec_quint "Quintiles within state + sector "
foreach s of local states {
    foreach sec in 0 1 {
            xtile temp_q = welfare_agg21_sp if state==`s' & sector2==`sec' [aw=pwt], nq(5)
            replace state_sec_quint = temp_q if state==`s' & sector2==`sec'
            drop temp_q
        }
    }


* Export for National quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_ALL.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(nat_quint)
gen year = 2022
gen level = "National"
gen area = "All"
gen dem = "All Households"
export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
restore

* Export for National sector 
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_ALL.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 sec_quint)
gen year = 2022
gen level = "National"
gen area = "urban/rural"
gen dem = "All Households"
export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
restore


* Export for State 
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_ALL.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state state_quint)
gen year = 2022
gen level = "State"
gen area = "All"
gen dem = "All Households"
export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
restore


* Export for State + Sector quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_ALL.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 state_sec_quint)
gen year = 2022
gen level = "State"
gen area = "urban/rural"
gen dem = "All Households"
export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
restore
	
	
** EDUCATION QUINTILES

use "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\hhmerge-hces-alldem-income_2022.dta", clear
keep if welfare_agg21_sp != .

gen edu_cat = .
replace edu_cat = 1 if edu_cat1 == 1
replace edu_cat = 2 if missing(edu_cat) & edu_cat2 == 1
replace edu_cat = 3 if missing(edu_cat) & edu_cat3 == 1
replace edu_cat = 4 if missing(edu_cat) & edu_cat4 == 1
replace edu_cat = 5 if missing(edu_cat) & edu_cat5 == 1
replace edu_cat = 6 if missing(edu_cat) & edu_cat6 == 1

lab def edu_lab 1 "No education" 2 "Primary" 3 "Secondary" 4 "Higher Secondary" 5 "Diploma/Graduate" 6 "Post-graduate"
lab val edu_cat edu_lab
keep if edu_cat != .

preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2 edu_cat)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_edu_2022.xlsx", replace
restore 



* National quintile + edu
gen nat_edu_quint = .
lab var nat_edu_quint "Quintiles within edu_cat overall"
levelsof edu_cat, local(edulevels)
foreach e of local edulevels {
    xtile temp_q = welfare_agg21_sp if edu_cat == `e' [aw=pwt], nq(5)
    replace nat_edu_quint = temp_q if edu_cat == `e'
    drop temp_q
}

* National quintile + edu + sector
gen sec_edu_quint = .
lab var sec_edu_quint "Quintiles within sector + edu_cat"

foreach sec in 0 1 {
    foreach e of local edulevels {
        xtile temp_q = welfare_agg21_sp if sector2== `sec' & edu_cat== `e' [aw=pwt], nq(5)
        replace sec_edu_quint = temp_q if sector2== `sec' & edu_cat== `e'
        drop temp_q
    }
}

* National quintile + edu + state
gen state_edu_quint = .
lab var state_edu_quint "Quintiles within state + edu_cat"
levelsof state, local(states)
foreach s of local states {
    foreach e of local edulevels {
        xtile temp_q = welfare_agg21_sp if state==`s' & edu_cat==`e' [aw=pwt], nq(5)
        replace state_edu_quint = temp_q if state==`s' & edu_cat==`e'
        drop temp_q
    }
}

* National quintile + edu + state + sector
gen state_sec_edu_quint = .
lab var state_sec_edu_quint "Quintiles within state + sector + edu_cat"

levelsof state, local(states)
levelsof edu_cat, local(edulevels)

foreach s of local states {
    foreach sec in 0 1 {
        foreach e of local edulevels {
            count if state == `s' & sector2 == `sec' & edu_cat == `e' & !missing(welfare_agg21_sp)
            if r(N) >= 5 {
                xtile temp_q = welfare_agg21_sp if state == `s' & sector2 == `sec' & edu_cat == `e' [aw=pwt], nq(5)
                replace state_sec_edu_quint = temp_q if state == `s' & sector2 == `sec' & edu_cat == `e'
                drop temp_q
            }
        }
    }
}





* Export for National + edu_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_EDU.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(edu_cat nat_edu_quint)
gen year = 2022
gen level = "National"
gen area = "All"
gen dem = "Education"
export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
restore

* Export for National sector + edu_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_EDU.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 edu_cat sec_edu_quint)
gen year = 2022
gen level = "National"
gen area = "urban/rural"
gen dem = "Education"
export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
restore


* Export for State + edu_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_EDU.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state edu_cat state_edu_quint)
gen year = 2022
gen level = "State"
gen area = "All"
gen dem = "Education"
export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
restore


* Export for State + Sector + edu_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_EDU.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 edu_cat state_sec_edu_quint)
gen year = 2022
gen level = "State"
gen area = "urban/rural"
gen dem = "Education"
export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
restore


	
** GENDER QUINTILES

gen gen_cat = .
foreach i in 1 2 {
    replace gen_cat = `i' if gender_cat`i' == 1
}


preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2 gen_cat)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_gen_2022.xlsx", replace
restore 


*National Gender

gen nat_gen_quint = .
lab var nat_gen_quint "Quintiles within gender_cat nationally"
levelsof gen_cat, local(genlevels)
foreach e of local genlevels {
    xtile temp_q = welfare_agg21_sp if gen_cat == `e' [aw=pwt], nq(5)
    replace nat_gen_quint = temp_q if gen_cat == `e'
    drop temp_q
}

* Sector + Gender
gen sec_gen_quint = .
lab var sec_gen_quint "Quintiles within sector + gen_cat"

foreach sec in 0 1 {
    foreach e of local genlevels {
        xtile temp_q = welfare_agg21_sp if sector2== `sec' & gen_cat== `e' [aw=pwt], nq(5)
        replace sec_gen_quint = temp_q if sector2== `sec' & gen_cat== `e'
        drop temp_q
    }
}

* State + Gender
gen state_gen_quint = .
lab var state_gen_quint "Quintiles within state + gen_cat"
levelsof state, local(states)
foreach s of local states {
    foreach e of local genlevels {
        xtile temp_q = welfare_agg21_sp if state==`s' & gen_cat==`e' [aw=pwt], nq(5)
        replace state_gen_quint = temp_q if state==`s' & gen_cat==`e'
        drop temp_q
    }
}

* State + Sector + Gender
gen state_sec_gen_quint = .
lab var state_sec_gen_quint "Quintiles within state + sector + gen_cat"
foreach s of local states {
    foreach sec in 0 1 {
        foreach e of local genlevels {
            xtile temp_q = welfare_agg21_sp if state==`s' & sector2==`sec' & gen_cat==`e' [aw=pwt], nq(5)
            replace state_sec_gen_quint = temp_q if state==`s' & sector2==`sec' & gen_cat==`e'
            drop temp_q
        }
    }
}


* Export for National + gen_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_GEN.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(gen_cat nat_gen_quint)
gen year = 2022
gen level = "National"
gen area = "All"
gen dem = "Gender"
export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
restore

* Export for National sector + gen_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_GEN.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 gen_cat sec_gen_quint)
gen year = 2022
gen level = "National"
gen area = "urban/rural"
gen dem = "Gender"
export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
restore


* Export for State + gen_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_GEN.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by( state gen_cat state_gen_quint)
gen year = 2022
gen level = "State"
gen area = "All"
gen dem = "Gender"
export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
restore


* Export for State + Sector + gen_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_GEN.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 gen_cat state_sec_gen_quint)
gen year = 2022
gen level = "State"
gen area = "urban/rural"
gen dem = "Gender"
export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
restore


******* AGE QUINTILES

gen age_cat = .
replace age_cat = 1 if age_cat1 == 1
replace age_cat = 2 if missing(age_cat) & age_cat2 == 1
replace age_cat = 3 if missing(age_cat) & age_cat3 == 1
replace age_cat = 4 if missing(age_cat) & age_cat4 == 1

lab def age_lab 1 "15-29" 2 "Working-age" 3 "Middle-age" 4 "Older"
lab val age_cat age_lab


preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2 age_cat)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_age_2022.xlsx", replace
restore 


* National + Age

gen nat_age_quint = .
lab var nat_age_quint "Quintiles within age_cat nationally"
levelsof age_cat, local(agelevels)
foreach a of local agelevels {
    xtile temp_q = welfare_agg21_sp if age_cat == `a' [aw=pwt], nq(5)
    replace nat_age_quint = temp_q if age_cat == `a'
    drop temp_q
}

* National + Sector + Age 
gen sec_age_quint = .
lab var sec_age_quint "Quintiles within sector + age_cat"

foreach sec in 0 1 {
    foreach a of local agelevels {
        xtile temp_q = welfare_agg21_sp if sector2 == `sec' & age_cat == `a' [aw=pwt], nq(5)
        replace sec_age_quint = temp_q if sector2 == `sec' & age_cat == `a'
        drop temp_q
    }
}


* National + State + Age
gen state_age_quint = .
lab var state_age_quint "Quintiles within state + age_cat"

levelsof state, local(states)
foreach s of local states {
    foreach a of local agelevels {
        xtile temp_q = welfare_agg21_sp if state == `s' & age_cat == `a' [aw=pwt], nq(5)
        replace state_age_quint = temp_q if state == `s' & age_cat == `a'
        drop temp_q
    }
}

* National + State + Sector + Age
gen state_sec_age_quint = .
lab var state_sec_age_quint "Quintiles within state + sector + age_cat"

foreach s of local states {
    foreach sec in 0 1 {
        foreach a of local agelevels {
            count if state == `s' & sector2 == `sec' & age_cat == `a' & !missing(welfare_agg21_sp)
            if r(N) >= 5 {
                xtile temp_q = welfare_agg21_sp if state == `s' & sector2 == `sec' & age_cat == `a' [aw=pwt], nq(5)
                replace state_sec_age_quint = temp_q if state == `s' & sector2 == `sec' & age_cat == `a'
                drop temp_q
            }
        }
    }
}


* Export for National + age_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_AGE.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(age_cat nat_age_quint)
gen year = 2022
gen level = "National"
gen area = "All"
gen dem = "Age"
export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
restore


local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_AGE.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 age_cat sec_age_quint)
gen year = 2022
gen level = "National"
gen area = "urban/rural"
gen dem = "Age"
export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
restore

local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_AGE.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by (state age_cat state_age_quint)
gen year = 2022
gen level = "State"
gen area = "All"
gen dem = "Age"
export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
restore



local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_AGE.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 age_cat state_sec_age_quint)
gen year = 2022
gen level = "State"
gen area = "urban/rural"
gen dem = "Age"
export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
restore



********* HOUSEHOLD SIZE***********************************************
gen hhsize_cat = .
replace hhsize_cat = 1 if hhsize_cat1 == 1
replace hhsize_cat = 2 if missing(hhsize_cat) & hhsize_cat2 == 1
replace hhsize_cat = 3 if missing(hhsize_cat) & hhsize_cat3 == 1
replace hhsize_cat = 4 if missing(hhsize_cat) & hhsize_cat4 == 1

lab def hhsize_lab 1 "1–2 members" 2 "3–4 members" 3 "5–6 members" 4 "7+ members"
lab val hhsize_cat hhsize_lab
lab var hhsize_cat "Household Size Category (from dummy vars)"

preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2 hhsize_cat)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_hhsize_2022.xlsx", replace
restore 

*National
gen nat_hhsize_quint = .
lab var nat_hhsize_quint "Quintiles within hhsize_cat nationally"

levelsof hhsize_cat, local(hhlevels)
foreach h of local hhlevels {
    xtile temp_q = welfare_agg21_sp if hhsize_cat == `h' [aw=pwt], nq(5)
    replace nat_hhsize_quint = temp_q if hhsize_cat == `h'
    drop temp_q
}

*Sector + National
gen sec_hhsize_quint = .
lab var sec_hhsize_quint "Quintiles within sector + hhsize_cat"

foreach sec in 0 1 {
    foreach h of local hhlevels {
        xtile temp_q = welfare_agg21_sp if sector2 == `sec' & hhsize_cat == `h' [aw=pwt], nq(5)
        replace sec_hhsize_quint = temp_q if sector2 == `sec' & hhsize_cat == `h'
        drop temp_q
    }
}

*State 

gen state_hhsize_quint = .
lab var state_hhsize_quint "Quintiles within state + hhsize_cat"

levelsof state, local(states)
foreach s of local states {
    foreach h of local hhlevels {
        xtile temp_q = welfare_agg21_sp if state == `s' & hhsize_cat == `h' [aw=pwt], nq(5)
        replace state_hhsize_quint = temp_q if state == `s' & hhsize_cat == `h'
        drop temp_q
    }
}

*State + Sector
gen state_sec_hhsize_quint = .
lab var state_sec_hhsize_quint "Quintiles within state + sector + hhsize_cat"

foreach s of local states {
    foreach sec in 0 1 {
        foreach h of local hhlevels {
            count if state == `s' & sector2 == `sec' & hhsize_cat == `h' & !missing(welfare_agg21_sp)
            if r(N) >= 5 {
                xtile temp_q = welfare_agg21_sp if state == `s' & sector2 == `sec' & hhsize_cat == `h' [aw=pwt], nq(5)
                replace state_sec_hhsize_quint = temp_q if state == `s' & sector2 == `sec' & hhsize_cat == `h'
                drop temp_q
            }
        }
    }
}



* Export for National + hhsize_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_HH.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(hhsize_cat nat_hhsize_quint)
gen year = 2022
gen level = "National"
gen area = "All"
gen dem = "HH Size"
export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
restore


* Export for National sector + hhsize_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_HH.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 hhsize_cat sec_hhsize_quint)
gen year = 2022
gen level = "National"
gen area = "urban/rural"
gen dem = "HH Size"
export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
restore


* Export for State + hhsize_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_HH.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state hhsize_cat state_hhsize_quint)
gen year = 2022
gen level = "State"
gen area = "All"
gen dem = "HH Size"
export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
restore


* Export for State + Sector + hhsize_cat quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_HH.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 hhsize_cat state_sec_hhsize_quint)
gen year = 2022
gen level = "State"
gen area = "urban/rural"
gen dem = "HH Size"
export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
restore



			
** OWNS DWELLING
use "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2011\hces2022.dta", clear
keep if welfare_agg21_sp != .
keep if owns_dwelling != .

preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2 owns_dwelling)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_dwelling_2022.xlsx", replace
restore


gen nat_dwel_quint = .
lab var nat_dwel_quint "Quintiles within owns_dwelling nationally"
levelsof owns_dwelling, local(dwellevels)
foreach l of local dwellevels {
    xtile temp_q = welfare_agg21_sp if owns_dwelling == `l' [aw=pwt], nq(5)
    replace nat_dwel_quint = temp_q if owns_dwelling == `l'
    drop temp_q
}


gen sec_dwel_quint = .
lab var sec_dwel_quint "Quintiles within sector + owns_dwelling"

foreach sec in 0 1 {
    foreach l of local dwellevels {
        xtile temp_q = welfare_agg21_sp if sector2 == `sec' & owns_dwelling == `l' [aw=pwt], nq(5)
        replace sec_dwel_quint = temp_q if sector2 == `sec' & owns_dwelling == `l'
        drop temp_q
    }
}


gen state_dwel_quint = .
lab var state_dwel_quint "Quintiles within state + owns_dwelling"

levelsof state, local(states)
foreach s of local states {
    foreach l of local dwellevels {
        xtile temp_q = welfare_agg21_sp if state == `s' & owns_dwelling == `l' [aw=pwt], nq(5)
        replace state_dwel_quint = temp_q if state == `s' & owns_dwelling == `l'
        drop temp_q
    }
}

gen state_sec_dwel_quint = .
lab var state_sec_dwel_quint "Quintiles within state + sector + owns_dwelling"

foreach s of local states {
    foreach sec in 0 1 {
        foreach l of local dwellevels {
            count if state == `s' & sector2 == `sec' & owns_dwelling == `l' & !missing(welfare_agg21_sp)
            if r(N) >= 5 {
                xtile temp_q = welfare_agg21_sp if state == `s' & sector2 == `sec' & owns_dwelling == `l' [aw=pwt], nq(5)
                replace state_sec_dwel_quint = temp_q if state == `s' & sector2 == `sec' & owns_dwelling == `l'
                drop temp_q
            }
        }
    }
}
* Export for National + owns_dwelling quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_22_DWEL.xlsx"

	preserve
	collapse (median) welfare_agg21_sp [aw = pwt], by(owns_dwelling nat_dwel_quint)
	gen year = 2022
	gen level = "National"
	gen area = "All"
	gen dem = "Owns Dwelling"
	export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
	restore

* Export for National sector + owns_dwelling quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_22_DWEL.xlsx"

	preserve
	collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 owns_dwelling sec_dwel_quint)
	gen year = 2022
	gen level = "National"
	gen area = "urban/rural"
	gen dem = "Owns Dwelling"
	export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
	restore

* Export for State + owns_dwelling quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_22_DWEL.xlsx"

	preserve
	collapse (median) welfare_agg21_sp [aw = pwt], by(state owns_dwelling state_dwel_quint)
	gen year = 2022
	gen level = "State"
	gen area = "All"
	gen dem = "Owns Dwelling"
	export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
	restore

* Export for State + Sector + owns_dwelling quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_22_DWEL.xlsx"

	preserve
	collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 owns_dwelling state_sec_dwel_quint)
	gen year = 2022
	gen level = "State"
	gen area = "urban/rural"
	gen dem = "Owns Dwelling"
	export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
	restore
	





**** OWNS LAND

keep if owns_land != .

preserve
collapse (count) n_obs = welfare_agg21_sp, by(state sector2 owns_land)
gen ineligible = n_obs < 5
tempfile obsinfo
save `obsinfo', replace
export delimited "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\quintile_land_2022.xlsx", replace
restore

gen nat_land_quint = .
lab var nat_land_quint "Quintiles within owns_land nationally"

levelsof owns_land, local(landlevels)
foreach l of local landlevels {
    xtile temp_q = welfare_agg21_sp if owns_land == `l' [aw=pwt], nq(5)
    replace nat_land_quint = temp_q if owns_land == `l'
    drop temp_q
}


gen sec_land_quint = .
lab var sec_land_quint "Quintiles within sector + owns_land"

foreach sec in 0 1 {
    foreach l of local landlevels {
        xtile temp_q = welfare_agg21_sp if sector2 == `sec' & owns_land == `l' [aw=pwt], nq(5)
        replace sec_land_quint = temp_q if sector2 == `sec' & owns_land == `l'
        drop temp_q
    }
}


gen state_land_quint = .
lab var state_land_quint "Quintiles within state + owns_land"

levelsof state, local(states)
foreach s of local states {
    foreach l of local landlevels {
        xtile temp_q = welfare_agg21_sp if state == `s' & owns_land == `l' [aw=pwt], nq(5)
        replace state_land_quint = temp_q if state == `s' & owns_land == `l'
        drop temp_q
    }
}

gen state_sec_land_quint = .
lab var state_sec_land_quint "Quintiles within state + sector + owns_land"

foreach s of local states {
    foreach sec in 0 1 {
        foreach l of local landlevels {
            count if state == `s' & sector2 == `sec' & owns_land == `l' & !missing(welfare_agg21_sp)
            if r(N) >= 5 {
                xtile temp_q = welfare_agg21_sp if state == `s' & sector2 == `sec' & owns_land == `l' [aw=pwt], nq(5)
                replace state_sec_land_quint = temp_q if state == `s' & sector2 == `sec' & owns_land == `l'
                drop temp_q
            }
        }
    }
}



* Export for National + owns_land quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_LAND.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(owns_land nat_land_quint)
gen year = 2022
gen level = "National"
gen area = "All"
gen dem = "Owns Land"
export excel using "`out'", sheet("nat_quint") firstrow(variables) sheetreplace
restore


* Export for National sector + owns_land quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_LAND.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(sector2 owns_land sec_land_quint)
gen year = 2022
gen level = "National"
gen area = "urban/rural"
gen dem = "Owns Land"
export excel using "`out'", sheet("sec_quint") firstrow(variables) sheetreplace
restore


* Export for State + owns_land quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_LAND.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state owns_land state_land_quint)
gen year = 2022
gen level = "State"
gen area = "All"
gen dem = "Owns Land"
export excel using "`out'", sheet("state_quint") firstrow(variables) sheetreplace
restore


* Export for State + Sector + owns_land quintiles
local out "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\HCES-Monetary\2022\Welfare_quintiles_2022\welfare_21_quint_LAND.xlsx"

preserve
collapse (median) welfare_agg21_sp [aw = pwt], by(state sector2 owns_land state_sec_land_quint)
gen year = 2022
gen level = "State"
gen area = "urban/rural"
gen dem = "Owns Land"
export excel using "`out'", sheet("state_sec_quint") firstrow(variables) sheetreplace
restore


/*****************************************************************************************************************

R CODE FOR MERGING


setwd("C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/HCES-Monetary/2022/Welfare_quintiles_2022")

### Welfare Aggregate 2021

read_and_tag <- function(path) {
  sheet_names <- excel_sheets(path)
  bind_rows(lapply(sheet_names, function(sheet) {
    read_excel(path, sheet = sheet) %>% mutate(SourceSheet = sheet)
  }))
}

### 2022

merged_data_all <- read_and_tag("welfare_21_quint_ALL.xlsx")
merged_data_edu <- read_and_tag("welfare_21_quint_EDU.xlsx")
merged_data_gender <- read_and_tag("welfare_21_quint_GEN.xlsx")
merged_data_age <- read_and_tag("welfare_21_quint_AGE.xlsx")
merged_data_hhsize <- read_and_tag("welfare_21_quint_HH.xlsx")
merged_data_land <- read_and_tag("welfare_21_quint_LAND.xlsx")

hces2022_welfare_21 <- bind_rows(merged_data_all, merged_data_age, merged_data_edu, merged_data_gender, merged_data_hhsize, merged_data_land)

library(dplyr)

hces_welfare_2022 <- hces2022_welfare_21 %>%
  mutate(
    quintile_overall = coalesce(
      nat_quint,
      nat_age_quint,
      nat_edu_quint,
      nat_gen_quint,
      nat_hhsize_quint,
      nat_land_quint,
      
      sec_quint,
      sec_age_quint,
      sec_edu_quint,
      sec_gen_quint,
      sec_hhsize_quint,
      sec_land_quint,
      
      state_quint,
      state_age_quint,
      state_edu_quint,
      state_gen_quint,
      state_hhsize_quint,
      state_land_quint,
      
      state_sec_quint,
      state_sec_age_quint,
      state_sec_edu_quint,
      state_sec_gen_quint,
      state_sec_hhsize_quint,
      state_sec_land_quint
    )
  )

table(hces_welfare_all$quintile_overall, useNA = "always")

write.csv(hces_welfare_2022, "hces_welfare_2022.csv")

****************************************************************************************************************/










	