
	/* =============================================================================
Project				: 	Data for state dashboard  - 2011-12 & 2022-23 
File purpose		:   Master do file: repository of all the work
						All do files must be numbered
Authors				: 	Nancy/Vishali
Date created		: 	13 March 2025				
Date last modified	:   11 Aug 2025
============================================================================= */

	clear all
	set logtype text
	cap log close
	set more off
	set maxvar 10000

	version 17
	
/* -----------------------------------------------------------------------------
				1. Install programs 
----------------------------------------------------------------------------- */
// 	ssc install fastreshape, all replace
// 	ssc install ainequal, all replace
// 	ssc install distinct, all replace
// 	ssc install mdesc, all replace
// 	ssc install egenmore, all replace

/* -----------------------------------------------------------------------------
				2. Set Working Directory
----------------------------------------------------------------------------- */
* Update the path manually if required

*Nancy
else if c(username) == "wb595750" {
	glo microdata "C:/Users/`c(username)'/WBG/Nishtha Kochhar - INDDATA/HCES"
	glo directory "C:/Users/`c(username)'/WBG/Nandini Krishnan - INDPEW/Analysis/poverty estimates_2022/IND_hces_shared_analysis/Analysis/round_10"
	glo output "C:/Users/`c(username)'/WBG/Nandini Krishnan - INDPEW/Analysis/State Dashboard/Schemes/HCES/output"
}


*Vishali

else if c(username) == "wb608271" {
	glo microdata "C:/Users/`c(username)'/WBG/Nishtha Kochhar - INDDATA/HCES"
	glo directory "C:\Users\wb608271\WBG"
	glo output "C:/Users/wb608271/WBG/Nandini Krishnan - Schemes/HCES/output/30june"
}



/*------------------------------------------------------------------------------
				3. Set Paths
----------------------------------------------------------------------------- */

	glo other_data 	"${directory}/other_data" 
	glo data_cons 	"${directory}/Nandini Krishnan - data_constructed"
	glo input	 	"${directory}/output"

/* -----------------------------------------------------------------------------
THE END
----------------------------------------------------------------------------- */"
	glo output "C:/Users/`c(username)'/WBG/Nandini Krishnan - INDPEW/Analysis/State Dashboard/Schemes/HCES/output"

/*******************************************************************************
STEP 1: CREATE QUINTILES
*******************************************************************************/

foreach y in 2011 2022 {

	use "${input}/master_pov_input_`y'_all.dta", clear
	keep hhid state sector pwt mpce_sp_def_ind
	
	*Quintiles at national level
	xtile nat_quint = mpce_sp_def_ind [aw=pwt], nq(5)
	lab var nat_quint "Quintiles at national level"
	
	*Quintiles at sectoral level
	gen sec_quint = . 
	lab var sec_quint "Quintiles at sectoral level"
	foreach s in 1 2 {
	xtile temp_quint = mpce_sp_def_ind if sector==`s' [aw=pwt], nq(5)
	replace sec_quint = temp_quint if sector==`s'
	drop temp_quint
	}
	
	*Quintiles at state level
	gen state_quint = . 
	lab var state_quint "Quintiles at state level"
	levelsof state, local(state)
	foreach i of local state {
	xtile temp_quint = mpce_sp_def_ind if state==`i' [aw=pwt], nq(5)
	replace state_quint = temp_quint if state==`i'
	drop temp_quint 
	}
	
	*Quintiles at state-sector level
	gen state_sec_quint = .
	lab var state_sec_quint "Quintiles at state-sector level"
	levelsof state, local(state)
	foreach i of local state {
		foreach s in 1 2 {
	xtile temp_quint = mpce_sp_def_ind if state==`i' & sector==`s' [aw=pwt], nq(5)
	replace state_sec_quint = temp_quint if state==`i' & sector==`s'
	drop temp_quint 
	}
	}

	save "${output}/hh_quintiles_`y'", replace if year==2011
}


* link to dataset

* hh_quintiles_2011:${output}/hh_quintiles_2011.dta
* hh_quintiles_2022: ${output}/hh_quintiles_2022.dta

/*******************************************************************************
STEP 2: IDENTIFY HOUSEHOLDS RECEIVING SUBSIDIZED FOOD + NON-FOOD ITEMS - 2011
*******************************************************************************/

	use "C:\Users\wb608271\WBG\Nandini Krishnan - Welfare\master_consumption_2011.dta", clear 
	gen total_food_pds_subs = inlist(item, 101, 107, 171)
	gen kerosene_pds_subs = inlist(item, 334)
	collapse (sum) total_food_pds_subs kerosene_pds_subs, by(hhid)
	
	gen food_pds_subs = total_food_pds_subs!=0
	drop total_food_pds_subs
	lab var food_pds_subs "Dummy for households receiving food items at subsidized prices"
	lab var kerosene_pds_subs "Dummy for households receiving kerosene at subsidized prices"
	
	merge 1:1 hhid using "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\hh_quintiles_2011.dta", keep(3) nogen
	
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\merged_2011.dta", replace
	
	use "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\merged_2011.dta", clear

/*******************************************************************************
STEP 2A: IDENTIFY HOUSEHOLDS ACROSS WELFARE QUINTILES - 2011
*******************************************************************************/
	
glo output "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output"
	
* base path
local base "C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/Monetary-Schemes/2011"

* Define tempfiles
tempfile master_append
save `master_append', emptyok

* 1. nat_quint
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(nat_quint)
tostring nat_quint, replace
gen quintile = nat_quint
drop nat_quint
gen group = "nat_quint"
gen year = 2011
save `master_append', replace

* 2. sector sec_quint
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(sector sec_quint)
tostring sec_quint, replace
gen quintile = sec_quint
drop sec_quint
gen group = "sector_sec_quint"
gen year = 2011
append using `master_append'
save `master_append', replace

* 3. state state_quint
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(state state_quint)
tostring state_quint, replace
gen quintile = state_quint
drop state_quint
gen group = "state_quint"
gen year = 2011
append using `master_append'
save `master_append', replace

* 4. state sector state_sec_quint
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(state sector state_sec_quint)
tostring state_sec_quint, replace
gen quintile = state_sec_quint
drop state_sec_quint
gen group = "state_sector_quint"
gen year = 2011
append using `master_append'
save `master_append', replace

* 5. state only
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(state)
gen quintile = "All"
gen group = "state"
gen year = 2011
append using `master_append'
save `master_append', replace

* 6. sector and state
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(sector state)
gen quintile = "All"
gen group = "sector_state"
gen year = 2011
append using `master_append'
save `master_append', replace

* 7. sector only
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(sector)
gen quintile = "All"
gen group = "sector"
gen year = 2011
append using `master_append'
save `master_append', replace

* 8. national total
use "${output}/merged_2011.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt]
gen quintile = "All"
gen group = "india_all"
gen year = 2011
append using `master_append'
save `master_append', replace

* Save final outputs
use `master_append', clear
order group quintile year food_pds_subs kerosene_pds_subs sector state, last
export delimited using "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2011\merged_allgroups_2011.csv", replace
save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2011\merged_allgroups_2011.dta", replace

	
/*******************************************************************************
STEP 2B: IDENTIFY HOUSEHOLDS ACROSS POVERTY LEVELS - 2011
*******************************************************************************/


* Match with poverty data 
use "C:\Users\wb608271\Downloads\welfare_pov_final_ppp21.dta", clear
merge 1:1 hhid using "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\merged_2011.dta"
	
	
save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\merged_2011_pov.dta", replace



	* 101, 651 observations from 2011 data matched
	
glo output "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output"

* Define tempfiles
tempfile master_append
save `master_append', emptyok

* 1. national
use "${output}/merged_2011_pov.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(poor_420)
gen quintile = ""
gen group = "india_all"
gen year = 2011
save `master_append', replace

* 2. sector 
use "${output}/merged_2011_pov.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(sector poor_420)
gen quintile = " "
gen group = "sector"
gen year = 2011
append using `master_append'
save `master_append', replace

* 3. state 
use "${output}/merged_2011_pov.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(state poor_420)
gen quintile = " "
gen group = "state"
gen year = 2011
append using `master_append'
save `master_append', replace

* 4. state sector 
use "${output}/merged_2011_pov.dta", clear
collapse (mean) food_pds_subs kerosene_pds_subs [aw=pwt], by(state sector poor_420)
gen quintile = " "
gen group = "state_sector"
gen year = 2011
append using `master_append'
save `master_append', replace

* Save final outputs
use `master_append', clear
order group poor_420 sector state year food_pds_subs kerosene_pds_subs, last
export delimited using "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2011\merged-poor_2011", replace
save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2011\merged_poor_2011.dta", replace
	

	
/*******************************************************************************
STEP 3: IDENTIFY HOUSEHOLDS RECEIVING SUBSIDIZED AND FREE FOOD + NONFOOD ITEMS - 2022
*******************************************************************************/

	*FOOD - KEROSENE
	use "C:\Users\wb608271\WBG\Nandini Krishnan - data_constructed\master_consumption_2022.dta", clear
	merge m:1 item using "C:\Users\wb608271\WBG\Nandini Krishnan - data_constructed\master_item_list_2022.dta", keepusing(free pds) keep(3) nogen
	gen total_food_pds_subs = pds==1 & item!=334
	gen kerosene_pds_subs = inlist(item, 334)
	gen total_food_pds_free = free==1
	
	collapse (sum) total_food_pds_subs kerosene_pds_subs total_food_pds_free, by(hhid)
	gen food_pds_subs = total_food_pds_subs!=0
	gen food_pds_free = total_food_pds_free!=0
	drop total_food_pds_subs total_food_pds_free
	lab var food_pds_subs "Dummy for households receiving food items at subsidized prices"
	lab var food_pds_free "Dummy for households receiving food items for free"
	lab var kerosene_pds_subs "Dummy for households receiving kerosene at subsidized prices"
	
tempfile food_kerosene
save `food_kerosene'
save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\food_kerosene.dta", replace
	
	
	*LPG - ELECTRICITY - SCHOOL ITEMS - SCHOOL/COLLEGE FEE WAIVERED/REIMBURSED - PMJAY - BENEFITS AVAILED PMJAY	
	/* NOTE: Free school items include free textbooks, stationery, bags,
	uniform, school shoes and others. These items are spread across CSQ and DGQ. 
	Need to merge the two dataset to create this category. 
	*/
	
	use "C:\Users\wb608271\WBG\Nishtha Kochhar - HCES\2022-23\dta\307_csq.dta", clear
	merge 1:1 hhid using "C:\Users\wb608271\WBG\Nishtha Kochhar - HCES\2022-23\dta\411_dgq.dta", keepusing(clothing_free footwear_free) keep(3) nogen
	
	gen school_items_free = books_free==1 |stationary_free==1| schoolbag_free==1 | ///
	other_free==1 | clothing_free==1 | footwear_free==1
	gen books = books_free == 1
	gen stationary = stationary_free == 1
	gen schoolbag = schoolbag_free == 1
	gen other = other_free == 1
	gen clothing = clothing_free == 1
	gen footwear = footwear_free == 1
	gen gov_school_free = edu_institution==1 & school_items_free==1
	gen pvt_school_free = edu_institution==2 & school_items_free==1
	
	gen lpg_subs = lpg==1
	gen elec_free = electricity_free==1
	
	gen fees_waived = fees_reimbursed==1	
	ta fees_waived if edu_institution==2 //Fees is not waived or reimbursed for private institutions. This should be zero!!
	
	gen pmjay_ben = pmjay_otherbenefit==1
	gen pmjay_ben_avail = medical_pmjay_other==1
	
	keep hhid gov_school_free pvt_school_free lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail school_items_free books stationary schoolbag other clothing footwear
	
	lab var gov_school_free "Free school items - govt. institutions"
	lab var pvt_school_free "Free school items - pvt. institutions"
	lab var lpg_subs "Subsidized LPG"
	lab var elec_free "Free electricity"
	lab var fees_waived "School/college fees waived/reimbursed"
	lab var pmjay_ben "Beneficiary of PMJAY or any other health scheme"
	lab var pmjay_ben_avail "Benefit availed under PMJAY or any other health scheme in last 365 days"
	
tempfile cons_free_subs
save `cons_free_subs'
save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\cons_free_subs.dta", replace
	
	* DURABLES FREE 
	/* NOTE: Free durables include free laptop, tablet, mobile, bicycle, 
	motorcycles and other free durables (excluding school shoes and uniforms)
	*/
	use "C:\Users\wb608271\WBG\Nishtha Kochhar - HCES\2022-23\dta\411_dgq.dta", clear
	gen durables_free = laptop_free==1 | tablet_free==1 | mobile_free==1 | ///
	bicycle_free==1 | other_items_free==1
	gen laptop2 = laptop_free == 1
	gen tablet2 = tablet_free == 1
	gen mobile2 = mobile_free == 1
	gen bicycle2 = bicycle_free == 1
	gen other_items2 = other_items_free == 1

	keep hhid durables_free laptop2 tablet2 mobile2 bicycle2 other_items2
tempfile durables_free
save `durables_free'
save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\durables_free.dta", replace
	
	
use "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\food_kerosene.dta"
	merge 1:1 hhid using "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\cons_free_subs.dta", keep(3) nogen
	merge 1:1 hhid using "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\durables_free.dta", keep(3) nogen
	merge 1:1 hhid using "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\hh_quintiles_2022.dta", keep(3) nogen
	
	tabstat food_pds_free [aw=pwt], by(nat_quint) stat(mean)
	

	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\all_merged_2022.dta", replace
	
	use "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\all_merged_2022.dta", clear
	
	
	
	
	** National Quintiles 
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt], by(nat_quint)
	gen year = 2022
	gen type = "quintile"
	gen level = "National"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\nat-quint.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec1-nat-quint.xlsx", firstrow(variables) replace
	restore
	
	** Sector Quintiles - National
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
		lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt], by(sec_quint sector)
	gen year = 2022
	gen type = "quintile"
	gen level = "National"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\sec-quint.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec2-sec-quint.xlsx", firstrow(variables) replace
	restore
	
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
		lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt], by(state_quint state)
	gen year = 2022
	gen type = "quintile"
	gen level = "State"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\state-quint.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec3-state-quint.xlsx", firstrow(variables) replace
	restore
	
	
		
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt], by(state_sec_quint state sector)
	gen year = 2022
	gen type = "quintile"
	gen level = "State"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\state-sec-quint.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec4-state-sec-quint.xlsx", firstrow(variables) replace
	restore
	
		
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt], by(state)
	gen year = 2022
	gen type = "All"
	gen level = "State"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\stateall.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec5-stateall.xlsx", firstrow(variables) replace
	restore
	
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt], by(state sector)
	gen year = 2022
	gen type = "All"
	gen level = "State"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\stateall-sector.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec6-stateall-sector.xlsx", firstrow(variables) replace
	restore
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items books stationary schoolbag clothing footwear [aw=pwt], by(sector)
	gen year = 2022
	gen type = "All"
	gen level = "National"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\sectorall.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec7-sectall.xlsx", firstrow(variables) replace
	restore
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear [aw=pwt]
	gen year = 2022
	gen type = "All"
	gen level = "National"
	save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\indallall.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\excelsheets\2022hces\sec8-indiaall.xlsx", firstrow(variables) replace
	restore
	
	***************************************************************************************
		
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free lpg_subs elec_free pmjay_ben pmjay_ben_avail [aw=pwt], by(state)
	gen year = 2022
	gen type = "All"
	gen level = "State"
	save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\stateall.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\stateall.xlsx", firstrow(variables) replace
	restore
	
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free lpg_subs elec_free pmjay_ben pmjay_ben_avail [aw=pwt], by(state sector)
	gen year = 2022
	gen type = "All"
	gen level = "State"
	save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\state-sector.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\state-sector.xlsx", firstrow(variables) replace
	restore
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free lpg_subs elec_free pmjay_ben pmjay_ben_avail [aw=pwt], by(sector)
	gen year = 2022
	gen type = "All"
	gen level = "National"
	save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\sector-all.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\sector-all.xlsx", firstrow(variables) replace
	restore
	
	preserve
	collapse (mean) food_pds_subs kerosene_pds_subs food_pds_free lpg_subs elec_free pmjay_ben pmjay_ben_avail [aw=pwt]
	gen year = 2022
	gen type = "All"
	gen level = "National"
	save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\india-all.dta", replace
	export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022-sc\india-all.xlsx", firstrow(variables) replace
	restore
	
	*********************************************************************************************************
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	* Define base path
local path "C:/Users/wb608271/WBG/Nandini Krishnan - Schemes/HCES/output/2022"

* Load first dataset
use "`path'/nat-quint.dta", clear
gen quintile = nat_quint if !missing(nat_quint)
drop nat_quint

* Loop over the rest
foreach file in sec-quint state-quint state-sec-quint stateall stateall-sector sectorall indallall {
    append using "`path'/`file'.dta"

    * Harmonize quintile fields
    capture confirm variable sec_quint
    if !_rc {
        replace quintile = sec_quint if missing(quintile) & !missing(sec_quint)
        drop sec_quint
    }

    capture confirm variable state_quint
    if !_rc {
        replace quintile = state_quint if missing(quintile) & !missing(state_quint)
        drop state_quint
    }

    capture confirm variable state_sec_quint
    if !_rc {
        replace quintile = state_sec_quint if missing(quintile) & !missing(state_sec_quint)
        drop state_sec_quint
    }
}

* Save merged file
save "`path'/merged_summary_2022.dta", replace

* Optional: export to Excel
export excel using "`path'/merged_summary_2022.xlsx", firstrow(variables) replace

	
	
	
	
	
	****************************************************
	
	
	
	* Define base path for saving
local base "C:/Users/wb608271/OneDrive - WBG/Desktop/STATA_Output/Monetary-Schemes/2022"

* Create an empty master file to append into
clear
tempfile master_all
save `master_all', emptyok

* COMMON VARIABLES
local vars food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 ///
	mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear

* 1. National Quintile
preserve
collapse (mean) `vars' [aw=pwt], by(nat_quint)
gen year = 2022
gen type = "quintile"
gen level = "National"
gen quintile = nat_quint
drop nat_quint
tempfile part1
save `part1'
restore

* 2. Sector Quintile - National
preserve
collapse (mean) `vars' [aw=pwt], by(sec_quint sector)
gen year = 2022
gen type = "quintile"
gen level = "National"
gen quintile = sec_quint
drop sec_quint
tempfile part2
save `part2'
restore

* 3. State Quintile
preserve
collapse (mean) `vars' [aw=pwt], by(state_quint state)
gen year = 2022
gen type = "quintile"
gen level = "State"
gen quintile = state_quint
drop state_quint
tempfile part3
save `part3'
restore

* 4. State + Sector Quintile
preserve
collapse (mean) `vars' [aw=pwt], by(state_sec_quint state sector)
gen year = 2022
gen type = "quintile"
gen level = "State"
gen quintile = state_sec_quint
drop state_sec_quint
tempfile part4
save `part4'
restore

* 5. State Average
preserve
collapse (mean) `vars' [aw=pwt], by(state)
gen year = 2022
gen type = "All"
gen level = "State"
tempfile part5
save `part5'
restore

* 6. State + Sector
preserve
collapse (mean) `vars' [aw=pwt], by(state sector)
gen year = 2022
gen type = "All"
gen level = "State"
tempfile part6
save `part6'
restore

* 7. Sector Only
preserve
collapse (mean) `vars' [aw=pwt], by(sector)
gen year = 2022
gen type = "All"
gen level = "National"
tempfile part7
save `part7'
restore

* 8. All India
preserve
collapse (mean) `vars' [aw=pwt]
gen year = 2022
gen type = "All"
gen level = "National"
tempfile part8
save `part8'
restore

* Combine all parts
use `part1', clear
append using `part2'
append using `part3'
append using `part4'
append using `part5'
append using `part6'
append using `part7'
append using `part8'

* Save final merged dataset
save "`base'/merged_summary_2022.dta", replace

* Optional: export to Excel
export excel using "`base'/merged_summary_2022.xlsx", firstrow(variables) replace

	*********************Merge**************************
	
* Step 1: Import 2011 Excel file
import excel using "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\merged_all_2011.xlsx", firstrow clear
save "merged_2011.dta", replace

* Step 2: Import 2022 Excel file
import excel using "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\merged_all_2022.xlsx", firstrow clear
save "merged_2022.dta", replace

* Step 3: Append the two datasets
use "merged_2011.dta", clear
append using "merged_2022.dta"

* Step 4: Save the combined dataset
save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\merged_all_years.dta", replace
export excel "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\merged_all_years.xlsx", firstrow(variables) replace


*********************************************
* Create poverty variable for 2022 datasets
use "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output\2022\all_merged_2022.dta", clear
merge 1:1 hhid using "C:\Users\wb608271\Downloads\welfare_pov_final_ppp21.dta"

* all 261,696 observations merged


save "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output/merged_2022_pov.dta", replace

	
glo output "C:\Users\wb608271\WBG\Nandini Krishnan - Schemes\HCES\output"

local vars food_pds_subs kerosene_pds_subs food_pds_free gov_school_free pvt_school_free ///
	lpg_subs elec_free fees_waived pmjay_ben pmjay_ben_avail durables_free laptop2 tablet2 ///
	mobile2 bicycle2 other_items2 books stationary schoolbag clothing footwear


* Define tempfiles
tempfile master_append
save `master_append', emptyok

* 1. national
use "${output}/merged_2022_pov.dta", clear
collapse (mean) `vars' [aw=pwt], by(poor_420)
gen quintile = ""
gen group = "india_all"
gen year = 2022
save `master_append', replace

* 2. sector 
use "${output}/merged_2022_pov.dta", clear
collapse (mean) `vars' [aw=pwt], by(sector poor_420)
gen quintile = " "
gen group = "sector"
gen year = 2022
append using `master_append'
save `master_append', replace

* 3. state 
use "${output}/merged_2022_pov.dta", clear
collapse (mean) `vars' [aw=pwt], by(state poor_420)
gen quintile = " "
gen group = "state"
gen year = 2022
append using `master_append'
save `master_append', replace

* 4. state sector 
use "${output}/merged_2022_pov.dta", clear
collapse (mean) `vars' [aw=pwt], by(state sector poor_420)
gen quintile = " "
gen group = "state_sector"
gen year = 2022
append using `master_append'
save `master_append', replace

* Save final outputs
use `master_append', clear
order group poor_420 sector state year `vars', last
export delimited using "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022\merged-poor_2022.csv", replace
save "C:\Users\wb608271\OneDrive - WBG\Desktop\STATA_Output\Monetary-Schemes\2022\merged_poor_2022.dta", replace
	



