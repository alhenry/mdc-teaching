* ASRR messy data challenge *
* example analysis do-file *

* set working directory:
cd "N:\IHI teaching\mdc-teaching\"

* read in data:
use icu_data, clear

* flag first ICULOS  per patient 
sort patid iculos
egen patid_fl = tag(patid)

*----------------------*
* take a look at data:
*----------------------*
* what's in the data set
describe  

* distributions of each of the variables
codebook

* better visualisation of each of the variables
inspect

* complete case indicator:
egen nvar_miss = rowmiss(o2sat hr temp sbp map resp)

gen cc_fl = (nvar_miss == 0)

tab cc_fl
* only 23.9% of records have no missing vital signs


***********
* outcome *
***********
* how many people were diagnosed with sepsis?
tab sepsislabel
* 1657 (0.14% of the cohort)


* when do people get sepsis in ICU?
gen time_to_sepsis_temp = iculos if sepsislabel == 1

egen time_to_sepsis = min(time_to_sepsis_temp), by(patid)

su time_to_sepsis if patid_fl == 1, d
* min: 7 hours, max 331 hours (13.8 days)
* median: 45 hours

hist time_to_sepsis if patid_fl == 1


* create indicator for patient who get sepsis:
egen any_sepsis = max(sepsislabel), by(patid)


* drop iculos >= 6 
drop if iculos >= 6

*********************************
* imputing explanatory measures *
*********************************

* mean imputation
foreach var of varlist o2sat hr temp sbp dbp map resp {

egen `var'_mean = mean(`var') if iculos <= 5, by(patid)
gen `var'_imp1 = `var'
replace `var'_imp1 = `var'_mean if `var'_imp1 ==. & iculos <= 5

}

* first observation carried backwards
foreach var of varlist o2sat hr temp sbp dbp map resp {

gen `var'_imp2 = `var'
by patid (iculos), sort: replace `var'_imp2 = `var'[_n+1] if `var' == .
by patid (iculos), sort: replace `var'_imp2 = `var'[_n+2] if `var' == . & `var'[_n+1] == . 
by patid (iculos), sort: replace `var'_imp2 = `var'[_n+3] if `var' == . & `var'[_n+1] == . & `var'[_n+2] == .
by patid (iculos), sort: replace `var'_imp2 = `var'[_n+4] if `var' == . & `var'[_n+1] == . & `var'[_n+2] == . & `var'[_n+3] == .
}

* inspect missingness again among imputed variables
egen nvar_miss_imp1 = rowmiss(o2sat_imp1 hr_imp1 temp_imp1 sbp_imp1 map_imp1 resp_imp1)

gen cc_fl_imp1 = (nvar_miss_imp1 == 0)

tab cc_fl_imp1 if iculos == 1


egen nvar_miss_imp2 = rowmiss(o2sat_imp2 hr_imp2 temp_imp2 sbp_imp2 map_imp2 resp_imp2)

gen cc_fl_imp2 = (nvar_miss_imp2 == 0)

tab cc_fl_imp2 if iculos == 1

* 77% of rows non-missing for each imputation method.

*************
* modelling *
*************
* dummy indicators for hospital:
qui ta hospid, gen(h_)

* can use these to include hospital as a fixed-effect (i.e. create intercepts specific each hospital)
* we cannot include hospital as a random-effect as there are too few hospitals (n = 2)

* mean imputation:
glm any_sepsis age i.gender o2sat_imp1 hr_imp1 temp_imp1 ///
    sbp_imp1 map_imp1 resp_imp1 h_* if iculos == 1, ///
	f(binomial) l(logit) eform nocons 

* first obs carried backwards:
glm any_sepsis age i.gender o2sat_imp2 hr_imp2 temp_imp2 ///
    sbp_imp2 map_imp2 resp_imp2 h_* if iculos == 1, ///
	f(binomial) l(logit) eform nocons
	
* higher respiration rate among those with sepsis?
bysort any_sepsis: su resp_imp1 if patid_fl == 1

bysort any_sepsis: su resp_imp2 if patid_fl == 1



