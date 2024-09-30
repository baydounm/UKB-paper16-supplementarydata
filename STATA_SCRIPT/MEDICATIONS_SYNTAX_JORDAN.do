clear
set memory 4g  // Allocate 4 GB of RAM for Stata
set matsize 11000  // Increase the maximum matrix size to 11000
set maxvar 32767  // Increase the maximum number of variables


********************************************************************************
** load finaldata_unimputed.dta
********************************************************************************
use "C:\Users\weissj\Downloads\UKB_PAPER9C_POLYPHPDDEMMORT\finaldata_unimputed.dta"


********************************************************************************
** restrict to n_eid and medication variables
********************************************************************************
keep n_eid n_20003_0_*

********************************************************************************
** reshape to person-medication format
********************************************************************************
reshape long n_20003_0_, i(n_eid) j(medication_number)

********************************************************************************
** rename medication code and drop missing values
********************************************************************************
rename n_20003_0_ coding
drop if missing(coding)

********************************************************************************
** group by coding and count the number of subjects
********************************************************************************
egen count_uniq_subj = count(n_eid), by(coding)

********************************************************************************
** restrict to medications taken by >=5000 subjects
********************************************************************************
keep if count_uniq_subj >= 5000 
* 10000 yields 22 codes
* 7500 yield 26 codes
* 5000 yield 44 codes

********************************************************************************
** merge with coding4.tsv
********************************************************************************
tempfile finaldata_unimputed_tmp
save "`finaldata_unimputed_tmp'"

import delimited "C:\Users\weissj\Downloads\coding4.tsv", clear
tempfile coding4_tmp
save "`coding4_tmp'"

use "`finaldata_unimputed_tmp'", clear
merge m:1 coding using "`coding4_tmp'"
keep if _merge == 3
drop if meaning == "Free-text entry, unable to be coded"

********************************************************************************
** cleaning meaning variable
********************************************************************************
replace meaning = regexr(meaning, "[^a-zA-Z0-9/ ]", "_")
replace meaning = regexr(meaning, " ", "_")
replace meaning = regexr(meaning, "/", "_")
replace meaning = regexr(meaning, "/", "_")
replace meaning = regexr(meaning, " ", "_")
replace meaning = regexr(meaning, " ", "_")
replace meaning = substr(meaning, 1, 32)

********************************************************************************
** create indicators from levels of meaning
********************************************************************************
levelsof meaning, local(med_names)

foreach med of local med_names {
    gen `med' = (meaning == "`med'")
}
collapse(max)`med_names', by(n_eid)

********************************************************************************
** merge with finaldata_unimputed.dta and replace . with 0
********************************************************************************
merge m:1 n_eid using "C:\Users\weissj\Downloads\UKB_PAPER9C_POLYPHPDDEMMORT\finaldata_unimputed.dta"

local med_list statins bptreat metformin alendronate_sodium allopurinol amitriptyline amlodipine aspirin atenolol atorvastatin bendroflumethiazide bisoprolol calcichew_d3_tablet candesartan_cilexetil cetirizine chondroitin_product citalopram co_codamol cod_liver_oil_capsule diclofenac doxazosin evening_primrose_oil fluoxetine garlic_product glucosamine_product ibuprofen insulin_product lansoprazole levothyroxine_sodium lisinopril metformin multivitamin_mineral_preparation multivitamins omega_3_fish_oil_supplement omeprazole paracetamol perindopril ramipril ranitidine salbutamol seretide_50_evohaler simvastatin thyroxine_product tramadol ventolin_100micrograms_inhaler vitamin_c_product
foreach var of local med_list `var' {
quietly
    replace `var' = 0 if `var' == .
}


keep n_eid statins bptreat metformin alendronate_sodium allopurinol amitriptyline amlodipine aspirin atenolol atorvastatin bendroflumethiazide bisoprolol calcichew_d3_tablet candesartan_cilexetil cetirizine chondroitin_product citalopram co_codamol cod_liver_oil_capsule diclofenac doxazosin evening_primrose_oil fluoxetine garlic_product glucosamine_product ibuprofen insulin_product lansoprazole levothyroxine_sodium lisinopril metformin multivitamin_mineral_preparation multivitamins omega_3_fish_oil_supplement omeprazole paracetamol perindopril ramipril ranitidine salbutamol seretide_50_evohaler simvastatin thyroxine_product tramadol ventolin_100micrograms_inhaler vitamin_c_product

recode statins bptreat metformin alendronate_sodium allopurinol amitriptyline amlodipine aspirin atenolol atorvastatin bendroflumethiazide bisoprolol calcichew_d3_tablet candesartan_cilexetil cetirizine chondroitin_product citalopram co_codamol cod_liver_oil_capsule diclofenac doxazosin evening_primrose_oil fluoxetine garlic_product glucosamine_product ibuprofen insulin_product lansoprazole levothyroxine_sodium lisinopril metformin multivitamin_mineral_preparation multivitamins omega_3_fish_oil_supplement omeprazole paracetamol perindopril ramipril ranitidine salbutamol seretide_50_evohaler simvastatin thyroxine_product tramadol ventolin_100micrograms_inhaler vitamin_c_product (1 = 2) (0 = 1)


/*
// Run the latent class analysis
quietly gsem (alendronate_sodium-bptreat <- ), family(bernoulli) link(logit) lclass(C 7)

// Estimate and store the latent class probabilities
predict pr_class*, latent(C)

// Convert probabilities to log-odds using a loop
foreach i of numlist 1/7 {
    gen ln_odds_class`i' = ln(pr_class`i' / (1 - pr_class`i'))
}
