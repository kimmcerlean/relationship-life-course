
********************************************************************************
* Project: Relationship Life Course Analysis
* Owner: Kimberly McErlean
* Started: September 2024
* File: predict_clusters
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes the compiled cluster file
* And predicts membership in a given cluster by various characteristics

use "$created_data/PSID_clusters_analysis.dta", clear

********************************************************************************
* Between cluster descriptives
********************************************************************************
// Couple education group
tab couple_educ_type mc5_factor, row // how to do this with mi? Is this just where we have to use regression?
// yes, the below with no controls matches this exactly

global educ_controls "i.raceth_woman i.same_race c.age_man1 c.age_woman1 c.couple_earnings_t1"

mi estimate: mlogit mc5_factor i.couple_educ_type, cluster(couple_id) baseoutcome(1)
mimrgns couple_educ_type, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) pwcompare
mimrgns couple_educ_type, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post // predict(pr) // pwcompare
outreg2 using "$models/couple_educ.xls", stats(coef se ci_low ci_high) sideway label(proper) ctitle(base) replace

mi estimate, saving("$models/couple_educ", replace) post: mlogit mc5_factor i.couple_educ_type $educ_controls, cluster(couple_id) baseoutcome(1)
mimrgns couple_educ_type, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post // predict(pr) // pwcompare
outreg2 using "$models/couple_educ.xls", stats(coef se ci_low ci_high) sideway label(insert) ctitle(controls) append

// Woman's race/ethnicity
global race_controls "i.couple_educ_type i.same_race c.age_man1 c.age_woman1 c.couple_earnings_t1"

tab raceth_woman mc5_factor, row 
tab same_race mc5_factor, row 

mi estimate: mlogit mc5_factor i.raceth_woman, cluster(couple_id) baseoutcome(1)
mimrgns raceth_woman, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/raceth.xls", stats(coef se ci_low ci_high) sideway label(proper) ctitle(base) replace

mi estimate, saving("$models/raceth", replace) post: mlogit mc5_factor i.raceth_woman $race_controls, cluster(couple_id) baseoutcome(1)
mimrgns raceth_woman, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/raceth.xls", stats(coef se ci_low ci_high) sideway label(insert) ctitle(controls) append


// Woman's birth cohort
global bc_controls "i.raceth_woman i.same_race c.age_man1 c.couple_earnings_t1 i.couple_educ_type"

tab bcohort_woman mc5_factor, row 

mi estimate: mlogit mc5_factor i.bcohort_woman, cluster(couple_id) baseoutcome(1)
mimrgns bcohort_woman, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/bcohort_woman.xls", stats(coef se ci_low ci_high) sideway label(proper) ctitle(base) replace

mi estimate, saving("$models/bcohort_woman", replace) post: mlogit mc5_factor i.bcohort_woman $bc_controls, cluster(couple_id) baseoutcome(1)
mimrgns bcohort_woman, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/bcohort_woman.xls", stats(coef se ci_low ci_high) sideway label(insert) ctitle(controls) append

// Woman's age group
global age_controls "i.raceth_woman i.same_race c.age_man1 c.couple_earnings_t1 i.couple_educ_type"

tab age_gp_woman1 mc5_factor, row 

mi estimate: mlogit mc5_factor i.age_gp_woman1, cluster(couple_id) baseoutcome(1)
mimrgns age_gp_woman1, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/age_woman.xls", stats(coef se ci_low ci_high) sideway label(proper) ctitle(base) replace

mi estimate, saving("$models/age_woman", replace) post: mlogit mc5_factor i.age_gp_woman1 $age_controls, cluster(couple_id) baseoutcome(1)
mimrgns age_gp_woman1, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/age_woman.xls", stats(coef se ci_low ci_high) sideway label(insert) ctitle(controls) append

// Relationship cohort
global rc_controls "i.raceth_woman i.same_race c.age_man1 c.age_woman1 c.couple_earnings_t1 i.couple_educ_type"

tab rel_cohort mc5_factor, row 

mi estimate: mlogit mc5_factor i.rel_cohort, cluster(couple_id) baseoutcome(1)
mimrgns rel_cohort, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/rel_cohort.xls", stats(coef se ci_low ci_high) sideway label(proper) ctitle(base) replace

mi estimate, saving("$models/rel_cohort", replace) post: mlogit mc5_factor i.rel_cohort $rc_controls, cluster(couple_id) baseoutcome(1)
mimrgns rel_cohort, predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) predict(outcome(4)) predict(outcome(5)) post
outreg2 using "$models/rel_cohort.xls", stats(coef se ci_low ci_high) sideway label(insert) ctitle(controls) append