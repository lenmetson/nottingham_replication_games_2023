/***************************************************************

Stata Replication File for "The Incumbency Curse: Weak Parties, Term Limits, and Unfulfilled Accountability"
by Marko Klasnja and Rocio Titiunik

Note: 
These results use the 2016 version of rdrobust, but use the 2014 version of the bwselect function ("rdbwselect_2014") which is part of the 2016 rdrobust release to facilitate backward compatibility
Except for the bandwidth selectors, the 2016 and 2014 version of rdrobust produce identical results
These are the final results presented in the paper ==> the 2016 version of rdrobust was released after the article was accepted for publication
The results using the 2016 version of rdrobust with the 2016 version of the bwselect function are reported separately

May 11, 2016

************************************************************/
clear all
capture log close
set more off
program drop _all
pause off

global path "." 

/* Run functions used below*/
do "$path/functions.do"

/* Start log file */
log using "$path/replication-results-log.txt", text replace

/* Define globals */
global parties  PMDB PSDB DEM PP
global parties5 PMDB PSDB DEM PP PT

global covariates pib pibpc population winner_age winner_educ winner_male  numpar_candidates_eff dnorte dnordeste dsul dsudeste  dcentrooeste revenue_total expend_total

global pubgoods  housing_programfor1 housing_program_matfor1 expmun_asoc_totfor1 nxt4yr_mn_des_sum2_shdestot growth_func_admdir_totalfor1 


/******************************************
Load data
*******************************************/

use "$path/KlasnjaTitiunik-Brazil-data.dta", clear

/******************************************
Start postfile 
*******************************************/

start_postfile "$path/replication-results.dta" 

/******************************************
Table 1 -- Main paper 
*******************************************/
/* Overall sample*/
sum pib population votesh_n1 votesh_n2  party_PSDB_runs party_PMDB_runs party_DEM_runs party_PT_runs party_PP_runs numpar_candidates_eff
/* Sample of close races*/
sum pib population votesh_n1 votesh_n2  party_PSDB_runs party_PMDB_runs party_DEM_runs party_PT_runs  party_PP_runs numpar_candidates_eff  if abs(votesh_n1 - votesh_n2) <=2 

/******************************************
Table 2 -- RD effects on unconditional victory -- Main paper 
*******************************************/
/* Incumbent Party  */
analyze_and_post_2014 9999 "INC" "inc_party_wonfor1_b1" "mv_incparty" 0  ""         

/* Individual Parties */
foreach x of global parties  {
        analyze_and_post_2014 9999 "`x'" "party_`x'_wonfor1_b1" "mv_party_`x'" 0 ""         
}

/******************************************
Table 6 --  RD effects on unconditional victory for Incumbent and Open Seat samples -- Main paper
*******************************************/
/* Incumbent Party  */
analyze_and_post_2014 9999 "INC" "inc_party_wonfor1_b1" "mv_incparty" 1  "dlameduck_runs"         

/* Individual Parties */
foreach x of global parties  {
        analyze_and_post_2014 9999 "`x'" "party_`x'_wonfor1_b1" "mv_party_`x'" 1 "dlameduck_party_`x'_runs"         
}

/******************************************
Table 7 -- RD effects on proxy public good provision indicators (incumbent party) -- Main paper 
*******************************************/
foreach y of global pubgoods  {
		analyze_and_post_2014 9999 "INC" "`y'" "mv_incparty" 1 "dlameduck_runs"
}

/******************************************
Table 9 -- RD effect on uncondtional victory for PT -- Main paper
*******************************************/
analyze_and_post_2014 9999 "PT" "party_PT_wonfor1_b1" "mv_party_PT" 1 "dlameduck_party_PT_runs"

/******************************************
Table S3-- RD effect on predetermined covariates (incumbent party) -- Supplemental appendix
*******************************************/
/* Incumbent party */
foreach y of global covariates {
        analyze_and_post_2014 9999 "INC" "`y'" "mv_incparty" 1 "dlameduck_runs"
}

/******************************************
Table S4-- RD effects on unconditional victory at t-1 for individual parties -- Supplemental appendix
*******************************************/

/* Individual Parties */
foreach x of global parties5  {
    foreach y of varlist party_`x'_wonlag1  {
        analyze_and_post_2014 9999 "`x'" "`y'" "mv_party_`x'" 1 "dlameduck_party_`x'_runs"         
    }
}    
/* Density tests (p-values appear in histograms) */
rddensity mv_incparty, c(0) 
foreach x in PMDB PSDB DEM PP PT{
    rddensity mv_party_`x', c(0) 
}

/******************************************
Table S5 -- RD effects on unconditional victory with covariates -- Supplemental appendix
*******************************************/
*global covs pib population numpar_candidates_eff dnorte dsul revenue_total expend_total
* rescale public finance variables to avoid numerical problems
gen temp1 = expend_total/1000
gen temp2 = revenue_total/1000
global covs pib population numpar_candidates_eff dnorte dsul temp1 temp2

rdbwselect_2014 inc_party_wonfor1_b1 mv_incparty, bwselect(CCT) c(0) p(1)
local h = e(h_CCT)
local b = e(b_CCT)
rdrobust inc_party_wonfor1_b1 mv_incparty, c(0) p(1) covs($covs) h(`e(h_CCT)') b(`e(b_CCT)') 
post filepost        (9999) ("INC") ("inc_party_wonfor1_b1") ("all-withCOV") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (`h') (`b')  (e(tau_cl)) (e(pv_rb))  (e(ci_l_rb))  (e(ci_r_rb))  (0) (.) (.) (.) (.) (.)       

foreach x of global parties5  {
	display as error "Party `x'"
	rdbwselect_2014 party_`x'_wonfor1_b1  mv_party_`x', bwselect(CCT) c(0) p(1)
	local h = e(h_CCT)
	local b = e(b_CCT)
	rdrobust party_`x'_wonfor1_b1 mv_party_`x',  c(0) p(1) covs($covs) h(`e(h_CCT)') b(`e(b_CCT)') 
	post filepost (9999) ("`x'") ("party_`x'_wonfor1_b1") ("all-withCOV")  (e(bwselect)) (e(N_h_r)) (e(N_h_l))  (`h') (`b')  (e(tau_cl)) (e(pv_rb))  (e(ci_l_rb))  (e(ci_r_rb))  (0) (.) (.) (.) (.) (.)       
}

/******************************************
Table S6 -- RD yearly effects unconditional victory -- Supplemental appendix
*******************************************/
/* Incumbent Party **/
foreach year in 2000 2004 2008 {    
		preserve 
		keep if year == `year' 
		analyze_and_post_2014 `year' "INC" "inc_party_wonfor1_b1" "mv_incparty" 0 ""
		restore
}    

/* Individual Parties */
foreach year in 1996 2000 2004 2008 {    
   foreach x of global parties5  {
		preserve 
		keep if year == `year'        
		analyze_and_post_2014 `year' "`x'" "party_`x'_wonfor1_b1" "mv_party_`x'" 0 ""       
		restore	  	
   }
}

/******************************************
Table S7 -- RD yearly effects on conditional victory -- Supplemental appendix
*******************************************/
/* Incumbent Party **/
foreach year in 2000 2004 2008 {    
		preserve 
		keep if year == `year' 
		analyze_and_post_2014 `year' "INC" "inc_party_wonfor1" "mv_incparty" 0 ""
		restore
}    

/* Individual Parties */
foreach year in 1996 2000 2004 2008 {    
   foreach x of global parties5  {
		preserve 
		keep if year == `year'        
		analyze_and_post_2014 `year' "`x'" "party_`x'_wonfor1" "mv_party_`x'" 0 ""       
		restore	  	
   }
}

/******************************************
Table S8 -- RD effects on Candidacy at t+1 -- Supplemental appendix
*******************************************/

/* Incumbent Party  */
analyze_and_post_2014 9999 "INC" "inc_party_runsfor1" "mv_incparty" 0  ""         

/* Individual Parties */
foreach x of global parties5  {
        analyze_and_post_2014 9999 "`x'" "party_`x'_runsfor1" "mv_party_`x'" 0 ""         
}


/******************************************
Table S9 --  RD effects on Vote Margin at t+1 -- Supplemental appendix
*******************************************/

/* Incumbent Party  */
analyze_and_post_2014 9999 "INC" "mv_incpartyfor1" "mv_incparty" 0  ""         

/* Individual Parties */
foreach x of global parties5  {
        analyze_and_post_2014 9999 "`x'" "mv_party_`x'for1" "mv_party_`x'" 0 ""         
}

/******************************************
Table S10 --  RD effects on Conditional Victory at t+1 -- Supplemental appendix
*******************************************/

/* Incumbent Party  */
analyze_and_post_2014 9999 "INC" "inc_party_wonfor1" "mv_incparty" 0  ""         

/* Individual Parties */
foreach x of global parties5  {
        analyze_and_post_2014 9999 "`x'" "party_`x'_wonfor1" "mv_party_`x'" 0 ""         
}

/******************************************
Table S18 --  RD effects on proxy public good provision indicators (PT) -- Supplemental appendix
*******************************************/
foreach y of global pubgoods  {
		analyze_and_post_2014 9999 "PT" "`y'" "mv_party_PT" 1 "dlameduck_party_PT_runs"
}

/******************************************
Close postfile 
*******************************************/
end_postfile

/******************************************
Table S19 -- Mayoral Victories of Incumbent Party by Type of Seat, 2000-2012 -- Supplemental appendix
*******************************************/
tab inc_party_won dlameduck_runs

/******************************************

Party system weakness indicators, reported in text in main paper 
Note: this code modifies variables, so make sure NOT to run any other analysis after you run this code
      if you need to run further analysis, open the data again, i.e. type 'use "KlasnjaTitiunik-Brazil-data.dta", clear'
******************************************/
use "$path/KlasnjaTitiunik-Brazil-data.dta", clear
/* pedersen index */
foreach i in PP PT PMDB DEM PSDB {
	replace votesh_party_`i' = 0 if votesh_party_`i' == .
	replace votesh_party_`i'for1 = 0 if votesh_party_`i'for1 == .
	gen pedersen_`i' = abs(votesh_party_`i'for1-votesh_party_`i') 
	}
egen pedersen = rowtotal(pedersen*), missing
replace pedersen = pedersen/2
sum pedersen

/* difference in vote share for mayor and vereadores */
foreach i in PP PT PMDB DEM PSDB {
	replace ver_votesh_party_`i' = 0 if ver_votesh_party_`i' == .
	gen diff_ver_mayor_`i' = abs(ver_votesh_party_`i'-votesh_party_`i')
	}
egen diff_ver_mayor = rowmean(diff_ver_mayor*)
sum diff_ver_mayor

/* raw propensity to run at t+1 conditional on running at t */
foreach i in PP PT PMDB DEM PSDB {
	gen runs_`i' = .
	replace runs_`i' = 0 if party_`i'_runs == 1 ///
		& party_`i'_runsfor1 == 0
	replace runs_`i' = 1 if party_`i'_runs == 1 ///
		& party_`i'_runsfor1 == 1
	}
egen runs = rowmean(runs_*)
sum runs

/* change in size of coalition relative to size at t (%) */
foreach i in PP PT PMDB DEM PSDB {
	gen coal_diff_size_`i' = (abs(coal_size_party_`i'for1-coal_size_party_`i'))/coal_size_party_`i'
	}
egen coal_diff_size = rowmean(coal_diff_size*)
sum coal_diff_size
log close

/*******************************

Take a look at results are remove duplicate rows
******************************/
use "$path/replication-results.dta", clear
* remove duplicates that may have been generated above (bc when I set option ubsample=1 it also analyzes the full sample)
bys year outcome party sample: gen count= _N
bys year outcome party sample: gen indx= _n
keep if ((count ==1) | (count==2 & indx==1))
drop count indx 
bys year outcome party sample: gen count= _N
tab count
if(r(r)>1) display as error "STOP! THERE ARE STILL DUPLICATES"
save "$path/replication-results.dta", replace

