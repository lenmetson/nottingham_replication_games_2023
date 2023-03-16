/****************************************************************

Programs that run RD estimation and inference using rdrobust and post those results to a postfile
Called by ./01-run-analysis.do

*****************************************************************/

/*** Program that creates postfile that is used by the program posteffects below ***/
program define start_postfile
	args filename 
	postfile filepost double(year) str8(party) str35(outcome) str80(sample) str50(options) double(Ntr Nco h b tau PVALrb CIlrb CIurb error diffmeans diffmeanspval diffmeanst diffmeanscil diffmeansciu) using "`filename'", replace
end

/** Program that closes the postfile */
program define end_postfile
   postclose filepost
end

/*** Program that performs RD estimation and inference and then posts results to postfile 'filepost' created by program start_postfile 
     Using version 2014 of rdbwselect for bandwidth selection and version 2016 of rdrobust for RD estimation and inference
***/
do rdbwselect_2014_functions.do  /* this file compiles the necessary MATA functions for rdbwselect_2014 to work*/
program define analyze_and_post_2014
	args year party y r subsamples ifvar
	/* Full sample */
	local ifs = ""
	capture noisily rdbwselect_2014 `y' `r' , bwselect(CCT) c(0) p(1)
	local h = e(h_CCT)
	local b = e(b_CCT)
	capture noisily rdrobust `y' `r' , h(`e(h_CCT)') b(`e(b_CCT)') c(0) p(1) 
	local rc = _rc
	if(`rc' == 0) post filepost (`year') ("`party'") ("`y'") ("all") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (`h')    (`b')  (e(tau_cl)) (e(pv_rb))  (e(ci_l_rb))  (e(ci_r_rb))  (`rc') (.) (.) (.) (.) (.)
	if(`rc' > 0)  post filepost (`year') ("`party'") ("`y'") ("all") (e(bwselect)) (.)          (.)      (.)       (.)        (.)         (.)        (.)            (.)         (`rc') (.) (.) (.) (.) (.)

	if(`subsamples' == 1) {
		/* Lame duck sample */
		local ifs = "if `ifvar'==1"
		capture noisily rdbwselect_2014 `y' `r' if `ifvar'==1, bwselect(CCT) c(0) p(1)
		local h = e(h_CCT)
		local b = e(b_CCT)
		capture noisily rdrobust `y' `r'  if `ifvar'==1,  h(`e(h_CCT)') b(`e(b_CCT)') c(0) p(1)  
		local rc   = _rc
		local rcLD = _rc
		if(`rc' == 0) post filepost (`year') ("`party'") ("`y'") ("if `ifvar'==1") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (`h')     (`b')  (e(tau_cl)) (e(pv_rb)) (e(ci_l_rb))  (e(ci_r_rb))  (`rc') (.) (.) (.) (.) (.)
		if(`rc' > 0)  post filepost (`year') ("`party'") ("`y'") ("if `ifvar'==1") (e(bwselect)) (.)           (.)      (.)       (.)        (.)          (.)        (.)          (.)        (`rc') (.) (.) (.) (.) (.)
		local diffLD      = e(tau_cl)
		local diffLD_bc   = e(tau_bc)
		local diffLDse_bc = e(se_tau_rb)
		
		/* Open seat sample */
		local ifs = "if `ifvar'==0" 
		capture noisily rdbwselect_2014 `y' `r' if `ifvar'==0, bwselect(CCT) c(0) p(1)
		local h = e(h_CCT)
		local b = e(b_CCT)
		capture noisily rdrobust `y' `r' if `ifvar'==0,  h(`e(h_CCT)') b(`e(b_CCT)') c(0) p(1)  
		local rc   = _rc
		local rcOS = _rc
		if(`rc' == 0) post filepost  (`year') ("`party'") ("`y'") ("if `ifvar'==0") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (`h')     (`b')  (e(tau_cl)) (e(pv_rb))  (e(ci_l_rb))  (e(ci_r_rb))  (`rc') (.) (.) (.) (.) (.)
		if(`rc' > 0)  post filepost  (`year') ("`party'") ("`y'") ("if `ifvar'==0") (e(bwselect)) (.)            (.)      (.)     (.)        (.)           (.)         (.)           (.)     (`rc') (.) (.) (.) (.) (.)
		local diffOS      = e(tau_cl)
		local diffOS_bc   = e(tau_bc)
		local diffOSse_bc = e(se_tau_rb)
		
		/* Difference between lamed duck and open seat estimates */
		local diff = `diffLD' - `diffOS'
		local tstat =  (`diffLD_bc' - `diffOS_bc') / (sqrt(`diffLDse_bc'^2+`diffOSse_bc'^2) )
		local pval = 2*(1-normal(abs(`tstat'))) 
		local cil =  (`diffLD_bc' - `diffOS_bc') - 1.96 * (sqrt(`diffLDse_bc'^2+`diffOSse_bc'^2) )
		local ciu =  (`diffLD_bc' - `diffOS_bc') + 1.96 * (sqrt(`diffLDse_bc'^2+`diffOSse_bc'^2) )
		if(`rcLD' == 0 & `rcOS' == 0) post filepost  (`year') ("`party'") ("`y'") ("LD-OS Difference") (e(bwselect)) (.)       (.)      (.)       (.)        (.)          (.)          (.)         (.)     (`rc') (`diff') (`pval') (`tstat') (`cil') (`ciu')
		if(`rcLD' >  0 | `rcOS' >  0) post filepost  (`year') ("`party'") ("`y'") ("LD-OS Difference") (e(bwselect)) (.)       (.)      (.)       (.)        (.)          (.)          (.)         (.)     (`rc')    (.)     (.)       (.)      (.)    (.)
	}
end


/*** Program that performs RD estimation and inference and then posts results to postfile 'filepost' created by program start_postfile 
     Using version 2016 of rdrobust for both bandwidth selection and RD estimation and inference
***/
program define analyze_and_post_2016
	args year party y r subsamples ifvar

	/* Full sample */
	local ifs = ""
	capture noisily rdrobust `y' `r' ,  bwselect(mserd) c(0) p(1) 
	local rc = _rc
	if(`rc' == 0) post filepost (`year') ("`party'") ("`y'") ("all") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (e(h_l)) (e(b_l))  (e(tau_cl)) (e(pv_rb))  (e(ci_l_rb))  (e(ci_r_rb))  (`rc') (.) (.) (.) (.) (.)
	if(`rc' > 0)  post filepost (`year') ("`party'") ("`y'") ("all") (e(bwselect)) (.)          (.)      (.)       (.)        (.)         (.)        (.)            (.)         (`rc') (.) (.) (.) (.) (.)

	if(`subsamples' == 1) {
		/* Lame duck sample */
		local ifs = "if `ifvar'==1"
		capture noisily rdrobust `y' `r'  if `ifvar'==1,  bwselect(mserd) c(0) p(1) 
		local rc   = _rc
		local rcLD = _rc
		if(`rc' == 0) post filepost (`year') ("`party'") ("`y'") ("if `ifvar'==1") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (e(h_l)) (e(b_l))  (e(tau_cl)) (e(pv_rb)) (e(ci_l_rb))  (e(ci_r_rb))  (`rc') (.) (.) (.) (.) (.)
		if(`rc' > 0)  post filepost (`year') ("`party'") ("`y'") ("if `ifvar'==1") (e(bwselect)) (.)           (.)      (.)       (.)        (.)          (.)        (.)          (.)        (`rc') (.) (.) (.) (.) (.)
		local diffLD      = e(tau_cl)
		local diffLD_bc   = e(tau_bc)
		local diffLDse_bc = e(se_tau_rb)
		
		/* Open seat sample */
		local ifs = "if `ifvar'==0" 
		capture noisily rdrobust `y' `r' if `ifvar'==0,  bwselect(mserd) c(0) p(1) 
		local rc   = _rc
		local rcOS = _rc
		if(`rc' == 0) post filepost  (`year') ("`party'") ("`y'") ("if `ifvar'==0") (e(bwselect)) (e(N_h_r)) (e(N_h_l)) (e(h_l)) (e(b_l))  (e(tau_cl)) (e(pv_rb))  (e(ci_l_rb))  (e(ci_r_rb))  (`rc') (.) (.) (.) (.) (.)
		if(`rc' > 0)  post filepost  (`year') ("`party'") ("`y'") ("if `ifvar'==0") (e(bwselect)) (.)            (.)      (.)       (.)        (.)           (.)         (.)           (.)     (`rc') (.) (.) (.) (.) (.)
		local diffOS      = e(tau_cl)
		local diffOS_bc   = e(tau_bc)
		local diffOSse_bc = e(se_tau_rb)
		
		/* Difference between lamed duck and open seat estimates */
		local diff = `diffLD' - `diffOS'
		local tstat =  (`diffLD_bc' - `diffOS_bc') / (sqrt(`diffLDse_bc'^2+`diffOSse_bc'^2) )
		local pval = 2*(1-normal(abs(`tstat'))) 
		local cil =  (`diffLD_bc' - `diffOS_bc') - 1.96 * (sqrt(`diffLDse_bc'^2+`diffOSse_bc'^2) )
		local ciu =  (`diffLD_bc' - `diffOS_bc') + 1.96 * (sqrt(`diffLDse_bc'^2+`diffOSse_bc'^2) )
		if(`rcLD' == 0 & `rcOS' == 0) post filepost  (`year') ("`party'") ("`y'") ("LD-OS Difference") (e(bwselect)) (.)       (.)      (.)       (.)        (.)          (.)          (.)         (.)     (`rc') (`diff') (`pval') (`tstat') (`cil') (`ciu')
		if(`rcLD' >  0 | `rcOS' >  0) post filepost  (`year') ("`party'") ("`y'") ("LD-OS Difference") (e(bwselect)) (.)       (.)      (.)       (.)        (.)          (.)          (.)         (.)     (`rc')    (.)     (.)       (.)      (.)    (.)
	}
end

