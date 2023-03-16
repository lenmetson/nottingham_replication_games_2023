##############################
###
## Prepare final tables for paper
## "The Incumbency Curse: Weak Parties, Term Limits, and Unfulfilled Accountability", APSR, forthcoming
## Marko Klasnja and Rocio Titiunik
## May 11, 2016
##############################

library(readstata13)
library(Hmisc)
options(width=150)

data = read.dta13("replication-results.dta")
path = "./tables"

r2  = function(x) sprintf("%.2f",as.numeric(x))
r3  = function(x) sprintf("%.3f",as.numeric(x))
r2  = function(x) format(as.numeric(x), format="f",digits=0,nsmall=2,big.mark=",", scientific=FALSE)
r3  = function(x) format(as.numeric(x), format="f",digits=0,nsmall=3,big.mark=",", scientific=FALSE)
r0  = function(x) format(as.numeric(x), format="f",digits=0,nsmall=0,big.mark=",", scientific=FALSE)

data$party[data$party=="INC"] = "Incumbent"
data$sub = substr(data$sample,4, 12)
data$subl = nchar(data$sample)
data$ld  = rep(9, nrow(data))
data$ld[substr(data$sample,data$subl-2, data$subl) == "==1"] = 1
data$ld[substr(data$sample,data$subl-2, data$subl) == "==0"] = 0

data$ld2  = rep(1, nrow(data))
data$ld2[substr(data$sample,data$subl-2, data$subl) == "==1"] = 2
data$ld2[substr(data$sample,data$subl-2, data$subl) == "==0"] = 3

partyorder = function(dat) {
    dat$partyindx = 9
    dat$partyindx[dat$party == "Incumbent" | dat$party=="INC"] = 1
    dat$partyindx[dat$party == "PMDB"] = 2
    dat$partyindx[dat$party == "PSDB"] = 3
    dat$partyindx[dat$party == "DEM"] = 4
    dat$partyindx[dat$party == "PP"] = 5
    dat$partyindx[dat$party == "PT"] = 6
    return(dat[order(dat$partyindx),])
}

partyordersub = function(dat) {
    dat$partyindx = 9
    dat$partyindx[dat$party == "Incumbent" | dat$party=="INC"] = 1
    dat$partyindx[dat$party == "PMDB"] = 2
    dat$partyindx[dat$party == "PSDB"] = 3
    dat$partyindx[dat$party == "DEM"] = 4
    dat$partyindx[dat$party == "PP"] = 5
    dat$partyindx[dat$party == "PT"] = 6
    dat = dat[order(dat$partyindx),]
    return(dat[order(dat$partyindx,dat$ld2),])
}

####################################################################################
##
## Tables in main paper
###
#####################################################################################

#################
## Table 2 -- RD effects on unconditional victory
#################
indx = (data$year == 9999 &  data$sample == "all" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1_b1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1_b1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1_b1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1_b1")  |
         (data$party == "PP" & data$outcome == "party_PP_wonfor1_b1") )
    )
dat = data[indx,]
dat = partyorder(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))
print(dat)

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
rownms  = c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}")
colnms  = c("Party", "Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 7
cgroup = "\\textsc{Outcome: Unconditional Victory $t+1$}"
table=latex(tab, file=paste(path,"/table2-IA-all.txt",sep=""), title="", table.env=FALSE, center="none", col.just = c("l",rep("c",ncol(tab)-1)),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE)

      #n.rgroup = n.rgroup, rgroup=rgroup, rowname=rowname)
rm(dat,ci, tab)

#################
## Table 6 -- RD effects on unconditional victory for Incumbent and Open Seat samples 
################# 
indx = (data$year == 9999 &  data$sub == "dlameduck" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1_b1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1_b1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1_b1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1_b1")  |
         (data$party == "PP" & data$outcome == "party_PP_wonfor1_b1") )
    )
    
subs = rep(c("Incumbent", "Open Seat"), 5)
dat = data[indx,]
dat = partyordersub(dat)
print(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, subs, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)

## add last column wiht CI of the difference
indx = (data$year == 9999 &  data$sample == "LD-OS Difference" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1_b1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1_b1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1_b1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1_b1")  |
         (data$party == "PP" & data$outcome == "party_PP_wonfor1_b1") )
    )
dd = data[indx,]
dd = partyorder(dd)
print(dd)
diffci =  paste("[\\;$", r3(dd$diffmeanscil), "$\\;,\\;$", r3(dd$diffmeansciu), "$\\;]", sep="")
diffcol = NULL
for(i in 1:nrow(dd)) {
    diffcol = c(diffcol, r3(dd$diffmeans[i]), diffci[i])
}
tab = cbind(tab, diffcol)

# Now remove two first columns, because I pass them as arguments in latex() function below
tab= tab[,c(-1,-2)]

colnms  = c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$", "Difference")
rownms  = rep(c("Incumbent Sample", "Open Seat Sample"), 5)
n.cgroup = ncol(tab)
cgroup = "\\textsc{Outcome: Unconditional Victory $t+1$}"
rgroup = c("\\textsc{Incumbent Party}", "\\textsc{PMDB}",  "\\textsc{PSDB}",  "\\textsc{DEM}", "\\textsc{PP}")
n.rgroup = c(2,2,2,2,2)
table=latex(tab, file=paste(path,"/table6-IA-subsamples.txt",sep=""), title="",
            table.env=FALSE, center="none", col.just = c(rep("c",ncol(tab)-1), "|c"),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup,
            rgroup=rgroup, rowname=rownms)
rm(dat,ci,dd,tab)

#################
## Table 7 -- RD effects on proxy public good provision indicators (incumbent party) 
#################
# create index to put in order
data$indxsort = 9
data$indxsort[data$outcome == "nxt4yr_mn_des_sum2_shdestot"]  = 1
data$indxsort[data$outcome == "growth_func_admdir_totalfor1"] = 2
data$indxsort[data$outcome == "expmun_asoc_totfor1"]          = 3
data$indxsort[data$outcome == "housing_programfor1"]          = 4
data$indxsort[data$outcome == "housing_program_matfor1"]      = 5
data$indxsort2 = 9
data$indxsort2[data$sample =="all"] = 1
data$indxsort2[data$ld == 1] = 2
data$indxsort2[data$ld == 0] = 3

dataORD = data[order(data$indxsort,data$indxsort2),]

indx = ((dataORD$outcome == "nxt4yr_mn_des_sum2_shdestot" | dataORD$outcome == "growth_func_admdir_totalfor1" | dataORD$outcome == "expmun_asoc_totfor1" |
         dataORD$outcome ==  "housing_programfor1" | dataORD$outcome == "housing_program_matfor1")   & dataORD$sample != "LD-OS Difference" &
        dataORD$party == "Incumbent")
dat = dataORD[indx,]

rgroup = c("Share HESA Expend.", "Adm. Employment Growth", "Social Assistance Expend.", "Housing Program", "Housing Materials Program")
subs = rep(c("All Seats", "Incumbent", "Open Seat"), 5)
ci = paste("[\\;$", r2(dat$CIlrb), "$\\;,\\;$", r2(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r2(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, subs, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)

## add last column wiht CI of the difference
indx = ((dataORD$outcome == "nxt4yr_mn_des_sum2_shdestot" | dataORD$outcome == "growth_func_admdir_totalfor1" | dataORD$outcome == "expmun_asoc_totfor1" |
        dataORD$outcome ==  "housing_programfor1" | dataORD$outcome == "housing_program_matfor1")   & dataORD$sample == "LD-OS Difference" & dataORD$party == "Incumbent")
dd = dataORD[indx,]
diffci =  paste("[\\;$", r2(dd$diffmeanscil), "$\\;,\\;$", r2(dd$diffmeansciu), "$\\;]", sep="")
diffcol = NULL
for(i in 1:nrow(dd)) {
    diffcol = c(diffcol, "", r2(dd$diffmeans[i]), diffci[i])
}
tab = cbind(tab, diffcol)

# Now remove first and second column, since I pass the necessary info in latex() function below
tab= tab[,c(-1,-2)]

colnms  =  c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$", "Difference")
rownms  = rep(c("All Seats", "Incumbent Sample", "Open Seat Sample"), 5)
n.cgroup = ncol(tab)
cgroup = "\\textsc{Outcome: Various Proxy Measures of Public Good Provision}"
rgroup = c("Share HESA Expend.", "Adm. Employment Growth", "Social Assistance Expend.", "Housing Program", "Housing Materials Program")
n.rgroup = rep(3, 5)
table=latex(tab, file=paste(path,"/table7-publicgoods-Inc.txt",sep=""), title="", table.env=FALSE,
            center="none", col.just = c(rep("c",ncol(tab)-1), "|c"),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup,
            rgroup=rgroup, rowname=rownms)

rm(dataORD, dat,ci, tab)

#################
## Table 9 -- RD effect on unconditional victory for PT
#################
indx = (data$year == 9999 &  (data$party == "PT" & data$outcome == "party_PT_wonfor1_b1") & (data$sample == "all" | data$sub == "dlameduck"))
    
subs = rep(c("All Seats", "Incumbent", "Open Seat"), 1)
dat = data[indx,]
dat = partyordersub(dat)
print(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, subs, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)

## add last column wiht CI of the difference
indx = (data$year == 9999 &  (data$party == "PT" & data$outcome == "party_PT_wonfor1_b1") & data$sample == "LD-OS Difference" )
dd = data[indx,]

diffci =  paste("[\\;$", r3(dd$diffmeanscil), "$\\;,\\;$", r3(dd$diffmeansciu), "$\\;]", sep="")
diffcol = NULL
for(i in 1:nrow(dd)) {
    diffcol = c(diffcol, "", r3(dd$diffmeans[i]), diffci[i])
}
tab = cbind(tab, diffcol)

# Now remove two first columns, because I pass them as arguments in latex() function below
tab= tab[,c(-1,-2)]

colnms  = c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$", "Difference")
rownms  = rep(c("All Seats", "Incumbent Sample", "Open Seat Sample"), 1)
n.cgroup = ncol(tab)
cgroup = "\\textsc{Outcome: PT Unconditional Victory $t+1$}"
rgroup = c("\\textsc{PT}")
n.rgroup = c(3)
table=latex(tab, file=paste(path,"/table9-IA-PT.txt",sep=""), title="",
            table.env=FALSE, center="none", col.just = c(rep("c",ncol(tab)-1), "|c"),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, rowname=rownms)
rm(dat, ci, dataORD)

####################################################################################
##
## END of tables in main paper
##
#####################################################################################


####################################################################################
##
## Tables in Supplemental Appendix
##
#####################################################################################

##############################
## Table S3 -- RD effect on predetermined covariates (incumbent party) 
#############################
indx = ((data$outcome == "pibpc" | data$outcome == "population" | data$outcome ==  "winner_age" |
         data$outcome == "winner_educ" | data$outcome == "winner_male" | data$outcome == "numpar_candidates_eff" |
         data$outcome == "dcentrooeste" | data$outcome == "dnordeste" | data$outcome == "dnorte" | data$outcome == "dsudeste" | 
         data$outcome == "expend_total" | data$outcome == "revenue_total")
       & data$sample != "LD-OS Difference" & data$party == "Incumbent")
dat = data[indx,]
print(dat)

subs = rep(c("All Seats", "Incumbent", "Open Seat"), 12)
ci = paste("[\\;$", r2(dat$CIlrb), "$\\;,\\;$", r2(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r2(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, subs, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)

# Now remove first and second column, since I pass the necessary info in latex() function below
tab= tab[,c(-1,-2)]

colnms  =  c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$", "Difference")
rownms  = rep(c("All Seats", "Incumbent Sample", "Open Seat Sample"), 12)
n.cgroup = ncol(tab)
cgroup = "\\textsc{RD Effects on Several Covariates}"
rgroup = c("GDP per capita", "Population", "Winner age", "Winner educ", "Winner male", "No. effective parties",
           "North", "Northeast", "Southeast", "Centerwest", "Total revenue", "Total expenditures")
n.rgroup = rep(3, 12)
table=latex(tab, file=paste(path,"/tableS3-cov-Inc.txt",sep=""), title="", table.env=FALSE,
            center="none", col.just = c(rep("c",ncol(tab)-1), "c"),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup,
            rgroup=rgroup, rowname=rownms)
rm(dat)

##############################
## Table S4 -- RD effects on unconditional victory at t-1 for individual parties
#############################
nv = 5
indx = (data$year == 9999 &  data$sample != "LD-OS Difference" & 
        ((data$party == "PMDB" & data$outcome == "party_PMDB_wonlag1_b1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonlag1_b1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonlag1_b1")  |
         (data$party == "PT" & data$outcome == "party_PT_wonlag1_b1")  |         
         (data$party == "PP" & data$outcome == "party_PP_wonlag1_b1"))
    )

dat = data[indx,]
dat = partyordersub(dat)
print(dat)

subs = rep(c("All Seats", "Incumbent", "Open Seat"), nv)
ci = paste("[\\;$", r2(dat$CIlrb), "$\\;,\\;$", r2(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r2(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, subs, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)

# Now remove first and second column, since I pass the necessary info in latex() function below
tab= tab[,c(-1,-2)]

colnms  =  c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$", "Difference")
rownms  = rep(c("All Seats", "Incumbent Sample", "Open Seat Sample"), nv)
n.cgroup = ncol(tab)
cgroup = "\\textsc{Outcome: Unconditional Victory $t-1$}"
rgroup = c("PMDB","PSDB", "DEM", "PP", "PT")
n.rgroup = rep(3, nv)
table=latex(tab, file=paste(path,"/tableS4-prevvic.txt",sep=""), title="", table.env=FALSE,
            center="none", col.just = c(rep("c",ncol(tab)-1), "c"),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup,
            rgroup=rgroup, rowname=rownms)
rm(dat)

#############################
## Table S5 -- RD effects on unconditional victory with covariates
#############################
indx = (data$year == 9999 &  data$sample == "all-withCOV" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1_b1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1_b1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1_b1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1_b1")  |
         (data$party == "PP" & data$outcome == "party_PP_wonfor1_b1") )
    )
dat = data[indx,]
dat = partyorder(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))
print(dat)

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
rownms  = c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}")
colnms  = c("Party", "Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 7
cgroup = "\\textsc{Outcome: Unconditional Victory $t+1$ with covariates}"
table=latex(tab, file=paste(path,"/tableS5-IAwithcov.txt",sep=""), title="", table.env=FALSE, center="none", col.just = c("l",rep("c",ncol(tab)-1)),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE)

 #n.rgroup = n.rgroup, rgroup=rgroup, rowname=rowname)
rm(dat,ci, tab)

#############################
## Table S6  -- RD yearly effects unconditional victory
#############################
nv = 6
indx = (data$year != 9999 &  data$sample == "all" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1_b1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1_b1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1_b1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1_b1")  |
         (data$party == "PT" & data$outcome == "party_PT_wonfor1_b1")  |         
         (data$party == "PP" & data$outcome == "party_PP_wonfor1_b1") )
    )

dat = data[indx,]
dat = dat[order(dat$year),]
print(dat)
ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
# Now remove first column, since I pass the necessary info in latex() function below
tab= tab[,c(-1)]

rownms  = c(rep(c("\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}", "\\textsc{PT}"), 1),
            rep(c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}", "\\textsc{PT}"), 3))
colnms  = c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 6
cgroup = "\\textsc{Outcome: Unconditional Victory $t+1$}"
rgroup = c("1996","2000", "2004", "2008")
n.rgroup = c(rep(nv-1, 1), rep(nv,3))

table=latex(tab, file=paste(path,"/tableS6-IA-yearly.txt",sep=""), title="", table.env=FALSE, center="none", col.just = c(rep("c",ncol(tab))),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup, rgroup=rgroup, rowname=rownms)
rm(dat, ci, tab)

#############################
## Table S7 -- RD yearly effects on conditional victory
#############################
nv = 6
indx = (data$year != 9999 &  data$sample == "all" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1")  |
         (data$party == "PT" & data$outcome == "party_PT_wonfor1")  |         
         (data$party == "PP" & data$outcome == "party_PP_wonfor1") )
    )

dat = data[indx,]
dat = dat[order(dat$year),]
print(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
# Now remove first column, since I pass the necessary info in latex() function below
tab= tab[,c(-1)]

rownms  = c(rep(c("\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}","\\textsc{PT}"), 1),
            rep(c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}", "\\textsc{PT}"), 3))
colnms  = c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 6
cgroup = "\\textsc{Outcome: Conditional Victory $t+1$}"
rgroup = c("1996","2000", "2004", "2008")
n.rgroup = c(rep(nv-1, 1), rep(nv,3))

table=latex(tab, file=paste(path,"/tableS7-IAcondvic-yearly.txt",sep=""), title="", table.env=FALSE, center="none",
            col.just = c(rep("c",ncol(tab))),
            n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup,
            rgroup=rgroup, rowname=rownms)
rm(dat, ci, tab)

#################
## Table S8 --  RD effects on Candidacy at t+1
#################
indx = (data$year == 9999 &  data$sample == "all" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_runsfor1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_runsfor1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_runsfor1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_runsfor1")  |
         (data$party == "PT" & data$outcome == "party_PT_runsfor1")  |         
         (data$party == "PP" & data$outcome == "party_PP_runsfor1") )
    )
dat = data[indx,]
dat = partyorder(dat)
print(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
rownms  = c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}", "\\textsc{PT}")
colnms  = c("Party", "Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 7
cgroup = "\\textsc{Outcome: Candidacy $t+1$}"
table=latex(tab, file=paste(path,"/tableS8-IAcand-all.txt",sep=""), title="", table.env=FALSE, center="none",
            col.just = c("l",rep("c",ncol(tab)-1)), n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE)

#################
## Table S9 --  RD effects on Vote Margin at t+1
#################
indx = (data$year == 9999 &  data$sample == "all" & 
        ((data$party == "Incumbent"  & data$outcome == "mv_incpartyfor1")  |
         (data$party == "PMDB" & data$outcome == "mv_party_PMDBfor1")  |
         (data$party == "PSDB" & data$outcome == "mv_party_PSDBfor1")  |
         (data$party == "DEM" & data$outcome == "mv_party_DEMfor1")  |
         (data$party == "PT" & data$outcome == "mv_party_PTfor1")  |         
         (data$party == "PP" & data$outcome == "mv_party_PPfor1") )
    )

dat = data[indx,]
dat = partyorder(dat)
print(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
rownms  = c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}", "\\textsc{PT}")
colnms  = c("Party", "Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 7
cgroup = "\\textsc{Outcome: Vote Margin $t+1$}"
table=latex(tab, file=paste(path,"/tableS9-IAmargin-all.txt",sep=""), title="", table.env=FALSE, center="none",
            col.just = c("l",rep("c",ncol(tab)-1)), n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE)
rm(dat, ci, tab)

#################
## Table S10 --  RD effects on Conditional Victory at t+1
#################
indx = (data$year == 9999 &  data$sample == "all" & 
        ((data$party == "Incumbent"  & data$outcome == "inc_party_wonfor1")  |
         (data$party == "PMDB" & data$outcome == "party_PMDB_wonfor1")  |
         (data$party == "PSDB" & data$outcome == "party_PSDB_wonfor1")  |
         (data$party == "DEM" & data$outcome == "party_DEM_wonfor1")  |
         (data$party == "PT" & data$outcome == "party_PT_wonfor1")  |         
         (data$party == "PP" & data$outcome == "party_PP_wonfor1") )
    )

dat = data[indx,]
dat = partyorder(dat)
print(dat)

ci = paste("[\\;$", r3(dat$CIlrb), "$\\;,\\;$", r3(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r3(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)
tab[,1] = paste0("\\textsc{", tab[,1], "}")
rownms  = c("\\textsc{Incumbent}", "\\textsc{PMDB}", "\\textsc{PSDB}", "\\textsc{DEM}", "\\textsc{PP}", "\\textsc{PT}")
colnms  = c("Party", "Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$")
n.cgroup = 7
cgroup = "\\textsc{Outcome: Conditional Victory $t+1$}"
table=latex(tab, file=paste(path,"/tableS10-IAcondvic-all.txt",sep=""), title="", table.env=FALSE, center="none",
            col.just = c("l",rep("c",ncol(tab)-1)), n.cgroup = n.cgroup, cgroup=cgroup, colheads=colnms, booktabs = TRUE)
rm(dat, ci, tab)

#################
## Table S18 -- RD effects on proxy public good provision indicators (PT) 
#################
# create index to put in order
data$indxsort = 9
data$indxsort[data$outcome == "nxt4yr_mn_des_sum2_shdestot"]  = 1
data$indxsort[data$outcome == "growth_func_admdir_totalfor1"] = 2
data$indxsort[data$outcome == "expmun_asoc_totfor1"]          = 3
data$indxsort[data$outcome == "housing_programfor1"]          = 4
data$indxsort[data$outcome == "housing_program_matfor1"]      = 5
data$indxsort2 = 9
data$indxsort2[data$sample =="all"] = 1
data$indxsort2[data$ld == 1] = 2
data$indxsort2[data$ld == 0] = 3

dataORD = data[order(data$indxsort,data$indxsort2),]

indx = ((dataORD$outcome == "nxt4yr_mn_des_sum2_shdestot" | dataORD$outcome == "growth_func_admdir_totalfor1" | dataORD$outcome == "expmun_asoc_totfor1" |
         dataORD$outcome ==  "housing_programfor1" | dataORD$outcome == "housing_program_matfor1")   & dataORD$sample != "LD-OS Difference" &
        dataORD$party == "PT")
dat = dataORD[indx,]

rgroup = c("Share HESA Expend.", "Adm. Employment Growth", "Social Assistance Expend.", "Housing Program", "Housing Materials Program")
subs = rep(c("All Seats", "Incumbent", "Open Seat"), 5)
ci = paste("[\\;$", r2(dat$CIlrb), "$\\;,\\;$", r2(dat$CIurb), "$\\;]", sep="")
dat[,c("tau", "h")] = apply(dat[,c("tau", "h")], c(1,2), r2)
dat[,c("PVALrb")] = r2(dat[,c( "PVALrb")])
dat[,c("Ntr", "Nco")] =  apply(dat[,c("Ntr", "Nco")], c(1,2), r0)
dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")] = apply(dat[,c("tau", "PVALrb", "h", "Ntr", "Nco")], c(1,2), function(x) paste0("$",x,"$"))

tab = cbind(dat$party, subs, dat$tau, ci, dat$PVALrb, dat$h, dat$Ntr, dat$Nco)

## add last column wiht CI of the difference
indx = ((dataORD$outcome == "nxt4yr_mn_des_sum2_shdestot" | dataORD$outcome == "growth_func_admdir_totalfor1" | dataORD$outcome == "expmun_asoc_totfor1" |
        dataORD$outcome ==  "housing_programfor1" | dataORD$outcome == "housing_program_matfor1")   & dataORD$sample == "LD-OS Difference" & dataORD$party == "PT")
dd = dataORD[indx,]
diffci =  paste("[\\;$", r2(dd$diffmeanscil), "$\\;,\\;$", r2(dd$diffmeansciu), "$\\;]", sep="")
diffcol = NULL
for(i in 1:nrow(dd)) {
    diffcol = c(diffcol, "", r2(dd$diffmeans[i]), diffci[i])
}
tab = cbind(tab, diffcol)

# Now remove first and second column, since I pass the necessary info in latex() function below
tab= tab[,c(-1,-2)]

colnms  =  c("Estimate", "95\\% CI", "p-val", "$h$", "$n_\\mathtt{tr}$", "$n_\\mathtt{co}$", "Difference")
rownms  = rep(c("All Seats", "Incumbent Sample", "Open Seat Sample"), 5)
n.cgroup = ncol(tab)
cgroup = "\\textsc{Outcome: Various Proxy Measures of Public Good Provision}"
rgroup = c("Share HESA Expend.", "Adm. Employment Growth", "Social Assistance Expend.", "Housing Program", "Housing Materials Program")
n.rgroup = rep(3, 5)
table=latex(tab, file=paste(path,"/tableS18-publicgoods-PT.txt",sep=""), title="", table.env=FALSE,
            center="none", col.just = c(rep("c",ncol(tab)-1), "|c"),
            n.cgroup = n.cgroup, cgroup=cgroup,
            colheads=colnms, booktabs = TRUE, n.rgroup = n.rgroup,
            rgroup=rgroup, rowname=rownms)
rm(dat, dataORD, ci, tab)

####################################################################################
##
## END of tables in Supplemental Appendix
##
#####################################################################################
