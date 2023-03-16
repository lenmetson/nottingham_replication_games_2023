################################################
#
# Do country-by-country Incumbent Party Analysis for paper
# "The Incumbency Curse: Weak Parties, Term Limits, and Unfulfilled Accountability", APSR, forthcoming
# Marko Klasnja and Rocio Titiunik
# May 11, 2016
################################################
library(foreign)
library(rdrobust)
library(readstata13)

#########################
# RD effects on for incumbent party in various countries: Victory t+1 (unconditional)
# Set axis labels and titles 
#
#######################
ylab = "Victory t+1 (=1 if ran and won, =0 else)"
xlab = "Incumbent Party Vote Margin at t"
tit.reelec    = ""
tit.noreelec  = ""
path = "./figures"

#########################
#
# Colombia
#
########################
data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$country == "Colombia",]
table(data$year)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)
coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = signif(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path, "/Colombia-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent,  x.lim= c(-30,30), y.lim=c(0,1), 
       y.label = ylab, x.label = xlab, title = "", binselect = "qsmv") 
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                               rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()

#########################
#
# Mexico
#
########################

data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$country == "Mexico",]
table(data$year)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)

coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = round(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path, "/Mexico-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent, y.label = ylab, x.label = xlab, title = "", 
       binselect = "qsmv", x.lim= c(-30,30), y.lim=c(0,1), cex.lab=1.3)
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                                   rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()

#########################
#
# Peru
#
########################

data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$country == "Peru",]
table(data$year)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)
coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = round(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path,"/Peru-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent, x.lim= c(-30,30), y.lim=c(0,1), y.label = ylab, x.label = xlab, 
       title = "", binselect = "qsmv", cex.lab=1.3)
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                                   rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()

#########################
#
# Costa Rica
#
########################
data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$country == "Costa Rica",]
table(data$year)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)
coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = round(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path,"/CostaRica-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent, x.lim= c(-30,30), y.lim=c(0,1),y.label = ylab, x.label = xlab, 
       title = "", numbinr=40, numbinl=40, cex.lab=1.3)
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                                   rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()

#########################
#
# Chile
#
########################
data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$country == "Chile",]
table(data$year)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)
coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = round(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path,"/Chile-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent, x.lim= c(-30,30), y.lim=c(0,1), y.label = ylab, x.label = xlab, 
       title = "", binselect = "qsmv", cex.lab=1.3)
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                                   rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()

#########################
#
# Pooled Analysis: term-limited versus non-term-limited countries
#
########################

# Term-limited countries
data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$term_lim == 1,]
table(data$country)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)
coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = round(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path,"/TerlimCountries-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent, x.lim= c(-30,30), y.lim=c(0,1), y.label = ylab, x.label = xlab, 
       title = "", binselect = "qsmv", cex.lab=1.3)
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                                   rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()

# Non-term-limited countries
data = read.dta13("./KlasnjaTitiunik-LAcountries-data.dta")
data = data[data$term_lim == 0,]
table(data$country)

rout = rdrobust(y=data$post_winning_incumbent_unc, x=data$mv_incumbent)
coef = round(rout$coef[1], digits=2)
pval = round(rout$pv[3], digits=2)
cil = round(rout$ci[3,1], digits=2)
ciu = round(rout$ci[3,2], digits=2)
print(rout)

pdf(paste(path,"/ReelecCountries-IncParty-UncVic.pdf", sep=""))
rdplot(y=data$post_winning_incumbent_unc, x=data$mv_incumbent, x.lim= c(-30,30), y.lim=c(0,1), y.label = ylab, x.label = xlab, 
       title = "", binselect = "qsmv", cex.lab=1.3)
legend(x=-33, y=1.11, legend=paste("Local-linear RD effect: ", coef, "\nRobust 95% CI:  [", cil,", ", ciu, "] \n","Observations: ", 
                                   rout$N_l, "(Tr) ", rout$N_r," (Co) \n",sep=""), cex=1.3, bty="y", box.col="black", y.intersp=1.6, bg="white")
dev.off()
