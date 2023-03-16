##############################
###
## Prepare final figures for paper
## "The Incumbency Curse: Weak Parties, Term Limits, and Unfulfilled Accountability", APSR, forthcoming
## Marko Klasnja and Rocio Titiunik
## May 11, 2016
##############################

rm(list = ls())
library(rdrobust)
library(readstata13)
library(foreign)
library(Hmisc)
library(lattice)
library(ggplot2)
library(plyr)
options(width=150)
source("./rddensity.R")  

data =  read.dta13("./KlasnjaTitiunik-Brazil-data.dta")
path = "./figures"

####################################################################################
##
## Figures in main paper
###
#####################################################################################

########################################
#
# Figure 1: Incumbent party, Unconditional Victory
#
#####################################

y = data$inc_party_wonfor1_b1
x = data$mv_incparty
outnm = "Victoryt+1-UNCON"
partynm = "Inc"
sample = 'Full'
tit = paste('RD effect on Victory t+1 for Incumbent Party', sep='')
tit = ''
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "Incumbent Party Vote Margin at t",
       y.label = 'Victory t+1 (=1 if ran and won, =0 else)', 
       x.lim = c(-25, 25), y.lim = c(0,0.7))
abline(v=0, lwd=2, col='red')
dev.off()

########################################
#
# Figure 2: Incumbent party, Unconditional Victor t+1, Open versus Incumbent Samples
#
#####################################
y = data$inc_party_wonfor1_b1[data$dlameduck_runs==1]
x = data$mv_incparty[data$dlameduck_runs==1]
outnm = "Victoryt+1-UNCON"
partynm = "Inc"
sample = 'Lameduck'
tit = paste('RD effect on Victory t+1 for Incumbent Party', sep='')
tit = ''
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')

pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit,
       x.label = "Incumbent Party Vote Margin at t", y.label ='Victory t+1 (=1 if ran and won, =0 else)',
       x.lim = c(-25, 25), y.lim = c(0,0.7))
abline(v=0, lwd=2, col='red')
dev.off()

y = data$inc_party_wonfor1_b1[data$dlameduck_runs==0]
x = data$mv_incparty[data$dlameduck_runs==0]
outnm = "Victoryt+1-UNCON"
partynm = "Inc"
sample = 'NOlameduck'
tit = paste('RD effect on Victory t+1 for Incumbent Party', sep='')
tit = ''
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit,
       x.label = "Incumbent Party Vote Margin at t", y.label ='Victory t+1 (=1 if ran and won, =0 else)',
       x.lim = c(-25, 25), y.lim = c(0,0.7))
abline(v=0, lwd=2, col='red')
dev.off()


########################################
#
# Figure 3: See file 02-prepare-final-figures-LAcountries.R
#
#####################################

####################################################################################
##
## END of figures in main paper
##
#####################################################################################


####################################################################################
##
## Figures in Supplemental Appendix
##
#####################################################################################

########################################
#
# Figure S1: Covariates Density Tests and Histograms
#
#####################################

# Incparty
x = data$mv_incparty
x = x[data$year > 1996 & data$year <= 2012]
histogram(x) 
denout = rddensity(X = x, c =0, print.screen=TRUE)
mc = denout$test$p.jk

#  main="Histogram of Incumbent Party's Margin of Victory at t", 
pdf(paste(path,"/histogram-incparty.pdf", sep=""))
tit = paste("P-value density test: ", round(mc,2), sep='')
hist(x[x>=-10 & x<=10],breaks=40, col='cyan',xlab="Incumbent Party's Margin of Victory at t", main=NULL)
abline(v=0, col='red', lwd=3)
legend('topright',tit, cex=1.1, bty='n')
dev.off()

# PSDB
x = data$mv_party_PSDB
x = x[data$year <= 2012]
denout = rddensity(X = x, c =0, print.screen=TRUE)
mc = denout$test$p.jk

pdf(paste(path,"/histogram-PSDB.pdf", sep=""))
tit = paste("P-value density test: ", round(mc,2), sep='')
hist(x[x>=-10 & x<=10],breaks=40, col='cyan',  xlab="PSDB's Margin of Victory at t", main=NULL)
abline(v=0, col='red', lwd=3)
legend('topleft',tit, cex=1.1, bty='n')
dev.off()

# PMDB
x = data$mv_party_PMDB
x = x[data$year <= 2012]
denout = rddensity(X = x, c =0, print.screen=TRUE)
mc = denout$test$p.jk

pdf(paste(path,"/histogram-PMDB.pdf", sep=""))
tit = paste("P-value density test: ", round(mc,2), sep='')
hist(x[x>=-10 & x<=10],breaks=40, col='cyan', main=NULL, xlab="Margin of Victory at t")
abline(v=0, col='red', lwd=3)
legend('topright',tit, cex=1.1, bty='n')
dev.off()

# DEM
x = data$mv_party_DEM
x = x[data$year <= 2012]
denout = rddensity(X = x, c =0, print.screen=TRUE)
mc = denout$test$p.jk

pdf(paste(path,"/histogram-DEM.pdf", sep=""))
tit = paste("P-value density test: ", round(mc,2), sep='')
hist(x[x>=-10 & x<=10],breaks=40, col='cyan', xlab="DEM's Margin of Victory at t", main=NULL)
abline(v=0, col='red', lwd=3)
legend('topright',tit, cex=1.1, bty='n')
dev.off()

# PT
x = data$mv_party_PT
x = x[data$year <= 2012]
denout = rddensity(X = x, c =0, print.screen=TRUE)
mc = denout$test$p.jk

pdf(paste(path,"/histogram-PT.pdf", sep=""))
tit = paste("P-value density test: ", round(mc,2), sep='')
hist(x[x>=-10 & x<=10],breaks=40, col='cyan', main="Histogram of PT's Margin of Victory at t", xlab="Margin of Victory at t")
abline(v=0, col='red', lwd=3)
legend('topright',tit, cex=1.1, bty='n')
dev.off()

# PP
x = data$mv_party_PP
x = x[data$year <= 2012]
denout = rddensity(X = x, c =0, print.screen=TRUE)
mc = denout$test$p.jk

pdf(paste(path,"/histogram-PP.pdf", sep=""))
tit = paste("P-value density test: ", round(mc,2), sep='')
hist(x[x>=-10 & x<=10],breaks=40, col='cyan', main="Histogram of PP's Margin of Victory at t", xlab="Margin of Victory at t")
abline(v=0, col='red', lwd=3)
legend('topright',tit, cex=1.1, bty='n')
dev.off()

########################################
#
# Figure S2: RD placebo "treatment effect" on covariates
#
#####################################
outnm = "Llastyr_pib"
partynm = "Inc"
sample = 'Full'
y = data$pib
x = data$mv_incparty
max(y/1000, na.rm=T)
tit = paste('RD effect on GDP at t')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
l = 25
pdf(file)
rdplot(y = y/1000, x = x , c = 0,  binselect = "esmv",title = tit, x.label = "Incumbent Party Vote Margin at t",
            y.label = "GDP at t  (million reais)", scale = 1, x.lim = c(-l, l), y.lim = c(15,1200))
abline(v=0, lwd=2, col='red')
dev.off()

outnm = "EffParties"
partynm = "Inc"
sample = 'Full'
y = data$numpar_candidates_eff
x = data$mv_incparty
tit = paste('RD effect on Number of Effective Parties at t')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv", title = tit, x.label = "Incumbent Party Vote Margin at t", y.label = "Number of Effective Parties at t", scale = 1,
       x.lim = c(-25, 25), y.lim = c(1,4))
abline(v=0, lwd=2, col='red')
dev.off()

outnm = "Population"
partynm = "Inc"
sample = 'Full'
y = data$population
x = data$mv_incparty
tit = paste('RD effect on Population at t')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y/1000, x = x , c = 0, binselect = "esmv",title = tit, x.label = "Incumbent Party Vote Margin at t", y.label = 'Population at t (thousands)', scale = 1,
            x.lim = c(-25, 25), y.lim = c(1,90))
abline(v=0, lwd=2, col='red')
dev.off()

outnm = "PMDBCand_t-1"
partynm = "Inc"
sample = 'Full'
y = data$party_PMDB_runslag1
x = data$mv_incparty
tit = paste('RD effect on PMDB Candidate t-1')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "Incumbent Party Vote Margin at t", y.label = 'PMDB Candidate t-1', scale = 1,
            x.lim = c(-25, 25), y.lim = c(0,1))
abline(v=0, lwd=2, col='red')
dev.off()


outnm = "PSDBCand_t-1"
partynm = "Inc"
sample = 'Full'
y = data$party_PSDB_runslag1
x = data$mv_incparty
tit = paste('RD effect on PSDB Candidate t-1')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "Incumbent Party Vote Margin at t", y.label = 'PSDB Candidate t-1', scale = 1,
            x.lim = c(-25, 25), y.lim = c(0,1))
abline(v=0, lwd=2, col='red')
dev.off()

outnm = "Victory_t-1"
partynm = "PSDB"
sample = 'Full'
y = data$party_PSDB_wonlag1_b1
x = data$mv_party_PSDB
tit = paste('RD effect on PSDB Victory t-1')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "PSDB Vote Margin at t", y.label = 'PSDB Victory at t-1', scale = 1,
            x.lim = c(-25, 25), y.lim = c(0,1))
abline(v=0, lwd=2, col='red')
dev.off()


outnm = "Victory_t-1"
partynm = "PMDB"
sample = 'Full'
y = data$party_PMDB_wonlag1_b1
x = data$mv_party_PMDB
tit = paste('RD effect on PMDB Victory t-1')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "PMDB Vote Margin at t", y.label = 'PMDB Victory at t-1', scale = 1,
            x.lim = c(-25, 25), y.lim = c(0,1))
abline(v=0, lwd=2, col='red')
dev.off()


outnm = "Victory_t-1"
partynm = "DEM"
sample = 'Full'
y = data$party_DEM_wonlag1_b1
x = data$mv_party_DEM
tit = paste('RD effect on DEM Victory t-1')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "DEM Vote Margin at t", y.label = 'DEM Victory at t-1', scale = 1,
            x.lim = c(-25, 25), y.lim = c(0,1))
abline(v=0, lwd=2, col='red')
dev.off()


outnm = "Victory_t-1"
partynm = "PT"
sample = 'Full'
y = data$party_PT_wonlag1_b1
x = data$mv_party_PT
tit = paste('RD effect on PT Victory t-1')
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect = "esmv",title = tit, x.label = "PT Vote Margin at t", y.label = 'PT Victory at t-1', scale = 1,
            x.lim = c(-25, 25), y.lim = c(0,1))
abline(v=0, lwd=2, col='red')
dev.off()


########################################
#
# Figure S3: Incumbent party, Candidacy t+1 and Conditional Victory t+1
#
#####################################
# candidacy 
y = data$inc_party_runsfor1
x = data$mv_incparty
outnm = "Candidatet+1"
partynm = "Inc"
sample = 'Full'
tit = paste('RD effect on Candidate t+1 for Incumbent Party', sep='')
tit = ''
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect="esmv", title = tit, x.label = "Incumbent Party Vote Margin at t", y.label = 'Candidate t+1' ,
            x.lim = c(-25, 25), y.lim = c(0,0.7))
abline(v=0, lwd=2, col='red')
dev.off()

# conditional victory
y = data$inc_party_wonfor1
x = data$mv_incparty
outnm = "Victoryt+1"
partynm = "Inc"
sample = 'Full'
tit = paste('RD effect on Victory t+1 for Incumbent Party', sep='')
tit =  ''
file = paste(path,'/RDplot-', partynm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rdplot(y = y, x = x , c = 0, binselect="esmv", title = tit, x.label = "Incumbent Party Vote Margin at t", y.label = 'Victory t+1 (=1 if ran and won, =0 if ran and lost)', 
            x.lim = c(-25, 25), y.lim = c(0,0.7))
abline(v=0, lwd=2, col='red')
dev.off()

########################################
#
# Figure S4: Histogram of close races in Brazil and US Congress
#
#####################################

# Brazil mayoral elections: difference in vote share between winner and runnner-up
data =  read.dta13("./KlasnjaTitiunik-Brazil-data.dta")
names(data)
y  = abs(data$votesh_n1 - data$votesh_n2)
# Assign y=100 when the race is uncontested
y[is.na(data$votesh_n2)] = data$votesh_n1[is.na(data$votesh_n2)]

ii = rep(TRUE, nrow(data))
table(data$year[ii])
summary(y[ii])
quantile(y[ii], probs=c(0, 0.10, 0.15, 0.20,0.25, 0.30, 0.35, 0.40, 0.50,0.75,1), na.rm=T)
mean(y[ii] <= 10)
med = quantile(y[ii], probs=c(0.50), na.rm=T)
pdf(paste(path,"/extval-plot-all-n1versusn2-1996-2012.pdf", sep=""))
hist(y[ii], xlab="Winner's Margin of Victory", col="cyan", axes=T, main=NULL, breaks=20)
axis(side=1,at=c(med), labels=c(paste(round(med,1))))
abline(v=med, col="red", lty=2,lwd=2)
arrows(30,2500,med,2500,length=0.10)
text(x=37,y=2500,labels="Median")
dev.off()

# US House elections: difference in vote share between winner and runnner-up 
data = read.dta13("./KlasnjaTitiunik-USHouse-data.dta")

idem2 = (data$othcand_order ==1 & data$demcand_order==2)
irep2 = (data$othcand_order ==1 & data$repcand_order==2)

othmv = rep(NA, nrow(data))
othmv[idem2] = (data$othvotesh[idem2] - data$demvotesh[idem2])
othmv[irep2] = (data$othvotesh[irep2] - data$repvotesh[irep2])

y = rep(NA, nrow(data))
y[data$demcand_order==1  & data$demvotesh > 0 & !is.na(data$demvotesh) & data$year >= 1996] = data$demmv[data$demcand_order==1  & data$demvotesh > 0 & !is.na(data$demvotesh) & data$year >= 1996]
y[data$repcand_order==1  & data$repvotesh > 0 & !is.na(data$repvotesh) & data$year >= 1996] = data$repmv[data$repcand_order==1  & data$repvotesh > 0 & !is.na(data$repvotesh) & data$year >= 1996]
y[data$othcand_order ==1 & data$othvotesh > 0 & !is.na(data$othvotesh) & data$year >= 1996] = othmv[data$othcand_order ==1 & data$othvotesh > 0 & !is.na(data$othvotesh) & data$year >= 1996]
y = abs(y * 100)
y = y[data$year>=1996]
summary(y)
quantile(y, probs=c(0, 0.10, 0.15, 0.20,0.25, 0.30, 0.35, 0.40, 0.50,0.75,1), na.rm=T)
mean(y <= 10, na.rm=T)
mean(y <= 5, na.rm=T)
med = quantile(y, probs=c(0.50), na.rm=T)
pdf(paste(path,"/extval-plot-all-n1versusn2-USH-1996-2010.pdf", sep=''))
hist(y, xlab="Winner's Margin of Victory", col="cyan", axes=T, main=NULL,ylab=NULL, breaks=20)
axis(side=1,at=c(med), labels=c(paste(round(med,1))))
abline(v=med, col="red", lty=2, lwd=2)
arrows(60,100,med,100,length=0.10)
text(x=67,y=100,labels="Median")
dev.off()

####################################################################################
##
## END of figures in Supplemental Appendix
##
#####################################################################################

