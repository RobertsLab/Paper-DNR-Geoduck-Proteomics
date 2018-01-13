# Run models 
# From Stats3 Script, use the following metrics for each envir. parameter: Variance, Minimum AND/OR Maximum, Mean AND/OR Median, % > 1 SD from Mean.
# Use the transformed abundance data in one peptide ("Pep1) to generate model. Then, test on Pep2. 
library(MASS)

# View plot matrix of HSP90 against all env. variables
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/DO-Regression-Matrix.png", width = 2000, height = 1000)
pairs(data.pepsum.Env.Stats.HSP90[,c(7,10:17)], main="Plot Matrix, HSP90 - Pep1 vs DO Stats", lower.panel = panel.cor, cex=1.5, cex.labels = 2)
dev.off()

png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/pH-Regression-Matrix.png", width = 2000, height = 1000)
View(data.pepsum.Env.Stats)
pairs(data.pepsum.Env.Stats.HSP90[!grepl("PGE",data.pepsum.Env.Stats.HSP90$BOTH),c(7,18:27)], main="Plot Matrix, HSP90 - Pep1 vs pH Stats", lower.panel = panel.cor, cex=1.5, cex.labels = 2)
dev.off()

png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/Salinity-Regression-Matrix.png", width = 2000, height = 1000)
pairs(data.pepsum.Env.Stats.HSP90[!grepl("PGB",data.pepsum.Env.Stats.HSP90$BOTH),c(7,28:35)], main="Plot Matrix, HSP90 - Pep1 vs Salinity Stats", lower.panel = panel.cor, cex=2, cex.labels = 2.5)
dev.off()

png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/Temp-Regression-Matrix.png", width = 2000, height = 1000)
pairs(data.pepsum.Env.Stats.HSP90[,c(7,36:43)], main="Plot Matrix, HSP90 - Pep1 vs Temp Stats", lower.panel = panel.cor, cex=2, cex.labels = 2.5)
dev.off()

# Variable selection using automatic methods, leaps package 
library(leaps)
test.leaps <- regsubsets(Pep1 ~ DO.median.loc + DO.max.loc + DO.min.loc + DO.var.loc + DO.sd.1.loc + pH.var.loc + pH.median.loc + pH.min.loc + pH.sd.1.loc + pH.sd.2.loc + pH.Percent.high.loc + pH.Percent.low.loc +   Temperature.median.loc + Temperature.min.loc + Temperature.max.loc + Temperature.var.loc + Temperature.sd.1.loc, data=data.pepsum.Env.Stats.HSP90, nbest=20, nvmax=NULL, force.in=NULL, force.out=NULL, method="exhaustive")
plot(test.leaps, scale="adjr2")

test.leaps.3 <- regsubsets(Pep1 ~ DO.median.loc + DO.max.loc + DO.min.loc + DO.var.loc + DO.sd.1.loc + pH.var.loc + pH.median.loc + pH.min.loc + pH.sd.1.loc + + pH.sd.2.loc + pH.Percent.high.loc + pH.Percent.low.loc + Temperature.median.loc + Temperature.min.loc + Temperature.max.loc + Temperature.var.loc + Temperature.sd.1.loc, data=data.pepsum.Env.Stats.HSP90, nbest=20, method="forward")
plot(test.leaps.3, scale="adjr2")
summary(test.leaps.3)

# Common variables: DO.var, pH.sd.2 
summary(lm(`Pep1` ~ DO.sd.1.loc + pH.sd.2.loc + Temperature.median.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.median
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.median.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.median

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.median.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.median
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.median.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.median

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.max.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.max
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.max.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.max

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.min.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.min
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.min.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.min

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.sd.1.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.sd.1
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + DO.sd.1.loc, data=data.pepsum.Env.Stats.HSP90.1)) #add DO.sd.1

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.var.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.var
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.var.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.var

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.median.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.median
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.median.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.median

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.min.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.min
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.min.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.min

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.sd.1.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.sd.1
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.sd.1.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.sd.1

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.Percent.high.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.Percent.high
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.Percent.high.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.Percent.high

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.Percent.low.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.Percent.low
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + pH.Percent.low.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add pH.Percent.low

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.median.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.median
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.median.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.median

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.min.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.min
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.min.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.min

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.max.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.max
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.max.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.max

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.var.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.var
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.var.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.var

summary(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.sd.1.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.sd.1
ols_stepwise(lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc + Temperature.sd.1.loc , data=data.pepsum.Env.Stats.HSP90.1)) #add Temperature.sd.1

## Test out models
install.packages("olsrr")
library(olsrr)

# Create separate dataframes for each peptide, so I can test the regression model from Pep1 on the others
data.pepsum.Env.Stats.HSP90.1 <- data.pepsum.Env.Stats.HSP90[-8:-9]
data.pepsum.Env.Stats.HSP90.2 <- data.pepsum.Env.Stats.HSP90[c(-7,-9)]
data.pepsum.Env.Stats.HSP90.3 <- data.pepsum.Env.Stats.HSP90[-7:-8]

best.lm.HSP90.1 <- lm(`Pep1` ~ DO.var.loc + pH.sd.2.loc, data=data.pepsum.Env.Stats.HSP90.1)
summary(best.lm.HSP90.1)
best.lm.HSP90.1.pred1 <- predict(best.lm.HSP90.1, data.pepsum.Env.Stats.HSP90.2)
best.lm.HSP90.1.pred1/data.pepsum.Env.Stats.HSP90.2$Pep2
best.lm.HSP90.1.pred2 <- predict(best.lm.HSP90.1, data.pepsum.Env.Stats.HSP90.3)
plot(best.lm.HSP90.1.pred2/data.pepsum.Env.Stats.HSP90.3$Pep3)




best.lm.HSP90 <- lm(`Pep1`+`Pep2` +`Pep3` ~ DO.var.loc + pH.sd.2.loc, data=data.pepsum.Env.Stats.HSP90)
summary(best.lm.HSP90)
ols_stepwise(best.lm.HSP90 )
anova(best.lm.HSP90)

# Plot growth against HSP90 Pep1 abundance
par(mfrow=c(1,3))
plot(data.pepsum.Env.Stats.HSP90$Pep1 ~ data.pepsum.Env.Stats.HSP90$Perc.Growth, main="HSP90 Pep1 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.HSP90$Pep2 ~ data.pepsum.Env.Stats.HSP90$Perc.Growth, main="HSP90 Pep2 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.HSP90$Pep3 ~ data.pepsum.Env.Stats.HSP90$Perc.Growth, main="HSP90 Pep3 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")

# Plot peptides against growth & calculate coefficients, R^2, Pvalue. Summary() shows equation with R^2
# all peptides 
HSP90.Growth <- lm(`Pep1`+`Pep2`+`Pep3` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP90)
summary(HSP90.Growth )
with(data.pepsum.Env.Stats.HSP90, plot(`Perc.Growth`, `Pep1`+`Pep2`+`Pep3`))
abline(HSP90.Growth)

# Peptide 1 x Peptide 2
HSP90.Pep1Growth <- lm(`Pep1` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP90)
summary(HSP90.Pep1Growth)
with(data.pepsum.Env.Stats.HSP90, plot(`Perc.Growth`, `Pep1`))
abline(HSP90.Pep1Growth)
HSP90.Pep2Growth <- lm(`Pep2` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP90)
summary(HSP90.Pep2Growth)
with(data.pepsum.Env.Stats.HSP90, plot(`Perc.Growth`, `Pep2`))
abline(HSP90.Pep2Growth)
HSP90.Pep3Growth <- lm(`Pep3` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP90)
summary(HSP90.Pep3Growth)
with(data.pepsum.Env.Stats.HSP90, plot(`Perc.Growth`, `Pep3`))
abline(HSP90.Pep3Growth)


### BONEYARD 

# Test env. variable separately 
testDO.HSP90 <- lm(Pep1 ~ -1 + DO.median.loc + DO.min.loc + DO.max.loc + DO.var.loc + DO.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testDO.HSP90)
par(mfrow=c(1,1))
plot(rstandard(testDO.HSP90), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testDO.HSP90, which=1:4)
anova(testDO.HSP90)
ols_stepwise(testDO.HSP90)

testpH.HSP90 <- lm(Pep1 ~ -1 + pH.median.loc + pH.min.loc + pH.max.loc + pH.var.loc + pH.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testpH.HSP90)
par(mfrow=c(1,1))
plot(rstandard(testpH.HSP90), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testpH.HSP90, which=1:4)
anova(testpH.HSP90)
ols_stepwise(testpH.HSP90)

testTemp.HSP90 <- lm(Pep1 ~ -1 + Temperature.median.loc + Temperature.min.loc + Temperature.max.loc + Temperature.var.loc + Temperature.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testTemp.HSP90)
par(mfrow=c(1,1))
plot(rstandard(testTemp.HSP90), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testTemp.HSP90, which=1:4)
anova(testTemp.HSP90)
ols_stepwise(testTemp.HSP90)

# Run parameters identified in the above: 
# --> DO.var.loc
# --> pH.sd.1.loc
# --> Temperature.median.loc


testBest1.HSP90 <- lm(Pep1 ~ -1 + DO.var.loc + pH.sd.1.loc + Temperature.median.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testBest1.HSP90)
anova(testBest1.HSP90)
ols_stepwise(testBest1.HSP90)

#pairs(data.pepsum.Env.Stats.HSP90[,c(7,17,23,26,36)], main="Plot Matrix, Pep1 vs Best Stats")

testBest2.HSP90 <- lm(Pep2 ~ -1 + DO.var.loc + DO.max.loc + pH.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testBest2.HSP90)
anova(testBest2.HSP90)
ols_stepwise(testBest2.HSP90)

testBest3.HSP90 <- lm(Pep3 ~ -1 + DO.var.loc + DO.max.loc + pH.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testBest3.HSP90)
anova(testBest3.HSP90)
ols_stepwise(testBest3.HSP90)

# FINDINGS:  DO.var.loc + DO.max.loc + pH.sd.1.loc 
# High concentrations and variance in DO, and extreme pH values are best fit independent variables in multiple linear regression models, predicting high levels of HSP90

# Test out Structural Equation Model
install.packages("sem")
library(sem)
test.cov <- cov(data.pepsum.Env.Stats.HSP90[,c(-1:-6,-8,-9)]) #create a covariance matrix
fullsem <- specify.model()

library(foreign)
testdata <- data<-read.spss("http://www.methodsconsultants.com/data/intelligence.sav", to.data.frame=TRUE)
head(testdata)
