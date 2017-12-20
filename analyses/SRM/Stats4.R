# Run models 
# From Stats3, use the following metrics for each envir. parameter: Variance, Minimum AND/OR Maximum, Mean AND/OR Median, % > 1 SD from Mean.
# Use the transformed abundance data in one peptide ("Pep1) to generate model. Then, test on Pep2. 
library(MASS)
# Convert environmental stats to numeric
data.pepsum.Env.Stats[10:85] <- as.numeric(as.character(unlist(data.pepsum.Env.Stats[10:85])), na.action=na.omit)

# Isolate data related to diff. expressed proteins: 
# ====> Puromycin-sensitive aminopeptidase
# ====> HSP70
# ====> HSP90-alpha
# ====> Trifunctional enzyme subunit

data.pepsum.Env.Stats.Puromycin <- data.pepsum.Env.Stats[grepl(c("Puromycin"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.HSP70 <- data.pepsum.Env.Stats[grepl(c("HSP70"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.HSP90 <- data.pepsum.Env.Stats[grepl(c("HSP90"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.Trifunctional <- data.pepsum.Env.Stats[grepl(c("Trifunctional"), data.pepsum.Env.Stats$Protein.Name),]

# View plot matrix of HSP90 against all env. variables
pairs(data.pepsum.Env.Stats.HSP90[,c(7,10:17)], main="Plot Matrix, Pep1 vs DO Stats")
pairs(data.pepsum.Env.Stats.HSP90[,c(7,18:25)], main="Plot Matrix, Pep1 vs pH Stats")
pairs(data.pepsum.Env.Stats.HSP90[,c(7,26:33)], main="Plot Matrix, Pep1 vs Salinity Stats")
pairs(data.pepsum.Env.Stats.HSP90[,c(7,34:41)], main="Plot Matrix, Pep1 vs Temp Stats")

install.packages("olsrr")
library(olsrr)

testAll.HSP90 <- lm(Pep1 ~ -1 + DO.median.loc + DO.min.loc + DO.max.loc + DO.var.loc + DO.sd.1.loc + pH.median.loc + pH.min.loc + pH.max.loc + pH.var.loc + pH.sd.1.loc + Temperature.median.loc + Temperature.min.loc + Temperature.max.loc + Temperature.var.loc + Temperature.sd.1.loc + Salinity.median.loc + Salinity.min.loc + Salinity.max.loc + Salinity.var.loc + Salinity.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testAll.HSP90)
par(mfrow=c(1,1))
plot(rstandard(testAll.HSP90), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testAll.HSP90, which=1:4)
anova(testAll.HSP90)
# Only includes the first 6 ... 

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

testSalinity.HSP90 <- lm(Pep1 ~ -1 + Salinity.median.loc + Salinity.min.loc + Salinity.max.loc + Salinity.var.loc + Salinity.sd.1.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testSalinity.HSP90)
par(mfrow=c(1,1))
plot(rstandard(testSalinity.HSP90), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testSalinity.HSP90, which=1:4)
anova(testSalinity.HSP90)
ols_stepwise(testSalinity.HSP90)

# Run parameters identified in the above: 
# --> DO.var.loc
# --> pH.sd.1.loc
# --> Temperature.median.loc
# --> Salinity.max.loc 

testBest1.HSP90 <- lm(Pep1 ~ -1 + DO.var.loc + pH.sd.1.loc + Temperature.median.loc + Salinity.max.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testBest.HSP90)
anova(testBest.HSP90)
pairs(data.pepsum.Env.Stats.HSP90[,c(7,17,23,26,36)], main="Plot Matrix, Pep1 vs Best Stats")

testBest2.HSP90 <- lm(Pep2 ~ -1 + DO.var.loc + pH.sd.1.loc + Temperature.median.loc + Salinity.max.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testBest2.HSP90)
anova(testBest2.HSP90)

testBest3.HSP90 <- lm(Pep3 ~ -1 + DO.var.loc + pH.sd.1.loc + Temperature.median.loc + Salinity.max.loc, data=data.pepsum.Env.Stats.HSP90)
summary(testBest3.HSP90)
anova(testBest3.HSP90)
