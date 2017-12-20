# Run models 
# From Stats3 Script, use the following metrics for each envir. parameter: Variance, Minimum AND/OR Maximum, Mean AND/OR Median, % > 1 SD from Mean.
# Use the transformed abundance data in one peptide ("Pep1) to generate model. Then, test on Pep2. 
library(MASS)
# Convert environmental stats to numeric

# Plot growth against HSP70 Pep1 abundance
par(mfrow=c(1,3))
plot(data.pepsum.Env.Stats.HSP70$Pep1 ~ data.pepsum.Env.Stats.HSP70$Perc.Growth, main="HSP70 Pep1 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.HSP70$Pep2 ~ data.pepsum.Env.Stats.HSP70$Perc.Growth, main="HSP70 Pep2 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")


# View plot matrix of HSP70 against all env. variables
pairs(iris, upper.panel = panel.cor)

pairs(data.pepsum.Env.Stats.HSP70[,c(7,10:17)], main="Plot Matrix, HSP70 - Pep1 vs DO Stats", upper.panel = panel.cor)
pairs(data.pepsum.Env.Stats.HSP70[,c(7,18:25)], main="Plot Matrix, HSP70 - Pep1 vs pH Stats")
pairs(data.pepsum.Env.Stats.HSP70[,c(7,26:33)], main="Plot Matrix, HSP70 - Pep1 vs Salinity Stats")
pairs(data.pepsum.Env.Stats.HSP70[,c(7,34:41)], main="Plot Matrix, HSP70 - Pep1 vs Temp Stats")
install.packages("ggcorplot")

library(olsrr)
best.lm.HSP70 <- lm(`Pep1`+`Pep2` ~ DO.var.loc + pH.sd.2.loc, data=data.pepsum.Env.Stats.HSP70)
summary(best.lm.HSP70)
ols_stepwise(best.lm.HSP70 )
anova(best.lm.HSP70)

# Plot peptides against growth & calculate coefficients, R^2, Pvalue. Summary() shows equation with R^2
HSP70.Growth <- lm(`Pep1`+`Pep2` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP70)
summary(HSP70.Growth )
with(data.pepsum.Env.Stats.HSP70, plot(`Perc.Growth`, `Pep1`+`Pep2`))
abline(HSP70.Growth)

HSP70.Pep1Growth <- lm(`Pep1` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP70)
summary(HSP70.Pep1Growth)
with(data.pepsum.Env.Stats.HSP70, plot(`Perc.Growth`, `Pep1`))
abline(HSP70.Pep1Growth)
HSP70.Pep2Growth <- lm(`Pep2` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.HSP70)
summary(HSP70.Pep2Growth)
with(data.pepsum.Env.Stats.HSP70, plot(`Perc.Growth`, `Pep2`))
abline(HSP70.Pep2Growth)


### BONEYARD

# Only includes the first 6 ... 

# Test env. variable separately 
testDO.HSP70 <- lm(Pep1 ~ -1 + DO.median.loc + DO.min.loc + DO.max.loc + DO.var.loc + DO.sd.1.loc, data=data.pepsum.Env.Stats.HSP70)
summary(testDO.HSP70)
par(mfrow=c(1,1))
plot(rstandard(testDO.HSP70), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testDO.HSP70, which=1:4)
anova(testDO.HSP70)
ols_stepwise(testDO.HSP70)

testpH.HSP70 <- lm(Pep1 ~ -1 + pH.median.loc + pH.min.loc + pH.max.loc + pH.var.loc + pH.sd.1.loc, data=data.pepsum.Env.Stats.HSP70)
summary(testpH.HSP70)
par(mfrow=c(1,1))
plot(rstandard(testpH.HSP70), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testpH.HSP70, which=1:4)
anova(testpH.HSP70)
ols_stepwise(testpH.HSP70)

testTemp.HSP70 <- lm(Pep1 ~ -1 + Temperature.median.loc + Temperature.min.loc + Temperature.max.loc + Temperature.var.loc + Temperature.sd.1.loc, data=data.pepsum.Env.Stats.HSP70)
summary(testTemp.HSP70)
par(mfrow=c(1,1))
plot(rstandard(testTemp.HSP70), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testTemp.HSP70, which=1:4)
anova(testTemp.HSP70)
ols_stepwise(testTemp.HSP70)

testSalinity.HSP70 <- lm(Pep1 ~ -1 + Salinity.median.loc + Salinity.min.loc + Salinity.max.loc + Salinity.var.loc + Salinity.sd.1.loc, data=data.pepsum.Env.Stats.HSP70)
summary(testSalinity.HSP70)
par(mfrow=c(1,1))
plot(rstandard(testSalinity.HSP70), main="Standardized Residuals \nShould be between [-2,2]")
par(mfrow=c(1,4))
plot(testSalinity.HSP70, which=1:4)
anova(testSalinity.HSP70)
ols_stepwise(testSalinity.HSP70)

# Run parameters identified in the above HSP70 env. parameters: 
# --> DO.max.loc
# --> pH.var.loc
# --> Temperature.median.loc
# --> Salinity.max.loc 

DO.var.loc + DO.max.loc + pH.sd.1.loc
testBest1.HSP70 <- lm(Pep1 ~ -1 + DO.var.loc + DO.max.loc + pH.var.loc +  + pH.sd.1.loc + Temperature.median.loc + Salinity.max.loc, data=data.pepsum.Env.Stats.HSP70)
summary(testBest1.HSP70)
anova(testBest1.HSP70)
ols_stepwise(testBest1.HSP70)


testBest2.HSP70 <- lm(Pep2 ~ -1 + DO.max.loc + pH.var.loc + Temperature.median.loc + Salinity.max.loc, data=data.pepsum.Env.Stats.HSP70)
summary(testBest2.HSP70)
anova(testBest2.HSP70)

