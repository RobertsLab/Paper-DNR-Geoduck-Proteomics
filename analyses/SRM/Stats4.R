# Run models 
# From Stats3, use the following metrics for each envir. parameter: Variance, Minimum AND/OR Maximum, Mean AND/OR Median, % > 1 SD from Mean.
# Use the transformed abundance data in one peptide ("Pep1) to generate model. Then, test on Pep2. 
library(MASS)
library(plotly)
library("ggpubr")
library(olsrr)
require(gridExtra)

# View plot matrix of HSP90 against all env. variables
pairs(data.pepsum.Env.Stats.HSP90[,c(7,10:17)], main="Plot Matrix, Pep1 vs DO Stats")
pairs(data.pepsum.Env.Stats.HSP90[,c(7,18:25)], main="Plot Matrix, Pep1 vs pH Stats")
pairs(data.pepsum.Env.Stats.HSP90[,c(7,26:33)], main="Plot Matrix, Pep1 vs Salinity Stats")
pairs(data.pepsum.Env.Stats.HSP90[,c(7,34:41)], main="Plot Matrix, Pep1 vs Temp Stats")

#Plot all pep1, pep2, and pep3 values for all proteins against PERCENT GROWTH
plot(data.pepsum.Env.Stats$Pep1 ~ data.pepsum.Env.Stats$Perc.Growth.y, main="All Proteins Pep1 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats$Pep2 ~ data.pepsum.Env.Stats$Perc.Growth.y, main="All Proteins Pep2 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats$Pep3 ~ data.pepsum.Env.Stats$Perc.Growth.y, main="All Proteins Pep3 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")

Growth.Peps.plot <- plot_ly(data=data.pepsum.Env.Stats, x=~Perc.Growth, marker = list(size = 7)) %>%
  add_trace(y=~Pep1, color=~SITE, hovertext=~Protein.Name, showlegend = FALSE) %>%
  add_trace(y=~Pep1, color=~SITE, hovertext=~Protein.Name, showlegend = FALSE) %>%
  add_trace(y=~Pep3, color=~SITE, hovertext=~Protein.Name) %>%
  layout(title="All Peptides against Percent Groth",
         yaxis = list(title = 'Peptide Abundance'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(Growth.Peps.plot), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/June2016-Growth-PepAbundance.html")

#QQNorm plots of summary stats & %growth
par(mfrow = c(2, 3))
for (i in 11:45) {
  qqnorm(data.pepsum.Env.Stats[,i], main = colnames(data.pepsum.Env.Stats[i]),
         xlab = "Theoretical Quantiles", ylab = "Parameter Value", plot.it = TRUE)
  qqline(data.pepsum.Env.Stats[,i], main = colnames(data.pepsum.Env.Stats[i]))
}

# Generate correlation plots for the 3 diff. abundant proteins, using Pep1 abundance
HSP90.corr.plots <- list()
Puromycin.corr.plots <- list()
Trifunctional.corr.plots <- list()

# Run loop to generate scatter plots with each env. summary variable
#HSP 90
for (i in 11:45) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.HSP90, x=colnames(data.pepsum.Env.Stats.HSP90[i]), y="Pep1", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.HSP90[i]), ylab="HSP90 Pep1 Abundance (lambda-transformed)", main=paste("HSP90 Pep1 abundance ~ ", colnames(data.pepsum.Env.Stats.HSP90[i]), sep=""))
    print(i)
    print(p1)
    HSP90.corr.plots[[i]] <<- p1
})

#Puromycin-sensitive aminopeptidase
for (i in 11:45) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.Puromycin, x=colnames(data.pepsum.Env.Stats.Puromycin[i]), y="Pep1", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.Puromycin[i]), ylab="Puromycin Pep1 Abundance (lambda-transformed)", main=paste("Puromycin Pep1 abundance ~ ", colnames(data.pepsum.Env.Stats.Puromycin[i]), sep=""))
    print(i)
    print(p1)
    Puromycin.corr.plots[[i]] <<- p1
})

#Trifunctional Enzyme
for (i in 11:45) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.Trifunctional, x=colnames(data.pepsum.Env.Stats.Trifunctional[i]), y="Pep1", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.Trifunctional[i]), ylab="Trifunctional Pep1 Abundance (lambda-transformed)", main=paste("Trifunctional Pep1 abundance ~ ", colnames(data.pepsum.Env.Stats.Trifunctional[i]), sep=""))
    print(i)
    print(p1)
    Trifunctional.corr.plots[[i]] <<- p1
  })

# Save all plots in 1 PDF for each protein
pdf("../../analyses/SRM/HSP90-corr-plots.pdf")
grid.arrange( for (i in 11:45) {
  print(HSP90.corr.plots[[i]])
})
dev.off()

pdf("../../analyses/SRM/Puromycin-corr-plots.pdf")
grid.arrange( for (i in 11:45) {
  print(Puromycin.corr.plots[[i]])
})
dev.off()

pdf("../../analyses/SRM/Trifunctional-corr-plots.pdf")
grid.arrange( for (i in 11:45) {
  print(Trifunctional.corr.plots[[i]])
})
dev.off()



#P value for correlation plots, bonferroni corrected (most strict)
0.05/ncol(data.pepsum.Env.Stats.HSP90[,11:45])
# = 0.001428571

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


