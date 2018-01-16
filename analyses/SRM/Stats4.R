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

### Generate correlation plots and stats

# First standardize peptide data so I can regress all peptides from one protein against environmental parameters 
data.pepsum.Env.Stats.HSP90$Pep1.Z <- scale(data.pepsum.Env.Stats.HSP90$Pep1, center=T, scale=T)
data.pepsum.Env.Stats.HSP90$Pep2.Z <- scale(data.pepsum.Env.Stats.HSP90$Pep2, center=T, scale=T)
data.pepsum.Env.Stats.HSP90$Pep3.Z <- scale(data.pepsum.Env.Stats.HSP90$Pep3, center=T, scale=T)
data.pepsum.Env.Stats.HSP90.Z <- melt(data.pepsum.Env.Stats.HSP90, id.vars = colnames(data.pepsum.Env.Stats.HSP90[,1:44]))

data.pepsum.Env.Stats.Puromycin$Pep1.Z <- scale(data.pepsum.Env.Stats.Puromycin$Pep1, center=T, scale=T)
data.pepsum.Env.Stats.Puromycin$Pep2.Z <- scale(data.pepsum.Env.Stats.Puromycin$Pep2, center=T, scale=T)
data.pepsum.Env.Stats.Puromycin$Pep3.Z <- scale(data.pepsum.Env.Stats.Puromycin$Pep3, center=T, scale=T)
data.pepsum.Env.Stats.Puromycin.Z <- melt(data.pepsum.Env.Stats.Puromycin, id.vars = colnames(data.pepsum.Env.Stats.Puromycin[,1:44]))

data.pepsum.Env.Stats.Trifunctional$Pep1.Z <- scale(data.pepsum.Env.Stats.Trifunctional$Pep1, center=T, scale=T)
data.pepsum.Env.Stats.Trifunctional$Pep2.Z <- scale(data.pepsum.Env.Stats.Trifunctional$Pep2, center=T, scale=T)
data.pepsum.Env.Stats.Trifunctional$Pep3.Z <- scale(data.pepsum.Env.Stats.Trifunctional$Pep3, center=T, scale=T)
data.pepsum.Env.Stats.Trifunctional.Z <- melt(data.pepsum.Env.Stats.Trifunctional, id.vars = colnames(data.pepsum.Env.Stats.Trifunctional[,1:44]))
             
# Create empty list to house correlation plots
HSP90.corr.plots <- list()
Puromycin.corr.plots <- list()
Trifunctional.corr.plots <- list()

# Run loop to generate scatter plots with each env. summary variable
#HSP 90

for (i in 10:44)  # environmental parameters are in columns 10:44
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.HSP90.Z, x=colnames(data.pepsum.Env.Stats.HSP90.Z[i]), y="value", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.HSP90.Z[i]), ylab="HSP90 Abundance (Z-Score)", main=paste("HSP90 Peptide Z-Score ~ ", colnames(data.pepsum.Env.Stats.HSP90.Z[i]), sep=""))
    print(i)
    print(p1)
    HSP90.corr.plots[[i]] <<- p1
  })

#Puromycin
for (i in 10:44) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.Puromycin.Z, x=colnames(data.pepsum.Env.Stats.Puromycin.Z[i]), y="value", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.Puromycin.Z[i]), ylab="Puromycin Abundance (Z-Score)", main=paste("Puromycin Peptide Z-Score ~ ", colnames(data.pepsum.Env.Stats.Puromycin.Z[i]), sep=""))
    print(i)
    print(p1)
    Puromycin.corr.plots[[i]] <<- p1
  })

#Trifunctional
for (i in 10:44) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.Trifunctional.Z, x=colnames(data.pepsum.Env.Stats.Trifunctional.Z[i]), y="value", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.Trifunctional.Z[i]), ylab="Trifunctional Abundance (Z-Score)", main=paste("Trifunctional Peptide Z-Score ~ ", colnames(data.pepsum.Env.Stats.Trifunctional.Z[i]), sep=""))
    print(i)
    print(p1)
    Trifunctional.corr.plots[[i]] <<- p1
  })

# Save all plots in 1 PDF for each protein
#HSP90
pdf("../../analyses/SRM/HSP90-corr-plots.pdf")
grid.arrange( for (i in 10:44) {
  print(HSP90.corr.plots[[i]])
})
dev.off()

#Puromycin
pdf("../../analyses/SRM/Puromycin-corr-plots.pdf")
grid.arrange( for (i in 10:44) {
  print(Puromycin.corr.plots[[i]])
})
dev.off()

#Trifunctional
pdf("../../analyses/SRM/Trifunctional-corr-plots.pdf")
grid.arrange( for (i in 10:44) {
  print(Trifunctional.corr.plots[[i]])
})
dev.off()

### Run correlations between growth & environmental parameters 

# Generate correlation plots for the 3 diff. abundant proteins, using Pep1 abundance
Growth.corr.plots <- list()
for (i in 10:43) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats, x=colnames(data.pepsum.Env.Stats[i]), y="Perc.Growth", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats[i]), ylab="Percent Growth", main=paste("Percent Growth ~ ", colnames(data.pepsum.Env.Stats[i]), sep=""))
    print(i)
    print(p1)
    Growth.corr.plots[[i]] <<- p1
  })

pdf("../../analyses/SRM/Growth-corr-plots.pdf")
grid.arrange( for (i in 10:43) {
  print(Growth.corr.plots[[i]])
})
dev.off()

# Calculate correlation coefficient & p-value for all peptides combined, against growth & environmental stats
data.pepsum.Env.Stats$Pep1.Z <- scale(data.pepsum.Env.Stats$Pep1, center=T, scale=T)
data.pepsum.Env.Stats$Pep2.Z <- scale(data.pepsum.Env.Stats$Pep2, center=T, scale=T)
data.pepsum.Env.Stats$Pep3.Z <- scale(data.pepsum.Env.Stats$Pep3, center=T, scale=T)
data.pepsum.Env.Stats.Z <- melt(data.pepsum.Env.Stats, id.vars = colnames(data.pepsum.Env.Stats[,1:44]))

All.corr.plots <- list()
for (i in 10:44) 
  local({
    i <- i
    p1 <- ggscatter(data=data.pepsum.Env.Stats.Z, x=colnames(data.pepsum.Env.Stats.Z[i]), y="value", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab=colnames(data.pepsum.Env.Stats.Z[i]), ylab="All Peptides (z-score)", main=paste("All Peptides (z-score) ~ ", colnames(data.pepsum.Env.Stats.Z[i]), sep=""))
    print(i)
    print(p1)
    All.corr.plots[[i]] <<- p1
  })

pdf("../../analyses/SRM/AllPeptides-corr-plots.pdf")
grid.arrange( for (i in 10:44) {
  print(All.corr.plots[[i]])
})
dev.off()

png("../../analyses/SRM/Env-parameters-corr.png", width =800 , height =800)
# check out the correlative environmental parameters to see if any correlate with each other (and thus can be left out)
pairs(data.pepsum.Env.Stats.HSP90.Z[,c("DO.mean.loc", "DO.sd.loc", "DO.var.loc", "pH.sd.loc", "pH.var.loc", "Salinity.mean.loc", "Temperature.mean.loc", "Perc.Growth")], main="Plot Matrix, Env Stats for Models")
# DO.sd & DO.var highly correlated. Select DO.sd  since it's correlation p=value is slightly smaller  
# pH.sd & pH.var highly correlated. Select pH.var since it's correlation p=value is smaller and r value higher
dev.off()

# Standardize correlative environmental parameters so I can compare their importance in a linear model
data.pepsum.Env.Stats.HSP90.Z$DO.mean.loc.Z <- scale(data.pepsum.Env.Stats.HSP90.Z$DO.mean.loc, center=T, scale=T)
data.pepsum.Env.Stats.HSP90.Z$DO.sd.loc.Z <- scale(data.pepsum.Env.Stats.HSP90.Z$DO.sd.loc, center=T, scale=T)
data.pepsum.Env.Stats.HSP90.Z$pH.var.loc.Z <- scale(data.pepsum.Env.Stats.HSP90.Z$pH.var.loc, center=T, scale=T)
data.pepsum.Env.Stats.HSP90.Z$Salinity.mean.loc.Z <- scale(data.pepsum.Env.Stats.HSP90.Z$Salinity.mean.loc, center=T, scale=T)
data.pepsum.Env.Stats.HSP90.Z$Temperature.mean.loc.Z <- scale(data.pepsum.Env.Stats.HSP90.Z$Temperature.mean.loc, center=T, scale=T)

# Construct linear model with selected parameters & inspect 
lm.HSP90 <- lm(value ~ DO.mean.loc.Z + DO.sd.loc.Z + pH.var.loc.Z + Salinity.mean.loc.Z + Temperature.mean.loc.Z, data=data.pepsum.Env.Stats.HSP90.Z, singular.ok = TRUE)
summary(lm.HSP90)
plot(rstandard(lm.HSP90), main="Standardized Residuals \nShould be between [-2,2]")
anova(lm.HSP90)
ols_stepwise(lm.HSP90, details = TRUE)
library(leaps)
test.leaps <- regsubsets(value ~ DO.mean.loc + DO.sd.loc + pH.var.loc + Salinity.mean.loc + Temperature.mean.loc + Perc.Growth, data=data.pepsum.Env.Stats.HSP90.Z, nbest=10)
summary(test.leaps)
plot(test.leaps, scale="r2", statistic="rsq")
