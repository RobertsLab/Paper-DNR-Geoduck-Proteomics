# Statistical analysis on protein SRM abundance data. 
# In this analysis, I will assume protein abundances are independent of each other.

# Test for normality
Protein.names <- c(unique(data.melted.plus.pepsum$Protein.Name))
par(mfrow = c(3, 3))
for (i in 1:length(Protein.names)) {
    qqnorm(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["Area"]], main = Protein.names[[i]],
           xlab = "Theoretical Quantiles", ylab = "Peptide Abundance", plot.it = TRUE)
    qqline(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["Area"]])
}

# Some are not normal. Let's use "transformTukey" to identify the best lambda value to transform our dataa
# Great resource: http://rcompanion.org/handbook/I_12.html
library(rcompanion)

#Identify lambda value to use to transform data; value is printed in the console and the following plots are generated: lambda test values, transformed data distribution, transformed data qqplot; each row represents one protein
par(mfrow = c(4, 3)) #NEED TO figure out how to add protein names to the plots
for (i in 1:length(Protein.names)) {
  print(Protein.names[[i]])
  transformTukey(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1, plotit=TRUE, statistic = 1) 
}

#Create new column in dataframe with lambda-transformed area data
data.melted.plus.pepsum$lambda.t <- c(rep("x", times=nrow(data.melted.plus.pepsum)))

# Transform abundance data via its designated lambda value 
#Arachidonate
data.melted.plus.pepsum[grepl(c("Arachidonate"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Arachidonate"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.3

#Catalase
data.melted.plus.pepsum[grepl(c("Catalase"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Catalase"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.15

#Cytochrome
data.melted.plus.pepsum[grepl(c("Cytochrome"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Cytochrome"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.325

#Glycogen
data.melted.plus.pepsum[grepl(c("Glycogen"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Glycogen"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.65

#HSP70
data.melted.plus.pepsum[grepl(c("HSP70"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("HSP70"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.375

#HSP90-alpha
data.melted.plus.pepsum[grepl(c("HSP90-alpha"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("HSP90-alpha"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.425

#PDI
data.melted.plus.pepsum[grepl(c("PDI"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("PDI"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.375

#Peroxiredoxin-1
data.melted.plus.pepsum[grepl(c("Peroxiredoxin-1"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Peroxiredoxin-1"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.65

#Puromycin-sensitive
data.melted.plus.pepsum[grepl(c("Puromycin-sensitive"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Puromycin-sensitive"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.425

#Ras-related
data.melted.plus.pepsum[grepl(c("Ras-related"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Ras-related"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.65

#Sodium/potassium-transporting
data.melted.plus.pepsum[grepl(c("Sodium/potassium-transporting"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Sodium/potassium-transporting"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.675

#Superoxide
data.melted.plus.pepsum[grepl(c("Superoxide"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Superoxide"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.4

#Trifunctional
data.melted.plus.pepsum[grepl(c("Trifunctional"), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] <- (data.melted.plus.pepsum[grepl(c("Trifunctional"), data.melted.plus.pepsum$Protein.Name),][["Area"]]+1)^0.525

#convert lambda.t values to numeric
data.melted.plus.pepsum$lambda.t <- as.numeric(data.melted.plus.pepsum$lambda.t) 

# Regenerate all QQplots using the transformed area data, lambda.t
par(mfrow = c(3, 3))
for (i in 1:length(Protein.names)) {
  qqnorm(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]], main = paste(Protein.names[[i]], "\nlambda-transformed", sep=""),
         xlab = "Theoretical Quantiles", ylab = "Peptide Abundance", plot.it = TRUE)
  qqline(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]])
}

# Is the data balanced? Result: FB-Bare has less data points b/c I removed G057 from the data, which is from FB-B. Not sure exactly how to incorporate that into the analysis... 
replications(lambda.t ~ Protein.Name*BOTH, data=data.melted.plus.pepsum)

# Create box plots and ID outliers
OutVals <- vector("list", length(Protein.names))
names(OutVals) <- Protein.names
par(mfrow = c(3, 3))
for (i in 1:length(Protein.names)) {
  OutVals[[i]] = boxplot(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] ~ data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["BOTH"]], main = paste(Protein.names[[i]], "\nlambda-transformed", sep=""), xlab = "Location", ylab = "Peptide Abundance (transf.)", type="p")$out
}

# Generate a dataframe of outliers to inspect. If ANOVA results indicate differences in the peptides/locations included in the outliers list, then consider whether the outliers should be removed from the dataset.
outliers = list()
for (i in 1:length(Protein.names)) {
  outliers[[i]] <- data.melted.plus.pepsum[data.melted.plus.pepsum$lambda.t %in% OutVals[[i]],]
}
Prot.outliers  = do.call(rbind, outliers) #this is a dataframe with outliers, as determined by boxplots

# 2-way ANOVA on protein abundance for each protein individually: data is analyzed by protein, but each point represents the sum of transitions within a peptide, lambda-transformed. 
p.ANOVA <- vector("list", length(Protein.names))
p.tukey <- vector("list", length(Protein.names))
names(p.ANOVA) <- Protein.names
names(p.tukey) <- Protein.names
for (i in 1:length(Protein.names)) {
  temp1 <- aov(lambda.t ~ REGION*SITE*TREATMENT, data=data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),]) 
  temp2 <- summary(temp1)
  p.ANOVA[[i]] <- as.data.frame(temp2[[1]])
  p.ANOVA[[i]]$Protein <- c((Protein.names[i]))
  p.ANOVA[[i]]$Comparison <- rownames(p.ANOVA[[i]])
  p.tukey[[i]] <- TukeyHSD(temp1, conf.level=.95)
}
Prot.ANOVA <- do.call(rbind, p.ANOVA) #this is a dataframe with ANOVA results for each protein with each comparison. 
# To check out results of the Tukey HSD test for specific proteins, use: p.tukey["HSP70"], p.tukey["HSP90-alpha"], etc.

# Use a few multiple comparison corrections to adjust the significance (alpha) standard.  The most conservative P-adjusted is using the Bonferroni method, which multiplies the P-value by the # comparisons (in this case, 13 due to the 13 proteins).
Comparisons <- unique(Prot.ANOVA$Comparison)
Prot.ANOVA$P.adj.bonf <- NA
Prot.ANOVA$P.adj.BH <- NA
Prot.ANOVA$P.adj.holm <- NA
for (i in 1:length(unique(Prot.ANOVA$Comparison))) {
  Prot.ANOVA[grepl(c(Comparisons[[i]]), Prot.ANOVA$Comparison),"P.adj.bonf"] <- p.adjust(Prot.ANOVA[grepl(c(Comparisons[[i]]), Prot.ANOVA$Comparison),][["Pr(>F)"]], method="bonferroni")
  Prot.ANOVA[grepl(c(Comparisons[[i]]), Prot.ANOVA$Comparison),"P.adj.BH"] <- p.adjust(Prot.ANOVA[grepl(c(Comparisons[[i]]), Prot.ANOVA$Comparison),][["Pr(>F)"]], method="BH")
  Prot.ANOVA[grepl(c(Comparisons[[i]]), Prot.ANOVA$Comparison),"P.adj.holm"] <- p.adjust(Prot.ANOVA[grepl(c(Comparisons[[i]]), Prot.ANOVA$Comparison),][["Pr(>F)"]], method="holm")
}

# Re-create box plots using the REGION grouping factor to ID outliers; if any of the ANOVA results indicate differences in proteins included in the outliers list, then consider whether the outliers should be removed from the dataset.
OutVals.reg <- vector("list", length(Protein.names))
names(OutVals.reg) <- Protein.names
par(mfrow = c(2, 2))
for (i in 1:length(Protein.names)) {
  OutVals.reg[[i]] = boxplot(data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["lambda.t"]] ~ data.melted.plus.pepsum[grepl(c(Protein.names[[i]]), data.melted.plus.pepsum$Protein.Name),][["REGION"]], main = paste(Protein.names[[i]], "\nlambda-transformed", sep=""), xlab = "Region", ylab = "Peptide Abundance (transf.)", type="p")$out
}
outliers.reg = list()
for (i in 1:length(Protein.names)) {
  outliers.reg[[i]] <- data.melted.plus.pepsum[data.melted.plus.pepsum$lambda.t %in% OutVals.reg[[i]],]
}
Prot.outliers.reg  = do.call(rbind, outliers.reg) #this is a dataframe with outliers, as determined by boxplots
View(Prot.outliers.reg)
# No HSP70, HSP90-alpha, or Trifunctional data points, included on the regional outlier list. Should move forward with those two proteins, grouped by REGION.

# Findings: the following proteins are significantly different between regions (North = Fidalgo Bay, Port Gamble; South = Case Inlet, Willapa Bay): 
# ====> Puromycin-sensitive aminopeptidase
# ====> HSP70
# ====> HSP90-alpha
# ====> Trifunctional enzyme subunit
# While ANOVA indicates Arachidonate is significantly different between sites, there are 5 outlying data points; should consider removing those, re-running ANOVA to see if the outliers are having a large effect. 

# Plot peptides within a protein against each other. Should be linearly correlated. summary() shows equation with R^2
# Peptide 1 x Peptide 2
pep12 <- lm(`1` ~ `2`, data=data.melted.plus.pepsum.wide)
summary(pep12)
with(data.melted.plus.pepsum.wide, plot(`1`,`2`))
abline(pep12)
# Peptide 1 x Peptide 3
pep13 <- lm(`1` ~ `3`, data=data.melted.plus.pepsum.wide)
summary(pep13)
with(data.melted.plus.pepsum.wide, plot(`1`,`3`))
abline(pep13)
# Peptide 2 x Peptide 3
pep23 <- lm(`2` ~ `3`, data=data.melted.plus.pepsum.wide)
summary(pep23)
with(data.melted.plus.pepsum.wide, plot(`2`,`3`))
abline(pep13)

# Select 1 of the peptides to develop the linear model. 
# I will select peptide #1. 