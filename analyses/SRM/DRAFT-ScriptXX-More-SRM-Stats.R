# More stats on SRM data

# Kruskal-Wallis rank sum test, investigate whether the population distributions are identical without assuming normal distribution.

options(scipen = 999) #disable scientific notation

Proteins <- list("Arachidonate"=Arachidonate, "Catalase"=Catalase, "Cytochrome"=Cytochrome, "Glycogen"=Glycogen, "HSP70"=HSP70, "HSP90"=HSP90, "Peroxiredoxin"=Peroxiredoxin, "PDI"=PDI, "Puromycin"=Puromycin, "Rab.11B"=Rab.11B, "NAK"=NAK, "Superoxide"=Superoxide, "Trifunctional"=Trifunctional)

SRM.stats <- data.frame(matrix(vector(), length(Proteins)*4, 5, dimnames=list(c(), c("Protein", "Group", "KW.chi", "KW.df", "KW.p"))), stringsAsFactors = F)
nProteins <- length(Proteins)
for (i in 1:nProteins) {
  kw.site <- kruskal.test(Area ~ SITE, data=Proteins[[i]]) #Run KW grouped by Site
  SRM.stats[i,1] <- names(Proteins[i])
  SRM.stats[i,2] <- kw.site$data.name
  SRM.stats[i,3] <- kw.site$statistic
  SRM.stats[i,4] <- kw.site$parameter
  SRM.stats[i,5] <- kw.site$p.value
  kw.treatment <- kruskal.test(Area ~ TREATMENT, data=Proteins[[i]]) #Run KW grouped by Treatment
  SRM.stats[i+nProteins,1] <- names(Proteins[i])
  SRM.stats[i+nProteins,2] <- kw.treatment$data.name
  SRM.stats[i+nProteins,3] <- kw.treatment$statistic
  SRM.stats[i+nProteins,4] <- kw.treatment$parameter
  SRM.stats[i+nProteins,5] <- kw.treatment$p.value
  kw.both <- kruskal.test(Area ~ BOTH, data=Proteins[[i]]) #Run KW grouped by both Site & Treatment
  SRM.stats[i+2*nProteins,1] <- names(Proteins[i])
  SRM.stats[i+2*nProteins,2] <- kw.both$data.name
  SRM.stats[i+2*nProteins,3] <- kw.both$statistic
  SRM.stats[i+2*nProteins,4] <- kw.both$parameter
  SRM.stats[i+2*nProteins,5] <- kw.both$p.value
  Proteins[[i]][,6] <- as.factor(Proteins[[i]][,6]) #make Region a factor ('cause it wasn't)
  kw.region <- kruskal.test(Area ~ REGION, data=Proteins[[i]]) #Run KW grouped by Region
  SRM.stats[i+3*nProteins,1] <- names(Proteins[i])
  SRM.stats[i+3*nProteins,2] <- kw.region$data.name
  SRM.stats[i+3*nProteins,3] <- kw.region$statistic
  SRM.stats[i+3*nProteins,4] <- kw.region$parameter
  SRM.stats[i+3*nProteins,5] <- kw.region$p.value
}
View(SRM.stats[order(SRM.stats[,5]),])

### Dunn's test of multiple comparisons using rank sums
library(dunn.test)
dunn.test(x=data.melted.plus$Area, g=data.melted.plus$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=data.melted.plus$Area, g=data.melted.plus$TREATMENT, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)

dunn.test(x=HSP70$Area, g=HSP70$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=HSP90$Area, g=HSP90$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=PDI$Area, g=PDI$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)


### Use Tukey's Honest Significant Differest test, since sample sizes are all equal
TukeyHSD(HSP70, Area)
?TukeyHSD


### PERMANOVA - figure out how to do this! 



### Pairwise T-test
pairwise.t.test(HSP90$Area, HSP90$SITE, p.adj = "none")
pairwise.t.test(data.melted.plus$Area, data.melted.plus$SITE, p.adj = "none")

### Boneyard

# Is data normal? 

library(reshape)
library(ggplot2)
library(tidyr)

SRM.data.mean.t.melted <- melt(SRM.data.mean.t)
hist(SRM.data.mean.t.melted$value)
shapiro.test(SRM.data.mean.t.melted$value)
qqnorm(SRM.data.mean.t.melted$value)
qqline(SRM.data.mean.t.melted$value)

# is it normal after log transformation?
SRM.data.mean.t.log.melted <- melt(SRM.data.mean.t.log)
hist(SRM.data.mean.t.log.melted$value)
shapiro.test(SRM.data.mean.t.log.melted$value)
qqnorm(SRM.data.mean.t.log.melted$value)
qqline(SRM.data.mean.t.log.melted$value)
