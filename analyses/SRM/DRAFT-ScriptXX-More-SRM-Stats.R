# More stats on SRM data

# ANOSIM via loop for all proteins
Proteins4Anosim <- list("Arachidonate"=Arachidonate.4anosim, "Catalase"=Catalase.4anosim, "Cytochrome"=Cytochrome.4anosim, "Glycogen"=Glycogen.4anosim, "HSP70"=HSP70.4anosim, "HSP90"=HSP90.4anosim, "Peroxiredoxin"=Peroxiredoxin.4anosim, "PDI"=PDI.4anosim, "Puromycin"=Puromycin.4anosim, "Rab.11B"=Rab.11B.4anosim, "NAK"=NAK.4anosim, "Superoxide"=Superoxide.4anosim, "Trifunctional"=Trifunctional.4anosim)
nProteins4anosim <- length(Proteins4Anosim)
stats.ANOSIM <- data.frame(matrix(vector(), length(Proteins4Anosim)*3, 4, dimnames=list(c(), c("Protein", "Group", "ANOSIM-R", "ANOSIM-P"))), stringsAsFactors = F)

for (i in 1:nProteins4anosim) {
  ANOSIM.vegdist <- vegdist(Proteins4Anosim[[i]][,!grepl(c("SITE|TREATMENT|BOTH|REGION"), colnames(Proteins4Anosim[[i]]))], 'bray', na.rm=TRUE)
  ANOSIM.site <- anosim(ANOSIM.vegdist, Proteins4Anosim[[1]]$SITE, permutations = 2000) 
  stats.ANOSIM[i,1] <- names(Proteins4Anosim[i])
  stats.ANOSIM[i,2] <- "Site"
  stats.ANOSIM[i,3] <- ANOSIM.site$statistic
  stats.ANOSIM[i,4] <- ANOSIM.site$signif
  ANOSIM.treatment <- anosim(ANOSIM.vegdist, Proteins4Anosim[[i]]$TREATMENT, permutations = 2000) 
  stats.ANOSIM[i+nProteins4anosim,1] <- names(Proteins4Anosim[i])
  stats.ANOSIM[i+nProteins4anosim,2] <- "Treatment"
  stats.ANOSIM[i+nProteins4anosim,3] <- ANOSIM.treatment$statistic
  stats.ANOSIM[i+nProteins4anosim,4] <- ANOSIM.treatment$signif
  ANOSIM.region <- anosim(ANOSIM.vegdist, Proteins4Anosim[[i]]$SITE, permutations = 2000) 
  stats.ANOSIM[i+2*nProteins4anosim,1] <- names(Proteins4Anosim[i])
  stats.ANOSIM[i+2*nProteins4anosim,2] <- "Region"
  stats.ANOSIM[i+2*nProteins4anosim,3] <- ANOSIM.region$statistic
  stats.ANOSIM[i+2*nProteins4anosim,4] <- ANOSIM.region$signif
}
View(stats.ANOSIM)
write.csv(file="../../analyses/SRM/ANOSIM-Stats.csv", stats.ANOSIM)

# Kruskal-Wallis rank sum test, investigate whether the population distributions are identical without assuming normal distribution.
# options(scipen = 999) #disable scientific notation

Proteins <- list("Arachidonate"=Arachidonate, "Catalase"=Catalase, "Cytochrome"=Cytochrome, "Glycogen"=Glycogen, "HSP70"=HSP70, "HSP90"=HSP90, "Peroxiredoxin"=Peroxiredoxin, "PDI"=PDI, "Puromycin"=Puromycin, "Rab.11B"=Rab.11B, "NAK"=NAK, "Superoxide"=Superoxide, "Trifunctional"=Trifunctional)
nProteins <- length(Proteins)

# All data:
kruskal.test(Area ~ SITE, data=data.melted.plus) 
kruskal.test(Area ~ TREATMENT, data=data.melted.plus)
kruskal.test(Area ~ REGION, data=data.melted.plus)

# Proteins individually:
stats.KW <- data.frame(matrix(vector(), length(Proteins)*4, 5, dimnames=list(c(), c("Protein", "Group", "KW.chi", "KW.df", "KW.p"))), stringsAsFactors = F)
for (i in 1:nProteins) {
  kw.site <- kruskal.test(Area ~ SITE, data=Proteins[[i]]) #Run KW grouped by Site
  stats.KW[i,1] <- names(Proteins[i])
  stats.KW[i,2] <- kw.site$data.name
  stats.KW[i,3] <- kw.site$statistic
  stats.KW[i,4] <- kw.site$parameter
  stats.KW[i,5] <- kw.site$p.value
  kw.treatment <- kruskal.test(Area ~ TREATMENT, data=Proteins[[i]]) #Run KW grouped by Treatment
  stats.KW[i+nProteins,1] <- names(Proteins[i])
  stats.KW[i+nProteins,2] <- kw.treatment$data.name
  stats.KW[i+nProteins,3] <- kw.treatment$statistic
  stats.KW[i+nProteins,4] <- kw.treatment$parameter
  stats.KW[i+nProteins,5] <- kw.treatment$p.value
  kw.both <- kruskal.test(Area ~ BOTH, data=Proteins[[i]]) #Run KW grouped by both Site & Treatment
  stats.KW[i+2*nProteins,1] <- names(Proteins[i])
  stats.KW[i+2*nProteins,2] <- kw.both$data.name
  stats.KW[i+2*nProteins,3] <- kw.both$statistic
  stats.KW[i+2*nProteins,4] <- kw.both$parameter
  stats.KW[i+2*nProteins,5] <- kw.both$p.value
  Proteins[[i]][,6] <- as.factor(Proteins[[i]][,6]) #make Region a factor ('cause it wasn't)
  kw.region <- kruskal.test(Area ~ REGION, data=Proteins[[i]]) #Run KW grouped by Region
  stats.KW[i+3*nProteins,1] <- names(Proteins[i])
  stats.KW[i+3*nProteins,2] <- kw.region$data.name
  stats.KW[i+3*nProteins,3] <- kw.region$statistic
  stats.KW[i+3*nProteins,4] <- kw.region$parameter
  stats.KW[i+3*nProteins,5] <- kw.region$p.value
}
View(stats.KW[order(stats.KW[,5]),])
write.csv(file="../../analyses/SRM/Krusgall-Wallis-Stats.csv", stats.KW)

### Dunn's test of multiple comparisons using rank sums
library(dunn.test)
dunn.test(x=data.melted.plus$Area, g=data.melted.plus$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=data.melted.plus$Area, g=data.melted.plus$TREATMENT, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=HSP70$Area, g=HSP70$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=HSP90$Area, g=HSP90$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=PDI$Area, g=PDI$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)

dunn.test(x=HSP70$Area, g=HSP70$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=HSP70$Area, g=HSP70$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)
dunn.test(x=PDI$Area, g=PDI$SITE, kw=TRUE, label=TRUE, list=TRUE, alpha=0.05)

### Use Tukey's Honest Significant Differest test, since sample sizes are all equal
TukeyHSD(HSP70, Area)
?TukeyHSD

### PERMANOVA - figure out how to do this! 
length(unique(data.melted.plus.prosum$SAMPLE))

### Pairwise T-test
pairwise.t.test(HSP90$Area, HSP90$SITE, p.adj = "none")
pairwise.t.test(data.melted.plus$Area, data.melted.plus$SITE, p.adj = "none")

### 1-way ANOVA on Protein area, which represent the sum of the transitions in those proteins.

# on transitions pooled by sample # for each protein individually
transumpro4stats.log <- transumpro4stats
transumpro4stats.log[,6:18] <- lapply(transumpro4stats[,6:18], log)
Proteins4aov <- noquote(names(transumpro4stats.log[-1:-5]))
nProteins4aov <- length(Proteins4aov)
stats.aov <- data.frame(matrix(vector(), length(Proteins4aov)*3+3, 3, dimnames=list(c(), c("Protein", "Group", "ANOVA-P"))), stringsAsFactors = F)
stats.tukey <- data.frame(matrix(vector(), length(Proteins4aov)*8+8, 4, dimnames=list(c(), c("Protein", "Group", "Comparison", "P"))), stringsAsFactors = F)
tukeyseq <- c(seq(from=0, to=104, by=8))
for (i in 1:nProteins4aov) { 
  aov.site <- aov(transumpro4stats.log[[5+i]] ~ transumpro4stats.log[[2]], data=transumpro4stats.log) 
  stats.aov[i,1] <- Proteins4aov[i]
  stats.aov[i,2] <- "Site"
  stats.aov[i,3] <- summary(aov.site)[[1]][["Pr(>F)"]][[1]]
  aov.treatment <- aov(transumpro4stats.log[[5+i]] ~ transumpro4stats.log[[3]], data=transumpro4stats.log)  
  stats.aov[i+nProteins,1] <- Proteins4aov[i]
  stats.aov[i+nProteins,2] <- "Treatment"
  stats.aov[i+nProteins,3] <- summary(aov.treatment)[[1]][["Pr(>F)"]][[1]]
  aov.region <- aov(transumpro4stats.log[[5+i]] ~ transumpro4stats.log[[5]], data=transumpro4stats.log) 
  stats.aov[i+2*nProteins,1] <- Proteins4aov[i]
  stats.aov[i+2*nProteins,2] <- "Region"
  stats.aov[i+2*nProteins,3] <- summary(aov.region)[[1]][["Pr(>F)"]][[1]]
  tukey.site <- TukeyHSD(aov.site, ordered=T, conf.level=0.95)              #####not working for tukey table
  tukey.treatment <- TukeyHSD(aov.treatment, ordered=T, conf.level=0.95)
  tukey.region <- TukeyHSD(aov.region, ordered=T, conf.level=0.95)
  stats.tukey[tukeyseq[i]+1,1] <- Proteins4aov[i]
  stats.tukey[tukeyseq[i]+1,2] <- "Treatment"
  stats.tukey[tukeyseq[i]+1,3] <- rownames(tukey.treatment$`transumpro4stats.log[[3]]`)
  stats.tukey[tukeyseq[i]+1,4] <- tukey.treatment$`transumpro4stats.log[[3]]`[4]
  stats.tukey[tukeyseq[i]+2,1] <- Proteins4aov[i]
  stats.tukey[tukeyseq[i]+2,2] <- "Region"    
  stats.tukey[tukeyseq[i]+2,3] <- rownames(tukey.region$`transumpro4stats.log[[5]]`)
  stats.tukey[tukeyseq[i]+2,4] <- tukey.region$`transumpro4stats.log[[5]]`[1,4]
  for (j in 1:6) {
    stats.tukey[tukeyseq[i]+2+j,1] <- Proteins4aov[i]
    stats.tukey[tukeyseq[i]+2+j,2] <- "Site"
    stats.tukey[tukeyseq[i]+2+j,3] <- names(tukey.site$`transumpro4stats.log[[2]]`[1:6,4][j])
    stats.tukey[tukeyseq[i]+2+j,4] <- tukey.site$`transumpro4stats.log[[2]]`[1:6,4][j]
  }
}

# on transitions pooled by sample # for all proteins, add to stats datasets created above
transumpro4stats.melt.log <- data.melted.plus.prosum
transumpro4stats.melt.log$Area <- log(transumpro4stats.melt.log$Area)
shapiro.test(transumpro4stats.melt.log$Area) # is this normal? yes.

aov.all.site <- aov(transumpro4stats.melt.log$Area ~ transumpro4stats.melt.log$SITE, data=transumpro4stats.melt.log)
stats.aov[nrow(stats.aov)-2,1] <- "All Proteins"
stats.aov[nrow(stats.aov)-2,2] <- "Site"
stats.aov[nrow(stats.aov)-2,3] <- summary(aov.all.site)[[1]][["Pr(>F)"]][[1]]
tukey.all.site <- TukeyHSD(aov.all.site, ordered = T, conf.level = 0.95)
for (k in 1:6) {
  stats.tukey[nrow(stats.tukey)-(k+1),1] <- "All Proteins"
  stats.tukey[nrow(stats.tukey)-(k+1),2] <- "Site"
  stats.tukey[nrow(stats.tukey)-(k+1),3] <- names(tukey.all.site$`transumpro4stats.melt.log$SITE`[1:6,4][k])
  stats.tukey[nrow(stats.tukey)-(k+1),4] <- tukey.all.site$`transumpro4stats.melt.log$SITE`[1:6,4][k]
}
aov.all.region <- aov(transumpro4stats.melt.log$Area ~ transumpro4stats.melt.log$REGION, data=transumpro4stats.melt.log)
stats.aov[nrow(stats.aov)-1,1] <- "All Proteins"
stats.aov[nrow(stats.aov)-1,2] <- "Region"
stats.aov[nrow(stats.aov)-1,3] <- summary(aov.all.region)[[1]][["Pr(>F)"]][[1]]
tukey.all.region <- TukeyHSD(stats.aov.all.region, ordered=T, conf.level = 0.95)
stats.tukey[nrow(stats.tukey)-1,1] <- "All Proteins"
stats.tukey[nrow(stats.tukey)-1,2] <- "Region"
stats.tukey[nrow(stats.tukey)-1,3] <- rownames(tukey.all.region$`transumpro4stats.melt.log$REGION`)
stats.tukey[nrow(stats.tukey)-1,4] <- tukey.all.region$`transumpro4stats.melt.log$REGION`[1,4]
aov.all.treatment <- aov(transumpro4stats.melt.log$Area ~ transumpro4stats.melt.log$TREATMENT, data=transumpro4stats.melt.log)
stats.aov[nrow(stats.aov),1] <- "All Proteins"
stats.aov[nrow(stats.aov),2] <- "Treatment"
stats.aov[nrow(stats.aov),3] <- summary(aov.all.treatment)[[1]][["Pr(>F)"]][[1]]
tukey.all.region <- TukeyHSD(aov.all.treatment, ordered=T, conf.level = 0.95)
stats.tukey[nrow(stats.tukey),1] <- "All Proteins"
stats.tukey[nrow(stats.tukey),2] <- "Region"
stats.tukey[nrow(stats.tukey),3] <- rownames(tukey.all.region$`transumpro4stats.melt.log$TREATMENT`)
stats.tukey[nrow(stats.tukey),4] <- tukey.all.region$`transumpro4stats.melt.log$TREATMENT`[1,4]
write.csv(file="../../analyses/SRM/ANOVA-Stats.csv", stats.aov)
write.csv(file="../../analyses/SRM/TukeyHSD-Stats.csv", stats.tukey)

stats.aov[c(stats.aov$ANOVA.P < .05),]
stats.tukey[c(stats.tukey$P < .05),]


### Overall stats for paper
library(plyr)
summary(SRM.reps4stats.plots$variance) # summary stats on CV for technical reps, BEFORE screening out poor quality reps. 
summary(SRM.reps4stats.s.plots$variance) # summary stats on CV for technical reps, AFTER screening out poor quality reps. 
count(SRM.reps4stats.s.plots[c(SRM.reps4stats.s.plots$variance > 100),])

To <- nrow(SRM.data[!grepl(c("PRTC|Protein Name"), SRM.data$`Protein Name`),]) 
Tf <- nrow(SRM.data.screened[!grepl(c("PRTC|Protein Name"), SRM.data.screened$`Protein Name`),])
Tf/To # % transitions kept in analysis
Tf-To # # transitions discarded
Pro <- nrow(count(SRM.data[!grepl(c("PRTC|Protein Name"), SRM.data$`Protein Name`),1]))
Prf <- nrow(count(SRM.data.screened.noPRTC$`Protein Name`))
Peo <- nrow(count(SRM.data[!grepl(c("PRTC|Protein Name"), SRM.data$`Protein Name`),3]))
Pef <- nrow(count(SRM.data.screened.noPRTC$`Peptide Sequence`))
Prf/Pro # % proteins kept in analysis
Pef/Peo # % peptides kept in analysis
Ro <- nrow(SRM.temp2) #original # tech reps
Rf <- nrow(SRM.temp2.screened)  # final # tech reps
Rf/Ro
summary(data.melted.plus$Area)
summary(data.melted.plus.prosum$Area) #summary stats on protein abundance (summed transitions)
data.melted.plus.prosum$log <- log(data.melted.plus.prosum$Area)
Agg <- do.call(data.frame, aggregate(Area ~ Protein.Name + SITE + REGION, data=data.melted.plus, FUN= function(x) c(mn = mean(x), sd = sd(x))))
Agg$Area.cv <- Agg$Area.sd / Agg$Area.mn
Agg.HSP90.site <- do.call(data.frame, aggregate(Area ~ Protein.Name + SITE + REGION, data=subset(data.melted.plus.prosum, Protein.Name %in% "HSP90-alpha"), FUN= function(x) c(mn = mean(x), sd = sd(x))))
Agg.HSP90.site$Area.cv <- Agg.HSP90.site$Area.sd/Agg.HSP90.site$Area.mn
Agg.HSP70.region <- do.call(data.frame, aggregate(Area ~ Protein.Name + REGION, data=subset(data.melted.plus.prosum, Protein.Name %in% "HSP70"), FUN= function(x) c(mn = mean(x), sd = sd(x))))
Agg.HSP70.region$Area.cv <- Agg.HSP70.region$Area.sd/Agg.HSP70.region$Area.mn
Agg.PDI.region <- do.call(data.frame, aggregate(Area ~ Protein.Name + REGION, data=subset(data.melted.plus.prosum, Protein.Name %in% "PDI"), FUN= function(x) c(mn = mean(x), sd = sd(x))))
Agg.PDI.region$Area.cv <- Agg.PDI.region$Area.sd/Agg.PDI.region$Area.mn

hist(log(data.melted.plus.prosum$Area))


Agg
Agg.HSP70.region
16899494/8648894 #N/S

Agg.HSP90.site
38426083/29888163 #FB/PG
38426083/((16554723+19046271)/2) #FB/South
29888163/((16554723+19046271)/2) #PG/South

Agg.PDI.region
14947785/8178076 #N/S

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

# Checking normality of HSP70 area (transitions summed)
hist(transumpro4stats$HSP70) 
hist(log(transumpro4stats$HSP70))
shapiro.test(transumpro4stats$HSP70) #is HSP70 summed transition data normal? no.
shapiro.test(log(transumpro4stats$HSP70)) #is HSP70 summed transition data log-transformed normal? yes
qqnorm(transumpro4stats$HSP70)
qqnorm(log(transumpro4stats$HSP70))

# Checking normality of HSP90 area (transitions summed)
hist(transumpro4stats$`HSP90-alpha`)
hist(log(transumpro4stats$`HSP90-alpha`))
shapiro.test(transumpro4stats$`HSP90-alpha`) #is HSP90 summed transition data normal? no
shapiro.test(log(transumpro4stats$`HSP90-alpha`)) #log? yes
qqnorm(transumpro4stats$`HSP90-alpha`)
qqnorm(log(transumpro4stats$`HSP90-alpha`))

# Checking normality of PDI area (transitions summed)
hist(transumpro4stats$PDI)
hist(log(transumpro4stats$PDI))
shapiro.test(transumpro4stats$PDI) #is PDI summed transition data normal?
shapiro.test(log(transumpro4stats$PDI)) #is PDI summed transition data normal?
qqnorm(transumpro4stats$PDI)
qqnorm(log(transumpro4stats$PDI))

# is it normal after log transformation?
SRM.data.mean.t.log.melted <- melt(SRM.data.mean.t.log)
hist(SRM.data.mean.t.log.melted$value)
shapiro.test(SRM.data.mean.t.log.melted$value)
qqnorm(SRM.data.mean.t.log.melted$value)
qqline(SRM.data.mean.t.log.melted$value)
