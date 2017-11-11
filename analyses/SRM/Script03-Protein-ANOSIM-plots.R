# Script #3 in data processing for SRM data (not normalized)

######## Run ANOSIM for each protein separately & plot.

# Use data4anosim.noNA dataset, melt data to prepare for ggplot
library(reshape2)
data.melted <- melt(data4anosim.noNA, id=c("SAMPLE", "SITE", "TREATMENT", "BOTH", "REGION"), variable.name = "Transition", value.name = "Area")

View(data4anosim.noNA)
View(data.melted)

# Merge protein names back to abundance data
SRM.proteins <- data.frame(SRM.data.screened.noPRTC[,1:4]) #protein name to each transition
SRM.proteins[,1] <- sub(" cds.*", "", SRM.proteins[,1])
data.melted.plus <- merge(x=data.melted, y=SRM.proteins, by.x = "Transition", by.y = "row.names", all.x=TRUE, all.y=FALSE)
colnames(data.melted.plus)[1] <- "Pep.Trans"
write.csv(data.melted.plus, "../../analyses/SRM/SRM-data-meaned-melted.csv")

# Prepare data for anosim by protein, isolating the melted & annotated area data
Arachidonate <- data.melted.plus[grepl(c("Arachidonate"), data.melted.plus$Protein.Name),]
Catalase <- data.melted.plus[grepl(c("Catalase"), data.melted.plus$Protein.Name),]
Cytochrome <- data.melted.plus[grepl(c("Cytochrome"), data.melted.plus$Protein.Name),]
Glycogen <- data.melted.plus[grepl(c("Glycogen"), data.melted.plus$Protein.Name),]
HSP70 <- data.melted.plus[grepl(c("HSP70"), data.melted.plus$Protein.Name),]
HSP90 <- data.melted.plus[grepl(c("HSP90-alpha"), data.melted.plus$Protein.Name),]
Peroxiredoxin <- data.melted.plus[grepl(c("Peroxiredoxin"), data.melted.plus$Protein.Name),]
PDI <- data.melted.plus[grepl(c("PDI"), data.melted.plus$Protein.Name),]
Puromycin <- data.melted.plus[grepl(c("Puromycin-sensitive"), data.melted.plus$Protein.Name),]
Rab.11B <- data.melted.plus[grepl(c("Ras-related"), data.melted.plus$Protein.Name),]
NAK <- data.melted.plus[grepl(c("Sodium/potassium-transporting"), data.melted.plus$Protein.Name),]
Superoxide <- data.melted.plus[grepl(c("Superoxide"), data.melted.plus$Protein.Name),]
Trifunctional <- data.melted.plus[grepl(c("Trifunctional"), data.melted.plus$Protein.Name),]

# Convert data to long form by casting it, making first column row names
Arachidonate.4anosim <- data.frame(merge(dcast(Arachidonate[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE", all.y = TRUE), row.names = 1)
Catalase.4anosim <- data.frame(merge(dcast(Catalase[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Cytochrome.4anosim <- data.frame(merge(dcast(Cytochrome[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Glycogen.4anosim <- data.frame(merge(dcast(Glycogen[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
HSP70.4anosim <- data.frame(merge(dcast(HSP70[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
HSP90.4anosim <- data.frame(merge(dcast(HSP90[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Peroxiredoxin.4anosim <- data.frame(merge(dcast(Peroxiredoxin[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
PDI.4anosim <- data.frame(merge(dcast(PDI[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Puromycin.4anosim <- data.frame(merge(dcast(Puromycin[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Rab.11B.4anosim <- data.frame(merge(dcast(Rab.11B[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
NAK.4anosim <- data.frame(merge(dcast(NAK[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Superoxide.4anosim <- data.frame(merge(dcast(Superoxide[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)
Trifunctional.4anosim <- data.frame(merge(dcast(Trifunctional[,c(1,2,3,6,7)], SAMPLE~Pep.Trans, value.var="Area"), samples4anosim, by.x="SAMPLE", by.y="SAMPLE"), row.names = 1)

# ANOSIM between sites, each protein

# P=0.25837
Arachidonate.vegdist <- vegdist(Arachidonate.4anosim[,1:9], 'bray', na.rm=TRUE)
Arachidonate.ANOSIM.site <-anosim(Arachidonate.vegdist, Arachidonate.4anosim$SITE, permutations = 2000)
Arachidonate.ANOSIM.region <-anosim(Arachidonate.vegdist, Arachidonate.4anosim$REGION, permutations = 2000)
summary(Arachidonate.ANOSIM.site)
summary(Arachidonate.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Arachidonate%03d.png")
plot(Arachidonate.ANOSIM.site, main="Arachidonate, ANOSIM by Site")
plot(Arachidonate.ANOSIM.region, main="Arachidonate, ANOSIM by Region")
# dev.off()
# graphics.off()
Arachidonate.ANOSIM.site$signif

# P=0.83008
Catalase.vegdist <- vegdist(Catalase.4anosim[,1:6], 'bray', na.rm=TRUE)
Catalase.ANOSIM.site <-anosim(Catalase.vegdist, grouping=Catalase.4anosim$SITE, permutations = 2000)
Catalase.ANOSIM.region <-anosim(Catalase.vegdist, grouping=Catalase.4anosim$REGION, permutations = 2000)
summary(Catalase.ANOSIM.site)
summary(Catalase.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Catalase%03d.png")
plot(Catalase.ANOSIM.site,  main="Catalase, ANOSIM by Site")
plot(Catalase.ANOSIM.region,  main="Catalase, ANOSIM by Region")
# dev.off()
# graphics.off()

# P=0.69715
Cytochrome.vegdist <- vegdist(Cytochrome.4anosim[,1:9], 'bray', na.rm=TRUE)
Cytochrome.ANOSIM.site <-anosim(Cytochrome.vegdist, grouping=Cytochrome.4anosim$SITE, permutations = 2000)
Cytochrome.ANOSIM.region <-anosim(Cytochrome.vegdist, grouping=Cytochrome.4anosim$REGION, permutations = 2000)
summary(Cytochrome.ANOSIM.site)
summary(Cytochrome.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Cytochrome%03d.png")
plot(Cytochrome.ANOSIM.site, main="Cytochrome P450, ANOSIM by Site")
plot(Cytochrome.ANOSIM.region, main="Cytochrome P450, ANOSIM by Region")
# dev.off()
# graphics.off()

# P=0.63768
Glycogen.vegdist <- vegdist(Glycogen.4anosim[,1:9], 'bray', na.rm=TRUE)
Glycogen.ANOSIM.site <-anosim(Glycogen.vegdist, grouping=Glycogen.4anosim$SITE, permutations = 2000)
Glycogen.ANOSIM.region <-anosim(Glycogen.vegdist, grouping=Glycogen.4anosim$REGION, permutations = 2000)
summary(Glycogen.ANOSIM.site)
summary(Glycogen.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Glycogen%03d.png")
plot(Glycogen.ANOSIM.site, main="Glycogen, ANOSIM by Site")
plot(Glycogen.ANOSIM.region, main="Glycogen, ANOSIM by Region")
# dev.off()
# graphics.off()

# P=0.0009995
HSP70.vegdist <- vegdist(HSP70.4anosim[,1:6], 'bray', na.rm=TRUE)
HSP70.ANOSIM.site <-anosim(HSP70.vegdist, grouping=HSP70.4anosim$SITE, permutations = 2000)
HSP70.ANOSIM.region <-anosim(HSP70.vegdist, grouping=HSP70.4anosim$REGION, permutations = 2000)
summary(HSP70.ANOSIM.site) #R=0.1525, P= 0.00049975 
summary(HSP70.ANOSIM.region) #R=0.2074, 0.00049975 
png("../../analyses/SRM/ANOSIM-HSP70%03d.png")
plot(HSP70.ANOSIM.site, main="HSP70, ANOSIM by Site")
plot(HSP70.ANOSIM.region, main="HSP70, ANOSIM by Region")
dev.off()
graphics.off()

# P=0.035982
HSP90.vegdist <- vegdist(HSP90.4anosim[,1:9], 'bray', na.rm=TRUE)
HSP90.ANOSIM.site <-anosim(HSP90.vegdist, grouping=HSP90.4anosim$SITE, permutations = 2000)
HSP90.ANOSIM.region <-anosim(HSP90.vegdist, grouping=HSP90.4anosim$REGION, permutations = 2000)
summary(HSP90.ANOSIM.site) #R=0.06817, P=0.026987 
summary(HSP90.ANOSIM.region) #R=0.1117, P=0.0044978 
png("../../analyses/SRM/ANOSIM-HSP90%03d.png")
plot(HSP90.ANOSIM.site, main="HSP90, ANOSIM by Site")
plot(HSP90.ANOSIM.region, main="HSP90, ANOSIM by Region")
dev.off()
graphics.off()

#P=0.26637
Peroxiredoxin.vegdist <- vegdist(Peroxiredoxin.4anosim[,1:3], 'bray', na.rm=TRUE)
Peroxiredoxin.ANOSIM.site <-anosim(Peroxiredoxin.vegdist, grouping=Peroxiredoxin.4anosim$SITE, permutations = 2000)
Peroxiredoxin.ANOSIM.region <-anosim(Peroxiredoxin.vegdist, grouping=Peroxiredoxin.4anosim$REGION, permutations = 2000)
summary(Peroxiredoxin.ANOSIM.site)
summary(Peroxiredoxin.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Peroxiredoxin%03d.png")
plot(Peroxiredoxin.ANOSIM.site, main="Peroxiredoxin, ANOSIM by Site")
plot(Peroxiredoxin.ANOSIM.region, main="Peroxiredoxin, ANOSIM by Region")
# dev.off()
# graphics.off()

#P=0.017991
PDI.vegdist <- vegdist(PDI.4anosim[,1:6], 'bray', na.rm=TRUE)
PDI.ANOSIM.site <-anosim(PDI.vegdist, grouping=PDI.4anosim$SITE, permutations = 2000)
PDI.ANOSIM.region <-anosim(PDI.vegdist, grouping=PDI.4anosim$REGION, permutations = 2000)
summary(PDI.ANOSIM.site) #R=0.08905, P=0.018491 
summary(PDI.ANOSIM.region) #R=R: 0.1533, P=0.0014993 
png("../../analyses/SRM/ANOSIM-PDI%03d.png")
plot(PDI.ANOSIM.site, main="Protein Disulfide Isomerase, ANOSIM by Site")
plot(PDI.ANOSIM.region, main="Protein Disulfide Isomerase, ANOSIM by Region")
dev.off()
graphics.off()

#P=0.51874
Puromycin.vegdist <- vegdist(Puromycin.4anosim[,1:9], 'bray', na.rm=TRUE)
Puromycin.ANOSIM.site <-anosim(Puromycin.vegdist, grouping=Puromycin.4anosim$SITE, permutations = 2000)
Puromycin.ANOSIM.region <-anosim(Puromycin.vegdist, grouping=Puromycin.4anosim$REGION, permutations = 2000)
summary(Puromycin.ANOSIM.site)
summary(Puromycin.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Puromycin%03d.png")
plot(Puromycin.ANOSIM.site, main="Puromycin, ANOSIM by Site")
plot(Puromycin.ANOSIM.region, main="Puromycin, ANOSIM by Region")
# dev.off()
# graphics.off()

#P= 0.80460
Rab.11B.vegdist <- vegdist(Rab.11B.4anosim[,1:3], 'bray', na.rm=TRUE)
Rab.11B.ANOSIM.site <-anosim(Rab.11B.vegdist, grouping=Rab.11B.4anosim$SITE, permutations = 2000)
Rab.11B.ANOSIM.region <-anosim(Rab.11B.vegdist, grouping=Rab.11B.4anosim$REGION, permutations = 2000)
summary(Rab.11B.ANOSIM.site)
summary(Rab.11B.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Rab11B%03d.png")
plot(Rab.11B.ANOSIM.site, main="Ras-related Rab 11B, ANOSIM by Site")
plot(Rab.11B.ANOSIM.region, main="Ras-related Rab 11B, ANOSIM by Region")
# dev.off()
# graphics.off()

#P=0.53473
NAK.vegdist <- vegdist(NAK.4anosim[,1:6], 'bray', na.rm=TRUE)
NAK.ANOSIM.site <-anosim(NAK.vegdist, grouping=NAK.4anosim$SITE, permutations = 2000)
NAK.ANOSIM.region <-anosim(NAK.vegdist, grouping=NAK.4anosim$REGION, permutations = 2000)
summary(NAK.ANOSIM.site)
summary(NAK.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-NAK%03d.png")
plot(NAK.ANOSIM.site, main="Na/K Transporting ATPase, ANOSIM by Site")
plot(NAK.ANOSIM.region, main="Na/K Transporting ATPase, ANOSIM by Region")
# dev.off()
# graphics.off()

#P=0.31934
Superoxide.vegdist <- vegdist(Superoxide.4anosim[,1:2], 'bray', na.rm=TRUE)
Superoxide.ANOSIM.site <-anosim(Superoxide.vegdist, grouping=Superoxide.4anosim$SITE, permutations = 2000)
Superoxide.ANOSIM.region <-anosim(Superoxide.vegdist, grouping=Superoxide.4anosim$REGION, permutations = 2000)
summary(Superoxide.ANOSIM.site)
summary(Superoxide.ANOSIM.region)
# png("../../analyses/SRM/ANOSIM-Superoxide%03d.png")
plot(Superoxide.ANOSIM.site, main="Superoxide Dismutase, ANOSIM by Site")
plot(Superoxide.ANOSIM.region, main="Superoxide Dismutase, ANOSIM by Region")
# dev.off()
# graphics.off()

Trifunctional.vegdist <- vegdist(Trifunctional.4anosim[,1:9], 'bray', na.rm=TRUE)
Trifunctional.ANOSIM.site <-anosim(Trifunctional.vegdist, grouping=Trifunctional.4anosim$SITE, permutations = 2000)
Trifunctional.ANOSIM.region <-anosim(Trifunctional.vegdist, grouping=Trifunctional.4anosim$REGION, permutations = 2000)
summary(Trifunctional.ANOSIM.site) #R=0.05226, P=0.074463  
summary(Trifunctional.ANOSIM.region) #R=0.08946, P=0.011494 
# png("../../analyses/SRM/ANOSIM-Trifunctional%03d.png")
plot(Trifunctional.ANOSIM.site, main="Trifunctional enzyme, ANOSIM by Site")
plot(Trifunctional.ANOSIM.region, main="Trifunctional enzyme, ANOSIM by Region")
# dev.off()
# graphics.off()

########## ANOSIM on interesting proteins, where transitions have been summed. 
transumpro4stats <- dcast(data.melted.plus.prosum, ...~Protein.Name, value.var="Area")
# transumpep4stats <- dcast(data.melted.plus.pepsum, ...~Peptide.Sequence, value.var="Area")
View(transumpep4stats)
# See "other stats"


########## Box plots of proteins found to be significantly different between sites (ANOSIM)

library(ggplot2)
data.melted.plus$SITE <- factor(data.melted.plus$SITE, levels=c("WB", "CI", "PG", "FB"))
data.melted.plus$REGION <- factor(data.melted.plus$REGION, levels=c("South", "North"))

# HSP 90
png("../../analyses/SRM/boxplot-HSP90-site.png", width = 400, height = 500)
ggplot(subset(data.melted.plus, Pep.Trans %in% "GVVDSEDLPLNISR y7"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Heat shock 90 \nabundance by site") + 
  theme(plot.title = element_text(size=18), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() +
  annotate("text", label="ANOSIM Results: \nObserved R=0.08 \nExpected R=0.001 \nP=0.014493", x = 1, y = 4.5e+06, size = 4.5) 
dev.off()

# HSP 70
png("../../analyses/SRM/boxplot-HSP70-site.png", width = 400, height = 500)
ggplot(subset(data.melted.plus, Pep.Trans %in% "IINEPTAAALAYGLDK y12"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Heat shock 70 \nabundance by site") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() +
  annotate("text", label="ANOSIM Results: \nObserved R=0.156 \nExpected R=0 \nP=0.0009995", x = 1, y = 1.15e+07, size = 4.5) 
dev.off()

# PDI
png("../../analyses/SRM/boxplot-PDI-site.png", width = 400, height = 500)
ggplot(subset(data.melted.plus, Pep.Trans %in% "DNVVVIGFFK y5"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Protein Disulfide Isomerase \nabundance by site") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() +
  annotate("text", label="ANOSIM Results: \nObserved R=0.093 \nExpected R=0 \nP=0.011994", x = 1, y = 1650000, size = 4.5) 
dev.off()

########## Box plots where the transitions have been summed by protein 

data.melted.plus.prosum <- aggregate(Area ~ Protein.Name + SAMPLE + SITE + TREATMENT + BOTH + REGION, data.melted.plus, sum)
data.melted.plus.pepsum <- aggregate(Area ~ Peptide.Sequence + Protein.Name + SAMPLE + SITE + TREATMENT + BOTH + REGION, data.melted.plus, sum)


# HSP 90
png("../../analyses/SRM/boxplot-HSP90-site-trans-sums.png", width = 400, height = 500)
ggplot(subset(data.melted.plus.prosum, Protein.Name %in% "HSP90-alpha"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Heat shock 90 \nabundance by site, transition sums") + 
  theme(plot.title = element_text(size=18), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() +
  annotate("text", label="ANOSIM with Transitions, Results: \nObserved R=0.08 \nExpected R=0.001 \nP=0.01799", x = 1, y = 5e+07, size = 4.5) 
dev.off()

# HSP 70
png("../../analyses/SRM/boxplot-HSP70-site-trans-sums.png", width = 400, height = 500)
ggplot(subset(data.melted.plus.prosum, Protein.Name %in% "HSP70"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Heat shock 70 \nabundance by site, transition sums") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() +
  annotate("text", label="ANOSIM with Transitions, Results: \nObserved R=0.156 \nExpected R=0 \nP=0.000500", x = 1, y = 2.15e+07, size = 4.5) 
dev.off()

# PDI
png("../../analyses/SRM/boxplot-PDI-site-trans-sums.png", width = 400, height = 500)
ggplot(subset(data.melted.plus.prosum, Protein.Name %in% "PDI"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Protein Disulfide Isomerase \nabundance by site, transitions sums") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() +
  annotate("text", label="ANOSIM with Transitions, Results: \nObserved R=0.093 \nExpected R=0 \nP=0.0125", x = 1, y = 2.0E7, size = 4.5) 
dev.off()

# Peroxiredoxin-1
ggplot(subset(data.melted.plus.prosum, Protein.Name %in% "Peroxiredoxin-1"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Peroxiredoxin-1 \nabundance by site, transitions sums") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() #+
  #annotate("text", label="ANOSIM with Transitions, Results: \nObserved R=0.093 \nExpected R=0 \nP=0.0125", x = 1, y = 5.5E6, size = 4.5) 

# Trifuctional Enzyme Subunit
ggplot(subset(data.melted.plus.prosum, Protein.Name %in% "Trifunctional"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Trifunctional Enzyme Subunit-beta \nabundance by site, transitions sums") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() #+
  #annotate("text", label="ANOSIM with Transitions, Results: \nObserved R=0.093 \nExpected R=0 \nP=0.0125", x = 1, y = 2E6, size = 4.5) 

# Puromycin-sensitive
ggplot(subset(data.melted.plus.prosum, Protein.Name %in% "Puromycin-sensitive"), aes(x=SITE, y=Area, fill=SITE)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Puromycin-sensitive Aminopeptidase \nabundance by site, transitions sums") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) +
  scale_fill_discrete(labels=c("Willapa Bay", "Case Inlet", "Port Gamble", "Fidalgo Bay")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip() #+
  #annotate("text", label="ANOSIM with Transitions, Results: \nObserved R=0.093 \nExpected R=0 \nP=0.0125", x = 1, y = 9E6, size = 4.5) 

########## Box plots of proteins found to be significantly different between regions (ANOSIM)

# HSP 90
png("../../analyses/SRM/boxplot-HSP90-region.png", width = 400, height = 500)
ggplot(subset(data.melted.plus, Pep.Trans %in% "GVVDSEDLPLNISR y7"), aes(x=REGION, y=Area, fill=REGION)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Heat shock 90 \nabundance by region") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = c(0.82, .135), legend.title=element_blank(), legend.key.size = unit(3,"line"), legend.text=element_text(size=12)) + 
  scale_fill_discrete(labels=c("Southern Sites", "Northern Sites")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip()
dev.off()

# HSP 70
png("../../analyses/SRM/boxplot-HSP70-region.png", width = 400, height = 500)
ggplot(subset(data.melted.plus, Pep.Trans %in% "IINEPTAAALAYGLDK y12"), aes(x=REGION, y=Area, fill=REGION)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Heat shock 70 \nabundance by region") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = c(0.82, .135), legend.title=element_blank(), legend.key.size = unit(3,"line"), legend.text=element_text(size=12)) + 
  scale_fill_discrete(labels=c("Southern Sites", "Northern Sites")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip()
dev.off()

# PDI
png("../../analyses/SRM/boxplot-PDI-region.png", width = 400, height = 500)
ggplot(subset(data.melted.plus, Pep.Trans %in% "DNVVVIGFFK y5"), aes(x=REGION, y=Area, fill=REGION)) + 
  geom_boxplot(color="black", position = position_dodge()) + 
  ggtitle("Protein Disulfide Isomerase \nabundance by region") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = c(0.82, .135), legend.title=element_blank(), legend.key.size = unit(3,"line"), legend.text=element_text(size=12)) + 
  scale_fill_discrete(labels=c("Southern Sites", "Northern Sites")) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip()
dev.off()

### Scatter plots of all transitions by sample for proteins, coded by site

#HSP90
png("../../analyses/SRM/scatterbysample-HSP90.png", width = 800, height = 700)
ggplot(subset(data.melted.plus, Protein.Name %in% "HSP90-alpha"), aes(x=SAMPLE, y=Area, color=SITE, shape=Peptide.Sequence)) + 
  geom_point(position = position_dodge()) + 
  ggtitle("Heat shock 90 \nTransition abundances by sample") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position = c(0.95, .85), legend.title=element_blank(), legend.key.size = unit(2,"line"), legend.text=element_text(size=12)) + 
  ylab("Protein Abundance (Peak Intensity)") + 
  coord_flip()
dev.off()

#HSP70
png("../../analyses/SRM/scatterbysample-HSP70.png", width = 800, height = 700)
ggplot(subset(data.melted.plus, Protein.Name %in% "HSP70"), aes(x=SAMPLE, y=Area, color=SITE, shape=Peptide.Sequence)) + 
  geom_point(position = position_dodge()) + 
  ggtitle("Heat shock 70 \nTransition abundances by sample") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position = c(0.95, .85), legend.title=element_blank(), legend.key.size = unit(2,"line"), legend.text=element_text(size=12)) + 
  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip()
dev.off()

#PDI 
png("../../analyses/SRM/scatterbysample-PDI.png", width = 800, height = 700)
ggplot(subset(data.melted.plus, Protein.Name %in% "PDI"), aes(x=SAMPLE, y=Area, color=SITE, shape=Peptide.Sequence)) + 
  geom_point(position = position_dodge()) + 
  ggtitle("Protein Disulfide Isomerase \nTransition abundances by sample") + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position = c(0.95, .85), legend.title=element_blank(), legend.key.size = unit(2,"line"), legend.text=element_text(size=12)) +  ylab("Protein Abundance (Peak Intensity)") +
  coord_flip()
dev.off()
