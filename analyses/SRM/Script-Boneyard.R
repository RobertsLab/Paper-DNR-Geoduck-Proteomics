############# BONE YARD SCRIPT ############### 

# Principal Component Analysis
SRM.nmds.pca <- rda(SRM.data.t.noNA, scale = TRUE)
summary(SRM.nmds.pca)
plot(SRM.nmds.pca, scaling = 3)
dim(SRM.data.t.noNA)
biplot(SRM.nmds.pca, scaling = -1)
SRM.nmds.ca <- cca(SRM.data.t.noNA)
plot(SRM.nmds.ca)
#inertia is the sum of all variance in transitions; eigenvalues sum to total inertia, aka each eigenvalue "explains" a certain proportion of the total variance. Percent that each eigenvalue is responsible for total variance is: eigenvalue/total inertia. For example, PC1/total inertia = 83%


require(plotrix)
# Standard error for tech reps ###FYI THIS IS NOT WORKING !!!!!!!!!!!!
G001.err <- std.error(c(SRM.data.screened.noPRTC$`G001-A`, SRM.data.screened.noPRTC$`G001-B`))
G002.err <- std.error(c(SRM.data.screened.noPRTC$`G002-A`, SRM.data.screened.noPRTC$`G002-B`, SRM.data.screened.noPRTC$`G002-C`))
G003.err <- std.error(c(SRM.data.screened.noPRTC$`G003-A`, SRM.data.screened.noPRTC$`G003-B`)) #C removed
G007.err <- std.error(c(SRM.data.screened.noPRTC$`G007-A`, SRM.data.screened.noPRTC$`G007-B`))
G008.err <- std.error(c(SRM.data.screened.noPRTC$`G008-A`, SRM.data.screened.noPRTC$`G008-B`))
G009.err <- std.error(c(SRM.data.screened.noPRTC$`G009-A`, SRM.data.screened.noPRTC$`G009-B`))
G110.err <- std.error(c(SRM.data.screened.noPRTC$`G110-A`, SRM.data.screened.noPRTC$`G110-B`))
G012.err <- std.error(c(SRM.data.screened.noPRTC$`G012-A`, SRM.data.screened.noPRTC$`G012-B`, SRM.data.screened.noPRTC$`G012-C`))
G013.err <- std.error(c(SRM.data.screened.noPRTC$'G013-A', SRM.data.screened.noPRTC$'G013-C'))
G014.err <- std.error(c(SRM.data.screened.noPRTC$`G014-A`, SRM.data.screened.noPRTC$`G014-B`))
G015.err <- std.error(c(SRM.data.screened.noPRTC$`G015-A`, SRM.data.screened.noPRTC$`G015-B`))
G016.err <- std.error(c(SRM.data.screened.noPRTC$`G016-A`, SRM.data.screened.noPRTC$`G016-B`, SRM.data.screened.noPRTC$`G016-C`))
G017.err <- std.error(c(SRM.data.screened.noPRTC$`G017-A`, SRM.data.screened.noPRTC$`G017-B`))
G031.err <- std.error(c(SRM.data.screened.noPRTC$`G031-A`, SRM.data.screened.noPRTC$`G031-B`, SRM.data.screened.noPRTC$`G031-C`))
G032.err <- std.error(c(SRM.data.screened.noPRTC$`G032-A`, SRM.data.screened.noPRTC$`G032-B`))
G040.err <- std.error(c(SRM.data.screened.noPRTC$`G040-A`, SRM.data.screened.noPRTC$`G040-B`))
G041.err <- std.error(c(SRM.data.screened.noPRTC$`G041-A`, SRM.data.screened.noPRTC$`G041-B`))
G042.err <- std.error(c(SRM.data.screened.noPRTC$`G042-A`, SRM.data.screened.noPRTC$`G042-B`)) #C removed
G043.err <- std.error(c(SRM.data.screened.noPRTC$`G043-A`, SRM.data.screened.noPRTC$`G043-B`))
G045.err <- std.error(c(SRM.data.screened.noPRTC$`G045-A`, SRM.data.screened.noPRTC$`G045-B`))
G047.err <- std.error(c(SRM.data.screened.noPRTC$`G047-A`, SRM.data.screened.noPRTC$`G047-B`))
G049.err <- std.error(c(SRM.data.screened.noPRTC$`G049-A`, SRM.data.screened.noPRTC$`G049-B`))
G053.err <- std.error(c(SRM.data.screened.noPRTC$`G053-A`, SRM.data.screened.noPRTC$`G053-remake-C`, SRM.data.screened.noPRTC$`G053-remake-D`)) #B removed 
G054.err <- std.error(c(SRM.data.screened.noPRTC$`G054-A`, SRM.data.screened.noPRTC$`G054-B`))
G055.err <- std.error(c(SRM.data.screened.noPRTC$`G055-A`, SRM.data.screened.noPRTC$`G055-B`, SRM.data.screened.noPRTC$`G055-C`))
G057.err <- std.error(c(SRM.data.screened.noPRTC$`G057-A`, SRM.data.screened.noPRTC$`G057-C`)) #B removed
G060.err <- std.error(c(SRM.data.screened.noPRTC$`G060-A`, SRM.data.screened.noPRTC$`G060-B`))
G062.err <- std.error(c(SRM.data.screened.noPRTC$`G062-B`, SRM.data.screened.noPRTC$`G062-C`))
G064.err <- std.error(c(SRM.data.screened.noPRTC$`G064-A`, SRM.data.screened.noPRTC$`G064-B`))
G066.err <- std.error(c(SRM.data.screened.noPRTC$`G066-A`, SRM.data.screened.noPRTC$`G066-B`))
G070.err <- std.error(c(SRM.data.screened.noPRTC$`G070-A`, SRM.data.screened.noPRTC$`G070-B`, SRM.data.screened.noPRTC$`G070-C`))
G071.A.err <- std.error(cor(SRM.data.screened.noPRTC$`G071-A-A`, SRM.data.screened.noPRTC$`G071-A-B`))
G071.B.err <- std.error(cor(SRM.data.screened.noPRTC$`G071-B-A`, SRM.data.screened.noPRTC$`G071-B-B`))
G073.err <- std.error(c(SRM.data.screened.noPRTC$`G073-A`, SRM.data.screened.noPRTC$`G073-B`, SRM.data.numeric$`G073-C`))
G074.err <- std.error(c(SRM.data.screened.noPRTC$`G074-A`, SRM.data.screened.noPRTC$`G074-B`))
G079.err <- std.error(c(SRM.data.screened.noPRTC$`G079-A`, SRM.data.screened.noPRTC$`G079-B`))
G081.err <- std.error(c(SRM.data.screened.noPRTC$`G081-A`, SRM.data.screened.noPRTC$`G081-B`))
G104.err <- std.error(c(SRM.data.screened.noPRTC$`G104-A`, SRM.data.screened.noPRTC$`G104-remake-C`, SRM.data.screened.noPRTC$`G104-remake-D`)) #B removed
G105.err <- std.error(c(SRM.data.screened.noPRTC$`G105-A`, SRM.data.screened.noPRTC$`G105-B`))
G109.err <- std.error(c(SRM.data.screened.noPRTC$`G109-A`, SRM.data.screened.noPRTC$`G109-C`))
G114.err <- std.error(c(SRM.data.screened.noPRTC$`G114-A`, SRM.data.screened.noPRTC$`G114-B`, SRM.data.screened.noPRTC$`G114-remake-C`, SRM.data.screened.noPRTC$`G114-remake-D`))
G116.err <- std.error(c(SRM.data.screened.noPRTC$`G116-A`, SRM.data.screened.noPRTC$`G116-B`))
G120.err <- std.error(c(SRM.data.screened.noPRTC$`G120-A`, SRM.data.screened.noPRTC$`G120-B`))
G122.err <- std.error(c(SRM.data.screened.noPRTC$`G122-A`, SRM.data.screened.noPRTC$`G122-B`))
G127.err <- std.error(c(SRM.data.screened.noPRTC$`G127-A`, SRM.data.screened.noPRTC$`G127-C`)) #B removed
G128.err <- std.error(c(SRM.data.screened.noPRTC$`G128-A`, SRM.data.screened.noPRTC$`G128-C`,SRM.data.screened.noPRTC$`G128-D`))
G129.err <- std.error(c(SRM.data.screened.noPRTC$`G129-A`, SRM.data.screened.noPRTC$`G129-B`))
G132.err <- std.error(c(SRM.data.screened.noPRTC$`G132-A`, SRM.data.screened.noPRTC$`G132-C`, SRM.data.screened.noPRTC$`G132-D`))

SRM.data.stderr <- cbind.data.frame(rownames(SRM.data.screened.noPRTC), G001.err,G002.err,G003.err,G007.err,G008.err,G009.err,G110.err,G012.err,G013.err,G014.err,G015.err,G016.err,G017.err,G031.err,G032.err,G040.err,G041.err,G042.err,G043.err,G045.err,G047.err,G049.err,G053.err,G054.err,G055.err,G057.err,G060.err,G062.err,G064.err,G066.err,G070.err,G071.A.err,G071.B.err,G073.err, G074.err,G079.err,G081.err,G104.err,G105.err,G109.err,G114.err,G116.err,G120.err,G122.err,G127.err,G128.err,G129.err,G132.err)


# Eigenvectors 
eigen.ml <- envfit(SRM.mean.log.nmds$points, SRM.data.mean.t.log, perm=1000)
eigen.ml


# PLOT TOTAL ABUNDANCE BY SITE WITH ERROR BARS

####### check out total abundance & other stats by site
TotAbund.SITE <- do.call(data.frame, aggregate(Area ~ SITE, data.melted, function(x) c(sum=sum(x), sd=sd(x), range=range(x), min=min(x), max=max(x))))

#Bar plot showing total abundance by site, with error bars
library(ggplot2)
TotSum.bar <- ggplot(TotAbund.SITE, aes(x=SITE, y=Area.sum, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Total Abundance by Site") +
  geom_errorbar(aes(ymin=Area.sum-Area.sd, ymax=Area.sum+Area.sd), width=.2, position=position_dodge(.9)) 
ggplot(data.melted, aes(SITE, Area)) + geom_boxplot(aes(colour=SITE)) + ggtitle("Abundance Distribution by Site")
ggplot(data.melted, aes(SITE, Area)) + geom_violin(aes(colour=SITE)) + ggtitle("Abundance Distribution by Site")
ggplot(data.melted, aes(SITE, Area)) + geom_boxplot(aes(colour=SAMPLE)) + ggtitle("Abundance Distribution in each sample, grouped by site")
ggplot(data.melted, aes(SITE, Area)) + geom_violin(aes(colour=SAMPLE)) + ggtitle("Abundance Distribution in each sample, grouped by site")
ggplot(data.melted, aes(SITE, Area)) + geom_boxplot(aes(colour=TREATMENT)) + ggtitle("Abundance Distribution in each treatment, grouped by site")
ggplot(data.melted, aes(SITE, Area)) + geom_violin(aes(colour=TREATMENT)) + ggtitle("Abundance Distribution in each treatment, grouped by site")

#Log+1 transform summary data for plotting purposes
data.melted.log <- data.melted
data.melted.log$Area <- log(data.melted.log$Area+1)
TotAbund.SITE.log <- do.call(data.frame, aggregate(Area ~ SITE, data.melted.log, function(x) c(sum=sum(x), sd=sd(x), range=range(x), min=min(x), max=max(x))))
TotSum.log.bar <- ggplot(TotAbund.SITE.log, aes(x=SITE, y=Area.sum, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Total Abundance by Site, log+1 transformed") +
  geom_errorbar(aes(ymin=Area.sum-Area.sd, ymax=Area.sum+Area.sd), width=.2, position=position_dodge(.9)) 
area.log.box.site <- ggplot(data.melted.log, aes(SITE, Area)) + geom_boxplot(aes(colour=SITE)) + ggtitle("Abundance Distribution in each site, log+1 transformed")
area.log.viol.site <- ggplot(data.melted.log, aes(SITE, Area)) + geom_violin(aes(colour=SITE)) + ggtitle("Abundance Distribution in each site, log+1 transformed")
area.log.box.smpl <- ggplot(data.melted.log, aes(SITE, Area)) + geom_boxplot(aes(fill=TREATMENT, colour=SAMPLE)) + ggtitle("Abundance Distribution in each sample log+1 transformed, by site") + guides(colour=FALSE)
area.log.viol.smpl <- ggplot(data.melted.log, aes(SITE, Area)) + geom_violin(aes(colour=SAMPLE)) + ggtitle("Abundance Distribution in each sample log+1 transformed, by site")
area.log.box.trmt <- ggplot(data.melted.log, aes(SITE, Area)) + geom_boxplot(aes(colour=TREATMENT)) + ggtitle("Abundance Distribution in each treatment log+1 transformed, by site")
area.log.viol.trmt <- ggplot(data.melted.log, aes(SITE, Area)) + geom_violin(aes(colour=TREATMENT)) + ggtitle("Abundance Distribution in each treatment log+1 transformed, by site")

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-All-Sample-plot%03d.png")
TotSum.bar
area.log.box.site
area.log.box.smpl
area.log.box.trmt
dev.off()
#######

# Write program with summary statistics http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
data_summary <- function(data, varname, groupnames){
  require(plyr)
  require(plotrix)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      st.err = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
SRM.stats <-  data_summary(data.melted.plus, varname="Area", groupnames=c("SITE", "Protein.Name", "Peptide.Sequence", "Pep.Trans")) #run program with my data
write.csv(SRM.stats, file="Analyses/2017-September_SRM-results/2017-09-04_SRM-data-notNORM-summary-stats.csv")

#### PLOT SITE MEANS FOR EACH PROTEIN, BROKEN INTO PROTEIN, PEPTIDE & TRANSITION
SRM.stats4plots <- SRM.stats[grepl(c("APGLPAQIK y5|AGELGGSDPDYAMR y6|QSLLPFGATGPR y8|APNSFNLR y5|IINEPTAAALAYGLDK y12|GVVDSEDLPLNISR y6|ALFIIDDK y4|DNVVVIGFFK y7|SIQQSVENIR y6|STIGVEFATR y7|TVIEPMAGDGLR y8|ISLTGPHSIIGR y8|FNLWGGSLSLGHPFGATGVR y8"), SRM.stats$Pep.Trans),]
levels(SRM.stats4plots$SITE)
SRM.stats4plots$SITE <- factor(SRM.stats4plots$SITE, levels=c("PG", "FB", "CI", "WB"))

library(ggplot2)

# Arachidonate 5-lipoxygenase
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-ArachidonatePro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Arachidonate 5-lipoxygenase"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Arachidonate 5-lipoxygenase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-ArachidonatePep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Arachidonate 5-lipoxygenase"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Arachidonate 5-lipoxygenase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-ArachidonateTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Arachidonate 5-lipoxygenase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Arachidonate 5-lipoxygenase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Arachidonate.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% "Arachidonate 5-lipoxygenase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Arachidonate 5-lipoxygenase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Catalase
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-CatalasePro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Catalase"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Catalase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-CatalasePep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Catalase"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Catalase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-CatalaseTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Catalase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Catalase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Catalase.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% "Catalase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Catalase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Cytochrome P450
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-CytochromePro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Cytochrome P450"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Cytochrome P450") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-CytochromePep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Cytochrome P450"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Cytochrome P450") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-CytochromeTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Cytochrome P450"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Cytochrome P450") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Cytochrome.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% "Cytochrome P450"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Cytochrome P450") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Glycogen phosphorylase, muscle form
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-GlycogenPro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Glycogen phosphorylase, muscle form"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Glycogen phosphorylase, muscle form") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-GlycogenPep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Glycogen phosphorylase, muscle form"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Glycogen phosphorylase, muscle form") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-GlycogenTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Glycogen phosphorylase, muscle form"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Glycogen phosphorylase, muscle form") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Glycogen.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% "Glycogen phosphorylase, muscle form"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Glycogen phosphorylase, muscle form") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Heat shock 70 kDa protein
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP70Pro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Heat shock 70 kDa protein"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 70") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP70Pep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Heat shock 70 kDa protein"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 70") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP70Tran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Heat shock 70 kDa protein"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 70") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP70.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[313,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 70, mean abundance by site") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + 
  theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.title=element_text(size=25), legend.text=element_text(size=30), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = "bottom") + 
  guides(fill=guide_legend(
    keywidth=0.3,
    keyheight=0.5,
    default.unit="inch"))

# Heat shock protein HSP 90-alpha
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP90Pro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Heat shock protein HSP 90-alpha"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 90") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP90Pep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Heat shock protein HSP 90-alpha"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 90") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP90Tran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Heat shock protein HSP 90-alpha"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 90") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-HSP90.png", width = 400, height = 600)

ggplot(subset(SRM.stats4plots, Protein.Name %in% "Heat shock protein HSP 90-alpha"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Heat shock 90") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Peroxiredoxin-1  

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PeroxiredoxinPro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Peroxiredoxin-1"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Peroxiredoxin-1") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PeroxiredoxinPep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Peroxiredoxin-1"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Peroxiredoxin-1") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PeroxiredoxinTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Peroxiredoxin-1"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Peroxiredoxin-1") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Peroxiredoxin.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[328,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Peroxiredoxin-1") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Protein disulfide-isomerase (PDI)

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PDIPro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Protein disulfide-isomerase (PDI)"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Protein disulfide-isomerase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PDIPep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Protein disulfide-isomerase (PDI)"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Protein disulfide-isomerase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PDITran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Protein disulfide-isomerase (PDI)"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Protein disulfide-isomerase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PDI.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[331,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Protein disulfide-isomerase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Puromycin-sensitive aminopeptidase 

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PuromycinPro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Puromycin-sensitive aminopeptidase"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Puromycin-sensitive aminopeptidase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PuromycinPep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Puromycin-sensitive aminopeptidase"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Puromycin-sensitive aminopeptidase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-PuromycinTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Puromycin-sensitive aminopeptidase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Puromycin-sensitive aminopeptidase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Puromycin.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[337,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Puromycin-sensitive aminopeptidase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Ras-related protein Rab-11B 

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-RasrelatedRabPro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Ras-related protein Rab-11B"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Ras-related protein Rab-11B") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-RasrelatedRabPep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Ras-related protein Rab-11B"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Ras-related protein Rab-11B") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-RasrelatedRabTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Ras-related protein Rab-11B"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Ras-related protein Rab-11B") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-RasrelatedRab.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[346,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Ras-related protein Rab-11B") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Sodium/potassium-transporting ATPase subunit alpha-4  

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-NAKtransprotingPro.png")
ggplot(subset(SRM.stats, Protein.Name %in% SRM.stats[257,2]), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Na/K-transporting ATPase subunit alpha-4") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-NAKtransprotingPep.png")
ggplot(subset(SRM.stats, Protein.Name %in% SRM.stats[257,2]), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Na/K-transporting ATPase subunit alpha-4") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-NAKtransprotingTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% SRM.stats[257,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Na/K-transporting ATPase subunit alpha-4") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-NAKtransproting.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[257,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Na/K-transporting ATPase subunit alpha-4") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Superoxide dismutase

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-SuperoxidePro.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Superoxide dismutase"), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Superoxide dismutase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-SuperoxidePep.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Superoxide dismutase"), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Superoxide dismutase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-SuperoxideTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% "Superoxide dismutase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Superoxide dismutase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-Superoxide.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% "Superoxide dismutase"), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Superoxide dismutase") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

#++++++++++++++++++++++

# Trifunctional enzyme subunit beta, mitochondrial

png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-TrifunctEnzymePro.png")
ggplot(subset(SRM.stats, Protein.Name %in% SRM.stats[360,2]), aes(x=Protein.Name, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Trifunctional enzyme subunit beta, mitochondrial") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-TrifunctEnzymePep.png")
ggplot(subset(SRM.stats, Protein.Name %in% SRM.stats[360,2]), aes(x=Peptide.Sequence, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Trifunctional enzyme subunit beta, mitochondrial") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-TrifunctEnzymeTran.png")
ggplot(subset(SRM.stats, Protein.Name %in% SRM.stats[360,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Trifunctional enzyme subunit beta, mitochondrial") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9))
dev.off()
png("Analyses/2017-September_SRM-results/2017-09-04_NotNORM-plot-TrifunctEnzyme.png", width = 400, height = 600)
ggplot(subset(SRM.stats4plots, Protein.Name %in% SRM.stats[360,2]), aes(x=Pep.Trans, y=Area, fill=SITE)) + 
  geom_bar(stat="identity", color="black", position = position_dodge()) + ggtitle("Trifunctional enzyme subunit beta, mitochondrial") +
  geom_errorbar(aes(ymin=Area-st.err, ymax=Area+st.err), width=.2, position=position_dodge(.9)) + theme(plot.title = element_text(size=22), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()
