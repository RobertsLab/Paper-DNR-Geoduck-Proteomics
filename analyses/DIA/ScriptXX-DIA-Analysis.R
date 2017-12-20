# Script to analyze DIA Protein Abundance Data

## IMPORTANT: the first step in this script creates a directory, where all .raw files will be downloaded. Feel free to change the location of this directory

setwd("../../data/DIA") #set your working directory to the one you just created
# Download annotated geoduck gonad transcriptome from GitHub
annotations <- data.frame(read.csv(url("https://github.com/sr320/paper-pano-go/raw/master/data-results/Geo-v3-join-uniprot-all0916-condensed.txt"), header = T, sep = "\t", fill = TRUE, stringsAsFactors = F))
annotations$GeoID <- as.character(annotations$GeoID)

# Download DIA Protein abundance report from Owl
#DIA <- read.csv(url("http://owl.fish.washington.edu/generosa/Generosa_DNR/DIA-Report-long.csv"), header = T, na.strings = "#N/A", stringsAsFactors = F)
# DIA <- read.csv("DIA-Report-long.csv", header = T, na.strings = "#N/A", stringsAsFactors = F)

# Import Total Ion Current (TIC) data, which was provided by Emma from Lumos
TIC <- read.csv("../../data/DIA/DIA-TIC.csv")
TIC$File <- sub("geoduck ", "", TIC$File)
TIC <- TIC[1:20,]
TIC$File <- as.numeric(TIC$File)

# Create new column in DIA data with simplified protein ID to merge with annotations
DIA$GeoID <- DIA$Protein.Name #make new column 
DIA$GeoID <- sub("\\|.*", "", DIA$GeoID) 
DIA$GeoID <- sub("^cds.", "", DIA$GeoID)
# DIA$GeoID <- sub("\\..", "", DIA$GeoID)
# DIA$GeoID <- as.character(DIA$GeoID)

# Merge the annotations with the DIA results
DIA.annotated <-merge(x=DIA, y=annotations, by.x="GeoID", by.y="GeoID", all.x=T, all.y=F) 
# Edited the DIA file name to only include the number
DIA.annotated$Replicate <- sub("envtstress_geoduck", "", DIA.annotated$Replicate) #remove extraneous info from rep name
DIA.annotated$Replicate <- sub("\\_.*", "", DIA.annotated$Replicate) #remove extraneous # from the rep 7 name
DIA.annotated$Replicate <- as.numeric(DIA.annotated$Replicate) 

# Download MS/MS sequence file to an R object, which identifies .raw data files' corresponding vial contents
DIAsequence <- read.csv(url("https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/raw/master/data/DIA/2017_January_23_sequence_file.csv"), header=FALSE, stringsAsFactors=FALSE) 

# Extract geoduck-related file names & associated sample names from sequence file, merge with TIC data,
GeoFiles <- subset(DIAsequence[,1:2], grepl(c("geoduck"), DIAsequence[,2]))
names(GeoFiles) <- c("File", "Sample")
GeoFiles$File <- sub(".*geoduck", "", GeoFiles$File)
GeoFiles$Sample <- sub("geoduck", "", GeoFiles$Sample)
GeoFiles$Sample <- as.numeric(GeoFiles$Sample)
GeoFiles <- merge(x=GeoFiles, y=TIC, by.x="File", by.y="File", all.x=T)

# Download geoduck sample code info 
SampleInfo <- read.csv(url("https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/raw/master/data/SRM/2017-08-14-Geoduck-samples.csv"), header=T, stringsAsFactors=FALSE)
SampleInfo$PRVial <- sub("G", "", SampleInfo$PRVial)
SampleInfo$PRVial <- as.numeric(SampleInfo$PRVial)

# Merge GeoFiles and SampleInfo togeter to annotate file #'s
GeoFile.ann <- merge(x=GeoFiles, y=SampleInfo, by.x="Sample", by.y="PRVial", all.x=T)
GeoFile.ann <- GeoFile.ann[,1:10]

# Merge sample annotations with the DIA.annotated file
DIA.annotated <- merge(x=DIA.annotated, y=GeoFile.ann, by.x="Replicate", by.y="File", all.x=T)
DIA.annotated$Area.adj <- DIA.annotated$Area/DIA.annotated$TIC
write.csv(file="DIA-annotated.csv", DIA.annotated)

# Write out Uniprot ID's for 1) Total transcriptome, and 2) ID'd proteins
write.csv(file="DIA-annotated-UniprotID.csv", unique(DIA.annotated[which(!is.na(DIA.annotated$SPID)),"SPID"])) # figure out how to write out removing NA's  - this isn't working yet
write.csv(file="Geoannotations-UniprotID.csv", annotations$SPID)

# Generate new dataframe that has transitions summed for each peptide: 
DIA4vegan <- dcast(na.omit(DIA.annotated), Sample+Replicate+Site+Patch~Peptide.Sequence, value.var="Area.adj", sum)
rownames(DIA4vegan) <- paste("G", DIA4vegan[,2], sep="")
DIA4vegan[,2] <- paste(DIA4vegan[,3], DIA4vegan[,4], sep="")
colnames(DIA4vegan)[2] <- "BOTH"
View(DIA4vegan[1:20])
DIA4vegan <- DIA4vegan[order(rownames(DIA4vegan)),]

library(vegan)
# Run ANOSIM 
DIA.vegdist <- vegdist(DIA4vegan[,c(-1:-4)], method='bray', na.rm = TRUE)
DIA.ANOSIM <- anosim(DIA.vegdist, DIA4vegan$BOTH, permutations=2000)
plot(DIA.ANOSIM)

# Generate NMDS
DIA4vegan.log <- log(DIA4vegan[,c(-1:-4)] + 1) #First Log+1 transform
DIA.NMDS <- metaMDS(DIA4vegan.log, distance = "bray", k=2, trymax = 3000, autotransform = FALSE)
stressplot(DIA.NMDS)
plot(DIA.NMDS)
DIA.NMDS.samples <- scores(DIA.NMDS, display="sites")
plot(DIA.NMDS.samples) #plot initially to get aspect ratio
library(plotly)
DIA.p <- plot_ly(data=as.data.frame(DIA.NMDS.samples), x=~NMDS1, y=~NMDS2, type="scatter", mode="text", text=~DIA4vegan$Sample, hoverinfo=~rownames(DIA.NMDS.samples), color = ~DIA4vegan$Site) %>%
  layout(title="NMDS of DIA data, all tech and bio reps",
         xaxis=list(title="NMDS Axis 1"),
         yaxis=list(title="NMDS Axis 2"))

DIA.p2 <- plot_ly(data=as.data.frame(DIA.NMDS.samples), x=~NMDS1, y=~NMDS2, type="scatter", mode="text", text=~rownames(DIA.NMDS.samples), hoverinfo=~DIA4vegan$Sample, color = ~DIA4vegan$Site) %>%
  layout(title="NMDS of DIA data, all tech and bio reps",
         xaxis=list(title="NMDS Axis 1"),
         yaxis=list(title="NMDS Axis 2"))

plot.default(x=NULL, y=NULL, type-"n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-0.5, 1.5), ylim=c(-0.7, 0.4), main="NMDS of DIA data, all tech and bio reps")
text(DIA.NMDS.samples, labels=rownames(DIA.NMDS.samples))

# Remove the following reps: G12, G15, G17, G18, G20 then mean the other tech reps
DIA.noOuts <- DIA.annotated[!grepl("12|15|17|18|20", DIA.annotated$Replicate),]
# Mean remaining tech reps across samples 
DIA.noOuts.mean <- aggregate(Area.adj~Sample+., na.omit(DIA.noOuts[,c(3,5,8,9,10,14,19,21,23)]), mean)
# Sum all transitions across proteins for each sample
DIA.noOuts.mean.protSum <- aggregate(Area.adj~Protein.Name+., DIA.noOuts.mean[,-3], sum)
#Create new column to assign Region, CI & WB = South, FB & PG = North
DIA.noOuts.mean.protSum$Region <- DIA.noOuts.mean.protSum$Site
DIA.noOuts.mean.protSum$Region <- gsub("CI|WB", "South", DIA.noOuts.mean.protSum$Region)
DIA.noOuts.mean.protSum$Region <- gsub("FB|PG", "North", DIA.noOuts.mean.protSum$Region)
# Remove SK data from dataframe
DIA.noOuts.mean.protSum <- DIA.noOuts.mean.protSum[!grepl("SK", DIA.noOuts.mean.protSum$Region),]
# Average abundance for each protein by region. Result is 2 abundance data points per protein for North vs. South, and include SD & Cv
DIA.mean.region <- aggregate(Area.adj~Region+., data=DIA.noOuts.mean.protSum[,c(-2,-6,-7)], mean)
DIA.mean.region.sd <- aggregate(Area.adj~Region+., data=DIA.noOuts.mean.protSum[,c(-2,-6,-7)], sd)
DIA.mean.region$sd <- DIA.mean.region.sd$Area.adj
DIA.mean.region$cv <- DIA.mean.region$sd/DIA.mean.region$Area.adj
colnames(DIA.mean.region)[6:8] <- c("Area.mean", "SD.mean", "CV.mean")
hist(log(DIA.mean.region$Area.mean)) #check out mean data (log transformed) for normality
hist(DIA.mean.region$CV.mean) #Check out how the CV looks across all proteins
summary(DIA.mean.region$CV.mean) 

# Create a dataframe with mean, SD and CV abundances for North and South in separate columns to calculate Fold Change
DIA.mean.north <- DIA.mean.region[grepl("North", DIA.mean.region$Region),]
colnames(DIA.mean.north)[6:8] <- c("Area.mean.N", "SD.mean.N", "CV.mean.N")
DIA.mean.south <- DIA.mean.region[grepl("South", DIA.mean.region$Region),]
colnames(DIA.mean.south)[6:8] <- c("Area.mean.S", "SD.mean.S", "CV.mean.S")
DIA.mean.region.wide <- merge(x=DIA.mean.north[,-1], y=DIA.mean.south[,c("Area.mean.S", "SD.mean.S", "CV.mean.S","Protein.Name")], by.x="Protein.Name", by.y="Protein.Name")

#Calculate Fold Change,  N/S
DIA.mean.region.wide$N.over.S <- DIA.mean.region.wide$Area.mean.N/DIA.mean.region.wide$Area.mean.S
#Calculate Fold Change,  S/N
DIA.mean.region.wide$S.over.N <- DIA.mean.region.wide$Area.mean.S/DIA.mean.region.wide$Area.mean.N

# Extract proteins with fold change >2 
DIA.FC2 <- DIA.mean.region.wide[(DIA.mean.region.wide[,"N.over.S"]>2|DIA.mean.region.wide[,"S.over.N"]>2),]

# Scrub Fold Change >2 data of any proteins that had a >100% CV in either region (remember, N=4 for each region)
DIA.FC2.CV100 <- DIA.FC2[(DIA.FC2[,"CV.mean.N"]<1&DIA.FC2[,"CV.mean.S"]<1),]
write.csv(file="DIA.fold-change2-CV100.csv", DIA.FC2.CV100) #Save this dataframe; I can check out the diff. expressed GO terms

# Isolate SRM target proteins from full list
SRM.Targets <- read.csv("../SRM-Transitions.csv")
SRM.Target.proteins <- unique(SRM.Targets$Heat.shock.protein.HSP.90.alpha.cds.comp132209_c0_seq1.m.20047)
SRM.Target.proteins <- gsub(".*lipoxygenase |.*PDI\\) |.*aminopeptidase |.*alpha-4  |.*Rab-11B |.*mitochondrial  |Peroxiredoxin-1 |Catalase |.*P450 |.*muscle form |.*dismutase |.*kDa protein |.*90-alpha ", "", unique(SRM.Targets$Heat.shock.protein.HSP.90.alpha.cds.comp132209_c0_seq1.m.20047))
DIA.SRM.targets <- subset(DIA.annotated, Protein.Name %in% SRM.Target.proteins)
DIA.SRM.target.proteins <- DIA.SRM.targets[!duplicated(DIA.SRM.targets[,"GeoID"]),]
write.csv(file="DIA-annotations_SRM-targets.csv", DIA.SRM.target.proteins)
