# Script to ID proteins found in DIA data

## IMPORTANT: the first step in this script creates a directory, where all .raw files will be downloaded. Feel free to change the location of this directory

setwd("../../data/DIA") #set your working directory to the one you just created
# Download annotated geoduck gonad transcriptome from GitHub
annotations <- data.frame(read.csv(url("https://github.com/sr320/paper-pano-go/raw/master/data-results/Geo-v3-join-uniprot-all0916-condensed.txt"), header = T, sep = "\t", fill = TRUE, stringsAsFactors = F))
annotations$GeoID <- as.character(annotations$GeoID)

# Download DIA Protein abundance report from Owl
DIA <- read.csv(url("http://owl.fish.washington.edu/generosa/Generosa_DNR/DIA-Report-long.csv"), header = T, na.strings = "#N/A", stringsAsFactors = F)

# Import Total Ion Current (TIC) data, which was provided by Emma from Lumos
TIC <- read.csv("../../data/DIA/DIA-TIC.csv")
TIC$File <- sub("geoduck ", "", TIC$File)
TIC$File <- as.numeric(TIC$File)

# Create new column in DIA data with simplified protein ID to merge with annotations
DIA$GeoID <- DIA$Protein.Name #make new column 
DIA$GeoID <- sub("\\|.*", "", DIA$GeoID) 
DIA$GeoID <- sub("^cds.", "", DIA$GeoID)
# DIA$GeoID <- sub("\\..", "", DIA$GeoID)
# DIA$GeoID <- as.character(DIA$GeoID)

# Merge the annotations with the DIA results
DIA.annotated <-merge(x=DIA, y=annotations, by.x="GeoID", by.y="GeoID", all.x=T) 
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
write.csv(file="DIA-annotated.csv", DIA.annotated)



