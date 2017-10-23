# Script to download .raw files corresponding to geoduck tissue sample SRM data from Vantage MS/MS

## IMPORTANT: the first step in this script creates a directory, where all .raw files will be downloaded. Feel free to change the location of this directory
getwd() # see which directory is currently your working directory; change wd if desired
dir.create("/2017-Geoduck-DIA-raw/") 
setwd("/2017-Geoduck-DIA-raw/")

# Install package not available in base R
install.packages("stringr", dep=TRUE) #only install stringr package if you don't already have it
install.packages("curl", dep=TRUE)

# Scrape website that houses all .raw files of file names
url <- "http://owl.fish.washington.edu/generosa/Generosa_DNR/Lumos_Raw/" #site on Owl that contains all .raw files
html <- paste(readLines(url), collapse="\n")
library(stringr)
rawfiles <- as.data.frame(str_match_all(html, "<a href=\"(.*?)\""), stringsAsFactors = FALSE) #document with names of links/files on site
rawfiles.1 <- as.data.frame(subset(rawfiles[,2], grepl(".raw", rawfiles[,2])), stringsAsFactors = FALSE) #only .raw file names
rawfiles.2 <- apply(rawfiles.1, 2, function(y) gsub(".raw", "", y)) #remove .raw from file names for subsetting purposes

# Download MS/MS sequence file to an R object, which identifies .raw data files' corresponding vial contents
sequence <- read.csv(url("https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/raw/master/data/DIA/2017_January_23_sequence_file.csv"), header=FALSE, stringsAsFactors=FALSE) 

# If you're downloaded repo, can use the following: 
# sequence <- read.csv(url("../../data/DIA/2017_January_23_sequence_file.csv"), header=FALSE, stringsAsFactors=FALSE) 

# Merge the sequence file info with the names of files on Owl
Merged <- merge(x=sequence, y=rawfiles.2, by.x=1, by.y=1, all.x=FALSE, all.y=TRUE)

# Extract geoduck-related file names
GeoFiles <- subset(Merged[,1:2], grepl("g", Merged[,1]))

# Create a database with a column of url's for each .raw data file 
GeoURLS <-  as.data.frame(paste0("http://owl.fish.washington.edu/generosa/Generosa_DNR/Lumos_Raw/", GeoFiles[,1], ".raw"), stringsAsFactors = FALSE)
nrow(GeoURLS) # number of .raw files you will download

# Download all url's in your GeoURL database - #this could take a while depending on your computer
library(curl)
for(i in 1:nrow(GeoURLS)) {
  curl_download(GeoURLS[i,1], basename(GeoURLS[i,1]), quiet = TRUE, mode="wb")
}

# Files should now be in the directory ../Downloads/2017-Geoduck-SRM-raw/ ... let's confirm that we have all of them:
howmanyfiles <- list.files(pattern="*.raw")
length(howmanyfiles) == nrow(GeoURLS) #should equal TRUE
