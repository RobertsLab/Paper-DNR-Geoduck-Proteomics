## Downloading .raw files 

### Software
  * [RStudio](https://www.rstudio.com/) or [R](https://www.r-project.org/)
 
### Notes on SRM data
  * Data produced by the Vantage MS/MS are formatted as .raw files, one file per sample injection  
  * An "injection" is a volume of liquid pulled from one prepared sample  
  * In this experiment I ran 2 technical replicates per sample, aka each sample was prepared in one autosampler vial with standard, then was injected twice separated by 3-6 days  
  * In a few cases, I ran a third technical replicate  
  * I remade three samples, and ran 2 technical replicates of them as well: 53-remake, 104-remake, 114-remake  
  * All .raw data files are saved on the Roberts Lab Owl server: [2017-July-SRM-Data](http://owl.fish.washington.edu/generosa/Generosa_DNR/2017-July-SRM-Data/)  
  * .raw files are labeled "2017_July_10_bivalves_###"_, where "###" is a 3-digit number generated chronologically as the samples were injected  
  * The .raw files on the Owl server include data for: geoduck gill peptides, PRTC/bsa QC, true blanks, and dilution curve peptides  
  * The [sequence file](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/SRM-Sequence-final.csv) indicates which sample each file corresponds to
  
### Step 1: Open this [00-Downloading-raw-files.R](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/analyses/SRM/00-Downloading-raw-files.R) script in RStudio or R. The script will:
  1) install required R packages
  2) Create a directory in the Downloads folder
  3) Download all .raw files associated with geoduck gill samples and dilution curve samples
  
### Ste 2: Execute R script
NOTE: the .raw files will require 2.57GB memory on your local Downloads directory

### You are now ready to upload to a Skyline project, see [01-Create-Skyline-Project.md](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/notebooks/SRM/01-Create-Skyline-Project.md)
  
  
  

