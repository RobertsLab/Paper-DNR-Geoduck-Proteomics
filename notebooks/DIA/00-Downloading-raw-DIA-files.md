## Downloading .raw DIA files 

### Software 
  * Either [RStudio](https://www.rstudio.com/) or [R](https://www.r-project.org/)
 
### Notes on DIA data
  * Data produced by the Lumos MS/MS are formatted as .raw files, one file per sample injection  
  * An "injection" is a volume of liquid pulled from one prepared sample  
  * In this experiment I ran 2 technical replicates per sample, aka each sample was prepared in one autosampler vial with standard, then was injected twice separated by 3-6 days  
  * Sample # 131 had no protein and thus no detected peptides, so was only run once; aka "geoduck16" file does not exist (was going to be the duplicate run on sample 131) 
  * An error occurred when on the injection resulting in file named geoduck7, so Lumos immediately re-injected and re-ran that sample, resulting in file name geoduck7_170124190430. 
  * All .raw data files are saved on the Roberts Lab Owl server: [Lumos_Raw](http://owl.fish.washington.edu/generosa/Generosa_DNR/Lumos_Raw/)  
  * .raw files on owl are labeled "2017_January_23_envtstress_geoduck##.raw" & "2017_January_23_envtstress_blank##.raw" where "##" is a 2-digit number generated chronologically as the samples were injected. 
  * The .raw files on the Owl server include data for: geoduck gill peptides + PRTC ("geoduck) and one blank prepared alongside the samples ("geoduckBlank"); true blank injections (should not contain any peptides, "blank")
  * The [sequence file](../../data/DIA/2017_January_23_sequence_file.csv) indicates to which sample each file corresponds

  
### Step 1: Open this [00-Downloading-raw-DIA-files.R](../../analyses/DIA/Script00-Downloading-raw-DIA-files.R) script in RStudio or R. The script will:
  1) install required R packages
  2) Create a directory in which the .raw files will be saved
  3) Download all .raw files associated with geoduck gill samples, and the one blank sample
  
### Ste 2: Execute R script
NOTE: the .raw files will require 10.6GB 

### You are now ready to demultiplex and convert the .raw data to .mzML in preparation for PECAN, see [01-Demultiplexing .raw data.ipynb](../../notebooks/DIA/01-Demultiplexing .raw data.ipynb)


