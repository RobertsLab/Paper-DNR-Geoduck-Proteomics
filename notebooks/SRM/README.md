### These notebooks detail steps needed to analyze data from the Vantage MS/MS. Notebooks include the following:

#### 00-Create-Skyline-Project
  - Use Skyline project files created in DIA analysis to easily create a new Skyline project file for SRM data
  - Import SRM data, adjust Skyline view
#### 01-Retention-Time-Calcs.md
  - Export RT from SRM & DIA datasets
  - Regress SRM RT against DIA RT for PRTC peptides
  - Calcuated predicted RT for proteins in SRM data
#### 02-Picking-Peaks-&-QC.md
  - Correct incorrectly picked peaks in SRM pepties
  - Adjust peak boundaries
  - Remove peaks from replicates/peptides that don't have peaks @ predicted RT
  - Note poor quality replicates/peptides
#### 03-SRM Data Analysis.ipynb	
  - Execute all in R-Studio
  - Annotate SRM peak abundance data with site, treatment, sample #
  - NMDS plots
  - Average technical reps
  - ANOSIM 
  - Barplots
