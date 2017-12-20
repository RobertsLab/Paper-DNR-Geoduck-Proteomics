The following was executed in RStudio, and then on the online DAVID and ReviGO resources. 

# Analyzing DIA results:
Identifying biological functions that we were able to detect in DIA data across ALL samples

  * Merged DIA abundance data with protein annotations (Protein name, SPID, E-value, species, etc.)
  * Normalized abundance data by Total Ion Current (TIC) within replicates
  * Generated .csv with Uniprot IDs for 1) Whole Transcriptome, and 2) Proteins detected in DIA 
  * Used DAVID & ReviGO to assess major GO terms/biological functions detected via DIA method 

# Analyzing DIA results in light of SRM results: 
In light of the SRM data I returned to the DIA data to see if there are any large patterns in the biological functions that were differentially expressed between regions, North (FB, PGB) vs. South (CI, WB).

### Performed the following in R-Studio
  * Ran ANOSIM for all transitions by site (bay+habitat) to assess global protein expression between sites (more just b/c I'm interested, as n=1 for each site)
  * Generated NMDS plots of all DIA abundances to assess quality of technical replicates
  * Identified 5 replicates that were very poor (>0.2 ordination distance), and removed them from my dataset: G12, G15, G17, G18, G20
  * Calculated the mean transition abundance for each biological replicate 
  * Calculated the sum of all transition abundance for each protein
  * Calculated the mean, SD & CV of the protein abundance across all 4 samples within each region, N vs. S
  * Calculated fold change of mean abundance, N/S & S/N 
  * Extracted a dataset that includes:  All proteins that were, on average, expressed >2x in North or South samples, AND mean CV<1 for both regions. 
  
### Uploaded Uniprot IDs to DAVID along with the full list of UniprotID's that we detected in DIA data. I did this for each region separately. 
  ![DAVID Analysis](../../images/DAVID2-01.PNG?raw=true)
  ![DAVID Analysis](../../images/DAVID2-02.PNG?raw=true)
### Selected "Biological Function" Chart
  ![DAVID Analysis](../../images/DAVID2-03.PNG?raw=true)
  ![DAVID Analysis](../../images/DAVID2-04.PNG?raw=true)
### Opened DAVID results in Excel, then copied the GO and P-value columns  
  ![DAVID Analysis](../../images/DAVID2-05.PNG?raw=true)
  ![ReviGO Analysis](../../images/DAVID2-06.PNG?raw=true)
### Pasted GO & Pvalues into ReviGO
  ![ReviGO Analysis](../../images/DAVID2-07.PNG?raw=true)

### 428 proteins 2x more abundant in Northern Sites, in the following GO categories:
  ![2x More in North](../../images/DAVID2-11-FC2-NorthUp.PNG?raw=true)
  ![2x More in North](../../images/DAVID2-12-FC2-NorthUp2.PNG?raw=true)
  ![2x More in North](../../images/DAVID2-13-FC2-NorthUp3.PNG?raw=true)
  
### 126 proteins 2x more abundant in Southern Sites: 
   ![2x More in South](../../images/DAVID2-14-FC2-SouthUp1.PNG?raw=true)
   ![2x More in South](../../images/DAVID2-15-FC2-SouthUp2.PNG?raw=true)
