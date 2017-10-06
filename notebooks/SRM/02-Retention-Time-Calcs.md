### Retention Time R2 Calcs

Software: 
  * Skyline Daily
  * Excel   

One of the first steps in processing SRM data is to confirm that the selected peaks actually represent the peptides, aka that our assay works.  To do this, we use linear regression between PRTC retention times in DIA and SRM to calculate predicted transition RTs in collected SRM data. Then, we calculate the R^2 for PRTC and experimental peptides compared to predicted. 

#### Step 1
Opened my SRM Skyline project file, which includes PRTC peptides. 
![RT 01](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-01.png?raw=true)

#### Step 2
The method file used in mass spec run resulted in us not collecting all PRTC peptides, so I removed those from the Skyline document. 
![RT 02](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-02.png?raw=true)

Some of the PRTC peptides had no signal in several of my replicates. These peptides eluted at ~14 or 18 minutes.  I removed the peaks from all reps which had no signal at the predicted RT.  In this screen shot the peptides with lots of signal missing are unfolded on the left side, and one of the cruddy reps' chromatogram is pulled up. There should be a peak @ around 17mins.
![RT 03](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-03.png?raw=true)

Notice that the PRTC peptide signals vary, which is due to me using different PRTC mixes. I documented which mix I used, and the concentrations of each. If I want to normalize my assay data using PRTC abundance, I'll need to account for the different concentraitons. More on that later.

#### Step 3
Exported retention times from Skyline via File -> Export -> Report -> Peptide RT Results. Saved as [2017-08-11-SRM-Retention-Times.csv](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/data/SRM/2017-08-11_SRM-Retention-Times.csv)
![RT 04](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-04.png?raw=true)

#### Step 4
In Excel: Use all SRM replicates' PRTC retention times to calculate average, then regress against DIA. Spreadsheet with calcs and plots are saved as [2017-07-18-Predicted-SRM-Retention-Times-LHS.xlsx](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/data/SRM/2017-07-18-Predicted-SRM-Retention-Times-LHS.xlsx)

First, I opened SRM RT data, copied and pasted PRTC data & pasted into new spreadsheet. Removed all #N/A values (via Find/Replace function), then averaged all replicate RT for each peptide. Results (and notice averageif() equation): 
![RT 05](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-05.png?raw=true)

Regressed the average PRTC retention time (RT) in SRM against DIA:
![RT 06](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-06.png?raw=true)

Averaged predictions over all DIA replicates: 
![RT 07](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-07.png?raw=true)

Compared predicted vs. actual RT in SRM: 
![Predicted RT vs. Actual](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-08-Predicted-vs-Actual-RT.png?raw=true)

![Predicted RT vs. Actual by protein](https://github.com/RobertsLab/Paper-DNR-Proteomics/blob/master/images/SRM-RT-Calcs-09-Predicted-vs-Actual-RT-byProtein.png?raw=true)

All proteins but Cytochrome P450 have R^2 > 0.96. 
