### Retention Time R2 Calcs

One of the first steps in processing SRM data is to confirm that the selected peaks actually represent the peptides, aka that our assay works.  To do this, we use linear regression between PRTC retention times in DIA and SRM to calculate predicted transition RTs in collected SRM data. Then, we calculate the R^2 for PRTC and experimental peptides compared to predicted.

#### Step 1
Copied PRTC peptides from another Skyline project file, pasted into my Geoduck assay Skyline file. 
![image](https://user-images.githubusercontent.com/17264765/29236292-9f138b24-7ebd-11e7-82f7-5d3496b14f3d.png)

#### Step 2
The method file used in mass spec run resulted in us not collecting all PRTC peptides, so I removed those from the Skyline document. 
![snip20170811_2](https://user-images.githubusercontent.com/17264765/29236301-a44f7ce2-7ebd-11e7-96ff-4068741eff19.png)

Some of the PRTC peptides had no signal in several of my replicates. These peptides eluted at ~14 or 18 minutes.  I removed the peaks from all reps which had no signal at the predicted RT.  In this screen shot the peptides with lots of signal missing are unfolded on the left side, and one of the cruddy reps' chromatogram is pulled up. There should be a peak @ around 17mins.
![image](https://user-images.githubusercontent.com/17264765/29236477-b7cc25ac-7ebf-11e7-864c-67f83fe5dbb5.png)

Notice that the PRTC peptide signals vary, which is due to me using different PRTC mixes. I documented which mix I used, and the concentrations of each, so I will need to account for that when I normalize my assay daya using PRTC data. More on that later.

#### Step 3
Exported retention times from Skyline via File -> Export -> Report -> Peptide RT Results. Saved in my Geoduck-DNR Repo as [2017-08-11-SRM-Retention-Times.csv](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_SRM-Retention-Times.csv)
![image](https://user-images.githubusercontent.com/17264765/29236607-b32011ce-7ec1-11e7-89a3-2072db660a51.png)

#### Step 4
In Excel: Use all SRM replicates' PRTC retention times to calculate average, then regress against DIA. Spreadsheet with calcs and plots are saved in my [Geoduck-DNR/2017-August_SRM-Analysis Repo](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Analyses/2017-August_SRM-Analysis/2017-08-11-Predicted-SRM-Retention-Times-ALL-DATA.xlsx)

First, I opened SRM RT data, copied and pasted PRTC data & pasted into new spreadsheet. Removed all #N/A values (via Find/Replace function), then averaged all replicate RT for each peptide. Results (and notice averageif() equation): 
![image](https://user-images.githubusercontent.com/17264765/29236856-058bd6a0-7ec7-11e7-814a-6f687fb247e0.png)

Regressed the average PRTC retention time (RT) in SRM against DIA:
![image](https://user-images.githubusercontent.com/17264765/29237096-cce05150-7ecb-11e7-94ae-42c5db872143.png)

Averaged predictions over all DIA replicates: 
![image](https://user-images.githubusercontent.com/17264765/29237073-73bddfa2-7ecb-11e7-8c2d-605ea8d0d099.png)

Compared predicted vs. actual RT in SRM: 
![Predicted RT vs. Actual](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Analyses/2017-August_SRM-Analysis/2017-08-11-Predicted-vs-Actual-RT.png?raw=true)

![Predicted RT vs. Actual by protein](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Analyses/2017-August_SRM-Analysis/2017-08-11-Predicted-vs-Actual-RT-byProtein.png?raw=true)

All proteins but Cytochrome P450 have R^2 > 0.96.  I'll have to investigate what's up with Cytochrome P450 RT to see what might be the culprit. 

