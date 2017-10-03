## Picking peaks & QC'ing abundance results in Skyline

## Before exporting data from Skyline for data analysis, complete the following:  
  1) Pick all correct peaks for all replicates (using Predicted RT)
  2) Adjust peak boundaries as necessary
  3) Remove transitions that do not align with the predicted RT
  4) Remove any peaks that do not have at least 2 transitions

Some helpful keyboard shortcuts:
  * Scroll between replicates: Ctrl+Up or Ctrl+Down 
  * Auto-zoom to best peak: F11
  * Un-autozoom out from best beak: Shift+F11

### Step 1) Picking peaks

Skyline automatically picks chromatogram peaks for each peptide in each replicate, but it doesn't always do a good job. Using the results from the Predicting Retetion Time step, you need to fix any errors that Skyline made in picking the correct peaks.  For example...

#### Skyline has picked the peak for this HSP90 peptide @ a retention time of 53.7, as indicated by the small black carrot at that retention time, and the fact that Skyline zooms to this "peak" when you auto-zoom to the best peak. 
![1](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-01.PNG?raw=true)

#### However, zooming out, we see that there is a much larger, clearer peak at RT 28.9.  Also, using our Predicted Retention Time calculations (previous notebook), we should see a peak at 29.22.  
![2](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-02.PNG?raw=true)

#### To fix the error, we hover over the correct peak's point, click, thus "picking" that peak.  
![3](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-03.PNG?raw=true)

#### Now, hit F11 again and you'll see that Skyline has zoomed into the newly selected peak, and it is a much cleaner chromatogram. 
![4](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-04.PNG?raw=true)

#### #### You don't have to do this manually for all your replicates. Instead, right-click anywhere in the chromatogram pane, and select "apply peak to all"
![5](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05.PNG?raw=true)

#### You'll notice that the retention times for your replicates will have changed (see top right pane in this screen shot):
![6](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-06.PNG?raw=true)

Complete this step for all peptides.

### Step 2: Adjust peak boundaries as necessary

Skyline can incorrectly assign peak boundaries, such that it is either not measuring the entire peak area, or it captures a secondary peak that it should not.  For example...

This is wrong, since the boundary is too wide, capturing noise on the left: 
![7]()

However over the incorrect boundary, then click and drag it to the correct position, resulting in something like this: 
![8]()

Unfortunately there isn't an "apply to all" option for peak boundaries.  I err on not adjusting boundaries if they are close to perfect, since I'd rather have Skyline consistently pick boundaries that are slightly off than manually adjust all inconsistently. 


### Step 3: Remove transitions that are messy or do not align with the predicted RT

Some transition peaks look poor, but they are present. Here is an example of replicate data for the Superoxide Dismutase protein, showing the overall view and the zoomed view:
![image](https://user-images.githubusercontent.com/17264765/29227951-c8e7a056-7e8c-11e7-8546-0a469b5cdf0b.png)
![image](https://user-images.githubusercontent.com/17264765/29227945-befdd22c-7e8c-11e7-9960-bcec1c7aa1b4.png)

This is in comparison to the following replicate, where there is no peak present betwen RT 14-15:
![image](https://user-images.githubusercontent.com/17264765/29228094-4b28bda2-7e8d-11e7-8dc9-5a97c60a933e.png)
![image](https://user-images.githubusercontent.com/17264765/29228099-5475d9c6-7e8d-11e7-8416-be4c4152b686.png)

Not sure what to do in the situation where a peak is split into 2 peaks (as below); Skyline opted for the boundaries to encompass both peaks. I will do the same, as the total RT for both peaks appears to be similar to that of other reps:
![image](https://user-images.githubusercontent.com/17264765/29229248-1c40b03a-7e92-11e7-8b0e-379fedea422e.png)

#### Notes:
  * Poor quality reps: 178, 254, 208, 212, 213, 297_170728020436, 
  * A peptide with RT ~18 must be co-eluting, as it pops up in a few res/peptides. Perhaps it is the [PEPTIDE W/ 18], that gets stuck in the column and pops up. For example, the following is a zoomed-out view of Ras-related protein peptide, which should have it's peak around 22.7. This is rep #254, but it pops up in lots of reps: 
![image](https://user-images.githubusercontent.com/17264765/29232255-83b93838-7e9f-11e7-8595-18a112d4e0a9.png)
![image](https://user-images.githubusercontent.com/17264765/29232330-fa8a6676-7e9f-11e7-943d-35a15dd45e0e.png)
A couple peptides elute @ ~18min, and could be the culprit: Sodium/potassium-transporting ATPase subunit alpha-4, MVTGDNVNTAR; Catalase, LYSYSDTHR; 

#### Total actions performed on SRM data in Skyline:
  * Removed peaks from replicates where no peak was present @ designated retention time (as per DIA/SRM regression). Replications w/ no peaks for peptides will be represented as #N/A when data is exported. 
  * ID'd peptides with very poor data across multiple replicates; I may not use these peptides in my analysis; TBD. See [peptides in red](https://user-images.githubusercontent.com/17264765/29098024-ee33168e-7c51-11e7-912f-a0fd8d2b2a18.png)
  * Adjusted retention time boundaries for all reps all peptides. I erred on NOT adjusting boundaries if they looked OK to maintain consistency. 
  * ID'd and deleted 2 transitions that do not align with other transitions at designated RT. Transitions are:
    - Superoxide dismutase, TIVVHADVDDLGK, y4 
    - Ras-related protein Rab-11B VVLVGDSGVGK, y4
  * Documented peptides and transitions w/ poor quality over several samples and saved in my [Geoduck-DNR Analysis Repo](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Analyses/2017-August_SRM-Analysis/2017-08-11-SRM-Transition-Cleanup.xlsx)
  
#### Exported data from Skyline: 
Export -> Report, then I edited the Transition Results report with the following metrics: Protein Name, Transitions, Peptide Sequence, Fragment Ion, Peptide Retention Time, Area; I selected "Pivot Replicate Name".  Here's a preview of the report:
![image](https://user-images.githubusercontent.com/17264765/29233937-bbc960dc-7ea8-11e7-9e5d-3417e367fd40.png)

I then exported the same report, NOT pivoted by replicate name. 

Then, I modified the report to remove retention time, and exported in both the pivoted and not-pivoted formats. 

### All files were uploaded to my [Geoduck-DNR/Data](https://github.com/laurahspencer/Geoduck-DNR/tree/master/Data) repo:
[SRM Transition Results, pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified.csv)   
[SRM Transitoin Results, not pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified%2Crep-name-not-pivoted.csv)  
[SRM Transition Results, no RT, pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified-noRT-pivoted.csv)  
[SRM Transition Results, no RT, not pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified-noRT.csv)  

Next steps: 
  * Determine if any .raw files should be discarded, based on poor-quality data and those that I re-ran & re-made
  * Look @ all blank runs to see if there are any weird signals that linger
  * Check out biological blank (sample prepped as per the protein extraction process, but had no tissues) to see if there is any contamination. Should also check out Yaamini's blanks. 
  * Normalize based on PRTC peptides
  * Review Dilution Curve results 
  * Generate NMDS plot
  * .... 
  
