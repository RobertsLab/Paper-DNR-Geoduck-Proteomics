## Picking peaks & QC'ing abundance results in Skyline

## Before exporting data from Skyline for data analysis, complete the following:  
  1) Pick correct peaks for all pe[ptides, replicates (using Predicted RT)
  2) Adjust peak boundaries as necessary
  3) Remove transitions that do not align with the predicted RT
  4) Remove any peptides that do not have at least 2 transitions

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
![7](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-07.PNG?raw=true)

However over the incorrect boundary, then click and drag it to the correct position, resulting in something like this: 
![8](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-08.PNG?raw=true)

Unfortunately there isn't an "apply to all" option for peak boundaries.  I err on not adjusting boundaries if they are close to perfect, since I'd rather have Skyline consistently pick boundaries that are slightly off than manually adjust all inconsistently. 

### Step 3: Remove transitions with no clear peak at the predicted retention time

Some replicates (aka data files) do not have a peak at the predicted RT; for example there should be a peak between RT 14-15 fir this peptide in Superoxide Dismutase, but none is present. First image is the zoomed out view, second is the zoomed-to-best-peak view. 
![11](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-11.PNG?raw=true)
![12](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-12.PNG?raw=true)

When no peaks are found I remove the peak from the data by right-clicking and selecting "remove peak":
![13](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-13.PNG?raw=true)

In some reps the transition peaks look poor, but they are present. In this situation I kept the peaks, but recorded which peptides which had poor quality peaks across multiple replicates, so I can consider removing them from my data during analysis: 
![09](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-09.PNG?raw=true)
![10](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-10.PNG?raw=true)

#### A note on 2 peaks: 
Not sure what to do in the situation where a peak is split into 2 peaks (as below); Skyline opted for the boundaries to encompass both peaks. I did the same, as the total RT for both peaks appears to be similar to that of other reps:
![17](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-17.PNG?raw=true)

### 4) Remove any peptides that do not have at least 2 transitions
To be confident that data captured for a peptide actually represents that peptide, it's important that at least 2 transitions have peaks at the predicted RT.  Remove peptides with <2 transitions.  This did not occur in my data. 

### Now you should be ready to export you data as a report. 

#### Notes from my data:
  * Poor quality reps: 178, 254, 208, 212, 213, 297_170728020436, 
  * A peptide with RT ~18 must be co-eluting, as it pops up in a few reps/peptides. For example, the following is a zoomed-out view of Ras-related protein peptide, which should have it's peak around 22.7. A couple peptides elute @ ~18min, and could be the culprit: Sodium/potassium-transporting ATPase subunit alpha-4, MVTGDNVNTAR; Catalase, LYSYSDTHR; The following are images from rep #254, but it pops up in lots of reps: 
![14](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-14.PNG?raw=true)
![15](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-15.PNG?raw=true)

### Total actions performed on my SRM data in Skyline:
  * Removed peaks from replicates where no peak was present @ designated retention time (as per DIA/SRM regression). Replications w/ no peaks for peptides will be represented as #N/A when data is exported. 
  * ID'd peptides with very poor data across multiple replicates; I may not use these peptides in my analysis; TBD. See [peptides in red](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-18.PNG?raw=true)
  * Adjusted retention time boundaries for all reps all peptides. I erred on NOT adjusting boundaries if they looked OK to maintain consistency. 
  * ID'd and deleted 2 transitions that do not align with other transitions at designated RT. Transitions are:
    - Superoxide dismutase, TIVVHADVDDLGK, y4 
    - Ras-related protein Rab-11B VVLVGDSGVGK, y4
  * Documented peptides and transitions w/ poor quality over several samples and saved as [2017-08-11-SRM-Transition-Cleanup.xlsx](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/2017-08-11-SRM-Transition-Cleanup.xlsx)
  
#### Exported data from Skyline: 
**_Export -> Report_**, then I edited the Transition Results report with the following metrics: Protein Name, Transitions, Peptide Sequence, Fragment Ion, Peptide Retention Time, Area; I selected "Pivot Replicate Name".  Here's a preview of the report:
![16](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-16.PNG?raw=true)

I then exported the same report, NOT pivoted by replicate name. 
Then, I modified the report to remove retention time, and exported in both the pivoted and not-pivoted formats. 

### All files were uploaded to my [Geoduck-DNR/Data](https://github.com/laurahspencer/Geoduck-DNR/tree/master/Data) repo:
[SRM Transition Results, pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified.csv)   
[SRM Transitoin Results, not pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified%2Crep-name-not-pivoted.csv)  
[SRM Transition Results, no RT, pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified-noRT-pivoted.csv)  
[SRM Transition Results, no RT, not pivoted](https://github.com/laurahspencer/Geoduck-DNR/blob/master/Data/2017-08-11_Transition%20Results_LHS%20modified-noRT.csv)  
