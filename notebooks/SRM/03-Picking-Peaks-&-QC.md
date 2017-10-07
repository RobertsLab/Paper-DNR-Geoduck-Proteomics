## Picking peaks & QC'ing abundance results in Skyline

### Software: 
  * Skyline Daily
  
### Files: 
  * [SRM Transitions](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/SRM-Transitions.csv)
  * [Predicted RT calculations]()
  * [PRTC peptides skyline project file]()
  
### Before exporting data from Skyline for data analysis, complete the following:  
  0) Remove peptides not targeted
  1) Add PRTC peptides to targets 
  2) Pick correct peaks for all pe[ptides, replicates (using Predicted RT)
  3) Adjust peak boundaries as necessary
  4) Remove transitions that do not align with the predicted RT
  5) Remove any peptides that do not have at least 2 transitions

Some helpful keyboard shortcuts:
  * Scroll between replicates: Ctrl+Up or Ctrl+Down 
  * Auto-zoom to best peak: F11
  * Un-autozoom out from best beak: Shift+F11

## Step 0) Remove peptides not targeted 
For the MS/MS run on Vantage we supplied a methods file [Vantage_nanoAcq_geoduckLS_60min_DrM27cm.meth](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/Vantage_nanoAcq_geoduckLS_60min_DrM27cm.meth) that indicated a set of peptides/transitions to measure for each protein we wanted to target. Not all the peptides that make up a protein were measured (that would require too much time, and is unnecessary); our standard was to measure 3 peptides per protein, and 3 transitions per peptide, thus 9 transitions per protein.  

When we created our Skyline project file in [Notebook 01](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/notebooks/SRM/01-Create-Skyline-Project.md) all proteins' peptides populated on the analyte tree/"Targets" pane.  Before analyzing our data, let's remove the unnecessary peptides:

Open the [SRM Transitions](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/SRM-Transitions.csv) and the SkylineDaily project.  In the Targets pane of the Skyline project, click on the + sign next to a protein to unfold all peptides.  You'll notice that a few peptides have a colored dot next to them (red, yellow, or green).  Those are likely the peptides you targeted.

To remove peptides, right click on its sequence and select "delete." Note that you can highlight more than one at a time, and delete in bulk, and the "delete" key on your keyboard also works. 

![00](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00.PNG?raw=true)
![00a](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00a.PNG?raw=true)
![00b](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00b.PNG?raw=true)

Once you've removed extraneous peptides, unfold each remaining peptide to reveal the transitions, and delete extraneous transitions. Again, the colored circle next to a transition will indiate one of your targets (but you can confirm this using the ["SRM Transitions" .csv file). NOTE if you accidentally delete a transition erroneously, simply Ctrl+Z to undo. 

![00c](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00c.PNG?raw=true)

The resulting analyte tree should look something like this: 
![00d](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00d.PNG?raw=true)

## Step 1) Add PRTC peptides to targets 
We'll use the PRTC peptides to assess technical rep quality.  We have the PRTC peptides already populated in another Skyline project file.  Start a new Skyline Daily window, and open the [PRTC peptides Skyline project file](); if Skyline starts to automatically import results from your DIA run, you can click "cancel import" for these purposes.  Right click on the PRTC peptide name in the Targets window, select copy, and then paste into your Skyline document into the Targets pane.

![00e](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00e.PNG?raw=true)
![00f](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00f.PNG?raw=true)

There will also be extraneous PRTC peptides, which you can delete. Your final analyte tree should look something like this: 

![00g](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-00g.PNG?raw=true)

## Step 2) Picking peaks

Skyline automatically picks chromatogram peaks for each peptide in each replicate, but it doesn't always do a good job. Using the results from the Predicting Retetion Time step, you need to fix any errors that Skyline made in picking the correct peaks.  For example...

#### Skyline has picked the peak for this HSP90 peptide @ a retention time of 53.7, as indicated by the small black carrot at that retention time, and the fact that Skyline zooms to this "peak" when you auto-zoom to the best peak. 
![1](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-01.PNG?raw=true)

#### However, zooming out, we see that there is a much larger, clearer peak at RT 28.9.  Also, using our Predicted Retention Time calculations (previous notebook), we should see a peak at 29.22.  
![2](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-02.PNG?raw=true)

#### To fix the error, we hover over the correct peak's point, click, thus "picking" that peak.  
![3](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-03.PNG?raw=true)

#### Now, hit F11 again and you'll see that Skyline has zoomed into the newly selected peak, and it is a much cleaner chromatogram. 
![4](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-04.PNG?raw=true)

#### You don't have to do this manually for all your replicates. Instead, right-click anywhere in the chromatogram pane, and select "apply peak to all"
![5](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05.PNG?raw=true)

#### However, Skyline still won't autopick all peaks correctly. For example, for this Superoxide Dismutase peptide I auto-picked the peak at ~14.5 (as per predicted RT), but you'll see that Skyline still did not pick the correct peak for the replicate shown.  Also, notice in the RT pane that RT is not consistently at 14 for all replicates, so you'll need to manually review each peak not at ~14.5 and either pick the correct peak OR remove it (if no peak at 14.5 exists). Additionally, if no peak has been detected for the peak you need to pick, you'll need to draw boundaries; do so by holding the cursor over the x axis where your peak begins until the cursos changes to a double-arrow, click and drag to where the peak ends. Boundary lines should have been drawn, and the peak automatically picked. 

![5a](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05a.PNG?raw=true)

#### You'll notice that the retention times for your replicates will have changed (see top right pane in this screen shot):
![6](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-06.PNG?raw=true)

Complete this step for all peptides.

## Step 3: Adjust peak boundaries as necessary

Skyline can incorrectly assign peak boundaries, such that it is either not measuring the entire peak area, or it captures a secondary peak that it should not.  For example...

This is wrong, since the boundary is too wide, capturing noise on the left: 
![7](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-07.PNG?raw=true)

However over the incorrect boundary, then click and drag it to the correct position, resulting in something like this: 
![8](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-08.PNG?raw=true)

Unfortunately there isn't an "apply to all" option for peak boundaries.  I err on not adjusting boundaries if they are close to perfect, since I'd rather have Skyline consistently pick boundaries that are slightly off than manually adjust all inconsistently. 

## Step 4: Remove peaks for replicates with no clear peak at the predicted retention time

Some replicates (aka data files) do not have a peak at the predicted RT; for example there should be a peak between RT 14-15 for this peptide in Superoxide Dismutase, but none is present. First image is the zoomed out view, second is the zoomed-to-best-peak view. 
![11](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-11.PNG?raw=true)
![12](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-12.PNG?raw=true)

When no peaks are found I remove the peak from the data by right-clicking and selecting "remove peak":
![13](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-13.PNG?raw=true)

In some reps peaks look poor, but they are present. In this situation I kept the peaks, but recorded which peptides which had poor quality peaks across multiple replicates, so I can consider removing them from my data during analysis: 
![09](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-09.PNG?raw=true)
![10](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-10.PNG?raw=true)

#### A note on 2 peaks: 
Not sure what to do in the situation where a peak is split into 2 peaks (as below); Skyline opted for the boundaries to encompass both peaks. I did the same, as the total RT for both peaks appears to be similar to that of other reps:
![17](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-17.PNG?raw=true)

### 4) Remove any peptides that do not have at least 2 transitions
To be confident that data captured for a peptide actually represents that peptide, it's important that at least 2 transitions have peaks at the predicted RT.  Remove peptides with <2 transitions.  This did not occur in my data. 

### Export data from Skyline: 
**_Export -> Report_**, then I edited the default Transition Results report to only include the following metrics: Protein Name, Transitions, Peptide Sequence, Fragment Ion, Peptide Retention Time, Area; I selected "Pivot Replicate Name".  Here's a preview of the report:
![19](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-19.PNG?raw=true)
![16](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-16.PNG?raw=true)

### "Share" skyline project file:
Save your Skyline project, then go to **_File -> Share_**, select "Complete" option and current Skyline-daily format, and click Share. Name the zip folder.  
My resulting Skyline project file is saved on Owl: [2017-geoduck-SRM.sky.zip](http://owl.fish.washington.edu/generosa/Generosa_DNR/2017-July-SRM-various-files/2017-geoduck-SRM.sky.zip)

![20](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-20.PNG?raw=true)

## Notes from my data:
#### Poor quality peptides, where several reps had no peak at predicted RT:
  * Superoxide Dismutase: K.THGAPTDEER.H [68, 77]
  * Catalase: R.LYSYSDTHR.H [351, 359]
  * Na/K-transporting ATPase subunit alpha-4: R.MVTGDNVNTAR.S [727, 737]
  * Protein Disulfide-isomerase (PDI): R.NNKPSDYQGGR.Q [125, 135]
  * PRTC: DIPVPKPK, HVLTSIGEK, GISNEGQNASIK, SSAAPPPPPR
Example of a poor-quality peptide, which is missing from several replicates, is THGAPTDEER in Superoxide Dismutase: 
  ![05b](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05b.PNG?raw=true)
    
#### Replicates with consistently missing peaks @ poor quality peptides: 208, 212, 213
Here is a screen shot of 208 at the Catalase LYSYSDTHR peptide, where no signal occurs until ~22 min then signal sharply appears: 
![05c](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05c.PNG?raw=true)

This is compared to a more regular signal for replicate # 292 for the same peptide:
![05d](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05d.PNG?raw=true)

#### A peptide with RT ~18 must be co-eluting, as it pops up in a few reps/peptides. For example, the following is a zoomed-out view of Ras-related protein peptide, which should have it's peak around 22.7. A couple peptides elute @ ~18min, and could be the culprit: Sodium/potassium-transporting ATPase subunit alpha-4, MVTGDNVNTAR; Catalase, LYSYSDTHR; The following are images from rep #254, but it pops up in lots of reps: 
![14](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-14.PNG?raw=true)
![15](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-15.PNG?raw=true)

## Total actions performed on my SRM data in Skyline:
  * Picked peaks based on predicted RT; in some cases I needed to draw a peak/beak boundaries if Skyline had not detected one.
  * Removed peaks from replicates where no peak was present @ designated retention time (as per DIA/SRM regression). Replications w/ no peaks for peptides will be represented as #N/A when data is exported. 
  * ID'd 4 peptides with very poor data across multiple replicates; I may not use these peptides in my analysis; TBD.
    * Superoxide Dismutase: K.THGAPTDEER.H [68, 77]
    * Catalase: R.LYSYSDTHR.H [351, 359]
    * Na/K-transporting ATPase subunit alpha-4: R.MVTGDNVNTAR.S [727, 737]
    * Protein Disulfide-isomerase (PDI): R.NNKPSDYQGGR.Q [125, 135]
    * PRTC: DIPVPKPK, HVLTSIGEK, GISNEGQNASIK, SSAAPPPPPR
  * Reviewed peak boundaries to see if they needed adjusting. I did NOT adjusting boundaries that Skyline automatically selected, since manually moving boundaries is not reproducible. 
  * ID'd and deleted 2 transitions that do not align with other transitions at designated RT. Transitions are:
    - Superoxide dismutase, TIVVHADVDDLGK, y4 
    - Ras-related protein Rab-11B VVLVGDSGVGK, y4
  * Exported report: [2017-Geoduck-SRM-Skyline-Report.csv](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/2017-Geoduck-SRM-Skyline-Report.csv)
