## Picking peaks in Skyline

Using the results from the Predicting Retetion Time step you need to fix any errors that Skyline made in picking the correct peaks.  For example...

Skyline has picked the peak for this peptide @ a retention time of 53.7, as indicated by the small black carrot at that retention time, and the fact that Skyline zooms to this "peak" when you auto-zoom to the best peak (keyboard shortcut to do that is F11). 
![1](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-01.PNG?raw=true)

#### Zooming out (Shift+F11), we see that there is a much larger, clearer peak at RT 28.9.  Also, using our Predicted Retention Time calculations, we should see a peak at 29.22.  
![2](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-02.PNG?raw=true)

#### To fix the error, we hover over the correct peak's point, click, thus "picking" that peak.  
![3](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-03.PNG?raw=true)

#### Now, hit F11 again and you'll see that Skyline has zoomed into the newly selected peak, and it is a much cleaner chromatogram. 
![4](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-04.PNG?raw=true)

#### #### You don't have to do this manually for all your replicates. Instead, right-click anywhere in the chromatogram pane, and select "apply peak to all"
![5](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-05.PNG?raw=true)

#### You'll notice that the retention times for your replicates will have changed (see top right pane in this screen shot):
![6](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/Picking-Peaks-06.PNG?raw=true)
