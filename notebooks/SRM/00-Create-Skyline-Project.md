## Creating a Skyline Document & Uploading .raw files

Here's a quick and easy way to make a Skyline project file to analyze your SRM data in Skyline, leveraging work already done in the DIA analysis step.  
IMPORTANT NOTE: Use **Skyline Daily**, as opposed to Skyline.  The Daily version automatically updates when the developers push changes.  If you try to open a project in Sklyine that was created in the Daily version it likely won't work.

### Step 1: Open Skyline daily project files that house your PRTC peptides and your targeted proteins. 

#### Open the PRTC Skyline Daily file used in the DIA analysis: 
![01](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-01.PNG?raw=true)

#### Open the Targeted Transitions for SRM Sklyine Daily file from the DIA analysis, which only includes the proteins of interest for SRM: 
![02](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-02.PNG?raw=true)

#### You'll now have two Skyline projects open at the same time. 
![03](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-03.PNG?raw=true)

### Step 2: Create a new Skyline project file with peptides from both proteins of interest and PRTC 

#### In the PRTC project in the Targets frame click on the PRTC peptides, right click, and select copy
![04](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-04.PNG?raw=true)

#### Move to the Targeted Transitions Skyline project, right click anywhere in the Targets frame, select paste
![05](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-05.PNG?raw=true)

#### You should now have both your PRTC and targeted proteins populated on the left side of the screen, in "Targets."  **Save the project under a new name**, e.g. for this project I saved as "[2017-Geoduck-SRM-Results.sky.zip](http://owl.fish.washington.edu/generosa/Generosa_DNR/2017-July-SRM-various-files/)".  
![06](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-06.PNG?raw=true)

#### Remove DIA results from Sklyine project file by selecting _Edit -> Manage Results -> Remove All_. You should still see the peptides in "Targets", but no chromatograms should remain: 
![07](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-07.PNG?raw=true)

### Step 3: Import .raw files into Skyline.  

#### _NOTE: there are lots of .raw files with data from blank and QC/PRTC standard vials.  I do not recommend adding this data to the same Skyline project file as your sample data, as it can be confusing when trying to pick peaks and QC your peptides._

#### Download all biological replicate .raw files from the SRM run, using the finalized sequence file as reference.  The sequence file is exported from the Vantage MS/MS at the end of the run, and it corresponds .raw files to sample names. Here is the sequence file for this run: [SRM-Sequence-final.csv](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/data/SRM/SRM-Sequence-final.csv). Here is a screenshot of the sequence file, showing a geoduck SRM data file name circled in BLUE, and the corresponding geoduck sample number circled in RED.  
![08](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-08.PNG?raw=true)

#### Once your sample .raw files are saved locally, in Sklyine go to _File -> Import -> Results_
![09](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-09.PNG?raw=true)

#### A box will pop up; I used the default settings:
![10](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-10.PNG?raw=true)

#### Another box will pop up giving you the option to remove extraneous info for each replicate, which I recommend: 
![11](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-11.PNG?raw=true)

#### As the SRM data is importing you'll see chromatograms popping up. 
![12](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-12.PNG?raw=true)

### Step 4: Re-arrange Skyline window

#### Once all files are imported, re-arrange your view as follows:
 * _View -> Retention Time -> Replicate Comparison_
 * _View -> Peak Areas -> Replicate Comparison_

#### Your Skyline window should look something like this, and you're now ready to QC the data: 
![13](https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/blob/master/images/SRM-Skyline-Project-13.PNG?raw=true)
