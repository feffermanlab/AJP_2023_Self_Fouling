Trade-offs in Resource Access and Health by Avoidance of Self-Fouling, Motivated via Disgust
Alexander J. Pritchard, Nina H. Fefferman

CODE FILE NOTES

Before running any code, first decide where to put the files. Right now, the code should run with minimal edits if you place everything in: “C:\Data\Buff”. Within this directory, the output should end up in the subdirectory: “C:\Data\Buff\OUTPUT\”. Before executing the code, you will need to put the NOAA National Climatic Data Center's Global Historical Climatology Network Daily data in the same directory. The direct link is: "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/KEM00063686.dly" 

---
KEM00063686.dly Citations:
Menne, M.J., I. Durre, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012:  An overview 
of the Global Historical Climatology Network-Daily Database.  Journal of Atmospheric 
and Oceanic Technology, 29, 897-910, doi:10.1175/JTECH-D-11-00103.1.

Menne, M.J., I. Durre, B. Korzeniewski, S. McNeal, K. Thomas, X. Yin, S. Anthony, R. Ray, 
R.S. Vose, B.E.Gleason, and T.G. Houston, 2012: Global Historical Climatology Network - 
Daily (GHCN-Daily), Version 3.12. 
NOAA National Climatic Data Center. http://doi.org/10.7289/V5D21VHZ [accessed: December, 17, 2021].
---

Also, please be aware, if you choose to extract the zip to a different directory, then the directories in all of the R files will need to be updated.

Due to space constraints, the zipped code does not contain the results files we used for our figure generation. If you would like to use our exact results, then either contact us for the results files or precisely follow details below. 


Important code files for execution:
----------------

Self_Fouling_Code.R
If unpacked into the correct directory, with the OUTPUT subdirectory, and if the KEM00063686.dly file was downloaded from the NOAA's database, then this code should execute without error. The parameters are automatically generated at the start of the first run. Catchment, however, was manually altered (line 92). To speed execution, we ran each of the Catchment values simultaneously [with a seed of 50 for Dis_params = c(0.1, 1.0, 10, 100, 4, 7), and a seed of 120 for Dis_params = c(25, 50, 75)].

----------------

Analysis_5ExtractFork.R ; Analysis_5ImageFork.R ; Analysis_5_Plus.R
These files extract summary metrics for analysis and create visualizations, including the figures presented in the manuscript. Note that some re-analyses requiring merged results files [rbind()] after extracting values. For instance, to combine the outputs across Catchments. 

----------------
