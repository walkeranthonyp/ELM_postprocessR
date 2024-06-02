# Functions to post-process ELM/CLM netcdf output # 

Scripts/functions that will take annual ELM/CLM netcdf output files and concatenate them.
Spin up cases are also autmatically concatenated.
A single netcdf is produced for all concatenated output.  
Also produced are RDS files (R binaries) that reproduce the netcdf data in a way. 
The RDS files contain a list composed of necdf metadata and a list of arrays that contain the netcdf data.
There are as many arrays as dimension combinations in the netcdf, e.g. there is a single array for all variables with the dimensions lndgrd and time.
Continuing the example, each varible in the the netcdf with these dimensions will be contained in this array, so the array will have three dimensions: lndgrd, time, and vars.
This array will be named '`lndgrd,time`'.   
  

Written in R. 
Developed on Mac OS Catalina with R version 4.0.
Designed to be run from the command line. The plotting script can be copied to a work directory and modifed as needed.  



### Use guidelines ### 

If you want to use this code then please do! 
This is completely experimental code and comes with no guarantees that it wortks in many situations. 
Please feel free to contribute by creating a newbranch that includes a username label and push and make a PR.  


### dependencies ###

netcdf4.0 libraries

R packages: ncdf4, ggplot2, png, viridis 

### Run ###

Use Rscript from the command line to run `functions_processing.R` and once that has been run, to run `functions_plotting.R`. The later can also be called from the former.
Look inside each of the above scripts to see command line arguments and how they are to be specified.  
 


### Related publications ###

### Sponsorship ###

We are grateful for the support from the U.S. Department of Energy (DOE) Office of Science, Biological and Environmental Research (BER). 

### Please direct questions to ###

Anthony Walker (walkerap@ornl.gov)



<!-- END -->
