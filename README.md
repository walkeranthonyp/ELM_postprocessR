# Functions to post-process ELM/CLM netcdf output # 

Scripts/functions that will take annual ELM/CLM netcdf output files and concatenate them.
Developed exclusively using ELM-FATES for testing and operation.
Spin up cases (or all cases with the same output frequency) are autmatically concatenated.
A single, multiyear netcdf is produced for all concatenated output.  

Also produced are RDS files (R binaries) that include all the netcdf data. 
The RDS files contain a list composed of necdf metadata and a list of arrays that contain the netcdf data.
Each array contains variables of common dimension. 
The arrays are named with the ELM combination of dimension names. 
There are as many arrays in the list as dimension combinations in the netcdf, e.g. there is a single array for all variables with the dimensions lndgrd and time.
Continuing the example, each varible in the the netcdf with these dimensions will be contained in this array, so the array will have three dimensions: lndgrd, time, and vars.
This array will be named '`lndgrd,time`'.   
  

Written in R. 
Developed on Mac OS Catalina & Tahoe with R version >4.0.
Designed to be run from the command line on HPC. 



### Use guidelines ### 

If you want to use this code then please do! 
This is completely experimental code and comes with no guarantees that it wortks in many situations. 
Please feel free to contribute by creating a newbranch that includes a username label and push and make a PR.  


### dependencies ###

netcdf4.0 libraries

R packages: ncdf4, ggplot2, png, viridis 

### Run ###

Use Rscript from the command line to run `main.R`.
Look inside the above scripts to see command line arguments and how they are to be specified.  
 


### Related publications ###

### Sponsorship ###

We are grateful for the support from the U.S. Department of Energy (DOE) Office of Science, Biological and Environmental Research (BER). 

### Please direct questions to ###

Anthony Walker (walkerap@ornl.gov)



<!-- END -->
