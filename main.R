##############################
#
# Open ELM/CLM netcdf files, join, & put into lists of R arrays
#
# AWalker
# April 2021
#
##############################

rm(list=ls())

library(parallel)
library(ncdf4)



### Initialise
#######################################

##################################
# command line options and defaults

# any one of the below variables to line 100 or so can be specified as a command line argument by following the call to this script with a character string
# each argument string (separated by a space) is parsed and interpreted individually as R code.

#       Rscript process_ELM_output.R "object1<-value1" "object2<-value2"
#  e.g. Rscript process_ELM_output.R "wd_mod_out<-'/home/alp/'" "zip<-T"
#  OR from a calling script can be a single argument variable
#  e.g. ARGS="wd_mod_out<-'/home/alp/' zip<-T"
#       Rscript process_ELM_output.R $ARGS
#       Rscript process_ELM_output.R "$ARGS varconv<-F"


# paths
# directory where model run directories live
wd_mod_out      <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output'
# directory where to save output
wd_out          <- NULL


# filename etc variables
caseidprefix    <- NULL 
caseidsuffix    <- NULL
sites           <- 'US-DUK'
cases           <- c('I1850ELMFATES_ad_spinup', 'I1850ELMFATES', 'I20TRELMFATES' )
case_labs       <- c('spins', 'trans' )
# either NULL or an integer number of ensemble members, NULL means no ensemble
uq              <- NULL
# the id number for a member in a UQ ensemble to process
uq_index        <- NULL
# the id number(s) of members in a UQ ensemble to exclude (e.g. failed to run)
uq_exclude      <- NULL
# model name in output files
mod             <- 'elm'
# start date and time of output files
# - currently only this one is supported, anything other then output files covering integer years requires development
fstart_datetime <- '-01-01-00000'
# netcdf dimensions that contain character variables, can be a vector
char_dims       <- 'string_length'

# time variables
# - syear, years, & tsteps vectors are the same extent as cases and elements correspond
# APW: syear & years may vary by site but currently not supported
# start year of each case
syear           <- c(1, 1, 1850 )
# number of years of each case
years           <- c(60, 60, 145 )
# output timesteps per year of each case
tsteps          <- c(1, 1, 365 )
# number of years in each output file
fout_nyears     <- c(1, 1, 1 )
# number for history file
hist            <- 0
days_in_m       <- c(31,28,31,30,31,30,31,31,30,31,30,31)
time_vars       <- c('mcdate', 'mcsec', 'mdcur', 'mscur', 'nstep',
                     'time_bounds', 'date_written', 'time_written' )
# start date and time of output files
# - currently only this one is supported, anything other then output files covering integer years requires development
fstart_datetime <- '-01-01-00000'
# missing value for new netcdf file
missval         <- 1.e36

# switches
sep_spin           <- T    # separate spin cases from transient ones, done automatically if tsteps are different
zip                <- F    # zip concatenated netcdf
vsub               <- NULL # subscripts (or a character vector) to put only those variables into new netcdf and RDS files
yrzero             <- F    # ignore first year for spinups, currently automatic needs dev
to_annual          <- T    # where tsteps > 1 also produce an annual RDS file
varconv            <- T    # covert variables using functions in var_conv list
timeconv           <- T    # covert variables using functions in time_conv list, only implemented if varconv also TRUE (necessary?)
call_plot          <- T    # call plotting script
plot_only          <- F    # only call plotting or concatenation script if their switches are true, do not process data
highfreq_plots     <- F    # plot high-frequency plots where sub-annual data available
png                <- F    # convert pdf plots into png images to reduce size
plot_nutrients     <- F    # make nutrient plots (if caseidprefix contains 'NP' 'RD' or 'ECA' will be automatic)
plot_quick_pft     <- F    # make plots of biomass by PFT only!! 
# APW: could expand to concat all caseloop elements
concatenate_caseid <- F    # concatenate RDS files for all sites in a given caseloop element 
concatenate_uq     <- F    # concatenate RDS files for all runs in a UQ ensemble
concatenate_daily  <- F    # concatenate only RDS annual files



##################################
# parse command line arguments

print('',quote=F); print('',quote=F)
print('Read command line arguments:',quote=F)
print('',quote=F)
print(commandArgs(T))
if(length(commandArgs(T))>=1) {
  for( ca in 1:length(commandArgs(T)) ) {
    eval(parse(text=commandArgs(T)[ca]))
  }
}

if(concatenate_uq & is.null(uq))          concatenate_uq <- F 
if(concatenate_caseid & length(sites)==1) concatenate_caseid <- F 


### Start processing
#######################################

# source directory
wd_src <- getwd()

# check lengths of input vectors match
length_inv <- c(length(cases), length(syear), length(years), length(tsteps), length(fout_nyears))
if((sum(length_inv)%%length(cases)) != 0 | 
   (length(cases)==1 & sum(length_inv)!=5) 
  ) {
  print('ERROR:: input vectors not of same length')
  print(paste('cases length: ', length(cases) )) 
  print(paste('syear length: ',  length(syear) )) 
  print(paste('years length: ', length(years) )) 
  print(paste('tsteps length: ', length(tsteps) )) 
  print(paste('fout_nyears length: ', length(fout_nyears) ))
  stop('fix input and try again.')
} 


# Can run the ensembles in a serial loop here or in parallel from an external call script  
# -  to run in parallel from an external scrip pass the variable uq_index from the external script
nuq <- if(is.null(uq)) 1 else uq
if(!is.null(uq)) {
  nuq <- if(is.null(uq_index)) 1:nuq else uq_index
  if(!is.null(uq_exclude)) nuq <- nuq[-uq_exclude] 
}
if(!is.null(uq_index) & concatenate_uq) {
  print('Concatenate UQ ensemble selected but a single ensemble member id (uq_index) also passed,', quote=F) 
  print('    set concatenate_uq to FALSE.', quote=F)
  concatenate_uq <- F 
} 


setwd(paste0(wd_src,'/functions'))
source('functions_processing.R')

# separate spins from transient cases
if(length(tsteps)>1) { 
  if(!sum(tsteps==tsteps[1])==length(tsteps)) {
    spinss      <- grepl('1850', cases )
    cases       <- breakout_cases(cases, spinss, case_labs )
    syear       <- breakout_cases(syear, spinss, case_labs )
    years       <- breakout_cases(years, spinss, case_labs )
    tsteps      <- breakout_cases(tsteps, spinss, case_labs )
    fout_nyears <- breakout_cases(fout_nyears, spinss, case_labs )
  } else {
    cases        <- list(cases)
    case_labs    <- 'allcases'
    names(cases) <- case_labs 
    syear        <- list(syear)
    years        <- list(years)
    tsteps       <- list(tsteps)
    fout_nyears  <- list(fout_nyears)
  } 
} else {
  case_labs    <- 'allcases'
  names(cases) <- case_labs 
}
#print(cases,quote=F)


# determine caseid prefix suffix loops 
prefix <- T
suffix <- F
caseloop <- caseidprefix
if(!is.null(caseidprefix) & !is.null(caseidsuffix)) {
  if(length(caseidprefix)>1 & length(caseidsuffix)>1 ) 
    stop("caseidprefix and caseidsuffix lengths both greater than 1")
} else if(is.null(caseidprefix) & is.null(caseidsuffix)) {
  stop("caseidprefix and caseidsuffix both null")
} 

if(!is.null(caseidsuffix)) {
  if(length(caseidsuffix)>1 ) {
    prefix   <- F 
    suffix   <- T
    caseloop <- caseidsuffix  
  }
}  

print('',quote=F)
print('Looping over:',quote=F)
print(caseloop,quote=F)


# caseidprefix/suffix loop
#for(cid in 1:length(caseidprefix)) {
null_wd_out <- F
for(cid in 1:length(caseloop)) {

  # create dir/filename tag
  od <-
    if(prefix & !is.null(caseidsuffix))      paste(caseloop[cid],caseidsuffix,sep='_') 
    else if(suffix & !is.null(caseidprefix)) paste(caseidprefix,caseloop[cid],sep='_') 
    else                                     caseloop[cid]

  # create dir name
  if(is.null(wd_out) | null_wd_out) {
    wd_out <- paste0(wd_mod_out,'/',od,'_processed')
    setwd(wd_mod_out)
    null_wd_out <- T
  }
  if(!file.exists(wd_out)) dir.create(wd_out)

  print('',quote=F)
  print('Caseidprefix:',quote=F)
  if(prefix) print(caseloop[cid],quote=F) else print(caseidprefix,quote=F)
  print('Caseidsuffix:',quote=F)
  if(suffix) print(caseloop[cid],quote=F) else print(caseidsuffix,quote=F)
  print('',quote=F)
  print('Postprocessing output directory:',quote=F)
  print(wd_out,quote=F)
  print('',quote=F)


  # Process netcdf files
  if(!plot_only) {
    print('',quote=F); print('',quote=F); print('',quote=F); print('',quote=F)
    print('Processing runs in model output directory:',quote=F)
    print(wd_mod_out,quote=F)
  
    setwd(paste0(wd_src,'/scripts'))
    source('process_netcdf.R', local=T )
  }
  
  
  # Make figures
  if(call_plot) {
    setwd(paste0(wd_src,'/scripts'))
    source('plot_ELM.R', local=T )
  }
  
  
  # concatenate UQ or caseidprefix/suffix
  if(concatenate_caseid | concatenate_uq) {
    setwd(paste0(wd_src,'/scripts'))
    source('concatenate_ELM.R', local=T )
  }
  
# case prefix/suffix loop
}

print('',quote=F)
print('',quote=F)
print('Full postprocessing script done.',quote=F)
print('',quote=F)



### END ###
