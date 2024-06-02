##############################
#
# Open ELM/CLM netcdf files, join, & put into lists of R arrays
#
# AWalker
# April 2021
# BSharma (update,  May 2024)
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
caseidprefix    <- 'FACEconly_exptest'
sites           <- 'US-DUK'
#cases           <- c('I1850CLM45ED_ad_spinup', 'I1850CLM45ED', 'I20TRCLM45ED' )
cases           <- c('I1850ELMFATES_ad_spinup', 'I1850ELMFATES', 'I20TRELMFATES' )
case_labs       <- c('spins', 'trans' )
# either NULL or an integer number of ensemble members, NULL means no ensemble
uq              <- NULL
# the id number member in a UQ ensemble
uq_index        <- NULL
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
concatenate_caseid <- F    # concatenate RDS files for all runs in caseidprefix vector
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


# Functions
source('functions_processing.R')



### Start processing
#######################################
wd_src <- getwd()

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

# separate spins from transient cases
spinss      <- grepl('1850', cases )
cases       <- breakout_cases(cases, spinss, case_labs )
syear       <- breakout_cases(syear, spinss, case_labs )
years       <- breakout_cases(years, spinss, case_labs )
tsteps      <- breakout_cases(tsteps, spinss, case_labs )
fout_nyears <- breakout_cases(fout_nyears, spinss, case_labs )
#print(cases,quote=F)

# Can run the ensembles in a serial loop here or in parallel from an external call script  
nuq <- if(is.null(uq)) 1 else  uq
nuq <- if(is.null(uq_index)) 1:nuq else  uq_index
if(!is.null(uq_index) & concatenate_uq) {
  print('Concatenate UQ ensemble selected but a single ensemble member id (uq_index) also passed,', quote=F) 
  print('    set concatenate_uq to FALSE.', quote=F)
  concatenate_uq <- F 
} 


if(!plot_only) {

print('',quote=F); print('',quote=F); print('',quote=F); print('',quote=F)
print('Processing runs in model output directory:',quote=F)
print(wd_mod_out,quote=F)

# caseidprefix loop
for(cid in 1:length(caseidprefix)) {

  null_wd_out <- F
  if(is.null(wd_out)) {
    wd_out <- paste0(wd_mod_out,'/',caseidprefix[cid],'_processed')
    setwd(wd_mod_out)
    null_wd_out <- T
  }
  if(!file.exists(wd_out)) dir.create(wd_out)

  print('',quote=F)
  print('Postprocessing output directory:',quote=F)
  print(wd_out,quote=F)
  print('',quote=F)
  print('Caseidprefix:',quote=F)
  print(caseidprefix[cid],quote=F)
  print('',quote=F)


  # cases loop
  for(c in 1:length(cases)) {
    setwd(wd_mod_out)

    # extract info for simulations to be combined
    cases_current       <- cases[[c]]
    syear_current       <- syear[[c]]
    years_current       <- years[[c]]
    tsteps_current      <- tsteps[[c]]
    fout_nyears_current <- fout_nyears[[c]]
    ntsteps_current     <- sum(years_current * tsteps_current)
    #if(names(years)[c]=='spins' & fout_nyears_current[1]>1) ntsteps_current <- ntsteps_current + 1 

    # combine sim variables to get all simulations to be put in a single file
    sims <- apply(as.matrix(expand.grid(caseidprefix[cid],sites,cases_current)), 1, paste, collapse='_' )
    print('',quote=F)
    print('Processing (& concatenating) case(s):',quote=F)
    print(sims,quote=F)
    print(cases_current,quote=F)
    print(syear_current,quote=F)
    print(years_current,quote=F)
    print(tsteps_current,quote=F)
    print(fout_nyears_current,quote=F)
  
    # UQ/Ensemble loop 
    for(u in nuq) {
      print('',quote=F);print('',quote=F);print('',quote=F)
      if(!is.null(uq)) print(paste('Processing uq member:',u), quote=F )
      
      # simulations loop
      # - sims simulations will end up in the same nc and RDS file
      for(s in 1:length(sims)) {

        wd_mod_out_sim <- 
          if(is.null(uq)) {
            paste(wd_mod_out,sims[s],'run', sep='/' )
          } else {
            uq_member <- paste0('g',formatC(u,width=5,flag=0))
            paste(wd_mod_out,'UQ',sims[s],uq_member, sep='/' )
          }

        setwd(wd_mod_out_sim)
        if(sims[s]==sims[1]) {
          print('',quote=F)
          print('Setting up new netcdf file ... ',quote=F)
          print('  based on initial output file in:',quote=F)
          print(paste('  ',wd_mod_out_sim),quote=F)

          fdate      <- paste0(formatC(syear_current[1], width=4, format="d", flag="0"), fstart_datetime )
          ifile      <- paste(sims[s],mod,paste0('h',hist),fdate,'nc',sep='.')
          ncdf1      <- nc_open(ifile)
          vdims_list <- lapply(ncdf1$var, function(l) sapply(l$dim, function(l) l$name ) )
          vdims_len  <- lapply(ncdf1$var, function(l) sapply(l$dim, function(l) l$len ) )
          vars_units <- lapply(ncdf1$var, function(l) l$units )

          # redefine existing dimensions
          # - to include all output timesteps
          newvars <-
            lapply(ncdf1$var,
                   function(var) {
                     if(any(vdims_list[[var[['name']]]]=='time'))
                       var$dim[[which(vdims_list[[var[['name']]]]=='time')]]$len <- ntsteps_current
                     var
                   })
          nc_close(ncdf1)


          # subset variables
          if(!is.null(vsub)) newvars <- newvars[vsub]
          vnames <- names(newvars)

          # create new nc file
          setwd(wd_out)
          new_fname <- paste(caseidprefix[cid],sites,names(cases)[c],sep='_')
          if(!is.null(uq)) new_fname <- paste(new_fname,uq_member,sep='_')
          print(paste('  file:',new_fname),quote=F)
          print(paste('  time steps in new file:',ntsteps_current),quote=F)
          newnc     <- nc_create(paste0(new_fname,'.nc'), newvars )
          tend_prev <- tcaug <- 0
          setwd(wd_mod_out_sim)

          # list of variables & key info in new file
          vars_list <- lapply(newnc$var,
                              function(l) list(
                                name     = l$name,
                                longname = l$longname,
                                dims     = sapply(l$dim, function(l) l$name ),
                                len      = sapply(l$dim, function(l) l$len ),
                                units    = l$units
                              ))

          # list of dimension variables
          dimvars_list <- lapply(ncdf1$dim,
                                 function(l) {
                                   if(l$dimvarid$id[1] > 0) {
                                     list(
                                       name     = l$name,
                                       vals     = l$vals,
                                       len      = l$len,
                                       units    = l$units
                                     )
                                   } else NULL
                                 })

          print('done.',quote=F)
        # end of first sim if
        }


        # join history files along redefined dimensions
        ##############################################

        print('',quote=F)
        print('',quote=F)
        print('',quote=F)
        print('Processing case:',quote=F)
        print(sims[s],quote=F)
        print('',quote=F)
        print('Model output at:',quote=F)
        print(wd_mod_out_sim,quote=F)

        # output file years
        if(names(years)[c]=='spins') {
          read_final_year   <- if(tsteps_current[s]==1) 1 else 0
          if(fout_nyears_current[s]>1) {
            year_range <- seq(syear_current[s], years_current[s], fout_nyears_current[s] )
            #if(cases_current[s]==cases_current[1]) year_range <- c(year_range, years_current[s]+read_final_year )  
            year_range <- c(year_range, years_current[s]+read_final_year )  
          } else {
            # the + read_final_year here means yr 1 is not read, because for a spin that shows the initialisation values
            year_range <- syear_current[s]:years_current[s] + read_final_year
          }
          timecount_augment <- tcaug
        } else {
          year_range   <- syear_current[s]:(syear_current[s] + years_current[s] - 1)
          timecount_augment <- 0
        }


        # history file loop
        # can multicore this loop but requires OOP or other method
        print('',quote=F)
        print('Reading data and adding to new netcdf file ... ',quote=F)
        print(paste('  model output file years to read:'),quote=F)
        print(paste(year_range),quote=F)
        print('',quote=F)
        for(y in year_range) {
          fdate <- paste0(formatC(y, width=4, format="d", flag="0"), fstart_datetime )
          ifile <- paste(sims[s], mod, paste0('h',hist), fdate, 'nc', sep='.' )

          # time counting variables
          # - time steps in input file
          tsteps_in_file <- tsteps_current[s] * fout_nyears_current[s]
          if(y==(years_current[s]+1)) tsteps_in_file <- 1
          # - time data in input file start for output file 
          if(names(years)[c]=='spins' & tsteps_current[s]==1) {
            tstart <- y-1
            if(fout_nyears_current[s]>1) {
              #if(cases_current[s]!=cases_current[1]) tsteps_in_file <- tsteps_in_file + 1
              tstart <- 1
            }
          } else {
            tstart <- (y-year_range[1]) * tsteps_current[s] + 1
          }
          tstart <- tstart + tend_prev

          print('',quote=F)
          print(paste('  attempting to read file:',ifile), quote=F )
          if(file.exists(ifile)) {
            ncdf2 <- nc_open(ifile)
            print(paste('  file open'), quote=F )

            # add time dim value
            timevals <- ncvar_get(ncdf2, 'time' )
            #print(timevals)
            print(paste('  number of time values in file:',length(timevals)), quote=F)
            # in the second spin with fout_nyears > 1 the first value is at t=0 and needs to be dropped
            tcount   <- tsteps_in_file 
            if(fout_nyears_current[s]>1 & y==1 ) {
              #if(cases_current[s]!=cases_current[1]) {
                tcount   <- tcount - 1
                timevals <- timevals[-1]
              #}
            }
            print(paste('  write time values in output file starting at:',tstart), quote=F)
            print(paste('  number of time values to write to output file:',tcount), quote=F)
            ncvar_put(newnc, 'time', timevals + timecount_augment, tstart, tcount )
            print(paste('  time values written'), quote=F )
          } else {
            print(paste('  file does not exist.'), quote=F )
          }

          # add var values
          # APW :: currently excludes non-time dimensioned variables
          pv <- 1
          for(v in vnames) {
            if(any(vdims_list[[v]]=='time')) {
              # netcdf start and count arguments for output file
              td        <- which(vdims_list[[v]]=='time')
              vdl       <- vdims_len[[v]]
              vdl[td]   <- tsteps_in_file 
              count     <- vdl
              start     <- rep(1, length(vdl) )
              start[td] <- tstart

              # get values and paste them
              vals <- if(file.exists(ifile)) ncvar_get(ncdf2, v, collapse_degen=F ) else rep(missval, prod(count) )

              # for spins with fout_nyears > 1 the first value is at t=0 and needs to be dropped for the second spin
              # drop the first element of the time dimension as it is at t=0 
              if(fout_nyears_current[s]>1 & y==1) {
                #if(cases_current[s]!=cases_current[1]) {
                   
                  # create a subset matrix to remove the first element of the time dimension
                  # -- create a list of dimension elements (sl), remove first element from time dim, 
                  # -- create a subset matrix from list (sm), reduce time dim by one in count vector    
                  sl        <- lapply(1:length(vdl), function(i, ad ) 1:ad[i], ad=vdl  )
                  sl[[td]]  <- 2:vdl[td] 
                  sm        <- as.matrix(expand.grid(sl)) 
                  count[td] <- tcount
                    
                  # subset vals array with sm and reconfigure to an array of extent - 1 in the time dimension
                  if(F) {
                  #if(v=='FATES_GPP') {
                    print('')
                    print('vals subsetting to remove first time element')
                    print(dim(vals))
                    print(vdl)
                    print(sl) 
                    print(sm) 
                    print(array(vals[sm], count )) 
                  }
                  vals       <- array(vals[sm], count )
                #}
              }
              if(pv<5) {
                print(paste('  write values in output file starting at:', paste(start, collapse=' ' )), quote=F)
                print(paste('  number of values to write to output file:', paste(count, collapse=' ' )), quote=F)
                print(paste('  dimension extents of values array:', paste(dim(vals), collapse=' ' )), quote=F)
                pv <- pv + 1 
              } 
              ncvar_put(newnc, v, vals, start, count )
#                ncvar_put(newnc, v, ncvar_get(ncdf2, v ), start, count )
#              } else {
#                ncvar_put(newnc, v, rep(missval, count ), start, count )
#              }
            }
          }
          print(paste('  all data values written'), quote=F )
          if(fout_nyears_current[s]>1) tend_prev <- tend_prev + tcount 
        }
        print('done.',quote=F)

        # increment counter for last timestep added to new file
        # APW :: not sure if this will behave correctly if fout_nyears > 1 and tsteps > 1, using the tcount variable above should work maybe 
        if(fout_nyears_current[s]==1) tend_prev <- tend_prev + years_current[s]*tsteps_in_file
        tcaug <- timevals[length(timevals)]
        setwd(wd_mod_out)

      # sim loop
      }


      # take data in newnc and create labeled R arrays
      dim_combos <- sapply(unique(vdims_list), function(v) paste(v, collapse=',' ) )
      dc_ss      <- sapply(vdims_list, function(v) which(dim_combos==paste(v, collapse=',' ))  )
      dc_nvars   <- data.frame(dim_combos, nvars=tabulate(dc_ss) )
      dlen       <- sapply(newnc$dim, function(l) l$len )

      # for each unique combination of dimensions create an array
      print('',quote=F)
      print('Creating R arrays ... ',quote=F)
      al <- list()
      for(dc in 1:length(dim_combos)) {

        # find variables with dim_combos[dc] dimensions
        avar_names <- names(dc_ss)[which(dc_ss==dc)]

        # create and label array
        ndims                    <- length(unique(vdims_list)[[dc]])
        dimnames                 <- c(rep(list(NULL),ndims), list(vars=avar_names))
        names(dimnames)[1:ndims] <- unique(vdims_list)[[dc]]
        # APW: need to add dimnames where appropriate, e.g. for ground levels, time? (could add a POSIX standard)
        adim <- dlen[names(dimnames)[1:ndims]]
        a    <- array(dim=c(adim,vars=length(avar_names)), dimnames=dimnames )
        amss <- as.matrix(expand.grid(lapply(adim, function(i) 1:i)))

        # populate array with variables & convert units
        for(v in avar_names) {
          vss                <- which(avar_names==v)
          a[cbind(amss,vss)] <- ncvar_get(newnc, v )
        }

        # combine arrays into a list
        al <- c(al, list(a) )
        names(al)[length(al)] <- dim_combos[dc]
      }
      print('done.',quote=F)

      # close/write nc file
      nc_close(newnc)
      # zip nc file
      setwd(wd_out)
      if(zip) system(paste('gzip -f ', paste0(new_fname,'.nc') ))


      # Create annual data from daily data if requested
      ald <- NULL
      if(to_annual & tsteps_current[1]>1) {
        print('',quote=F)
        print('Aggregating sub-annual data ... ',quote=F)

        ald <- lapply(al, function(a) {
          adim <- dim(a)
          if('time'%in%names(adim) & !any(names(adim)%in%char_dims)) {
            a <- summarize_array(a, summarise_dim='time', extent=tsteps_current[1] )
          }
          a
        })

        print('done.',quote=F)
      }


      # Variable conversion if requested
      if(varconv) {
        print('',quote=F)
        print('Variable conversions for R arrays ... ',quote=F)

        al  <- lapply(al, var_conv_array, var_conv=var_conv, vars_units=vars_units, tstep=tsteps_current[1] )
        ald <- if(!is.null(ald))
               lapply(ald, var_conv_array, var_conv=var_conv, vars_units=vars_units, tstep=tsteps_current[1] )

        vars_units <- lapply(vars_units, function(c)
          if(is.null(var_conv[[c]])) c else var_conv[[c]]$newunits )
        print('done.',quote=F)

        if(timeconv) {
          print('',quote=F)
          print('Time unit conversions for R arrays ... ',quote=F)

          al  <- lapply(al, var_conv_array, var_conv=time_conv, vars_units=vars_units, tstep=tsteps_current[1] )
          ald <- if(!is.null(ald))
            lapply(ald, var_conv_array, var_conv=time_conv, vars_units=vars_units, tstep=1 )

          vars_units <- lapply(vars_units, function(c)
            if(is.null(time_conv[[c]])) c else time_conv[[c]]$newunits )
          print('done.',quote=F)
        }

        # update vars_list
        vars_list <- lapply(vars_list, function(l) {
          l$units <- vars_units[[l$name]]
          l
        })
      }


      # create output list(s) & save RDS file(s)
      print('',quote=F)
      print('Writing RDS file(s) ... ',quote=F)

      setwd(wd_out)
      l1 <- list(dimensions=dlen, dim_combinations=dc_nvars,
                 variables=vars_list, dim_variables=dimvars_list,
                 data_arrays=al )
      saveRDS(l1, paste0(new_fname,'.RDS') )

      if(!is.null(ald)) {
        l1 <- list(dimensions=dlen, dim_combinations=dc_nvars,
                   variables=vars_list, dim_variables=dimvars_list,
                   data_arrays=ald )
        saveRDS(l1, paste0(new_fname,'_annual.RDS') )
      }
      print('done.',quote=F)

      if(null_wd_out) wd_out <- NULL
      # caseidprefix & cases & uq loops, & plot only if
}}}}


if(call_plot) {
  setwd(wd_src)
  source('plot_ELM.R', local=T )
}


if(concatenate_caseid | concatenate_uq) {
  setwd(wd_src)
  source('concatenate_ELM.R', local=T )
}


print('',quote=F)
print('',quote=F)
print('Full postprocessing script done.',quote=F)
print('',quote=F)



### END ###
