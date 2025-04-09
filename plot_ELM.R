##############################
#
# Open ELM/CLM lists of R arrays and plot
#
# AWalker
# April 2021
#
##############################


# Script to plot ELM-FATES output
# Designed to be called from ELMpostprocessr.R 

library(lattice)
library(grid)
library(viridis)


# variables not passsed from processing script
wd_out_plots   <- NULL
case_xlabs     <- c(spins='Spin-up', trans='Transient', aCO2='Ambient CO2', eCO2='Elevated CO2' )
case_timestep  <- c(spins='year', trans='day', aCO2='day', eCO2='day' )
plot_nutrients <- F

print('',quote=F);print('',quote=F);print('',quote=F)

if(exists('call_plot')) {

  print('Plotting function called from processing script.',quote=F)

} else {

  print('Plotting function called as stand alone.',quote=F)

  # paths
  wd_out       <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output/FACEconly_exptest_processed'
  wd_out       <- '/Volumes/disk2/Research_Projects/FATES/runs/tests/FACEconly_exptest/raw_output/FACEconly_exptest_vcmax70_processed'
  # directory where to save output, if NULL same as wd_out

  # filename variables
  caseidprefix   <- 'FACEconly_exptest'
  caseidprefix   <- 'FACEconly_exptest_vcmax70'
  sites          <- 'US-DUK'
  case_labs      <- c('spins', 'trans' )
  cases          <- c('I1850ELMFATES_ad_spinup', 'I1850ELMFATES', 'I20TRELMFATES' )
  highfreq_plots <- F

  # either NULL or an integer number of ensemble members
  uq             <- NULL
  # to parallelize the plotting function externally, `uq_index` can be passed via command line 
  uq_index       <- NULL

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

  # check cases 
  spinss      <- grepl('1850', cases )
  cases       <- breakout_cases(cases, spinss, case_labs )

  # calculate uq/ensemble indexing 
  nuq <- if(is.null(uq)) 1 else  uq
  nuq <- if(is.null(uq_index)) 1:nuq else  uq_index
}



#######################################
# Initialise

# Functions
source('functions_plotting.R')


# plotting lists
source('info_plotting_fatesoutv2.R')



#############################################
# open RDS lists of arrays

print('',quote=F)
print('Plotting cases in processed model output directory:',quote=F)
print(wd_out,quote=F)
setwd(wd_out)

if(is.null(wd_out_plots)) {
  wd_out_plots <- wd_out
} else if(!file.exists(wd_out_plots)) dir.create(wd_out_plots)
print('  to figure directory:',quote=F)
print(wd_out_plots,quote=F)


for(c in 1:length(names(cases))) {

  # UQ/Ensemble loop 
  # - can run the ensembles in a serial loop here or in parallel  
  for(u in nuq) {
  
  fname   <- paste(caseidprefix,sites,case_labs[c],sep='_')
  fname_a <- paste0(fname,'_annual.RDS')

  # overwriting the fname and fname_a if running UQ ensembles
  if(!is.null(uq)) {
    print('',quote=F); print('',quote=F); print('',quote=F)
    print(paste('Plotting for ensemble index : ', u, ' / ', uq ))
    fname     <- paste(caseidprefix,sites,case_labs[c],sep='_')
    uq_member <- paste0('g',formatC(u,width=5,flag=0))
    fname     <- paste0(fname,'_',uq_member)
    fname_a   <- paste0(fname,'_',uq_member,'_annual.RDS')
  }

  if(file.exists(fname_a)) {
    l1    <- readRDS(fname_a)
  } else {
    l1    <- readRDS(paste0(fname,'.RDS'))
  }

  # extract data
  # site and time dimensioned variables
  algtime      <- l1$data_arrays$`lndgrid,time`
  # site, ground levels, and time dimensioned variables
  alglgtime    <- l1$data_arrays$`lndgrid,levgrnd,time`
  # site, pft, and time dimensioned variables
  algpfttime   <- l1$data_arrays$`lndgrid,fates_levpft,time`
  # site, size class, and time dimensioned variables
  algsclstime  <- l1$data_arrays$`lndgrid,fates_levscls,time`
  # site, cohort age class, and time dimensioned variables
  algcaclstime <- l1$data_arrays$`lndgrid,fates_levcacls,time`

  # plot required figures
  plots <- make_figures(algtime, plotlist=plotlist, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
  plot_figures(plots, paste0(fname,'.pdf'), png=png )

  plots <- make_figures(algtime, alglgtime, plotlist=plotlist_phys, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
  plot_figures(plots, paste0(fname,'_physiology','.pdf'), png=png )

  if(grepl('NP|RD|ECA',caseidprefix) | plot_nutrients) {
    print('',quote=F)
    print('PLOTTING NUTRIENTS')
    plots <- make_figures(algtime, plotlist=plotlist_nutrients, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
    plot_figures(plots, paste0(fname,'_nutrients','.pdf'), png=png )
  }

  if(l1$dimensions['fates_levpft'] > 1) {
    print('',quote=F)
    print('PLOTTING PFT')
    vcl   <- paste0('PFT', 1:l1$dimensions['fates_levpft'] )
    plots <- make_figures(algpfttime, plotlist=plotlist_pft, xlab=case_xlabs[case_labs[c]],
                          timestep=case_timestep[case_labs[c]], glab=vcl )
    plot_figures(plots, paste0(fname,'_pft','.pdf'), png=png )
  }

  if(l1$dimensions['fates_levscls'] > 1) {
    print('',quote=F)
    print('PLOTTING SIZE CLASS')
    scls <- l1$dim_variables$fates_levscls$vals
    for(i in 1:length(scls)) {
       vc <-
        if(i!=length(scls)) paste(scls[i],'-',scls[i+1],'cm')
        else paste('>',scls[i],'cm')
      vcl <- if(i==1) vc else c(vcl,vc)
    }

    plots <- make_figures(algsclstime, a3d2=algcaclstime, plotlist=plotlist_size_class, xlab=case_xlabs[case_labs[c]],
                          timestep=case_timestep[case_labs[c]], glab=vcl )
    plot_figures(plots, paste0(fname,'_size_class','.pdf'), png=png )
  }

  # extract & plot daily data
  if(file.exists(fname_a) & highfreq_plots) {
    l2 <- readRDS(paste0(fname,'.RDS'))
    algtime_highfreq   <- l2$data_arrays$`lndgrid,time`
    alglgtime_highfreq <- l2$data_arrays$`lndgrid,levgrnd,time`

    plots <- make_figures(algtime_highfreq, plotlist=plotlist, xlab=case_xlabs[case_labs[c]], timestep='year' )
    plot_figures(plots, paste0(fname,'_highfreq','.pdf'), png=png )

    plots <- make_figures(algtime, alglgtime_highfreq, plotlist=plotlist_phys, xlab=case_xlabs[case_labs[c]], timestep=case_timestep[case_labs[c]] )
    plot_figures(plots, paste0(fname,'_highfreq_physiology','.pdf'), png=png )

    if(grepl('NP|RD|ECA',caseidprefix) | plot_nutrients) {
      plots <- make_figures(algtime_highfreq, plotlist=plotlist_nutrients, xlab=case_xlabs[case_labs[c]], timestep='year' )
      plot_figures(plots, paste0(fname,'_highfreq_nutrients','.pdf'), png=png )
    }
  }
}
}


### END ###
