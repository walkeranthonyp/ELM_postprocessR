##############################
#
# Plotting lists - each list describes a separate plotting pdf
#
# AWalker
# April 2021
#
##############################

# plotting lists
# - each element in these plotlist lists describe a separate figure when passed to the make_figures function
# - naming of each element in the highest level of the lists is arbitrary
# - naming of the second level in the list is specific and is used for arguments in the functions in plotting_functions.R

# list of base plots
plotlist <- list(
  p0.1 = list(
    vvars = c('TBOT'),
    ylab  = expression('Air Temperature ['^o*'C]')
  ),
  p0.2 = list(
    vvars = c('RAIN'),
    ylab  = expression('Rain [mm]')
  ),
  p0.3 = list(
    vvars = c('FSDS'),
    ylab  = expression('Incoming short-wave radiation ['*W*m^-2*']')
  ),
  p0.4 = list(
    vvars = c('QBOT'),
    ylab  = expression('Air specific huumidity ['*kg*kg^-1*']')
  ),
  p0.4 = list(
    vvars = c('WIND'),
    ylab  = expression('Wind Speed ['*m*s^-1*']')
  ),
  p0.4 = list(
    vvars = c('PCO2'),
    ylab  = expression('Air '*CO[2]*' partial pressure [Pa]')
  ),
  p0 = list(
    vvars = c('FATES_GPP','FATES_NPP','FATES_AUTORESP','FATES_EXCESS_RESP'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0.5 = list(
    vvars = c('NEE'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0.6 = list(
    vvars = c('FATES_HET_RESP','FATES_MAINT_RESP','FATES_GROWTH_RESP','FATES_AUTORESP','FATES_EXCESS_RESP'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
#  ),
#  p0.7 = list(
#    vvars = c('FATES_L2FR'),
#    ylab  = expression('leaf to fineroot biomass multiplier for target allometry ['*kg*kg^-1*']')
  ),
  p1 = list(
    vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('FATES_STRUCTC','FATES_NONSTRUCTC','FATES_FROOTC','FATES_VEGC','FATES_VEGC_ABOVEGROUND','FATES_LEAFC','FATES_SAPWOODC','FATES_STOREC','FATES_REPROC'),
    ylab  = expression('Veg C Pool [gC '*m^-2*']')
  ),
  p2.3 = list(
    vvars = c('SOILC'),
    ylab  = expression('Soil C Pool [gC '*m^-2*']')
  ),
  p3 = list(
    vvars = c('RAIN','QRUNOFF','QDRAI','QVEGT','QVEGE','QSOIL'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p4 = list(
    vvars = c('SOILWATER_10CM'),
    ylab  = 'Soil Water Content [%]'
  ),
  p5 = list(
    vvars = c('BTRAN'),
    ylab  = 'Plant water status [0-1]'
  ),
  p6 = list(
    vvars = c('TLAI'),
    ylab  = expression('LAI ['*m^2*m^-2*']')
  ),
  p7 = list(
    vvars = c('FATES_PROMOTION_CARBONFLUX','FATES_DEMOTION_CARBONFLUX','FATES_MORTALITY_CFLUX_CANOPY','FATES_MORTALITY_CFLUX_USTORY'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p7.6 = list(
    vvars = c('MORTALITY_CROWNAREA_CANOPY','MORTALITY_CROWNAREA_UNDERSTORY'),
    ylab  = expression('Crown area ['*m^2*']')
  ),
  p7.5 = list(
    vvars = c('FATES_CA_WEIGHTED_HEIGHT','FATES_BA_WEIGHTED_HEIGHT'),
    ylab  = 'Mean canopy height [m]'
  ),
#  p8 = list(
#    vvars = c('FATES_DISTURBANCE_RATE_FIRE','FATES_DISTURBANCE_RATE_LOGGING','FATES_DISTURBANCE_RATE_P2P','FATES_DISTURBANCE_RATE_P2S',
#              'FATES_DISTURBANCE_RATE_POTENTIAL','FATES_DISTURBANCE_RATE_S2S','FATES_DISTURBANCE_RATE_TREEFALL'),
#    ylab  = 'Fraction of ground area disturbed [0-1]'
#  ),
  p9 = list(
    vvars = c('FATES_NCOHORTS','FATES_NPATCHES'),
    ylab  = 'FATES diagnostics [#]'
  ),
  p1.1 = list(
    stackplot = T,
    norm  = F,
    vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p1.2 = list(
    stackplot = T,
    norm  = T,
    vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p2.1 = list(
    stackplot = T,
    norm  = F,
    vvars = c('FATES_STRUCTC','FATES_FROOTC','FATES_LEAFC','FATES_SAPWOODC','FATES_STOREC','FATES_REPROC'),
    ylab  = expression('Veg C Pool [gC '*m^-2*']')
  ),
  p2.2 = list(
    stackplot = T,
    norm  = T,
    vvars = c('FATES_STRUCTC','FATES_FROOTC','FATES_LEAFC','FATES_SAPWOODC','FATES_STOREC','FATES_REPROC'),
    ylab  = expression('Veg C Pool [gC '*m^-2*']')
  ),
  p3 = list(
    vvars = 'FATES_UNGERM_SEED_BANK',
    ylab  = expression('Seed bank [g C '*m^-2*']')
  ),
  p3.05 = list(
    vvars = 'FATES_SEEDLING_POOL',
    ylab  = expression('Seedling pool [g C '*m^-2*']')
  )
)


# list of plots for nutrient-related variables
plotlist_nutrients <- list(
  p1 = list(
    vvars = c('FATES_NONSTRUCTC','FATES_VEGC'),
    ylab  = expression('C Pool [gC '*m^-2*']')
  ),
  p2 = list(
    vvars = c('FATES_VEGN','FATES_SAPWOODN','FATES_LEAFN','FATES_FROOTN'),# 'SOILN'),
    ylab  = expression('N Pool [gN '*m^-2*']')
  ),
  p3 = list(
    vvars = c('FATES_VEGP','FATES_SAPWOODP','FATES_LEAFP','FATES_FROOTP'),# 'SOILP'),
    ylab  = expression('P Pool [gP '*m^-2*']')
  ),
  p4 = list(
    vvars = c('FATES_VMAXNH4','FATES_VMAXNO3')
    ylab  = expression('N Vmax [gN '*m^-2*' timestep'^-1*']')
  ),
  p4.1 = list(
    vvars = c('FATES_NO3UPTAKE','FATES_NH4UPTAKE','FATES_NDEMAND','FATES_NFIX_SYM','FATES_NEFFLUX'),
    ylab  = expression('N Flux [gN '*m^-2*' timestep'^-1*']')
  ),

  p5 = list(
    vvars = c('FATES_VMAXPO4')
    ylab  = expression('P Vmax [gP '*m^-2*' timestep'^-1*']')
  ),
  p5.1 = list(
    vvars = c('FATES_FROOTMAINTAR_NETVMAX')
    ylab  = expression('Respiration due to Vmax variation [gC '*m^-2*' timestep'^-1*']')
  ),
  p5.2 = list(
    vvars = c('FATES_PUPTAKE','FATES_PDEMAND','FATES_PEFFLUX'),
    ylab  = expression('P Flux [gP '*m^-2*' timestep'^-1*']')
  ),
  p6 = list(
    vvars = c('GROSS_NMIN','NET_NMIN'),
    ylab  = expression('N Flux [gN '*m^-2*' timestep'^-1*']')
  ),
  p7 = list(
    vvars = c('BIOCHEM_PMIN','BIOCHEM_PMIN_TO_PLANT','GROSS_PMIN','NET_PMIN'),
    ylab  = expression('P Flux [gP '*m^-2*' timestep'^-1*']')
  ),
  px = list(
    vvars = c('FATES_STOREC','FATES_STOREN','FATES_STOREP'),
    ylab  = expression('C/N/P Store [g '*m^-2*']')
  ),
  px.1 = list(
    vvars = c('FATES_STOREN','FATES_STOREP'),
    ylab  = expression('N/P Store [g '*m^-2*']')
  ),
  px.2 = list(
    vvars = c('FATES_STOREN_TF','FATES_STOREP_TF'),
    ylab  = expression('N/P Store Target [g '*m^-2*']')
  ),
  px.3 = list(
    vvars = c('FATES_STOREC_TF','FATES_STOREN_TF','FATES_STOREP_TF'),
    ylab  = expression('C/N/P Store Target [g '*m^-2*']')
#  ),
#  px.4 = list(
#    vvars = c('FATES_L2FR'),
#    ylab  = expression('leaf to fineroot biomass multiplier for target allometry ['*kg*kg^-1*']')
  )
)


# list of plots for physiology-related variables
plotlist_phys <- list(
  p0 = list(
    vvars = c('FATES_GPP','FATES_AUTORESP'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0b = list(
    vvars = c('FATES_MAINT_RESP','FATES_GROWTH_RESP','FATES_EXCESS_RESP'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0a = list(
    vvars = c('QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('TLAI','FATES_TRIMMING'),
    sum_vars='TLAI',
    div_sumvars='FATES_TRIMMING',
    ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]')
  ),
  p1c = list(
    vvars = NULL,
    ylab  = expression('veg WUE [g.'*kg^-1*']'),
    sum_vars = c('FATES_GPP'),
    div_sumvars = c('QVEGT')
  ),
  p1d = list(
    vvars = NULL,
    ylab  = expression('ecosystem WUE [g.'*kg^-1*']'),
    sum_vars = c('FATES_GPP'),
    div_sumvars = c('QVEGT','QVEGE','QSOIL')
  ),
  p1.1 = list(
    vvars = NULL,
    ylab  = expression('iWUE GPP/'*g[c]*' ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']'),
    sum_vars = list(
      sum_vars=c('FATES_GPP'),
      scale=1e12/(86400*12) # convert g C umol-1 H2O to umol C mol-1 H2O
      ),
    div_prodvars = c('FATES_STOMATAL_COND','TLAI')
  ),
  p1a = list(
    vvars = c('FATES_STOMATAL_COND'),
    sum_vars='FATES_GPP',
    product_vars=c('FATES_STOMATAL_COND','TLAI'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1b = list(
    vvars = c('FATES_LBLAYER_COND'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1 = list(
    vvars = c('BTRAN'),
    ylab  = expression('Plant water status [0-1]')
  ),
  p3 = list(
    vvars = c('RAIN','QVEGT','QVEGE','QSOIL'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']'),
    sum_vars = c('QRUNOFF','QDRAI')
  ),
  p4 = list(
    vvars   = c('H2OSOI'),
    v4d     = T,
    dim_sub = 1:10,
    ylab    = 'Soil Water Content [%]'
  ),
  p5 = list(
    vvars = c('SOILPSI'),
    v4d     = T,
    dim_sub = 1:10,
    ylab    = expression('Soil Water Potential, '*Psi*' [MPa]')
  ),
  p6 = list(
    vvars = c('SOILPSI'),
    v4d     = T,
    dim_sub = 3:10,
    ylab    = expression('Soil Water Potential, '*Psi*' [MPa]')
  )
)


# list of plots differentiated by PFT
plotlist_pft <- list(
  p1 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_VEGC_PF',
    ylab  = expression('Veg C [gC '*m^-2*']')
  ),
  p1.1 = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_VEGC_PF',
    ylab  = expression('Veg C proportion [-]')
  ),
  p2.1 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_CROWNAREA_PF',
    ylab  = expression('Crown area proportion [-]')
  ),
  p2.2 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_CANOPYCROWNAREA_PF',
    ylab  = expression('Crown area proportion [-]')
  ),
  p2.3 = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_CANOPYCROWNAREA_PF',
    ylab  = expression('Crown area proportion [-]')
  ),
  p3 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_UNGERM_SEED_BANK_PF',
    ylab  = expression('Seed bank [g C '*m^-2*']')
  ),
  p3.05 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_SEEDLING_POOL_PF',
    ylab  = expression('Seedling pool [g C '*m^-2*']')
  ),
  p3.1 = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_MORTALITY_PF',
    ylab  = expression('Mortality individuals proportion [-]')
  ),
  p4.1 = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_MORTALITY_CFLUX_PF',
    ylab  = expression('Mortality mass proportion [-]')
  ),
  p1.1 = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_NPP_PF',
    ylab  = expression('NPP proportion [-]')
  ),
  p1.1 = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_RECRUITMENT_PF',
    ylab  = expression('RECRUITMENT proportion [-]')
  )
)


# list of plots differentiated by size-class
plotlist_size_class <- list(
  p1 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_VEGC_ABOVEGROUND_SZ',
    ylab  = expression('Veg C Aboveground [gC '*m^-2*']')
  ),
  p1a = list(
    stackplot = T,
    norm  = T,
    vvars = 'FATES_VEGC_ABOVEGROUND_SZ',
    ylab  = expression('Veg C Aboveground proportion [-]')
  ),
  p2 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_BASALAREA_SZ',
    ylab  = expression('Basal Area ['*m^2*m^-2*']')
  ),
  p3 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_NPLANT_SZ',
    ylab  = expression('Stand Density [# '*m^-2*']')
  ),
#  p3.1 = list(
#    stackplot = T,
#    norm  = F,
#    vvars = 'FATES_NPLANT_AC',
#    ylab  = expression('Plant number by age class [# '*m^-2*']')
#  ),
  p4 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_DDBH_CANOPY_SZ',
    ylab  = expression('Canopy DBH growth [m '*m^-2*yr^-1*']')
  ),
  p5 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_DDBH_USTORY_SZ',
    ylab  = expression('Understorey DBH growth [m '*m^-2*yr^-1*']')
  ),
  p6 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_LAI_CANOPY_SZ',
    ylab  = expression('Canopy LAI ['*m^2*m^-2*']')
  ),
  p7 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_LAI_USTORY_SZ',
    ylab  = expression('Understorey LAI ['*m^2*m^-2*']')
  ),
  p8 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_CANOPY_SZ',
    ylab  = expression('Canopy Mortality [# '*m^-2*yr^-1*']')
  ),
  p9 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_USTORY_SZ',
    ylab  = expression('Understorey Mortality [# '*m^-2*yr^-1*']')
  ),
  p10 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_BACKGROUND_SZ',
    ylab  = expression('Background Mortality [# '*m^-2*yr^-1*']')
  ),
  p11 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_TERMINATION_SZ',
    ylab  = expression('Termination Mortality [# '*m^-2*yr^-1*']')
  ),
  p12 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_IMPACT_SZ',
    ylab  = expression('Impact Mortality [# '*m^-2*yr^-1*']')
  ),
  p13 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_CSTARV_SZ',
    ylab  = expression('C Starvation Mortality [# '*m^-2*yr^-1*']')
  ),
  p14 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_HYDRAULIC_SZ',
    ylab  = expression('Hydraulic Mortality [# '*m^-2*yr^-1*']')
  ),
  p15 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_FIRE_SZ',
    ylab  = expression('Fire Mortality [# '*m^-2*yr^-1*']')
  ),
  p16 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_FREEZING_SZ',
    ylab  = expression('Freeze Mortality [# '*m^-2*yr^-1*']')
  ),
  p17 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_LOGGING_SZ',
    ylab  = expression('Logging Mortality [# '*m^-2*yr^-1*']')
  ),
  p18 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_SENESCENCE_SZ',
    ylab  = expression('Senescence Mortality [# '*m^-2*yr^-1*']')
  ),
  p19 = list(
    stackplot = T,
    norm  = F,
    vvars = 'FATES_MORTALITY_AGESCEN_SZ',
    ylab  = expression('Age Senescence Mortality [# '*m^-2*yr^-1*']')
  )
)



### END ###
