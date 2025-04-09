##############################
#
# Plotting lists - each list describes a separate plotting pdf
#
# AWalker
# April 2021
#
##############################

# plotting lists
# - each element in these plotlist lists result describe a separate figure when passed to the make_figures function 

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
  p0 = list(
    vvars = c('GPP','NPP','AR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p1 = list(
    vvars = c('NPP_CROOT','NPP_FROOT','NPP_LEAF','NPP_SEED','NPP_STEM','NPP_STOR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('ED_balive','ED_bdead','ED_bfineroot','ED_biomass','ED_bleaf','ED_bsapwood','ED_bstore'),
    ylab  = expression('Veg C Pool [gC '*m^-2*']')
  ),
  p2.1 = list(
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
    vvars = c('DEMOTION_CARBONFLUX','MORTALITY_CARBONFLUX_CANOPY','MORTALITY_CARBONFLUX_UNDERSTORY'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p8 = list(
    vvars = c('DISTURBANCE_RATE_FIRE','DISTURBANCE_RATE_LOGGING','DISTURBANCE_RATE_P2P','DISTURBANCE_RATE_P2S',
              'DISTURBANCE_RATE_POTENTIAL','DISTURBANCE_RATE_S2S','DISTURBANCE_RATE_TREEFALL'),
    ylab  = 'Fraction of ground area disturbed [0-1]'  
  ),
  p9 = list(
    vvars = c('ED_NCOHORTS','ED_NPATCHES'),
    ylab  = 'FATES diagnostics [#]'
  )
)


plotlist_nutrients <- list(
  p1 = list(
    vvars = c('ED_balive','ED_biomass'),
    ylab  = expression('C Pool [gC '*m^-2*']')
  ),
  p2 = list(
    vvars = c('TOTVEGN','SAPWN','LEAFN','FNRTN'),#'SOILN'),
    ylab  = expression('N Pool [gN '*m^-2*']')
  ),
  p3 = list(
    vvars = c('TOTVEGP','SAPWP','LEAFP','FNRTP'),#'SOILP'),
    ylab  = expression('P Pool [gP '*m^-2*']')
  ),
  p4 = list(
    #vvars = c('NUPTAKE','NNEED','NEFFLUX'),
    vvars = c('NO3UPTAKE','NH4UPTAKE','NNEED','NEFFLUX'),
    ylab  = expression('N Flux [gN '*m^-2*' timestep'^-1*']')
  ),
  p5 = list(
    vvars = c('PUPTAKE','PNEED','PEFFLUX'),
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
    vvars = c('ED_bstore','STOREN','STOREP'),
    ylab  = expression('C/N/P Store [g '*m^-2*']')
  ),
  px.1 = list(
    vvars = c('STOREN','STOREP'),
    ylab  = expression('N/P Store [g '*m^-2*']')
  )
)


plotlist_phys <- list(
  p0 = list(
    vvars = c('GPP','AR'),
    ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']')
  ),
  p0a = list(
    vvars = c('QVEGT','QVEGE'),
    ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']')
  ),
  p2 = list(
    vvars = c('TLAI','TRIMMING'),
    sum_vars='TLAI',
    div_sumvars='TRIMMING',
    ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]')
  ),
  p1c = list(
    vvars = NULL,
    ylab  = expression('veg WUE [g.'*kg^-1*']'),
    sum_vars = c('GPP'),
    div_sumvars = c('QVEGT')
  ),
  p1d = list(
    vvars = NULL,
    ylab  = expression('ecosystem WUE [g.'*kg^-1*']'),
    sum_vars = c('GPP'),
    div_sumvars = c('QVEGT','QVEGE','QSOIL')
  ),
  p1.1 = list(
    vvars = NULL,
    ylab  = expression('iWUE GPP/'*g[c]*' ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']'),
    sum_vars = list(
      sum_vars=c('GPP'),
      scale=1e12/(86400*12) # convert g C umol-1 H2O to umol C mol-1 H2O
      ),
    div_prodvars = c('C_STOMATA','TLAI')
  ),
  p1a = list(
    vvars = c('C_STOMATA'),
    sum_vars='GPP',
    product_vars=c('C_STOMATA','TLAI'),
    ylab  = expression(g['s,c']*' ['*mu*mol*' '*H[2]*O*' '*m^-2*' s'^-1*']')
  ),
  p1b = list(
    vvars = c('C_LBLAYER'),
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
    dim_sub = 2:10,
    ylab    = expression('Soil Water Potential, '*Psi*' [MPa]')
  )
)



### END ###
