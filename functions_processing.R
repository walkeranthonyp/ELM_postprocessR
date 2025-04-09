##############################
#
# Functions for processing ELM/CLM netcdf files 
#
# AWalker
# April 2021
#
##############################



##############################
# separates spins and transient cases into a list of sets of cases to concatenate
# -- i.e. each list element contains a vector of spins or transient cases to concatenate
breakout_cases <- function(v, spinss, case_labs ) {
  l <- list(v) 
  if(sum(spinss)==length(spinss)) {
    case_labs <- 'spins'
  } else if(any(spinss)) {
    l <- list(
      v[spinss],
      v[!spinss]
    ) 
  } 

  names(l) <- case_labs
  l
} 


##############################
# conversion functions 

conv_sh_to_rh <- function(q, t, p=101325 ) {
  # convert specific humidity to relative
  # Buck 1981 used for sat vp (as does SDGVM)
  
  # q - water:dry air mass ratio, g/g
  # t - temperature,              K
  # p - pressure,                 Pa
  
  paw  <- p / (0.62198/q + 1)
  paws <- 6.1121 * exp(17.502*(t-273.15) / (240.97 + t - 273.15)) * 100
  #print(c(paw,paws))
  paw/paws * 1000      # units for SDGVM 0.1 % or parts per mil
}

change_unit_label        <- function(v,...)  v                                        # no conversion, just change label
conv_K_to_oC             <- function(v,...) (v - 273.15)                              # converts K to oC
conv_kgpha_to_gpm2       <- function(v,...)  v * 1e-1                                 # kg ha-1 to g m-2   
conv_kgpm2_to_gpm2       <- function(v,...)  v * 1e3                                  # kg m-2 to g m-2   
conv_kgphapd_to_gpm2psec <- function(v,...)  v * 1e-1 / (3600*24)                     # kg ha-1 d-1 to g m-2 s-1   
conv_kgpyear_to_gpsec    <- function(v,...)  v * 1e3 / (3600*24*365)                  # kg yr-1 to g s-1   
conv_psecond_to_pts      <- function(v,tsteps_in_yr,...) v * 3600*24*365/tsteps_in_yr # per second units to the timestep of the data     
conv_pday_to_pts         <- function(v,tsteps_in_yr,...) v * 365/tsteps_in_yr         # per second units to the timestep of the data     


# unit conversion functions and new units
# - elements of list named with units variable in the netcdf file
# - water units in kg (mm) m-2
# - C units in g m-2
var_conv <- list(
  K = list(
    func = conv_K_to_oC,
    newunits = 'oC'
  ),
  `kgC/m2/yr` = list(
    func = conv_kgpyear_to_gpsec,
    newunits = 'gC/m2/s'
  ),
  `kgC ha-1` = list(
    func = conv_kgpha_to_gpm2,
    newunits = 'gC/m2'
  ),
  `kg m-2 s-1` = list(
    func = conv_kgpm2_to_gpm2,
    newunits = 'g/m2/s'
  ),
  `kg m-2` = list(
    func = conv_kgpm2_to_gpm2,
    newunits = 'g/m2'
  ),
  `kgN ha-1` = list(
    func = conv_kgpha_to_gpm2,
    newunits = 'gN/m2'
  ),
  `kgP ha-1` = list(
    func = conv_kgpha_to_gpm2,
    newunits = 'gP/m2'
  ),
  `kgN ha-1 d-1` = list(
    func = conv_kgphapd_to_gpm2psec,
    newunits = 'gN/m2/s'
  ),
  `kgN d-1 ha-1` = list(
    func = conv_kgphapd_to_gpm2psec,
    newunits = 'gN/m2/s'
  ),
  `kgP ha-1 d-1` = list(
    func = conv_kgphapd_to_gpm2psec,
    newunits = 'gP/m2/s'
  ),
  `kgP d-1 ha-1` = list(
    func = conv_kgphapd_to_gpm2psec,
    newunits = 'gP/m2/s'
  ),
  `gN/m^2/s` = list(
    func = change_unit_label, 
    newunits = 'gP/m2/s'
  ),
  `gP/m^2/s` = list(
    func = change_unit_label, 
    newunits = 'gP/m2/s'
  )
)


# time unit conversion functions and new units
# - elements of list named with units variable in the netcdf file
time_conv <- list(
  `gC/m^2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'gC/m2/timestep'
  ), 
  `g/m2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'g/m2/timestep'
  ),
  `gC/m2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'gC/m2/timestep'
  ),
  `gN/m2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'gN/m2/timestep'
  ),
  `gP/m2/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'gP/m2/timestep'
  ), 
  `mm/s` = list(
    func = conv_psecond_to_pts,
    newunits = 'mm/timestep'
  ), 
  `m2 m-2 d-1` = list(
    func = conv_pday_to_pts,
    newunits = 'm2 m-2 timestep-1'
  )
)


##############################
# this summarises an array and preserves names/labelling
# - default takes a single time dimension and summarises by a certain number of timesteps
summarize_array <- function(a1, summarise_dim='time', extent=365, scale=NULL, func=mean ) {
  
  # determine scale vector
  if(is.null(scale)) {
    scale <- rep(1,extent)
  } else if(length(scale)!=extent) {
    stop('sumarise_array: scale argument must be of length extent.')
  }
  
  # summarize array
  keep_dims <- (1:length(dim(a1)))[-which(names(dim(a1))==summarise_dim)] 
  a2 <- apply(a1, keep_dims, function(v) apply(matrix(v,extent)*scale,2,func) )
  
  # update names(dim) and dimnames
  dn <- dimnames(a2)
  names(dim(a2)) <- names(dimnames(a2))
  dimnames(a2)   <- dn
  
  # permute to original dim order
  perm_dims <- match(names(dim(a1)), names(dim(a2)) )
  a2 <- aperm(a2, perm_dims )
  
  a2
}


# change units in an array
# - requires that the last dimension of the array is the variables to convert, labeled and named vars
var_conv_array <- function(a1, var_conv, vars_units, tstep ) {
  
  # convert to matrix - assumes that vars is the last dimension
  adim      <- dim(a1)
  ndim      <- length(adim)
  adimnames <- dimnames(a1)
  if(which(names(adimnames)=='vars')!=ndim) stop('last array dimension not the var dimension')
  m         <- apply(a1, ndim, function(v) v )
  
  # apply conversion functions over second dimension of array
  for(var in adimnames$vars) {
    vars_units[[var]]
    if(!is.null(var_conv[[vars_units[[var]]]])) {
      print(paste('convert var:',var,'from',vars_units[[var]],'to',var_conv[[vars_units[[var]]]]$newunits), quote=F )
      m[,var] <- var_conv[[vars_units[[var]]]]$func(m[,var], tstep ) 
    }
  }
  
  # convert matrix back to array
  array(m, adim, adimnames )
}



### END ###
