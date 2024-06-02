##############################
#
# Functions for plotting processed ELM/CLM netcdf files
#
# AWalker
# April 2021
#
##############################

library(ggplot2)
library(png)


# plot a set of timeseries from a 3D array
plot_3dim <- function(a3d=algtime, vvars=c('GPP','NPP','AR'), xdim='time',
                      vcol=NULL, leg_cols=2, print2screen=T, ... ) {
  lv <- length(vvars)
  lx <- dim(a3d)[xdim]
  if(is.null(vcol)) vcol <- viridis(lv)
  if(lv<leg_cols)   leg_cols <- lv

  p1 <-
    xyplot(a3d[,,vvars] ~ rep(1:lx,lv), groups=rep(vvars,each=lx),
           type='l',
           scales=list(tck=c(-0.5,-0.5)),
           par.settings=simpleTheme(col=vcol,lwd=2),# lwd=2, lty = 1:3 ),
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1,
                         columns=leg_cols, border=T, cex=0.8, background='white' ),
           ... )

  if(print2screen) print(p1)
  p1
}


# plot a ggplot stack timeseries with polygons for each variable / PFT / size class
plot_stack <- function(a3d=algtime,
                       vvars=c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
                       norm=F, xdim='time', ylabi, xlabi, glab=NULL,
                       vcol=NULL, print2screen=T, ... ) {

  pft <- F; scls <- F
  if(any(names(dim(a3d))=='fates_levpft')) {
    pft <- T
    lv  <- dim(a3d)['fates_levpft']
    if(length(vvars)>1) stop('ERROR: plot_stack called with both PFTs and multiple variables.')
  } else if(any(names(dim(a3d))=='fates_levscls')) {
    scls <- T
    lv   <- dim(a3d)['fates_levscls']
    if(length(vvars)>1) stop('ERROR: plot_stack called with both size class and multiple variables.')
  } else {
    lv <- length(vvars)
  }
  if(is.null(vcol)) vcol <- viridis(lv)

  # set x dimension (currently time)
  lx <- dim(a3d)[xdim]

  # stack data
  # - assumes lndgrid is just a single point
  if(pft|scls) {
    as   <- t(a3d[,,,vvars])
    tas  <- as.data.frame(as) # assumes a3d is a 2D array
    if(!is.null(glab)) names(tas) <- glab
    stas <- stack(tas)
  } else {
    as   <- a3d[,,vvars]
    tas  <- as.data.frame(as)    # assumes a3d is a 2D array
    stas <- stack(tas, vvars )
  }

  # normalise data to sum to one
  if(norm) {
    sumas <- apply(as, 1, sum )
    stas$values <- stas$values / sumas
  }

  # make figure object
  p1 <-
    ggplot(stas, aes(x=rep(1:lx,lv), y=values, fill=ind)) +
    geom_area() +
    ylab(ylabi) +
    xlab(xlabi) +
    labs(fill='') +
    theme(legend.position = "top")

  if(print2screen) print(p1)
  p1
}


# plot a set of timeseries from a 3D array and allow variables to be combined according to the arguments passed
plot_3dim_combvars <- function(a3d=algtime, vvars=c('GPP'), sum_vars=c('NPP','AR'),
                               product_vars=NULL,
                               subtract_vars=NULL,
                               div_sumvars=NULL,
                               div_prodvars=NULL,
                               div_zero=1e-2,
                               xdim='time', vcol=NULL, 
                               leg_cols=2, print2screen=T,
                               days=1, ... ) {

  lv <- length(vvars) + !is.null(sum_vars)
  lx <- dim(a3d)[xdim]

  # averaging / summing
  sum_vars.scale <- 1
  if(is.list(sum_vars)) {sum_vars.scale <- sum_vars$scale*1/days; sum_vars <- sum_vars$sum_vars }

  asum    <- apply(a3d[,,sum_vars,drop=F]*sum_vars.scale, 2:1, sum )
  nv_name <- paste0('sum(',paste(sum_vars,collapse=','),')')
  if(!is.null(product_vars)) {
    asum <- apply(a3d[,,product_vars,drop=F], 2:1, prod )
    nv_name <- paste0('prod(',paste(product_vars,collapse=','),')')
  }
  if(!is.null(subtract_vars)) {
    asub <- apply(a3d[,,subtract_vars,drop=F], 2:1, sum )
    asum <- asum[drop=F] - asub[drop=F]
    nv_name <- paste0(nv_name,' - sum(',paste(subtract_vars,collapse=','),')')
  } else if(!is.null(div_sumvars)) {
    adiv <- apply(a3d[,,div_sumvars,drop=F], 2:1, sum )
    adiv[adiv<div_zero] <- NA
    asum <- asum[drop=F] / adiv[drop=F]
    nv_name <- paste0(nv_name,' / sum(',paste(div_sumvars,collapse=','),')')
  } else if(!is.null(div_prodvars)) {
    adiv <- apply(a3d[,,div_prodvars,drop=F], 2:1, prod )
    adiv[adiv<div_zero] <- NA
    asum <- asum[drop=F] / adiv[drop=F]
    nv_name <- paste0(nv_name,' / prod(',paste(div_prodvars,collapse=','),')')
  }


  groups <- c(vvars, nv_name  )
  if(is.null(vcol))           vcol <- viridis(length(groups))
  if(length(groups)<leg_cols) leg_cols <- length(groups)

  dummy_var <- if(!('FATES_TRIMMING'%in%vvars)) 'FATES_TRIMMING' else 'FATES_CROOT_ALLOC'
  a3d[,,dummy_var] <- asum[,]
  dimnames(a3d)[[3]][which(dimnames(a3d)[[3]]==dummy_var)] <- nv_name

  p1 <-
    xyplot(a3d[,,groups] ~ rep(1:lx,lv), groups=rep(groups,each=lx),
           type='l', scales=list(tck=c(-0.5,-0.5)),
           par.settings=simpleTheme(col=vcol,lwd=2),# lty = 1:3 ),
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1,
                         columns=leg_cols, border=T, cex=0.8, background='white' ),
           ... )

  if(print2screen) print(p1)
  p1
}


# plot a set of timeseries from a 4D array
plot_4dim <- function(a4d=alglgtime, vvars=c('H2OSOI'), xdim='time', zdim='levgrnd', dim_sub=NULL,
                      vcol=NULL, leg_cols=3, print2screen=T, ... ) {
  lv <- length(vvars)
  lx <- dim(a4d)[xdim]
  lz <- if(is.null(dim_sub)) dim(a4d)[zdim] else length(dim_sub)
  if(is.null(dim_sub)) dim_sub <- 1:lz
  if(is.null(vcol)) vcol <- viridis(lz)
  if(lz<leg_cols)   leg_cols <- lz

  p1 <-
    xyplot(a4d[,dim_sub,,vvars] ~ rep(1:lx,each=lz), groups=rep(paste(vvars,letters[dim_sub]),lx),
           type='l',
           scales=list(tck=c(-0.5,-0.5)),
           par.settings=simpleTheme(col=vcol,lwd=2),# lwd=2, lty = 1:3 ),
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1,
                         columns=leg_cols, border=T, cex=0.8, background='white' ),
           ... )

  if(print2screen) print(p1)
  p1
}


# check variables (vvars) exist in a 3D/4D array
check_vars <-  function(ar, vvars, vdim ) {
  if(is.list(vvars)) vvars <- vvars[[1]]
  for(v in vvars) {
    vexists <- v%in%dimnames(ar)[[vdim]]
    if(!vexists) stop(paste('ERROR: variable does not exist in dataset:\n', v ))
  }
}


# plot set of figures in a single pdf
# - figures described by a "plotlist" and these can be found in files "input_plotting<>.R"
# - takes a 3D and a 4D array as inputs
make_figures <- function(a3d, a4d, plotlist, xlab='Spin-up', timestep='year', print2screen=F, ... ) {

  lapply(plotlist, function(l) {

    print('',quote=F)
    print('Making figure:',quote=F)
    print(paste('vars:',paste(l$vvars,collapse=',')),quote=F)
    combvars <- F
    if(!is.null(l$sum_vars))      {print(paste('sum vars:',paste(l$sum_vars,collapse='+')),quote=F); combvars <- T }
    if(!is.null(l$subtract_vars)) {print(paste('subtract vars:',paste(l$subtract_vars,collapse='+')),quote=F); combvars <- T }
    if(!is.null(l$div_sumvars))   {print(paste('divide vars:',paste(l$div_sumvars,collapse='+')),quote=F); combvars <- T }
    if(!is.null(l$div_prodvars))  {print(paste('divide vars:',paste(l$div_prodvars,collapse='*')),quote=F); combvars <- T }

    days <- 1
    if(timestep=='year') {
      days <- 365
    }

    if(!is.null(l$stackplot)) {
      ld <- length(dim(a3d))
      check_vars(a3d, l$vvars, ld )
      plot_stack(a3d, vvars=l$vvars, norm=l$norm, ylabi=l$ylab, xlabi=paste(xlab,timestep), print2screen=print2screen, days=days, ... )

    } else if(is.null(l$v4d)) {
      if(combvars) {
        check_vars(a3d, l$vvars, 3 )
        check_vars(a3d, l$sum_vars, 3 )
        check_vars(a3d, l$product_vars, 3 )
        check_vars(a3d, l$subtract_vars, 3 )
        check_vars(a3d, l$div_sumvars, 3 )
        check_vars(a3d, l$div_prodvars, 3 )
        plot_3dim_combvars(a3d, sum_vars=l$sum_vars, product_vars=l$product_vars,
                           subtract_vars=l$subtract_vars,
                           div_sumvars=l$div_sumvars, div_prodvars=l$div_prodvars,
                           vvars=l$vvars, ylab=l$ylab, xlab=paste(xlab,timestep),
                           print2screen=print2screen, days=days )
      } else {
        check_vars(a3d, l$vvars, 3 )
        plot_3dim(a3d, vvars=l$vvars, ylab=l$ylab, xlab=paste(xlab,timestep), print2screen=print2screen, days=days )
      }

    } else {
      if(!is.null(a4d)) {
        check_vars(a4d, l$vvars, 4 )
        plot_4dim(a4d, vvars=l$vvars, dim_sub=l$dim_sub, ylab=l$ylab, xlab=paste(xlab,timestep),
                  print2screen=print2screen, days=days )
      } else {
        warning('a4d argument not given, no 4d plots.')
      }
    }
  })
}


# plot a png object as a png
plotPNG <- function(png) {
  plot(c(0,1),c(0,1),type="n")
  rasterImage(png,0,0,1,1)
}


# take a list of plot objects and make into a pdf of png's
# - reduces size of vector grahics figures with many components
plotlist_png <- function(plotlist, fname='plotlist_png.pdf', ... ) {

  if(!file.exists('plots')) dir.create('plots')
  setwd('plots')

  # plot as png's
  png('list-test%03d.png', width=960, height=480 )
  #eval(plotlist)
  lapply(plotlist, print )
  dev.off()

  # read png's to list
  plotlist_png <- lapply(1:length(plotlist), function(i) readPNG(paste0("list-test",formatC(i, width=3, format="d", flag="0"),".png")) )

  # write png's to single pdf
  setwd('..')
  pdf(fname, width=8, height=4 )
  par(mai=c(0,0,0,0))
  dummy <- lapply(plotlist_png, plotPNG )
  dev.off()
}


# write to pdf a list of plot objects as either vector graphics or raster graphics (png=F/T)
plot_figures <- function(plots, plotname='plots.pdf', nper_page=2, png=F ) {

  print('', quote=F )
  if(png) plotname <- paste0(strsplit(plotname,'\\.')[[1]][1], '_png.pdf')
  print(paste('Saving figures to:',plotname), quote=F )

  if(png) {
    plotlist_png(plots, plotname )

  } else {
    print(plotname)
    print (length(plots))
    print(nper_page)
    pdf(plotname, width=9, height=7 )
    lapply(1:length(plots), function(p) {
      print(plots[p],
            split=c(1,p%%nper_page+if(p%%nper_page==0) nper_page else 0,1,nper_page),
            more=p%%nper_page )
      numeric(0)
    })
    dev.off()

  }
}



### END ###
