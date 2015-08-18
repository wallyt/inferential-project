## -------------------------- ##
## An R Function that Shades  ##
## under a normal density.    ##
##                            ##
## This is a convenience      ##
## function for polygon()     ##
## -------------------------- ##

shadenorm = function(below=NULL, above=NULL, pcts = c(0.025,0.975), mu=0, sig=1, numpts = 500, color = "gray", dens = 40,
                     lines=FALSE,between=NULL,outside=NULL){
    if(is.null(between)){
        bothnull = is.null(below) & is.null(above)
        if(bothnull==TRUE){
            below = ifelse(is.null(below), qnorm(pcts[1],mu,sig), below)
            above = ifelse(is.null(above), qnorm(pcts[2],mu,sig), above)
        }
    }
    
    if(is.null(outside)==FALSE){
        if(is.numeric(outside)==FALSE){if(outside==TRUE){outside=qnorm(pcts,mu,sig)}}
        below = min(outside)
        above = max(outside)
    }
    
    lowlim = mu - 4*sig
    uplim  = mu + 4*sig
    
    x.grid = seq(lowlim,uplim, length= numpts)
    dens.all = dnorm(x.grid,mean=mu, sd = sig)
    
    if(lines==FALSE){
        plot(x.grid, dens.all, type="l", xlab="X", ylab="Density")
    }
    
    if(lines==TRUE){
        lines(x.grid,dens.all)
    }
    
    if(is.null(below)==FALSE){
        x.below    = x.grid[x.grid<below]
        dens.below = dens.all[x.grid<below]
        polygon(c(x.below,rev(x.below)),c(rep(0,length(x.below)),rev(dens.below)),col=color,density=dens)
    }
    
    if(is.null(above)==FALSE){
        x.above    = x.grid[x.grid>above]
        dens.above = dens.all[x.grid>above]
        polygon(c(x.above,rev(x.above)),c(rep(0,length(x.above)),rev(dens.above)),col=color,density=dens)
    }
    
    
    
    if(is.null(between)==FALSE){
        if(is.numeric(between)==FALSE){if(between==TRUE){between=qnorm(pcts,mu,sig)}}
        from = min(between)
        to   = max(between)
        
        x.between    = x.grid[x.grid>from&x.grid<to]
        dens.between = dens.all[x.grid>from&x.grid<to]
        polygon(c(x.between,rev(x.between)),c(rep(0,length(x.between)),rev(dens.between)),col=color,density=dens)
    }
}