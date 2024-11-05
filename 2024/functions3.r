require (chron)
require (lattice)
require (MASS)
require (doBy)
require (car)
require (effects)
require (RODBC)
library  (nlme)
require (stats)
require(reshape)
#require(NCStats)
require(epiDisplay)
require(Hmisc)
 require(gdata)
#----------------------------------------------------
sort.data.frame.decrease <- function(x, key, ...) {
    if (missing(key)) {
        rn <- rownames(x)
        if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
        x[order(rn, ...), , drop=FALSE]
    } else {
        x[do.call("order", c(x[key], decreasing=TRUE, ...)), , drop=FALSE]
    }
}

# ---------------------------------
sort.data.frame.increase <- function(x, key, ...) {
    if (missing(key)) {
        rn <- rownames(x)
        if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
        x[order(rn, ...), , drop=FALSE]
    } else {
        x[do.call("order", c(x[key], ...)), , drop=FALSE]
    }
}

# ---------------------------------
compute.sums = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index],
  	FUN=function(q) { sum(q, na.rm=T)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}


  # --------------------------
compute.means = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index],
  	FUN=function(q) { mean(q, na.rm=T)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}
  # --------------------------
compute.median = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index],
  	FUN=function(q) { median(q, na.rm=T)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}

  # --------------------------

compute.sd = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index], 
  	FUN=function(q) { sd(q, na.rm=T)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}

  # --------------------------

compute.lengths = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index], 
  	FUN=function(q) { length(q)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}
# --------------------------
compute.unique.lengths = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index], 
  	FUN=function(q) { length(unique(q))}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}
  # --------------------------

compute.max = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index], 
  	FUN=function(q) { max(q, na.rm=T)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}

# --------------------------
compute.min = function (x, var, index) {
  res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index], 
  	FUN=function(q) { min(q, na.rm=T)}, simplify=T))
  for (i in index) { res[i,] = as.integer( res[i,] ) }
  return(res)
}

# --------------------------
rename.df = function(x, n0, n1) {
    names(x)[which(names(x)==n0)] = n1
    return(x)
  }

# --------------------------

 factor2num= function(x, vars) {
    for (i in vars) x[,i] = as.numeric(as.character(x[,i]))
    return(x)
  }

  # --------------------------

  plot.lines.2 = function( x, xvar, yvar, catvar, categories, points=F, title="" ) {
  x[,xvar] = as.numeric( as.character( x[,xvar] ))
  x[,yvar] = as.numeric( as.character( x[,yvar] ))
  windows()
  par(oma=c(1,1,1,3))
  plot( x[, xvar], x[, yvar], type="n", main=title, xlab=xvar, ylab="Landings (mt) Areas 1-4", 
  las=1, cex.main=0.9, cex.axis=0.8, cex.lab=0.8)
  ncat = length(categories)
  for (j in 1:ncat) {
    subset = which (x[, catvar] == categories[j] )
    lines( x[subset, xvar], x[subset, yvar], lty=j  )
    if (points)  points( x[subset, xvar], x[subset, yvar], pch=j  )
  }
  legend( x="topright", legend=categories, lty=1:ncat, pch=1:ncat, title=catvar, cex=0.8, bty="n" )
}

 # --------------------------   
  plot.lines = function( x, xvar, yvar, catvar, categories, points=F, title="" ) {
  x[,xvar] = as.numeric( as.character( x[,xvar] ))
  x[,yvar] = as.numeric( as.character( x[,yvar] ))
  windows()
  plot( x[, xvar], x[, yvar], type="n", main=title, xlab=xvar, ylab=yvar, ylim=c(0,2500000)  )
  ncat = length(categories)
  for (j in 1:ncat) {
    subset = which (x[, catvar] == categories[j] )
    lines( x[subset, xvar], x[subset, yvar], lty=j  )
    if (points)  points( x[subset, xvar], x[subset, yvar], pch=j, col=j  )
  }
  
  legend( x="topleft", legend=categories, lty=1:ncat, pch=1:ncat, title=catvar, col=1:ncat )  
}
# ------------------------------------ 

plot.bars = function( x, xvar, yvar, catvar, categories, points=F, title="" ) {
  x[,xvar] = as.numeric( as.character( x[,xvar] ))
  x[,yvar] = as.numeric( as.character( x[,yvar] ))
  windows()
  barplot( x[, xvar], x[, yvar], type="n", main=title, xlab=xvar, ylab=yvar  )
  ncat = length(categories)
  for (j in 1:ncat) {
    subset = which (x[, catvar] == categories[j] )
    lines( x[subset, xvar], x[subset, yvar], lty=j  )
    if (points)  points( x[subset, xvar], x[subset, yvar], pch=j  )
  }
  
  legend( x="center", legend=categories, lty=1:ncat, pch=1:ncat, title=catvar )  
}
# ------------------------------------ 
# error bars easy
superpose.eb <-
function (x, y, ebl, ebu = ebl, length = 0.08, ...)
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
    length = length, ...)
# ------------------------------------
prepanel.ci <- function(x, y, ly, uy, subscripts, ...) {
    x <- as.numeric(x)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    list(ylim = range(y, uy, ly, finite = TRUE)) }

panel.ci <- function(x, y, ly, uy, subscripts, pch = 16, ...) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    panel.arrows(x, ly, x, uy, col = "black",
                 length = 0.25, unit = "native",
                 angle = 90, code = 3)
    panel.xyplot(x, y, pch = 16, ...)}
#----------------------------------------------------------------------------    
## from Benoit
#filepath <- file.choose()   ## choose a file that is in the folder of interest
#
#dirpath = dirname(filepath) ## get the directory path of the selected file
#
### list all the csv files in that directory
#all.files <- list.files(path=dirpath,full.names=TRUE,pattern=".csv")
#n.files <- length(all.files)
#
### If needed ,use this kind of code to extract the file names only
#file name = substr(dirpath, nchar(dirpath)-22, nchar(dirpath)-4)
#
### Opens all the files in the folder
### you can use something else than "n.files" to name the files
#
#for (i in n.files) {
#name=paste("data",i,sep="_")
#assign(name, read.csv( file=all.files[i]))
#}
#----------------------------------------------------------------------------   
extract.summary.data.for.one.variable = function( x, y ) {
 # extract various (unadjusted) summary stats: medians, quantiles,
#etc
 d0 = tapply(X=x, INDEX=y, FUN=mean, na.rm=T, simplify=T)
 d1 = tapply(X=x, INDEX=y, FUN=sd, na.rm=T, simplify=T)
 d2 = tapply(X=x, INDEX=y, FUN=function(x) length(x[is.finite(x)]),
simplify=T)
 d3 = tapply(X=x, INDEX=y, FUN=median, na.rm=T, simplify=T)
 d4 = tapply(X=x, INDEX=y, FUN=function(x) quantile(x, probs=0.25,
na.rm=T), simplify=T)
 d5 = tapply(X=x, INDEX=y, FUN=function(x) quantile(x, probs=0.75,
na.rm=T), simplify=T)
 out = rbind( mean=exp(d0)-1, sd=exp(d1), n=d2, median=exp(d3)-1,
q25=exp(d4)-1, q75=exp(d5)-1 )
 print (out)
 return (out)
}
#-------------------------------------------------------------------------------
get.effects.from.model = function( lm.model  ) {

    model.statement = formula( lm.model )
    all.variables = all.vars(model.statement)
    
    if (length( all.variables ) < 2) {
      print (" No results " )
      return ()
    }

    dependent.var = all.variables[1] 
    all.terms = attr( terms( model.statement), "term.labels" )
    

    # general stats (unadjusted medians/quantiles) 
    # for main effects that are significant
    
    # unadjusted effects 
    lm.data = lm.model$model  # internal copy of the data table
    results.unadjusted = list()
    for ( effect.variable in all.terms ) {
      print("---")
      print ( paste( effect.variable, " -- unadjusted: ") )
      results.unadjusted[[effect.variable]] =
extract.summary.data.for.one.variable( lm.data[,1],lm.data[,effect.variable])
} 
    lm.data$alldata = 1
    results.unadjusted[["alldata"]] =
extract.summary.data.for.one.variable( lm.data[,1], lm.data[,"alldata"]) 
results.adjusted = NULL
    for ( effect.variable in all.terms ) {
      print( paste( effect.variable, "..." ) )
      res = try( effect( effect.variable, lm.model ), silent=T )
      if (class(res)=="try-error") {
        print( "There was a problem extracting the effects from:" )
        print( effect.variable  )
        print( "And here is the error message:")
        print( res )
        print( "Here is the data and a summary of the data:")
        print( lm.model$model[[effect.variable]] )
        print( summary(lm.model$model[[effect.variable]]) )
        print( "try combining some levels? ..." )
        plot( lm.model$model[[effect.variable]]  ) 
      } else {
      med = as.vector(summary(res, typical="median")$effect)
      res.summary = summary(res)
      res.effect = res.summary$effect
      m = expand.grid(dimnames(res.effect))  
      if (!is.na(dim(res.effect)[2])) {
        n = paste(as.character(m[,1]), as.character(m[,2]), sep=".")
      } else {
        n = as.character(m[,1])
      }
      
     
      results.adjusted = rbind(results.adjusted, 
        cbind(dependent.var, effect.variable, n , exp(med)-1, 
          exp(as.vector(res.summary$effect))-1, 
          exp(as.vector(res.summary$lower))-1, 
          exp(as.vector(res.summary$upper))-1  ) )

filename = paste("adjusted.means", dependent.var, effect.variable,"png", sep=".") 
filename = gsub(":", "__", filename, fixed=T) 
      png( filename=filename )
       plot(effect(effect.variable, lm.model ))
      dev.off()

#      win.metafile(filename = graphfile, width = 7, height = 5)
#      plot(effect(effect.variable, lm.model ))
#      dev.off() 
     }
    }
    rownames(results.adjusted) = NULL
    colnames(results.adjusted) = c("interval", "effect", "level", "median", "mean", "95%CI.lower", "95%CI.upper")
    results.adjusted = as.data.frame(results.adjusted)
    for (i in 4:6) results.adjusted[,i] = as.numeric(as.character(results.adjusted[,i])) 
    print(" *.PNG Figures have been saved in your work directory" )
    
    return(list( results.unadjusted=results.unadjusted,
results.adjusted=results.adjusted)) 

}

# function to send results to Excel clipboard so they can be pasted easily 
clipboard = function (results) {
  write.table(results, "clipboard", sep="\t")
  return ("Now you can paste the table into excel")
}

    

######################################################
## Cool graphics, see volcano dataset
##
#wireframe(volcano, shade = TRUE,
#          aspect = c(61/87, 0.4),
#     light.source = c(10,0,10))
##
#x11()
#require(grDevices); require(graphics)
#filled.contour(volcano, color.palette = terrain.colors, asp = 1)
#title(main = "volcano data: filled contour map")
###
##