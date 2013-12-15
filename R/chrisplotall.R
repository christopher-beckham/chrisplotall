#' Private function
#' @param data
#' @return null
init <- function(data) {
  quantitatives <- {}
  factors <- {}
  test <- sapply(data,class)
  for( i in 1:length(test)) {
    if( test[i] == "integer" || test[i] == "numeric") {
      quantitatives <- append(quantitatives, names(test[i]))
    }
    else if(test[i] == "factor" || test[i] == "logical") {
      factors <- append(factors, names(test[i]))
    }
  }
  return( list(quantitatives, factors) )
}

#' Create folder. Will delete folder if it exists before creating new one.
#' @param str folder name
createfolder <- function(str) {
  unlink(str,recursive=TRUE)
  write(paste("Creating directory called:",str), stdout())
  dir.create(str)
}

#' Create scatterplots out of all possible quantitative variables in the data.
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
scatterall <- function(data, no.export=TRUE, ...) {
  foldername = paste("scatterall_",substitute(loansData),sep="")
  createfolder(foldername)
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  # simple 2d scatterplots
  for( i in 1:length(quantitatives)) {
    for( j in i:length(quantitatives)) {
      if( i != j) {
        if(no.export == FALSE) {
          png(filename=paste(foldername,"/","scatter_",paste(i),"_",
                             paste(j),".png",sep=""))
        }
        plot( x=get(quantitatives[i],pos=data),
              y=get(quantitatives[j],pos=data),
              xlab=quantitatives[i],
              ylab=quantitatives[j],
              ...)
        if(no.export == FALSE) {
          dev.off()
        }
      }
    }
  }
  write("Done!",stdout())
}

#' Create scatterplots out of all possible quantitative variables in the data using factors to colour the points.
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
scatterall3d <- function(data, no.export=TRUE, ...) {
  foldername = paste("scatterall3d_",substitute(loansData),sep="")
  createfolder(foldername)
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  for( i in 1:length(quantitatives)) {
    for( j in i:length(quantitatives)) {
      for( k in 1:length(factors)) {
        if( i != j) {
          if(no.export == FALSE) {
            png(filename=paste(foldername,"/","scatter3d_",paste(i),
                               "_",paste(j),".png",sep=""))
          }
          plot( x=get(quantitatives[i],pos=data),
                y=get(quantitatives[j],pos=data),
                xlab=quantitatives[i],
                ylab=quantitatives[j],
                col=get(factors[k],pos=data),
                ...)
          if(no.export == FALSE) {
            dev.off()
          }
        }
      }
    }
  }
  write("Done!",stdout())
}

#' Create histograms out of all possible quantitative variables in the data.
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
histoall <- function(data, no.export=TRUE) {
  foldername = paste("hist_",substitute(loansData),sep="")
  createfolder(foldername)
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  for( i in 1:length(quantitatives) ) {
    if( no.export == FALSE ) {
      png(filename=paste(foldername,"/","hist_",paste(i),".png",sep=""))
    }
    hist( get(quantitatives[i],pos=data), xlab=quantitatives[i], main=quantitatives[i] )
    if( no.export == FALSE ) {
      dev.off()
    }
  }
  write("Done!",stdout())
}

#' Create boxplots of all quantitative variables using all factor variables
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
boxplotall <- function(data, no.export=TRUE, ...) {
  foldername = paste("boxplot_",substitute(loansData),sep="")
  createfolder(foldername)
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  for( i in 1:length(factors)) {
    for(j in 1:length(quantitatives)) {
      if( no.export == FALSE ) {
        png(filename=paste(foldername,"/","boxplot",paste(i),"_",paste(j),".png",sep=""))
      }
      boxplot( get(quantitatives[j],pos=data) ~ get(factors[i],pos=data),
               xlab=factors[i],
               ylab=quantitatives[j],
               ...)
      if( no.export == FALSE ) {
        dev.off()
      }
    }
  }
  write("Done!",stdout())
}