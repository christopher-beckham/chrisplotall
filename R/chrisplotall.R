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

#' Create scatterplots out of all possible quantitative variables in the data.
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
scatterall <- function(data, no.export=TRUE, ...) {
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  # simple 2d scatterplots
  for( i in 1:length(quantitatives)) {
    for( j in i:length(quantitatives)) {
      if( i != j) {
        if(no.export == FALSE) {
          png(filename=paste("scatter_",paste(i),"_",paste(j),".png",sep=""))
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
}

#' Create histograms out of all possible quantitative variables in the data.
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
histoall <- function(data, no.export=TRUE) {
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  for( i in 1:length(quantitatives) ) {
    if( no.export == FALSE ) {
      png(filename=paste("hist_",paste(i),".png",sep=""))
    }
    hist( get(quantitatives[i],pos=data), xlab=quantitatives[i], main=quantitatives[i] )
    if( no.export == FALSE ) {
      dev.off()
    }
  }
}

#' Create boxplots of all quantitative variables using all factor variables
#' @param data the data frame to use
#' @param no.export if this is set to false, then graphs will be exported to png
#' @return null
#' @export
boxplotall <- function(data, no.export=TRUE, ...) {
  lst <- init(data)
  quantitatives <- lst[[1]]
  factors <- lst[[2]]
  for( i in 1:length(factors)) {
    for(j in 1:length(quantitatives)) {
      if( no.export == FALSE ) {
        png(filename=paste("boxplot",paste(i),"_",paste(j),".png",sep=""))
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
}