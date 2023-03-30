#' @export
makeParallel <- function(p,parms2vary,values){
  ju = list()
  k = length(p)
if(parms2vary %in% c('LS','max', 'Fmax','notch.compliance')){
    for(i in 1:length(values)){

      n = paste('X',values[i],sep="")
      ju[[i]] = p
      ju[[i]][parms2vary] <- values[i]
      names(ju)[i] <- n
    }
}
  if(parms2vary %in% c('window')){

    for(i in 1:length(values)){
      n = paste('X',values[[i]],collapse="", sep="")
      p[[k+1]]<- values[[i]]
      names(p)[k+1] <- parms2vary
      ju[[i]] = p

    }
  }

  return(ju)

}
