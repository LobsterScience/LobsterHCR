#' @title getGroMat2
#' @description Prediction of moulting based on day since last moult and size
#' @param \code{p} :parameter list containing (at a minimum) lens (length bins), doy (day of year)
#' @return The transition matrix for moulting and probability of moulting at length
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export


getGroMat2 = function(p){

	mat=matrix(NA,length(p$lens),length(p$lens))
	pM=c()
	for (i in 1:(nrow(mat)-1)){
		# get probablitiy of molting
  		pM[i] = predMolt(p=p,cl=p$lens[i])
		# use molt increment mean & sd to determine which size classes to molt into then multiply by probability of molting
		incr = na.omit(p$Incr[i,]) * pM[i]
		# fill in row with proportion molted by size class
		mat[i,i:ncol(mat)] = incr
	}
	mat[is.na(mat)] = 0
	std = apply(mat,1,sum)
	std = ifelse(std==0,1,std)
	mat = (mat /std ) #all molted need to go somewhere

	return(list(transMatrix=mat, pMolt=c(pM,0)))
}
