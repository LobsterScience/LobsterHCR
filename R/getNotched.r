#' @title getNotched
#' @param \code{p} parameter list containing the proportion of berried that are notched (default of 20%) and whether the definition is protection with or without setal hairs
#' @return The number of vnotched that get protection
#' @author  Adam Cook, \email{Adam.Cook@dfo-mpo.gc.ca}
#' @export


getNotched = function(p){
	pB =  1 / (1+exp(-0.1*(p$doy-p$gestation)))
	pR =  1 / (1+exp(-0.1*(p$doy-(p$gestation+p$brood)))) #proportion released eggs


	pB = pB * pMat(p, cl=p$lens) #proportion berried at length

	return(list(pB=pB,pR=pR))
}
