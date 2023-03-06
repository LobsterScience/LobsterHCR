#' @title getBerried
#' @param \code{p} parameter list (p) containing (at a minimum) lens (length bins), doy (day of year), gestation period (360),
#' @return The probability of bearing eggs at length (pB) and the probability of releasing eggs (pR)
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export


getBerried = function(p){
	pB =  1 / (1+exp(-0.1*(p$doy-p$gestation)))
	pR =  1 / (1+exp(-0.1*(p$doy-(p$gestation+p$brood)))) #proportion released eggs


	pB = pB * pMat(p, cl=p$lens) #proportion berried at length

	return(list(pB=pB,pR=pR))
}
