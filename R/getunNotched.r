#' @title getunNothced
#' @param \code{p} parameter list (p) containing  doy (day of year), gestation period (360),
#' @return The probability of moulting out of a notch
#' @author  Adam Cook, \email{adam.cook@@dfo-mpo.gc.ca}
#' @export


getunNotched = function(p){
  unNotch = sample(c(365,730,1095),prob=c(0.2,.6,.2),size=1)
  pUN =1 / (1+exp(-0.1*(p$doy-unNotch)))
	return(pUN)
}
