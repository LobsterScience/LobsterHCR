#' @title predMolt
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) area, doy (day of year), temp (temperature at doy)
#' @param \code{cl} : carapace width
#' @return The predicted probability of moulting
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export


predMolt <- function(p,cl,gdd=FALSE,sex) {

	with(p,{
		d = doy

		if(gdd) {

					if(cl<=39)       				{ a=-1.835; b=0.005} #gom20
					if(cl >39& cl<60) 				{ a=-2.252; b=0.006} #gom20
					if(cl >=60& cl<=99 & sex==1) 	{ a=-10.554; b=0.017}
					if(cl >=60& cl<=99 & sex==2) 	{ a=-9.875; b=0.016}
					if(cl >99 & cl<=129& sex==1)	{ a=-4.758; b=0.006}
					if(cl >99 & cl<=129& sex==2)	{ a=-4.678; b=0.007}
					if(cl >129 & sex==1)			{ a=-3.226; b=0.004}
					if(cl >129 & sex==2)			{ a=-4.426; b=0.005}
									}
		if(!gdd) {

					if(cl<=39)       				{ a=-1.687; b=0.0183} #gom20
					if(cl >39& cl<60) 				{ a=-1.951; b=0.023} #gom20
					if(cl >=60& cl<=99 & sex==1) 	{ a=-2.56; b=0.0147}
					if(cl >=60& cl<=99 & sex==2) 	{ a=-2.156; b=0.0149}
					if(cl >99 & cl<=129& sex==1)	{ a=-3.654; b=0.0122}
					if(cl >99 & cl<=129& sex==2)	{ a=-3.461; b=0.0107}
					if(cl >129 & sex==1)			{ a=-2.287; b=0.0081}
					if(cl >129 & sex==2)			{ a=-4.392; b=0.0109}
					}


			pPrMolt = 1 / (1+ exp(-(a+b*d)))
			return(pPrMolt)
			})

}
