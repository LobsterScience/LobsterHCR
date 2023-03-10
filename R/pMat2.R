#' @export
pMat2 = function(p,cl,lfa=NULL){


	if(is.null(lfa)){
		if(p$LFA == "27N" || p$LFA == "27S" || p$LFA == "27")  lfa = "LFA27-30" # 27
		if(p$LFA == "29")                                        lfa = "LFA29" # 29
		if(p$LFA == "28" || p$LFA == "30")                      lfa = "LFA28,30" # 30
		if(p$LFA == "31A")                                       lfa = "LFA29" # 31A
		if(p$LFA == "31B" || p$LFA == "32")                     lfa = "LFA32" # 31B & 32
		if(p$LFA == "33E" || p$LFA == "33W" || p$LFA == "33")  lfa = "LFA33" # 33
		if(p$LFA == "34")                                        lfa = "LFA34" # 33
		if(p$LFA == "35")                                        lfa = "LFA35" # 33
		if(p$LFA == "36")                                        lfa = "LFA36" # 33
		if(p$LFA == "38")                                        lfa = "LFA38" # 33



	}

	pl = data.frame(LFA=c("LFA27-30","LFA29","LFA28,30","LFA31A","LFA32x","LFA32","LFA33x","LFA33","LFA34","LFA35","LFA36",'LFA38'),
		a=c(14.266, 14.173, 16.505, 14.53521, 10.4, 18.99223, 14.23,24.87275,22.37302,22.96,12.41,16.797),
		b=c(-0.1959, -0.1727, -0.2132,-0.20347, -0.112,-0.21128, -0.144,-0.25725,-0.23187,-0.24,-0.127,-0.178))

	pMat = with(subset(pl,LFA==lfa),	1/(1+(exp(a+(b*cl)))))


	return(pMat)
}

#LFA 35 a=22.96, b=-0.24
#LFA 36 a=12.41 b=-0.127
#LFA 38 a=16.979 b=-0.178
