#' @title getIncr
#' @description Prediction of moult increment based on carapace length and sex
#' @param \code{p} :parameter list containing carapace length (lens) and sex
#' @param \code{cl} : carapace width
#' @return The predicted moult increment
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export


getIncr = function(p){

    require(rstanarm)
	res = matrix(NA,length(p$lens),length(p$lens))

	for(i in 1:(length(p$lens)-1)){

		cl = p$lens[i]

		x = c(p$lens[which(p$lens==cl):length(p$lens)],max(p$lens)+diff(p$lens)[1])

		if(p$sex==1)Incr=posterior_predict(p$moltModel$maleMoltIncrModel,newdata=data.frame(CL=cl),fun=exp)
		if(p$sex==2)Incr=posterior_predict(p$moltModel$femaleMoltIncrModel,newdata=data.frame(CL=cl),fun=exp)

		new.lens = cl + Incr
		new.lens = new.lens[new.lens<max(p$lens)+diff(p$lens)[1]]
		a = hist(new.lens,breaks=x,plot=F)
		res[i,which(p$lens==cl):length(p$lens)] = a$counts/sum(a$counts)
	}
	return(res)


}
