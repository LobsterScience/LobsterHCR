#' @export
getDegreeDaysAboveThresh = function(p,t){
	#degree days above mint
	d = t * p$timestep #total days
	d2 = d - (p$timestep-1)
	v = p$dailytemps[d2:d]
	v = sum(v[which(v>p$mint)])
	dd = sum(p$dailytemps[(d-p$doy+1):(d-d2)])

	dd
}
