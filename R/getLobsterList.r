#' @export
getLobsterList = function(p){

		p$StartPop = 1000

	  p$nt = p$nyr * round(365/p$timestep)
		#reproduction
		if(is.null(p$season)){
		#commercial season timing
		if(p$LFA == "27N" || p$LFA == "27S" || p$LFA == "27")  p$season = c(as.Date("2000-05-16"),as.Date("2000-07-15")) # 27
		if(p$LFA == "29")                                        p$season = c(as.Date("2000-05-01"),as.Date("2000-06-30")) # 29
		if(p$LFA == "30")                                        p$season = c(as.Date("2000-05-20"),as.Date("2000-07-20")) # 30
		if(p$LFA == "31A")                                       p$season = c(as.Date("2000-04-30"),as.Date("2000-06-30")) # 31A
		if(p$LFA == "31B" || p$LFA == "32")                     p$season = c(as.Date("2000-04-20"),as.Date("2000-06-20")) # 31B & 32
		if(p$LFA == "33E" || p$LFA == "33W" || p$LFA == "33")  p$season = c(as.Date("1999-11-28"),as.Date("2000-05-31")) # 33
		if(p$LFA == "34")                                        p$season = c(as.Date("1999-11-28"),as.Date("2000-05-31")) # 34
		if(p$LFA == "35")                                        p$season = c(as.Date("1999-10-15"),as.Date("1999-12-31"),as.Date("2000-03-01"),as.Date("2000-07-31")) # 35
		if(p$LFA == "36")                                        p$season = c(as.Date("1999-11-12"),as.Date("2000-01-15"),as.Date("2000-04-01"),as.Date("2000-06-29")) # 36
		if(p$LFA == "38")                                        p$season = c(as.Date("1999-11-12"),as.Date("2000-06-25")) # 38
  }

	  p$startDate = as.Date("1999-10-01")


		#season adjustment
		#browser()
		p$season = as.Date(p$season)

		if(length(p$season)==2){
			if(Sclose=='end')p$season[2] = p$season[2] - (p$season[2]-p$season[1])*(1-Sadj)
			if(Sclose=='start')p$season[1] = p$season[1] + (p$season[2]-p$season[1])*(1-Sadj)
		}

		if(length(p$season)==4){
			if(Sclose=='end'){p$season[2] = p$season[2] - (p$season[2]-p$season[1])*(1-Sadj);p$season[4] = p$season[4] - (p$season[4]-p$season[3])*(1-Sadj)}
			if(Sclose=='start'){p$season[1] = p$season[1] + (p$season[2]-p$season[1])*(1-Sadj); p$season[3] = p$season[3] + (p$season[4]-p$season[3])*(1-Sadj)}
		}


		#mortality

		if(is.null(p$F))p$F = getF(p)

		# fishing mortality adjustment
		p$F = p$F * Fadj


		if(p$LFA == '31A') p$LFA = 311
		if(p$LFA == '31B') p$LFA = 312

				#p$dailytemps = TempModelPredict(p)

		p$dailytemps = glorysTempImport(p)

		if(p$LFA == 311) p$LFA = '31A'
		if(p$LFA == 312) p$LFA = '31B'

		p$mint = 10 # minimum temperature for a lobster to molt

		#growth increment
  	p$Incr = getIncr(p)

	return(plist)
}
