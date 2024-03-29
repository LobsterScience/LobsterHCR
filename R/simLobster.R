#' @title simLobster
#' @description Prediction lobster molt, maturity, maturity and fishing
#' @param \code{p} :parameter list containing (at a minimum) maturity ogive parameters (a, b), natural mortality (m), fishing mortality (F), timing of fishing (season), number of annual time steps (nAnn)
#' @param \code{recruitment.vector} : Variable Recruitment vector of length p$nt/round(365/p$timestep)
#' @return The predicted probability of moulting
#' @author  Brad Hubley, Adam Cook\email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export

simLobster = function(p,gdd=T,nit = 1){
  start = Sys.time()
  out = list()
for(i in 1:nit){

	# array N[time, length, # of molts, time since last molt]
	totalPop = array(0,dim=c(p$nt,length(p$lens)))
	totalBerried = totalPop
	totalRemovals = totalPop
	totalNotch = totalPop
	totalNewNotch = totalPop
	totalEggs = totalPop
	totalPop[1,1] = p$StartPop # start with
	nts = round(365.25/p$timestep)
  interval = rep(1:nts,times = p$nyr)
  notchedM1nm = notchedM2nm = notchedM2 = notchedM1 = rep(0,times=length(p$lens))
	if(length(p$F) == 1) {
		p$Fl = getFvec(p$F,p$LS,p$lens,p$window,p$max)
		} else {
			p$Fl = p$F
		}
    if(length(p$M)==1){
      p$Ml = rep(p$M,length(p$lens))
    } else{
      p$Ml = p$M
    }

	ty = p$timestep/365
	Fty = FadjSeason(p) # adjust F time by amount of open season in timestep

	cat('|')

	for(t in 1:(p$nt-1)){
	    spawners = 0
      if(!exists('moltDate',p)) {
                moltTime = sample(c(18:21),1)
              }else{
                moltTime = round(as.numeric((p$moltDate - p$startDate)/p$timestep))
                }
      if(!exists('spawnDate',p)){
      spawnTime = moltTime+1
      } else{
        spawnTime = round(as.numeric((p$spawnDate - p$startDate)/p$timestep))
        }
			p$doy =  interval[t] * p$timestep # days since last molt
			p$ddoy = p$doy

				# mortality comes before moulting and getting berried
				for(l in 1:length(p$lens)){
				   safe = totalPop[t,l] * p$reserve * exp(-p$Ml[l] * ty) #if some pop is considered uncatchable only M
				   totalPop[t+1,l] = totalPop[t,l] * (1 - p$reserve) #remove those uncatchable or reserve lobster from the fishery
				   totalRemovals[t,l] = totalPop[t+1,l]  * ((p$Fl[l] * Fty[t]) / (p$Fl[l] * Fty[t] + p$Ml[l]* ty)) * (1 - exp(-(p$Fl[l] * Fty[t] + p$Ml[l] * ty))) #Baranov

				  totalPop[t+1,l] = totalPop[t+1,l] * exp(-p$Ml[l] * ty) - totalRemovals[t,l] + safe #add the uncatchable back in
				  if(p$sex==2){
				    totalBerried[t+1,l] = totalBerried[t,l] * exp(-p$Ml[l] * ty)
				    notchedM1[l] = notchedM1[l] * exp(-p$Ml[l] * ty)
				    notchedM2[l] = notchedM2[l] * exp(-p$Ml[l] * ty)
				    totalNewNotch[t+1,l] =  totalNewNotch[t,l] + (totalBerried[t+1,l]  *  ((p$Fl[l] * Fty[t]) / (p$Fl[l] * Fty[t] + p$Ml[l]* ty)) * (1 - exp(-(p$Fl[l] * Fty[t] + p$Ml[l] * ty)))) * p$notch.compliance # catch of berried, all berried get notched
				    totalBerried[t+1,l] = totalBerried[t+1,l] - totalNewNotch[t+1,l]
				    totalNotch[t+1,l] = totalNewNotch[t+1,l] + notchedM1[l] + notchedM2[l]
				    }
				}

			if(interval[t]== moltTime){
			  gm = getGroMat2(p)
				noMolt = totalPop[t+1,] * (1 - gm$pMolt) #NoMolt
				molted = (totalPop[t+1,] * gm$pMolt) %*% gm$transMatrix #Molt
				totalPop[t+1,] = noMolt + molted # combine newly molted into 1 timestep since last molt slot

				if(p$sex==2){
				  released = totalBerried[t+1,] + totalNewNotch[t+1,] #for egg production
				  spawners = released
				  totalEggs[t+1,] = spawners * Fecundity(cl = p$lens)

				  releasedAndMolt = totalBerried[t+1,] %*% gm$transMatrix #all berried releasing eggs molt
				  totalPop[t+1,] = totalPop[t+1,] + releasedAndMolt #do not return notch to population

				  #order matters on notching return to pop
				  notchedM2nm = notchedM2* (1 - gm$pMolt) #not moulting
				  notchedM2 = notchedM2* (gm$pMolt) #moulting
          notchedM3 = notchedM2  %*% gm$transMatrix #after 3 moults all return to pop
				  totalPop[t+1,] = totalPop[t+1,] + notchedM3

				  notchedM1nm = notchedM1* (1 - gm$pMolt)
				  notchedM1 = notchedM1* ( gm$pMolt)
				  notchedM2 = notchedM1 %*% gm$transMatrix

				  notchedM2go = notchedM2 *(p$notchGrowOut[2]) #notch grow out
				  notchedM2 = notchedM2 * (1-p$notchGrowOut[2]) + notchedM2nm
				  totalPop[t+1,] = totalPop[t+1,] + notchedM2go

				  notchedAndReleased = totalNewNotch[t+1,]
				  notchedM1 = notchedAndReleased %*% gm$transMatrix + notchedM1nm #Molt
				  notchedM1go = notchedM1 *(p$notchGrowOut[1])
				  notchedM1= notchedM1 *(1-p$notchGrowOut[1])
				  totalPop[t+1,] = totalPop[t+1,] + notchedM1go
				  totalNewNotch[t+1,] = 0
				  }
  			}
			if(p$sex==2 & interval[t]==spawnTime){
				bf = pMat2(p, cl=p$lens)
				  rn = runif(length(bf),0.1,.3)
				  bf = bf * rn # proportion of females that are actually berried
				  notBerried = totalPop[t+1,] * (1 - bf) # Not Berried
				  berried = totalPop[t+1,] * bf # Berried
				  notchAndBerried = notchedM2 * bf
				  notchedM2 = notchedM2 *(1-bf)
				  totalBerried[t+1,] = berried +notchAndBerried  #add to totalBerried
				  totalPop[t+1,] = notBerried # Return berried that release eggs to total Pop but dont let the reberry in the same year
				}
		}
	cat('|')

	finalPop = data.frame(totalPop)
	names(finalPop)=paste0("CL",p$lens)
	finalBerried = data.frame(totalBerried)
	names(finalBerried)=paste0("CL",p$lens)
	totalRemovals = data.frame(totalRemovals)
	names(totalRemovals)=paste0("CL",p$lens)
	totalNotch = data.frame(totalNotch) #this is only new notches applied
	names(totalNotch)=paste0("CL",p$lens)
	totalNewNotch = data.frame(totalNewNotch) #this is only new notches applied
	names(totalNewNotch)=paste0("CL",p$lens)
	totalEggs = data.frame(totalEggs) #this is only new notches applied
	names(totalEggs)=paste0("CL",p$lens)

	out[[i]] = (list(finalPop=finalPop,finalBerried=finalBerried,totalEggs=totalEggs,totalRemovals=totalRemovals,totalNotch = totalNotch,totalNewNotch = totalNewNotch))
}
  print(Sys.time()-start)

  return(out)
}

