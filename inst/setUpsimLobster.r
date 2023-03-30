
# Lobster population simulation

p = list()
p$project.name = "bio.lobster"

p$libs = c('rgdal',"devtools","fields",  "parallel","sp",
                     'MASS','doBy','bio.lobster','bio.utilities',
                     "lubridate",'LobsterHCR','dplyr')

lapply(p$libs,require, character.only=T)

load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')

p$current.assessment.year=year(Sys.time())

p$LFA= 34 #c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")

####### Base
	p$exploitation_rate = 0.8 #per year; gets adjusted to season length and turned to F
	p$sex=2
	p$LS=82.5
	p$Fadj = 1
	p$Sadj = 1
	p$Sclose='end'
	p$nyr = 15					# number of timesteps
	p$lens = seq(50,200,5)		# carapace length bins (mm)
	p$timestep = 14  			# in days
	p$M = 0.1 #or vector of length lens
	p$reserve = 0.1 # % of lobsters that don't trap
	p$handlingM = 0.01 #applied to berried only at this point
	#can specify p$season as p$season = c(as.Date("2000-05-16"),as.Date("2000-07-15")) # 27 or let getLobsterList fill in commercial seasons
	p$startDate = as.Date("1999-10-01")
	p$moltDate = as.Date("2000-07-14")
	p$spawnDate = as.Date("2000-08-01")
	p$moltModel = moltModel(redo=F)
  p$max = 220
	#window
	p$window=NULL
	p$notch = NULL
	p$notch.compliance=0 #everyone is doing it=1 no notch =0
	p$notchGrowOut = c(0.1,.3,.6) #retainable vnotch prob post molt, Moult1, moult2, moult3 all return to pop

	o = getLobsterList(p)

	#example
	x= simLobster(o)
  plot(apply(x$finalPop,1,sum),type='h') #you will see spikes in teh female pop post egg release and molt, then drop off again with berried happening right after
  plot(apply(x$finalBerried,1,sum),type='h')
  plot(apply(x$totalRemovals,1,sum),type='h')


  #MLS

  Measure = c(82.5,85,87.5,90)
  HCR = data.frame(Measure=Measure,Eggs=NA,LandNum=NA,LandKg=NA)

  plist = makeParallel(o,parms2vary='LS',values=Measure)

  load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')
  niterations=10
	mlist = mclapply(X = plist, FUN = simLobster,nit=niterations)
	if(niterations==1){
          	out = simSummary(plist,lapply(mlist,"[[",1))
          	HCR$Eggs = pdiff(out$eggs,ind=1)
          	HCR$LandNum = pdiff(out$landnum,ind=1)
          	HCR$LandKg = pdiff(out$landkg,ind=1)
          	} else {
        	  out = simSummary(plist,mlist)
        	  HCR$Eggs = pdiff(out$eggs,ind=1)
        	  HCR$LandNum = pdiff(out$landnum,ind=1)
        	  HCR$LandKg = pdiff(out$landkg,ind=1)
    }



##### Window size
	load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')

	WindowSize = list(c(220,220),c(115,125),c(105,125))
	names(WindowSize) = c("noWin","SmallWin","BigWin")

	plist = makeParallel(o,parms2vary='window',values=WindowSize)
  niterations = 10
#
		mlist = mclapply(X = plist, FUN = simLobster,nit=niterations)

		HCR = data.frame(Measure=names(WindowSize),Eggs=NA,LandNum=NA,LandKg=NA)
		if(niterations==1){
		  out = simSummary(plist,lapply(mlist,"[[",1))
		  HCR$Eggs = pdiff(out$eggs,ind=1)
		  HCR$LandNum = pdiff(out$landnum,ind=1)
		  HCR$LandKg = pdiff(out$landkg,ind=1)
		} else {
		  out = simSummary(plist,mlist)
		  HCR$Eggs = pdiff(out$eggs,ind=1)
		  HCR$LandNum = pdiff(out$landnum,ind=1)
		  HCR$LandKg = pdiff(out$landkg,ind=1)
		}

		##### max size
load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')

		maxsize = c(220,115,120,125,130,135)
    o$max=220
		plist = makeParallel(o,parms2vary='max',values=maxsize)

		mlist = mclapply(X = plist, FUN = simLobster,nit=niterations)

		HCR = data.frame(Measure=maxsize,Eggs=NA,LandNum=NA,LandKg=NA)
		if(niterations==1){
		  out = simSummary(plist,lapply(mlist,"[[",1))
		  HCR$Eggs = pdiff(out$eggs,ind=1)
		  HCR$LandNum = pdiff(out$landnum,ind=1)
		  HCR$LandKg = pdiff(out$landkg,ind=1)
		} else {
		  out = simSummary(plist,mlist)
		  HCR$Eggs = pdiff(out$eggs,ind=1)
		  HCR$LandNum = pdiff(out$landnum,ind=1)
		  HCR$LandKg = pdiff(out$landkg,ind=1)
		}


##### notch
		load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')
  niterations =10
		notch = c(0,seq(0.5,1,by=.1))
		o$notch.compliance=0
		plist = makeParallel(o,parms2vary='notch.compliance',values=notch)

		mlist = mclapply(X = plist, FUN = simLobster,nit=niterations)

		HCR = data.frame(Measure=notch,Eggs=NA,LandNum=NA,LandKg=NA)
		if(niterations==1){
		  out = simSummary(plist,lapply(mlist,"[[",1))
		  HCR$Eggs = pdiff(out$eggs,ind=1)
		  HCR$LandNum = pdiff(out$landnum,ind=1)
		  HCR$LandKg = pdiff(out$landkg,ind=1)
		} else {
		  out = simSummary(plist,mlist)
		  HCR$Eggs = pdiff(out$eggs,ind=1)
		  HCR$LandNum = pdiff(out$landnum,ind=1)
		  HCR$LandKg = pdiff(out$landkg,ind=1)
		}

