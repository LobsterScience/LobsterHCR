
# Lobster population simulation

p = list()
p$project.name = "bio.lobster"

p$libs = c('rgdal',"devtools","fields",  "parallel","sp",
                     'MASS','doBy','bio.lobster','bio.utilities',
                     "lubridate",'LobsterHCR','dplyr')

lapply(p$libs,require, character.only=T)

load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')

p$current.assessment.year=year(Sys.time())

p$LFA= 27

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

	p = getLobsterList(p)

	#example
	x= simLobster(p)
  plot(apply(x$finalPop,1,sum),type='h') #you will see spikes in teh female pop post egg release and molt, then drop off again with berried happening right after
  plot(apply(x$finalBerried,1,sum),type='h')
  plot(apply(x$totalRemovals,1,sum),type='h')

