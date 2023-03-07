
# LobsterHCR


The goal of LobsterHCR is to provide stand alone package for developing / testing / evaluating lobster harvest controls in a reproducible manner.

## Installation

You can install the development version of LobsterHCR like so:

``` r
require(devtools)
install_github('LobsterScience/LobsterHCR')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Lobster population simulation

p = list()
p$project.name = "bio.lobster"

p$libs = c('rgdal',"devtools","fields",  "parallel","sp",
                     'MASS','doBy','bio.lobster','bio.utilities',
                     "lubridate",'LobsterHCR','dplyr')

lapply(p$libs,require, character.only=T)
p$current.assessment.year=year(Sys.time())
p$LFA= 27

redoTaggingModels=F

  p$TempModel =TempModel(areas = 'subarea',redo=redoTaggingModels)$Model
	p$moltModel = moltModel(p,redo.dd=F,redo=redoTaggingModels)

	p$F = 1.2
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
	p$startDate = as.Date("1999-10-01")
	p$moltDate = as.Date("2000-07-14")
	p$spawnDate = as.Date("2000-08-01")

	#window
	p$window=NULL
	p$notch.compliance=1 #everyone is doing it
	p$notchGrowOut = c(0.1,.3,.6) #retainable vnotch prob post molt, Moult1, moult2, moult3

	x= simLobster(p)
  plot(apply(x$finalPop,1,sum),type='h') 
  plot(apply(x$finalBerried,1,sum),type='h')
  plot(apply(x$totalRemovals,1,sum),type='h')

```

