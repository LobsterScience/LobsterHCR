
# Lobster population simulation

p = list()
p$project.name = "bio.lobster"

p$libs = c('rgdal',"devtools","fields",  "parallel","sp",
                     'MASS','doBy','bio.lobster','bio.utilities',
                     "lubridate",'LobsterHCR','dplyr')

lapply(p$libs,require, character.only=T)
load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')



p$current.assessment.year=year(Sys.time())

p$LFA= 27 #c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")

#p$moltModel = readRDS(file.path(project.datadirectory('bio.lobster'),'analysis','Tagging','moltModel_moltIncre.rds'))



####### Base
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
	#can specify p$season as p$season = c(as.Date("2000-05-16"),as.Date("2000-07-15")) # 27 or let getLobsterList fill in commercial seasons
	p$startDate = as.Date("1999-10-01")
	p$moltDate = as.Date("2000-07-14")
	p$spawnDate = as.Date("2000-08-01")

	#window
	p$window=NULL
	p$notch = NULL
	p$notch.compliance=1 #everyone is doing it=1 no notch =0
	p$notchGrowOut = c(0.1,.3,.6) #retainable vnotch prob post molt, Moult1, moult2, moult3 all return to pop
	plist = getLobsterList(p)
	names(plist) = p$lfas

	x= simLobster(p)
  plot(apply(x$finalPop,1,sum),type='h') #you will see spikes in teh female pop post egg release and molt, then drop off again with berried happening right after
  plot(apply(x$finalBerried,1,sum),type='h')
  plot(apply(x$totalRemovals,1,sum),type='h')


	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas


	plist = getSimList(p,sex=2)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResults2931aBase.rdata"))




##### Legal Size

	LegalSize = c(81,84, 86)
	names(LegalSize) = paste0("LS",LegalSize)

	for(i in 1:length(LegalSize)){

		plist = getSimList(p,sex=1,LS=LegalSize[i])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,LS=LegalSize[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults2931a",names(LegalSize)[i],".rdata")))
	}


##### Window size

	WindowSize = list(c(115,125),c(105,125))
	names(WindowSize) = c("SmallWin","BigWin")

	for(i in 1:length(WindowSize)){

		plist = getSimList(p,sex=1,window=WindowSize[[i]])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,window=WindowSize[[i]])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults2931a",names(WindowSize)[i],".rdata")))
	}



###### combine all areas to one rdata object

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResults2931aBase.rdata"))

	LFAs = c( "29", "31A")

	simSumLegalSize = simSummary(runs=c("Base", "LS81","LS84","LS86"),lfas=LFAs, fn='simResults2931a')

	mlsEggs = as.data.frame(simSumLegalSize$eggs)
	names(mlsEggs)= c('LFA29', 'LFA31a')
	mlsEggs$mls = c(82.5,81,84,86)

	simSumWindow = simSummary(runs=c("Base","FSmallWin","FBigWin","SmallWin","BigWin"),lfas=LFAs)
	simSumMaxSize = simSummary(runs=c("Base","FMax125","FMax130","FMax135","Max125","Max130","Max135"),lfas=LFAs)


	LStabRP = data.frame(rbind(
		round(100*(simSumLegalSize[[1]][1,]/simSumLegalSize[[1]][2,]-1)),
		round(100*(simSumLegalSize[[1]][5,]/simSumLegalSize[[1]][1,]-1)),
		round(100*(simSumLegalSize[[1]][4,]/simSumLegalSize[[1]][1,]-1))
		))
	names(LStabRP) = LFAs
	rownames(LStabRP) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStabRP70.csv"),row.names=T)

	LStabNo = data.frame(rbind(
		round(100*(simSumLegalSize[[2]][6,]/simSumLegalSize[[2]][1,]-1)),
		round(100*(simSumLegalSize[[2]][5,]/simSumLegalSize[[2]][1,]-1)),
		round(100*(simSumLegalSize[[2]][4,]/simSumLegalSize[[2]][1,]-1))
		))
	names(LStabNo) = LFAs
	rownames(LStabNo) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStabNo70.csv"),row.names=T)

	LStabKg = data.frame(rbind(
		round(100*(simSumLegalSize[[3]][6,]/simSumLegalSize[[3]][1,]-1)),
		round(100*(simSumLegalSize[[3]][5,]/simSumLegalSize[[3]][1,]-1)),
		round(100*(simSumLegalSize[[3]][4,]/simSumLegalSize[[3]][1,]-1))
		))
	names(LStabKg) = LFAs
	rownames(LStabKg) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStabKg70.csv"),row.names=T)

	OStabRP = data.frame(rbind(
		round(100*(simSumSeason[[1]][6,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][5,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][4,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][3,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][2,]/simSumSeason[[1]][1,]-1))
		))
	names(OStabRP) = LFAs
	rownames(OStabRP) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStabRP.csv"),row.names=T)
	OStabNo = data.frame(rbind(
		round(100*(simSumSeason[[2]][6,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][5,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][4,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][3,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][2,]/simSumSeason[[2]][1,]-1))
		))
	names(OStabNo) = LFAs
	rownames(OStabNo) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStabNo.csv"),row.names=T)
	OStabKg = data.frame(rbind(
		round(100*(simSumSeason[[3]][6,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][5,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][4,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][3,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][2,]/simSumSeason[[3]][1,]-1))
		))
	names(OStabKg) = LFAs
	rownames(OStabNo) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStabKg.csv"),row.names=T)

	WintabRP = data.frame(rbind(
		round(100*(simSumWindow[[1]][5,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][4,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][3,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][2,]/simSumWindow[[1]][1,]-1))
		))
	names(WintabRP) = LFAs
	rownames(WintabRP) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","WintabRP.csv"),row.names=T)
	WintabNo = data.frame(rbind(
		round(100*(simSumWindow[[2]][5,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][4,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][3,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][2,]/simSumWindow[[2]][1,]-1))
		))
	names(WintabNo) = LFAs
	rownames(WintabNo) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","WintabNo.csv"),row.names=T)
	WintabKg = data.frame(rbind(
		round(100*(simSumWindow[[3]][5,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][4,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][3,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][2,]/simSumWindow[[3]][1,]-1))
		))
	names(WintabKg) = LFAs
	rownames(WintabKg) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","WintabKg.csv"),row.names=T)




	MaxtabRP = data.frame(rbind(
		round(100*(simSumMaxSize[[1]][7,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][6,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][5,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][4,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][3,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][2,]/simSumMaxSize[[1]][1,]-1))
		))
	names(MaxtabRP) = LFAs
	rownames(MaxtabRP) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","MaxtabRP.csv"),row.names=T)
	MaxtabNo = data.frame(rbind(
		round(100*(simSumMaxSize[[2]][7,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][6,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][5,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][4,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][3,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][2,]/simSumMaxSize[[2]][1,]-1))
		))
	names(MaxtabNo) = LFAs
	rownames(MaxtabNo) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","MaxtabNo.csv"),row.names=T)
	MaxtabKg = data.frame(rbind(
		round(100*(simSumMaxSize[[3]][7,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][6,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][5,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][4,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][3,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][2,]/simSumMaxSize[[3]][1,]-1))
		))
	names(MaxtabKg) = LFAs
	rownames(MaxtabKg) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","MaxtabKg.csv"),row.names=T)

	for(i in 1:9){

		LS=data.frame(Eggs=LStabRP[,i],Numbers=LStabNo[,i],Weight=LStabKg[,i],row.names= c("LS90", "LS87.5", "LS85"))
		OS=data.frame(Eggs=OStabRP[,i],Numbers=OStabNo[,i],Weight=OStabKg[,i],row.names= c("SS5", "SS6", "SS7", "SS8", "SS9"))
		Win=data.frame(Eggs=WintabRP[,i],Numbers=WintabNo[,i],Weight=WintabKg[,i],row.names= c("BigWin", "SmallWin","FBigWin", "FSmallWin"))
		Max=data.frame(Eggs=MaxtabRP[,i],Numbers=MaxtabNo[,i],Weight=MaxtabKg[,i],row.names= c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125"))
		write.csv(rbind(LS,OS,Win,Max),file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("LFA",LFAs[i],".csv")),row.names=T)
	}


	cols=tim.colors(9)
	ltys=1:9
	pchs=c(1:6,15:17)


	#x11()
	png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumLegalSize.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(70,90,2.5),simSumLegalSize$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	axis(4)
	abline(v=82.5,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(70,90,2.5),simSumLegalSize$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	axis(4)
	abline(v=82.5,lty=3,col='grey')
	matplot(x=seq(70,90,2.5),simSumLegalSize$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	axis(4)
	abline(v=82.5,lty=3,col='grey')
	mtext("Minimum Legal Size (mm)",1,3)

	dev.off()

	#x11()
	png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumSeason.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	axis(4)
	#abline(v=0,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	axis(4)
	#abline(v=0,lty=3,col='grey')
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	axis(4)
	#abline(v=0,lty=3,col='grey')
	mtext("Season reduction (%)",1,3)

	dev.off()



	#####
	x11()
	#png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumLegalSize.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(82.5,90,2.5),simSumLegalSize$landnum[6:9,],type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	par(new=T)
	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))



	#abline(v=82.5,lty=3,col='grey')
	#legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(82.5,90,2.5),simSumLegalSize$landkg[6:9,],type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	par(new=T)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	#abline(v=82.5,lty=3,col='grey')
	matplot(x=seq(82.5,90,2.5),simSumLegalSize$eggs[6:9,]/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	par(new=T)
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	#abline(v=82.5,lty=3,col='grey')
	mtext("Minimum Legal Size (mm)",1,3)

	#dev.off()
	par(new=T)
	#x11()
	#png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumSeason.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	#abline(v=0,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	#abline(v=0,lty=3,col='grey')
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	#abline(v=0,lty=3,col='grey')
	mtext("Season reduction (%)",1,3)

	dev.off()


# from 1.IndicatorEstimation.CohortAnalysis.r
ad
outS
bins=seq(80,150,5)

### LFA 27

is=which(ad$LFA==27&ad$YEAR>2008)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'27N'$totalRemovals[,7:31])+
colSums(rlist$mlist$'27S'$totalRemovals[,7:31])+
colSums(rlist$flist$'27N'$totalRemovals[,7:31])+
colSums(rlist$flist$'27S'$totalRemovals[,7:31])


compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA27a.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 27 (2009-2015)",labels=c("At Sea Sampling","Simulation"))

### LFA 33

is=which(ad$LFA==33&ad$YEAR>2001)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

#load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'33W'$totalRemovals[,7:31])+
colSums(rlist$mlist$'33E'$totalRemovals[,7:31])+
colSums(rlist$flist$'33W'$totalRemovals[,7:31])+
colSums(rlist$flist$'33E'$totalRemovals[,7:31])

compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA33.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 33",labels=c("At Sea Sampling","Simulation"))




### LFA 30

is=which(ad$LFA==30&ad$YEAR>1999)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

#load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'30'$totalRemovals[,7:31])+
colSums(rlist$flist$'30'$totalRemovals[,7:31])

compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA30.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 30",labels=c("At Sea Sampling","Simulation"))



### LFA 27
bins=seq(70,150,5)

is=which(ad$LFA==27&ad$YEAR<2001)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>70&lens<max(bins)],breaks=bins,plot=F)$counts

load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS70.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'27N'$totalRemovals[,7:31])+
colSums(rlist$mlist$'27S'$totalRemovals[,7:31])+
colSums(rlist$flist$'27N'$totalRemovals[,7:31])+
colSums(rlist$flist$'27S'$totalRemovals[,7:31])


compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA27b.pdf"),LS=c(70,70),xl=c(0,length(bins)),title="LFA 27 (1985-1999)",labels=c("At Sea Sampling","Simulation"))



##############



	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim3ResultsBase.rdata"))

	transMat = getTransMatrix(rlist)
