# run 1.steUpsimLobster.r
#keep the original p

o = p



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

