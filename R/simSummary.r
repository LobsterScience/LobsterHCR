#' @export
simSummary = function(plist,mlist){
  runs = length(mlist)
  nm = names(mlist)
  a=c(0.000608,0.001413,0.00482)
  b=c(3.0583,2.8746,2.638)

  if(all(names(mlist[[1]])!='finalPop')){
    x = length(mlist[[1]])
    ou=list()
    for(r in 1:runs){
            w = mlist[[r]]
            wn = names(w[[1]])
            for(i in 1:length(wn)){
              ou[[i]] = Reduce("+",lapply(w,"[[",i))/x #mean of runs
              names(ou)[i] <- wn[i]

       }
    mlist[[r]] = ou
      }
    }
    	eggs = landnum = landkg = rep(NA,(runs))
          		for(r in 1:runs){
          	  		lens = plist[[r]]$lens
          				bins =  lens+diff(plist[[1]]$lens[1:2])/2
          				mwv = a[1]*bins^b[1]
          				fwv = a[2]*bins^b[2]
          				eggs[r] = sum(mlist[[r]]$totalEggs)
          				landnum[r] = sum(mlist[[r]]$totalRemovals)
          				landkg[r] = sum(sweep(mlist[[r]]$totalRemovals,2,FUN='*',fwv))/1000
          			}
	list(eggs=eggs, landnum=landnum, landkg=landkg)
    }

