#' @export
pdiff <- function(v,ind=1,type='pchg'){

  if(type=='pdiff') (v-v[ind]) / ((v+v[ind])/2) * 100
  if(type=='pchg')  (v-v[ind]) / (v[ind]) * 100

}
