#' @export
glorysTempImport <- function(p=p, file=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','GlorysClimatologies1993-2022byDOYWithLFA.rds'),
                             start = p$startDate, end = p$startDate+(p$nt*p$timestep), redo=F){

  re = readRDS(file)
  re$geometry <- NULL
  if(p$LFA=='31A')
  re = subset(re, LFA == p$LFA)
  if(p$LFA %in% c(27,29,30,311,312,32)) re = subset(re, z>2 & z<30)
  re = as_tibble(re)
  rea = re %>% group_by(doy) %>% summarize(across(starts_with('BT'),mean, .names = "{.col}")) %>% tidyr::pivot_longer(cols=starts_with('BT'),values_to='temp')
  rea$yr = as.numeric(substr(rea$name,4,7)  )

  rea$y =	rea$doy/365+rea$yr
  st = decimal_date(start)
  ed = decimal_date(end)

  fin = subset(rea,y>=st & y<=ed)$temp


}

