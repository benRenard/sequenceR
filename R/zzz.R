.onLoad <- function(libname, pkgname){
  # setup default player on Linux (Ubuntu)
  # to be improved (try other players if rhythmbox is not available)
  os=Sys.info()['sysname']
  if(os=='Linux') {tuneR::setWavPlayer('rhythmbox')}
}
