# library(dplyr)
# sample=oscillator(freq=110,duration=5) %>%
#   applyEnvelope(envelope(t=c(0,0.01,1),v=c(0,1,1)))
#
# # Check this for disto functions:
# # https://anasounds.com/od-disto-fuzz-differences/
# # https://www.audiokinetic.com/en/library/edge/?source=Help&id=wwise_guitar_distortion_plug_in
#
# disto_clip <- function(x,level){
#   cut=1/(level+1)
#   y=x
#   y[x > cut] = cut
#   y[x < -1*cut] = -1*cut
#   return(y)
# }
#
# disto_tanh <- function(x,level){
#   if(level<=0){y=x} else {y=tanh(x*level)}
#   return(y)
# }
#
# applyDisto <- function(sample,dfunk=disto_clip,level=2,rescale=FALSE){
#   out=sample
#   out$wave=dfunk(sample$wave,level)
#   if(rescale) out$wave=rescale(out$wave,-1,1)
#   return(out)
# }
#
# final=oscillator(freq=110,duration=5) %>%
#   applyEnvelope(envelope(t=c(0,0.1,1),v=c(0,1,1))) %>%
#   applyDisto(dfunk=disto_tanh,level=100) %>%
#   as.Wave()
# plot(final)
# play(final)
