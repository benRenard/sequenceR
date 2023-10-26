# getOneHarmonic <- function(f0,k,peak,decay,duration,sustain,type){
#   w=oscillator(type=type,freq=k*f0,duration=duration)
#   w=applyEnvelope(w,envelope(t=c(0,peak,peak+(decay-peak)/k,duration)/duration,v=c(0,1,sustain,0)))
#   return(w)
# }
#
# getOneNote <-function(f0,nHarmonics,peak,decay,duration,sustain,type){
#   for(i in 1:nHarmonics){
#     h=getOneHarmonic(f0,i,peak,decay,duration,sustain/i^4,type)
#     if(i==1){w=h} else {w$wave=w$wave+(1/i)*h$wave}
#   }
#   return(w)
# }
#
# createSynth <- function(notes,nHarmonics=5,
#                         peak=0.03,decay=0.8,duration=1,sustain=0.25,
#                         type='sine'){
#   f=getFrequencies(notes)
#   samples=vector('list',length(notes))
#   for(i in 1:length(notes)){
#     samples[[i]]=getOneNote(f[i],nHarmonics,peak,decay,duration,sustain,type)
#   }
#   return(instrument(samples, notes))
# }
#
