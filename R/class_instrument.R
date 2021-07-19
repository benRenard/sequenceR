#***************************************************************************----
# Constructor ----
#' Instrument constructor.
#'
#' Creates a new instance of an 'instrument' object.
#' An instrument is a named list of sound samples (all with the same sampling rate).
#'
#' @param samples list of sound samples
#' @param notes string vector, name given to each sample
#' @return An object of class 'Instrument'.
#' @examples
#' drumset <- instrument(samples=list(kick,snare,hiHat),notes=c('boom','tat','cheet'))
#' @export
instrument<-function(samples,notes=as.character(1:length(samples))){
  o<-new_instrument(samples,notes)
  return(validate_instrument(o))
}

#***************************************************************************----
# Instrument Utilities ----

# play.instrument <-function (inst,notes=1:length(inst),
#                 time=seq(0,(length(inst)-1)*0.25,length.out=length(inst)),
#                 volume=rep(1,length(inst)),pan=rep(0,length(inst)),
#                 fadein=rep(0.01,length(inst)),fadeout=fadein,
#                 env=NULL,nmax=10*10^6){}

#***************************************************************************----
# internal constructor ----
new_instrument<-function(samples,notes){
  stopifnot(is.list(samples))
  stopifnot(is.character(notes) )
  if( length(samples) != length(notes) ){
    mess=paste0("Invalid instrument: samples and notes should have the same size")
    stop(mess,call.=FALSE)
  }
  if( any(duplicated(notes)) ){
    mess=paste0("Invalid notes: duplicated strings")
    stop(mess,call.=FALSE)
  }
  o <- samples
  names(o) <- notes
  class(o) <- 'instrument'
  return(o)
}

#***************************************************************************----
# validator ----
validate_instrument<-function(inst){
  rate=NA
  for(note in names(inst)){
    if(class(inst[[note]])!='soundSample'){
      mess=paste0("Invalid samples: they should all be of class soundSample")
      stop(mess,call.=FALSE)
    }
    if(is.na(rate)){
      rate=inst[[note]]$rate
    } else {
      if(inst[[note]]$rate != rate) {
        mess=paste0("Invalid samples: they should all have the same sampling rate")
        stop(mess,call.=FALSE)
      }
    }
  }
  return(inst)
}
