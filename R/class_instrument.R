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

#' Play an instrument
#'
#' Take a sound sample and repeat it following given timeline,
#' volume and pan.
#'
#' @param inst Instrument object.
#' @param notes String or integer vector, the notes of the instrument to be played,
#'   either by name or by index.
#' @param time Numeric vector, time (in seconds) at which each note should be played.
#'   Should be non-negative, non-decreasing and have same size as notes.
#' @param volume Numeric vector, volume between 0 and 1,
#' @param pan Numeric vector, pan between -1 (left) and 1 (right) (0 = centered).
#'   Same size as notes.
#' @param fadein Numeric vector, fade-in duration (in seconds), same size as notes.
#' @param fadeout Numeric vector, fade-out duration (in seconds), same size as notes.
#'   Use Inf for 'let ring'.
#' @param env list of envelope objects, envelope applied to each note.
#' @param nmax Integer, max number of values for each channel of the resulting Wave.
#'    Default value (10*10^6) roughly corresponds to a 150 Mb stereo wave, ~3 min 45s.
#' @return an S4 Wave object (from package tuneR).
#' @examples
#' # Create an instrument
#' samples=list(oscillator(freq=110),oscillator(freq=220),oscillator(freq=261.63),
#'              oscillator(freq=293.66),oscillator(freq=392))
#' notes=c('A2','A3','C4','D4','G4')
#' onTheMoon <- instrument(samples,notes)
#' # Play it
#' w=play.instrument(onTheMoon)
#' # View the result
#' plot(w)
#' # Uncomment to listen to the result
#' # play(w)
#' # Use options
#' w=play.instrument(onTheMoon,time=c(0,0.2,0.4,0.6,0.8,0.9),
#'                   notes=c('A2','G4','D4','C4','A3','A2'),
#'                   volume=seq(0.2,1,length.out=6),pan=c(0,-1,1,-1,1,0),
#'                   fadeout=c(Inf,0.01,0.01,0.01,Inf,Inf))
#' # View the result
#' plot(w)
#' # Uncomment to listen to the result
#' # play(w)
#' @export
play.instrument <-function (inst,notes=1:length(inst),
               time=seq(0,(length(notes)-1)*0.25,length.out=length(notes)),
               volume=rep(1,length(notes)),pan=rep(0,length(notes)),
               fadein=rep(0.01,length(notes)),fadeout=fadein,
               env=NULL,nmax=10*10^6){
  # Check input arguments are valid
  checkSeqArgs(list(notes=notes,time=time,volume=volume,pan=pan,
                    fadein=fadein,fadeout=fadeout,env=env))
  # total duration of the sequence (max time + length of the last note)
  duration <- max(time) + inst[[notes[length(notes)]]]$duration
  # size of the sequence
  rate=inst[[notes[1]]]$rate
  n <- 1+round(duration*rate)
  # Check size of resulting wave
  checkMaxSize(n,nmax)
  # initialize
  left <- numeric(n)
  right <- numeric(n)
  for(i in 1:NROW(time)){
    sample=inst[[notes[i]]]
    # apply envelop
    if(!is.null(env)){sample=applyEnvelope(sample,env[[i]])}
    # apply fadein
    e=envelope(t=c(0,min(1,fadein[i]/sample$duration),1),v=c(0,1,1))
    sample=applyEnvelope(sample,e)
    # apply fadeout
    if(i<NROW(time)){ # no fade out on last note
      if(fadeout[i]<Inf){ # otherwise let ring
        e=envelope(t=c(0,min(1,(time[i+1]-time[i])/sample$duration),
                       min(1,(time[i+1]-time[i]+fadeout[i])/sample$duration),1),
                   v=c(1,1,0,0))
        sample=applyEnvelope(sample,e)
      }
    }
    # work out indices in sequence
    from <- 1+round(time[i]*rate)
    upto <- min(from+sample$n-1,n)
    # get basic signal
    w <- volume[i]*sample$wave[1:(upto-from+1)]
    # get panned left-right signals
    v=pan2vol(pan[i])
    w.left <- v$left * w
    w.right <- v$right * w
    # update to overall channels
    left[from:upto] <- left[from:upto] + w.left
    right[from:upto] <- right[from:upto] + w.right
  }
  wave <- tuneR::Wave(left=left,right=right,samp.rate=rate,bit=16)
  wave <- tuneR::normalize(wave, unit = "16")
  return(wave)
}

#' Write an instrument to file
#'
#' Write each sound sample of the instrument as a separate .wav or .mp3 file.
#'
#' @param inst Instrument object.
#' @param dir String, directory where files should be written.
#' @param fmt String, 'wav' or 'mp3'.
#' @return nothing - writing function.
#' @examples
#' # Create an instrument
#' drumset <- instrument(samples=list(kick,snare,hiHat),notes=c('boom','tat','cheet'))
#' # Write to files (one per element)
#' write.instrument(drumset)
#' @export
write.instrument <-function (inst,dir=getwd(),fmt='wav'){
  if(!(fmt %in% c('wav','WAV','mp3','MP3'))){stop('Unknown format')}
  noms=names(inst)
  for(i in 1:length(inst)){
    fname=file.path(dir,paste0(noms[i],'.',fmt))
    write.soundSample(inst[[i]],fname)
  }
}

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
