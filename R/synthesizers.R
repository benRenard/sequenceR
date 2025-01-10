#' Harmonics sound sample
#'
#' Creates a sound sample corresponding to the kth harmonics of a given frequency
#' @param freq Numeric, base frequency in Hz
#' @param k Integer >=1, kth harmonics
#' @param peak Numeric, peak time in seconds
#' @param decay Numeric, end-of-decay time in seconds
#' @param duration Numeric, total duration in seconds
#' @param sustain Numeric, sustain volume
#' @param type String, oscillator type, one of 'sine', 'saw', 'square' or 'triangle'.
#'     If an unknowm string is provided, a sine oscillator will be used.
#' @return An object of class 'soundSample'.
#' @examples
#' sam1 <- getHarmonics(440,1)
#' plot(sam1)
#' sam2 <- getHarmonics(440,3)
#' plot(sam2)
#' \dontrun{
#' # This line of code is wrapped in \dontrun{} since it relies
#' # on an external audio player to listen to the audio sample.
#' # See ?tuneR::setWavPlayer for setting a default player.
#' listen(sam2)}
#' @export
getHarmonics <- function(freq,k,peak=0.03,decay=0.8,duration=1,sustain=0.25,type='sine'){
  if(sustain<0 | sustain>1){
    mess=paste0("Invalid sustain: should be between 0 and 1")
    stop(mess,call.=FALSE)
  }
  if( ! (peak<=decay & decay<=duration)){
    mess=paste0("peak < decay < duration is not respected")
    stop(mess,call.=FALSE)
  }
  w=oscillator(type=type,freq=k*freq,duration=duration)
  w=applyEnvelope(w,envelope(t=c(0,peak,decay,duration)/duration,v=c(0,1,sustain,0)))
  return(w)
}

#' Single note from a synthesizer
#'
#' Creates one note with frequency freq from an additive, Hammond-inspired synth.
#' Higher harmonics decay faster and have smaller sustain.
#' @param freq Numeric, base frequency in Hz
#' @param nHarmonics Integer >=1, number of harmonics
#' @param peak Numeric, peak time in seconds
#' @param decay Numeric, end-of-decay time in seconds
#' @param duration Numeric, total duration in seconds
#' @param sustain Numeric, sustain volume
#' @param decayPar Numeric, the higher the value the smaller the decay time for higher harmonics
#' @param sustainPar Numeric, the higher the value the smaller the sustain volume for higher harmonics
#' @param type String, oscillator type, one of 'sine', 'saw', 'square' or 'triangle'.
#'     If an unknown string is provided, a sine oscillator will be used.
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- getSynthNote(440,nHarmonics=7)
#' plot(sam)
#' \dontrun{
#' # This line of code is wrapped in \dontrun{} since it relies
#' # on an external audio player to listen to the audio sample.
#' # See ?tuneR::setWavPlayer for setting a default player.
#' listen(sam)}
#' @export
getSynthNote <-function(freq,nHarmonics=5,
                        peak=0.03,decay=0.8,duration=1,sustain=0.25,
                        decayPar=1,sustainPar=4,type='sine'){
  for(i in 1:nHarmonics){
    h=getHarmonics(freq,i,peak,peak+(decay-peak)/(i^decayPar),duration,sustain/(i^sustainPar),type)
    if(i==1){w=h} else {w$wave=w$wave+(1/i)*h$wave}
  }
  return(w)
}

#' Synthesizer
#'
#' Creates an additive, Hammond-inspired Synthesizer.
#' Higher harmonics decay faster and have smaller sustain.
#' @param notes Character vector, note names
#' @param nHarmonics Integer >=1, number of harmonics
#' @param peak Numeric, peak time in seconds
#' @param decay Numeric, end-of-decay time in seconds
#' @param duration Numeric, total duration in seconds
#' @param sustain Numeric, sustain volume
#' @param decayPar Numeric, the higher the value the smaller the decay time for higher harmonics
#' @param sustainPar Numeric, the higher the value the smaller the sustain volume for higher harmonics
#' @param type String, oscillator type, one of 'sine', 'saw', 'square' or 'triangle'.
#'     If an unknown string is provided, a sine oscillator will be used.
#' @return An object of class 'instrument'.
#' @examples
#' synth <- getSynth(c('E2','B2','E3','G3','A3'))
#' w=play.instrument(synth,time=(0:(length(synth)-1))*0.5,fadeout=rep(Inf,length(synth)))
#' tuneR::plot(w)
#' \dontrun{
#' # This line of code is wrapped in \dontrun{} since it relies
#' # on an external audio player to play the audio sample.
#' # See ?tuneR::setWavPlayer for setting a default player.
#' tuneR::play(w)}
#' @export
getSynth <- function(notes,nHarmonics=5,
                     peak=0.03,decay=0.8,duration=1,sustain=0.25,
                     decayPar=1,sustainPar=4,type='sine'){
  f=getFrequencies(notes)
  samples=vector('list',length(notes))
  for(i in 1:length(notes)){
    samples[[i]]=getSynthNote(f[i],nHarmonics,peak,decay,duration,sustain,decayPar,sustainPar,type)
  }
  return(instrument(samples, notes))
}

