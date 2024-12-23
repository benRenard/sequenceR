#' Distortion effect
#'
#' Apply a distortion to a sound sample
#'
#' @param sample soundSample object, input sample
#' @param type Character string, the distortion type
#' @param level Numeric >0, distortion level
#' @param ... other parameters passed to the distortion transfer function
#' @param rescale Logical. If TRUE, the soundSample wave is rescaled to [-1,1]
#' @return The distorted sound sample
#' @examples
#' # example code
#' raw=oscillator(freq=110,duration=0.5)
#' plot(raw)
#' dist=applyDisto(raw,type='tanh',level=5)
#' plot(dist)
#' @export
applyDisto <- function(sample,type=c('clip','tanh'),level=2,...,rescale=FALSE){
  out=sample
  fname=paste0('disto_',type)
  out$wave=do.call(fname,list(sample$wave,level,...))
  if(rescale) out$wave=rescale(out$wave,-1,1)
  return(out)
}

#' Delay effect
#'
#' Apply a delay to a sound sample. See https://en.wikipedia.org/wiki/Comb_filter
#'
#' @param sample soundSample object, input sample
#' @param type Character string, the delay type: feedforward or feedback
#' @param delayTime Numeric >0, delay time in s.
#' @param echoes Numeric vector >0. The size of the vector gives the number of echoes, the values the level of each echo (generally decreases to 0).
#' @return The sound sample with a delay effect
#' @examples
#' # example code
#' notes=c('E3','G3','A3','B3','D4','E4','G4')
#' synth=getSynth(notes)
#' raw=as.soundSample(play.instrument(synth,notes=notes[c(1,2,3,2,3,4,3,4,5,4,5,6,5,6,7)]))
#' plot(raw)
#' \dontrun{listen(raw)}
#' # Single echo by default
#' cooked=applyDelay(raw)
#' plot(cooked)
#' \dontrun{listen(cooked)}
#' # Multiple echoes
#' cooked=applyDelay(raw,echoes=1/(1:10))
#' plot(cooked)
#' \dontrun{listen(cooked)}
#' # Feedback-type delay
#' cooked=applyDelay(raw,echoes=1/(1:10),type='feedback')
#' plot(cooked)
#' \dontrun{listen(cooked)}
#' @export
applyDelay <- function(sample,type='feedforward',delayTime=0.6,echoes=c(0.8)){
  if(!(type %in% c('feedforward','feedback'))){
    mess="Unknwon delay type; only known types are 'feedforward' and 'feedback'"
    stop(mess,call.=FALSE)
  }
  # delay length in number of samples
  delayLength=round(delayTime*sample$rate)
  # Initialize output: wave duration needs to be augmented to avoid sudden clipping
  if(type=='feedforward'){
    nAdd=delayLength * (length(echoes)+1)
  } else { # trickyer, semi-empirical augmentation for feedback delay. 2DO: improve this
    nAdd=delayLength * sum(echoes*seq(length(echoes),1,-1))
  }
  w0=c(sample$wave,rep(0,nAdd)) # add trailing zeros to wave
  out=sample
  out$wave=w0
  out$n=out$n+nAdd
  out$duration=out$n/out$rate
  # Add echoes to initial wave
  for(i in 1:length(echoes)){
    # get indices of delayed signal
    ix=(1:out$n)-delayLength*i
    ix[ix<1]=1
    # alter wave
    if(type=='feedforward'){
      out$wave=out$wave+echoes[i]*w0[ix]
    } else { # recursive echo
      out$wave=out$wave+echoes[i]*out$wave[ix]
    }
  }
  out$wave=rescale(out$wave,-1,1)
  return(out)
}

#' Pitch shifter
#'
#' Shift the pitch of a sound sample by n semitones. Note that the duration of the
#' resulting sample is not the same as that of the original.
#'
#' @param sample Sound sample object.
#' @param n numeric, number of semitones.
#' @return A sound sample object.
#' @examples
#' # Define a A sound sample and get a D by adding 5 semitones
#' A <- soundSample(sin(2*pi*seq(0,0.5,1/44100)*220)) # 0.5-second A (220 Hz)
#' D <- shiftPitch(A,5)
#' @export
shiftPitch <- function(sample,n){
  k=(2^(n/12))
  t.in=seq(from=0,by=1/sample$rate,length.out=sample$n)
  t.out=seq(from=0,to=sample$duration,by=k/sample$rate)
  foo=stats::approx(x=t.in,y=sample$wave,xout=t.out)
  out=soundSample(foo$y[!is.na(foo$y)])
  return(out)
}

#***************************************************************************----
# Private utilities ----
# 2DO: check this for more disto functions:
# https://anasounds.com/od-disto-fuzz-differences/
# https://www.audiokinetic.com/en/library/edge/?source=Help&id=wwise_guitar_distortion_plug_in

#' Clip distortion
#'
#' Transfer function for 'clip' distortion
#' @param x Numeric vector in [-1,1], input signal
#' @param level Numeric (>=0), distortion level
#' @return a numeric vector containing the distorted output signal
disto_clip <- function(x,level){
  cut=1/(level+1)
  y=x
  y[x > cut] = cut
  y[x < -1*cut] = -1*cut
  return(y)
}

#' Tanh distortion
#'
#' Transfer function for 'tanh' distortion
#' @param x Numeric vector in [-1,1], input signal
#' @param level Numeric (>=0), distortion level
#' @return a numeric vector containing the distorted output signal
disto_tanh <- function(x,level){
  if(level<=0){y=x} else {y=tanh(x*level)}
  return(y)
}
