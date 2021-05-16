#***************************************************************************----
# Constructor ----
#' Sound sample constructor.
#'
#' Creates a new instance of a 'soundSample' object.
#' A sound sample can be viewed as a minimalistic version of
#' an "audio wave" object (see package tuneR for instance).
#' It is necessarily mono and the wave time series is normalized
#' between -1 and 1.
#'
#' @param wave Numeric vector, wave time series
#' @param rate Numeric, sampling rate (default 44100 Hz)
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)) # 1-second A (440 Hz)
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)+0.1*rnorm(44100)) # 1-second noisy A
#' @export
soundSample<-function(wave,rate=44100){
  o<-new_soundSample(wave,rate)
  return(validate_soundSample(o))
}

#***************************************************************************----
# plot ----
#
#' Plot a sound sample
#'
#' Plot a sound sample. Uses plotly to add zooming capability.
#'
#' @param x sound sample object.
#' @param ... further arguments passed to tuneR plotting function.
#' @return nothing - plotting function.
#' @examples
#' # Define sound sample
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)+0.1*rnorm(44100)) # 1-second noisy A
#' # plot it
#' plot(sam)
#' @importFrom tuneR Wave plot
#' @method plot soundSample
#' @export
plot.soundSample <- function(x,...){
  wave <- tuneR::Wave(left = x$wave,samp.rate = x$rate, bit = 16)
  tuneR::plot(wave,...)
}

#***************************************************************************----
# listen ----
#
#' Listen to a sound sample
#'
#' Listen to a sound sample. Based on tuneR function 'play'
#'
#' @param x sound sample object.
#' @return nothing - listening function.
#' @examples
#' # Define sound sample
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)+0.1*rnorm(44100)) # 1-second noisy A
#' # Uncomment to play it
#' # listen(sam)
#' @importFrom tuneR Wave normalize play
#' @export
listen <- function(x){
  wave <- tuneR::Wave(left = x$wave, right = x$wave, samp.rate = x$rate, bit = 16)
  wave <- tuneR::normalize(wave, unit = "16")
  tuneR::play(wave)
}

#***************************************************************************----
# sequence ----
#
#' Sequence a sound sample
#'
#' Take a sound sample and repeat it following given timeline,
#' volume and pan.
#'
#' @param sample Sound sample object.
#' @param time Numeric vector, time (in seconds) at which sample should be repeated
#' @param letRing Logical. If TRUE overlapping samples are added;
#'     if FALSE, a new sample stops the previous one (=> beware of the click!))
#' @param volume Numeric vector, volume between 0 and 1.
#' @param pan Numeric vector, pan between -1 (left) and 1 (right) (0 = centered).
#' @param nmax Integer, max number of values for each channel of the resulting Wave.
#'    Default value (10*10^6) roughly corresponds to a 150 Mb stereo wave, ~3 min 45s.
#' @return an S4 Wave object (from package tuneR).
#' @examples
#' # EXAMPLE 1
#' # Define a sound sample
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)+0.1*rnorm(44100)) # 1-second noisy A
#' # Sequence it
#' s <- sequence(sam,time=c(0,0.5,0.75),letRing=FALSE,volume=c(0.4,1,1),pan=c(-1,0,1))
#' # View the result
#' plot(s)
#' # Uncomment to play it
#' # play(s)
#'
#' #' EXAMPLE 2 - make it funkyer
#' # 2-second sequence based on hi-hat sample
#' s <- sequence(hiHat,time=seq(0,2,,16),volume=rep(c(1,rep(0.5,3)),4))
#' # View the result
#' plot(s)
#' # Uncomment to play it
#' # play(s)
#' @export
sequence <- function(sample,time,letRing=TRUE,
                     volume=rep(1,NROW(time)),pan=rep(0,NROW(time)),
                     nmax=10*10^6){
  if( max(volume)>1 | min(volume)<0){
    mess=paste0("Invalid volume: should be normalized between 0 and 1")
    stop(mess,call.=FALSE)
  }
  if( NROW(volume) != NROW(time) ){
    mess=paste0("Invalid volume: should have same size as time ")
    stop(mess,call.=FALSE)
  }
  if( max(pan) > 1 | min(pan) < -1){
    mess=paste0("Invalid pan: should be normalized between -1 and 1")
    stop(mess,call.=FALSE)
  }
  if( NROW(pan) != NROW(time) ){
    mess=paste0("Invalid pan: should have same size as time ")
    stop(mess,call.=FALSE)
  }
  # total duration of the sequence (max time + length of one sample)
  duration <- max(time) + sample$duration
  # size of the sequence
  n <- 1+round(duration*sample$rate)
  # Check size of resulting wave
  if(n>nmax){
    mess=paste0("Max size exceeded: ",
                "size of sequenced wave ",
                "[n=",n,"; approx. size:",round(2*n*8/2^20)," Mb] ",
                "exceeds nmax [",nmax,"]. ",
                "Either increase nmax or decrease sampling rate.")
    stop(mess,call.=FALSE)
  }
  # initialize
  p <- NROW(sample$wave)
  left <- numeric(n)
  right <- numeric(n)
  for(i in 1:NROW(time)){
    # work out indices in sequence
    from <- 1+round(time[i]*sample$rate)
    upto <- min(from+p-1,n)
    # get basic signal
    w <- volume[i]*sample$wave[1:(upto-from+1)]
    # get panned left-right signals
    v=pan2vol(pan[i])
    w.left <- v$left * w
    w.right <- v$right * w
    # update to overall channels
    left[from:upto] <- w.left + ifelse(letRing,1,0)*left[from:upto]
    right[from:upto] <- w.right + ifelse(letRing,1,0)*right[from:upto]
  }
  wave <- tuneR::Wave(left=left,right=right,
                      samp.rate=sample$rate,bit=16)
  wave <- tuneR::normalize(wave, unit = "16")
  return(wave)
}

#***************************************************************************----
# internal constructor ----
new_soundSample<-function(wave,rate){
  stopifnot(is.numeric(wave))
  stopifnot(is.numeric(rate))
  n <- NROW(wave)
  w0 <- as.numeric(wave)
  if(max(abs(w0))==0){ # silence !
    w=w0
  } else { # standardize
    w=w0/max(abs(w0))
  }
  o <- list(wave=w,rate=rate,n=n,duration=n/rate)
  class(o) <- 'soundSample'
  return(o)
}

#***************************************************************************----
# validator ----
validate_soundSample<-function(sam){
  if( max(abs(sam$wave))>1 ){
    mess=paste0("Invalid wave: should be a normalized between -1 and 1")
    stop(mess,call.=FALSE)
  }
  if( sam$rate <=0 ){
    mess=paste0("Invalid rate: should be a strictly positive")
    stop(mess,call.=FALSE)
  }
  return(sam)
}
