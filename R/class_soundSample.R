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
# Sound Sample Utilities ----
#
#' Cast to a sound sample
#'
#' Convert a tuneR::Wave object into a soundSample.
#'
#' @param w tuneR Wave object
#' @param pan Numeric in [-1;1], panoramic. -1 (resp. 1) only select the left
#'     (resp. right) channel of w (if the latter is stereo). 0 averages both channels
#' @return An object of class 'soundSample'.
#' @examples
#' w <- tuneR::Wave(left=sin(2*pi*seq(0,1,,44100)*440)) # 1-second A
#' sam <- as.soundSample(w)
#' plot(sam)
#' @export
as.soundSample <- function(w,pan=0){
  v=pan2vol(pan);wave=0
  if(length(w@left)>0){wave=wave+v$left*w@left}
  if(length(w@right)>0){wave=wave+v$right*w@right}
  sam <- soundSample(wave=wave,rate=w@samp.rate)
  return(sam)
}

#' Cast to a tuneR::Wave object
#'
#' Convert a soundSample into a tuneR::Wave object.
#'
#' @param x sound sample object.
#' @return a tuneR Wave object.
#' @examples
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)) # 1-second A (440 Hz)
#' w <- as.Wave(sam)
#' plot(w)
#' @export
as.Wave <- function(x){
  w <- tuneR::Wave(left = x$wave, right = x$wave, samp.rate = x$rate, bit = 16)
  w <- tuneR::normalize(w, unit = "16")
  return(w)
}

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

#' Read a sound sample
#'
#' Read a sound sample from a .mp3 or .wav file.
#'
#' @param file string, file with extension .wav or.mp3
#' @param ... additional arguments passed to function tuneR::readWave
#' @return An object of class 'soundSample'.
#' @examples
#' sam=try(read.soundSample(file='vignettes/07027201.mp3'))
#' @importFrom tuneR readWave readMP3
#' @export
read.soundSample <- function(file,...){
  ext=tools::file_ext(file)
  if(ext %in% c('mp3','MP3')){
    w=tuneR::readMP3(file)
  } else if (ext %in% c('wav','WAV')){
    w=tuneR::readWave(file,...)
  } else {
    stop('unknown file extension')
  }
  sam=as.soundSample(w)
  return(sam)
}

#' Write a sound sample
#'
#' Write a sound sample in .wav or .mp3 format.
#'
#' @param x sound sample object.
#' @param file string, destination file. Default file format is .wav.
#'    If file extension is .mp3, conversion to mp3 is attempted using ffmpeg,
#'    which hence needs to be available (see https://ffmpeg.org/).
#' @return nothing - writing function.
#' @examples
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)) # 1-second A (440 Hz)
#' write.soundSample(sam,'A440.wav')
#' @export
write.soundSample <- function(x,file){
  w=as.Wave(x)
  ext=tools::file_ext(file)
  if (ext %in% c('wav','WAV')){
    tuneR::writeWave(w,file)
  } else if (ext %in% c('mp3','MP3')){ # try to convert with ffmpeg
    tuneR::writeWave(w,'temp.wav')
    cmd=paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',file)
    out=system(cmd)
    file.remove('temp.wav' )
    if(out!=0){stop('Conversion to .mp3 has failed: install ffmpeg or use a .wav format.')}
  } else {
    tuneR::writeWave(w,file)
    warning('Unknown file extension: file has been written but in a WAVE format.')
  }
}

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

#' Get sampling time
#'
#' Get the times steps associated with a sound sample.
#'
#' @param x sound sample object.
#' @return a numeric vector containing the sampling times in second.
#' @examples
#' # Define sound sample
#' sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)+0.1*rnorm(44100)) # 1-second noisy A
#' # Compute sampling times
#' timeSteps=getTime(sam)
#' @export
getTime <- function(x){
  return(seq(0,x$duration,length.out=x$n))
}

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
  # Check input arguments are valid
  checkSeqArgs(list(volume=volume,time=time,pan=pan))
  # total duration of the sequence (max time + length of one sample)
  duration <- max(time) + sample$duration
  # size of the sequence
  n <- 1+round(duration*sample$rate)
  # Check size of resulting wave
  checkMaxSize(n,nmax)
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

#' Apply an envelope
#'
#' Apply a volume envelope to a sound sample.
#'
#' @param sample Sound sample object.
#' @param env Envelope object. Envelope values should all be between 0 and 1.
#' @return A sound sample object.
#' @examples
#' # Define the sound sample
#' sam <- soundSample(sin(2*pi*seq(0,0.5,1/44100)*220)) # 0.5-second A (220 Hz)
#' # Define the envelope
#' env <- envelope(t=c(0,0.03,1),v=c(0,1,0))
#' # Apply it
#' res <- applyEnvelope(sam,env)
#' # Compare waveforms
#' par(mfrow=c(1,2))
#' plot(sam,main='before');plot(res,main='after')
#' # Uncomment to listen to the result
#' # listen(res)
#' @export
applyEnvelope <- function(sample,env){
  # Check envelop values are between 0 and 1
  if( max(env$v)>1 | min(env$v)<0){
    mess=paste0("Invalid volume envelope: should be normalized between 0 and 1")
    stop(mess,call.=FALSE)
  }
  # Get time steps of input sound sample
  tim=getTime(sample)
  # regrid envelope on those time steps
  v=stats::approx(x=env$t*sample$duration,y=env$v,xout=tim)$y
  # modify sample
  out=sample;out$wave=out$wave*v
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
