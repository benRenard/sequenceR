#***************************************************************************----
# mix ----
#
#' Mix several waves
#'
#' Take several wave objects (package tuneR) and mix them according to volume and pan.
#'
#' @param waves List of wave S4 objects (tuneR)
#' @param volume Numeric vector, volume between 0 and 1.
#' @param pan Numeric vector, pan between -1 (left) and 1 (right) (0 = centered).
#' @return the result of th mix, an S4 Wave object (from package tuneR).
#' @examples
#' # A 2-second drum groove (4/4 measure)
#' # hi-hat on 16th notes
#' hh <- sequence(hiHat,time=2*(0:15)/16,volume=rep(c(1,rep(0.5,3)),4))
#' # bass kick on 1 and 3
#' k <- sequence(kick,time=2*c(0,8)/16)
#' # snare on 2 and 4
#' s <- sequence(snare,time=2*c(4,12)/16)
#' # Mix the 3 tracks
#' m1 <- mix(list(hh,k,s))
#' \dontrun{tuneR::play(m1)}
#' # Try with less hihat, more kick
#' m2 <- mix(list(hh,k,s),volume=c(0.3,1,0.8))
#' \dontrun{tuneR::play(m2)}
#' @export
mix <- function(waves,volume=rep(1,length(waves)),pan=rep(0,length(waves))){
  p <- length(waves)
  if( max(volume)>1 | min(volume)<0){
    mess=paste0("Invalid volume: should be normalized between 0 and 1")
    stop(mess,call.=FALSE)
  }
  if( NROW(volume) !=  p){
    mess=paste0("Invalid volume: should have same size as list of waves ")
    stop(mess,call.=FALSE)
  }
  if( max(pan) > 1 | min(pan) < -1){
    mess=paste0("Invalid pan: should be normalized between -1 and 1")
    stop(mess,call.=FALSE)
  }
  if( NROW(pan) != p ){
    mess=paste0("Invalid pan: should have same size as list of waves ")
    stop(mess,call.=FALSE)
  }
  # Check sample rates
  rate=waves[[1]]@samp.rate
  for(i in 1:p){
    if( waves[[i]]@samp.rate != rate){
      mess=paste0("Invalid waves: all waves shoudl have the same sampling rate")
      stop(mess,call.=FALSE)
    }
  }
  # Get longuest wave
  n=NROW(waves[[1]]@left)
  for(i in 1:p){
    if( NROW(waves[[i]]@left) > n){ n <- NROW(waves[[i]]@left)}
  }
  # initialize
  left <- numeric(n)
  right <- numeric(n)
  # mix
  for(i in 1:p){
    k=NROW(waves[[i]]@left)
    v=pan2vol(pan[i])
    left[1:k] <- left[1:k] + volume[i]*v$left*waves[[i]]@left
    right[1:k] <- right[1:k] + volume[i]*v$right*waves[[i]]@right
  }
  wave <- tuneR::Wave(left=left,right=right,
                      samp.rate=rate,bit=16)
  wave <- tuneR::normalize(wave, unit = "16")
  return(wave)
}

#***************************************************************************----
# Pan to volume ----
#
#' Pan-to-volume function
#'
#' Transforms a pan (between -1 for full left and 1 for full right) into right/left
#' normalized volumes between 0 and 1
#'
#' @param pan Numeric between -1 and 1
#' @return a list of length 2 with fields left and right.
#' @keywords internal
pan2vol<-function(pan,channel){
  out <- list(left=abs(-1+pan)/2,right=(1+pan)/2)
  return(out)
}

#***************************************************************************----
# Rescaling and mapping ----
#
#' Rescale function
#'
#' Rescale a series between two bounds
#'
#' @param x Numeric vector
#' @param low Numeric, lower bound
#' @param high Numeric, higher bound
#' @return a rescaled numeric vector
#' @examples
#' # example code
#' rescale(1:10)
#' rescale(rnorm(10), 100, 101)
#' @export
rescale<-function(x,low=0,high=1){
  mini <- min(x,na.rm=T)
  maxi <- max(x,na.rm=T)
  u <- (x-mini)/(maxi-mini) # between 0 and 1
  out <- low+(high-low)*u #between low and high
  return(out)
}

#' Pitch mapping function
#'
#' Maps a series of values into pitches of notes
#'
#' @param x Numeric vector
#' @param notes character vector, notes onto which values are map (i.e. the musical scakle).
#'     Notes should be written in Scientific pitch notation, e.g. c('C4','E4','G4')
#'     (see \url{https://en.wikipedia.org/wiki/Scientific_pitch_notation})
#' @return a character vector representing the original values transformed into pitches
#' @examples
#' pitchMapping(x=1:10,notes=c('C4','E4','G4'))
#' pitchMapping(rnorm(20),notes=c('E3','Gb3','G3','A3','B3','C4','D4'))
#' @export
pitchMapping <- function(x,notes){
  # map to integer indices between 1 and length(notes)
  ix=round(rescale(x,0.5+0.000001,length(notes)+0.5-0.000001))
  return(notes[ix])
}

#***************************************************************************----
# Music utilities ----
#
#' Note-frequency table
#'
#' Builds a dataframe containing notes (in
#' \href{https://en.wikipedia.org/wiki/Scientific_pitch_notation}{scientific pitch notation})
#' and corresponding frequencies.
#'
#' @param minOctave integer, smallest (lowest-pitched) octave
#' @param maxOctave integer, largest (highest-pitched) octave
#' @return a data frame with 4 columns: note name 1 (written with 'b'),
#'  note name 2 (written with '#'),index (in semitones with respect to A4)
#'  and frequency (in Hz)
#' @examples
#' # example code
#' noteFrequencyTable()
#' @export
noteFrequencyTable <- function(minOctave=0,maxOctave=8){
  nlist1=c('C','Db','D','Eb','E','F','Gb','G','Ab','A','Bb','B')
  nlist2=c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B')
  mini=ifelse(minOctave<=4,minOctave,4)
  maxi=ifelse(maxOctave>=4,maxOctave,4)
  DF=data.frame()
  for(i in mini:maxi){
    DF=rbind(DF,data.frame(note1=paste0(nlist1,i),note2=paste0(nlist2,i),octave=i))
  }
  DF=cbind(DF,index=(1:NROW(DF))-which(DF$note1=='A4'))
  DF=cbind(DF,frequency=440*2^(DF$index/12))
  return(DF[DF$octave>=minOctave & DF$octave<=maxOctave,-3])
}

#' Notes-to-frequencies function
#'
#' Get frequencies from note names (in
#' \href{https://en.wikipedia.org/wiki/Scientific_pitch_notation}{scientific pitch notation}).
#'
#' @param notes Character vector, note names.
#' @param minOctave integer, smallest (lowest-pitched) octave
#' @param maxOctave integer, largest (highest-pitched) octave
#' @return a numeric vector of frequencies (in Hz)
#' @examples
#' # example code
#' getFrequencies(c('A3','A4','A5','C#6','Db6','A9','X0'))
#' getFrequencies(c('A3','A4','A5','C#6','Db6','A9','X0'),maxOctave=9)
#' @export
getFrequencies <- function(notes,minOctave=0,maxOctave=8){
  out=rep(NA,length(notes))
  DF=noteFrequencyTable(minOctave,maxOctave)
  for(i in 1:length(notes)){
    k=which(DF$note1==notes[i])
    if(length(k)==0) {k=which(DF$note2==notes[i])}
    if(length(k)==0) {out[i]=NA} else {out[i]=DF$frequency[k]}
  }
  return(out)
}

#' Frequencies-to-notes function
#'
#' Get notes (in
#' \href{https://en.wikipedia.org/wiki/Scientific_pitch_notation}{scientific pitch notation})
#' from frequencies. The note with the closest frequency is returned.
#'
#' @param frequencies numeric vector, frequencies in Hz
#' @param minOctave integer, smallest (lowest-pitched) octave
#' @param maxOctave integer, largest (highest-pitched) octave
#' @param option character, use 'b' or '#' in note names?
#' @return a character vector of notes
#' @examples
#' # example code
#' getNotes(seq(440,10000,100))
#' getNotes(seq(440,10000,100),maxOctave=10,option='#')
#' @export
getNotes <- function(frequencies,minOctave=0,maxOctave=8,option='b'){
  out=rep(NA,length(frequencies))
  DF=noteFrequencyTable(minOctave,maxOctave)
  colname=ifelse(option=='#','note2','note1')
  for(i in 1:length(frequencies)){
    k=which.min(abs(frequencies[i]-DF$frequency))
    out[i]=DF[[colname]][k]
  }
  return(out)
}

#***************************************************************************----
# Miscellaneous private utilities ----

#' timeVector function
#'
#' Compute the time vector starting from 0 associated with a duration and a sampling rate
#' @param duration Numeric
#' @param rate Numeric
#' @return a numeric vector
timeVector <- function(duration=1,rate=44100){
  return(seq(0,duration,1/rate))
}

#' Check sequencer arguments
#'
#' Check that the arguments used in sequencing functions (e.g. time, volume, pan, etc.) are valid.
#' @param argList list, a named list containg the arguments
#' @return nothing - just stops execution with an error message if something is invalid
checkSeqArgs <- function(argList){
  if(!is.null(argList$time)){
    z=argList$time
    if( !is.numeric(z) | min(z)<0 | any(diff(z)<0)){
      mess=paste0("Invalid time: should be non-decreasing and non-negative")
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(argList$volume)){
    z=argList$volume
    if( !is.numeric(z) | max(z)>1 | min(z)<0){
      mess=paste0("Invalid volume: should be normalized between 0 and 1")
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(argList$pan)){
    z=argList$pan
    if( !is.numeric(z) | max(z) > 1 | min(z) < -1){
      mess=paste0("Invalid pan: should be normalized between -1 and 1")
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(argList$fadein)){
    z=argList$fadein
    if( !is.numeric(z) | min(z) < 0){
      mess=paste0("Invalid fadein: should be non-negative")
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(argList$fadeout)){
    z=argList$fadeout
    if( !is.numeric(z) | min(z) < 0){
      mess=paste0("Invalid fadeout: should be non-negative")
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(argList$notes)){
    z=argList$notes
    if( !is.integer(z) & !is.character(z)){
      mess=paste0("Invalid notes: should be either an integer or a character vector")
      stop(mess,call.=FALSE)
    }
    if( is.integer(z) & min(z)<0){
      mess=paste0("Invalid notes: negative integers not allowed")
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(argList$env)){
    z=argList$env
    if( any(sapply(z,class)!='envelope') ){
      mess=paste0("Invalid env: should be a vector of objects of class 'envelope'")
      stop(mess,call.=FALSE)
    }
  }
  # Check all arguments have same length
  ll=rep(NA,length(argList))
  for(i in 1:length(ll)){
    if(!is.null(argList[[i]])){ll[i]=length(argList[[i]])}
  }
  if(diff(range(ll,na.rm=TRUE))>0){
    mess=paste0("Some argument(s) in (",paste0(names(argList),collapse=','),") are invalid: they should all have the same length")
    stop(mess,call.=FALSE)
  }
}

#' Check wave size
#'
#' Check that the size of a wave does not exceed the maximum allowed size.
#'
#' @param n integer, size to be checked
#' @param nmax integer, maximum allowed size
#' @return nothing - just stops execution with an error message if n>nmax
checkMaxSize <- function(n,nmax){
  if(n>nmax){
    mess=paste0("Max size exceeded: ",
                "length of sequenced wave ",
                "[n=",n,"; approx. size:",round(2*n*8/2^20)," Mb] ",
                "exceeds nmax [",nmax,"]. ",
                "Either increase nmax or decrease sampling rate.")
    stop(mess,call.=FALSE)
  }
}

