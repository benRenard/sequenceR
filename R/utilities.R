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
#' # Uncomment to play the result
#' # play(m1)
#'
#' # Try with less hihat, more kick
#' m2 <- mix(list(hh,k,s),volume=c(0.3,1,0.8))
#' # Uncomment to play the result
#' # play(m2)
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
#' Transforms a pan (between -1 for full left and 1 for full right) into right/left
#' normalized volumes between 0 and 1
#' @param pan Numeric between -1 and 1
#' @return a list of length 2 with fields left and right.
#' @keywords internal
pan2vol<-function(pan,channel){
  out <- list(left=abs(-1+pan)/2,right=(1+pan)/2)
  return(out)
}

#***************************************************************************----
# Rescale ----
#
#' Rescale function
#' Rescale a series between two bounds
#' @param x Numeric vector
#' @param low Numeric, lower bound
#' @param high Numeric, higher bound
#' @return a rescaled numeric vector
#' @export
rescale<-function(x,low=0,high=1){
  mini <- min(x,na.rm=T)
  maxi <- max(x,na.rm=T)
  u <- (x-mini)/(maxi-mini) # between 0 and 1
  out <- low+(high-low)*u #between low and high
  return(out)
}

#***************************************************************************----
# Miscellaneous private utilities ----

#' timeVector function
#' Compute the time vector starting from 0 associated with a duration and a sampling rate
#' @param duration Numeric
#' @param rate Numeric
#' @return a numeric vector
timeVector <- function(duration=1,rate=44100){
  return(seq(0,duration,1/rate))
}

#' Check sequencer arguments
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
#' Check that the size of a wave does not exceed the maximum allowed size.
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

