#***************************************************************************----
# Constructor ----
#' Envelope constructor.
#'
#' Creates a new instance of an 'envelope' object
#' (https://en.wikipedia.org/wiki/Envelope_(music)).
#' In this package an envelop is viewed as a curve v(t),
#' where t is the time and v the value of the envelope.
#' Time t is normalized between 0 and 1 so that 1 corresponds
#' to the end of the sound sample the envelope is applied to
#' (and 0 to its beginning).
#' The curve is defined by a discrete set of points (t,v) (linear interpolation in between).
#'
#' @param t Numeric vector, normalized time. Vector of increasing values starting at 0 and ending at 1.
#' @param v Numeric vector, same size as t, envelop values v(t).
#' @return An object of class 'envelope'.
#' @examples
#' # A triangular envelop
#' env <- envelope(t=c(0,0.3,1),v=c(0,1,0))
#' # An ADSR envelope (https://en.wikipedia.org/wiki/Envelope_(music)#ADSR)
#' env <- envelope(t=c(0,0.1,0.3,0.8,1),v=c(0,1,0.4,0.4,0))
#' # An envelope that could be used for a 1-octave frequency modulation (from 440 to 220 Hz)
#' env <- envelope(t=c(0,1),v=c(440,220))
#' # An envelope that could be used for phase modulation
#' # (https://en.wikipedia.org/wiki/Phase_modulation)
#' env <- envelope(t=seq(0,1,0.01),v=(-pi/2)*sin(2*pi*4*seq(0,1,0.01)))
#' @export
envelope <- function(t,v){
  o<-new_envelope(t,v)
  return(validate_envelope(o))
}

#***************************************************************************----
# Envelope utilities ----
#
#' Plot
#'
#' Plot an envelope.
#'
#' @param x envelope object.
#' @param ... further arguments passed to the base plot function.
#' @return nothing - plotting function.
#' @examples
#' # Define envelope
#' env <- envelope(t=c(0,0.1,0.3,0.8,1),v=c(0,1,0.4,0.4,0))
#' # plot it
#' plot(env)
#' @method plot envelope
#' @export
plot.envelope <- function(x,...){
  plot(x$t,x$v,type='b',xlab='normalized time',ylab='envelope',pch=19,...)
}

#***************************************************************************----
# internal constructor ----
new_envelope<-function(t,v){
  stopifnot(is.numeric(t))
  stopifnot(is.numeric(v))
  o <- list(t=t,v=v)
  class(o) <- 'envelope'
  return(o)
}

#***************************************************************************----
# validator ----
validate_envelope<-function(e){
  if( length(e$t) != length(e$v)){
    mess=paste0("Invalid envelope: t and v should have the same length")
    stop(mess,call.=FALSE)
  }
  if( (min(e$t)!=0) | (max(e$t)!=1) | any(diff(e$t)<0) ){
    mess=paste0("Invalid envelope: value in t are not increasing between 0 and 1")
    stop(mess,call.=FALSE)
  }
  return(e)
}

