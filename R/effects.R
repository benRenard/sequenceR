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
