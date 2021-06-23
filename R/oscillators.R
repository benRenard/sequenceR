#' Sine oscillator
#'
#' Creates a soundSample using a sine oscillator.
#' @param freq Numeric, note frequency in Hz
#' @param duration Numeric, note duration in second
#' @param phase Numeric, phase in radians (typically between 0 and 2*pi)
#' @param rate Numeric, sampling rate in Hz
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- oscillator_sine(freq=220,duration=0.1)
#' plot(sam)
#' # listen(sam) # uncomment to listen
#' @export
oscillator_sine <- function(freq=440,duration=1,phase=0,rate=44100){
  time=timeVector(duration,rate)
  wave=sin(phase+2*pi*time*freq)
  return(soundSample(wave,rate))
}

#' Square oscillator
#'
#' Creates a soundSample using a square oscillator.
#' @param freq Numeric, note frequency in Hz
#' @param duration Numeric, note duration in second
#' @param phase Numeric, phase in radians (typically between 0 and 2*pi)
#' @param rate Numeric, sampling rate in Hz
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- oscillator_square(freq=220,duration=0.1)
#' plot(sam)
#' # listen(sam) # uncomment to listen
#' @export
oscillator_square <- function(freq=440,duration=1,phase=0,rate=44100){
  w=oscillator_sine(freq,duration,phase,rate)
  wave=sign(w$wave)
  return(soundSample(wave,rate))
}

#' Triangle oscillator
#'
#' Creates a soundSample using a triangle oscillator.
#' @param freq Numeric, note frequency in Hz
#' @param duration Numeric, note duration in second
#' @param phase Numeric, phase in radians (typically between 0 and 2*pi)
#' @param rate Numeric, sampling rate in Hz
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- oscillator_triangle(freq=220,duration=0.1)
#' plot(sam)
#' # listen(sam) # uncomment to listen
#' @export
oscillator_triangle <- function(freq=440,duration=1,phase=0,rate=44100){
  w=oscillator_sine(freq,duration,phase,rate)
  wave=(2/pi)*asin(w$wave)
  return(soundSample(wave,rate))
}

#' Saw oscillator
#'
#' Creates a soundSample using a saw oscillator.
#' @param freq Numeric, note frequency in Hz
#' @param duration Numeric, note duration in second
#' @param phase Numeric, phase in radians (typically between 0 and 2*pi)
#' @param rate Numeric, sampling rate in Hz
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- oscillator_saw(freq=220,duration=0.1)
#' plot(sam)
#' # listen(sam) # uncomment to listen
#' @export
oscillator_saw <- function(freq=440,duration=1,phase=0,rate=44100){
  time=timeVector(duration,rate)
  wave=(-2/pi)*atan(1/tan(phase+pi*(time-floor(time))*freq))
  return(soundSample(wave,rate))
}

#' General oscillator
#'
#' Creates a soundSample using a oscillator.
#' @param type String, oscillator type, one of 'sine', 'saw', 'square' or 'triangle'.
#'     If an unknowm string is provided, a sine oscillator will be used.
#' @param freq Numeric, note frequency in Hz
#' @param duration Numeric, note duration in second
#' @param phase Numeric, phase in radians (typically between 0 and 2*pi)
#' @param rate Numeric, sampling rate in Hz
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- oscillator(type='saw',freq=220,duration=0.1)
#' plot(sam)
#' # listen(sam) # uncomment to listen
#' @export
oscillator <- function(type='sine',freq=440,duration=1,phase=0,rate=44100){
  switch(type,
         sine=oscillator_sine(freq,duration,phase,rate),
         square=oscillator_square(freq,duration,phase,rate),
         saw=oscillator_saw(freq,duration,phase,rate),
         triangle=oscillator_triangle(freq,duration,phase,rate),
         oscillator_sine(freq,duration,phase,rate))
}

#' Pattern-based oscillator
#'
#' Creates a soundSample by repeating a user-provided pattern.
#' @param pattern Numeric vector, pattern.
#' @param freq Numeric, note frequency in Hz
#' @param duration Numeric, note duration in second
#' @param rate Numeric, sampling rate in Hz
#' @return An object of class 'soundSample'.
#' @examples
#' sam <- oscillator_pattern(pattern=airquality$Ozone,freq=110,duration=0.1)
#' plot(sam)
#' # listen(sam) # uncomment to listen
#' @export
oscillator_pattern <- function(pattern,freq=440,duration=1,rate=44100){
  time=seq(0,1/freq,1/rate) # time vector for a single pattern
  # regrid pattern on time vector
  regrid=stats::approx(x=seq(0,time[length(time)],length.out=length(pattern)),
             y=pattern,xout=time)
  pat=rescale(regrid$y,-1,1)
  wave=rep_len(pat,floor(duration*rate))
  return(soundSample(wave,rate))
}
