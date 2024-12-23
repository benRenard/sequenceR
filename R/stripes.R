#' Climate stripes sonification
#'
#' Sonification of climate stripes data, or more generally, of a time series of values.
#' A smoothed version of the time series is computed by moving average, then sonification
#' proceeds as follows:
#' \itemize{
#' \item Backtrack is a standard house-like tune, including a four-on-the-floor
#'      kick+hi-hat pattern on the drum, a bass following the drum kick, and 3 chords
#'      played by a synthesizer
#' \item The smoothed time series controls the master volume and the amount of 'distortion'
#'      in the synthesizer's sound
#' \item Large anomalies below / above the smoothed series trigger percussion sounds (by default
#'      a snare and a hand clap) that are panned full left (negative anomalies) and full
#'      right (positive anomalies)
#' }
#'
#' @param values Numeric vector, values to sonify. Default is global temperature anomalies over the period 1850-2021
#' @param bpm Numeric > 0, tempo in beat per minute
#' @param minVol Numeric >= 0, minimum volume reached when smoothed series is minimum
#' @param nma Numeric >=0 , number of moving average steps on each side of the current value
#'          (i.e. moving average window is 2*nma+1 when possible, nma+1 on the series' edges)
#' @param pClap Numeric in (0,0.5). "Large" anomalies triggering claps/snare are defined
#'    as anomalies below (resp. above) the pClap (resp. (1-pClap))-quantile of anomalies.
#' @param synthVar Numeric >= 0 , controls the variability of the synthesizer sound.
#'    When zero, the synthesizer sound does not change.
#'    Large values induce more variability in the synthesizer sound.
#' @param kick soundSample, sound sample used to play the kick drum.
#' @param hihat soundSample, sound sample used to play the closed hi-hat.
#' @param openHihat soundSample, sound sample used to play the open hi-hat.
#' @param posPercussion soundSample, sound sample used to play the positive-anomaly percussion.
#' @param negPercussion soundSample, sound sample used to play the negative-anomaly percussion.
#' @param bassNote string, bass note (in \href{https://en.wikipedia.org/wiki/Scientific_pitch_notation}{scientific pitch notation}).
#' @param chord1 string vector, first chord played by synthesizer.
#' @param chord2 string vector, second chord played by synthesizer.
#' @param chord3 string vector, third chord played by synthesizer.
#' @param videoFile file path, full path to video file. When NULL, video is not created.
#' @param videoResFactor Numeric > 0 , video resolution, 2 recommended for good-quality video.
#' @return A list with the following components:
#'\itemize{
#'  \item mix, tuneR::Wave object, the final mix of the sonification.
#'  \item dat, data frame with 4 columns: time step, raw value, smoothed value, anomaly
#'  \item quantiles, numeric vector of size 2, the quantiles defining large negative/positive anomalies
#'  \item waves, list of tuneR::Wave object, individual waves for each instrument in case you wish to
#'        mix them in your own way.
#'}
#' @examples
#' w <- sonifyStripes()
#' @export
sonifyStripes=function(values=globalT$Anomaly,bpm=135,minVol=0.1,nma=10,pClap=0.15,synthVar=0.5,
                       kick=mini909$bass,hihat=mini909$hihat,openHihat=mini909$hihat_o,
                       posPercussion=mini909$snare,negPercussion=mini909$clap,
                       bassNote='E1',
                       chord1=c('E2','E3','G3','D4','Gb4'),
                       chord2=c('E2','D3','Gb3','A3','E4'),
                       chord3=c('E2','B2','Gb3','G3','D4'),
                       videoFile=NULL,videoResFactor=1){
  spb=60/bpm # seconds per beat
  spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
  range_vol=c(minVol,1)
  n=length(values)
  ix=1:n
  # Compute moving averages
  ma=rep(NA,n)
  for(i in 1:n){
    foo=values
    foo[ix>i+nma | ix<i-nma]=NA
    ma[i]=mean(foo,na.rm = TRUE)
  }
  # Map moving average to master volume
  spts=rep(spt,length(values))
  tim=cumsum(spts)-spt
  master=rescale(ma,range_vol[1],range_vol[2])
  # Get drum kick - 4 in the floor !
  vol=master*rep_len(c(1,0,0,0),length(tim))
  ki=sequence(kick,time=tim,volume=vol)
  # Get open HiHat on 3rd 16th note
  vol=master*rep_len(c(0,0,1,0),length(tim))
  hho=sequence(openHihat,time=tim,volume=vol)
  # Get closed HiHat
  vol=master*rep_len(c(0,1,0,0.5),length(tim))
  hh=sequence(hihat,time=tim,volume=vol)
  # Get bass on top of the kick drum
  if(!is.null(bassNote)){
    bass=getSynthNote(getFrequencies(bassNote))
    vol=master*rep_len(c(1,0,0,0),length(tim))
    ba=sequence(bass,time=tim,volume=vol)
  } else {
    ba=sequence(kick,time=0,volume=0)
  }
  # Get percussions for strong anomalies below / above moving average
  anom=values-ma
  qs=quantile(anom,probs=c(pClap,1-pClap))
  if(diff(range(qs))>0){
    volPos=master*rescale((anom-qs[2])*(anom > qs[2]))
    volNeg=master*rescale((qs[1]-anom)*(anom < qs[1]))
  } else {
    volPos=volNeg=0*master
  }
  posP=sequence(posPercussion,time=tim,volume=volPos)
  negP=sequence(negPercussion,time=tim,volume=volNeg)

  # Get keybords
  types=c('sine','square') # oscillator used to create keyboard
  key=vector('list',length(types))
  dur=mean(spt)*4 # duration of each chords
  for(i in 1:length(types)){
    type=types[i]
    # Get sounds for the three chords
    inst=getSynth(unique(chord1),peak=0,decay=dur,duration=dur,type=type,sustain=0)
    i0=play.instrument(inst,time=rep(0,length(chord1)),fadeout = rep(Inf,length(chord1)))
    C1=as.soundSample(i0)
    inst=getSynth(unique(chord2),peak=0,decay=dur,duration=dur,type=type,sustain=0)
    i0=play.instrument(inst,time=rep(0,length(chord2)),fadeout = rep(Inf,length(chord2)))
    C2=as.soundSample(i0)
    inst=getSynth(unique(chord3),peak=0,decay=dur,duration=dur,type=type,sustain=0)
    i0=play.instrument(inst,time=rep(0,length(chord3)),fadeout = rep(Inf,length(chord3)))
    C3=as.soundSample(i0)
    chords=instrument(list(C1,C2,C3))
    # Get rythm
    foo=rep_len(c(0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,
                  0,0,2,0,0,2,0,0,2,0,0,2,0,0,2,0,
                  0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,
                  0,0,3,0,0,3,0,0,3,0,2,0,0,2,0,0),length(tim))
    # Assemble everything
    mask=foo>0
    key[[i]]=play.instrument(chords,notes=as.integer(foo[mask]),time=tim[mask],
                             volume=master[mask]^(i^synthVar),fadeout = rep(Inf,sum(mask)))
  }
  # Final mix
  final=mix(waves=c(list(ki,ki,hh,hho,posP,negP,ba,key[[1]],key[[2]])),
            volume=c(1,1,0.25,0.65,1,0.8,0.75,0.65,0.5),
            pan=c(-0.5,0.5,0,0,1,-1,0,0,0))
  # Useful data
  dat=data.frame(time=ix,value=values,movingAverage=ma,anomaly=anom)
  # Create video is required
  if(!is.null(videoFile)){
    wavFile=tempfile()
    tuneR::writeWave(final,wavFile)
    ymin=min(dat$value);ymax=max(dat$value)
    makeplot <- function(alfa=0.6,trail=20){
      for(tstep in 1:(NROW(dat)+trail)){
        message(paste0('Creating image ',tstep,'/',NROW(dat)+trail))
        dd=dat[1:tstep,]
        g=ggplot2::ggplot(dd)+
          ggplot2::geom_rect(ggplot2::aes(xmin=time-0.5,xmax=time+0.5,ymin=ymin,ymax=ymax,fill=value))+
          ggplot2::scale_fill_distiller(palette='RdBu',limits=range(dat$value))+
          ggplot2::geom_line(ggplot2::aes(x=time,y=movingAverage),alpha=alfa)+
          ggplot2::geom_segment(ggplot2::aes(x=time,y=movingAverage,yend=value),linewidth=2,alpha=alfa)+
          ggplot2::geom_point(data=dd[ dd$anomaly < qs[1],],ggplot2::aes(x=time,y=value,size=qs[1]-anomaly),alpha=alfa)+
          ggplot2::geom_point(data=dd[ dd$anomaly > qs[2],],ggplot2::aes(x=time,y=value,size=anomaly-qs[2]),alpha=alfa)+
          ggplot2::scale_size(range=c(0,8),limits=c(0,max(qs[1]-dat$anomaly,dat$anomaly-qs[2])))+
          ggplot2::xlim(range(dat$time)+c(-1,1))+ggplot2::ylim(range(dat$value))+
          ggplot2::theme_void()+ggplot2::theme(legend.position='none')
        print(g)
      }
    }
    av::av_capture_graphics(makeplot(),output=videoFile,audio=wavFile,framerate=1/spt,
              res=72*videoResFactor, width=1280*videoResFactor,height=720*videoResFactor)
  }
  return(list(mix=final,dat=dat,quantiles=qs,
              waves=list(kick=ki,hihat=hh,openHihat=hho,
                         posPercussion=posP,negPercussion=negP,
                         bass=ba,key1=key[[1]],key2=key[[2]])))
}

