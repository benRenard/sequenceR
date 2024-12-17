sonifyStripes=function(dat=globalT,bpm=135,minVol=0.1,nma=10,
                       pClap=0.15,randomness=0.025,
                       kick=mini909$bass,hihat=mini909$hihat,openHihat=mini909$hihat_o,
                       posPercussion=mini909$snare,negPercussion=mini909$clap,
                       bassNote='E1',
                       chord1=c('E2','E3','G3','D4','Gb4'),
                       chord2=c('E2','D3','Gb3','A3','E4'),
                       chord3=c('E2','B2','Gb3','G3','D4'),doVariant=FALSE){
  spb=60/bpm # seconds per beat
  spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
  range_vol=c(minVol,1)

  years=dat[,1]
  values=dat[,2]
  n=length(values)
  ix=1:n
  ma=rep(NA,n)
  for(i in 1:n){
    foo=values
    foo[ix>i+nma | ix<i-nma]=NA
    ma[i]=mean(foo,na.rm = TRUE)
  }
  plot(years,values)
  lines(years,ma)

  spts=rep(spt,length(values))*rlnorm(n,0,0)
  tim=cumsum(spts)-spt
  master=rescale(ma,range_vol[1],range_vol[2])

  vol=master*rep_len(c(0,1,0,0.5),length(tim))
  hh=sequence(hihat,time=tim,volume=vol)

  vol=master*rep_len(c(0,0,1,0),length(tim))
  hho=sequence(openHihat,time=tim,volume=vol)

  vol=master*rep_len(c(1,0,0,0),length(tim))
  ki=sequence(kick,time=tim,volume=vol)

  if(!is.null(bassNote)){
    bass=getSynthNote(getFrequencies(bassNote))
    vol=master*rep_len(c(1,0,0,0),length(tim))
    ba=sequence(bass,time=tim,volume=vol)
  } else {
    ba=sequence(kick,time=0,volume=0)
  }

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

  if(doVariant){dur=mean(spt)*36} else{dur=mean(spt)*4}
  expo=0.5
  types=c('sine','square')
  key=vector('list',length(types))
  for(i in 1:length(types)){
    type=types[i]
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
    if(doVariant){
      foo=rep_len(c(1,rep(0,15),2,rep(0,15),3,rep(0,25),2,rep(0,5)),length(tim))
    } else {
      foo=rep_len(c(0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,
                    0,0,2,0,0,2,0,0,2,0,0,2,0,0,2,0,
                    0,0,3,0,0,3,0,0,3,0,0,3,0,0,3,0,
                    0,0,3,0,0,3,0,0,3,0,2,0,0,2,0,0),length(tim))
    }
    mask=foo>0
    key[[i]]=play.instrument(chords,notes=as.integer(foo[mask]),time=tim[mask],
                             volume=master[mask]^(i^expo),fadeout = rep(Inf,sum(mask)))
  }

  final=mix(waves=c(list(ki,ki,hh,hho,posP,negP,ba,key[[1]],key[[2]])),
            volume=c(1,1,0.25,0.65,1,0.8,0.75,0.65,0.5),
            pan=c(-0.5,0.5,0,0,1,-1,0,0,0))
  return(list(mix=final,data=data.frame(year=years,value=values,movingAverage=ma,anomalies=anom),
              waves=list(kick=ki,hihat=hh,openHihat=hho,
                         posPercussion=posP,negPercussion=negP,
                         bass=ba,key1=key[[1]],key2=key[[2]])))
}

w=sonifyStripes(nma=50)
writeWave(w$mix,'temp.wav')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav','stripes.mp3'))
file.remove('temp.wav')






# range_bpm=c(100,140)
# range_master=c(0.2,1)
# range_bass=c(0,1)
# range_hihat=c(0.2,1)
#
# spb=60/range_bpm # seconds per beat
# spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
# style='major'
# linkNotes=TRUE
# volPar=2
#
# bassNote='E1'
# oneNote='E3'
#
# if(style=='oriental'){
#   notes=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
# } else if(style=='major'){
#   notes=c('E2','A2','Db3','E3','B3','Eb4','E4','A4')
# } else if(style=='minor'){
#   notes=c('E2','G2','B2','D3','Gb3','A3','C4',
#           'E4','G4','B4','D5','Gb5','A5','C5','E5')
#   # notes=c('E3','B3','E4','B4','D4','E4','G4','B4')
# } else if(style=='lydian'){
#   notes=c('E3','Ab3','Bb3','B3','Db4','E4','Ab4','Bb4','Bb4')
# } else if(style=='penta'){
#   notes=c('E3','G3','A3','B3','D4','E4','G4','A4','B4','D5','E5')
# } else if(style=='oneNote'){
#   notes=c('E3','E3')
# }
#
# inst1=getSynth(unique(notes),type='triangle',sustain=0.05)
# inst2=getSynth(unique(notes),sustain=0.05)
#
# bass=getSynth(bassNote,duration=3,sustain=0.05)
#
#
#
# dat=globalT #[globalT$Year %in% 1951:2001,]
# years=dat[,1]
# values=dat[,2]
# is.anomaly=TRUE
# mask= !is.na(years) & !is.na(values)
# x=years[mask];y=values[mask]
# if(!is.anomaly){
#   y=(y-mean(y))/sd(y)
# }
# ix=1:length(y)
# nma=7
# ma=rep(NA,length(y))
# for(i in 1:length(y)){
#   foo=y;
#   foo[ix>i+nma | ix<i-nma]=NA
#   ma[i]=mean(foo,na.rm = TRUE)
# }
# plot(x,y)
# lines(x,ma)
#
# master=rescale(ma,range_master[1],range_master[2])
#
# spts=rescale(y,spt[1],spt[2])
# tim=cumsum(spts)-spts[1]
# pitch=rescale(y-ma)
# ix=as.integer(rescale(pitch)*(length(notes)-1))+1
# vol=master*rescale(y-ma,0,1)^volPar
# if(linkNotes){
#   mask=c(TRUE,diff(ix)!=0)
# } else {
#   mask=rep(TRUE,length(vol))
# }
# i1=play.instrument(inst1,notes=notes[ix[mask]],time=tim[mask],
#                    volume=vol[mask],
#                    fadeout=rep(Inf,sum(mask)))
# vol=master*rescale(y-ma,1,0)^volPar
# i2=play.instrument(inst2,notes=notes[ix[mask]],time=tim[mask],
#                    volume=vol[mask],
#                    fadeout=rep(Inf,sum(mask)))
#
# pattern=c(1,0.1,0.02,0.1)
# vol=rep_len(pattern,length(tim))
# cy=sequence(ride,time=tim,volume=vol)
#
# mask=y<ma
# tim2=tim[mask]
# vol=master[mask]*rescale((y-ma)[mask],range_bass[2],range_bass[1])^volPar
# dk=sequence(kick,time=tim2,volume=vol)
# ba=play.instrument(bass,notes=as.integer(rep(1,length(tim2))),
#                    time=tim2,volume=vol,
#                    fadein=rep(0.01,length(tim2)),
#                    fadeout=rep(Inf,length(tim2)))
#
# mask=y>ma
# tim2=tim[mask]
# vol=master[mask]*rescale((y-ma)[mask],range_hihat[1],range_hihat[2])
# hh=sequence(hiHat,time=tim2,volume=vol)
#
# final=mix(waves=c(list(i1,i2,cy,dk,ba,hh)),volume=c(1,1,1,1,1,1),pan=c(-1,1,0,0,0,0))
#
# writeWave(final,'temp.wav')
# system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav','stripes.mp3'))
# file.remove('temp.wav')
#
#
# #######
# # OLD VERSION
# ######
#
# # library(ggplot2);library(gganimate)
# # loadInstruments <- function(instdir=file.path('instruments')){
# #   load(file.path(instdir,'bassStandup.RData'))
# #   bass=bassStandup
# #   load(file.path(instdir,'guitarHarmonics.RData'))
# #   guitar=guitarHarmonics
# #   load(file.path(instdir,'pianoSteinway.RData'))
# #   piano=pianoSteinway
# #   load(file.path(instdir,'drumkitStahl.RData'))
# #   drum=drumkitStahl
# #   load(file.path(instdir,'hangDrum.RData'))
# #   return(list(bass=bass,drum=drum,piano=piano,guitar=guitar,hangDrum=hangDrum))
# # }
# #
# # # inst=loadInstruments()
# # bpm=90
# # style='oriental'
# # drum=inst$drum
# # inst1=inst$piano
# # inst2=inst$hangDrum
# # inst3=inst$bass
# # p1=0.35
# # p2=0.35
# # p3=0.25
# # pCymbals=0.02
# # minBassVol=0.4
# # minCymbalsVol=0.4
# # dat=globalT[globalT$Year %in% 1951:2001,]
# # t0=0
# # fname='stripes'
# # invertMapping=FALSE
# #
# # spb=60/bpm # seconds per beat
# # spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
# #
# # if(style=='oriental'){
# #   n1=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
# #   n2=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
# #   n3=c('E1','G1','A1','B1','C2','Eb2','E2','Gb2','G2','A2','B2','C3')
# # } else if(style=='major'){
# #   n1=c('E3','E3','B3','Eb4','E4','Ab4','B4','B4')
# #   n2=c('E3','E3','B3','Eb4','E4','Ab4','B4','B4')
# #   n3=c('E1','Ab1','A1','B1','Db2','E2','Ab2','A2','B2','Db3')
# # } else if(style=='minor'){
# #   n1=c('E3','B3','E4','B4','D4','E4','G4','B4')
# #   n2=c('E3','B3','E4','B4','D4','E4','G4','B4')
# #   n3=c('E1','G1','A1','B1','D2','E2','G2','A2','B2','D3')
# # } else if(style=='lydian'){
# #   n1=c('E3','Ab3','Bb3','B3','Db4','E4','Ab4','Bb4','Bb4')
# #   n2=c('E3','Ab3','Bb3','B3','Db4','E4','Ab4','Bb4','Bb4')
# #   n3=c('E1','E1','B1','E2','B2','E3','E3')
# # }
# #
# # inst1=createSynth(unique(n1),duration=3,sustain=0.05)
# # inst2=createSynth(unique(n2),type='triangle',duration=4,sustain=0.05)
# # inst3=createSynth(unique(n3),duration=10)
# #
# # x0=dat[,1]
# # y0=dat[,2]
# #
# # # x0=1:100
# # # y0=rnorm(100)
# # # y0=rescale(1:NROW(x0),-1,1)
# # # Remove NAs
# # mask0= !is.na(x0) & !is.na(y0)
# # x0=x0[mask0];y0=y0[mask0]
# # # 2DO: check x and y vary
# # # Sort and normalize
# # foo=sort.int(x0,index.return=TRUE)
# # x=x0[foo$ix];y=y0[foo$ix];
# # x=rescale(x)
# # if(invertMapping) y=-1*y
# #
# # tstep=diff(range(x))/(NROW(x)-1)
# #
# # tim0=t0+(x/tstep)*spt
# # timregular=t0+((1:NROW(x))-1)*spt
# #
# # # Instrument 1 highlights positive values
# # thresh=quantile(y[y>=0],1-p1)
# # if(is.na(thresh)){
# #   tim=tim0[1]
# #   ix=1
# #   vol=0
# # } else {
# #   mask=y>=thresh
# #   tim=tim0[mask]
# #   pitch=rescale(y[mask])
# #   ix=as.integer(rescale(pitch)*(length(n1)-1))+1
# #   vol=rescale(y[mask])
# # }
# #
# # i1=play.instrument(inst1,notes=n1[ix],time=tim,
# #                    volume=vol,fadeout=max(ix)-ix+1)
# #
# # # Crash highlights very high positive values
# # thresh=quantile(y[y>=0],1-pCymbals)
# # if(is.na(thresh)){
# #   td=tim0[1];vd=0;nd='crash'
# # } else {
# #   mask=y>=thresh
# #   td=tim0[mask]
# #   vd=rescale(y[mask],minCymbalsVol,1)
# #   if(any(is.na(vd))) vd=rep(1,sum(mask))
# #   nd=rep('crash',sum(mask))
# # }
# #
# # drum_crash=play.instrument(drum,notes=nd,time=td,volume=vd)
# #
# # # Instrument 2 drum highlights low values
# # thresh=quantile(y[y<=0],p2)
# # if(is.na(thresh)){
# #   tim=tim0[1]
# #   ix=1
# #   vol=0
# # } else {
# #   mask=y<=thresh
# #   tim=tim0[mask]
# #   pitch=rescale(y[mask])
# #   ix=as.integer(rescale(pitch)*(length(n2)-1))+1
# #   vol=rescale(y[mask],1,0)
# # }
# # i2=play.instrument(inst2,notes=n2[ix],time=tim,
# #                    volume=vol,fadeout=0*ix+Inf)
# # drum_bass=play.instrument(drum,notes=rep('bass',length(tim)),
# #                           time=tim,volume=vol)
# #
# # # Splash highlights very low negative values
# # thresh=quantile(y[y<=0],pCymbals)
# # if(is.na(thresh)){
# #   td=tim0[1];vd=0;nd='china'
# # } else {
# #   mask=y<=thresh
# #   td=tim0[mask]
# #   vd=rescale(y[mask],minCymbalsVol,1)
# #   if(any(is.na(vd))) vd=rep(1,sum(mask))
# #   nd=rep('china',sum(mask))
# # }
# #
# # drum_splash=play.instrument(drum,notes=nd,time=td,volume=vd)
# #
# # # Instrument 3 (Bass) highlights both low and high values
# # thresh=quantile(abs(y),1-p3)
# # mask=abs(y)>=thresh
# # tim=tim0[mask]
# # pitch=rescale(y[mask])
# # ix=as.integer(rescale(pitch)*(length(n3)-1))+1
# # vol=rescale(abs(y)[mask],minBassVol,1)
# # i3=play.instrument(inst3,notes=n3[ix],time=tim,volume=vol,
# #                      fadein=rep(0.1,length(ix)),fadeout=rep(0.5,length(ix)))
# # m=y[mask]>=0
# # if(sum(m)==0){
# #   td=tim0[1];vd=0;nd='snare'
# # } else
# # {
# #   td=tim[m];vd=vol[m]
# #   nd=rep('snare',sum(m))
# # }
# # drum_snare=play.instrument(drum,notes=nd,time=td,volume=vd)
# #
# # m=y[mask]<=0
# # if(sum(m)==0){
# #   td=tim0[1];vd=0;nd='bass'
# # } else
# # {
# #   td=tim[m];vd=vol[m]
# #   nd=rep('bass',sum(m))
# # }
# # drum_bass=play.instrument(drum,notes=nd,time=td,volume=vd)
# #
# # # Drumkit:  hihat and ride
# # tim=timregular[rep_len(c(F,F,T,F),length(timregular))]
# # vol=0*tim+1
# # drum_hh=play.instrument(drum,notes=rep('hihat_f',length(tim)),time=tim,volume=vol)
# # tim=timregular[rep_len(c(T,F,F,T),length(timregular))]
# # vol=rep_len(c(0.1,0.02),length(tim))
# # drum_ride=play.instrument(drum,notes=rep('ride',length(tim)),time=tim,volume=vol,
# #                           fadeout=rep(Inf,length(tim)))
# #
# # final=mix(waves=c(list(i1,i2,i3,drum_bass,drum_snare,drum_ride,drum_hh,drum_crash,drum_splash)),
# #           vol=c(0.7,1,1,1,1,0.7,0.3,0.5,0.5),pan=c(-0.9,0.9,0,0,0,-0.7,0.7,0,0))
# #
# # writeWave(final,'temp.wav')
# # system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',file.path(paste0(fname,'.mp3'))))
# # file.remove('temp.wav')
# #
# # DF=data.frame(indx=1:NROW(x),x=x,y=1,time=x0,value=y0)
# # DF=rbind(DF,
# #          data.frame(indx=NROW(x)+(1:40),x=max(x),y=1,time=max(DF$time),value=DF$value[NROW(x)]))
# # g=ggplot(DF)+geom_tile(aes(x,y,fill=value,group=indx))+
# #   scale_fill_distiller(palette='RdBu',guide=NULL)+
# #   theme_void()+
# #   transition_reveal(indx)+
# #   shadow_wake(1,wrap=FALSE)
# #
# # animate(g, renderer = av_renderer(file.path(paste0(fname,'.mp4')),
# #                                   audio = file.path(paste0(fname,'.mp3'))),
# #         width=1280,height=360,res=104,fps=1/spt,nframes=NROW(DF))
# #
# #
# #
# # # inst=pianoSteinway
# # # foo=globalT
# # # values=foo$Anomaly
# # # times=foo$Year
# # # chord=c('E1','E2') #c('E1','B1','E2','B2','E3')
# # # gamme=c('Db','Eb','E','Gb','Ab','Bb','B')
# # # notes=c(paste0(gamme,2),paste0(gamme,3),paste0(gamme,4),paste0(gamme,5))
# # # duration=10
# # #
# # # n=length(values)
# # # pitches=pitchMapping(values,notes)
# # # mask= rep(TRUE,n)  # c(TRUE,pitches[1:(n-1)]!=pitches[2:n]) #
# # #
# # # ch=play.instrument(inst,
# # #                    notes=chord,
# # #                    time=rep(0,length(chord)),
# # #                    fadeout = rep(Inf,length(chord)))
# # # track1=sequence(as.soundSample(ch),seq(0,duration,8*duration/n))
# # #
# # #
# # # track2=play.instrument(inst,
# # #                     notes=pitches[mask],
# # #                     time=rescale(times[mask],0,duration),
# # #                     volume=rescale(-1*values[mask]),
# # #                     pan=rescale(values[mask],-1,1),
# # #                     fadein=rep(2*duration/n,sum(mask)),
# # #                     fadeout=rep(30*duration/n,sum(mask)))
# # # final=mix(list(track1,track2),volume=c(0.5,1))
# # # play(final)
# #
# #
# # # values=globalT$Anomaly
# # # f0=110
# # # duration=5
# # # type='sine'
# # # freqRatio=c(1/2,2/3,1:6,8)
# # # h=vector('list',length(freqRatio))
# # # tim=rescale(1:length(values))
# # # for (i in 1:length(freqRatio)){
# # #   h[[i]]=oscillator(type,freqRatio[i]*f0,duration,phase=0,rate=44100)
# # #   E=envelope(tim,rescale(values))
# # #   if(i>3)  h[[i]]=applyEnvelope(h[[i]],E)
# # #   h[[i]]=as.Wave(h[[i]])
# # # }
# # # note=mix(h)
# # # plot(note)
# # # play(note)
