# library(ggplot2);library(gganimate)
# loadInstruments <- function(instdir=file.path('instruments')){
#   load(file.path(instdir,'bassStandup.RData'))
#   bass=bassStandup
#   load(file.path(instdir,'guitarHarmonics.RData'))
#   guitar=guitarHarmonics
#   load(file.path(instdir,'pianoSteinway.RData'))
#   piano=pianoSteinway
#   load(file.path(instdir,'drumkitStahl.RData'))
#   drum=drumkitStahl
#   load(file.path(instdir,'hangDrum.RData'))
#   return(list(bass=bass,drum=drum,piano=piano,guitar=guitar,hangDrum=hangDrum))
# }
#
# # inst=loadInstruments()
# bpm=90
# style='oriental'
# drum=inst$drum
# inst1=inst$piano
# inst2=inst$hangDrum
# inst3=inst$bass
# p1=0.35
# p2=0.35
# p3=0.25
# pCymbals=0.02
# minBassVol=0.4
# minCymbalsVol=0.4
# dat=globalT[globalT$Year %in% 1951:2001,]
# t0=0
# fname='stripes'
# invertMapping=FALSE
#
# spb=60/bpm # seconds per beat
# spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
#
# if(style=='oriental'){
#   n1=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
#   n2=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
#   n3=c('E1','G1','A1','B1','C2','Eb2','E2','Gb2','G2','A2','B2','C3')
# } else if(style=='major'){
#   n1=c('E3','E3','B3','Eb4','E4','Ab4','B4','B4')
#   n2=c('E3','E3','B3','Eb4','E4','Ab4','B4','B4')
#   n3=c('E1','Ab1','A1','B1','Db2','E2','Ab2','A2','B2','Db3')
# } else if(style=='minor'){
#   n1=c('E3','B3','E4','B4','D4','E4','G4','B4')
#   n2=c('E3','B3','E4','B4','D4','E4','G4','B4')
#   n3=c('E1','G1','A1','B1','D2','E2','G2','A2','B2','D3')
# } else if(style=='lydian'){
#   n1=c('E3','Ab3','Bb3','B3','Db4','E4','Ab4','Bb4','Bb4')
#   n2=c('E3','Ab3','Bb3','B3','Db4','E4','Ab4','Bb4','Bb4')
#   n3=c('E1','E1','B1','E2','B2','E3','E3')
# }
#
# inst1=createSynth(unique(n1),duration=3,sustain=0.05)
# inst2=createSynth(unique(n2),type='triangle',duration=4,sustain=0.05)
# inst3=createSynth(unique(n3),duration=10)
#
# x0=dat[,1]
# y0=dat[,2]
#
# # x0=1:100
# # y0=rnorm(100)
# # y0=rescale(1:NROW(x0),-1,1)
# # Remove NAs
# mask0= !is.na(x0) & !is.na(y0)
# x0=x0[mask0];y0=y0[mask0]
# # 2DO: check x and y vary
# # Sort and normalize
# foo=sort.int(x0,index.return=TRUE)
# x=x0[foo$ix];y=y0[foo$ix];
# x=rescale(x)
# if(invertMapping) y=-1*y
#
# tstep=diff(range(x))/(NROW(x)-1)
#
# tim0=t0+(x/tstep)*spt
# timregular=t0+((1:NROW(x))-1)*spt
#
# # Instrument 1 highlights positive values
# thresh=quantile(y[y>=0],1-p1)
# if(is.na(thresh)){
#   tim=tim0[1]
#   ix=1
#   vol=0
# } else {
#   mask=y>=thresh
#   tim=tim0[mask]
#   pitch=rescale(y[mask])
#   ix=as.integer(rescale(pitch)*(length(n1)-1))+1
#   vol=rescale(y[mask])
# }
#
# i1=play.instrument(inst1,notes=n1[ix],time=tim,
#                    volume=vol,fadeout=max(ix)-ix+1)
#
# # Crash highlights very high positive values
# thresh=quantile(y[y>=0],1-pCymbals)
# if(is.na(thresh)){
#   td=tim0[1];vd=0;nd='crash'
# } else {
#   mask=y>=thresh
#   td=tim0[mask]
#   vd=rescale(y[mask],minCymbalsVol,1)
#   if(any(is.na(vd))) vd=rep(1,sum(mask))
#   nd=rep('crash',sum(mask))
# }
#
# drum_crash=play.instrument(drum,notes=nd,time=td,volume=vd)
#
# # Instrument 2 drum highlights low values
# thresh=quantile(y[y<=0],p2)
# if(is.na(thresh)){
#   tim=tim0[1]
#   ix=1
#   vol=0
# } else {
#   mask=y<=thresh
#   tim=tim0[mask]
#   pitch=rescale(y[mask])
#   ix=as.integer(rescale(pitch)*(length(n2)-1))+1
#   vol=rescale(y[mask],1,0)
# }
# i2=play.instrument(inst2,notes=n2[ix],time=tim,
#                    volume=vol,fadeout=0*ix+Inf)
# drum_bass=play.instrument(drum,notes=rep('bass',length(tim)),
#                           time=tim,volume=vol)
#
# # Splash highlights very low negative values
# thresh=quantile(y[y<=0],pCymbals)
# if(is.na(thresh)){
#   td=tim0[1];vd=0;nd='china'
# } else {
#   mask=y<=thresh
#   td=tim0[mask]
#   vd=rescale(y[mask],minCymbalsVol,1)
#   if(any(is.na(vd))) vd=rep(1,sum(mask))
#   nd=rep('china',sum(mask))
# }
#
# drum_splash=play.instrument(drum,notes=nd,time=td,volume=vd)
#
# # Instrument 3 (Bass) highlights both low and high values
# thresh=quantile(abs(y),1-p3)
# mask=abs(y)>=thresh
# tim=tim0[mask]
# pitch=rescale(y[mask])
# ix=as.integer(rescale(pitch)*(length(n3)-1))+1
# vol=rescale(abs(y)[mask],minBassVol,1)
# i3=play.instrument(inst3,notes=n3[ix],time=tim,volume=vol,
#                      fadein=rep(0.1,length(ix)),fadeout=rep(0.5,length(ix)))
# m=y[mask]>=0
# if(sum(m)==0){
#   td=tim0[1];vd=0;nd='snare'
# } else
# {
#   td=tim[m];vd=vol[m]
#   nd=rep('snare',sum(m))
# }
# drum_snare=play.instrument(drum,notes=nd,time=td,volume=vd)
#
# m=y[mask]<=0
# if(sum(m)==0){
#   td=tim0[1];vd=0;nd='bass'
# } else
# {
#   td=tim[m];vd=vol[m]
#   nd=rep('bass',sum(m))
# }
# drum_bass=play.instrument(drum,notes=nd,time=td,volume=vd)
#
# # Drumkit:  hihat and ride
# tim=timregular[rep_len(c(F,F,T,F),length(timregular))]
# vol=0*tim+1
# drum_hh=play.instrument(drum,notes=rep('hihat_f',length(tim)),time=tim,volume=vol)
# tim=timregular[rep_len(c(T,F,F,T),length(timregular))]
# vol=rep_len(c(0.1,0.02),length(tim))
# drum_ride=play.instrument(drum,notes=rep('ride',length(tim)),time=tim,volume=vol,
#                           fadeout=rep(Inf,length(tim)))
#
# final=mix(waves=c(list(i1,i2,i3,drum_bass,drum_snare,drum_ride,drum_hh,drum_crash,drum_splash)),
#           vol=c(0.7,1,1,1,1,0.7,0.3,0.5,0.5),pan=c(-0.9,0.9,0,0,0,-0.7,0.7,0,0))
#
# writeWave(final,'temp.wav')
# system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',file.path(paste0(fname,'.mp3'))))
# file.remove('temp.wav')
#
# DF=data.frame(indx=1:NROW(x),x=x,y=1,time=x0,value=y0)
# DF=rbind(DF,
#          data.frame(indx=NROW(x)+(1:40),x=max(x),y=1,time=max(DF$time),value=DF$value[NROW(x)]))
# g=ggplot(DF)+geom_tile(aes(x,y,fill=value,group=indx))+
#   scale_fill_distiller(palette='RdBu',guide=NULL)+
#   theme_void()+
#   transition_reveal(indx)+
#   shadow_wake(1,wrap=FALSE)
#
# animate(g, renderer = av_renderer(file.path(paste0(fname,'.mp4')),
#                                   audio = file.path(paste0(fname,'.mp3'))),
#         width=1280,height=360,res=104,fps=1/spt,nframes=NROW(DF))
#
#
#
# # inst=pianoSteinway
# # foo=globalT
# # values=foo$Anomaly
# # times=foo$Year
# # chord=c('E1','E2') #c('E1','B1','E2','B2','E3')
# # gamme=c('Db','Eb','E','Gb','Ab','Bb','B')
# # notes=c(paste0(gamme,2),paste0(gamme,3),paste0(gamme,4),paste0(gamme,5))
# # duration=10
# #
# # n=length(values)
# # pitches=pitchMapping(values,notes)
# # mask= rep(TRUE,n)  # c(TRUE,pitches[1:(n-1)]!=pitches[2:n]) #
# #
# # ch=play.instrument(inst,
# #                    notes=chord,
# #                    time=rep(0,length(chord)),
# #                    fadeout = rep(Inf,length(chord)))
# # track1=sequence(as.soundSample(ch),seq(0,duration,8*duration/n))
# #
# #
# # track2=play.instrument(inst,
# #                     notes=pitches[mask],
# #                     time=rescale(times[mask],0,duration),
# #                     volume=rescale(-1*values[mask]),
# #                     pan=rescale(values[mask],-1,1),
# #                     fadein=rep(2*duration/n,sum(mask)),
# #                     fadeout=rep(30*duration/n,sum(mask)))
# # final=mix(list(track1,track2),volume=c(0.5,1))
# # play(final)
#
#
# # values=globalT$Anomaly
# # f0=110
# # duration=5
# # type='sine'
# # freqRatio=c(1/2,2/3,1:6,8)
# # h=vector('list',length(freqRatio))
# # tim=rescale(1:length(values))
# # for (i in 1:length(freqRatio)){
# #   h[[i]]=oscillator(type,freqRatio[i]*f0,duration,phase=0,rate=44100)
# #   E=envelope(tim,rescale(values))
# #   if(i>3)  h[[i]]=applyEnvelope(h[[i]],E)
# #   h[[i]]=as.Wave(h[[i]])
# # }
# # note=mix(h)
# # plot(note)
# # play(note)
