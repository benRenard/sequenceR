library(sequenceR)

buildInstrument <- function(packDir,files,names,timeBeforePeak=NA){
  allFiles=list.files(packDir)
  sam=vector(mode='list',length=length(files))
  for(i in 1:length(files)){
    j=which(grepl(files[i],allFiles,fixed=TRUE))
    if(length(j)==0){stop(paste('File',files[i],'not found or matched'))}
    if(length(j)>1){stop(paste('Several matches for file',files[i]))}
    foo=tuneR::readMP3(file.path(packDir,allFiles[j]))
    if(foo@stereo){
      w=0.5*(foo@left+foo@right)
    } else {
      if(length(foo@left>0)){w=foo@left} else {w=foo@right}
    }
    # remove leading silences
    if(is.na(timeBeforePeak)){
      start=1
    } else {
      peak=min(which(abs(w)>=0.5*max(w)))
      start=max(c(peak-timeBeforePeak*foo@samp.rate,1))
    }
    sam[[i]]=soundSample(wave=w[start:length(w)],rate=foo@samp.rate)
  }
  inst=instrument(samples=sam,notes=names)
}

chromatic=c('C','Db','D','Eb','E','F','Gb','G','Ab','A','Bb','B')

# Piano
packDir=file.path('data-raw','samplePacks','pianoSteinway')
names=c(paste0(chromatic,'1'),paste0(chromatic,'2'),
        paste0(chromatic,'3'),paste0(chromatic,'4'),paste0(chromatic,'5'),
        paste0(chromatic,'6'),paste0(chromatic,'7'),'C8')
files=paste0('loud-',tolower(names),'.mp3')
pianoSteinway=buildInstrument(packDir,files,names)
save(pianoSteinway,file=file.path('instruments','pianoSteinway.RData'),compress='xz')

# Standup Bass
packDir=file.path('data-raw','samplePacks','standupBass')
chrom2=c('C','C-','D','D-','E','F','F-','G','G-','A','A-','B')
names=c(paste0(chromatic[5:12],'1'),paste0(chromatic,'2'),paste0(chromatic,'3'),paste0(chromatic[1:7],'4'))
names2=c(paste0(chrom2[5:12],'1'),paste0(chrom2,'2'),paste0(chrom2,'3'),paste0(chrom2[1:7],'4'))
files=paste0('normal-',tolower(names2),'.mp3')
bassStandup=buildInstrument(packDir,files,names)
save(bassStandup,file=file.path('instruments','bassStandup.RData'),compress='xz')

# Guitar
packDir=file.path('data-raw','samplePacks','philharmonia','Strings','guitar')
chrom2=c('C','Cs','D','Ds','E','F','Fs','G','Gs','A','As','B')
names=c(paste0(chromatic[5:12],'2'),paste0(chromatic,'3'),paste0(chromatic,'4'))
names2=c(paste0(chrom2[5:12],'2'),paste0(chrom2,'3'),paste0(chrom2,'4'))
files=paste0(names2,'_very-long_forte_normal.mp3')
guitarPhilharmonia=buildInstrument(packDir,files,names,timeBeforePeak=0.01)
save(guitarPhilharmonia,file=file.path('instruments','guitarPhilharmonia.RData'),compress='xz')

# Drumkit
packDir=file.path('data-raw','samplePacks','drumkits','stahl')
names=c('bass','snare','snare2','snare3',
        'hihat','hihat_f','hihat_o',
        'ride','ride2','ridebell',
        'crash','crash2','splash','splash2',
        'china','cowbell')
files=c('_bd6.','_snare-mid.','_snare-low.','_snare-hi.',
        '_hihatclose-low.','_hihat-mid.','_openhihat-mid.',
        '_ride-mid.','_ride-hi.','_ridebell.',
        '_crash1-hi.','_crash2-hi.','_splash1-hi.','_splash2-hi.',
        '_china-hi.','_cowbell-hi.')
drumkitStahl=buildInstrument(packDir,files,names)
save(drumkitStahl,file=file.path('instruments','drumkitStahl.RData'),compress='xz')

# Hang drum
packDir=file.path('data-raw','samplePacks','hangDrum')
sam=vector(mode='list',length=25)
names=c(paste0(chromatic,'3'),paste0(chromatic,'4'),'C5')
foo=tuneR::readMP3(file.path(packDir,'380504__jimthecab__hang-drum-1.mp3'))
sam[[1]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # C3
sam[[2]]=shiftPitch(sam[[1]],1) # Db3
sam[[3]]=shiftPitch(sam[[1]],2) # D3
sam[[4]]=shiftPitch(sam[[1]],3) # Eb3
sam[[5]]=shiftPitch(sam[[1]],4) # E3
foo=tuneR::readMP3(file.path(packDir,'380502__jimthecab__hang-drum-3.mp3'))
sam[[6]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # F3
sam[[7]]=shiftPitch(sam[[6]],1) # Gb3
foo=tuneR::readMP3(file.path(packDir,'380501__jimthecab__hang-drum-4.mp3'))
sam[[8]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # G3
foo=tuneR::readMP3(file.path(packDir,'380508__jimthecab__hang-drum-5.mp3'))
sam[[9]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # Ab3
sam[[10]]=shiftPitch(sam[[9]],1) # A3
sam[[11]]=shiftPitch(sam[[9]],2) # Bb3
sam[[12]]=shiftPitch(sam[[9]],3) # B3
foo=tuneR::readMP3(file.path(packDir,'380507__jimthecab__hang-drum-6.mp3'))
sam[[13]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # C4
foo=tuneR::readMP3(file.path(packDir,'380506__jimthecab__hang-drum-7.mp3'))
sam[[14]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # Db4
sam[[15]]=shiftPitch(sam[[14]],1) # D4
sam[[16]]=shiftPitch(sam[[14]],2) # Eb4
sam[[17]]=shiftPitch(sam[[14]],3) # E4
foo=tuneR::readMP3(file.path(packDir,'380505__jimthecab__hang-drum-8.mp3'))
sam[[18]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # F4
sam[[19]]=shiftPitch(sam[[18]],1) # Gb4
foo=tuneR::readMP3(file.path(packDir,'380509__jimthecab__hang-drum-9.mp3'))
sam[[20]]=soundSample(wave=foo@left+foo@right,rate=foo@samp.rate) # G4
sam[[21]]=shiftPitch(sam[[20]],1) # Ab4
sam[[22]]=shiftPitch(sam[[20]],2) # A4
sam[[23]]=shiftPitch(sam[[20]],3) # Bb4
sam[[24]]=shiftPitch(sam[[20]],4) # B4
sam[[25]]=shiftPitch(sam[[20]],5) # C5
hangDrum=instrument(sam,names[1:length(sam)])
save(hangDrum,file=file.path('instruments','hangDrum.RData'),compress='xz')

# inst=hangDrum
# play(play.instrument(inst,time=0.5*(1:length(inst)),fadein=rep(0,length(inst))))