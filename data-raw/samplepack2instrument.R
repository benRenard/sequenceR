library(sequenceR)

buildInstrument <- function(packDir,files,names,timeBeforePeak=NA,recursive=FALSE){
  allFiles=list.files(packDir,recursive=recursive)
  sam=vector(mode='list',length=length(files))
  for(i in 1:length(files)){
    j=which(grepl(files[i],allFiles,fixed=TRUE))
    if(length(j)==0){stop(paste('File',files[i],'not found or matched'))}
    if(length(j)>1){stop(paste('Several matches for file',files[i]))}
    f=allFiles[j]
    ext=substr(f,nchar(f)-2,nchar(f))
    if(ext %in% c('mp3','MP3')){
      foo=tuneR::readMP3(file.path(packDir,f))
    } else {
      foo=tuneR::readWave(file.path(packDir,f))
    }
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
# replace 'buzzing' sound samples
guitarPhilharmonia[['Gb2']]=shiftPitch(guitarPhilharmonia[['G2']],-1)
guitarPhilharmonia[['Gb3']]=shiftPitch(guitarPhilharmonia[['F3']],1)
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

# Guitar harmonics
packDir=file.path('data-raw','samplePacks','philharmonia','Strings','guitar')
names=c('E3','G3','A3','B3',
        'C4','D4','Eb4','E4',
        'F4','Gb4','G4','A4',
        'B4','C5','D5','E5',
        'Gb5','G5','B5','E6')
names2=c('E3_very-long_forte','G3_very-long_piano','A3_very-long_forte','B3_very-long_piano',
         'C4_very-long_piano','D4_very-long_forte','Ds4_very-long_piano','E4_very-long_forte',
         'F4_very-long_piano','Fs4_very-long_piano','G4_very-long_forte','A4_very-long_forte',
         'B4_very-long_forte','C5_very-long_piano','D5_very-long_forte','E5_very-long_forte',
         'Fs5_very-long_piano','G5_very-long_forte','B5_very-long_forte','E6_very-long_forte')
files=paste0(names2,'_harmonics.mp3')
guitarHarmonics=buildInstrument(packDir,files,names,timeBeforePeak=0.01)
save(guitarHarmonics,file=file.path('instruments','guitarHarmonics.RData'),compress='xz')

# Percussion kit
packDir=file.path('data-raw','samplePacks','percussions','FWS')
names=c('conga1_low','conga1_high','conga1_mute','conga2_low','conga2_high',
        'bongo_low','bongo_high','darbuka_low','darbuka_high','darbuka_mute',
        'tambourine_low','tambourine_high','claves','timbales_low','timbales_high',
        'shaker1','shaker2','triangle_closed','triangle_open',
        'woodblock_low','woodblock_high','agogo_low','agogo_high',
        'cowbell1','cowbell2','cowbell3','cowbell4',
        'cuica1_low','cuica1_high','cuica2_low','cuica2_high',
        'guiro','klank1','klank2',
        'bamboo','sticks','handdrum','kicksnare','metal','sleighbells')
files=c('Conga-Low-2','Conga-High-2','Conga-Mute-1','Conga-Low-1','Conga-High-1',
        'Bongo-Low','Bongo-High','Doumbek-Doum','Doumbek-Tek-1','Doumbek-Tek-2',
        'Tambourine-2','Tambourine-1','Claves','Timbales-Low','Timbales-High',
        'Shaker-2','Shaker-1','Triangle-Closed','Triangle-Open',
        'Woodblock-2','Woodblock-1','Agogo-Low','Agogo-High',
        'Cowbell-4','Cowbell-1','Cowbell-3','Cowbell-2',
        'Cuica-Low','Cuica-High','Cuica-2','Cuica-1',
        'Guiro','Klank-3','Klank-4',
        'Bamboo','Cross-Sticks','Hand-Drum','Kick-Snare','Metal-Hit','Sleigh-Bells')
percussionFWS=buildInstrument(packDir,files,names)
save(percussionFWS,file=file.path('instruments','percussionFWS.RData'),compress='xz')

# mini TR-909
packDir=file.path('data-raw','samplePacks','drumkits','TR-909')
names=c('bass','snare','clap','hihat','hihat_o','ride')
files=c('_bd18','_sn03','_clp02','_ch08','_oh14','_ride01')
mini909=buildInstrument(packDir,files,names)
save(mini909,file=file.path('instruments','mini909.RData'),compress='xz')
# full TR-909
names=c(paste0('bd',sprintf("%02d",1:33)), # bass drum
        paste0('sn',sprintf("%02d",1:37)), # snare
        paste0('ch',sprintf("%02d",1:32)), # closed hi-hat
        paste0('oh',sprintf("%02d",1:15)), # open hi-hat
        paste0('ride',sprintf("%02d",1:12)), # ride cymbal
        paste0('rs',sprintf("%02d",1:4)), # rim shot
        paste0('lt',sprintf("%02d",1:24)), # low tom
        paste0('mt',sprintf("%02d",1:23)), # medium tom
        paste0('ht',sprintf("%02d",1:22)), # high tom
        paste0('clp',sprintf("%02d",1:22)) # hand clap
        )
files=names
TR909=buildInstrument(packDir,files,names)
save(TR909,file=file.path('instruments','TR909.RData'),compress='xz')

# mini TR-808
packDir=file.path('data-raw','samplePacks','drumkits','TR-808')
names=c('bass','snare','clap','hihat','hihat_o','ride','congaLow','congaMid','congaHigh')
files=c('BD5050','SD5050','CP','CH','OH50','CY5050','LC50','MC50','HC50')
mini808=buildInstrument(packDir,files,names,recursive=TRUE)
save(mini808,file=file.path('instruments','mini808.RData'),compress='xz')
# Full TR-808
files=list.files(packDir,'.WAV',recursive=TRUE)
names=substr(files,4,nchar(files)-4)
TR808=buildInstrument(packDir,files,names,recursive=TRUE)
save(TR808,file=file.path('instruments','TR808.RData'),compress='xz')

inst=TR808
play(play.instrument(inst,time=0.5*(1:length(inst)),fadein=rep(0,length(inst))))
