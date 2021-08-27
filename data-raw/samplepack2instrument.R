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

# inst=drumkitStahl
# play(play.instrument(inst,fadein=rep(0,length(inst))))
