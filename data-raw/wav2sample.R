library(tuneR)
# hi-hat
f=file.path('data-raw','wav','hihat.wav')
w=readWave(f)
hiHat=soundSample(wave=w@left,rate=w@samp.rate)
save(hiHat,file=file.path('data','hiHat.RData'),compress='xz')
# open hi-hat
f=file.path('data-raw','wav','hihat_o.wav')
w=readWave(f)
hiHat_o=soundSample(wave=w@left,rate=w@samp.rate)
save(hiHat_o,file=file.path('data','hiHat_o.RData'),compress='xz')
# hi-hat 2
f=file.path('data-raw','wav','cymbal-hihat-stick.wav')
w=readWave(f)
hiHat2=soundSample(wave=w@left,rate=w@samp.rate)
save(hiHat2,file=file.path('data','hiHat2.RData'),compress='xz')
# snare
f=file.path('data-raw','wav','snare.wav')
w=readWave(f)
snare=soundSample(wave=w@left,rate=w@samp.rate)
save(snare,file=file.path('data','snare.RData'),compress='xz')
# snare 2
f=file.path('data-raw','wav','drum-snare-tap.wav')
w=readWave(f)
snare2=soundSample(wave=w@left,rate=w@samp.rate)
save(snare2,file=file.path('data','snare2.RData'),compress='xz')
# kick
f=file.path('data-raw','wav','kick.wav')
w=readWave(f)
kick=soundSample(wave=w@left,rate=w@samp.rate)
save(kick,file=file.path('data','kick.RData'),compress='xz')
# kick 2
f=file.path('data-raw','wav','drum-bass-lo-1.wav')
w=readWave(f)
kick2=soundSample(wave=w@left,rate=w@samp.rate)
save(kick2,file=file.path('data','kick2.RData'),compress='xz')
# ride
f=file.path('data-raw','wav','ride.wav')
w=readWave(f)
ride=soundSample(wave=w@left,rate=w@samp.rate)
save(ride,file=file.path('data','ride.RData'),compress='xz')
# bell
f=file.path('data-raw','wav','bell.wav')
w=readWave(f)
bell=soundSample(wave=w@left,rate=w@samp.rate)
save(bell,file=file.path('data','bell.RData'),compress='xz')
