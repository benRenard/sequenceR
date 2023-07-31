f=file.path('data-raw','WaggaWagga.txt')
WaggaWagga=read.table(f,header=TRUE)
save(WaggaWagga,file=file.path('data','WaggaWagga.RData'))

f=file.path('data-raw','globalTemperatureAnomaly.txt')
globalT=read.table(f,header=TRUE)
save(globalT,file=file.path('data','globalT.RData'))
