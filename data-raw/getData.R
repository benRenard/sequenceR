f=file.path('data-raw','WaggaWagga.txt')
WaggaWagga=read.table(f,header=TRUE)
save(WaggaWagga,file=file.path('data','WaggaWagga.RData'))
