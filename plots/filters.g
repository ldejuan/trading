set grid
set autoscale x
set autoscale y
plot '..\app\trend.out' using 1:7 with lines title 'close',\
'..\app\trend.out' using 1:10 w l t 'detrend'