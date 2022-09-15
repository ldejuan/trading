set grid
set logscale xy
set autoscale x
set autoscale y
plot '..\app\trend.out' using 1:3 with lines title 'spectrum in dB'
