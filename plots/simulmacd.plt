set tmargin 0
set bmargin 0
set lmargin 5
set rmargin 2
unset xtics
unset ytics
set multiplot layout 3,1 title "MACD CAC-40\n" font ",12"
set key autotitle column nobox samplen 1 noenhanced
unset title
set xrange ["01/01/2020":"13/09/2022"]
set xdata time
set autoscale y
set grid
set ytics
set xtics

set format x ""
plot '..\app\simulmacd.out' using 2:3:4:5:6 with financebars lt 8 title 'cac-index' ,\
	'..\app\simulmacd.out' using 2:8 with lines title 'ema Long',\
	'..\app\simulmacd.out' using 2:9 with lines title 'ema short'

set format x "%d/%m/%Y"
plot '..\app\simulmacd.out' using 2:12 with lines title 'index',\
	'..\app\simulmacd.out' using 2:15 with lines title 'strategy'


