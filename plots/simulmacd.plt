set tmargin 0
set bmargin 0
set lmargin 5
set rmargin 2
unset xtics
unset ytics
set multiplot layout 3,1 title "MACD CAC-40\n" font ",12"
set key autotitle column nobox samplen 1 noenhanced
unset title
set xdata time
set autoscale y
set grid
set ytics
set xtics

set format x ""
plot 'C:\Users\ldejuan\work\aaDATA\home\data\analysis\inputdir\simul_onemacd.csv'using 1:2:3:4:5 with financebars lt 8 title 'cac-index' ,\
	'C:\Users\ldejuan\work\aaDATA\home\data\analysis\inputdir\simul_onemacd.csv' using 1:9 with lines title 'short',\
	'C:\Users\ldejuan\work\aaDATA\home\data\analysis\inputdir\simul_onemacd.csv' using 1:10 with lines title 'long'

set format x "%Y%m%d"
plot 'C:\Users\ldejuan\work\aaDATA\home\data\analysis\inputdir\simul_onemacd.csv'using 13 with lines title 'index',\
	'C:\Users\ldejuan\work\aaDATA\home\data\analysis\inputdir\simul_onemacd.csv' using 16 with lines title 'strategy'


