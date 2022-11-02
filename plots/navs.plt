set tmargin 0
set bmargin 0
set lmargin 5
set rmargin 2
unset xtics
unset ytics

set multiplot layout 3,1 title "NAV Strategy CAC-40\n" font ",12"
set key autotitle column nobox samplen 1 noenhanced
unset title
set autoscale y
set format x ""
set grid
set ytics
set xtics
plot '..\app\simul.out' using 2:11 with lines title 'index' lc rgb "blue", \
	'..\app\simul.out' using 2:14 with lines title 'strategy' lc rgb "green"
set xtics
set xdata time
set xrange ["01/01/2000":"13/09/2022"]
set format x "%d/%m/%Y"
plot '..\app\simul.out' using 2:15 with lines title 'volatility' lc rgb "red"

unset multiplot


#set grid
#set xdata time
#set timefmt "%d/%m/%Y"
#set xrange ["01/01/2000":"13/09/2022"]
#set autoscale y
#plot '..\app\simul.out' using 2:11 with lines title 'index',\
# '..\app\simul.out' using 2:14 with lines title 'strategy'


#plot '..\app\simul.out' using 2:15 with lines title 'volatility'


# p for [col = 3:5] '..\app\output.txt' using 1:col w l t 'title'  

unset multiplot