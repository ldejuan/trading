set grid
set xdata time
set timefmt "%d/%m/%Y"
set xrange ["01/01/2000":"13/09/2022"]
set autoscale y
plot '..\app\simul.out' using 2:6 with lines title 'close',\
'..\app\simul.out' using 2:8 w l t 'short', \
'..\app\simul.out' using 2:9 w l t 'long'


# p for [col = 3:5] '..\app\output.txt' using 1:col w l t 'title'  
