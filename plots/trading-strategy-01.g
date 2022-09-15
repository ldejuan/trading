set grid
set xdata time
set timefmt "%d/%m/%Y"
set xrange ["01/01/2021":"13/09/2022"]
set autoscale y
plot '..\app\output.txt' using 2:12 with lines title 'div-index',\
'..\app\output.txt' using 2:14 w l t 'strategy'


# p for [col = 3:5] '..\app\output.txt' using 1:col w l t 'title'  
