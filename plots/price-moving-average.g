set grid
set xdata time
set timefmt "%d/%m/%Y"
set xrange ["01/01/2021":"13/09/2022"]
set yrange [5000: 7500]
plot '..\app\output.txt' using 2:7 with lines title 'close',\
'..\app\output.txt' using 2:8 w l t 'sema', \
'..\app\output.txt' using 2:9 w l t 'lema'



# p for [col = 3:5] '..\app\output.txt' using 1:col w l t 'title'  
