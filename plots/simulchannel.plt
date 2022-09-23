set grid
set xdata time
set timefmt "%d/%m/%Y"
set xrange ["01/01/2021":"13/09/2022"]
set autoscale y
plot '..\app\simulchannel.out' using 2:11 with lines title 'index',\
	'..\app\simulchannel.out' using 2:14 with lines title 'strategy'

#plot '..\app\simulchannel.out' using 2:4 with lines title 'high',\
#	'..\app\simulchannel.out' using 2:5 with lines title 'low',\
#	'..\app\simulchannel.out' using 2:8 with lines title 'minhigh',\
#	'..\app\simulchannel.out' using 2:9 with lines title 'maxlow'


# p for [col = 3:5] '..\app\output.txt' using 1:col w l t 'title'  
