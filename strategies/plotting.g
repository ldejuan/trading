set grid
plot 'output.txt' using 1:12 with lines title 'close',\
'output.txt' using 1:14 w l t 'section'

p for [col = 3:5] "output.txt" using 1:col w l t 'title'  
