# Library to handle trading strategies

## Definition of the variables
All the variables, constant etc are in lower case
## Definition of the internal variables

Variables with 2 letters defined input and output indices
Variable with 1  letter are used in loops indices

## ENV
The market data and results are stored in an env cube of dimesions: 
env(1:imax, 1:jmax, 1:kmax) 
imax : number of dates on the time series
jmax : number of properties to calculate including market data
kmax : number of assets

The storage in memory (column major) is compatible of the use of timeseries or vector analysis on a given colum property. so : passing to the timeseries functions : 
	env(idate, jproperty, kasset) as pointer to the value of   env(idate, jproperty, kasset) 
	then the vector v(i) in the function will get the values of env(idate+i-1,jproperty,kasset) ! 
	

