# trading
Fortran codes for automatic trading strategies

The directory **./app** is the compiled version of the strategies. Strategies reads the market data from the **../data** directory. Referenced to its launch path.
The directory **./data** handles the core market data

The directory **./compile** where the Makefile and the *.o objects
The directory **./src/strats** contains the definitions of the strategies developped herin
The directory **./src/core** contains the definitions of the core indicators (librairies)

The app is compiled via a docker fortran images
to run with service ports docker-compose run --service-ports analysis


