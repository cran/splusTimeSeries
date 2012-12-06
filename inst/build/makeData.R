###
### This file tells how to make the data sets in the external
###

### file that makes the following built-in datasets:
###   fed.rate
###   tbauc.3m
###   tbauc.6m
###   tbauc.1y
###   tcm.curve
###   tbond
###   djia
###   exch.rate
###   net.packet
###   say.wavelet

###
### all data files are here as well
###

library("TimeSeries", lib.loc="~/rPackages/tmpLib")

ascii.file.loc <- "/homes/anbruce/Data/"
out.loc <- "/homes/anbruce/rPackages/TimeSeries/inst/external/"

####### Creation of fed.rate

fedratestart <- timeDate( "1/1/1972" )

pr <- read.table( paste( ascii.file.loc, "prime.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "prime.rate" ),
		 as.is = c( TRUE, FALSE ))
prts <- timeSeries( positions=timeDate(pr$date, format="%02m/%02d/%Y"), 
                   data=data.frame( prime.rate = pr$prime.rate), units = "%" )
prts <- prts[ positions( prts ) >= fedratestart,]

dr <- read.table( paste( ascii.file.loc, "dwb.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "discount.rate" ),
		 as.is = c( TRUE, FALSE ))
drts <- timeSeries( positions=timeDate(dr$date, format="%02m/%02d/%Y"), 
		    data=data.frame( discount.rate = dr$discount.rate), 
		    units = "%" )
drts <- drts[ positions( drts ) >= fedratestart,]

ff <- read.table( paste( ascii.file.loc, "fedfund.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "fedfunds.rate" ),
		 as.is = c( TRUE, FALSE ))
ffts <- timeSeries( positions=timeDate(ff$date, format="%02m/%02d/%Y"), 
		    data=data.frame( fedfunds.rate = ff$fedfunds.rate), 
		    units = "%" )
ffts <- ffts[ positions( ffts ) >= fedratestart,]

cm <- read.table( paste( ascii.file.loc, "cm.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "mortgage.rate" ),
		 as.is = c( TRUE, FALSE ))
cmts <- timeSeries( positions=timeDate(cm$date, format="%02m/%02d/%Y"), 
		    data=data.frame( mortgage.rate = cm$mortgage.rate), 
		    units = "%" )
cmts <- cmts[ positions( cmts ) >= fedratestart,]

fed.rate <- seriesMerge( prts, drts, ffts, cmts, how="NA")
fed.rate@title <- "Federal Reserve Interest Rate Data"
fed.rate@documentation <- 
      c( "Data from the web site of the Federal Reserve Bank,",
	 "http://www.bog.frb.fed.us/releases/H15/data.htm,",
	 "including the Prime Rate, the Federal Discount Rate,",
	 "the Federal Funds Rate, and Average Mortgage Interest",
	 "Rate.  The first three are published daily (including",
	 "weekends and holidays), and the last weekly on Friday." )

save(list="fed.rate", file=paste(out.loc, "fedrate.rda", sep=""))

##### Creation of tbauc.* series

tba3 <- read.table( paste( ascii.file.loc, "tbaa3m.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tba3 <- tba3[ tba3[,2] != 0,]

tbauc.3m <- timeSeries( positions=timeDate(tba3$date, format="%02m/%02d/%Y"), 
			data=data.frame( rate = tba3$rate), 
			units = "%" )
tbauc.3m@title <- "3-Month Treasury Bill Auction, average rate"
tbauc.3m@documentation <-
      c( "Data from the web site of the Federal Reserve Bank,",
	 "http://www.bog.frb.fed.us/releases/H15/data.htm,",
	 "on the average rates for each auction of 3-month",
	 "Treasury Bills" )

save(list="tbauc.3m", file=paste(out.loc, "tbauc3m.rda", sep=""))

tba6 <- read.table( paste( ascii.file.loc, "tbaa6m.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tba6 <- tba6[ tba6[,2] != 0,]

tbauc.6m <- timeSeries( positions=timeDate(tba6$date, format="%02m/%02d/%Y"), 
			data=data.frame( rate = tba6$rate), 
			units = "%" )
tbauc.6m@title <- "6-Month Treasury Bill Auction, average rate"
tbauc.6m@documentation <-
      c( "Data from the web site of the Federal Reserve Bank,",
	 "http://www.bog.frb.fed.us/releases/H15/data.htm,",
	 "on the average rates for each auction of 6-month",
	 "Treasury Bills" )

save(list="tbauc.6m", file=paste(out.loc, "tbauc6m.rda", sep=""))

tba12 <- read.table( paste( ascii.file.loc, "tbaa1y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tba12 <- tba12[ tba12[,2] != 0,]

tbauc.1y <- timeSeries( positions=timeDate(tba12$date, format="%02m/%02d/%Y"), 
			data=data.frame( rate = tba12$rate), 
			units = "%" )
tbauc.1y@title <- "1-Year Treasury Bill Auction, average rate"
tbauc.1y@documentation <-
      c( "Data from the web site of the Federal Reserve Bank,",
	 "http://www.bog.frb.fed.us/releases/H15/data.htm,",
	 "on the average rates for each auction of 1-year",
	 "Treasury Bills" )

save(list="tbauc.1y", file=paste(out.loc, "tbauc1y.rda", sep=""))

##### creation of tcm.curve

tcmstart <- timeDate( "1/1/1982" )
tcm3m <- read.table( paste( ascii.file.loc, "tcm3m.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm3m <- tcm3m[ tcm3m[,2] != 0,]
tcm3mts <- timeSeries( positions=timeDate(tcm3m$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "three.month" = tcm3m$rate), 
		       units = "%" )
tcm3mts <- tcm3mts[ positions( tcm3mts ) >= tcmstart,]


tcm6m <- read.table( paste( ascii.file.loc, "tcm6m.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm6m <- tcm6m[ tcm6m[,2] != 0,]
tcm6mts <- timeSeries( positions=timeDate(tcm6m$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "six.month" = tcm6m$rate), 
		       units = "%" )
tcm6mts <- tcm6mts[ positions( tcm6mts ) >= tcmstart,]

tcm1y <- read.table( paste( ascii.file.loc, "tcm1y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm1y <- tcm1y[ tcm1y[,2] != 0,]
tcm1yts <- timeSeries( positions=timeDate(tcm1y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "one.year" = tcm1y$rate), 
		       units = "%" )
tcm1yts <- tcm1yts[ positions( tcm1yts ) >= tcmstart,]

tcm2y <- read.table( paste( ascii.file.loc, "tcm2y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm2y <- tcm2y[ tcm2y[,2] != 0,]
tcm2yts <- timeSeries( positions=timeDate(tcm2y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "two.year" = tcm2y$rate), 
		       units = "%" )
tcm2yts <- tcm2yts[ positions( tcm2yts ) >= tcmstart,]

tcm3y <- read.table( paste( ascii.file.loc, "tcm3y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm3y <- tcm3y[ tcm3y[,2] != 0,]
tcm3yts <- timeSeries( positions=timeDate(tcm3y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "three.year" = tcm3y$rate), 
		       units = "%" )
tcm3yts <- tcm3yts[ positions( tcm3yts ) >= tcmstart,]

tcm5y <- read.table( paste( ascii.file.loc, "tcm5y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm5y <- tcm5y[ tcm5y[,2] != 0,]
tcm5yts <- timeSeries( positions=timeDate(tcm5y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "five.year" = tcm5y$rate), 
		       units = "%" )
tcm5yts <- tcm5yts[ positions( tcm5yts ) >= tcmstart,]

tcm7y <- read.table( paste( ascii.file.loc, "tcm7y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm7y <- tcm7y[ tcm7y[,2] != 0,]
tcm7yts <- timeSeries( positions=timeDate(tcm7y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "seven.year" = tcm7y$rate), 
		       units = "%" )
tcm7yts <- tcm7yts[ positions( tcm7yts ) >= tcmstart,]

tcm10y <- read.table( paste( ascii.file.loc, "tcm10y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm10y <- tcm10y[ tcm10y[,2] != 0,]
tcm10yts <- timeSeries( positions=timeDate(tcm10y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "ten.year" = tcm10y$rate), 
		       units = "%" )
tcm10yts <- tcm10yts[ positions( tcm10yts ) >= tcmstart,]

tcm20y <- read.table( paste( ascii.file.loc, "tcm20y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm20y <- tcm20y[ tcm20y[,2] != 0,]

tcm20yhist <- read.table( paste( ascii.file.loc, "hcm20y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm20yhist <- tcm20yhist[ tcm20yhist[,2] != 0,]

tcm20yts <- timeSeries( positions=timeDate( c( tcm20yhist$date, tcm20y$date ),
			       format="%02m/%02d/%Y"), 
			data=data.frame( "twenty.year" = c( tcm20yhist$rate,
				      tcm20y$rate )), 
			units = "%" )
tcm20yts <- tcm20yts[ positions( tcm20yts ) >= tcmstart,]


tcm30y <- read.table( paste( ascii.file.loc, "tcm30y.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcm30y <- tcm30y[ tcm30y[,2] != 0,]
tcm30yts <- timeSeries( positions=timeDate(tcm30y$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "thirty.year" = tcm30y$rate), 
		       units = "%" )
tcm30yts <- tcm30yts[ positions( tcm30yts ) >= tcmstart,]

tcmlong <- read.table( paste( ascii.file.loc, "tcm10p.prn", sep = "" ),
		 header = FALSE,
		 row.names = NULL,
		 col.names = c( "date", "rate" ),
		 as.is = c( TRUE, FALSE ))
tcmlong <- tcmlong[ tcmlong[,2] != 0,]
tcmlongts <- timeSeries( positions=timeDate(tcmlong$date, format="%02m/%02d/%Y"), 
		       data=data.frame( "long.term" = tcmlong$rate), 
		       units = "%" )
tcmlongts <- tcmlongts[ positions( tcmlongts ) >= tcmstart,]

tcm.curve <- seriesMerge( tcm3mts, tcm6mts, tcm1yts, tcm2yts, tcm3yts,
		      tcm5yts, tcm7yts, tcm10yts, tcm20yts, tcm30yts, 
		      tcmlongts, how="NA" )
tcm.curve@title <- "Treasury Constant Maturity Curve"
tcm.curve@documentation <-
      c( "Data from the web site of the Federal Reserve Bank,",
	 "http://www.bog.frb.fed.us/releases/H15/data.htm,",
	 "calculated Treasury constant maturity curve. Note",
	 "that the 20-year column is a combination of the",
	 "Historical 20-year data from 1982-1986 and",
	 "the modern 20-year data from 1993-1997; there is",
	 "no 20-year data between 1987 and 1992.  The long-term",
	 "column is the composite of 10-year and longer maturities",
	 "provided by the Federal Reserve." )

save(list="tcm.curve", file=paste(out.loc, "tcmcurve.rda", sep=""))

#### creation of tbond series

tfut20m <- read.table( paste( ascii.file.loc, "cc20m.prn", sep = "" ),
		      header = FALSE,
		      row.names = NULL,
		      col.names = c( "date", "time", "high", "low" ),
		      as.is = c( TRUE, TRUE, FALSE, FALSE ))
tbond <- timeSeries( positions=timeDate( paste( tfut20m$date, tfut20m$time %/% 100,
				   tfut20m$time %% 100 ),
			    in.format = "%m/%d/%y %H %M",
			    format = "%02m/%02d/%Y %02H:%02M" ),
		      data=data.frame( high = tfut20m$high, low = tfut20m$low ))
tbond@title <- "Treasury Bond Futures Trading Data"
tbond@documentation <-
  c( "Treasury Bond futures trading data: high and low prices",
     "over 20-minute intervals, from Ohio State University web site",
     "http://www.cob.ohio-state.edu/~fin/osudown.htm" )

save(list="tbond", file=paste(out.loc, "tbond.rda", sep=""))

#### creation of the djia data set

djiadf <- read.table( paste( ascii.file.loc, "daily.prn", sep = "" ),
		   header = FALSE,
		   row.names = NULL,
		   col.names = c( "date", "open", "high", "low", 
		     "close", "volume" ),
		   as.is = c( TRUE, FALSE, FALSE, FALSE, FALSE, FALSE ))
djiadf$open[djiadf$open == 0] <- NA
djiadf$high[djiadf$high == 0] <- NA
djiadf$low[djiadf$low == 0] <- NA
djiadf$close[djiadf$close == 0] <- NA
djiadf$volume[djiadf$volume == 0] <- NA

djia <- timeSeries( positions=timeDate( djiadf$date, format="%02m/%02d/%Y" ),
		     data=data.frame( open = djiadf$open, high = djiadf$high,
				 low = djiadf$low, close = djiadf$close,
				 volume = djiadf$volume ))
djia@title <- "Dow Jones Industrial Average"
djia@documentation <- 
  c( "High, low, opening, and closing prices and trading volume,",
     "where available, for the Dow Jones Industrial Average.",
     "from Ohio State University web site",
     "http://www.cob.ohio-state.edu/~fin/osudown.htm" )

save(list="djia", file=paste(out.loc, "djia.rda", sep=""))

#### creation of exch.rate data set

exch <- read.table( paste( ascii.file.loc, "exchange.prn", sep = "" ),
		   header = TRUE,
		   row.names = NULL )
exch <- exch[ exch[,2] != 0,]

exch.rate <- timeSeries( positions=timeDate( as.character( exch$Date ),
			        in.format = "%02y%02m%02d",
			        format = "%02m/%02d/%Y" ),
                        data=data.frame( GBP = exch$BP, CAD = exch$CD,
                          DEM = exch$DM, JPY = exch$JY, 
                          CHF = exch$SF ))
exch.rate@title <- "Foreign Exchange Rates"
exch.rate@documentation <-
  c( "Exchange rates between the US Dollar and the British Pound,",
     "Canadian Dollar, German Mark, Japanese Yen, and Swiss Franc.",
     "Data from Andreas S. Weigend, Bernardo A. Huberman, and David",
     "E. Rumelhart, Predicting Sunspots and Exchange Rates with",
     "Connectionist Networks, pp. 395-432 in M. Casdagli and S.",
     "Eubank, eds, Nonlinear Modeling and Forecasting, Addison-Wesley,",
     "1992.  Extracted from the Andreas Weigend web site",
     "http://www.stern.nyu.edu/~aweigend/Time-Series/TSWelcome.html" )

save(list="exch.rate", file=paste(out.loc, "exchrate.rda", sep=""))

#### creation of the net.packet data set

netpack <- read.table( paste( ascii.file.loc, "netpack.txt", sep = "" ),
		   header = FALSE,
		   row.names = NULL,
		   col.names = c( "time", "type", "length" ),
		   as.is = c( TRUE, FALSE, FALSE ))

net.packet <- timeSeries( positions=timeDate( paste( "3/27/1998", netpack$time, "PST" ),
				in.format = "%m/%d/%y %H:%M:%S.%3N%2c %Z",
				format = "%02H:%02M:%02S.%03N %Z",
				zone = "PST" ),
			   data=data.frame( type = netpack$type, 
				       length = netpack$length ))
net.packet@title <- "Network Packet Traffic"
net.packet@documentation <- 
  c( "Time, type, and size (if available) for 10,000 network packets",
     "on MathSoft's intranet, just before 5PM on March 27, 1998, as",
     "reported by the Unix snoop command." )

save(list="net.packet", file=paste(out.loc, "netpack.rda", sep=""))

#### creation of the say.wavelet data set

sayit <- read.table( paste( ascii.file.loc, "wavelet.prn", sep = "" ),
		     header = FALSE, row.names = NULL )

say.wavelet <- signalSeries( data = sayit[[1]], from = 0, by = 1/11025 )
say.wavelet@title <- "Voice Saying \"Wavelet\""
say.wavelet@documentation <- 
  "Signal of a voice saying the word \"wavelet\", sampled at 11025 Hz."
say.wavelet@units.position <- "seconds"

save(list="say.wavelet", file=paste(out.loc, "saywave.rda", sep=""))


# test

rm( djia, exch.rate, fed.rate, net.packet, tbauc.1y, tbauc.3m, 
    tbauc.6m, tbond, tcm.curve, say.wavelet )

load( paste(out.loc, "fedrate.rda", sep = "" ))
load( paste(out.loc, "tbauc3m.rda", sep=""))
load( paste(out.loc, "tbauc6m.rda", sep=""))
load( paste(out.loc, "tbauc1y.rda", sep=""))
load( paste(out.loc, "tcmcurve.rda", sep=""))
load( paste(out.loc, "tbond.rda", sep=""))
load( paste(out.loc, "djia.rda", sep=""))
load( paste(out.loc, "exchrate.rda", sep=""))
load( paste(out.loc, "netpack.rda", sep=""))
load( paste(out.loc, "saywave.rda", sep=""))


# cleanup

rm( ascii.file.loc, out.loc )
rm( fedratestart, pr, prts, dr, drts, ff, ffts, cm, cmts )
rm( tba3, tba6, tba12 )
rm( tcmstart, tcm3m, tcm3mts, tcm6m, tcm6mts, tcm1y, tcm1yts,
    tcm2y, tcm2yts, tcm3y, tcm3yts, tcm5y, tcm5yts, tcm7y, tcm7yts,
    tcm10y, tcm10yts, tcm20y, tcm20yhist, tcm20yts, tcm30y, tcm30yts,
    tcmlong, tcmlongts )
rm( tfut20m )
rm( djiadf )
rm( exch )
rm( netpack )
rm( sayit )


