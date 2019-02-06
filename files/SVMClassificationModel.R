#SVM on basket of CCI indicators
#

trainSVM = function(data, ktype, C, crossvalid) {
	# Return a trained svm model
	trainedmodel = ksvm(targets ~ ., data = data, type = ktype, kernel="rbfdot", kpar=list(sigma=5), C = C, prob.model = TRUE, cross = crossvalid)
}

featureGen = function(market_data, returns) {

		# Generate the targets from the daily returns
		#Remove date from returns
		targets = coredata(returns)
		#Set target to -1 or 1 depending on positive or negative returns
		targets[targets>=0] = 1
		targets[targets<0] = -1
		targets = as.factor(targets)
		
		#Generate CCIs from High, Low and close prices and 2 - 30 day moving average
		cci2 = CCI(HLC(market_data), 2 )
		cci3 = CCI(HLC(market_data), 3 )
		cci4 = CCI(HLC(market_data), 4 )
		cci5 = CCI(HLC(market_data), 5 )
		cci6 = CCI(HLC(market_data), 6 )
		cci7 = CCI(HLC(market_data), 7 )
		cci8 = CCI(HLC(market_data), 8 )
		cci9 = CCI(HLC(market_data), 9 )
		cci10 = CCI(HLC(market_data), 10 )
		cci11 = CCI(HLC(market_data), 11 )
		cci12 = CCI(HLC(market_data), 12 )
		cci13 = CCI(HLC(market_data), 13 )
		cci14 = CCI(HLC(market_data), 14 )
		cci15 = CCI(HLC(market_data), 15 )
		cci16 = CCI(HLC(market_data), 16 )
		cci17 = CCI(HLC(market_data), 17 )
		cci18 = CCI(HLC(market_data), 18 )
		cci19 = CCI(HLC(market_data), 19 )
		cci20 = CCI(HLC(market_data), 20 )
		cci21 = CCI(HLC(market_data), 21 )
		cci22 = CCI(HLC(market_data), 22 )
		cci23 = CCI(HLC(market_data), 23 )
		cci24 = CCI(HLC(market_data), 24 )
		cci25 = CCI(HLC(market_data), 25 )
		cci26 = CCI(HLC(market_data), 26 )
		cci27 = CCI(HLC(market_data), 27 )
		cci28 = CCI(HLC(market_data), 28 )
		cci29 = CCI(HLC(market_data), 29 )
		cci30 = CCI(HLC(market_data), 30 )

		# Now we need to lag each generated CCI by 1 period.
		# This is to ensure we are not predicting the + or - return based
		# on the data from that day.
		# i.e. we want to use CCIs from on day piror
		cci2 = Lag(cci2, 1)
		cci3 = Lag(cci3, 1)
		cci4 = Lag(cci4, 1)
		cci5 = Lag(cci5, 1)
		cci6 = Lag(cci6, 1)
		cci7 = Lag(cci7, 1)
		cci8 = Lag(cci8, 1)
		cci9 = Lag(cci9, 1)
		cci10 = Lag(cci10, 1)
		cci11 = Lag(cci11, 1)
		cci12 = Lag(cci12, 1)
		cci13 = Lag(cci13, 1)
		cci14 = Lag(cci14, 1)
		cci15 = Lag(cci15, 1)
		cci16 = Lag(cci16, 1)
		cci17 = Lag(cci17, 1)
		cci18 = Lag(cci18, 1)
		cci19 = Lag(cci19, 1)
		cci20 = Lag(cci20, 1)
		cci21 = Lag(cci21, 1)
		cci22 = Lag(cci22, 1)
		cci23 = Lag(cci23, 1)
		cci24 = Lag(cci24, 1)
		cci25 = Lag(cci25, 1)
		cci26 = Lag(cci26, 1)
		cci27 = Lag(cci27, 1)
		cci28 = Lag(cci28, 1)
		cci29 = Lag(cci29, 1)
		cci30 = Lag(cci30, 1)

		# Set up all the data, target and lagged cci values
		data = data.frame(targets, cci2, cci3, cci4, cci5, cci6, cci7, cci8, cci9, cci10, cci11, cci12, cci13, cci14, cci15, cci16, cci17, cci18, cci19, cci20, cci21, cci22, cci23, cci24, cci25, cci26, cci27, cci28, cci29, cci30)
		# names(data) = c("targets", "data")
		# Results
		

		return(data)
	}
	
	
SVM = function(data, targets, returns, lookback=252, ktype='C-svc', crossvalid=10, C=10){

	library(kernlab)
	library(quantmod)

	

	# Generate stats to compare benchmark with our trading strategy
	Stats = function(strat, benchmark) {

		library(PerformanceAnalytics)
		#Compute stats of interest for strategy
		cumRetstrat = Return.cumulative(strat)
		annRetstrat = Return.annualized(strat, scale=252)
		sharpestrat = SharpeRatio.annualized(strat, scale=252)
		winpctstrat = length(strat[strat > 0])/length(strat[strat != 0])
		annSDstrat = sd.annualized(strat, scale=252)
		mastratDDstrat = maxDrawdown(strat)
		avDDstrat = AverageDrawdown(strat)

		
		#Compute stats of interest for benchmark
		cumRetbenchmark = Return.cumulative(benchmark)
		annRetbenchmark = Return.annualized(benchmark, scale=252)
		sharpebenchmark = SharpeRatio.annualized(benchmark, scale=252)
		winpctbenchmark = length(benchmark[benchmark > 0])/length(benchmark)
		annSDbenchmark = sd.annualized(benchmark, scale=252)
		maxDDbenchmark = maxDrawdown(benchmark)
		avDDbenchmark = AverageDrawdown(benchmark)
		#Return result vectors
		Benchmark = c(cumRetbenchmark, annRetbenchmark, sharpebenchmark, winpctbenchmark, annSDbenchmark, maxDDbenchmark, avDDbenchmark)
		Strategy = c(cumRetstrat, annRetstrat, sharpestrat, winpctstrat, annSDstrat, mastratDDstrat, avDDstrat)
		rowNames = c("Cumulative Return", "Annualized Return", "Annualized Sharpe Ratio", "Winning Percentage", "Annualized Volatility", "Maximum Drawdown", "Average Drawdown")
		result = data.frame(Strategy, Benchmark, row.names = rowNames)
		

		return(result)
	}	


	targets <- as.factor(targets)
	data$targets <- as.factor(data$targets)


	#Create backtest index
	targets = lookback:(nrow(data)-1)
	idx = data.frame(targets)

	#Extract index returns
	inx = index(returns[idx$targets])


	#Function for prediction
	pred1pd = function(t) {
		#Train the SVM model and make prediction from model
		model = trainSVM(data[(t-lookback):t, ], ktype, C, crossvalid)
		return(predict(model, data[t+1, -1], type="prob"))
		print(predict(model, data[t+1, -1], type="prob"))
	}

	#Use sapply to traverse over the index targets and apply prediction function
	predictions = sapply(idx$targets, pred1pd)
	print(predictions)



	print(max.col(predictions))
	max_c_p = max.col(t(predictions))
	predictions = data.frame(t(rbind(max_c_p, predictions)))

	print(predictions)
	print(Stats((returns[idx$targets] * (predictions$max_c_p*2-3)), returns[idx$targets]))
	



	#Plotting
	#Set up each series for benchmark and Strategy
	equity = xts(cumprod((returns[idx$targets] * (predictions$max_c_p*2-3))+1), inx)
	Buy_Hold = xts(cumprod(returns[idx$targets] + 1), inx)

	# Chart axis range
	Max = abs(max(equity, Buy_Hold))
	Min = abs(min(equity, Buy_Hold))

	chartSeries(equity, log.scale = TRUE, name='Returns', yrange=c(Min, Max))
	addTA(Buy_Hold, on=1, col='white')
}






