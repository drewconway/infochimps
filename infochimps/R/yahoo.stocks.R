yahoo.stocks <- 
function(symbol, begin.date, end.date, from=0, to.df=TRUE) {
    if(is.na(strptime(begin.date, format="%y%m%d")) | is.na(strptime(end.date, format="%y%m%d"))) {
        stop("One or both of the date strings is malformed. Must be YYYYMMdd format.")
    }
    stocks.url<-paste(.InfochimpsEnv$data$finance,"stocks/y_historical/price_range.json?_from=",from,"&symbol=",
        URLencode(symbol),"&beg_date=",begin.date,"&end_date=",end.date,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    stocks.get<-getURL(stocks.url)
    stocks.data<-fromJSON(stocks.get)
    if(is.null(stocks.data$results[[1]])) {
        warning("There were no results for your search, please check your stock search")
        return(NA)
    }
    else {
        if(to.df) {
            all.names<-c("exchange","symbol","date","open","close","adj_close","low","high","volume")
            return(jsonToDataFrame(stocks.data, all.names))
        }
        else {
            return(stocks.data)
        }
    }
}