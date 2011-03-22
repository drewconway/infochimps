wikipedia.abstratcs <- 
function(search.string, from=0, to.df=TRUE) {
    abs.url<-paste(.InfochimpsEnv$data$encyclopedic,"wikipedia/articles/abstract_search.json?from=",from,"&q=",
        URLencode(search.string),"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    abs.get<-getURL(abs.url)
    abs.get<-clean.george(abs.get)
    abs.data<-fromJSON(abs.get)
    if(abs.data$total=="0") {
        warning(paste("No results found for '",search.string,"'",sep=""))
        return(NA)
    }
    else {
        if(to.df) {
            all.names<-c("title","url","abstract")
            return(jsonToDataFrame(abs.data, all.names))
        }
        else {
            return(abs.data)
        }
    }
}