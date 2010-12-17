census <-
function(ip.address) {
    census.url<-paste(.InfochimpsEnv$data$ip,"combined.json?ip=",ip.address,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    census.get<-getURL(census.url)
    census.data<-fromJSON(census.get)
    if(is.null(census.data$error)) {
        return(census.data)
    }
    else {
        warning(census.data$message[[1]])
        return(NA)
    }
}

