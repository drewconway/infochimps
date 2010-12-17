ip.geo <-
function(ip.address) {
    geo.url<-paste(.InfochimpsEnv$data$de,"geo.json?ip=",ip.address,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    geo.get<-getURL(geo.url)
    geo.data<-fromJSON(geo.get)
    if(is.null(geo.data$error)) {
        return(geo.data)
    }
    else {
        warning(geo.data$message[[1]])
        return(NA)
    }
}

