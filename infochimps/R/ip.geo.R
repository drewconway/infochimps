ip.geo <-
function(ip.address,session) {
    geo.url<-paste(session$de,"geo.json?ip=",ip.address,"&apikey=",session$api.key,sep="")
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

