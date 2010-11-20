demographics <-
function(ip.address,session) {
    demographics.url<-paste(session$de,"demographics.json?ip=",ip.address,"&apikey=",session$api.key,sep="")
    demographics.get<-getURL(demographics.url)
    demographics.data<-fromJSON(demographics.get)
    if(is.null(demographics.data$error)) {
        return(demographics.data)
    }
    else {
        warning(demographics.data$message[[1]])
        return(NA)
    }
}

