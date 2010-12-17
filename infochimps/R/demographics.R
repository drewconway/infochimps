demographics <-
function(ip.address) {
    demographics.url<-paste(.InfochimpsEnv$data$de,"demographics.json?ip=",ip.address,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
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

