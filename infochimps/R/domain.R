domain <-
function(ip.address) {
    domain.url<-paste(.InfochimpsEnv$data$de,"domain.json?ip=",ip.address,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    domain.get<-getURL(domain.url)
    domain.data<-fromJSON(domain.get)
    if(is.null(domain.data$error)) {
        return(domain.data)
    }
    else {
        warning(domain.data$message[[1]])
        return(NA)
    }
}

