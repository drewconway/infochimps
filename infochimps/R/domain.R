domain <-
function(ip.address,session) {
    domain.url<-paste(session$de,"domain.json?ip=",ip.address,"&apikey=",session$api.key,sep="")
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

