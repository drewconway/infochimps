geo.names <-
function(prefix, country="", state="") {
    geo.url<-paste(.InfochimpsEnv$data$location,"geonames/autocomplete/place_names.json?prefix=",URLencode(prefix),
        "&country=",URLencode(country),"&state=",URLencode(state),"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    geo.get<-getURL(geo.url)
    geo.data<-fromJSON(geo.get)
    return(as.character(geo.data$completions))
}

