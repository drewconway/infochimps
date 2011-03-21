ufo <-
function(search.string, from=0, to.df=TRUE) {
    ufo.url<-paste(.InfochimpsEnv$data$science,"astronomy/seti/nuforc/ufo_sightings_search.json?_from=",from,"&q=",
        URLencode(search.string),"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    ufo.get<-getURL(ufo.url)
    ufo.data<-fromJSON(ufo.get)
    if(ufo.data$total=="0") {
        warning(paste("No results found for '",search.string,"'",sep=""))
        return(NA)
    }
    else {
        if(to.df) {
            all.names<-c("sighted_at", "reported_at", "location", "shape", "duration", "description")
            return(jsonToDataFrame(ufo.data, all.names))
        }
        else {
            return(ufo.data)
        }
    }
}