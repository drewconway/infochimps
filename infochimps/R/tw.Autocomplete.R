tw.Autocomplete <-
function(prefix) {
    auto.url<-paste(.InfochimpsEnv$data$social,"tw/util/autocomplete_sn.json?prefix=",URLencode(prefix),
        "&apikey=",.InfochimpsEnv$data$api.key,sep="")
    auto.get<-getURL(auto.url)
    auto.data<-fromJSON(auto.get)
    return(as.character(auto.data$completions))
}

