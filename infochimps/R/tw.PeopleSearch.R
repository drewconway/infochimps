tw.PeopleSearch <- 
function(search.string, from=0, to.df=TRUE) {
    search.url<-paste(.InfochimpsEnv$data$social,"tw/search/people_search.json?_from=",from,"&q=",
        URLencode(search.string),"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    search.get<-getURL(search.url)
    searh.get<-clean.george(search.get)
    search.data<-fromJSON(search.get)
    if(search.data$total=="0") {
        warning(paste("No results found for '",search.string,"'",sep=""))
        return(NA)
    }
    else {
        if(to.df) {
            all.names<-c("user_id","scraped_at","screen_name","name","url","location","description",
                "time_zone","utc_offset","lang","geo_enabled","verified","contributors_enabled")
            return(jsonToDataFrame(search.data, all.names))
        }
        else {
            return(search.data)
        }
    }
}