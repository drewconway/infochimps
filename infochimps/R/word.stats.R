word.stats <-
function(tok) {
    tok<-tolower(gsub("[[:punct:]]","",tok))
    word.url<-paste(.InfochimpsEnv$data$base,"word_stats.json?tok=",tok,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    word.get<-getURL(word.url)
    word.data<-fromJSON(word.get)
    # Simple error checking
    if(is.null(word.data$error)) {
        return(word.data)
    }
    else {
        warning(word.data$message[[1]])
        return(NA)
    }
}

