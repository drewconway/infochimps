word.stats <-
function(tok,session) {
    word.url<-paste(session$base,"word_stats.json?tok=",tok,"&apikey=",session$api.key,sep="")
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

