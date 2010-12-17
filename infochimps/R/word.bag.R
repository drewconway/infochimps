word.bag <-
function(screen.name,user.id=NA) {
    if(is.na(user.id)) {
        wordbag.url<-paste(.InfochimpsEnv$data$base,"wordbag.json?screen_name=",screen.name,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    else{
        wordbag.url<-paste(.InfochimpsEnv$data$base,"wordbag.json?user_id=",user.id,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    wordbag.get<-getURL(wordbag.url)
    wordbag.data<-fromJSON(wordbag.get)
    if(is.null(wordbag.data$error)) {
        # Get wordbag data
        words<-do.call("rbind", wordbag.data$toks)
        words.df<-as.data.frame(cbind(wordbag.data$user_id[[1]],words), stringsAsFactors=FALSE)
        words.names<-c("user.id","rel.freq","tok","user.freq.ppb")
        names(words.df)<-words.names
        for(c in 1:length(word.names)) {words.df[,c]<-unlist(words.df[,c])}
        words.list<-list(user.id=wordbag.data$user_id[[1]],vocab=wordbag.data$vocab[[1]],total.usages=wordbag.data$total_usages[[1]],tok.df=words.df)
        return(words.list)
    }
    else {
        warning(wordbag.data$message[[1]])
        return(NA)
    }
}

