trstrank <-
function(screen.name,user.id=NA) {
    if(is.na(user.id)) {
        trstrank.url<-paste(.InfochimpsEnv$data$base,"trstrank.json?screen_name=",screen.name,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    else{
        trstrank.url<-paste(.InfochimpsEnv$data$base,"trstrank.json?user_id=",user.id,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    trstrank.get<-getURL(trstrank.url)
    trstrank.data<-fromJSON(trstrank.get)
    # Simple error checking
    if(is.null(trstrank.data$error)) {
        return(trstrank.data)
    }
    else {
        warning(trstrank.data$message[[1]])
        return(NA)
    }
}

