strong.links <-
function(screen.name,user.id=NA) {
    if(is.na(user.id)) {
        strong.url<-paste(.InfochimpsEnv$data$base,"strong_links.json?screen_name=",screen.name,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    else{
        strong.url<-paste(.InfochimpsEnv$data$base,"strong_links.json?user_id=",user.id,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    strong.get<-getURL(strong.url)
    strong.data<-fromJSON(strong.get)
    # Simple error checking
    if(is.null(strong.data$error)){
        strong.edges<-do.call("rbind",strong.data$strong_links)
        strong.edges<-cbind(strong.data$user_id,strong.edges)
        strong.df<-as.data.frame(strong.edges, stringsAsFactors=FALSE)
        strong.names<-c("user.id","strong.edge","link.weight")
        names(strong.df)<-strong.names
        for(c in 1:length(strong.names)) {strong.df[,c]<-unlist(strong.df[,c])}
        return(strong.df)
    }
    else{
        warning(strong.data$message[[1]])
        return(NA)
    }
}