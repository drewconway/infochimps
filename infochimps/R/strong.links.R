strong.links <-
function(screen.name,session,user.id=NA) {
    if(is.na(user.id)) {
        strong.url<-paste(session$base,"strong_links.json?screen_name=",screen.name,"&apikey=",session$api.key,sep="")
    }
    else{
        strong.url<-paste(session$base,"strong_links.json?user_id=",user.id,"&apikey=",session$api.key,sep="")
    }
    strong.get<-getURL(strong.url)
    strong.data<-fromJSON(strong.get)
    # Simple error checking
    if(is.null(strong.data$error)){
        strong.edges<-do.call("rbind",strong.data$strong_links)
        strong.edges<-cbind(strong.data$user_id,strong.edges)
        strong.df<-as.data.frame(strong.edges)
        names(strong.df)<-c("user.id","strong.edge","link.weight")
        return(strong.df)
    }
    else{
        warning(strong.data$message[[1]])
        return(NA)
    }
}

