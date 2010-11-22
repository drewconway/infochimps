conversations <-
function(screen.name.a,screen.name.b,session,user.id.a=NA,user.id.b=NA) {
    if(is.na(user.id.a) & is.na(user.id.a)) {
        conversation.url<-paste(session$base,"conversation.json?user_a_sn=",screen.name.a,"&user_b_sn=",screen.name.b,"&apikey=",session$api.key,sep="")
    }
    else {
        if(is.na(user.id.a)==FALSE & is.na(user.id.a)==FALSE) {
            conversation.url<-paste(session$base,"conversation.json?user_a_id=",user.id.a,"&user_b_id=",user.id.b,"&apikey=",session$api.key,sep="")
        }
        else {
            if(is.na(user.id.na)) {
                conversation.url<-paste(session$base,"conversation.json?user_a_sn=",screen.name.a,"&user_b_id=",user.id.b,"&apikey=",session$api.key,sep="")
            }
            else {
                conversation.url<-paste(session$base,"conversation.json?user_a_id=",user.id.a,"&user_b_sn=",screen.name.b,"&apikey=",session$api.key,sep="")
            }
        }
    }
    conversation.get<-getURL(conversation.url)
    # Fix JSON for proper handling for conversation IDs
    conversation.get<-gsub("([0-9]+)","\\\"\\1\\\"\\2",conversation.get,perl=TRUE)
    conversation.data<-fromJSON(conversation.get)
    # Simple error checking
    if(is.null(conversation.data$error)) {
        user.id.a<-conversation.data$user_a_id[[1]]
        user.id.b<-conversation.data$user_b_id[[1]]
        conversations.matrix<-suppressWarnings(do.call("rbind", conversation.data$conversations))
        if(dim(conversations.matrix)[2]<3) {
            # JSON request returns no reply-to data
            reply.to<-NA
        }
        else {
            reply.to<-sapply(1:nrow(conversations.matrix), function(x) ifelse(conversations.matrix[x,2]=="re", conversations.matrix[x,3], NA))
        }
        conversations.df<-cbind(user.id.a, user.id.b, conversations.matrix[,1], conversations.matrix[,2],reply.to)
        conversations.df<-as.data.frame(conversations.df)
        names(conversations.df)<-c("user.id.a","user.id.b","conversation.id","conversation.type","reply.to.id")
        return(conversations.df)
    }
    else {
        warning(conversation.data$message[[1]])
        return(NA)
    }
}

