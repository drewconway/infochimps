infochimps <-
function(api.key) {
    if(is.character(api.key)){
        .InfochimpsEnv$data$api.key<-api.key   
    }
    else{
        warning("API key must be a string")
    }
}

