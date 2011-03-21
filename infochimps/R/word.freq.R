word.freq <-
function(word) {
    word.url<-paste(.InfochimpsEnv$data$language,"corpora/word_freq/bnc/word_stats?head_word=",URLencode(word),
        "&apikey=",.InfochimpsEnv$data$api.key,sep="")
    word.get<-getURL(word.url)
    word.data<-fromJSON(word.get)
    return(word.data)
}

