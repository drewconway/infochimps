# File-Name:       language_comparison.R           
# Date:            2010-12-07                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Compare the word statistics for various programming languages
# Data Used:       
# Packages Used:   infochimps, ggplot2
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# Load libraries
library(infochimps)
library(ggplot2)
library(XML)

# From http://langpop.com/
prog.langs<-c("java","php","javascript","python","sql","perl","ruby","actionscript","assembly",
    "lisp","delphi","pascal","scheme","haskell","tcl","lua","fortran","coldfusion","ada","cobol",
    "erlang","smalltalk","scala","ocaml","forth","rexx","clojure","rstats")
    
# Create infochimps session
api.key<-"your.infochimps.api.key"
ic<-infochimps(api.key)

# Get word stats for all languages
lang.stats<-lapply(prog.langs,function(t) unlist(word.stats(t,ic)))
lang.df<-as.data.frame(do.call("rbind",lang.stats), stringsAsFactors=FALSE)
lang.df$global_stdev_ppb<-as.numeric(lang.df$global_stdev_ppb)
lang.df$range<-as.numeric(lang.df$range)
lang.df$global_freq_ppb<-as.numeric(lang.df$global_freq_ppb)

# Dummy for common words
ambig.terms<-c("ada","forth","lua")
common<-rep(0,nrow(lang.df))
common[match(ambig.terms,lang.df$tok)]<-1
lang.df$common<-as.factor(common)

# Sort by frequency
lang.df<-lang.df[with(lang.df, order(global_freq_ppb)),]


### Create bar plots
lang.pop<-ggplot(lang.df, aes(xmin=(1:nrow(lang.df))-.5,xmax=(1:nrow(lang.df))+.5,ymin=0,ymax=global_freq_ppb))+
    geom_rect(aes(fill="lightblue",color="grey"))+coord_flip()+scale_x_continuous(breaks=1:nrow(lang.df),labels=lang.df$tok)+
    theme_bw()+scale_fill_manual(value=c("lightblue"="lightblue"),legend=FALSE)+scale_colour_manual(values=c("grey"="grey"),legend=FALSE)+
    opts(title="Global Frequnecy of Computer Languages Mentioned on Twitter",panel.grid.minor=theme_blank())+
    ylab("Parts Per-Billion")
ggsave(plot=lang.pop,filename="slides/images/lang_pop1.png",width=8,height=6)
    
lang.pop2<-ggplot(lang.df, aes(xmin=(1:nrow(lang.df))-.5,xmax=(1:nrow(lang.df))+.5,ymin=-1,ymax=global_freq_ppb))+
    geom_rect(aes(fill=common,color="grey"))+coord_flip()+scale_x_continuous(breaks=1:nrow(lang.df),labels=lang.df$tok)+
    theme_bw()+ylab("Parts Per-Billion")+scale_fill_manual(values=c("0"="lightblue","1"="red"),legend=FALSE)+
    scale_colour_manual(values=c("grey"="grey"),legend=FALSE)+
    opts(title="Global Frequnecy of Computer Languages Mentioned on Twitter",panel.grid.minor=theme_blank())
ggsave(plot=lang.pop2,filename="slides/images/lang_pop2.png",width=8,height=6)

com.langs<-subset(lang.df,common!=1)
lang.pop3<-ggplot(com.langs, aes(xmin=(1:nrow(com.langs))-.5,xmax=(1:nrow(com.langs))+.5,ymin=0,ymax=global_freq_ppb))+
    geom_rect(aes(fill="lightblue",color="grey"))+coord_flip()+scale_x_continuous(breaks=1:nrow(com.langs),labels=com.langs$tok)+
    theme_bw()+scale_fill_manual(value=c("lightblue"="lightblue"),legend=FALSE)+scale_colour_manual(values=c("grey"="grey"),legend=FALSE)+
    opts(title="Global Frequnecy of Computer Languages Mentioned on Twitter",panel.grid.minor=theme_blank())+
    ylab("log(Parts Per-Billion)")+scale_y_log10()
ggsave(plot=lang.pop3,filename="slides/images/lang_pop3.png",width=8,height=6)

### Create scatter plots

github<-read.delim("data/github_ranks.tsv", sep="\t", stringsAsFactors=FALSE)
github$Language<-tolower(github$Language)

# Get StackOverflow data
get.stack<-function(tok) {
    tok<-gsub("/| ","-",tok)
    base.stack<-"http://stackoverflow.com/questions/tagged/"
    stack.tree<-htmlTreeParse(paste(base.stack,tok,sep=""),useInternalNodes=TRUE)
    tag.count<-getNodeSet(stack.tree,"//div[@class='module']/div[@class='summarycount al']")
    tag.num<-suppressWarnings(as.numeric(gsub(",","",xmlValue(tag.count[[1]]),fixed=TRUE)))
    if(is.na(tag.num)) {
        warning(paste("There are no StackOverflow questions tagged '",tok,"'.\nNA returned",sep=""))
    }
    return(tag.num)
}
tag.nums<-sapply(github$Language,get.stack)
github$tag.count<-tag.nums
github$tag.rank<-rank(tag.nums)

# Add to data frame
lang.df$tag.count<-tag.nums
lang.df<-lang.df[with(lang.df, order(tok)),]

# Get github data, and merge in
github<-github[with(github, order(Language)),]
lang.merge<-merge(lang.df, github, by.x="tok",by.y="Language")

# Calculate rank for Twitter mentions
twitter.rank<-rank(lang.merge$global_freq_ppb)
lang.merge$Twitter.Rank<-twitter.rank

# Write data set
write.csv(lang.merge, "data/prog_lang_data.csv",row.names=FALSE)

com.tags<-subset(lang.merge,common!=1)
tag.scatter2<-ggplot(com.tags, aes(x=global_freq_ppb,y=tag.count))+geom_text(aes(label=com.tags$tok,colour="darkred",size=3))+
    stat_smooth(legend=FALSE,method="lm",se=FALSE)+xlab("Mentions on Twitter [log(Parts Per-Billion)]")+ylab("log(Number) of Questions Tagged on StackOverflow")+
    theme_bw()+opts(title="SO Questions Tagged vs. Twitter Mentions\n(Ambiguous Tokens Removed)")+scale_size(legend=FALSE)+
    scale_x_log10()+scale_y_log10()+scale_colour_manual(values=c("darkred"="darkred"),legend=FALSE)
ggsave(plot=tag.scatter2,filename="slides/images/tag_scatter2.png",width=8,height=6)

tag.scatter3<-ggplot(com.tags, aes(x=Twitter.Rank,y=Rank))+geom_text(aes(label=com.tags$tok,colour="darkred",size=3))+
    stat_smooth(legend=FALSE,method="lm",se=FALSE)+xlab("Mentions on Twitter Ranking")+ylab("Language Popularity Ranking on Github")+
    theme_bw()+opts(title="Language Rank on Github vs. Twitter Mentions\n(Ambiguous Tokens Removed)")+scale_size(legend=FALSE)+
    scale_colour_manual(values=c("darkred"="darkred"),legend=FALSE)
ggsave(plot=tag.scatter3,filename="slides/images/tag_scatter3.png",width=8,height=6)

tag.scatter4<-ggplot(github, aes(x=tag.count,y=Rank))+geom_text(aes(label=github$Language,colour="darkred",size=3))+
    stat_smooth(legend=FALSE,method="lm",se=FALSE)+xlab("log(Number) of Questions Tagged on StackOverflow")+
    ylab("Language Popularity Ranking on Github")+theme_bw()+
    opts(title="SO Questions Tagged vs. Language Rank on Github\n(Ambiguous Tokens Removed)")+scale_size(legend=FALSE)+
    scale_colour_manual(values=c("darkred"="darkred"),legend=FALSE)
ggsave(plot=tag.scatter4,filename="slides/images/tag_scatter4.png",width=8,height=6)
