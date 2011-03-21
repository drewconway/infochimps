# File-Name:       zzz.R           
# Date:            2010-12-17                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Create local environment for API data
# Data Used:       
# Packages Used:          
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# Local environment to store API data and user API key
.InfochimpsEnv<-new.env()
.InfochimpsEnv$data<-list()

.onLoad<-function(libname, pkgname) {
    if(is.null(.InfochimpsEnv$data)==FALSE) {
        .InfochimpsEnv$data <- list(
            api.key=NULL,
            base="http://api.infochimps.com/soc/net/tw/",
            de="http://api.infochimps.com/web/an/de/",
            ip="http://api.infochimps.com/web/an/ip_census/",
            social="http://api.infochimps.com/social/network/",
            science="http://api.infochimps.com/science/",
            location="http://api.infochimps.com/geo/location/",
            encyclopedic="http://api.infochimps.com/encyclopedic/dbpedia/",
            language="http://api.infochimps.com/language/",
            finance="http://api.infochimps.com/economics/finance/"
            )
    }
}

clean.george <- function(json.response) {gsub("(george_api_explorer|\\(|\\))","", json.response)}

jsonToDataFrame<- 
function(json, all.names) {
    json.vecs<-lapply(json$results, rbind)
    col.match<-lapply(json.vecs, function(x) match(all.names, colnames(x)))
    fixed.cols<-lapply(1:length(json.vecs), function(i) rbind(as.character(json.vecs[[i]][,col.match[[i]]])))
    data.matrix<-do.call(rbind, fixed.cols)
    data.df<-data.frame(data.matrix, stringsAsFactors=FALSE)
    names(data.df)<-all.names
    return(data.df)
}
