rforcecom.queryMore <-
function(session, nextRecordsUrl){
 # Trim the first slash
 nextRecordsUrl <- sub("^/", "", nextRecordsUrl)
 
 # Load packages
 if(!require(XML)){ install.packages("XML"); stop(!require(XML)) }
 if(!require(RCurl)){ install.packages("RCurl"); stop(!require(RCurl)) }
 
 # Retrieve XML via REST API
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getSoqlEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], nextRecordsUrl, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
 curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 # Parse XML
 x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
 
 # Check whether it success or not
 errorcode <- NA
 errormessage <- NA
 try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
 try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
 if(!is.na(errorcode) && !is.na(errormessage)){
  stop(paste(errorcode, errormessage, sep=": "))
 }
 
 # Convert XML to data frame
 xns <- getNodeSet(xmlParse(t$value()),'//records')
 xdf <- xmlToDataFrame(xns)
 xdf.iconv <- data.frame(lapply(xdf, iconv, from="UTF-8", to=""))
 
 # Check whether it has next record
 try(nextRecordsUrl <- iconv(xmlValue(x.root[['nextRecordsUrl']]), from="UTF-8", to=""), TRUE)
 if(!is.na(nextRecordsUrl)){
  nextRecords <- rforcecom.queryMore(session, nextRecordsUrl)
  xdf.iconv <- rbind(xdf.iconv,nextRecords)
 }
 
 return(data.frame(xdf.iconv))
}

