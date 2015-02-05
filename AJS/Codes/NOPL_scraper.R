library(XML)
library(httr)
library(RCurl)

getCookiesNO<-function(url='http://www.nieruchomosci-online.pl/'){
  require(XML)
  require(httr)
  require(RCurl)
  doc<-GET(url)
  return(doc$cookies)
}

getURLsIDsNO<-function(cookies,
                       startURL='http://www.nieruchomosci-online.pl/szukaj.html?2,mieszkanie,sprzedaz,wielkopolskie,Pozna%C5%84:9917&p=',
                       n=1){
  doc2<-htmlParse(getURI(paste0(startURL,n),.encoding='utf-8',
                         followlocation=TRUE,
                         cookie=paste('PHPSESSID',
                                cookies$PHPSESSID,sep='=')),
                  ,encoding='utf-8')
  values<-xpathSApply(doc2,'//input[@type="hidden"]',xmlGetAttr,'value')
  ids<-unique(unlist(regmatches(values,regexec('[0-9]{7,}',values))))
  siteUrl<-paste0('http://poznan.nieruchomosci-online.pl/mieszkanie,na-sprzedaz/',ids,'.html')
  return(list(ids=ids,urls=siteUrl))
}


#### get details
getDetailPageResultsNO<-function(url,cookies){
  doc3<-htmlParse(getURI(url,.encoding='utf-8',
               followlocation=TRUE,
               cookie=paste('PHPSESSID',cookies$PHPSESSID,sep='=')),
               encoding='utf-8')

  ### czy strona istnieje ##
  uwaga<-xpathApply(doc3,'//div[@class="text_404"]',xmlValue)
  if (length(uwaga)>0){
    warning('Strona nie istnieje',call.=FALSE)
    return(data.frame(blad='BLAD'))
  }

  ### reszta
  tab1<-xpathSApply(doc3,'//ul[@class="marg10_bottom"]/li/b',xmlValue)
  tab1<-gsub('\\s+|\\:','',gsub("\\b(\\w)","\\U\\1",tab1, perl=TRUE))
  tab2<-xpathSApply(doc3,'//ul[@class="marg10_bottom"]/li/span',xmlValue)
  tab2<-gsub('\\n+|\\s+$|^\\s+|;','',tab2)
  detailInfo<-as.data.frame(t(tab2))
  names(detailInfo)<-tab1

  tab3<-xpathSApply(doc3,'//div[@class="offer_box marg30_bottom"]//ul/li/b',xmlValue)
  tab3<-gsub('\\s+|\\:','',gsub("\\b(\\w)","\\U\\1",tab3, perl=TRUE))
  tab4<-xpathSApply(doc3,'//div[@class="offer_box marg30_bottom"]//ul/li/span',xmlValue)
  detailInfo2<-as.data.frame(t(tab4))
  names(detailInfo2)<-tab3

  detailDesc<-xpathSApply(doc3,'//div[@id="adv-desc" or @id="invest-desc"]/div[2]',xmlValue)
  if (length(detailDesc)==0){
    detailDesc<-xpathSApply(doc3,'//div[@id="adv-desc" or @id="invest-desc"]',xmlValue)
    if (length(detailDesc)==0){
      detailDesc<-'BRAK'
    }
  }

  detailOfferContact<-xpathSApply(doc3,'//ul[@class="no-print"]/li',xmlValue)[1]
  if (is.null(detailOfferContact)){
    detailOfferContact<-'BRAK'
  }

  detailDateAct<-xpathSApply(doc3,'//p[@class="offer-date"]/b',xmlValue)

  detailResults<-data.frame(url=url,
                            detailInfo,detailInfo2,
                            detailDesc=detailDesc,
                            detailOfferContact=detailOfferContact,
                            detailDateAct=detailDateAct)
  return(detailResults)
}




