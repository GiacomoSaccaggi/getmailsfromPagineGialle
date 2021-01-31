

path<-getwd()

library(xml2)
library(XML)
library(rvest)
library(gmailr)
library(RCurl)
library(plotrix)
library(fmsb)
library(RColorBrewer)
library(readr)
library(xml2)
library(XML)
library(rvest)
library(stringr, warn.conflicts = F)
library(readxl)


# 'lettura txt'
# Read in all files from a folder

prova<-(read.table(paste0(path,"/info.txt")))
if(ncol(prova)==1){
  informazioni<-as.character(prova[1,1])
}else{
  informazioni<-numeric()
  for(i in 1:ncol(prova)){
    informazioni<-paste(informazioni,as.character(prova[1,i]))
  }
  informazioni=substr(informazioni,2 , nchar(informazioni))
}

# 'associazione ricerca'
meta<-as.integer(str_locate(informazioni,";")[1,2])
luogo_info<-substr(informazioni,0,meta-1)

lavori<-substr(informazioni,meta+1,nchar(informazioni))
if(luogo_info=="Italia_tot"){
  Provincia<-c('Agrigento','Alessandria','Ancona','Aosta','Arezzo','Ascoli Piceno','Asti','Avellino','Bari','Barletta-Andria-Trani','Belluno','Benevento','Bergamo','Biella','Bologna','Bolzano','Brescia','Brindisi','Cagliari','Caltanissetta','Campobasso','Caserta','Catania','Catanzaro','Chieti','Como','Cosenza','Cremona','Crotone','Cuneo','Enna','Fermo','Ferrara','Firenze','Foggia','Forlì-Cesena','Frosinone','Genova','Gorizia','Grosseto','Imperia','Isernia',"L'Aquila",'La Spezia','Latina','Lecce','Lecco','Livorno','Lodi','Lucca','Macerata','Mantova','Massa-Carrara','Matera','Messina','Milano','Modena','Monza e Brianza','Napoli','Novara','Nuoro','Oristano','Palermo','Padova','Parma','Pavia','Perugia','Pesaro e Urbino','Pescara','Piacenza','Pisa','Pistoia','Pordenone','Potenza','Prato','Reggio Calabria','Ragusa','Ravenna','Reggio Emilia','Rieti','Rimini','Roma','Rovigo','Salerno','Sassari','Savona','Siena','Siracusa','Sondrio','Sud Sardegna','Taranto','Teramo','Terni','Torino','Trapani','Trento','Treviso','Trieste','Udine','Varese','Venezia','Verbano-Cusio-Ossola','Vercelli','Verona','Vibo Valentia','Vicenza','Viterbo')
  Regione<-c('Sicilia','Piemonte','Marche',"Valle d'Aosta",'Toscana','Marche','Piemonte','Campania','Puglia','Puglia','Veneto','Campania','Lombardia','Piemonte','Emilia-Romagna','Trentino-Alto Adige','Lombardia','Puglia','Sardegna','Sicilia','Molise','Campania','Sicilia','Calabria','Abruzzo','Lombardia','Calabria','Lombardia','Calabria','Piemonte','Sicilia','Marche','Emilia-Romagna','Toscana','Puglia','Emilia-Romagna','Lazio','Liguria','Friuli-Venezia Giulia','Toscana','Liguria','Molise','Abruzzo','Liguria','Lazio','Puglia','Lombardia','Toscana','Lombardia','Toscana','Marche','Lombardia','Toscana','Basilicata','Sicilia','Lombardia','Emilia-Romagna','Lombardia','Campania','Piemonte','Sardegna','Sardegna','Sicilia','Veneto','Emilia-Romagna','Lombardia','Umbria','Marche','Abruzzo','Emilia-Romagna','Toscana','Toscana','Friuli-Venezia Giulia','Basilicata','Toscana','Calabria','Sicilia','Emilia-Romagna','Emilia-Romagna','Lazio','Emilia-Romagna','Lazio','Veneto','Campania','Sardegna','Liguria','Toscana','Sicilia','Lombardia','Sardegna','Puglia','Abruzzo','Umbria','Piemonte','Sicilia','Trentino-Alto Adige','Veneto','Friuli-Venezia Giulia','Friuli-Venezia Giulia','Lombardia','Veneto','Piemonte','Piemonte','Veneto','Calabria','Veneto','Lazio')
}else{
  zone<-luogo_info
}


# 'ricerca'
if(luogo_info=="Italia_tot"){
  database_scar<-matrix(ncol = 5)
  colnames(database_scar)<-c("Sito-web","Email","Tipo di struttura","Provincia","Regione")
  posizione<-0
  for(zone in Provincia){
    posizione<-posizione+1
    regione<-Regione[posizione]
    errore=0
    for (i in 1:2000) {
      print(paste(lavori,zone,i))
      try({
      mail<-"le mail"
      page<-"ciao"
      try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
      if(length(page)==1){
        try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
      
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
      }}}}}}}}}
      if(length(page)!=1){
        try(txt<-as.character(html_nodes(page,xpath = '//a')))
        d<-str_locate(txt, "btn-black")
        h<-c(1:length(d[,1]))[-which(is.na(d[,1]))]
        sitiwebdent<-txt[h]
        sitodent<-numeric()
        for (t in 1:length(sitiwebdent)) {
          d<-str_locate(sitiwebdent[t],"www.paginegialle.it")
          if(is.na(as.integer(d[1,1]))){
            inizio<-as.integer(str_locate(sitiwebdent[t],"href=\"")[1,2])
            fine<-as.integer(str_locate(sitiwebdent[t],"\" target=\"_blank\"")[1,1])
            sitodent<-c(sitodent,substr(sitiwebdent[t],inizio+1,fine-1))
          }
        }
        if(length(sitodent)>0){
          for(t in 1:length(sitodent)){
            try(page<-read_html(sitodent[t]))
            try(txt<-as.character(html_nodes(page,xpath = '//a')))
            for(h in 1:length(txt)){
              d<-str_locate(txt[h],"mailto:")
              if(is.na(as.integer(d[1,1]))==F){
                inizio<-as.integer(str_locate(txt[h],"mailto:")[1,2])
                mail[t]<-substr(txt[h],inizio+1,nchar(txt[h]))
                fine<-as.integer(str_locate((mail[t]),"\"")[1,1])
                mail[t]<-substr(mail[t],0,fine-1)
              }
            }
          }
          database_scar<-rbind(database_scar,cbind(sitodent,mail,lavori,zone,regione))
        }
      }else{
        errore=errore+1
      }
      print(errore)
      if(errore>10){break}
    })}
  }
}else{
  database_scar<-matrix(ncol = 4)
  colnames(database_scar)<-c("Sito-web","Email","Tipo di struttura","Luogo")
  errore=0
  for (i in 1:2000) {
    print(paste(zone,i))
    try({
      mail<-"le mail"
      page<-"ciao"
      try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
      if(length(page)==1){
        try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
        
        if(length(page)==1){
          try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
          
          if(length(page)==1){
            try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
            
            if(length(page)==1){
              try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
              
              if(length(page)==1){
                try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
                
                if(length(page)==1){
                  try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
                  
                  if(length(page)==1){
                    try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
                    
                    if(length(page)==1){
                      try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
                      
                      if(length(page)==1){
                        try(page<-read_html(paste0("https://www.paginegialle.it/ricerca/",gsub(" ","%20",lavori),"/",gsub(" ","%20",zone),"/p-",i)))
                      }}}}}}}}}
      if(length(page)!=1){
        try(txt<-as.character(html_nodes(page,xpath = '//a')))
        d<-str_locate(txt, "btn-black")
        h<-c(1:length(d[,1]))[-which(is.na(d[,1]))]
        sitiwebdent<-txt[h]
        sitodent<-numeric()
        for (t in 1:length(sitiwebdent)) {
          d<-str_locate(sitiwebdent[t],"www.paginegialle.it")
          if(is.na(as.integer(d[1,1]))){
            inizio<-as.integer(str_locate(sitiwebdent[t],"href=\"")[1,2])
            fine<-as.integer(str_locate(sitiwebdent[t],"\" target=\"_blank\"")[1,1])
            sitodent<-c(sitodent,substr(sitiwebdent[t],inizio+1,fine-1))
          }
        }
        if(length(sitodent)>0){
          for(t in 1:length(sitodent)){
            try(page<-read_html(sitodent[t]))
            try(txt<-as.character(html_nodes(page,xpath = '//a')))
            for(h in 1:length(txt)){
              d<-str_locate(txt[h],"mailto:")
              if(is.na(as.integer(d[1,1]))==F){
                inizio<-as.integer(str_locate(txt[h],"mailto:")[1,2])
                mail[t]<-substr(txt[h],inizio+1,nchar(txt[h]))
                fine<-as.integer(str_locate((mail[t]),"\"")[1,1])
                mail[t]<-substr(mail[t],0,fine-1)
              }
            }
          }
          database_scar<-rbind(database_scar,cbind(sitodent,mail,lavori,zone))
        }
      }else{
        errore=errore+1
      }
      print(errore)
      if(errore>10){break}
    })}
}


# 'riordino'
dim(database_scar)
db<-data.frame(database_scar)
d<-which(is.na(db$Email))
if(length(d)>0){db<-db[-d,]}
d<-which(is.na(db$Sito.web))
if(length(d)>0){db<-db[-d,]}
d<-which(db$Email=="le mail")
if(length(d)>0){db<-db[-d,]}
d<-which(db$Sito.web=="")
if(length(d)>0){db<-db[-d,]}
d<-which(db$Email=="")
if(length(d)>0){db<-db[-d,]}
err<-numeric()
for(i in 1:length(db$Email)){
  db$Email[i]<-gsub("%20","",db$Email[i])
  db$Email[i]<-gsub("/","",db$Email[i])
  db$Email[i]<-gsub(",","",db$Email[i])
  db$Sito.web[i]<-gsub(",","",db$Sito.web[i])
  if(is.na(str_locate(db$Email[i],"%")[1,1])==F){
    err<-c(err,i)
  }
  if(is.na(str_locate(db$Email[i],"@")[1,1])){
    err<-c(err,i)
  }
}
if(length(err)>0){db<-db[-err,]}
liv<-levels(as.factor(db$Email))
a<-numeric()
for(i in 1:length(liv)){
  a<-c(a,which(db$Email==liv[i])[1])
}
d<-which(is.na(a))
if(length(d)>0){a<-a[-d]}
finale<-db[a,]

# 'salvo txt'
write.csv(finale,paste0(path,"/mail_scaricate.csv"))


# 'creo txt per dire che ho finito'
write.table("ciao",paste0(path,"/test.txt")) #, sep=";",col.names = FALSE, row.names = FALSE)
