###===================================================#
###      PREPARATION DES DONNEES DE CDT               #
###====================================================
## NETOYAGE
rm(list = ls())
##LIBRARIES
library(dplyr)
library(foreign)
library(raster)
library(reshape2)
##WORK SPACE
setwd("D:/PROJET/Article/Merging/Resultats")
##
Cordinate.Station <- read.dbf(file = "D:/IGEDD/Memoire/Projet_Nowkuy/DataBase/GeoData/Station_Fictives.dbf")%>%
  mutate(Name=paste0("Stat_",1:24))%>%
  dplyr::select(Name,Long,Lat)
##
dir.name <- c("RAIN","TMAX","TMIN")
CLIMDATA_BV_Nowk <- rbind()
j <- 0
repeat{
  j <- j+1
  if(j>length(dir.name))break
  file_path <- file.path(paste0(dir.name[j],"_MRG_BF"),"Extracted_DATA")
File.Names <- dir(path = file_path)
if(dir.name[j]=="RAIN"){
  DATE <- seq.Date(from = as.Date("1981-01-01"),
                   to = as.Date("2017-12-31"),by = "d")
}else{
  DATE <- seq.Date(from = as.Date("1961-01-01"),
                   to = as.Date("2017-06-30"),by = "d")
}
ExtractData <- rbind()
k <- 0
for (i in File.Names) {
  print(paste0("Extract ", dir.name[j]," :", round(k/length(File.Names)*100,2), "%" ))
  k <- k+1
  file_brick <- brick(x = file.path(file_path,i))
  extracted_data <- t(round(raster::extract(x = file_brick,Cordinate.Station[,-1]),2))
  ExtractData <- rbind(ExtractData,extracted_data)
  if(k==length(File.Names)){
    colnames(ExtractData) <- Cordinate.Station$Name
    ExtractData2 <- ExtractData%>%
      as_tibble()%>%
      mutate(DATE=DATE)%>%
      dplyr::select(DATE,everything())%>%
      melt(id.vars="DATE")%>%
      mutate(VAR=dir.name[j])
    colnames(ExtractData2) <- c("DATE","Station","Value","Variable")

  }
}
CLIMDATA_BV_Nowk <- rbind(CLIMDATA_BV_Nowk,ExtractData2)

}
#CLIMDATA_BV_Nowk$DATE <- as.character(CLIMDATA_BV_Nowk$DATE)
#dbWriteTable(conn = DataBase,name = "CDT_DATA",value = CLIMDATA_BV_Nowk,field.types=c(DATE="DATE"))

CDTData <- dbGetQuery(conn = DataBase,statement = "SELECT*FROM CDT_DATA WHERE Variable!='RAIN'")%>%
  group_by(DATE,Station)%>%
  summarize(TMean=mean(Value))%>%
  acast(DATE~Station,value.var = "TMean")%>%
  as.data.frame%>%
  mutate(DATE=as.Date(rownames(.)))%>%
  dplyr::select(DATE,everything())%>%
  daily2annual.data.frame(mean,na.rm = TRUE,dates =1)%>%
  as.data.frame%>%
  mutate(YEARS=rownames(.))%>%
  filter(YEARS>=1991,YEARS<=2015)%>%
  mutate(GroupeVar=sort(rep(1:5,5)))%>%
  dplyr::select(YEARS,GroupeVar,everything())%>%
  group_by(GroupeVar)%>%
  summarize(across(Stat_1:Stat_9,~round(mean(.x),2)))%>%
  mutate(DATE=str_remove_all(string = as.character(seq.Date(from = as.Date("2000-01-01"),to = as.Date("2000-01-05"),by = "d")),pattern = "-"))%>%
  dplyr::select(DATE,everything())%>%
  ungroup()

ENTETE <- t(Cordinate.Station)[2:3,]%>%as.data.frame()%>%
  mutate(DATE=c("LON","DAILY/LAT"))%>%
  dplyr::select(DATE,everything())

colnames(ENTETE) <- c("DATE",t(Cordinate.Station)[1,])
CDTDataFormated <- rbind(ENTETE,CDTData[,-c(2)])

save_path <- "D:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data/CDTData"
write.table(x =CDTDataFormated,file = file.path(save_path,"TMEAN_CDT.txt"),sep = "\t",row.names = FALSE,quote = FALSE,na = "-99")


