# TRAITEMENT PRIMAIRE DES DONNEES
## NETOYAGE
rm(list = ls())
## LIBRARIES
library(DBI)
library(RSQLite)
library(foreign)
library(raster)
library(dplyr)
## Repertoires
setwd("D:/IGEDD/Memoire/Projet_Nowkuy")
## Fonctions
source(file =file.path("RScripts","FONCTIONS.R"))
## Base de Données 
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","ProjectDataBase.sqlite"))
##
ThiessenP_dbf <- foreign::read.dbf(file =file.path("SIGProject","Data","NowkuyThiessenPolygon3.dbf"))%>%
  mutate(ID=1:8,Shape_Area=round(Shape_Area,2), Weidth=round(Shape_Area/sum(Shape_Area),2))%>%
  select(ID,id,Names,Long,Lat,Station,Shape_Area)%>%
  rename("forein_key"="id")
## Enregistrement
dbWriteTable(conn =DataBase,name = "Stations",ThiessenP_dbf,overwrite=TRUE )

ExtractPoint<- dbReadTable(conn =DataBase ,name = "Stations")
## EXTRACTION DES DONNEES
XYData <- ExtractPoint%>%
  dplyr::select(ID,Long,Lat)

ExtractPath <- "D:/PROJET/Article/Merging/Resultats"
VarDir <- c("RAIN_MRG_BF","TMAX_MRG_BF","TMIN_MRG_BF")
VarNames <- c("RAIN","TMAX","TMIN")
#
j <- 0
DataExtract <- cbind()
repeat{
  j <- j+1
  if(j>length(VarDir))break
  Extract_Files <- dir(path = file.path(ExtractPath,VarDir[j],"Extracted_DATA"),pattern = ".nc")
  ExtractFiles <- Extract_Files[grep(pattern = 19810101,x = Extract_Files):grep(pattern =20161231,x = Extract_Files)]
  
  if(VarNames[j]=="RAIN"){
    DATES <- as.integer(substr.mod(x =ExtractFiles,start = 16,stop = 23))
  }else{
    DATES <- as.integer(substr.mod(x =ExtractFiles,start = 6,stop = 13))
  }
  
  DATE <- DATES[DATES>=19810101&DATES<=20161231]
  ####
  n_iteration <- length(DATE)
  i <- 0
  VarData <- cbind()
  repeat{
    i <- i+1
    if(i>n_iteration)break
    print(paste0(VarDir[j]," : ", round(i/n_iteration,2)))
    NetCDFile <- raster::brick(x = file.path(ExtractPath,VarDir[j],"Extracted_DATA",ExtractFiles[i]))
    ExtracData <- raster::extract(x = NetCDFile,y = data.frame(x=XYData$Long,y=XYData$Lat))%>%
      as.data.frame()%>%
      mutate(Stat_ID=XYData$ID,
             DATE=DATE[i])%>%
      dplyr::select(Stat_ID,DATE,everything())
    colnames(ExtracData) <- c("Stat_ID","DATE",VarNames[j])
    
    VarData <- rbind(VarData,ExtracData)
  }
  if(is.null(DataExtract)){
    DataExtract <- VarData
  }else{
    VarData2 <- VarData%>%
      dplyr::select(3)
    DataExtract <- cbind(DataExtract,VarData2)
  }
  if(j==3){
    Data_Extract <- DataExtract%>%
      dplyr::select(Stat_ID,DATE,RAIN,TMAX,TMIN)
  }
  
}
dbWriteTable(conn = DataBase,name = "ObsData",value = Data_Extract)

