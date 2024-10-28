# TRAITEMENT DES SCENARIOS CLIMATIQUES
#Netoyage
rm(list = ls())
#LIBRARIES
library(dplyr)
library(stringr)
library(airGR)
library(DBI)
library(RSQLite)
library(lubridate)
library(hyfo)
library(reshape2)
library(Evapotranspiration)
#Fixation du repertoire
setwd("D:/IGEDD/Memoire/Projet_Nowkuy/DataBase/scenario_climatique")
#FONCTION
source("D:/IGEDD/Memoire/Projet_Nowkuy/RScripts/FONCTIONS.R")
#Base de données
DataBase <- dbConnect(drv = RSQLite::SQLite(),"D:/IGEDD/Memoire/Projet_Nowkuy/DataBase/ProjectDataBase.sqlite")
Stations <- dbReadTable(conn =DataBase,name = "Stations" )
dir_names <- dir()[!dir()%in%c("ARCHIVES","OUTPUT")]

rcp <- c("rcp45","rcp85")
vars <- c("pcp","tmp")[1]
for(i in dir_names){
  #i=dir_names[1]#supprimé
  for(j in rcp){
    #j=rcp[1]#supprimé
    for(k in vars){
      #k=vars[1]#supprimé
      pcp_files <- dir(path = file.path(i,j),pattern = k)
      m=0
      for (h in paste0("pcp_",c(Stations$forein_key))) {
        #h =pcp_files[1]#supprimé
        h=sub(".txt","",h)
        #Suivi de boucle
        m=m+1
        if(m<=17){
          print(m)
        }else{
          print("Error")
          a=m
        }
       
        if(m==1){
          data_value1 <- read.table(file = file.path(i,j,paste0(h,".txt")),header = F,sep = ",",skip = 0)#%>%
            #select(V2)
          colnames(data_value1) <- h
        }
        if(m==2){
          data_value2 <- read.table(file = file.path(i,j,paste0(h,".txt")),header = F,sep = ",",skip = 0)#%>%
            #select(V2)
          data_values <- data_value1%>%
            transform(var=as.vector(data_value2))
            colnames(data_values) <- c(colnames(data_values)[-m],h)
        }
        if(!m%in%c(1:2)){
          data_value3 <- read.table(file = file.path(i,j,paste0(h,".txt")),header = F,sep = ",",skip = 0)#%>%
            #select(V2)
          data_values <- data_values%>%
            transform(var=as.vector(data_value3))
          colnames(data_values) <- c(colnames(data_values)[-m],h)
        }
      }
      
      #start_date1 <- as.character(read.table(file = paste(".", i,j,paste0(h,".txt"),sep = "/"),header = F,sep = ",",nrows = 1))
      start_date1 <- as.integer(data_values[1,1])#pcp
      start_date= as.Date(paste(substr(x = start_date1,start = 1,stop = 4),
                                substr(x = start_date1,start = 5,stop = 6),
                                substr(x = start_date1,start = 7,stop = 8),sep = "-"))
      
      #data_nrow <- nrow(data_values)
      data_values <- data_values%>%
        slice(-1)%>%
        mutate(DATE=seq.Date(from =start_date,by = "day",length.out =nrow(.)))%>%
        mutate(DATE=as.character(DATE))%>%
        select(DATE,everything())
      #dbWriteTable(conn = DataBase,name =paste0(i,"_",j,"_","PCP"),
                   #value = data_values,field.types=c(DATE="DATE"),overwrite=T )
      write.table(x = data_values,file =file.path("OUTPUT",paste0(i,"_",j,"_","PCP",".txt")) ,sep = "\t",row.names = F,col.names = T )
      
    }
  }
    
}
dbDisconnect(conn = DataBase)

test_write <- dbReadTable(conn = DataBase,name =paste0(i,"_",j,"_","PCP"))


##
dir_names <- dir()[!dir()%in%c("ARCHIVES","OUTPUT")]
rcp <- c("rcp45","rcp85")
vars <- c("pcp","tmp")
PCPDATA1 <- rbind()
TEMPDatas <- rbind()
for(i in dir_names){
  for(j in rcp){
    for(k in vars){
    if(k=="pcp"){
      print(paste(i,"_",j,"_",k))
      FilesNames <-paste0(k,"_",c(Stations$forein_key),".txt")
      for (h in FilesNames) {
          pcpdata <- read.table(file = file.path(i,j,h),header = F,sep = ",",skip = 0)%>%
            slice(-1)%>%
            mutate(RCP=j,Names=i,ID=substr(x = h,start = 5,stop = 6))
          colnames(pcpdata) <- c("PCP","RCP","Names","ID")
          DATE=seq.Date(from =as.Date("1951-01-01"),length.out = nrow(pcpdata),by = "day")
          
          pcpdata <- pcpdata%>%
            mutate(DATE=DATE)%>%
            filter(DATE>=as.Date("1980-01-01"))%>%
            select(DATE,ID,Names,RCP,everything())
          PCPDATA1 <- rbind(PCPDATA1,pcpdata)
        }
    }

    if(k=="tmp"){ 
      print(paste(i,"_",j,"_",k))
      FilesNames <-paste0(" ",k,"_",c(Stations$forein_key),".txt")
      for (h in FilesNames) {
        TempData <- read.table(file = file.path(i,j,h),header = F,sep = ",",skip = 1)%>%
          mutate(RCP=j,Names=i,ID=substr(x = h,start = 6,stop = 7))
        colnames(TempData) <- c("Tmax","Tmin","RCP","Names","ID")
        DATE=seq.Date(from =as.Date("1951-01-01"),length.out = nrow(TempData),by = "day")
        
        TempData <- TempData%>%
          mutate(DATE=DATE)%>%
          filter(DATE>=as.Date("1980-01-01"))%>%
          select(DATE,ID,Names,RCP,everything())
        TEMPDatas <- rbind(TEMPDatas,TempData)
        
        SCENARIOS <- inner_join(PCPDATA1,TEMPDatas,by=c("DATE","ID","Names","RCP"))%>%
          mutate(DATE=as.character(DATE), ID=str_remove_all(string = ID,pattern = fixed(".")))
        
      } 
      }

      }
    
      dbWriteTable(conn = DataBase,name ="SCENARIOS",value = SCENARIOS,field.types=c(DATE="DATE"),overwrite=TRUE)
      #value = data_values,field.types=c(DATE="DATE"),overwrite=T )
      #write.table(x = data_values,file =file.path("OUTPUT",paste0(i,"_",j,"_","PCP",".txt")) ,sep = "\t",row.names = F,col.names = T )
      
    }
}


### Correction des données 
OBSData <- dbReadTable(conn = DataBase,name = "ObsData")%>%
  mutate(DATE=as.Date(paste(substr.mod(x = DATE,start = 1,stop = 4),
                            substr.mod(x = DATE,start = 5,stop = 6),
                            substr.mod(x = DATE,start = 7,stop = 8),
                            sep = "-")))

OBSData2 <- left_join(OBSData,Stations,by=c("Stat_ID"="ID"))%>%
  dplyr::select(DATE,Stat_ID,forein_key,RAIN,TMAX,TMIN)

### BOUCLES

ADJUSTPCPDATA <- rbind()
ADJUSTMAXDATA <- rbind()
ADJUSTMINDATA <- rbind()
vars <- c(5,6,7)
rcp <- c("rcp45","rcp85")

for(v in vars){ 
f <- v-1
for (l in 1:10) {
 for (r in rcp) {
    ModelRCPData <- dbGetQuery(conn = DataBase,statement = sprintf("SELECT * FROM SCENARIOS WHERE ID_Model=%s",l))%>%
    mutate(DATE=as.Date(DATE))%>%
    filter(DATE>=as.Date("1981-01-01"),RCP==r)
    
    ModelRCPData2 <- ModelRCPData[,c(1,2,v)] %>%
          acast(formula=DATE~ID)%>%
          as.data.frame()%>%
          mutate(DATE= base::row.names.data.frame(.))%>%
          dplyr::select(DATE,sort(colnames(.)))
        
    obs <- OBSData2[,c(1,3,f)]%>%
          filter(DATE<=as.Date("2016-12-31")& DATE>=as.Date("1981-01-01"))%>%
          acast(formula=DATE~forein_key)%>%
          as.data.frame()%>%
          mutate(DATE= base::row.names.data.frame(.))%>%
          dplyr::select(DATE,sort(colnames(.)))
        
    hindcast <- ModelRCPData2%>%
          filter(DATE<=as.Date("2016-12-31")& DATE>=as.Date("1981-01-01"))
        
    frc <- ModelRCPData2%>%
          filter(DATE>=as.Date("1981-01-01"))
        
       
    if(v==5){
      PCPAjustData <- biasCorrect(frc =frc,hindcast = hindcast,obs =obs,method = "eqm",preci = TRUE,prThreshold = 1,extrapolate = "constant")%>%
        melt(id.vars=c("DATE"))%>%
        mutate(ID_Model=l,RCP=r)
      
      colnames(PCPAjustData) <- c("DATE","ID","PCP","ID_Model","RCP")
      
      ADJUSTPCPDATA <- as_tibble(rbind(ADJUSTPCPDATA,PCPAjustData))
      
    }
    if(v==6){
      TmaxAjustData <- biasCorrect(frc =frc,hindcast = hindcast,obs =obs,method = "eqm",preci = FALSE,extrapolate = "constant")%>%
        melt(id.vars=c("DATE"))%>%
        mutate(ID_Model=l,RCP=r)
      
      colnames(TmaxAjustData) <- c("DATE","ID","TMAX","ID_Model","RCP")
      
      ADJUSTMAXDATA <- as_tibble(rbind(ADJUSTMAXDATA,TmaxAjustData))
      
    }
    if(v==7){
      TminAjustData <- biasCorrect(frc =frc,hindcast = hindcast,obs =obs,method = "eqm",preci = FALSE,extrapolate = "constant")%>%
        melt(id.vars=c("DATE"))%>%
        mutate(ID_Model=l,RCP=r)
      
      colnames(TminAjustData) <- c("DATE","ID","TMIN","ID_Model","RCP")
      
      ADJUSTMINDATA <- as_tibble(rbind(ADJUSTMINDATA,TminAjustData))
      
    }
      }

    }
 }    


## FORMATAGE SWAT
#setwd("../")
ClimateModel <- dbReadTable(conn = DataBase,name = "ClimateModel")
StationID <- dbReadTable(conn = DataBase,name = "Stations")%>%
  dplyr::select(forein_key,Names,Lat,Long,ELEVATION)%>%
  mutate(Names=str_remove(Names,"Station_"))
colnames(StationID) <- c("ID","NAME","LAT","LONG","ELEVATION")

model_id <- unique(ClimateModel$ID)
rcp <- c("rcp45","rcp85")
vars <- c("pcp","tmp")

for(p in model_id){#
  model_name <- ClimateModel$Model[p]#
  if(!dir.exists(file.path("SWATDATA",model_name))) dir.create(file.path("SWATDATA",model_name))
  for (m in rcp) {#
    for (w in vars) {#
      if(!dir.exists(file.path("SWATDATA",model_name,m))) dir.create(file.path("SWATDATA",model_name,m))
      if(w=="pcp"){#
          Model_Rcp_Data <- dbGetQuery(conn = DataBase,statement = sprintf("SELECT DATE,ID,RCP,PCP FROM ADJUSTDATAS WHERE ID_Model=%i",p))%>%
          filter(RCP==print(m))%>%
          dplyr::select(-RCP)%>%
            acast(DATE~ID)
          for (s in colnames(Model_Rcp_Data)) {#
            swatdat <- Model_Rcp_Data[,s]
            swat_data <- c("19810101",swatdat)
            write.table(x = swat_data,file = file.path("SWATDATA",model_name,m,paste0("pcp_",s,".txt")),row.names = FALSE,col.names = FALSE,quote = FALSE)
          }#
          }#
      if(!w=="pcp"){#
        Model_Rcp_Data <- dbGetQuery(conn = DataBase,statement = sprintf("SELECT DATE,ID, RCP,ID_Model,TMAX,TMIN FROM ADJUSTDATAS WHERE ID_Model=%i",p))%>%
          filter(RCP==print(m))%>%
          dplyr::select(-RCP)
        
        TMAXData <- Model_Rcp_Data%>%
          dplyr::select(DATE,ID,TMAX)%>%
          acast(DATE~ID)
          
        TMINData <- Model_Rcp_Data%>%
          dplyr::select(DATE,ID,TMIN)%>%
          acast(DATE~ID)#
        for (s in colnames(TMINData)) {#
            swat_data <- c("19810101",paste(round(TMAXData[,s],2),round(TMINData[,s],2),sep = ","))
            write.table(x = swat_data,file = file.path("SWATDATA",model_name,m,paste0("tmp_",s,".txt")),row.names = FALSE,col.names = FALSE,quote = FALSE)
        }#
        }#
      StationID %>%
        mutate(NAME=paste0(w,"_",NAME))%>%
        write.table(file = file.path("SWATDATA",model_name,m,paste0(w,".txt")),row.names = FALSE,col.names =TRUE,quote = FALSE,sep=",")
    }
  }
  }

#dbWriteTable(conn = DataBase,name = "Stations",value = Stations,overwrite=TRUE)
### Calcul des valeurs moyennes par la Méthode de Thiessen
stations_weidth <- as.matrix(Stations$Shape_Area[order(Stations$forein_key)] /sum(Stations$Shape_Area))

# initialisation de boucle)

rcp <- c("rcp45","rcp85")
value.var <- c("PCP","TMAX","TMIN")
MeanClimateData <- rbind()
VarData <- list()
model_id <- 0
repeat{#
  model_id <- model_id+1
  if(model_id>10)break
  model_data <- dbGetQuery(conn = DataBase,statement = sprintf("SELECT * FROM ADJUSTDATAS WHERE ID_Model=%i",model_id))
  for (r in rcp) {#
    rcp_data <- model_data%>%
      filter(RCP==print(r))
   
    for (var in value.var) {#☺
      var.data <- rcp_data%>%
        acast(DATE~ID,value.var =print(var))
      DATE <- as.Date(rownames(var.data))
      var.mean.value <-var.data%*%stations_weidth
      VarData[[var]] <- var.mean.value
      
    }
    VARDATAS<-  as.data.frame(VarData)%>%
      mutate(DATE=DATE,RCP=print(r),Model_id=print(model_id))%>%
      dplyr::select(DATE,Model_id,RCP,everything())
    MeanClimateData  <- rbind(MeanClimateData,VARDATAS)
  }
}

MeanClimateData2 <- MeanClimate_Data%>%
  mutate(DATE=as.Date(DATE))%>%
  left_join(MeanClimateData,by=c("DATE","Model_id","RCP"))%>%
  mutate(DATE=as.character(DATE))
dbWriteTable(conn = DataBase,name ="MeanClimateData",value = MeanClimateData2,field.types=c(DATE="DATE"),overwrite=TRUE )


## OBS DATA
ObsDatas <- dbReadTable(conn = DataBase,name = "ObsData")
  
stations_weidth2 <- as.matrix(Stations$Shape_Area[order(Stations$ID)] /sum(Stations$Shape_Area))
value.var2 <- c("EPT") #c("RAIN","TMAX","TMIN")
ObsMeanData <- list()
for (var in value.var2) {#☺
  var.obsdata <- ObsDatas%>%
    acast(DATE~Stat_ID,value.var =print(var))
  DATE <- as.Date(rownames(var.obsdata))
  obs.mean.value <-var.obsdata%*%stations_weidth2
  ObsMeanData[[var]] <- obs.mean.value
  
}
ObsMeanData2 <- as.data.frame(ObsMeanData)%>%
  mutate(DATE=as.character(DATE))%>%
  dplyr::select(DATE,everything())

## CACUL DE L'EVAPOTRANSPIRATION
TempData <- dbGetQuery(conn = DataBase,statement = "SELECT*FROM ObsData")%>%
  mutate(DATE=rep(seq.Date(as.Date("1981-01-01"),as.Date("2016-12-31"),by = 1),8))
StationElevation <- dbGetQuery(conn = DataBase,statement = "SELECT*FROM Stations")
EPTData <- rbind()
for(i in StationElevation$ID){ 
  Lat<- StationElevation$Lat[which(StationElevation$ID==i)]
  TempDataSelect <- TempData%>%
    filter(Stat_ID==i)%>%
    mutate(Jday=yday(DATE),Tmean=(TMAX+TMIN)/2)%>%
    dplyr::select(Jday,Tmean)
  attach(TempDataSelect)
  EPT <- data.frame(EPT=round(airGR::PE_Oudin(JD =Jday,Temp = Tmean,Lat =Lat,LatUnit = "deg"),2))%>%
    mutate(ID=i)
  detach(TempDataSelect)
  EPTData <- rbind(EPTData,EPT)
}
Temp_Data <- TempData%>%
  arrange(Stat_ID)
EPTData2 <- EPTData%>%
  arrange(ID)%>%
  mutate(DATE=Temp_Data$DATE)%>%
  dplyr::select(DATE,ID,everything())

TempData2 <-Temp_Data%>%
  left_join(EPTData2,by=c("DATE"="DATE","Stat_ID"="ID"))

dbWriteTable(conn = DataBase,name ="ObsMeanData" ,value = DataObsMean,overwrite=TRUE,field.types=c(DATE="DATE"))
TempDataTest <- dbReadTable(conn = DataBase,name = "ObsMeanData")

