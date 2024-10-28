###===================================================#
###             ANALYSE DES SCENARIOS HYDROLOGIQUES   #
###====================================================
## NETOYAGE
rm(list = ls())
## LIBRARIES
library(dplyr)
library(DBI)
library(RSQLite)
library(ggplot2)
library(reshape2)
library(plotrix)
library(hydroGOF)
library(RColorBrewer)
library(hyfo)
library(lubridate)
library(hydroTSM)
library(trend)
color.names <- rownames(brewer.pal.info)
## WORK SPACE
setwd("G:/IGEDD/Memoire/Projet_Nowkuy")
#FONCTION 
source(file = file.path("RScripts","FONCTIONS.R"))
## BASE DE DONNEES
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","ProjectDataBase.sqlite"))
## VIGNETTES
dplyr_vignette<- vignette(package="dplyr")$results%>%as.data.frame() 
vignette(dplyr_vignette$Item[1])
## ANALYSE DES DONNEES DE LA CALIBRATION
calib.periode <- interval(as.Date("2003-01-01"),as.Date("2008-12-31"))
valid.periode <- interval(as.Date("2009-01-01"),as.Date("2014-12-31"))

HydroModelDATA  <- dbReadTable(DataBase,"HydroModelDATA" )
QObs <- dbReadTable(DataBase,"QObsData")
write.table(x = HydroModelDATA,file ="Q_Sim.txt" ,sep = "\t",row.names = FALSE)
Qcalib <- QObs%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%calib.periode,!is.na(Debit))

Qvalid <- QObs%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%valid.periode,!is.na(Debit))

Model.Data <- HydroModelDATA%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(Model=="SWAT",DATE%in%Qcalib$DATE)

NSE(Model.Data$Debit,Qcalib$Debit)

### ANALYSE CALIBRATION & VALIDATOIN
QObs <- dbReadTable(DataBase,"QObsData")%>%
  as_tibble()%>%
  mutate(DATE=as.Date(DATE))%>%
  rename(Obs=Debit)%>%
  filter(DATE>=as.Date("2000-01-01"),DATE<=as.Date("2016-12-31"))%>%
  select(Obs)%>%unlist

HydroModelDATA  <- dbReadTable(DataBase,"HydroModelDATA" )%>%
  select(DATE,Model,Debit)%>%
  mutate(DATE=as.Date(DATE))%>%
  rename(Sim=Debit)%>%
  filter(DATE>=as.Date("2000-01-01"),DATE<=as.Date("2016-12-31"))%>%
  group_by(Model)%>%
  mutate(Obs=QObs)%>%
  ungroup()




mod.periode <- list(calib.periode,valid.periode)
mod.var <- c("Calibration","Validation")
ModData <- rbind()
for (i in 1:length(mod.periode)) {
  QSim.mod <-HydroModelDATA%>%
    filter(DATE%within%mod.periode[[i]])%>%
    mutate(days=yday(DATE))%>%
    group_by(Model,days)%>%
    summarize(Sim=round(mean(Sim,na.rm = TRUE),2),
              Obs=round(mean(Obs,na.rm = TRUE),2))%>%#
    mutate(operation=mod.var[i])
  ModData <- rbind(ModData,QSim.mod)
}
ModData <- ModData%>%
  ungroup()
ModDataH1 <- ModData%>%
  filter(!is.nan(Obs),days<=150)

ModData2 <- ModData%>%
  filter(!is.nan(Obs),days>=150)%>%
  rbind(ModDataH1)%>%
  group_by(Model,operation)%>%
  mutate(days=1:n())%>%
  melt(id.vars=c("days","Model","operation"))%>%
  mutate(variable=factor(variable,levels = c("Obs","Sim")))

ggplot(ModData2, aes(x=days,y=value))+
  geom_line(size=1.1,aes(color=variable))+
  scale_x_continuous(breaks = seq(0,366,50),labels = c(1,seq(0,366,50)[-1]))+
  facet_grid(operation~Model)+
  scale_color_manual("",values = c("black","red"),labels=c("Observation","Modèle"))+
  theme_bw()+
  xlab("Jours")+
  ylab(expression(paste("Débit [",{m^3},"/s]")))+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))



## Saisonnalité

ggplot(ModData2, aes(x=days,y=value))+
  geom_line(size=1.1,aes(color=variable))+
  scale_x_continuous(breaks = seq(0,366,50),labels = c(1,seq(0,366,50)[-1]))+
  facet_grid(operation~Model)+
  scale_color_manual("",values = c("black","red"),labels=c("Observation","Modèle"))+
  theme_bw()+
  xlab("Jours")+
  ylab(expression(paste("Débit [",{m^3},"/s]")))+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


## CRITERS DE PERFORMANCES
QObs <- dbReadTable(DataBase,"QObsData")%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%valid.periode)
ModelPerf <- rbind()

ModelName="SWAT"

ModelSimData  <- dbReadTable(DataBase,"HydroModelDATA" )%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%valid.periode,Model==ModelName)

Perf.Val <- t(gof(ModelSimData$Debit,QObs$Debit))%>%
  as_tibble()%>%
  mutate(Model=ModelName)%>%
  select(Model,everything())
 
ModelPerf <- rbind(ModelPerf,Perf.Val)

#write.table(x = ModelPerf,file = file.path("OUTPUT","Data","ModelPerfValid.txt"),sep = "\t",row.names = F,col.names = T )


