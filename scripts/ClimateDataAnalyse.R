### ANALYSE DES DONNEES 
rm(list = ls())
# Libraries
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
## Repertoire
setwd("D:/IGEDD/Memoire/Projet_Nowkuy")
#FONCTION
source(file = file.path("RScripts","FONCTIONS.R"))
#Base de données
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","ProjectDataBase.sqlite"))


## EVALUATION ET COMPARAISON DES MODELS
ObsDatas <- dbReadTable(conn = DataBase,name = "ObsMeanData")%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE))%>%
  filter(DATE>=as.Date("1981-01-01")& DATE<=as.Date("2005-12-31"))%>%
  group_by(YEARS)%>%
  summarize(MeanValue=sum(RAIN))%>%
  dplyr::select(YEARS,MeanValue)

MeanClimateData <- dbReadTable(conn = DataBase,name = "MeanClimateData")%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE))%>%
  filter(DATE>=as.Date("1981-01-01")& DATE<=as.Date("2005-12-31"),RCP=="rcp45")%>%
  group_by(Model_id,YEARS)%>%
  summarize(MeanValue=round(sum(PCP),2))
ModelCriter2 <- rbind()
for (i in 1:10) {
  MeanClimateData_1 <- MeanClimateData%>%
    filter(Model_id==i)
  ModelCriter1 <-cor(MeanClimateData_1$MeanValue,ObsDatas$MeanValue)%>% #t(gof(MeanClimateData_1$MeanValue,ObsDatas$MeanValue))%>%
    as.data.frame()%>%
    mutate(Model=i)%>%
    select(Model,everything())
  ModelCriter2 <- rbind(ModelCriter2,ModelCriter1)
}

Mean_Climate_Data <- MeanClimateData%>%
    group_by(Model_id)%>%
    summarize(SD=sd(x =MeanValue))



par(mar=c(0,0,0,0),mfcol=c(1,1),xpd=TRUE)
taylor_diagram(ref = ObsDatas$MeanValue,model = ObsDatas$MeanValue,add = F,
               col ="#000000",pos.cor = FALSE,xlab = "",ylab = "Ecart-type",main = "",
               ngamma = 4,sd.arcs = T,ref.sd = T,pcex =1,gamma.col = "black",normalize = F,
               grad.corr.lines = c(seq(0.1,1,by = 0.1),0.95,0.99),
               pch =19,cex.axis = .8)


color_names <- c(brewer.pal(9,"Set1"),brewer.pal(9,"Dark2"))[1:10]

rcp <- c("rcp45","rcp85")[1]
m_id <- 0
pch <- c(0,3,24,1,7,10,16,19,15,17)
repeat{
  m_id <- m_id+1
  if(m_id>10)break
    MeanClimateData2 <- MeanClimateData%>%
      filter(Model_id==m_id)
    
    #MeanCorData <-  biasCorrect(frc =MeanClimateData2 ,hindcast =MeanClimateData2 ,obs =ObsDatas ,method = "eqm",
                                 #preci = FALSE,extrapolate = "constant")
    
    taylor_diagram(ref = ObsDatas$MeanValue,model =MeanClimateData2$MeanValue,add = TRUE,
                   col =color_names[m_id],pos.cor = FALSE,xlab = "Ecart-type",ylab = "Ecart-type",main = "",
                   ngamma = 4,sd.arcs = T,ref.sd = T,pcex =1.5,gamma.col = "black",normalize = F,
                   grad.corr.lines = c(seq(0.1,1,by = 0.1),0.95,0.99),pch =pch[m_id])
  
}

#climatemodel <- dbReadTable(conn = DataBase,name = "ClimateModel")
clim.model <- dbReadTable(conn = DataBase,name = "ClimateModel")
n_end <- str_locate(string = clim.model$Model,pattern = "-")-1
clim.model.sort.name <- unique(str_sub(string =clim.model$Model,start = 1,end =n_end ))



legend(x =235,y =350,legend =c(clim.model.sort.name,"Obs"),col = c( color_names,"#000000"),pch = c(pch,20),pt.cex = 1.5)

## Calcul des critères 

## EVALUATION ET COMPARAISON DES MODELS
ObsDatas <- dbReadTable(conn = DataBase,name = "ObsMeanData")%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE))%>%
  filter(DATE>=as.Date("1981-01-01")& DATE<=as.Date("2005-12-31"))%>%
  group_by(YEARS)%>%
  summarize(MeanValue=sum(RAIN))%>%
  dplyr::select(YEARS,MeanValue)

MeanClimateData <- dbReadTable(conn = DataBase,name = "MeanClimateData")%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE>=as.Date("1981-01-01")& DATE<=as.Date("2005-12-31"), RCP=="rcp45")%>%
  acast(DATE~Model_id,value.var = "PCP")%>%as.data.frame%>%
  mutate(DATE=as.Date(rownames(.)))%>%
  dplyr::select(DATE,everything())%>%
  daily2annual.data.frame(FUN = sum,dates = 1,out.type = "data.frame")

criters.val <- sapply(X = MeanClimateData,FUN = function(x){
 gof(sim =x,obs=ObsDatas$MeanValue) 
})
ee <-  gof(sim =MeanClimateData$`1`,obs=ObsDatas$MeanValue) 
rownames(criters.val) <- rownames(ee)

write.table(x = criters.val,file = file.path("OUTPUT","Data","criters_val.txt"),sep = "\t",row.names = TRUE,col.names = TRUE)

## ETUDE DU CHANGEMENT CLIMATIQUE 
Obs_Data <- dbReadTable(conn = DataBase,name = "ObsMeanData")%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE),Month=month(DATE))%>%
  filter(DATE>=as.Date("1981-01-01")& DATE<=as.Date("2010-12-31"))%>%
  group_by(YEARS)%>%
  summarize(PCP=round(sum(RAIN),2),
            ETP=round(sum(EPT),2))


Obs_Datas <- rbind()  
for (i in c(1)) {
  Obs_Data1 <- Obs_Data%>% 
    mutate(RCP="rcp45",ID_Model=i)
  
  Obs_Data2 <- Obs_Data%>% 
    mutate(RCP="rcp85",ID_Model=i)
  Obs_Data12 <- rbind(Obs_Data1,Obs_Data2)
  Obs_Datas <- rbind(Obs_Datas,Obs_Data12)
}

Obs_Datas2 <- Obs_Datas%>%
  mutate(Var="Obs")%>%
  dplyr::select(RCP,YEARS,Var,PCP,ETP)%>%
  melt(id.vars=c("RCP","YEARS","Var"))
  


## Scenarios
RefModel <- dbGetQuery(conn = DataBase,statement = "SELECT*FROM MeanClimateData WHERE Model_id=1")
RefModel2 <- RefModel%>%
  dplyr::filter(DATE<=as.Date("2040-12-31")& DATE>=as.Date("2011-01-01"))%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE),Month=month(DATE))%>%
  group_by(RCP,YEARS)%>%
  summarize(PCP=round(sum(PCP),2),
            ETP=round(sum(EPT),2))%>%
  mutate(Var="CCCma")%>%
  dplyr::select(RCP,YEARS,Var,PCP,ETP)%>%
  melt(id.vars=c("RCP","YEARS","Var"))%>%
  ungroup()

#colnames(RefModel2) <-c("Model_id","RCP","Month","Var","variable","value")
PlotData <-rbind(Obs_Datas2,RefModel2)
LabelCOLNames <- c("RCP 4.5", "RCP 8.5")
#LabelROWNames <- c("CNRM-CERFACS-CNRM-CM5","NOAA-GFDL-GFDL-ESM2M")
#names(LabelROWNames) <- c(2,10)
names(LabelCOLNames) <- c("rcp45","rcp85")

PlotData2 <-RefModel2%>%# PlotData%>%
  filter(variable=="ETP")%>%
  mutate(Var=factor(x = Var,levels = c("Obs","CCCma")))
 
ggplot(data=PlotData2,aes(x=YEARS,y=value))+
  geom_line(size=1.15,show.legend = TRUE,color="blue")+
  geom_point(show.legend = FALSE,color="blue")+
  geom_smooth(method = "lm",color="black")+
  scale_x_continuous(breaks = c(seq(2011,2040,5),2040),labels =  c(seq(2011,2040,5),2040))+
  facet_grid(.~RCP,
             labeller = labeller(RCP=LabelCOLNames),scales = "free_y")+
  scale_color_manual("",values = c("black","red"),labels=c("Observation","CCCma"))+
  theme_bw()+
  xlab("Années")+
  ylab("EVAPOTRANSPIRATION [MM]")+
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

clim.model <- dbReadTable(conn = DataBase,name = "ClimateModel")
n_end <- str_locate(string = clim.model$Model,pattern = "-")-1
clim.model.sort.name <- unique(str_sub(string =clim.model$Model,start = 1,end =n_end ))

# TEST Statistiques

#RefModelData <- dbGetQuery(conn = DataBase,statement = "SELECT*FROM MeanClimateData WHERE Model_id=1 OR Model_id=2")
TestData <- RefModelData%>%
  dplyr::filter(DATE<=as.Date("2040-12-31")& DATE>=as.Date("2011-01-01"))
STATVALUES <- rbind()
var.names <- c("PCP","EPT","TMAX","TMIN")
IDModel <- c(1,2)
RCP <- c("rcp45","rcp85")
j <- 0
repeat{
  j=j+1
  if(j>2)break
  for (k in 1:2) {
    TestData2 <- TestData%>%
      mutate(DATE=as.Date(DATE))%>%
      filter(Model_id==IDModel[j],RCP==RCP[k])
      
    for (var in var.names) {
      if(var.names=="PCP"||var.names=="EPT"){
        TestData3 <- TestData2%>%
          dplyr::select(DATE,print(var))%>%
          daily2annual.data.frame(FUN = sum,dates =1)%>%
          as.vector()#
             }else{
        TestData3 <- TestData2%>%
          dplyr::select(DATE,print(var))%>%
          daily2annual.data.frame(FUN = mean,dates =1)%>%
          as.vector()# 
      }
      
      StatTest <- MannKendall(TestData3)#pettitt.test(TestData3)#
      StatTestData <- data.frame(STATValue=round(StatTest$sl,2))%>%
        mutate(Model_id=IDModel[j],RCP=RCP[k],variable=var)
      STATVALUES <- rbind(STATVALUES,StatTestData)
    }
    
  }
  
}
  STATVALUES_1 <- STATVALUES%>%
    filter(Model_id==1)
AnneIndex <- data.frame(Index=1:30,Annees=2011:2040)
MeanModelData <- RefModelData%>%
  filter(Model_id==1)%>%
  mutate(DATE=as.Date(DATE),
         YEARS=as.numeric(year(DATE)),
         Periode=ifelse(YEARS<2025,"Avant","Apres"))%>%
  group_by(RCP,Periode,YEARS)%>%
  summarize(SumVar=sum(EPT))%>%
  summarize(Mean=mean(SumVar))

## Graph
VarDatas <-STATVALUES%>%
  filter(variable%in%c("EPT"),Model_id==1)

  ggplot(VarDatas,aes(x=rep(1:30,2),y=STATValue))+
  geom_line(size=1.15,show.legend = FALSE,color="blue")+
  geom_point(show.legend = FALSE,color="black")+
  #geom_smooth(method = "lm",color="black")+
  scale_x_continuous(breaks = seq(1,30,4),labels = seq(2011,2040,4))+
  facet_grid(.~RCP,space = "free_y",labeller = labeller(.cols=LabelCOLNames))+
  scale_color_manual("",values = c("black","red"),labels=c("Observation","Modèle"))+
  theme_bw()+
  xlab("Années")+
  ylab("Statistique")+
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

