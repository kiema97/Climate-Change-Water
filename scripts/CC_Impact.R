###===================================================#
###      IMPACT DU CHANGEMENT CLIMATIQUE SUR LES RE   #
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
library(stringr)
library(forcats)
library(RColorBrewer)
##Vignettes
dplyrVignette <- vignette(package = "dplyr")[[4]]%>%as.data.frame()
vignette(package = "dplyr",topic = dplyrVignette$Item[1])
##color
DDColor <- RColorBrewer::brewer.pal.info%>%
  mutate(name=rownames(.))

RColorBrewer::display.brewer.all()

## WORK SPACE
setwd("D:/IGEDD/Memoire/Projet_Nowkuy")
#FONCTION 
source(file = file.path("RScripts","FONCTIONS.R"))
## BASE DE DONNEES
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","ProjectDataBase.sqlite"))


## IMPORTATION DES DONNNEES
EtudePeriode <- interval(start =as.Date("2011-01-01"),end = as.Date("2040-12-31") )
HistoricPeriode <- interval(start =as.Date("1981-01-01"),end = as.Date("2010-12-31") )

QObserve <- dbReadTable(conn = DataBase,
                        name ="QObsData")%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE))%>%
  filter(DATE%within%HistoricPeriode)%>%
  group_by(YEARS)%>%
  summarise(Module=mean(Debit,na.rm=TRUE)%>%round(2))%>%
  mutate(Type="Obs")

sd(QObserve$Module,na.rm=TRUE)

#write.table(x = QObserve,file = "DebitObsNowkuy.txt",sep = "\t",row.names = FALSE,na = "",col.names = TRUE )


ClimateModel <- dbReadTable(conn = DataBase,name = "ClimateModel")

SCENARIOSHYDRO <- dbReadTable(DataBase,"SCENARIOSHYDRO4")%>%
  as_tibble()%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%EtudePeriode)%>%
  mutate(RCP=fct_collapse(as.factor(RCP),RCP4.5=c("rcp45","RCP45"),RCP8.5=c("rcp85","RCP85")))
  

  
DebitMoyen <-SCENARIOSHYDRO%>%
  mutate(YEARS=year(DATE))%>%
  group_by(Model,ClimateModel,RCP,YEARS)%>%
  summarize(Module=round(mean(FLOW_OUT,na.rm = TRUE),2))%>%
  #group_by(HydroModel,ClimateModel,RCP,YEARS)%>%
  mutate(ModuleIndex=round((Module-mean(Module))/sd(Module),2),
         IndexType=ifelse(ModuleIndex>0,"Hummide","Seche"))%>%
  #filter(YEARS>=2011,ClimateModel==10)%>%
  dplyr::ungroup()
ModuleStatistic <- DebitMoyen%>%
  filter(Model=="SWAT",RCP=="RCP4.5")
summary.default(ModuleStatistic$Module)

DebitMoyen_1 <- DebitMoyen%>%
  dplyr::select(Model,RCP,YEARS,Module)%>%
  rename(Sim=Module)%>%
  melt(id.vars=c("Model","RCP","YEARS"))
  #filter(Model=="SWAT",RCP=="RCP4.5")

ObsDebitMoyen <- DebitMoyen_1%>%
  dplyr::select(Model,RCP)%>%
  group_by(Model,RCP)%>%
  mutate(YEARS=QObserve$YEARS,variable=QObserve$Type,value=QObserve$Module)

ObsSimData <- rbind(ObsDebitMoyen,DebitMoyen_1)
summary.default(DebitMoyen_1$Module)

DebitMoyen2 <- DebitMoyen%>%
  mutate(IndexHann=HanningFilter(ModuleIndex,type= "vector",
                                   value.var ="ModuleIndex",Date.col = "YEARS"),
  IndexType=ifelse(IndexHann>0,"Hummide","Seche"))%>%
  dplyr::ungroup()


ggplot(ObsSimData,aes(x=YEARS,y=value))+
  geom_line(aes(color=variable),size=1.2)+
  geom_point(aes(color=variable))+
  ##geom_bar(stat = "identity",color="black",aes(fill=IndexType))+
  geom_vline(xintercept = 2010)+
  geom_smooth(method = "lm",color="black",aes(linetype=variable),show.legend = FALSE)+
  facet_grid(Model~RCP,scale="free_y")+
  #scale_linetype_manual(name="",values = c(1,2),labels=c("Tendance Historique","Tendance Future"))+
  scale_x_continuous(breaks = seq(1981,2040,10))+
  scale_color_manual("",values = c("blue","red"),labels=c("Historique","Projection"))+
  xlab("Années")+
  ylab(expression(paste("Débit [",{m^3},"/s]")))+
  #ylab("Indice Standardisé")+
  theme_person



## Q vs PCP Plot
RainObs <- dbReadTable(conn = DataBase,name = "ObsMeanData")%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%HistoricPeriode)%>%
  dplyr::select(DATE,RAIN)%>%
  mutate(YEARS=year(DATE))%>%
  group_by(YEARS)%>%
  summarize(PCP=sum(RAIN,na.rm = TRUE)%>%round(2))




RainClimateModel <- dbGetQuery(conn = DataBase,statement = "SELECT*FROM MeanClimateData WHERE Model_id = 1")%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%EtudePeriode)%>%
  mutate(RCP=fct_collapse(as.factor(RCP),RCP4.5=c("rcp45"),RCP8.5=c("rcp85")))%>%
  dplyr::select(DATE,RCP,PCP)%>%
  mutate(YEARS=year(DATE))%>%
  group_by(RCP,YEARS)%>%
  summarize(PCP=sum(PCP,na.rm = TRUE)%>%round(2))

RainClimateModel2 <-DebitMoyen_1%>%
  select(Model,RCP)%>%
  left_join(RainClimateModel,by="RCP")%>%
  distinct(Model,RCP,YEARS,.keep_all = TRUE)%>%
  rename(PCP_Sim=PCP)%>%
  melt(id.vars=c("Model","RCP","YEARS"))%>%
  ungroup()

RainObs2 <- DebitMoyen_1%>%
  select(Model,RCP)%>%
  group_by(Model,RCP)%>%
  mutate(YEARS=RainObs$YEARS,
         PCP_Obs=RainObs$PCP)%>%
  melt(id.vars=c("Model","RCP","YEARS"))%>%
  ungroup()

Obs_Sim_Rain_Data <- dbReadTable(conn =DataBase,name ="QP_ObsSimData")
DebitMoyen2 <- ObsSimData%>%
  mutate(value=value*8.5,var="DEBIT")
#dbWriteTable(conn = DataBase,name = "QP_Obs_Sim_DATA",value = Q_Rain_DATA)
#Q_Rain_DATA <- rbind(Obs_Sim_Rain_Data,DebitMoyen2)
Q_Rain_DATA <- dbReadTable(conn = DataBase,name = "QP_Obs_Sim_DATA")
annot_text <- data.frame(x=rep(c(1991,2021),4),y=rep(c(1200),4),
                         lab=rep(c("Historique",
                                   "Projection"),4),
                         label=factor(x = c("GR4J","SWAT"),levels = c("GR4J","SWAT")))

ggplot(Q_Rain_DATA,aes(x=YEARS,y=value))+
  geom_line(aes(color=var),size=1.2)+
  geom_point(aes(color=var))+
  geom_vline(xintercept = 2010)+
  geom_smooth(method = "lm",aes(linetype=var),color="black",show.legend = FALSE)+
  facet_grid(Model~RCP,scale="free_y")+
  scale_x_continuous(breaks = seq(1981,2040,5))+
  scale_y_continuous(sec.axis = sec_axis(~./8.5,guide = guide_axis(title = expression(paste("Débit [",{m^3},"/s]")),
                                                                   angle = 90)))+
  scale_color_manual("",values = c("#984EA3","blue","#377EB8","#E41A1C","#4DAF4A"),labels=c("Débit","Pluie"))+
  xlab("Années")+
  ylab("Pluie [MM]")+
  geom_text(data =annot_text,mapping = aes(x=x,y =y,label=lab ))+
  #annotate(geom="text",x=rep(c(1991,2021),4),y=rep(c(1200,1200),4),label= c("Historique","Projection"),size=3.5)+
  #annotate(geom = "rect", xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf,
           #alpha = .3, fill = "blue")+
  #annotate(geom = "rect", xmin = 2011, xmax = 2040, ymin = -Inf, ymax = Inf,
           #alpha = .3, fill = "gray20")+
  theme_person

## ANALYSE HYDROGRAMMES
HydrogramObsData <- QObserve%>%
  mutate(Month=month(DATE))%>%
  filter(DATE>=as.Date("1987-01-01"),
         DATE<=as.Date("2010-12-31"))%>%
  group_by(Month)%>%
  summarize(FLOW_OUT=round(mean(Debit,na.rm = TRUE),2))%>%
  ungroup()

#SCENARIOSHYDRO <- dbReadTable(DataBase,"SCENARIOSHYDRO4")%>%
 #as_tibble()%>%
  #mutate(DATE=as.Date(DATE)) %>% 
#mutate(RCP=fct_collapse(as.factor(RCP),RCP4.5=c("rcp45","RCP45"),RCP8.5=c("rcp85","RCP85")))


HydrogramSimData <- SCENARIOSHYDRO%>%
  filter(DATE>=as.Date("1987-01-01"), DATE<=as.Date("2010-12-31"))%>%
  mutate(Month=month(DATE))%>%
  group_by(Model,RCP,Month)%>%
  summarize(FLOW_OUT=round(mean(FLOW_OUT),2))%>%
  mutate(Obs=HydrogramObsData$FLOW_OUT)%>%
  melt(id.vars=c("Model","RCP","Month"))

HydrogramSimData1 <- HydrogramSimData%>%
  filter(Month>=5)%>%
  mutate(Month=Month-4)
HydrogramSimData2 <- HydrogramSimData%>%
  filter(Month<=4)%>%
  mutate(Month=Month+8)
HydrogramSimData3 <- rbind(HydrogramSimData1,
                           HydrogramSimData2)
ggplot(data=HydrogramSimData3,aes(x=Month,y=value))+
  geom_line(size=1.15,show.legend = TRUE,aes(color=variable))+
  #geom_point(show.legend = FALSE,color="blue")+
  #geom_smooth(method = "lm",color="black")+
  scale_x_continuous(breaks = c(1:12),labels =c(month.abb[5:12],month.abb[1:4]))+
  facet_grid(Model~RCP)+
  scale_color_manual("",values = c("red","black"),labels=c("CCCma","Observation"))+
  xlab("Mois")+
  ylab(expression(paste("Débit [",{m^3},"/s]")))+
  theme_person
  
## ANALYSE DES COEFFICIENT MENSUELS DE DEBITS
QNowkuyObs <- dbReadTable(conn = DataBase,
                                name ="QObsData" )
CoefObsDebit <- QNowkuyObs%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE),Month=month(DATE))%>%
  filter(DATE>=as.Date("1981-01-01"),
         DATE<=as.Date("2010-12-31"))%>%
  group_by(YEARS,Month)%>%
  summarize(Debit_Mensuel=round(mean(Debit,na.rm = TRUE),2))%>%
  mutate(Module=mean(Debit_Mensuel,na.rm = TRUE),
         ObsCoef=round(Debit_Mensuel/Module,2))%>%
  group_by(Month)%>%
  summarize(Obs_Coef=round(mean(ObsCoef,na.rm = TRUE),2))
  
  

CoefMensuelDebit <- SCENARIOSHYDRO%>%
  mutate(YEARS=year(DATE),Month=month(DATE))%>%
  group_by(Model,RCP,YEARS,Month)%>%
  summarize(Debit_Mensuel=round(mean(FLOW_OUT),2))%>%
  mutate(CoefDebit=round(Debit_Mensuel/mean(Debit_Mensuel),2))%>%
  group_by(Model,RCP,Month)%>%
  summarize(Sim_Coef=round(mean(CoefDebit),2))
  
CoefData <- CoefObsDebit%>%
  inner_join(CoefMensuelDebit,by="Month")%>%
  melt(id.vars=c("Model","RCP","Month"))
 
names(CoefData)

ggplot(data=CoefData,aes(x=Month,y=value,fill=variable))+
  geom_bar(stat = "identity",position = "dodge",width = 0.8)+
  geom_hline(yintercept = 1,color="black")+
  scale_x_continuous(breaks = c(1:12),labels =month.abb)+
  facet_grid(Model~RCP)+
  scale_fill_manual("",values = c("black","red"),labels=c("Observation","CCCma"))+
  xlab("Mois")+
  ylab("CMD")+
  theme_person
  
# ANALYSE DES TENDANCES ANNUELLES
SCENARIOSHYDRO <- dbReadTable(DataBase,"SCENARIOSHYDRO4")%>%
mutate(DATE=as.Date(DATE))%>% 
mutate(RCP=fct_collapse(as.factor(RCP),RCP4.5=c("rcp45","RCP45"),RCP8.5=c("rcp85","RCP85")))


  TestData <- SCENARIOSHYDRO%>%
    dplyr::filter(DATE<=as.Date("2040-12-31")& DATE>=as.Date("2011-01-01"))%>%
    acast(DATE+ClimateModel+RCP~Model,value.var ="FLOW_OUT" )%>%
    as.data.frame()%>%
    mutate(id.vars=rownames(.),
           DATE=as.character(substr(x = id.vars,start = 1,stop = 10)),
           ClimateModel=as.character(substr(x = id.vars,start = 12,stop = 22)),
           RCP=str_remove_all(string = substr(x = id.vars,start = 24,stop = nchar(id.vars)),pattern = "_"))%>%
    select(DATE,ClimateModel,RCP,everything(),-id.vars)
    
 
  STATVALUES <- rbind()
  var.names <- c("GR4J","SWAT")
  IDModel <- c(2,10)
  RCP <- c("RCP4.5","RCP8.5")
  j <- 0
  repeat{
    j=j+1
    if(j>2)break
    for (k in 1:2) {
      TestData2 <- TestData%>%
        mutate(DATE=as.Date(DATE))%>%
        filter(RCP==RCP[k])
      
      for (var in var.names) {
        if(var.names=="PCP"||var.names=="EPT"){
          TestData3 <- TestData2%>%
            dplyr::select(DATE,print(var))%>%
            daily2annual.data.frame(FUN = sum,dates =1)%>%
            as.vector()#
        }else{
          TestData3 <- TestData2%>%
            dplyr::select(DATE,print(var))%>%
            daily2annual.data.frame(FUN = max,dates =1)%>%
            as.vector()# 
        }
        
        StatTest <- pettitt.test(TestData3)#
        StatTestData <- data.frame(STATValue=StatTest$data)%>%
          mutate(RCP=RCP[k],variable=print(var))
        STATVALUES <- rbind(STATVALUES,StatTestData)
      }
      
    }
    
  }
  
  VizSTATest <- TestData%>%
    mutate(YEARS=year(DATE))%>%
    group_by(RCP,YEARS)%>%
    summarize(across(.cols = c(GR4J,SWAT),~ round(max(.x),2)))%>%
    summarize(P_GR4J=round(mk.test(GR4J)$statistic,2),
           P_SWAT=round(mk.test(SWAT)$statistic,2))#%>%
    select(RCP,YEARS,starts_with("P"))%>%
    rename(GR4J=P_GR4J,SWAT=P_SWAT)%>%
    ungroup()%>%
    melt(id.vars=c("RCP","YEARS"))
    
    VizSTATest <- TestData%>%
      mutate(YEARS=year(DATE))%>%
      group_by(RCP,YEARS)%>%
      filter(RCP=="RCP4.5",!YEARS<=2029)
  ## Graph
  
  ggplot(VizSTATest,aes(x=YEARS,y=value))+
    geom_line(size=1.15,show.legend = FALSE,color="blue")+
    geom_point(show.legend = FALSE,color="black")+
    scale_x_continuous(breaks = seq(2011,2040,4),labels = seq(2011,2040,4))+
    facet_grid(variable~RCP,space = "free_y")+
    xlab("Années")+
    ylab("Statistique")+
    theme_person
## ANALYSE STATISTIQUES
TestData <- SCENARIOSHYDRO%>%
  dplyr::filter(DATE<=as.Date("2040-12-31")& DATE>=as.Date("2011-01-01"))
 
##Initialisation 
STATVALUES <- rbind()
HydroModel <- c("GR4J","SWAT")
j <- 0
## Boucle  
repeat{
    j=j+1
    if(j>2)break
    for (k in c("RCP4.5","RCP8.5")) {
      TestData2 <- SCENARIOSHYDRO%>%
        mutate(DATE=as.Date(DATE))%>%
        filter(Model==HydroModel[j],
               RCP==k)
      
      TestData3 <- TestData2%>%
        dplyr::select(DATE,FLOW_OUT)%>%
        as.data.frame()%>%
        daily2annual.data.frame(FUN = mean,dates=1)%>%
        as.vector()#
        
        StatTest <- MannKendall(TestData3)#pettitt.test(TestData3)#
        StatTestData <- data.frame(STATValue=round(StatTest$sl,2))%>%
          mutate(Model=HydroModel[j],RCP=k)
        STATVALUES <- rbind(STATVALUES,StatTestData)
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

  ggplot(STATVALUES,aes(x=rep(1:30,4),y=STATValue))+
    geom_line(size=1.15,show.legend = FALSE,color="blue")+
    geom_point(show.legend = FALSE,color="black")+
    #geom_smooth(method = "lm",color="black")+
    scale_x_continuous(breaks = seq(1,30,4),labels = seq(2011,2040,4))+
    facet_grid(Model~RCP,space = "free_y")+
    #scale_color_manual("",values = c("black","red"),labels=c("Observation","Modèle"))+
    xlab("Années")+
    ylab("Statistique")+
    theme_person
    
#####
  ObsModule <- QObserve%>%
    mutate(YEARS=year(DATE))%>%
    group_by(YEARS)%>%
    summarize(Qannuel=round(mean(Debit,na.rm = TRUE),2))%>%
    mutate(Qannuel=ifelse(is.nan(Qannuel),
                          round(mean(Qannuel,na.rm = TRUE),2),
                          Qannuel))%>%
    ungroup()
  
  QObsMax <- QObserve%>%
    mutate(YEARS=year(DATE))%>%
    group_by(YEARS)%>%
    summarize(Qmax=round(max(Debit,na.rm = TRUE),2))%>%
    mutate(Qmax=ifelse(is.infinite(Qmax),
                       NA,
                       Qmax))%>%
    mutate(Qmax=ifelse(is.na(Qmax),
                       round(mean(Qmax,na.rm = TRUE),2),
                       Qmax))%>%
    ungroup()
  
  QObsMin <- QObserve%>%
    mutate(YEARS=year(DATE))%>%
    group_by(YEARS)%>%
    summarize(Qmin=round(min(Debit,na.rm = TRUE),2))%>%
    mutate(Qmin=ifelse(is.infinite(Qmin),
                       NA,
                       Qmin))%>%
    mutate(Qmin=ifelse(is.na(Qmin),
                       round(mean(Qmin,na.rm = TRUE),2),
                       Qmin))%>%
    ungroup()
  
QObsData <- ObsModule%>%
   left_join(QObsMax,by = "YEARS")%>%
  left_join(QObsMin,by="YEARS")
  
FormateData <- SCENARIOSHYDRO%>%
  acast(formula=RCP+DATE~Model,value.var ="FLOW_OUT" )%>%
  as.data.frame()%>%
  mutate(RCP=substr(x = rownames(.),start = 1,stop = 6),
         DATE=as.Date(substr(x = rownames(.),start = 8,stop = 17)),
         YEARS=year(DATE))%>%
  select(DATE,RCP,everything())%>%
  group_by(RCP,YEARS)%>%
  summarize(across(GR4J:SWAT,mean))%>%
  left_join(QObsData,by="YEARS")
  
QSimModule <-SCENARIOSHYDRO%>%
  mutate(YEARS=year(DATE))%>%
  group_by(Model,ClimateModel,RCP,YEARS)%>%
  summarize(Module=round(mean(FLOW_OUT,na.rm = TRUE),2))%>%
  dplyr::ungroup()

QSimMax <-SCENARIOSHYDRO%>%
  mutate(YEARS=year(DATE))%>%
  group_by(Model,ClimateModel,RCP,YEARS)%>%
  summarize(QsimMax=round(max(FLOW_OUT,na.rm = TRUE),2))%>%
  dplyr::ungroup()

QSimMin <-SCENARIOSHYDRO%>%
  mutate(YEARS=year(DATE))%>%
  group_by(Model,ClimateModel,RCP,YEARS)%>%
  summarize(QsimMin=round(min(FLOW_OUT,na.rm = TRUE),2))%>%
  dplyr::ungroup()

QSimData <- QSimModule%>%
  left_join(QSimMax,by=c("Model","ClimateModel","RCP","YEARS"))%>%
  left_join(QSimMin,by=c("Model","ClimateModel","RCP","YEARS"))%>%
  group_by(Model,ClimateModel,RCP)%>%
  mutate(ObsModule=QObsData$Qannuel,
         ObsQmax=QObsData$Qmax,
         ObsQmin=QObsData$Qmin)%>%
  ungroup()

VizData <- QSimData%>%
  dplyr::select(Model,RCP,YEARS,QsimMin)%>%
  rename(QMin=QsimMin)%>%
  melt(id.vars=c("Model","RCP","YEARS"))#%>%
  #mutate(variable=factor(x=variable,levels = c("Qobs","Qsim")))

ggplot(VizData,aes(y=value,x=YEARS))+
  geom_line(size=1.3,color="blue")+
  geom_point(color="blue")+
  facet_grid(Model~RCP,scale="free_y")+
  geom_smooth(method = "lm",color="black")+
  #scale_color_manual("",values = c("blue","black"),labels=c("GR4J","SWAT"))+
  xlab("Années")+
  ylab(expression(paste("Débit [",{m^3},"/s]")))+
  #ylab("Indice Standardisé")+
  theme_person
  