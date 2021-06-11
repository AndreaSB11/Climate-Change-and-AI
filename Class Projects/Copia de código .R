#This script estimates a linear model using the model data table
dir.data<-"/Users/andreasb/Documents/GitHub/Climate-Change-and-AI/Data/Model/"
data<-read.csv(paste0(dir.data,"ModelData.csv"))

##let's look at the data first,
dim(data)
#194 renglones -> 194 países 
#77 variables 
summary(data)
#Hay variables que no tienen datos (NA), por lo que es necesario eliminarlas para eviar errores en la medición. 

#remove columns with only NA
#Quedarme con las variables que tenga más del 50% de valores no nulos
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.50])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS") 
bad.vars #Variables que eliminamos  

#let's select a response of interest
#MODELO PREDICTOR

#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
ids<-c("iso_code3","country")
response<-"EN.ATM.CO2E.PC" #Variable de respuesta
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars))) #Predictores 
length(predictors)
#Total de predictores con los que vamos a trabajar = 58

#Fórmula del modelo general
data.model<-data[,c(response,predictors)] #Aquí se puede cambiar para el modelo
data.model<-data.model[complete.cases(data.model),]

model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
model

#Modelo con todos los predictores  
full.model <- lm(model, data = data.model)
summary(full.model)
#No hay predictores significativos.
#Debemos encontrar la combinación de predictores que nos permitan explicar de mejor manera nuestra variable de respuesta. 
#Esta forma vamos a poder determinar cuáles son los predictores. 

#now let's select various smaller models
#=============================
#MÉTODO DE SELECCIÓN DE MODELO
#=============================
library(leaps)
#Hacemos validación cruzada con cada posible modelo. 
#Dividimos en dos nuestra muestra: entrenamiento y prueba. 
#Entre más complejo el modelo, mayor flexibilidad y se sobreajusta.


#first let's divide the sample into a test and a a train set
set.seed (55555)
train <- sample (c(TRUE ,FALSE), nrow(data.model ),rep=TRUE)
test  <- (!train )

#define maximum length of the model
#Intentamos correrlo con un máximo mayor pero duró mucho tiempo y no cargó. 
#Un modelo de no más de 7 variables  
max.vars<-7

#Con qué tipo de método vamos a elegir el modelo  
#let's do full search -> COmbinaciones y elegimos el que mejor se comporta
regfit.best <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T) #you can choose how large you want the search to be

#let's do forward stepwise selection -> Modelo nulo y se van ubicando las var que mejor se le acomoda y así se va construyendo el modelo. 
regfit.fwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "forward") #you can choose how large you want the search to be

#let's do backard stepwise selection -> Modelo con todos los predictores y se van quitando. 
regfit.bwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "backward") #you can choose how large you want the search to be


#explore different models


#now how do we select which model is best
#De los modelos posibles, cuál escojo -> validación cruzada

#Estimar el error de prueba   
predict.regsubsets <-function (object, model ,newdata ,id ){
  #  object<-regfit.best
  #  newdata<-data.model[test ,] Conjunto de prueba
  #  id<-4
  form<-model
  options(na.action='na.pass')
  mat<-model.matrix (form,newdata )
  coefi<-coef(object ,id=id)
  xvars<-names (coefi )
  pred<-mat[,xvars ]%*% coefi
  val.errors<- mean((newdata[,response]-pred)^2,na.rm=TRUE)
  val.errors
}

#now estimate test error for the different versions of the models
#best subset
cv.best<-data.frame(subset.type="best",
                    nvars=1,
                    test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="best",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],i))
  cv.best<-rbind(cv.best,pivot)
  
}
cv.best
#best model
subset(cv.best,test.mse==min(test.mse))
#actual model
coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

#El ideal con este tipo de selección son dos predictores. 
msize<-3
coef(regfit.best ,msize)



#forward method
cv.fwd<-data.frame(subset.type="fwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.fwd,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="fwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.fwd,model,data.model[test ,],i))
  cv.fwd<-rbind(cv.fwd,pivot)
  
}
cv.fwd
#best model
subset(cv.fwd,test.mse==min(test.mse))
#actual model
coef(regfit.fwd ,subset(cv.fwd,test.mse==min(test.mse))$nvars)

#Opcion del modelo con dos predictores 
msize<- 3
coef(regfit.fwd , msize)

#backward method
cv.bwd<-data.frame(subset.type="bwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.bwd,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="bwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.bwd,model,data.model[test ,],i))
  cv.bwd<-rbind(cv.bwd,pivot)
  
}
cv.bwd
#best model
subset(cv.bwd,test.mse==min(test.mse))
#actual model
coef(regfit.bwd ,subset(cv.bwd,test.mse==min(test.mse))$nvars)
#Opción del modelo con tres predictores 
msize<-3
coef(regfit.bwd , msize)

#Ya he escogido las variables, dos sociodemográficas y tres NDC. 
#=====================
#Estimación del modelo
#=====================
#Primer modelo
model1<-lm(EN.ATM.CO2E.PC~m_buildings+m_transport+m_energy+ SP.POP.GROW +NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model1)
#Observaciones: 
#Las indicadores sociodemográficos son significativos pero los indicadores de NDC no lo son, por esto es necesario observar 
#el impacto de los indicadores NDC sin tomar en cuenta los sociodemográficos 

#Nos permite ver el impacto de las variables NDC
model2<-lm(EN.ATM.CO2E.PC~m_buildings+m_energy+m_transport, data=data, na.action=na.omit)
summary(model2)
#Error 
cv.bwd
#test.mse= 10.201775

#La variable m_tranport no es significativa, por lo cual se elimina se observa nuevamente la regresión 
model3<-lm(EN.ATM.CO2E.PC~m_buildings+m_energy, data=data,na.action=na.omit)
summary(model3)
#Error 
cv.bwd
#test.mse =  8.703232

#Ambas variables son significativas 

#Modelo de las WORLD BANK
model4<-lm(EN.ATM.CO2E.PC~SP.POP.GROW +NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model4)
#Error 
cv.fwd
#test.mse= 5.335562

#Hacemos un nuevo modelo de cuatro variables 
model5<-lm(EN.ATM.CO2E.PC~m_buildings+m_energy+SP.POP.GROW+NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model5)
#Cuando se ingresa las cuestiones sociodemográficas al modelo estas se vuelven significativas, mientras que las de la NDC no. 
model6<-lm(EN.ATM.CO2E.PC~m_energy+NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model6)




#Validación cruzada de los modelos que combinan ambas variables -> NO SE PUDO HACER :( 
#Se justifica con el hecho de que al combinar varaibles, las únicas que se vuelven significativas son las sociodemográficas y no las de NDC 
#Se calularon los dos modelos por separado y sobre esos se estimaron los errores. 


