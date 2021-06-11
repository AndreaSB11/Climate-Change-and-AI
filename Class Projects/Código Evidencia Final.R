#Evidencia 1. Modelo predictivo 
#Ana Sofia Ruiz Buelna - A00227000
#Andrea Salinas Blancas - A00827263 

#Este script estima un modelo de regresión lineal múltiple tomando como base de datos la siguiente: 
dir.data<-"/Users/andreasb/Documents/GitHub/Climate-Change-and-AI/Data/Model/"
data<-read.csv(paste0(dir.data,"ModelData.csv"))

#Descripción de nuestra base de datos 
dim(data)
#194 países 
#77 indicadores   
summary(data)

#Hay indicadores que no tienen datos (NA), por lo que es necesario eliminarlas para evitar errores en la medición, 
#me quedo con las variables que tenga más del 50% de valores no nulos
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.50])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS") 
bad.vars 

#====================
#MODELO DE PREDICCIÓN
#====================
#Variable de respuesta: EN.ATM.CO2E.PC=CO2 emisioens (toneladas métricas per cápita)
ids<-c("iso_code3","country")
response<-"EN.ATM.CO2E.PC"
#Predictores después de limpieza de datos 
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars))) #Predictores 
length(predictors) #58 predictores 

#Estimación de un modelo con todos los predictores posibles 
#1. Formúla del modelo general 
data.model<-data[,c(response,predictors)] 
data.model<-data.model[complete.cases(data.model),]

model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
model
#Modelo con todos los predictores 
full.model <- lm(model, data = data.model)
summary(full.model)
#Podemos observar que no hay predictores significativo, 
#por lo que ebemos encontrar la combinación de predictores 
#que nos permitan explicar de mejor manera nuestra variable de respuesta. 

#=============================
#MÉTODO DE SELECCIÓN DE MODELO
#============================= 
library(leaps)
#Hacemos validación cruzada con cada posible modelo, para poder saber 
#cuántas varibles y cuáles son las ideales para nuestro modelo. 

#Dividimos en dos nuestra muestra: entrenamiento y prueba. 
set.seed (55555)
train <- sample (c(TRUE ,FALSE), nrow(data.model ),rep=TRUE)
test  <- (!train )

#Usamos los tres métodos de selección (Best subset, Forward method y Backward method)
#Notas importantes: 
#1. El modelo que se selecionará con este método no va tener más de 7 variables, esto porque
#dado la limitante de tiempo, no pudimos correr el código con un límite mayor. 
max.vars<-7

#Método Best subset
regfit.best <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T)
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
#Mejor modelo 
subset(cv.best,test.mse==min(test.mse))
#Modelo actual 
coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

#En base a los test.mse con este tipo de selección decidimos explorar el modelo con cinco predictores. 
#Predictores: 
msize<-5
coef(regfit.best ,msize)

#Método Forward  
regfit.fwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "forward") 
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
#Mejor modelo
subset(cv.fwd,test.mse==min(test.mse))
#Modelo actual
coef(regfit.fwd ,subset(cv.fwd,test.mse==min(test.mse))$nvars)

#En base a los test.mse, con este tipo de selección decidimos explorar el modelo con tres predictores. 
msize<- 2
coef(regfit.fwd , msize)

#Backward method
regfit.bwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "backward") 
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
#Mejor modelo 
subset(cv.bwd,test.mse==min(test.mse))
#Modelo actual
coef(regfit.bwd ,subset(cv.bwd,test.mse==min(test.mse))$nvars)
#Con esta opción decidimos explorar el  modelo con tres predictores 
msize<-3
coef(regfit.bwd , msize)

#Al observar las variables que se obtenían por cada método de selección, los errores estimados y la teoría investigada sobre el cambio climático 
#para cada combinación de variables se decidió estimar un modelo con las siguientes cinco variables: 
#NDC INDICADORES: 
#m_buildings -> Acciones de mitigación para el sector de edificios)
# m_energy -> Acciones de mitigación para el sector energético)
# m_transport -> Acciones de mitigación para el sector de transporte)
#INDICADORES SOCIODEMOGRÁFICOS: 
#NY.GNP.PCAP.CD -> PIB per cápita (Método Atlas)
#SP.POP.GROW -> Tasa de crecimiento poblacional (% anual)

#=======================
#ESTIMACIÓN DEL MODELO 
#=======================
#Modelo 1 
#Descripción: Regresión usando los cinco predictores antes descritos. 
model1<-lm(EN.ATM.CO2E.PC~m_buildings+m_transport+m_energy+ SP.POP.GROW +NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model1)
#Observaciones: 
#Las indicadores sociodemográficos son significativos(especialmente NY.GNP.PCAP.PP.CD) pero los indicadores de NDC no lo son, 
#por esto es necesario observar el impacto de los indicadores NDC sin tomar en cuenta los sociodemográficos y viceversa. 

#Modelo NDC´S 
model2<-lm(EN.ATM.CO2E.PC~m_buildings+m_energy+m_transport, data=data, na.action=na.omit)
summary(model2)
#Observaciones:
##La variable m_tranport no es significativa, aún cuando se excluyen los valores sociodemográficos
#por lo cual se estima nuevamente la regresión excluyendo dicha variable: 
model3<-lm(EN.ATM.CO2E.PC~m_buildings+m_energy, data=data,na.action=na.omit)
summary(model3)
#Observaciones, podemos notar que ambas variables son significativas, en especial m_energy, además que esta tiene signo negativo 
#lo cual indica que a mayores mitigaciones en cuanto a lo referente al sector de energía, menos emisiones de CO2 per cápita generan los países. 
#Validación cruzada de model3 
cv.bwd
#Tomando como referencia la validación cruzada realizada para cada método y el conjunto de entretamineto y de prueba antes especificado 
#el test.mse estimado para model3 corresponde al error caluculado en cv.bward para un subset de 2 variables, es decir: 
#test.mse =  8.703232

#Modelo sociodemográfico 
model4<-lm(EN.ATM.CO2E.PC~SP.POP.GROW +NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model4)
#Observaciones: 
#Ambas varaibles son significativas, sin embargo, la que tiene mayor peso es NY.GNP.PCAP.PP.CD, además su coeficiente estimado indica que 
# a mayor PIB per cápita mayores emisiones de CO2 genera el país. 
#Validación cruzada 
cv.fwd
##Tomando como referencia la validación cruzada realizada para cada método y el conjunto de entretamineto y de prueba antes especificado 
#el test.mse estimado para model3 corresponde al error caluculado en cv.fwd para un subset de 2 variables, es decir: 
#test.mse =  5.335562

#Modelo 2
#Después de regresar de manera separada los predictores, y determinar las variables significativas, volvemos a estimar nuestro modelo
#ahora con cuatro predictores solamente:
model5<-lm(EN.ATM.CO2E.PC~m_buildings+m_energy+SP.POP.GROW+NY.GNP.PCAP.PP.CD,data=data,na.action=na.omit)
summary(model5)
#Nuevamente, las únicas variables que resultan signficativas son las correspondientes a características sociodemográficas de los países. 
#En este modelo, al igual que en model1, podemos observar que el predictor con mayor peso es NY.GNP.PCAP.PP.CD. 

#Validación cruzada de model1 y model5
#Dado que al estimar los modelos juntando varaibles sociodemográficas y NDC´S no todas son significativas, sino únicamnete las sociodemográficas 
#estos modelos no se validaron, ya que esto modelos sólo se toman como referencia. 
#Se opto por mejor realizar la validación de los modelos por separado (sociodemográficos y NDC´s) ya que estos brindaban mejores predicciones, 
#esto tomando como referencia nuestra policy question.














