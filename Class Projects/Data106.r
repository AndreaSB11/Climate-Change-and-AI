#This script estimates a linear model using the model data table
dir.data<-"/Users/andreasb/Documents/GitHub/Climate-Change-and-AI/Data/Model/"
data<-read.csv(paste0(dir.data,"ModelData.csv"))
dim(data)

##let's look at the data first,
  summary(data)

#remove columns with only NA
 bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.50])
 bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")  
 data<-subset(data,is.na(response.binary)==FALSE)
 dim(data)
#let's select a response of interest

#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
ids<-c("iso_code3","country") #Remover datos  
response<-"EN.ATM.CO2E.PC" #Variable de respuesta 
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars))) #Predictores 
length(predictors)

#estimate full model
 data.model<-data[,c(response,predictors)]
 head(data.model)
 model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
 model
 full.model <- lm(model, data = data.model) #Qué pasa si meto todos los predictores 
 summary(full.model)
 
#Validar nuestro modelo 
#¿Cuál es la combinación más idenoea? 
#Modelos posibles: Millones, cuál es el mejor 
#Combinación específica es las ideonea 
#Características más importantes para el NDC 
 
#now let's select various smaller models
#install.packages("leaps")
#Validación cruzada -> Dividir en dos la base (prueba y entrenamiento)
#Entreno mi modelo con una parte de los datos  

 
#load library
 library(leaps)


#first let's divide the sample into a test and a a train set
#Dividir la base de otra forma
 #Conjunto de entrenamiento 
  set.seed (55555) #Establecer la semilla para que sea replicable 
  train <- sample (c(TRUE ,FALSE), nrow(data.model ),rep=TRUE)
#Conjunto de prueba   
  test  <- (!train )

#define maximum length of the model
#Los modelos no debe tener más de ocho variables 
  max.vars<-7
#¿Qué tan grande será la búsqueda? 
#La búsqeuda es de 7 porque tenemos la limitante de tiempo, lo intentamos hacer con 21, pero dicha búsqueda demandaba una gran cantidad de tiempo. 

#let's do full search
#regsubsets- especificar modelo completo (variable de interés)
#Elegir el que mejor se comporte 
 regfit.best <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T) #you can choose how large you want the search to be

#let's do forward stepwise selection
#Modelo nulo y agregar un precitor y vas agregando 
 regfit.fwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "forward") #you can choose how large you want the search to be

#let's do backard stepwise selection
#Empezar con el modelo completo y depurar hacia atrás 
 regfit.bwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "backward") #you can choose how large you want the search to be


#explore different models
#Jugar un poco con el número de predictores que queremos incluir en nuestro modelo. 
 msize<-6
  coef(regfit.best ,msize) 
  coef(regfit.fwd , msize)
  coef(regfit.bwd , msize)
  
#Investigue muchos modelos, una explicación muy puntual. 
#Un sólo predictor, hay mucho error asociado, modelo no muy bueno. 
#Cuál es nuestro mejor modelo -> ¿Qué modelo elegir? Investigación (teoría): 
#Modelo linear -> por qué justifico esa selección de modelo. 
#Mayor evidencia
#Más sentido   

#now how do we select which model is best
#Tengo 24 modelos posibles 
#Hacer validación cruzada 
  
predict.regsubsets <-function (object, model ,newdata ,id ){
  #object<-regfit.best
  #newdata<-data.model[test ,]
  #id<-4 #Quiero el modelo con cuatro predictores 
  form<-model
  options(na.action='na.pass')
  mat<-model.matrix (form,newdata ) #Matriz del modelo (ordena datos)
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
#El menor error, puedes explicar con un solo predictor. Explorar opciones, en función de investigación elegir uno en particular. 
#Error de prueba -> Entre más complejo haga el modelo, entre más complejo haga el modelo, el modelo encuentra patrones donde no existen.  

#best model
 subset(cv.best,test.mse==min(test.mse))
#actual model
 coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

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

#
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

#Probar nuestro propio modelo y hacer nuestra validación cruzada.

#Escoger este modelo en base a los que no sale en los diferentes métodos.  
model2<-lm(EN.ATM.CO2E.PC~m_industries+ a_transport+, data=data.model)
summary(model2)


#Predictores 
data.model<-data[,c(response,predictors)]
head(data.model)
model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
model