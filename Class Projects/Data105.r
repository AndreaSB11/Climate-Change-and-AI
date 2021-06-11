#This script estimates a linear model using the model data table
dir.data<-"/Users/andreasb/Documents/GitHub/Climate-Change-and-AI/Data/Model/"
data<-read.csv(paste0(dir.data,"ModelData.csv"))
dim(data) #194 predictores y 194 países 

#let's look at the data first,
  summary(data)

#remove columns with only NA
 bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.50])

#let's select a response of interest

#EN.ATM.CO2E.PC=CO2 (y) emissions (metric tons per capita)
#Quiero saber cómo las demás variables ayudan a predecir mi y.
#Policy Question -> ¿Qué tanto influye la NDC en qué un país sea de alta o baja emisiones? 
# O ¿qué tanto influye las sociedomográficas? O ¿qué tanto influye X(la que me interesa) variable? 
#También podemos enfocarnos sólo en un país, o región (Latinoamércia)
  
 ids<-c("iso_code3","country")
  response<-"EN.ATM.CO2E.PC"
  predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
# Tengo 118 predictores -> Voy a tratar de predecir y, usando estos 118 predictores.
  

#before we model, how correlated is our data.  
#Estas líneas limpia las bases de datos. 
#En los predictoes queremos mucha varianza y poca correlación (para que un mismo preditor no represente lo mismo que otro predictor). 
 cor.table<-data.frame(cor(data[,predictors], use = "complete.obs"))
 bad.cors<-sapply(cor.table, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.cors<-names(bad.cors[bad.cors>0.98])
 cor.table<-cor.table[,subset(colnames(cor.table),!(colnames(cor.table)%in%bad.cors))]
 cor.table<-cor.table[subset(rownames(cor.table),!(rownames(cor.table)%in%bad.cors)),]
 cor.table<-data.frame(apply(cor.table,c(1,2),function(x){ifelse(abs(x)>0.5,1,0)}))
 bad.cors2<-sapply(cor.table,mean)
 bad.cors2<-names(bad.cors2[bad.cors2>0.40])
 predictors<-subset(predictors,!(predictors%in%bad.cors2))
length(predictors)
#Al limpiar mi base de datos me quedan 91 predictores. 
#¿Cuál de esos 91 predictores importan más?
#Identificar cuál es el más relevante, cómo lo identifico y justifico que lo he identificado con rigor. 


#estimate model
 data.model<-data[,c(response,predictors)]
 model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
 full.model <- lm(model, data = data.model)
#Algo en la base de datos está generando muchos NA. 

#Try other models
#Estimar el modelo con los primeros 10, etc.  predictores únicamente
 model<-as.formula(paste0(response,"~",paste(predictors[1:20],collapse="+")))
 full.model <- lm(model, data = data.model)
 summary(full.model)
#Todavía no encontramos predictores importantes 
#Emplear otro modelo, o otra técnica de modelado.  
#Aquí falta una línea de código.
#Cuál es el mejor modelo 
 
#step wise selection
 library(MASS)
 step.model <- stepAIC(full.model, direction = "both",
                      trace = FALSE)
 summary(step.model)
