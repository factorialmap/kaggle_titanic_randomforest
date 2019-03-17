#Chamar os pacotes necessários
library(randomForest)
library(ggplot2)

#Importar os dados
train_set <- read.csv("train.csv", stringsAsFactors=T)
test_set  <- read.csv("test.csv",  stringsAsFactors=T)

#Check dados faltantes
colSums(is.na(train_set)) 
colSums(is.na(test_set))
colSums(train_set =="")
colSums(test_set =="")

#Criar coluna faltante no test_set
test_set$Survived <- NA

#Criando uma coluna para identificar se o dado é treino ou teste
train_set$IsTrainSet <-T
test_set$IsTrainSet  <-F

#Agrupar os datasets usando rbind
titanic_set <- rbind(train_set, test_set)

#Descritiva do conjunto
summary(titanic_set)

#Check dados faltantes
colSums(is.na(titanic_set)) 
colSums(is.na(titanic_set))
colSums(titanic_set =="")

#Transformações simples ETL/4Cs (Cleaning, completing, coreting, creating)
titanic_set$Survived                         <- as.factor(titanic_set$Survived)
titanic_set$Pclass                           <- as.factor(titanic_set$Pclass)
titanic_set$Age[is.na(titanic_set$Age)]      <- median(titanic_set$Age, na.rm = T)
titanic_set$SibSp                            <- as.numeric(titanic_set$SibSp)
titanic_set$Parch                            <- as.numeric(titanic_set$Parch)
titanic_set$Fare[is.na(titanic_set$Fare)]    <- median(titanic_set$Fare, na.rm = T)
titanic_set$Embarked[titanic_set$Embarked==""] <-"S"
titanic_set$Embarked                         <-as.factor(as.character(titanic_set$Embarked))
table(titanic_set$Embarked)

#Construir o modelo
titanic_train <- titanic_set[titanic_set$IsTrainSet==T,]
titanic_test  <- titanic_set[titanic_set$IsTrainSet==F,]

#Criando a formula
survived_formula <- as.formula("Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked")

#Criando o modelo
titanic_model <- randomForest(formula = survived_formula,
                              data = titanic_train,
                              ntree = 65,
                              importance = T)

#Interpretando resultados
titanic_model

#Mostrando graficos curva
plot(titanic_model)

#Gerando a matriz de importância das variáveis
importance_var   <- importance(titanic_model, type=1)

importance_var

#Dando uma formatada na tabela
tabela_de_importancia <- data.frame(variaveis=row.names(importance_var), 
                                    importancia=importance_var[,1]);tabela_de_importancia

#Gerando o grafico
grafico <- ggplot(tabela_de_importancia, 
                  aes(x=reorder(variaveis,importancia), y=importance_var)) +
                  geom_bar(stat="identity", fill="#5cc9c1") +
                  coord_flip() + 
                  theme_light(base_size=20) +
                  xlab("") +
                  ylab("Importância") + 
                  ggtitle("Importância das variáveis no Modelo RF") +
                  theme(plot.title=element_text(size=18))
grafico

#Preparando material para submissão

#Cria um data frame com o campo PassengerId
submission <- data.frame(PassengerId = test_set$PassengerId,
                         Survived = predict(titanic_model, newdata =  titanic_test))

#View nos dados de saida
View(submission)

write.csv(submission, file = "titanic_kaggle_r.csv", row.names=F)
