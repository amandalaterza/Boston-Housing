################################################
#Trabalho do final do ano - Substituindo outliers por m�dia


#Base de Boston Housing - Achar valor dos im�veis 

# Regress�o Linear M�ltipla

# Listar a biblioteca
setwd("C://Users//amand//OneDrive//�rea de Trabalho//Trabalho 2019")

#Importar a base de dados
base <- read.csv("train.csv",sep=",",dec=".")
head(base) #primeiras linhas da base de dados
names(base) #vari�veis da base


#An�lise Explorat�ria

#Vari�veis explicativas quantitativas

#crim (criminalidade) - tem outliers
summary(base$crim) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$crim) #Apresenta o histograma
boxplot(base$crim) #Apresenta o boxplot

#trocando outliers por m�dia

bench <- 3.67822 + 1.5*IQR(base$crim)
bench
base$crim[base$crim > bench ] <- mean(base$crim)
boxplot(base$crim)



#zn (zona residencial) - tem outlier
summary(base$zn) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$zn) #Apresenta o histograma
boxplot(base$zn) #Apresenta o boxplot

#trocando outliers por m�dia

bench2 <- 12.5 + 1.5*IQR(base$zn)
bench2
base$zn[base$zn > bench2 ] <- mean(base$zn)
boxplot(base$zn)



#indus(hectares de industria) - sem outliers
summary(base$indus) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$indus) #Apresenta o histograma
boxplot(base$indus) #Apresenta o boxplot



#nox (concentra��o de oxido de nitrogenio no ar) - sem outlier
summary(base$nox) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$nox) #Apresenta o histograma
boxplot(base$nox) #Apresenta o boxplot



#rm (quartos por habita��o)- tem outliers
summary(base$rm) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$rm) #Apresenta o histograma
boxplot(base$rm) #Apresenta o boxplot

#trocando outliers por m�dia

bench3 <- 6.595 + 1.5*IQR(base$rm)
bench3
base$rm[base$rm > bench3 ] <- mean(base$rm)
boxplot(base$rm)

bench4 <- 5.884 - 1.5*IQR(base$rm)
bench4
base$rm[base$rm < bench4 ] <- mean(base$rm)
boxplot(base$rm)


#age (casas velhas ocupadas) - sem outlier
summary(base$age) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$age) #Apresenta o histograma
boxplot(base$age) #Apresenta o boxplot


#dis (distancia media para5 polos de trabalho) - tem outlier
summary(base$dis) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$dis) #Apresenta o histograma
boxplot(base$dis) #Apresenta o boxplot

#trocando outliers por m�dia

bench5 <- 5.117  + 1.5*IQR(base$dis)
bench5
base$dis[base$dis > bench5 ] <- mean(base$dis)
boxplot(base$dis)



#tax (imposto de casa) - sem outlier
summary(base$tax) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$tax) #Apresenta o histograma
boxplot(base$tax) #Apresenta o boxplot


#ptratio (taxa aluno prof) - tem outlier
summary(base$ptratio) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$ptratio) #Apresenta o histograma
boxplot(base$ptratio) #Apresenta o boxplot

#trocando outliers por m�dia

bench6 <-  17.40 - (1.5*(20.20-17.4))
bench6
base$ptratio[base$ptratio < bench6 ] <- mean(base$ptratio)
boxplot(base$ptratio)



#black (imposto de casa) - tem outlier
summary(base$black) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$black) #Apresenta o histograma
boxplot(base$black) #Apresenta o boxplot

#trocando outliers por m�dia

bench7 <- 376.7 - 1.5*IQR(base$black)
bench7
base$black[base$black < bench7 ] <- mean(base$black)
boxplot(base$black)



#lstat (pessoas pobres por bairro) - tem outlier
summary(base$lstat) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$lstat) #Apresenta o histograma
boxplot(base$lstat) #Apresenta o boxplot

#trocando outliers por m�dia

bench8 <- 16.42 + 1.5*IQR(base$lstat)
bench8
base$lstat[base$lstat > bench8 ] <- mean(base$lstat)
boxplot(base$lstat)






#Vari�veis explicativas qualitativas

#chas (perto do rio)
table(base$chas) #Apresenta a distribui��o de frequencias absolutas
prop.table(table(base$chas)) #Apresenta a distribui��o de frequencias relativa

#rad (indice de mobilidade, perto da rodovia)
table(base$rad) #Apresenta a distribui��o de frequencias absolutas
prop.table(table(base$rad)) #Apresenta a distribui��o de frequencias relativa



#Vari�vel resposta

#medv (valor medio das casas) - tem outliers
summary(base$medv) #Apresenta as principais medidas de posi��o para a vari�vel dist�ncia da base base
hist(base$medv) #Apresenta o histograma
boxplot(base$medv) #Apresenta o boxplot

#trocando outliers por m�dia

bench9 <- 25.00 + 1.5*IQR(base$medv)
bench9
base$medv[base$medv > bench9 ] <- mean(base$medv)
boxplot(base$medv)

#Correla��o e an�lise visual das vari�veis quantitativas

#apresenta os scatter plot(abaixo,) histograma (diagonal principal) e correla��o (acima)
#precisamos ver se s�o correlacionadas
#escolhi fazer por vif mas deixei o c�digo da pearson aqui

#quanti <- base[,c("crim","zn","indus","nox","rm","age","dis","tax","ptratio","black","lstat","medv")]

#install.packages("psych")
#library(psych)

#pairs.panels(quanti[1:12])  #painel das colunas 1 a 4 


#escolhemos tirar a vari�vel dis pq � o que tem mais vars correlacionadas com ele


#Correla��o e an�lise visual das vari�veis quantitativas tirando a dis

#quanti <- base[,c("crim","zn","indus","nox","rm","age","tax","ptratio","black","lstat","medv")]

#pairs.panels(quanti[1:11])



#indus tem correlac��o com nox e com tax. tiramos ele agora

#quanti <- base[,c("crim","zn","nox","rm","age","tax","ptratio","black","lstat","medv")]

#pairs.panels(quanti[1:10])

#instalamos esses pacotes e esse codigo pra separar base teste e treino

#base$amostra<-ifelse(runif(nrow(base),0,1)>0.7,"TESTE", "TREINO")
#View(base)
#prop.table(table(base$amostra))




#outra op��o pra ver a correlc � o vif



#abaixo estou separando base teste e trainamento(tem que tirar as var correlacioanadas da base treinamento s�)
#Install.packages("caret")
library("caret")
#seed semente pra que sempre que algu�m rodar de igual a resposta

set.seed(123)
id_trainamento <- createDataPartition(y = base$medv, p = 0.70, list = FALSE)
trainamento <- base[id_trainamento,]
teste <- base[-id_trainamento,]

View(teste)
View(trainamento)


#agora que separamos as bases, vamos usar s� a trainamento depois da an�lise explorat�ria
#Para poder tirar as var correlac, precisamos rodar modelo primeiro


#LM � linear model. Y ~X1+X2+...
modelo<-lm(medv~
             #crim+
             #zn+
             #indus+  
             chas+
             #nox+
             rm+ 
             age+
             #dis+
             #rad+
             tax+
             ptratio+
             #black+
             lstat 
           , data=trainamento)#Sum�rio dos resultados do modelo
summary(modelo) #Apresenta o modelo

#abaixo estou vendo a correla��o das var. primeiro tem que rodar o modelo
#install.packages("HH")
library(HH)
#coloca o modelo dentro
vif(modelo)


#agora rodo abaixo o modelo acima mas com a base teste e o r quadrado tem que ser parecido

modeloteste<-lm(medv~
                  #crim+
                  #zn+
                  #indus+  
                  chas+
                  #nox+
                  rm+ 
                  age+
                  #dis+
                  #rad+
                  tax+
                  ptratio+
                  #black+
                  lstat 
                , data=teste)#Sum�rio dos resultados do modelo
summary(modeloteste) #Apresenta o modelo


#Calcular o Mean Absolute Error (MAE) na base de treino e teste
#Mean Absolute Error � a m�dia dos erros absolutos 
#install.packages("Metrics")
library(Metrics) #Pacote necess�rio para c�lculo do MAE

#INCLUI a coluna pmodelo na base trainamento e disse que o pmodelo � o valor predito

trainamento$pmodelo <- predict(modelo , trainamento) #Calcula os lucros preditos 
mae(pmodelo,trainamento$medv) #Calcula o MAE


View(trainamento)


#calcular o erro para a m�dia no lugar do valor predito

erro_usando_media <- mean((trainamento$medv - mean(trainamento$medv))^2)
erro_usando_media
sqrt(erro_usando_media)

mse <- mean((trainamento$medv - trainamento$pmodelo)^2)
mse
sqrt(mse)



#An�lise de residuos

#Teste de normalidade dos res�duos
res=residuals(modelo,type="response")
shapiro.test(res) 
#Como o p-valor � menor que 0,05, ent�o os res�duos n�o t�m distribui��o normal

#A vari�ncia � constante para cada n�vel da vari�vel independente
#De maneira ideal, os pontos devem cair aleatoriamente em ambos os lados de 0, sem padr�es nos pontos.
plot(modelo, which=1) # Residuals vs Fitted Plot. � o gr�fico dos res�duos versus os valores ajustados

