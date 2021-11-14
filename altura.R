setwd("C:/Users/Liliany/Desktop/LUAN/CAFÉ/ANÁLISE DADOS")
dados<-read.csv("altura.csv", header = TRUE, sep = ";")

dados

altura<-dados$altura
altura
sistema<-dados$sistema
sistema
espa<-dados$espacamento
espa


#Análise de variância
analise<-aov(altura~espa+sistema+espa:sistema)
anova(analise)
summary(analise)


ls(analise)
residuos<-analise$residuals

plot(residuos, xlab = "Valores ajustados")
qqnorm(residuos)
qqline(residuos, col="blue", lwd=3)

par(mfrow=c(2,2))
plot(analise)

par(mfrow=c(1,1))

#normalidade dos dados gerais
shapiro<-shapiro.test(altura)
shapiro


#Teste de Tukey para fator recipiente
ttukey<-TukeyHSD(analise, "espa")
ttukey
plot(ttukey)


#Teste de Tukey para fator especie
ttukey<-TukeyHSD(analise, "sistema")
ttukey
plot(ttukey)


#Teste de Tukey para interação
ttukey<-TukeyHSD(analise, "espa:sistema")
ttukey
plot(ttukey)


interaction.plot(espa, sistema, altura, fixed=T, ylab = "Valores Médio")
interaction.plot(sistema, espa, altura, fixed=T, ylab = "Valores Médio")

require(agricolae)
require(ExpDes.pt)
require(ExpDes)

#com teste de Tukey
fat2.dic(espa, sistema, altura, quali = c(TRUE, TRUE), 
         mcomp = "tukey",fac.names = c("espa", "sistema"), 
         sigT = 0.05, sigF = 0.05)






