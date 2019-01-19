## Installing Libraries
install.packages(c('psych','sandwich','lmtest','car'),
                 dependencies = T)

## Loading libraries
library(psych)
library(sandwich)
library(lmtest)
library(car)
library(ggplot2)

# Modal Function Calculation
moda <- function(x) {
  z <- table(as.vector(x)) 
  z = names(z)[z == max(z)]
  z = as.numeric(z)
  return(z)
}

## Selecting files directory
setwd('~/papa/')

## Loading Database
df <- read.csv('dados_competição.csv', sep = ';', 
               fileEncoding = 'latin1') 

## Adjusting Variables
df$votos.leg <- df$votos.leg / df$eleitorado
df$votos.exec <- df$votos.exec / df$eleitorado
df$pib_pc <- df$pib / df$eleitorado

df$development <- df$pib_pc + df$idh

##Descriptive Statistics
describe(df[c('ano','ideol','candleg','candexec',
              'coligacao','votos.leg','votos.exec',
              'eleitorado','pib','pib_cat','idh',
              'idh_cat','development')])

## Checking party effectiveness
# Por estado, quantos partidos são efetivos ?
mff = tapply(df$votos.leg, list(df$estado, df$partido), mean)

for (st in levels(df$estado)){
  mf = sort(mff[st,],decreasing = T)

  dd = data.frame(x = as.vector(names(mf)),
                  y = as.vector(mf))
  
  dd$x <- factor(dd$x, levels = dd$x[order(-dd$y)])
  
  p = ggplot(dd, aes(x = x, y = y)) + 
    labs(title = st, y = 'Frequência', x = '') +
    geom_bar(stat = 'identity') + 
    geom_hline(yintercept = .1,color = "red") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(p)
}

mxx = tapply(df$votos.exec, list(df$estado, df$partido), mean)

for (st in levels(df$estado)){
  mx = sort(mxx[st,],decreasing = T)
  
  dd = data.frame(x = as.vector(names(mx)),
                  y = as.vector(mx))
  
  dd$x <- factor(dd$x, levels = dd$x[order(-dd$y)])
  
  p = ggplot(dd, aes(x = x, y = y)) + 
    labs(title = st, y = 'Frequência', x = '') +
    geom_bar(stat = 'identity') + 
    geom_hline(yintercept = .3,color = "red") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(p)
}

## Constructing dependent variable
df$comp <- ifelse(df$votos.leg > .1, 1, 0)
df$comp <- df$comp + ifelse(df$votos.exec > .3, 1, 0)

## Graphical analysis of variables
for (v in c('ano','ideol','candleg','candexec',
            'coligacao','votos.leg','votos.exec',
            'eleitorado','pib','idh','development',
            'comp')) {
  
  dd = data.frame(x = df[,v])
  
  p = ggplot(dd, aes(x = x)) + 
    labs(title = v, y = 'Frequência', x = '') +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
    geom_density(alpha = .2, fill = "lightblue") 
  
  print(p)
}

## Viewing relationship between variables
for (v in c('ano','ideol','candleg','candexec',
            'eleitorado','pib','idh','development')) {
  
  dd = data.frame(y = df[,v],
                  x = as.factor(df$comp))
  
  dd <- na.omit(dd)
  
  p = ggplot(data = dd, aes(y = y, x = x)) +
    labs(title = '', y = v, x = 'competitivity') +
    geom_boxplot()
  
  print(p)
}

## Regression Model
reg <- lm(comp ~ ideol + candleg + candexec +
            development + I(development ^ 2) +
            factor(ano), data = df)

## Model Summary
summary(reg)

## Checking Model Assumptions
# Multicolinearity
vif(reg)

# Normality of Residues
hist(residuals(reg))

# Heteroscedasticity
plot(reg,which = 1)

# Correcting Heteroskedasticity
coeftest(reg,vcov. = vcovHC)
