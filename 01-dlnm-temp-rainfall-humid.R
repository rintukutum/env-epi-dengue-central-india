#-----------
rm(list=ls())
library('lubridate')
library('xts')
library('timetk')
library('tidyverse')
library('forecast')
library('data.table')
library('ggplot2')
library('gridExtra')
library('dlnm')
library('splines')
dir.create('figures',showWarnings = FALSE)

input <- read.table(
  "../NIREH-season/data/input1.txt",
  sep = '\t',header = TRUE,
  row.names = 1
)
# fix date
input$date <- as.Date(input$date,"%d-%m-%y")

# add month 
input$month <- month(input$date)

# day of the week character to numeric

input$dow <- factor(input$dow)
#
# dows <- c("Sunday" = 1,"Monday"=2, "Tuesday"=3,
#   "Wednesday"=4,"Thursday"=5,"Friday"=6,
#   "Saturday"=7)


########################
# DLNM model
# crossbasis function of rainfall
cb1.rainfall <- crossbasis(
  input$rainfall, lag=25, argvar=list(fun="poly"),
  arglag=list(fun="poly",degree=4)
)
model1 <- glm(
  formula = case ~ cb1.rainfall + ns(time, 7*14) + dow,
  family=quasipoisson(), data = input,maxit=100)
pred1.rainfall <- crosspred(
  cb1.rainfall, model1, at=0:135, bylag=1, cumul=TRUE)
pdf('./figures/01-rainfall-only-3D-FINAL.pdf',width = 7,height = 6)
plot(pred1.rainfall,xlab='Rainfall')
dev.off()

pdf('./figures/02-rainfall-only-FINAL.pdf',width = 7,height = 6)
vars <- c(20,40,60,80,100,120)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred1.rainfall,ptype = 'slices',
       var=vars[i], col=3,ci.arg=list(density=30,lwd=2),
       main = paste0("Rainfall=",vars[i],'(mm)'),
       ylim=c(0.8,1.4)
  )
}
dev.off()

pdf('./figures/02-rainfall-only-lags-FINAL.pdf',width = 7,height = 6)
lags <- c(0,3,5,10,15,20)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred1.rainfall, "slices", lag=lags[i], col=4,
       main = paste0("lag = ",lags[i],''),
       ci.arg=list(density=30,col=grey(0.7)))
}
dev.off()
########################
# DLNM model
#-----------------------
#' Mean Temperature
#' #-----------------------
cb1.mTemp <- crossbasis(
  input$Mean.temp, lag=25, argvar=list(fun="poly"),
  arglag=list(fun="poly",degree=4)
)
model.mTemp <- glm(
  formula = case ~ cb1.mTemp + ns(time, 7*14) + dow,
  family=quasipoisson(), data = input,maxit=100)
pred.mTemp <- crosspred(
  cb1.mTemp, model.mTemp, at=10:40, bylag=1, cumul=TRUE)
pdf('./figures/01-mean-temperature-only-3D-FINAL.pdf',width = 7,height = 6)
plot(pred.mTemp,xlab='Temperature')
dev.off()

pdf('./figures/01-mean-temperature-only-FINAL.pdf',width = 7,height = 6)
vars <- c(10,15,20,25,30,35,40)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.mTemp,ptype = 'slices',
       var=vars[i], col=3,ci.arg=list(density=30,lwd=2),
       main = paste0("Mean temp. =",vars[i],'(\u00B0C)'),
       ylim=c(0.8,1.4)
  )
}
dev.off()

pdf('./figures/01-temperature-only-lags-FINAL.pdf',width = 7,height = 6)
lags <- c(0,3,5,10,15,20)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.mTemp, "slices", lag=lags[i], col=4,
       main = paste0("lag = ",lags[i],''),
       ci.arg=list(density=30,col=grey(0.7)),xlab="Mean temp."
  )
}
dev.off()

########################
# DLNM model
#' DTR
cb1.dtr <- crossbasis(
  input$DTR, lag=25, argvar=list(fun="poly"),
  arglag=list(fun="poly",degree=4)
)
library(splines)
model.dtr <- glm(formula = case ~ cb1.dtr + ns(time, 7*14) + dow,
                 family=quasipoisson(), data = input,maxit=100)
pred.dtr <- crosspred(
  cb1.dtr, model.dtr, at=0:30, bylag=0.2, cumul=TRUE)
pdf('./figures/01-DTR-only-3d-FINAL.pdf',width = 7,height = 6)
plot(pred.dtr,xlab='DTR')
dev.off()

pdf('./figures/01-DTR-only-FINAL.pdf',width = 7,height = 6)
vars <- c(0,5,10,15,20,25)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.dtr,ptype = 'slices',
       var=vars[i], col=3,ci.arg=list(density=30,lwd=2),
       main = paste0("DTR =",vars[i],''),
       ylim=c(0.8,1.4)
  )
}
dev.off()

pdf('./figures/01-DTR-only-lags-FINAL.pdf',width = 7,height = 6)
lags <- c(0,3,5,10,15,20)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.dtr, "slices", lag=lags[i], col=4,
       main = paste0("lag = ",lags[i],''),
       ci.arg=list(density=30,col=grey(0.7)),xlab="DTR"
  )
}
dev.off()

########################
# DLNM model
#' RH only
cb1.rh <- crossbasis(
  input$RH., lag=25, argvar=list(fun="poly"),
  arglag=list(fun="poly",degree=4)
)
model.rh <- glm(formula = case ~ cb1.rh + ns(time, 7*14) + dow,
                family=quasipoisson(), data = input,maxit=100)
pred.rh <- crosspred(
  cb1.rh, model.rh, at=0:100, bylag=1, cumul=TRUE)
pdf('./figures/01-RH-only-3d-FINAL.pdf',width = 7,height = 6)
plot(pred.rh,xlab='RH')
dev.off()

pdf('./figures/01-RH-only-FINAL.pdf',width = 7,height = 6)
vars <- c(0,20,40,60,80,100)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.rh,ptype = 'slices',
       var=vars[i], col=3,ci.arg=list(density=30,lwd=2),
       main = paste0("RH =",vars[i],''),
       ylim=c(0.8,1.4)
  )
}
dev.off()

pdf('./figures/01-RH-only-lags-FINAL.pdf',width = 7,height = 6)
lags <- c(0,3,5,10,15,20)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.rh, "slices", lag=lags[i], col=4,
       main = paste0("lag = ",lags[i],''),
       ci.arg=list(density=30,col=grey(0.7)),xlab="RH"
  )
}
dev.off()

########################
# DLNM model
#' Abs. Humidity only
cb1.ah <- crossbasis(
  input$AH, lag=25, argvar=list(fun="poly"),
  arglag=list(fun="poly",degree=4)
)
model.ah <- glm(formula = case ~ cb1.ah + ns(time, 7*14) + dow,
                family=quasipoisson(), data = input,maxit=100)
pred.ah <- crosspred(
  cb1.ah, model.ah, at=0:100, bylag=1, cumul=TRUE)

pdf('./figures/01-AH-only-3d-FINAL.pdf',width = 7,height = 6)
plot(pred.ah,xlab="AH")
dev.off()
pdf('./figures/01-AH-only-FINAL.pdf',width = 7,height = 6)
vars <- c(0,20,40,60,80,100)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.ah,ptype = 'slices',
       var=vars[i], col=3,ci.arg=list(density=30,lwd=2),
       main = paste0("AH =",vars[i],''),
       ylim=c(0.1,3)
  )
}
dev.off()

pdf('./figures/01-AH-only-lags-FINAL.pdf',width = 7,height = 6)
lags <- c(0,3,5,10,15,20)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(pred.ah, "slices", lag=lags[i], col=4,
       main = paste0("lag = ",lags[i],''),
       ci.arg=list(density=30,col=grey(0.7)),xlab="AH"
  )
}
dev.off()
