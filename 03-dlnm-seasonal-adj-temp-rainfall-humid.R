rm(list=ls())
library('lubridate')
library('xts')
library('timetk')
library('tidyverse')
library('forecast')
library('data.table')
library('ggplot2')
library('gridExtra')

dir.create('figures',showWarnings = FALSE)
input <- read.table(
  "../NIREH-season/data/input1.txt",
  sep = '\t',header = TRUE,
  row.names = 1
)

# fix date
input$date <- dmy(input$date)

# add month 
input$month <- month(input$date)

# day of the week character to numeric
input$dow <- factor(input$dow)
#
# dows <- c("Sunday" = 1,"Monday"=2, "Tuesday"=3,
#   "Wednesday"=4,"Thursday"=5,"Friday"=6,
#   "Saturday"=7)
##################################################
#------ Rainfall -- Seasonal Adjustment
##################################################
rainfall <- subset(input, select = c(date, rainfall))
#--- convert rainfall data frames to 
#--- xts objects for use in time series plotting.
rainfall <- as.xts(rainfall[,-1,drop = FALSE], 
                   order.by = as.Date(rainfall[,1]))
#--- 
rainfall <- tk_tbl(rainfall, 
                  rainfall = TRUE, rename_index = "date") 
rainfall.ts <- ts(rainfall$rainfall, frequency = 1)
#-----
# sub. fig 1
rain.p1 <- mstl(rainfall.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="Rainfall (mm)",size=0.25,colour="blue2",
    is.date=FALSE) + theme_bw()
#-----
# additive series
# seasonal adjusted rainfall: trend+remainder
# Seasonal components are estimated iteratively using 
# STL (Seasonal Decomposition of Time Series by Loess)
trend_n <-tk_tbl(seasadj(mstl(rainfall.ts)), 
                 preserve_index = FALSE) 
# seasonal adjusted value
rainfall$rainfall_sea <- trend_n$value 
rainfall$rainfall_sea[rainfall$rainfall_sea < 0] <- 0

setnames(rainfall, old="rainfall", new="rainfall_obs")
#-----
# sub. fig 2
rain.p2 <- ggplot(as.data.frame(rainfall)) + 
  geom_line(aes(date, rainfall_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, rainfall_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Rainfall (mm)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(
    face="bold", size=10), 
    axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

png('figures/03-daily-rainfall-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(rain.p1,rain.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()
##################################################
#------ MeanTemperature -- Seasonal Adjustment
##################################################
temp <- subset(input, select = c(date, Mean.temp))
colnames(temp)[2] <- 'temperature'
#--- convert temperature data frames to 
#--- xts objects for use in time series plotting.
temp <- as.xts(temp[,-1,drop = FALSE], 
               order.by = as.Date(temp[,1]))

temp <-tk_tbl(temp, 
              temperature = TRUE, rename_index = "date") 
temp.ts <- ts(temp$temperature, frequency = 1)
temp.p1 <- mstl(temp.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="Avg. Temp. (°C)",size=0.25,colour="blue2",
    is.date=FALSE) + theme_bw()
#additive series
trend_n <-tk_tbl(seasadj(mstl(temp.ts)), 
                 preserve_index = FALSE) 
#seasonally-adjusted
temp$temp_sea <- trend_n$value 
temp$temp_sea[temp$temp_sea < 0] <- 0
setnames(temp, old="temperature", new="temp_obs")

temp.p2 <- ggplot(as.data.frame(temp)) + 
  geom_line(aes(date, temp_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, temp_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Avg. Temp. (°C)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(
    face="bold", size=10), 
    axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

#--------------
png('figures/03-daily-temp-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(temp.p1,temp.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()
##################################################
#------ Minimum Temperature -- Seasonal Adjustment
##################################################
temp <- subset(input, select = c(date, Min.Temp))
colnames(temp)[2] <- 'temperature'
#--- convert min. temperature  data frames to 
#--- xts objects for use in time series plotting.
temp <- as.xts(temp[,-1,drop = FALSE], 
               order.by = as.Date(temp[,1]))

#--- 
temp <-tk_tbl(temp, 
              temperature = TRUE, rename_index = "date") 
temp.ts <- ts(temp$temperature, frequency = 1)
temp.p1 <- mstl(temp.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="Min. Temp. (°C)",size=0.25,colour="blue2",
    is.date=FALSE) + theme_bw()
#additive series
trend_n <-tk_tbl(seasadj(mstl(temp.ts)), 
                 preserve_index = FALSE) 
#seasonally-adjusted rainfall: trend+remainder
temp$temp_sea <- trend_n$value 
temp$temp_sea[temp$temp_sea < 0] <- 0
setnames(temp, old="temperature", new="temp_obs")

temp.p2 <- ggplot(as.data.frame(temp)) + 
  geom_line(aes(date, temp_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, temp_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Min. Temp. (°C)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(
    face="bold", size=10), 
    axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

#--------------
png('figures/03-daily-Min-temp-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(temp.p1,temp.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()

##################################################
#------ Max Temperature -- Seasonal Adjustment
##################################################
temp <- subset(input, select = c(date, Max.temp))
colnames(temp)[2] <- 'temperature'
#--- convert Max. temperature data frames to 
#--- xts objects for use in time series plotting.
temp <- as.xts(temp[,-1,drop = FALSE], 
               order.by = as.Date(temp[,1]))
#--- 
temp <-tk_tbl(temp, 
              temperature = TRUE, rename_index = "date") 
temp.ts <- ts(temp$temperature, frequency = 1)
temp.p1 <- mstl(temp.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="Max. Temp. (°C)",size=0.25,colour="blue2",
    is.date=FALSE) + theme_bw()
#additive series
trend_n <-tk_tbl(seasadj(mstl(temp.ts)), 
                 preserve_index = FALSE) 
#seasonally-adjusted 
temp$temp_sea <- trend_n$value 
temp$temp_sea[temp$temp_sea < 0] <- 0
setnames(temp, old="temperature", new="temp_obs")

temp.p2 <- ggplot(as.data.frame(temp)) + 
  geom_line(aes(date, temp_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, temp_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Max. Temp.(°C)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(
    face="bold", size=10), 
    axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

#--------------
png('figures/03-daily-max-temp-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(temp.p1,temp.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()

##################################################
#------ Absolute humidity  -- Seasonal Adjustment
##################################################
hum <- subset(input, select = c(date, AH))
colnames(hum)[2] <- 'humidity'
#--- convert Abs. Humidity data frames to 
#--- xts objects for use in time series plotting.
hum <- as.xts(hum[,-1,drop = FALSE], 
               order.by = as.Date(hum[,1]))
#--- 
hum <-tk_tbl(hum, 
             humidity = TRUE, rename_index = "date") 
hum.ts <- ts(hum$humidity, frequency = 1)
hum.p1 <- mstl(hum.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="Abs. Humidity (g/m3)",size=0.25,colour="blue2",
    is.date=FALSE) + theme_bw()
#additive series
trend_n <-tk_tbl(seasadj(mstl(hum.ts)), 
                 preserve_index = FALSE) 
#seasonally-adjusted 
hum$hum_sea <- trend_n$value 
hum$hum_sea[temp$hum_sea < 0] <- 0
setnames(hum, old="humidity", new="hum_obs")

hum.p2 <- ggplot(as.data.frame(hum)) + 
  geom_line(aes(date, hum_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, hum_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Abs. Humidity(g/m3)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(
    face="bold", size=10), 
    axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

#--------------
png('figures/03-daily-abs-humidity-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(hum.p1,hum.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()

##################################################
#------ relative humidity  -- Seasonal Adjustment
##################################################
hum <- subset(input, select = c(date, RH.))
colnames(hum)[2] <- 'humidity'
#--- convert Rel. Humidity data frames to 
#--- xts objects for use in time series plotting.
hum <- as.xts(hum[,-1,drop = FALSE], 
              order.by = as.Date(hum[,1]))
#--- 
hum <-tk_tbl(hum, 
             humidity = TRUE, rename_index = "date") 
hum.ts <- ts(hum$humidity, frequency = 1)
hum.p1 <- mstl(hum.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="Relative Humidity (% RH)",size=0.25,colour="blue2",
    is.date=FALSE) + theme_bw()
#additive series
trend_n <-tk_tbl(seasadj(mstl(hum.ts)), 
                 preserve_index = FALSE) 
#seasonally-adjusted 
hum$hum_sea <- trend_n$value 
hum$hum_sea[temp$hum_sea < 0] <- 0
setnames(hum, old="humidity", new="hum_obs")

hum.p2 <- ggplot(as.data.frame(hum)) + 
  geom_line(aes(date, hum_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, hum_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Relative Humidity (% RH)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(
    face="bold", size=10), 
    axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

#--------------
png('figures/03-daily-relative-humidity-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(hum.p1,hum.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()