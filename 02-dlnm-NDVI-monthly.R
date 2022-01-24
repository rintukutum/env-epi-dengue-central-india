#'--------------------------------------
#' Montly analysis of NVDI was performed
#' as the methodlogydescribed in following article
#' https://www.nature.com/articles/s41598-019-56688-1
#' https://github.com/deusthindwa/dlnm.typhoid.nts.climate.blantyre.malawi
#'--------------------------------------
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
library('plotly')
library('MuMIn')
dir.create('figures',showWarnings = FALSE)
####
#### Monthly data of NDVI and cases
climate <- read.table(
  "../NIREH-season/data/dlnm-input-for-NDVI_NDBI.txt",sep = '\t',
  stringsAsFactors = FALSE,
  header = TRUE, row.names = 1
)
climate$date <- dmy(climate$date)
climate.nvdi <- subset(climate, select = c(date, Av_NDVI))

# convert data frames to xts objects for use 
# in time series plotting.
climate.nvdi <- as.xts(climate.nvdi[,-1,drop = FALSE], 
                       order.by = as.Date(climate.nvdi[,1]))
#--- 
climate.nvdi <-tk_tbl(climate.nvdi, 
                      preserve_index = TRUE, rename_index = "date") 

climate.nvdi.ts <- ts(na.omit(climate.nvdi$Av_NDVI), frequency = 12)
#additive series
trend_n <-tk_tbl(seasadj(mstl(climate.nvdi.ts)), preserve_index = FALSE) 
#seasonally-adjusted 
climate.nvdi$nvdi_sea <- trend_n$value 
climate.nvdi$nvdi_sea[climate.nvdi$nvdi_sea < 0] <- 0
setnames(climate.nvdi, old="Av_NDVI", new="nvdi_obs")
#-----
# sub. fig 1
nvdi.p1 <- mstl(climate.nvdi.ts,s.window="periodic") %>% 
  ggfortify:::autoplot.ts(
    main="A",xlab="Years (2012-2019)",
    ylab="NVDI",size=1,
    colour="blue2",is.date=FALSE) + theme_bw()

nvdi.p2 <- ggplot(as.data.frame(climate.nvdi)) + 
  geom_line(aes(date, nvdi_obs, color="Observed data"), size=0.8) + 
  geom_line(aes(date, nvdi_sea, color="Seasonal-adjusted"), size=0.8) + 
  scale_color_manual(values = c("Observed data"="magenta",
                                "Seasonal-adjusted"="blue3")) +
  labs(title="B", x ="Year", y = "NVDI") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(face="bold", size=10), axis.text.y = element_text(face="bold", size=10)) + 
  theme(legend.justification=c(0.5,0), 
        legend.position = c(0.2,0.05), 
        legend.text = element_text(size = 10), legend.title = element_text(face="bold", size=0)) + 
  theme(legend.key.height=unit(1,"line")) + 
  theme(legend.key.width=unit(1,"line"))

png('figures/02-monthly-NVDI-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(nvdi.p1,nvdi.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()

climate.nvdi.spin <- subset(climate.nvdi, 
                            select=c(date,nvdi_sea)) 
climate.nvdi.spin$month <- month(climate.nvdi.spin$date)
climate.nvdi.spin$year <- year(climate.nvdi.spin$date)
climate.nvdi.spin$date <- NULL
climate.nvdi.spin <- spread(climate.nvdi.spin, year, nvdi_sea)
climate.nvdi.spin <- as.matrix(climate.nvdi.spin)[,-1]


# interactive plot
plot_ly(x = c(2012:2019), 
        y = c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec"),
        z = ~climate.nvdi.spin, type = "contour", 
        colorscale = 'Earth', 
        contours = list(showlabels = TRUE)) %>% 
  colorbar(title = "Seasonal-adjusted \n NVDI ") %>%
  layout(title="<b>B</b>", xaxis=list(title ="Year",color="black"), 
         yaxis=list(title="Month",color="black"), font=list(size = 13))

#-----------
case <- climate[,c('date','case')]
#--- convert dataframes to xts objects for time series plot.
case <- as.xts(case[,-1,drop = FALSE], order.by = as.Date(case[,1]))
case.degue <-tk_tbl(case, preserve_index = TRUE, rename_index = "date") 
#----------seasonally-adjusted cases and climate
case.degue.ts <- ts(case.degue$case, frequency = 12)
# multiplicative series (log-transform); 
# already has at least 1 degue case.
trend_n <-tk_tbl(exp(seasadj(mstl(log(case.degue.ts+1)))), 
                 preserve_index = FALSE) 
#seasonally-adjusted cases: trend+remainder
case.degue$case_sea <- trend_n$value  
case.degue$case_sea[case.degue$case_sea < 0] <- 0
setnames(case.degue, old="case", new="case_obs")

case.p1 <- mstl(case.degue.ts,s.window="period") %>% 
  ggfortify:::autoplot.ts(
  main="",xlab="Years (2012-2019)",ylab="Monthly no. of cases",size=1,
  colour="orange2",is.date=FALSE) + theme_bw()

#-----
# sub. fig 2
case.p2 <- ggplot(as.data.frame(case.degue)) + 
  geom_line(aes(date, case_obs, color="Observed data"), 
            size=0.8) + 
  geom_line(aes(date, case_sea, color="Seasonal-adjusted"), 
            size=0.7, alpha = 0.8) + 
  scale_color_manual(values = c("Observed data"="blue2",
                                "Seasonal-adjusted"="magenta")) +
  labs(title="B", x ="Year", y = "Monthly no. of cases") + 
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

png('figures/02-monthly-cases-seasonal-adjust.png',
    width = 1200,height = 700,res = 150)
grid.arrange(grobs = list(case.p1,case.p2),
             ncol=2,widths=c(0.4,0.6))
dev.off()

#----------change AIC to QAIC for model comparisons
dengue.quasipoisson <- function(...) { 
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic 
  res
}
#----------prepare final monthly NTS dataset for use in DLNM
mo.dlnmN <- bind_cols(case.degue, climate.nvdi)
mo.dlnmN$date...4 <- NULL
colnames(mo.dlnmN)[1] <- 'date'
mo.dlnmN$time <- seq.int(from = 1, to=nrow(mo.dlnmN), by=1)
mo.dlnmN$year <- year(mo.dlnmN$date)
mo.dlnmN$month <- month(mo.dlnmN$date)
mo.dlnmN$case_seaX<-round(mo.dlnmN$case_sea, digits = 0)
mo.dlnmN$case_obsX<-round(mo.dlnmN$case_obs, digits = 0)
#---------------
# Modelling will be performed with case_seaX as
# seasonality was influencing in the monthly dataset
#---------------
# test all possible dfs combinations for 
# parameters and lag
QAICtable <- data.frame(
  model.no=rep(NA,9), lag.df=rep(NA,9), 
  fx1.df=rep(NA,9),  QAIC.degue=rep(NA,9)
)
l=1
for(j in 3:5){
  for(i in 3:5){
    degue.lagknots <- logknots(8, fun="ns", df=i)
    # nvdi_sea is used as there is a seasonal effect observed
    # in our monthly data
    degue.varknots.nvdi=equalknots(
      mo.dlnmN$nvdi_sea, fun="ns", df=j)
    degue.mo.cb.nvdi <- crossbasis(
      mo.dlnmN$nvdi_sea, lag=8, 
      argvar=list(fun="ns", knots=degue.varknots.nvdi), 
      arglag=list(knots=degue.lagknots)
    )
    degue.modelNVDI <- glm(
      mo.dlnmN$case_seaX~degue.mo.cb.nvdi + month + year, 
      family=dengue.quasipoisson(),
      na.action=na.exclude,
      mo.dlnmN)
    
    QAICtable[l,] <- c(l,i,j,QAIC(
      degue.modelNVDI, chat=summary(degue.modelNVDI)$dispersion))
    l=l+1  
  }
}
# construct cross-basis for cases using optimal 
# dfs 
df_ops <- QAICtable[order(QAICtable$QAIC.degue)[1],]
varknots <- equalknots(
  mo.dlnmN$nvdi_sea, 
  fun="ns", df=df_ops[,'fx1.df']
)
lagknots <- logknots(8, fun="ns", df=df_ops[,'lag.df'])
mo.cb.nvdi.case <- crossbasis(
  mo.dlnmN$nvdi_sea, lag =8, 
  argvar = list(knots=varknots), 
  arglag = list(knots=lagknots)
)
summary(mo.cb.nvdi.case)

#----------model fitting for iNTS
mo.model.NVDI <- glm(
  case_seaX ~  mo.cb.nvdi.case + month + year, 
  family = quasipoisson(), 
  na.action=na.exclude, mo.dlnmN)

#----------validated model predictions for cases
mo.pred.nvdi.case <- crosspred(mo.cb.nvdi.case, mo.model.NVDI)

#----------plotting countour and curves for rainfall on iNTS
pdf('./figures/02-NVDI-adj-seasonality-contour-plot.pdf',width = 9,height = 8)
par(mar=c(5,5,2,2)+0.1)
plot(mo.pred.nvdi.case, "contour", key.title=title("Cases"), 
     plot.title=
       title("", xlab ="Avg. NVDI", 
             ylab = "Monthly lag", 
             cex.lab=1.3, cex.axis=1.5,main="A"))
dev.off()
pdf('./figures/02-NVDI-adj-seasonality-slice-plot.pdf',
    width = 5,height = 8)
par(mfrow=c(2,1))
plot(mo.pred.nvdi.case, "slices", 
     xlab="Monthly lag (given mean NVDI 0.2 /month)", var=0.2, 
     col="orange2", 
     ci.arg=list(col=topo.colors(70, alpha = 1)), 
     ci.level=0.95, ci='b', lwd=4.5, ylab="Cases", 
     cex.lab=1.3, cex.axis=1.5,main="B")
plot(mo.pred.nvdi.case, "slices", 
     xlab="Monthly lag (given mean NVDI 0.35 /month)", 
     var=c(0.35), col="orange2", 
     ci.arg=list(col=topo.colors(70, alpha = 1)), 
     ci.level=0.95, ci='b', lwd=4.5, ylab="Cases", 
     cex.lab=1.3, cex.axis=1.5,main="C")
dev.off()
