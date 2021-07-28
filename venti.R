#install.packages("rWind")
library(rWind)  
library(raster)  
library(rworldmap)  
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(SmarterPoland)
library(maptools)
library(ggthemes)
library(shape) 
library(CircStats)
#unloadNamespace("rlang")
library(tidyverse)
library(RColorBrewer)
library(MASS)
library(vcd)
library(devtools)
#remotes::install_github("tim-salabim/metvurst")
#remotes::install_github("SigmaMonstR/gfs-r")
#library(metvurst)
library(ncdf4)
library(rvest)
library(gridExtra)
library(lubridate)
library(scales)
library(gplots)
library(GGally)
library(Directional)
library(PerformanceAnalytics)
library(lubridate)
library(fitdistrplus)
library(forecast)

global_path <- "E:/Analizy i sprawozdania/Tutoriale" #œcie¿ka do folderu z danymi
setwd(global_path) 
ds.map <- readOGR(dsn="gminyDLN","gminyDLN",use_iconv = TRUE,encoding="UTF-8") 
getwd()  

pal_month <- colorRampPalette(c( "blue","green4","yellow1","red2","blue"))(n = 12)
pal_reg <- colorRampPalette(c( "midnightblue","springgreen4","yellow3","red2","deeppink4"))(n = 12)

#############################################################################
wd9 <- wind.dl(2020,2,16,9,14,18,50,52, type="csv")
wd12 <- wind.dl(2020,2,16,12,14,18,50,52, type="csv")#Dolny Œl¹sk, o godzinie 12
head(wd9) 
#wind_data<-wind.fit(wd9) ##NIE DZIA£A
r_dir <- wind2raster(wd9) 
newmap <- getMap(resolution = "high") 
plot(r_dir)  


png("wichry1.png", width = 500, height = 500) 
alpha<- arrowDir(wd9)  
plot(r_dir$speed, main="wind direction (arrows) and speed (colours)")  
lines(newmap, lwd=3) 
lines(ds.map, lwd=1) 
Arrowhead(wd9$lon, wd9$lat, angle=alpha, arr.length = 0.25, arr.type="curved") 
dev.off()


w <- wind.dl(2020,2,16,1,16,16,51,51, type="csv")

lata <- c(2014,2016,2017,2018,2019,2020)
mies <- c(1:12)

godz <- c(0,3,6,9,12,15,18,21)

venti <- function(lata,mies,godz)
{
  i <- 1
  ws <- list()
  m30 <- c(4,6,9,11)
  m31 <- c(1,3,5,7,8,10,12)
  md <- 30
  for(r in lata)
  {
    for(m in mies)
    {
      if(m %in% m30)
      {
        md <- 30
      }
      else
      {
        if(m %in% m31)
        {
          md <- 31
        }
        else
        {
          md <- 28
        }
      }
      for(d in 1:md)
      {
        for(g in godz)
        {
          w<-wind.dl(r,m,d,g,16,16,51,51)  
          ws[[i]]<-w
          i <- i+1
        }
      }
    }
  }
  return(ws)
}
wiatry <- venti(lata,mies,godz)

df <- do.call(rbind.data.frame, wiatry)
write.csv(df, file = "wiatry21.csv",fileEncoding = "UTF-8")

df[df$speed == max(df$speed),]
df[df$speed >10,]

df2 <- df[-9941,]


png("wichry2.png", width = 800, height = 800) 
circ.plot(df2$dir, stack=TRUE, bins=90,shrink =6, cex = 1)
dev.off()


work <- function(bins = 90)
{
  box <- 360/bins
  
  praca <- matrix(0,bins,2)
  praca[,1] <- c(1:bins)
  for(i in 1:dim(df2)[1])
  {
    praca[floor(df2$dir[i]/box+1),2] <- praca[floor(df2$dir[i]/box+1),2] + df2$speed[i]
  }
  colnames(praca) <- c("id","v")
  praca <- as.data.frame(praca)
  return(praca)
}

praca <- work(bins = 32)
praca$value <- praca$v/100
labels <- data.frame(matrix(0,4,2))
colnames(labels) <- c("angle","text")
labels$angle <- c(90,0,270,180)
labels$text <- c("N","E","S","W")

p <- ggplot(praca, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) 
  #geom_text(data=labels, 
  #          aes(x=labels$angle, y=50, label=text), 
  #          color="black", fontface="bold",alpha=0.6, size=2.5, angle= labels$angle, inherit.aes = FALSE ) 
p
dev.off()

hist(df2$speed, xlim = c(0,15), breaks = seq(0,15,1), col = "springgreen4", freq = FALSE, 
     main = "Prêdkoœci wiatrów", xlab = "Prêdkoœæ mph")
fit.gamma <- fitdistr(df2$speed, "gamma") 
fit.logN <- fitdistr(df2$speed, "lognormal") 
fit.logN
fit.chisq <- fitdistr(df2$speed, "chisq") 
ks.test(df2$speed, "plnorm", fit1$estimate)
curve(dlnorm(x, meanlog = fit.logN$estimate[1], sdlog = fit.logN$estimate[2]), from = 0, col = "deeppink4", add = TRUE)


#œmieci
dane <- c(90,91,93,94,90,90,23,23,23,23,90,1,90)
circ.plot(dane, stack=TRUE, bins=8)
pr <- as.data.frame(matrix(c(1,2,3,4,20,10,5,1),4,2))
colnames(pr) <- c("id","value")


url_dir <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NCEP_Global_Best.htmlTable?tmpsfc[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],tmp2m[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],ugrd10m[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],vgrd10m[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],pratesfc[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],rh2m[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],prmslmsl[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],dlwrfsfc[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)],dswrfsfc[(2019-06-05):1:(2020-02-27T12:00:00Z)][(51):1:(51)][(16):1:(16)]"
ud <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NCEP_Global_Best.csv?tmpsfc[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],tmp2m[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],ugrd10m[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],vgrd10m[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],pratesfc[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],rh2m[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],prmslmsl[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],dlwrfsfc[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)],dswrfsfc[(2019-09-23):1:(2020-02-28T12:00:00Z)][(50):1:(50)][(56):1:(62)]"
udcsv <- read.csv(ud, header = FALSE, skip = 2, stringsAsFactors = FALSE)





############################            NOWA ANALIZA   ##################################




dfw <- read.csv("wiatry2.csv", header = FALSE, skip = 2, stringsAsFactors = FALSE)
nag³ówki <- read.csv("wiatry2.csv")[1,]
colnames(dfw) <- colnames(nag³ówki)

ds <- uv2ds(dfw$ugrd10m, dfw$vgrd10m)

dfw$dir <- ds[,1]
dfw$vel <- ds[,2]
dfw$temp <- dfw$tmp2m-272

min(dfw$tmp2m)-272
max(dfw$tmp2m)-272
max(dfw$vel)
dfw[dfw$vel == max(dfw$vel),]




dfwc <- dfw[-13085,] 
dfwc$month <- month(dfwc$time)
max(dfwc$vel)
dfw <- NaN

dP <- numeric(nrow(dfwc))
dT <- numeric(nrow(dfwc))
dT.doba <- numeric(nrow(dfwc))
dP.doba <- numeric(nrow(dfwc))
opady.doba <- numeric(nrow(dfwc))
W.wiatru.doba <- numeric(nrow(dfwc))
W.U.doba <- numeric(nrow(dfwc))
W.V.doba <- numeric(nrow(dfwc))
dir.mesos.doba <- numeric(nrow(dfwc))
dir.var.doba <- numeric(nrow(dfwc))
temp.mean <- numeric(nrow(dfwc))
temp.var <- numeric(nrow(dfwc))

month2season <- function(m)
{
  if(m<3 || m==12)
  {
    return("Zima")
  }
  else
  {
    if(m>=3 && m<6)
    {
      return("Wiosna")
    }
    else
    {
      if(m>=6 && m<9)
      {
        return("Lato")
      }
      else
      {
        return("Jesieñ")
      }
    }
  }
}
dir2wind <- function(d)
{
  if(d>=337.5 || d<22.5)
  {
    return("N")
  }
  else
  {
    if(d>=22.5 && d<67.5)
    {
      return("NE")
    }
    else
    {
      if(d>=67.5 && d<112.5)
      {
        return("E")
      }
      else
      {
        if(d>=112.5 && d<157.5)
        {
          return("SE")
        }
        else
        {
          if(d>=157.5 && d<202.5)
          {
            return("S")
          }
          else
          {
            if(d>=202.5 && d<247.5)
            {
              return("SW")
            }
            else
            {
              if(d>=247.5 && d<292.5)
              {
                return("W")
              }
              else
              {
                return("NW")
              }
            }
          }
        }
      }
    }
  }
}
dfwc$season <- unlist(lapply(dfwc$month, month2season ))#month2season(dfwc$month)
dfwc$wind <- unlist(lapply(dfwc$dir, dir2wind ))# dir2wind(dfwc$dir)

for(i in 2:nrow(dfwc))
{
  dP[i] <- dfwc$prmslmsl[i]-dfwc$prmslmsl[i-1]
  dT[i] <- dfwc$temp[i]-dfwc$temp[i-1]
}
for(i in 9:nrow(dfwc))
{
  dT.doba[i] <- dfwc$temp[i]-dfwc$temp[i-8]
  dP.doba[i] <- dfwc$prmslmsl[i]-dfwc$prmslmsl[i-8]
  opady.doba[i] <- sum(dfwc$pratesfc[(i-8):i], na.rm = TRUE)
  W.wiatru.doba[i] <- sum(dfwc$vel[(i-8):i], na.rm = TRUE) 
  W.U.doba[i] <- sum(dfwc$ugrd10m[(i-8):i], na.rm = TRUE) 
  W.V.doba[i] <- sum(dfwc$vgrd10m[(i-8):i], na.rm = TRUE)
  dir.mesos.doba[i] <- circ.mean(pi/180 * dfwc$dir[(i-8):i])
  dir.var.doba[i] <- circ.disp(pi/180 * dfwc$dir[(i-8):i])$var
  temp.mean[i] <- mean(dfwc$temp[(i-8):i], na.rm = TRUE)
  temp.var[i] <- var(dfwc$temp[(i-8):i], na.rm = TRUE)
}
#Directional::circ.summary(dfwc$dir,rads = FALSE)
#CircStats::circ.summary(dfwc$dir*(pi/180))
#circ.disp(dfwc$dir*(pi/180))

#test.data <- c(90,80,70,110)
#circ.summary(test.data, rads = FALSE)

dfwc$dP <- dP
dfwc$dT <- dT
dfwc$dT.doba <- dT.doba
dfwc$dP.doba <- dP.doba
dfwc$opady.doba <- opady.doba*3600*24
dfwc$W.wiatru.doba <- W.wiatru.doba
dfwc$W.U.doba <- W.U.doba*24*3600/1000
dfwc$W.V.doba <- W.V.doba*24*3600/1000
dfwc$dir.mesos.doba <- dir.mesos.doba
dfwc$dir.var.doba <- unlist(dir.var.doba)
dfwc$temp.mean <- temp.mean
dfwc$temp.var <- temp.var


#df.num <- as.data.frame(dfwc[,c(4:15,17:28)])
#df.num[1,22]
my_smooth <- function(data,mapping,...){ggplot(data=data,mapping=mapping)+geom_smooth(method = "loess",se=FALSE,fullrange=TRUE)+geom_point(...)+scale_shape_manual(values=c(0,1))}
my_density <- function(data,mapping,...){ggplot(data=data,mapping=mapping)+geom_density(...,lwd=1)}


png("pairs2.png",6000,6000)
ggpairs(dfwc, legend = 17, columns = c(6:15,21:28,30,17:18), mapping = ggplot2::aes(colour=season),
        lower = list(continuous = wrap("smooth", alpha = 0.2, size=0.1, method = "loess")),
        upper = list(continuous = wrap("smooth", alpha = 0.2, size=0.1, method = "loess")),
        diag=list(continuous=wrap(my_density,alpha=0.3)))+
  theme(legend.position = "bottom")
dev.off()

png("pairs3.png",6000,6000)
ggpairs(dfwc, legend = 18, columns = c(6:15,21:28, 17:18), mapping = ggplot2::aes(colour=wind),
        lower = list(continuous = wrap("smooth", alpha = 0.2, size=0.1, method = "loess")),
        upper = list(continuous = wrap("smooth", alpha = 0.2, size=0.1, method = "loess")),
        diag=list(continuous=wrap(my_density,alpha=0.3)))+
  theme(legend.position = "bottom")
dev.off()


########### wykresy ##############
w1 <- ggplot(data = dfwc, aes(x = dir, y = temp, color = month)) +
  geom_point(size=dfwc$vel, alpha = 0.4)+
  scale_color_gradientn(colours = pal_month)+
  #geom_path(alpha = 0.1)+
  coord_polar()+
  geom_hline(yintercept = seq(-20, 40, by = 5), colour = "grey", size = 0.2) +
  geom_vline(xintercept = seq(0, 360, by = 22.5), colour = "grey", size = 0.2) +
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 359.99, by = 22.5), labels=c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")) +
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, by = 10)) +
  scale_size_continuous(name="velocity")+
  theme_bw() +
  labs(title = "Wykres temperatura ~wiatr", y = "Temp [C]")+
  theme(panel.border = element_blank(),
        panel.grid  = element_blank(),
        legend.key = element_blank(),
        legend.direction = "vertical", legend.box = "vertical",
        legend.key.size = unit(5,"lines"),
        legend.title = element_text( size=20),
        legend.text = element_text(size=20),
        axis.ticks = element_line(colour = "grey"),
        axis.ticks.length = unit(-1, "lines"),
        axis.ticks.margin = unit(1.3,"lines"),
        axis.text =  element_text(size=24),
        axis.text.y = element_text(size=24),
        #axis.title.y =
        axis.title = element_blank(),
        axis.line=element_line(),
        axis.line.x=element_blank(),
        axis.line.y = element_line(colour = "grey"),
        plot.title = element_text(hjust = 0, size = 20))

png("ventemp2.png", width = 1600, height = 1600)
w1
dev.off()

w2<-w1
for(i in 1:12)
{
  w2 <- w2+geom_smooth(data = subset(dfwc, month == i),method=loess, col=pal_reg[i], size = 2)
}
#w2+geom_smooth(col = pal_reg, method = "loess", se = FALSE, aes(x= dir, y = temp, group = month, color = month))

png("ventemp3.png", width = 1600, height = 1600)
w2
dev.off()



#w2 +scale_linetype_manual(name="loess", values = 1:12,
#                        guide = guide_legend(override.aes = aes(color = pal_reg, fill = NA)))

w3 <- ggplot(data = dfwc, aes(x = dir, y = temp, color = month)) +
  geom_point(size=dfwc$vel, alpha = 0.4)+
  scale_color_gradientn(colours = pal_month)+
  #geom_path(alpha = 0.1)+
  geom_hline(yintercept = seq(-20, 40, by = 5), colour = "grey", size = 0.2) +
  geom_vline(xintercept = seq(0, 360, by = 22.5), colour = "grey", size = 0.2) +
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 359.99, by = 22.5), labels=c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")) +
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, by = 10)) +
  scale_size_continuous(name="velocity")+
  scale_fill_continuous(limits = c(0,12), breaks = c(1:12),
                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))+
  theme_bw() +
  labs(title = "Wykres temperatura ~wiatr", y = "Temp [C]")+
  theme(panel.border = element_blank(),
        panel.grid  = element_blank(),
        legend.key = element_blank(),
        legend.direction = "vertical", legend.box = "vertical",
        legend.key.size = unit(5,"lines"),
        legend.title = element_text( size=20),
        legend.text = element_text(size=20),
        axis.ticks = element_line(colour = "grey"),
        axis.ticks.length = unit(-1, "lines"),
        axis.ticks.margin = unit(1.3,"lines"),
        axis.text =  element_text(size=24),
        axis.text.y = element_text(size=24),
        #axis.title.y =
        axis.title = element_blank(),
        axis.line=element_line(),
        axis.line.x=element_blank(),
        axis.line.y = element_line(colour = "grey"),
        plot.title = element_text(hjust = 0, size = 20))

for(i in 1:4)
{
  w3 <- w3+geom_smooth(data = subset(dfwc, month == 3*i-2),method=loess, col=pal_reg[3*i-2], size = 2)
}
png("ventemp4.png", width = 1800, height = 1800)
w3
dev.off()

scalling <- c((log(max(dfwc$vel))-log(min(dfwc$vel)))/max(dfwc$rh2m), (max(dfwc$temp)-min(dfwc$temp))/max(dfwc$rh2m))
dfwc <- within(dfwc, {
  x.end <- log(vel)  + scalling[1]*rh2m*cos(dir)/25
  y.end <- temp + scalling[2] *rh2m*sin(dir)/25
})

w4 <- ggplot(data = dfwc, aes(x = log(vel), y = temp, color = month)) +
  #geom_point(size=dfwc$vel, alpha = 0.4)+
  geom_segment(data =  dfwc,
               size = 1, alpha = 0.2,
               aes(x = log(vel),
                   xend = x.end,
                   y = temp,
                   yend = y.end,
                   colour=month),
               arrow = arrow(length = unit(0.1, "cm")))+
  scale_color_gradientn(colours = pal_month)+
  #geom_path(alpha = 0.1)+
  geom_hline(yintercept = seq(-15, 35, by = 5), colour = "grey", size = 0.2) +
  geom_vline(xintercept = seq(-5, 3, by = 1), colour = "grey", size = 0.2) +
  scale_x_continuous(limits = c(-5, 3), expand = c(0, 0), breaks = seq(-5, 3, by = 1)) +
  scale_y_continuous(limits = c(-15, 35), breaks = seq(-15, 35, by = 5)) +
  scale_size_continuous(name="velocity")+
  scale_fill_continuous(limits = c(0,12), breaks = c(1:12),
                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))+
  theme_bw() +
  labs(title = "Wykres temperatura ~wiatr", y = "Temp [C]")+
  theme(panel.border = element_blank(),
        panel.grid  = element_blank(),
        legend.key = element_blank(),
        legend.direction = "vertical", legend.box = "vertical",
        legend.key.size = unit(5,"lines"),
        legend.title = element_text( size=20),
        legend.text = element_text(size=20),
        axis.ticks = element_line(colour = "grey"),
        axis.ticks.length = unit(-1, "lines"),
        axis.ticks.margin = unit(1.3,"lines"),
        axis.text =  element_text(size=24),
        axis.text.y = element_text(size=24),
        #axis.title.y =
        axis.title = element_blank(),
        axis.line=element_line(),
        axis.line.x=element_blank(),
        axis.line.y = element_line(colour = "grey"),
        plot.title = element_text(hjust = 0, size = 20))

#png("ventemp5.png", width = 1800, height = 1800)
#w4
#dev.off()

w5 <- w4 
for(i in 1:4)
{
  w5 <- w5+geom_smooth(data = subset(dfwc, month == 3*i-2),method=loess, col=pal_reg[3*i-2], size = 2)
}
png("ventemp6.png", width = 1800, height = 1800)
w5
dev.off()



w6 <- ggplot(data = dfwc, aes(x = log(vel), y = temp, color = month)) +
  geom_point(size=dfwc$rh2m/30, alpha = 0.4)+
  scale_color_gradientn(colours = pal_month)+
  #geom_path(alpha = 0.1)+
  geom_hline(yintercept = seq(-15, 35, by = 5), colour = "grey", size = 0.2) +
  geom_vline(xintercept = seq(-5, 3, by = 1), colour = "grey", size = 0.2) +
  scale_x_continuous(limits = c(-5, 3), expand = c(0, 0), breaks = seq(-5, 3, by = 1)) +
  scale_y_continuous(limits = c(-15, 35), breaks = seq(-15, 35, by = 5)) +
  scale_size_continuous(name="velocity")+
  scale_fill_continuous(limits = c(0,12), breaks = c(1:12),
                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))+
  theme_bw() +
  labs(title = "Wykres temperatura ~wiatr", y = "Temp [C]")+
  theme(panel.border = element_blank(),
        panel.grid  = element_blank(),
        legend.key = element_blank(),
        legend.direction = "vertical", legend.box = "vertical",
        legend.key.size = unit(5,"lines"),
        legend.title = element_text( size=20),
        legend.text = element_text(size=20),
        axis.ticks = element_line(colour = "grey"),
        axis.ticks.length = unit(-1, "lines"),
        axis.ticks.margin = unit(1.3,"lines"),
        axis.text =  element_text(size=24),
        axis.text.y = element_text(size=24),
        #axis.title.y =
        axis.title = element_blank(),
        axis.line=element_line(),
        axis.line.x=element_blank(),
        axis.line.y = element_line(colour = "grey"),
        plot.title = element_text(hjust = 0, size = 20))

w7 <- w6 
for(i in 1:4)
{
  w7 <- w7+geom_smooth(data = subset(dfwc, month == 3*i-2),method=loess, col=pal_reg[3*i-2], size = 2)
}
png("ventemp7.png", width = 1800, height = 1800)
w7
dev.off()

################### analiza prêdkoœci ############


hist(dfwc$vel, xlim = c(0,15), breaks = seq(0,15,1), col = "springgreen4", freq = FALSE, 
     main = "Szybkoœci wiatrów", xlab = "Szybkoœæ m/s")
fit.gamma <- fitdistr(dfwc$vel, "gamma") 
fit.logN <- fitdistr(dfwc$vel, "lognormal") 
fit.logN
fit.chisq <- fitdistr(dfwc$vel, "chi-squared", start = list(df=4)) 
fit.chisq
ks.test(df2$speed, "plnorm", fit1$estimate)
curve(dlnorm(x, meanlog = fit.logN$estimate[1], sdlog = fit.logN$estimate[2]), from = 0, col = "deeppink4", add = TRUE)
curve(dchisq(x, df = fit.logN$estimate[1]), from = 0, col = "blue", add = TRUE)


hist(dfwc$vel^2, xlim = c(0,200), col = "springgreen4", freq = FALSE, 
     main = "Szybkoœci wiatrów", xlab = "Szybkoœæ m^2/s^2", ylim = c(0,0.06))
fit.chisq2 <- fitdistr(dfwc$vel^2, "chi-squared", start = list(df=2)) 
curve(dchisq(x, df = 9), from = 0, col = "blue", add = TRUE)
curve(dchisq(x, df = 2), from = 0, col = "red", add = TRUE)
curve(dchisq(x, df = 3), from = 0, col = "green", add = TRUE)
curve(dchisq(x, df = 10), from = 0, col = "springgreen4", add = TRUE)
ks.test(dfwc$vel^2, "pchisq", 10)


hist(dfwc$vel[dfwc$month==1], xlim = c(0,15), col =  scales::alpha("cadetblue3",0.3), freq = FALSE, 
     main = "Szybkoœci wiatrów", xlab = "Szybkoœæ m/s", ylim = c(0,0.32))
hist(dfwc$vel[dfwc$month==4], xlim = c(0,15), col = scales::alpha("springgreen4",0.3), freq = FALSE, add = TRUE)
hist(dfwc$vel[dfwc$month==7], xlim = c(0,15), col = scales::alpha("khaki3",0.3), freq = FALSE, add = TRUE)
hist(dfwc$vel[dfwc$month==10], xlim = c(0,15), col = scales::alpha("deeppink4",0.3), freq = FALSE, add = TRUE)
fit1 <- fitdistr(dfwc$vel[dfwc$month==1], "lognormal") 
fit2 <- fitdistr(dfwc$vel[dfwc$month==4], "lognormal") 
fit3 <- fitdistr(dfwc$vel[dfwc$month==7], "lognormal") 
fit4 <- fitdistr(dfwc$vel[dfwc$month==10], "lognormal") 
curve(dlnorm(x, meanlog = fit1$estimate[1], sdlog = fit1$estimate[2]), from = 0, col = "cadetblue3", add = TRUE)
curve(dlnorm(x, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2]), from = 0, col = "springgreen4", add = TRUE)
curve(dlnorm(x, meanlog = fit3$estimate[1], sdlog = fit3$estimate[2]), from = 0, col = "khaki3", add = TRUE)
curve(dlnorm(x, meanlog = fit4$estimate[1], sdlog = fit4$estimate[2]), from = 0, col = "deeppink4", add = TRUE)


trdf <- dfwc$vel^2/mean(dfwc$vel^2)
hist(trdf,col = "springgreen4", freq = FALSE, 
     main = "Szybkoœci wiatrów", xlab = "Szybkoœæ m^2/s^2 / œredni kwadrat szybkoœci")
fit.chisq2b <- fitdistr(trdf, "chi-squared", start = list(df=2)) 
fit.chisq2b
curve(dchisq(x, df = 1), from = 0, col = "red", add = TRUE)
curve(dchisq(x, df = 2), from = 0, col = "green", add = TRUE)
curve(dchisq(x, df = 3), from = 0, col = "blue", add = TRUE)



qqplot(qchisq(ppoints(500), df = 1), trdf, main = expression("Q-Q plot for" ~~ {chi^2}[nu == 1])) 
  qqline(trdf, distribution = function(p) qchisq(p, df = 1), prob = c(0.1, 0.6), col = 2)

  
qqnorm(log(dfwc$vel), col = "springgreen4")
qqline(log(dfwc$vel),col="deeppink4")


# create a vector of quantiles
quants <-seq(0,1,length=81)[2:80]
# find quantiles for the fitted distribution
fit_quants <- qlnorm(quants,fit.logN$estimate['meanlog'], fit.logN$estimate['sdlog'])
# find quantiles of the original data
data_quants <- quantile(dfwc$vel,quants)
# fit and data quantiles side by side
data.frame(fit_quants,data_quants)
# create Q-Q plot
plot(fit_quants, data_quants, xlab="Theoretical Quantiles", ylab="Sample Quantiles", col = "springgreen4")
title(main = "Q-Q plot of lognormal fit against data")
abline(0,1,col="deeppink4")


par(mfrow=c(1,2))
descdist(dfwc$vel, discrete=FALSE, boot=500)
descdist(trdf, discrete=FALSE, boot=500)
par(mfrow=c(1,1))


#read.csv("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NCEP_Global_Best.csv?tmpsfc[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],tmp2m[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],ugrd10m[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],vgrd10m[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],pratesfc[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],rh2m[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],prmslmsl[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],dlwrfsfc[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)],dswrfsfc[(2020-03-03T12:00:00Z):1:(2020-03-03T12:00:00Z)][(-90.0):1:(90.0)][(0.0):1:(359.5)]")

plot(x=dfwc$dP[dfwc$pratesfc > 0], y = dfwc$pratesfc[dfwc$pratesfc > 0])
plot(x=dfwc$dP, y = dfwc$vel)
ggplot(dfwc, aes(vel, abs(dP))) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

ggplot(dfwc, aes(vel, abs(dT))) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

ggplot( subset(dfwc, dT<3), aes(vel, abs(dP))) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

ggplot( subset(dfwc, dT>3), aes(vel, abs(dP))) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

ggplot(dfwc, aes(dT, dP)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

ggplot(dfwc, aes(temp, prmslmsl)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

ggplot(dfwc, aes(pratesfc, prmslmsl)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

png("pairs.png", width = 2000, height = 2000)
pairs(dfwc[,c(4:20)])
dev.off()


#szeregi czasowe

pairs(dfwc[,c(4:15,17:28)])

df900 <-dfwc[seq(4,dim(dfwc)[1],8),]
df9.ts <- ts(df900$temp.mean, freq = 365, start = c(2014,30))
plot(df9.ts)
seasonplot(df9.ts, year.labels = TRUE, col = rainbow(8))
Acf(df9.ts)
df9.ts.dec <- decompose(df9.ts)
plot(df9.ts.dec)

df9.ts.u <- ts(df900$W.U.doba , freq = 365, start = c(2014,30))
plot(decompose(df9.ts.u))

opady.ts <- decompose(aggregate(ts(df900$opady.doba, freq = 360, start = c(2014,30)),nfrequency = 12, FUN = sum))

plot(decompose(aggregate(ts(df900$opady.doba, freq = 360, start = c(2014,30)),nfrequency = 12, FUN = sum)))

par(mfrow=c(2,2))
qqnorm(as.vector(opady.ts$random), main = "Wykres kwantylowy")
qqline(as.vector(opady.ts$random), col = "red")
hist(as.vector(opady.ts$random), col = "seagreen4", freq = FALSE, main = "Histogram")
curve(dnorm(x, sd = sd(as.vector(opady.ts$random), na.rm = TRUE)), col="deeppink4", lwd=2, add=TRUE)
Acf(as.vector(opady.ts$random), main = "ACF")
pacf(as.vector(opady.ts$random), na.action = na.pass, main = "PACF")
par(mfrow=c(1,1))


#boxploty
pal_wind <- c("firebrick2","yellow1", "orange1","olivedrab2", "lightskyblue", "lightpink2","seagreen2", "chartreuse1")#E N NE NW S SE SW W
pal_dens <- c("firebrick2","gold1","orange1","olivedrab4","blue4","deeppink4","seagreen4","chartreuse1")

jm_boxplots <- function(data, gr1, gr2, var, 
                        palette.box, palette.dens = NaN, palette.leg.gr1 = NaN, palette.leg.gr2 = NaN, 
                        xlab = "Value", scale = 5, xt = 5, pos = "topleft", add.dens = TRUE, add.leg = TRUE,
                        round.m = 2)
{
  len.gr1 <- nlevels(as.factor(data[,gr1]))
  len.gr2 <- nlevels(as.factor(data[,gr2]))+1
  lev.gr1 <- levels(as.factor(data[,gr1]))
  lev.gr2 <- levels(as.factor(data[,gr2]))
  
  m <- min(data[,var], na.rm = TRUE)
  M <- max(data[,var], na.rm = TRUE)
  y.M <- len.gr1*len.gr2
  ks.g <- 0
  ks.g.i <- 1
  max.mean.g <- m
  min.mean.g <- M
  max.mean.g.i <- 1
  min.mean.g.i <- 1
  if(gr1 == gr2)
  {
    box.scale = 1
  }
  else
  {
    box.scale = 2.5
  }
  
  plot(x=c(m,M), y = c(0,y.M+len.gr2+3), col = "white", xlab = xlab, ylab = "")
  lines(x=c(0,0),y=c(-2,y.M+len.gr2+8), lty = 1)
  lines(x=c(m-5,M+5), y=c(0,0))
  mean.line <- mean(data[,var], na.rm = TRUE)
  boxplot(data[,var], add=TRUE, notch = TRUE, col = "gray91", at = y.M+len.gr2+1.5, width = 2, horizontal = TRUE)
  dens.data <- density(data[is.na(data[,var]) == FALSE,var])
  dens.data$y <- scale*(len.gr2+1)*dens.data$y+(len.gr2+1)*len.gr1+1
  lines(dens.data, col = "grey21", lty = 3, lwd = 2)
  lines(x = c(mean.line, mean.line),y=c(-2,y.M+len.gr2+8), col = "plum4", lty = 4)
  gr.distr <- list()
  for(g1 in 1:(len.gr1)) #du¿e skrzynki
  {
    Y <- (len.gr2+1)*g1 #wysokoœæ du¿ego boxa
    data.gr <- data[data[,gr1] == lev.gr1[g1],var]
    gr.distr[[g1]] <- data.gr
    mean.gr <- mean(data.gr, na.rm = TRUE)
    if(g1>1)
    {
      for(i in 1:(g1-1))
      {
        if(ks.g < ks.test(data.gr,gr.distr[[i]])$statistic)
        {
          ks.g <- round(ks.test(data.gr,gr.distr[[i]])$statistic,2)
          ks.g.i <- c(g1, i)
        }
      }
    }
    if(max.mean.g<mean.gr)
    {
      max.mean.g <- mean.gr
      max.mean.g.i <- g1
    }
    if(min.mean.g>mean.gr)
    {
      min.mean.g <- mean.gr
      min.mean.g.i <- g1
    }
    if(gr1 != gr2)
    {
      dens.gr <- density(data.gr[is.na(data.gr) == FALSE])
      dens.gr$y <- scale*(len.gr2+1)*dens.gr$y+Y-len.gr2
      boxplot(data[data[,gr1] == lev.gr1[g1],var], add=TRUE, 
            notch = TRUE, col = "gray91", at = Y, width = 1.3, horizontal = TRUE , alpha = 0.8)
    }
    lines(x=c(mean.gr,mean.gr), y= c(Y-len.gr2,Y+1), col = "purple", lty = 3, lwd = 1.2)
    distr <- list()
    ks <- 0
    ks.i <- c(0,0)
    Jmax.mean <- m
    Jmin.mean <- M
    Jmax.mean.g <- 1
    Jmin.mean.g <- 1
    for(g2 in 1:(len.gr2-1)) #ma³e skrzynki - kolory
    {
      data.temp <- data[data[,gr1] == lev.gr1[g1] & data[,gr2] == lev.gr2[g2],var]
      distr[[g2]] <- data.temp
      mean.g <- mean(data.temp, na.rm = TRUE)
      if(!is.nan(mean.g))
      {
        if(mean.g < Jmin.mean)
        {
          Jmin.mean <- round(mean.g,round.m)
          Jmin.mean.g <- g2
        }
        if(mean.g > Jmax.mean)
        {
          Jmax.mean <- round(mean.g,round.m)
          Jmax.mean.g <- g2
        }
      }
      if(g2>1 && gr1 != gr2 )
      {
        for(i in 1:(g2-1))
        {
          if(ks < ks.test(data.temp,distr[[i]])$statistic)
          {
            ks <- round(ks.test(data.temp,distr[[i]])$statistic,2)
            ks.i <- c(g2, i)
          }
        }
      }
      if(length(data.temp)>1)
      {
        boxplot(data.temp, col = palette.box[g2], add = TRUE, at = Y-len.gr2+ g2, horizontal = TRUE, notch = TRUE, width = box.scale)
        lines(y=c(Y+1,Y+1), x=c(m,M), col = "black")
        if(add.dens)
        {
          dens.temp <- density(data.temp[is.na(data.temp) == FALSE])
          dens.temp$y <- scale*(len.gr2+1)*dens.temp$y+Y-len.gr2
          lines(dens.temp, col = palette.dens[g2], lwd = 2)
        }
      }
    }
    text(y=Y, x= xt,labels = lev.gr1[g1])
    if(gr1 != gr2)
    {
      text(y=Y-1,x=xt,labels = paste("max.KS =",ks, lev.gr2[ks.i[1]], lev.gr2[ks.i[2]]))
      text(y=Y-2,x=xt,labels=paste("max.mean =",Jmax.mean,lev.gr2[Jmax.mean.g]))
      text(y=Y-3,x=xt,labels=paste("min.mean =",Jmin.mean,lev.gr2[Jmin.mean.g]))
      if(add.dens)
      {
        lines(dens.gr, col = "grey31", lty = 3, lwd = 2)
      }
    }
  }
  if(add.leg)
  {
    legend(pos,lev.gr2, fill = palette.box)
  }
  text(y=(len.gr2+1)*(len.gr1+1)-1, x=xt, labels = paste("max.KS = ",ks.g,lev.gr1[ks.g.i[1]], lev.gr1[ks.g.i[2]]))
  text(y=(len.gr2+1)*(len.gr1+1)-2,x=xt,labels=paste("max.mean =",round(max.mean.g,round.m),lev.gr1[max.mean.g.i]))
  text(y=(len.gr2+1)*(len.gr1+1)-3,x=xt,labels=paste("min.mean =",round(min.mean.g,round.m),lev.gr1[min.mean.g.i]))
}
colnames(dfwc)

png("bp dT.png",800,900)
jm_boxplots(data = df900, gr1 = 17, gr2 = 18, var = 21, pal_dens,pal_dens,xlab="dobowe dT [K]", xt = 10)
dev.off()

png("bp dobowe opady.png",1200,1500)
jm_boxplots(data = df900, gr1 = 17, gr2 = 18, var = 23, pal_dens,pal_dens,xlab="dobowe opady [kg/m^2]", xt = 300, 
            pos = "topright", add.dens = FALSE)
dev.off()

png("bp dobowe dP.png",1200,1500)
jm_boxplots(data = df900, gr1 = 17, gr2 = 18, var = 22, pal_dens,pal_dens,xlab="dobowe dP [Pa]", xt = 2000,scale = 800)
dev.off()


png("bp long.png",1000,1200)
jm_boxplots(data = df900, gr1 = 17, gr2 = 18, var = 11, round.m = 0,
            pal_dens,pal_dens,xlab="net downward longwave radiation flux, W m-2", xt = 400,scale = 60)
dev.off()

par(mfrow=c(1,2))
jm_boxplots(data = df900, gr1 = 17, gr2 = 17, var = 25, c("red","yellow","chartreuse3","cadetblue"),
            c("red","yellow","chartreuse3","cadetblue"),xlab="dobowe przemnieszczenie powietrza E [km]", xt = 50,scale = 30,
            add.leg = FALSE)
jm_boxplots(data = df900, gr1 = 17, gr2 = 17, var = 26, c("red","yellow","chartreuse3","cadetblue"),
            c("red","yellow","chartreuse3","cadetblue"),xlab="dobowe przemnieszczenie powietrza N [km]", xt = 50,scale = 30,
            add.leg = FALSE)
par(mfrow=c(1,1))

ggplot(df900, aes(x=W.U.doba, y=W.V.doba)) + 
  geom_density_2d(data = subset(df900, season=="Jesieñ"), col = "deeppink4")+
  geom_density_2d(data = subset(df900, season=="Wiosna"), col = "springgreen4")+
  geom_density_2d(data = subset(df900, season=="Lato"), col = "yellow")+
  geom_density_2d(data = subset(df900, season=="Zima"), col = "cadetblue")

ggplot(df900, aes(x=W.U.doba, y=W.V.doba,colour = season)) + 
  geom_density_2d()+
  theme_dark()

ggplot(df900, aes(x=W.U.doba, y=W.V.doba)) + 
  stat_density_2d(aes(fill = stat(level)), geom = "polygon")+
  theme_dark()

png("heatmapsdir.png",800,800)
ggplot(df900, aes(x=W.U.doba, y=W.V.doba)) + 
stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  facet_wrap(. ~ season, ncol = 2) + scale_fill_viridis_c()+
  theme_dark()+
  labs(x = "Wektor E", y = "Wektor N")
dev.off()

plot(x=c(-20,15), y = c(0,10))
boxplot(dfwc[,21],add = TRUE, horizontal = TRUE, at = 4)
d1 <- density(dfwc[,21])
d1$y <- d1$y+7
lines(d1)
dstr <- list(rnorm(100),rexp(20),rnorm(100,2,5))
ksi <- list()
for(i in 1:3)
{
  for(j in i:3)
  {
    append(ksi, ks.test(dstr[[i]], dstr[[j]])$statistic)
  }
}
