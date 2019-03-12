library("raster", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("ggplot2", quietly = TRUE)
library("tidyr", quietly = TRUE)
library("purrr", quietly = TRUE)
library("tools", quietly = TRUE)


setwd("E:/MK_articuloP")

#Temperatura maxima####
climat1 <-'F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Meses/TemperaturaMaxima/Tmax_1910-1949'
covarFileList <- list_files_with_exts(climat1, "tif")
climat1 <- mean(raster::stack(covarFileList))

climat2 <-'F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Meses/TemperaturaMaxima/Tmax_1950-1979'
covarFileList <- list_files_with_exts(climat2, "tif")
climat2 <- mean(raster::stack(covarFileList))

climat3 <-'F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Meses/TemperaturaMaxima/Tmax_1980-2009'
covarFileList <- list_files_with_exts(climat3, "tif")
climat3 <- mean(raster::stack(covarFileList))

nea.df<-read.csv("neartic.csv")
nea.df<-dplyr::select(nea.df, "POINT_X", "POINT_Y")
nea_df_t1<-data.frame(raster::extract(climat1,nea.df))
names(nea_df_t1)<-c("Tmax")
nea_df_t1$Region<-"Neartic region"
nea_df_t1$Periods<-"1910-1949"

nea_df_t2<-data.frame(raster::extract(climat2,nea.df))
names(nea_df_t2)<-c("Tmax")
nea_df_t2$Region<-"Neartic region"
nea_df_t2$Periods<-"1950-1979"

nea_df_t3<-data.frame(raster::extract(climat3,nea.df))
names(nea_df_t3)<-c("Tmax")
nea_df_t3$Region<-"Neartic region"
nea_df_t3$Periods<-"1980-2009"

neartic<-rbind(nea_df_t1,nea_df_t2,nea_df_t3)

#ZOnadeTransicion
trans.df<-read.csv("transicion.csv")
trans.df<-dplyr::select(trans.df, "POINT_X", "POINT_Y")
trans_df_t1<-data.frame(raster::extract(climat1,trans.df))
names(trans_df_t1)<-c("Tmax")
trans_df_t1$Region<-"Transition zone "
trans_df_t1$Periods<-"1910-1949"

trans_df_t2<-data.frame(raster::extract(climat2,trans.df))
names(trans_df_t2)<-c("Tmax")
trans_df_t2$Region<-"Transition zone "
trans_df_t2$Periods<-"1950-1979"

trans_df_t3<-data.frame(raster::extract(climat3,trans.df))
names(trans_df_t3)<-c("Tmax")
trans_df_t3$Region<-"Transition zone "
trans_df_t3$Periods<-"1980-2009"

Ztrans<-rbind(trans_df_t1,trans_df_t2,trans_df_t3)

#Neotropical
neo.df<-read.csv("neotropico.csv")
neo.df<-dplyr::select(neo.df, "POINT_X", "POINT_Y")
neo_df_t1<-data.frame(raster::extract(climat1,neo.df))
names(neo_df_t1)<-c("Tmax")
neo_df_t1$Region<-"Neotropical region"
neo_df_t1$Periods<-"1910-1949"

neo_df_t2<-data.frame(raster::extract(climat2,neo.df))
names(neo_df_t2)<-c("Tmax")
neo_df_t2$Region<-"Neotropical region"
neo_df_t2$Periods<-"1950-1979"

neo_df_t3<-data.frame(raster::extract(climat3,neo.df))
names(neo_df_t3)<-c("Tmax")
neo_df_t3$Region<-"Neotropical region"
neo_df_t3$Periods<-"1980-2009"

neotropic <-rbind(neo_df_t1,neo_df_t2,neo_df_t3)

mexico<-rbind(neartic, Ztrans, neotropic)
mexico[2]<-c("Mexico")
mexicoAll<-rbind(neartic, Ztrans, neotropic, mexico)

write.csv(mexicoAll,"Bio01y12/mexicoAll_Tmax.csv")

tb_Tmax<-mexicoAll %>% split(.$Region) %>%  
  map(~ group_by(.x,.$Periods) %>% summarise(Promedio=mean(Tmax, na.rm = TRUE), 
                                             Mediana = median(Tmax, na.rm = TRUE),
                                             Variance = var(Tmax, na.rm = TRUE),
                                             Quar = IQR(Tmax, na.rm = TRUE),
                                             Minimum = min(Tmax, na.rm = TRUE), 
                                             Maximum = max(Tmax, na.rm = TRUE, 
                                             StandarDev = sd(Tmax, na.rm = TRUE))))

write.csv(tb_Tmax[[1]],"tb_Tmax_Mexico.csv")
write.csv(tb_Tmax[[2]],"tb_Tmax_Nea.csv")
write.csv(tb_Tmax[[3]],"tb_Tmax_Neo.csv")
write.csv(tb_Tmax[[4]],"tb_Tmax_Ztrans.csv")

#mexicoAll %>% split(.$Region) %>%  
 # map(~ group_by(.x,.$Periods)
      
  #mexicoAll %>%
  #tbl_df() %>%
  #nest(-Region) %>%
  #mutate(Quantiles = map(data, ~ quantile(.$Tmax)),
   #      Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
  #unnest(Quantiles))

#graficas por temperatura
sp_t<-cbind(mexicoAll[c("Periods")],mexicoAll[c("Region")],mexicoAll[c("Tmax")])
head(sp_t)
unique(sp_t$Periods)
sp_t$Region_f = factor(sp_t$Region, levels = c("Mexico","Neartic region", "Transition zone ", "Neotropical region"))

p <- ggplot(data = sp_t, aes(x=Periods, y=Tmax)) + 
  geom_boxplot(alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2.5, color="red", fill="red") 
p  + facet_grid(. ~ Region_f)+theme(legend.position="bottom")  + labs( y = "Maximum temperature (°C)")

ggsave("Bio01y12/Tmax.jpg", dpi = 300)

rm(mexicoAll, climat1, climat2, climat3)

#Temperatura minima####

climat1 <-'F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Meses/TemperaturaMinima/Tmin_1910-1949'
covarFileList <- list_files_with_exts(climat1, "tif")
climat1 <- mean(raster::stack(covarFileList))


climat2 <-'F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Meses/TemperaturaMinima/Tmin_1950-1979'
covarFileList <- list_files_with_exts(climat2, "tif")
climat2 <- mean(raster::stack(covarFileList))

climat3 <-'F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Meses/TemperaturaMinima/Tmin_1980-2009'
covarFileList <- list_files_with_exts(climat3, "tif")
climat3 <- mean(raster::stack(covarFileList))


nea.df<-read.csv("neartic.csv")
nea.df<-dplyr::select(nea.df, "POINT_X", "POINT_Y")
nea_df_t1<-data.frame(raster::extract(climat1,nea.df))
names(nea_df_t1)<-c("Tmin")
nea_df_t1$Region<-"Neartic region"
nea_df_t1$Periods<-"1910-1949"

nea_df_t2<-data.frame(raster::extract(climat2,nea.df))
names(nea_df_t2)<-c("Tmin")
nea_df_t2$Region<-"Neartic region"
nea_df_t2$Periods<-"1950-1979"

nea_df_t3<-data.frame(raster::extract(climat3,nea.df))
names(nea_df_t3)<-c("Tmin")
nea_df_t3$Region<-"Neartic region"
nea_df_t3$Periods<-"1980-2009"

neartic<-rbind(nea_df_t1,nea_df_t2,nea_df_t3)

#ZOnadeTransicion
trans.df<-read.csv("transicion.csv")
trans.df<-dplyr::select(trans.df, "POINT_X", "POINT_Y")
trans_df_t1<-data.frame(raster::extract(climat1,trans.df))
names(trans_df_t1)<-c("Tmin")
trans_df_t1$Region<-"Transition zone "
trans_df_t1$Periods<-"1910-1949"

trans_df_t2<-data.frame(raster::extract(climat2,trans.df))
names(trans_df_t2)<-c("Tmin")
trans_df_t2$Region<-"Transition zone "
trans_df_t2$Periods<-"1950-1979"

trans_df_t3<-data.frame(raster::extract(climat3,trans.df))
names(trans_df_t3)<-c("Tmin")
trans_df_t3$Region<-"Transition zone "
trans_df_t3$Periods<-"1980-2009"

Ztrans<-rbind(trans_df_t1,trans_df_t2,trans_df_t3)

#Neotropical
neo.df<-read.csv("neotropico.csv")
neo.df<-dplyr::select(neo.df, "POINT_X", "POINT_Y")
neo_df_t1<-data.frame(raster::extract(climat1,neo.df))
names(neo_df_t1)<-c("Tmin")
neo_df_t1$Region<-"Neotropical region"
neo_df_t1$Periods<-"1910-1949"

neo_df_t2<-data.frame(raster::extract(climat2,neo.df))
names(neo_df_t2)<-c("Tmin")
neo_df_t2$Region<-"Neotropical region"
neo_df_t2$Periods<-"1950-1979"

neo_df_t3<-data.frame(raster::extract(climat3,neo.df))
names(neo_df_t3)<-c("Tmin")
neo_df_t3$Region<-"Neotropical region"
neo_df_t3$Periods<-"1980-2009"

neotropic <-rbind(neo_df_t1,neo_df_t2,neo_df_t3)


mexico<-rbind(neartic, Ztrans, neotropic)
mexico[2]<-c("Mexico")
mexicoAll<-rbind(neartic, Ztrans, neotropic, mexico)

write.csv(mexicoAll,"Bio01y12/mexicoAll_Tmin.csv")

tb_Tmin<-mexicoAll %>% split(.$Region) %>%  
  map(~ group_by(.x,.$Periods) %>% summarise(Promedio=mean(Tmin, na.rm = TRUE), 
                                             Mediana = median(Tmin, na.rm = TRUE),
                                             Variance = var(Tmin, na.rm = TRUE),
                                             Quar = IQR(Tmin, na.rm = TRUE),
                                             Minimum = min(Tmin, na.rm = TRUE), 
                                             Maximum = max(Tmin, na.rm = TRUE, 
                                                           StandarDev = sd(Tmin, na.rm = TRUE))))

write.csv(tb_Tmin[[1]],"tb_Tmin_Mexico.csv")
write.csv(tb_Tmin[[2]],"tb_Tmin_Nea.csv")
write.csv(tb_Tmin[[3]],"tb_Tmin_Neo.csv")
write.csv(tb_Tmin[[4]],"tb_Tmin_Ztrans.csv")

#mexicoAll %>% split(.$Region) %>%  
#  map(~ group_by(.x,.$Periods)
      
#      mexicoAll %>%
#       tbl_df() %>%
#        nest(-Region) %>%
#        mutate(Quantiles = map(data, ~ quantile(.$Tmin2)),
#               Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
#        unnest(Quantiles))

#graficas por temperatura
sp_t<-cbind(mexicoAll[c("Periods")],mexicoAll[c("Region")],mexicoAll[c("Tmin")])
head(sp_t)
unique(sp_t$Periods)

sp_t$Region_f = factor(sp_t$Region, levels = c("Mexico","Neartic region", "Transition zone ", "Neotropical region"))

p <- ggplot(data = sp_t, aes(x=Periods, y=Tmin)) + 
  geom_boxplot(alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2.5, color="red", fill="red") 
p  + facet_grid(.~Region_f)+theme(legend.position="bottom")  + labs( y = "Minimum temperature (°C)")

ggsave("Bio01y12/Tmin.jpg", dpi = 300)