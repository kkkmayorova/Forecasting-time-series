#загружаем данные
data <- read.csv("C:/Users/Ksenia/data_nir/export_all_not_sa2.csv", header = TRUE)
data <- data[,-(102:103)]
data_no_season <- data

#подгружаем библиотеки
library(seasonal) #очистка от сезнности
library(x13binary)
library(xlsx)
library(zoo)
library(xts)
library(Metrics)
library(forecast)
library(ggplot2)
checkX13()

#для красивой палитры
library(RColorBrewer)
myColors <- brewer.pal(5,"Dark2")
names(myColors) <- levels(pl$x)
colScale <- scale_colour_manual(name = "x",values = myColors)


#очистка от сезонности и построение прогноза, используя ARIMA

rmse_arima = rep(0, dim(data)[2]-1)
for (j in c(1:192)) {
df <- data[,j]
df_ts<-ts(df,start=c(2005,01),frequency=4)
mod <- seas(df_ts) #remove seasonal
df_out<-xts(mod$data, as.Date(as.yearqtr(data$Q)))
df_out_season_log = log(df_out$final)
x = data.frame(Y=as.matrix(df_out_season_log), 
               date=time(df_out_season_log)) #from ts to df
m <- as.matrix(x[,-2])
data_no_season[,j] = m


prirost_train = diff(m)[1:39]
prirost_test = diff(m)[40:57]

predicted = numeric(len = length(prirost_test))

for (i in 1:length(prirost_test)) {
  model = auto.arima(diff(m)[1:(38+i)], seasonal=FALSE)
  predicted[i] = as.numeric(forecast(model,1)[4])
}

pl = data.frame(x = c(rep("actual",57), rep("forecast",57)), 
                y = c(diff(m), rep(NA, 57)), 
                date = c(as.yearqtr(data$Q)[-1], as.yearqtr(data$Q)[-1]))
pl[97:114,2] = predicted

rmse_arima[j-1] = rmse(diff(m)[40:57], predicted)


print(ggplot(pl, aes(x = date, y = y, colour = factor(x), group = x)) +
  geom_point(size = 1.9)+
  colScale+
  geom_line(size = 0.8)+
  scale_x_continuous(breaks=seq(2005, 2020, 2))+
  xlab('Time')+
  ylab('')+
  ggtitle(paste("ARIMA: ",colnames(data)[j]))+
  theme(legend.title = element_blank())+
  annotate("text", x=2019, y=0.22, label = paste("rmse = ", round(rmse_arima[j-1],3))))

}
