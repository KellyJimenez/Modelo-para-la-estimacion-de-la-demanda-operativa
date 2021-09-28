# Carga de librerias
library(rfm)
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)
library(ggplot2)
library(plotly)

# Lectura BD
rfm_data<- read_delim("D:/Usuarios/kjimenea/Desktop/Maestría/RFM/BD.csv",";", escape_double = FALSE, trim_ws = TRUE)
names(rfm_data)
head(rfm_data)

# Tipo de variables
class(rfm_data$customer_id)
rfm_data$customer_id=as.character(rfm_data$customer_id)
class(rfm_data$date_most_recent)
rfm_data$date_most_recent=as.Date(rfm_data$date_most_recent,format="%d/%m/%Y")
head(rfm_data)

# Cálculo de días hasta el actual
fecha_actual<-("2021-04-01")
class(fecha_actual)
fecha_actual=as.Date(fecha_actual)
rfm_data$recency_days<-(fecha_actual-rfm_data$date_most_recent)
rfm_data$recency_days=as.numeric(rfm_data$recency_days)
head(rfm_data)

# Descriptivos
summary(rfm_data)

# Eliminación NA
rfm_data <- rfm_data[!is.na(rfm_data$date_most_recent),]
summary(rfm_data)

# Descriptivos
summary(rfm_data)

# ingresos
summary(rfm_data$ingresos)
ggp <- ggplot(rfm_data, aes(x = ingresos))
ggp + geom_histogram(aes(y = ..density..),color = "gray", fill = "white")+
  geom_density(fill = "black", alpha = 0.2)+
  ggtitle("Daños por valor de la factura") + xlab("Valor Factura") + ylab("Densidad")

# recency_days
summary(rfm_data$recency_days)
ggp <- ggplot(rfm_data, aes(x = recency_days))
ggp + geom_histogram(aes(y = ..density..),color = "gray", fill = "white")+
  geom_density(fill = "black", alpha = 0.2)+
  ggtitle("Daño por recencia en días") + xlab("Recencia") + ylab("Densidad")



# number_of_orders
summary(rfm_data$number_of_orders)
ggp <- ggplot(rfm_data, aes(x = number_of_orders))
ggp + geom_histogram(color = "gray", fill = "white")+
  ggtitle("Número de daños en 30 días") + xlab("Daños 30 días") + ylab("Cantidad")


####Multivariado
rfm_data$number_of_orders2=as.character(rfm_data$number_of_orders)

qplot(ingresos, data = rfm_data, geom = "density", color = number_of_orders2,  facets = number_of_orders2 ~. )
qplot(number_of_orders2, ingresos, data = rfm_data, geom=c("boxplot"), fill=number_of_orders2)
qplot(number_of_orders2, ingresos, data = rfm_data, 
      geom="violin", stackdir = "center", binaxis = "y",color = number_of_orders2, fill = number_of_orders2)


qplot(recency_days, data = rfm_data, geom = "density", color = number_of_orders2,  facets = number_of_orders2 ~. )
qplot(number_of_orders2, recency_days, data = rfm_data, geom=c("boxplot"), fill=number_of_orders2)
qplot(number_of_orders2, recency_days, data = rfm_data, 
      geom="violin", stackdir = "center", binaxis = "y",color = number_of_orders2, fill = number_of_orders2)
qplot(recency_days, ingresos, data=rfm_data,col="gray")


# RFM
analysis_date  <-  lubridate :: as_date ( '2021-04-01' )
rfm_result  <-  rfm_table_customer ( rfm_data, 
                                     customer_id , 
                                     number_of_orders,
                                     recency_days , 
                                     ingresos,
                                     analysis_date,
                                     recency_bins = 5,
                                     frequency_bins = 5,
                                     monetary_bins = 5,
)
rfm_result
# Entre mayor el número mas critico el cliente y  mejor el agendamiento

#Heat Map
rfm_heatmap(rfm_result)

#Bar Chart
rfm_bar_chart(rfm_result)

rfm_bar_chart(rfm_result,
              bar_color = "gray27",
              xaxis_title = "Puntaje Monto",
              sec_xaxis_title = "Puntaje Frecuencia",
              yaxis_title = " ",
              sec_yaxis_title = "Puntaje Recencia")

#Histogram
rfm_histograms(rfm_result)

#Customers by Orders
rfm_order_dist(rfm_result)

##Scatter Plots

#Recency vs Monetary Value
rfm_rm_plot(rfm_result)

#Frequency vs Monetary Value
rfm_fm_plot(rfm_result)

#Recency vs Frequency
rfm_rf_plot(rfm_result)

# Segment New
segment_names <- c("Agenda < 12", "Agenda 12-24","Agenda 24-48","Agenda 48-72")

recency_lower <- c(5,3,2,1)
recency_upper <- c(5,5,5,5)

frequency_lower <- c(5,4,3,1)
frequency_upper <- c(5,5,5,5)

monetary_lower <- c(4,3,2,1)
monetary_upper <- c(5,5,5,5)



#segment_names <- c("Agenda < 12", "Agenda 12-24", "Agenda 12-24","Agenda 12-24","Agenda 24-48",
#                   "Agenda 24-48","Agenda 24-48","Agenda 48-72","Agenda 48-72","Agenda 48-72")


#recency_lower <- c(4, 4, 4, 2, 4, 4, 1, 4, 1, 1)
#recency_upper <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)

#frequency_lower <- c(4, 4, 2, 4, 4, 1, 4, 1, 1, 4)
#frequency_upper <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)

#monetary_lower <- c(4, 2, 4, 4, 1, 4, 4, 1, 4, 1)
#monetary_upper <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)

segments <-rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
                       frequency_lower, frequency_upper, monetary_lower, monetary_upper)

segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)


#Median Recency
rfm_plot_median_recency(segments)

#Median Frequency
rfm_plot_median_frequency(segments)

#Median Monetary Value
rfm_plot_median_monetary(segments)


write.table(segments, file = "D:/Usuarios/kjimenea/Desktop/Maestría/RFM/segments.txt", row.names = FALSE)


#####################
muestra<- sample_n(segments, size= 1000)
nrow(muestra)
head(muestra)

df<-muestra %>% 
  mutate(
    agenda = case_when( 
      recency_days<=31 & transaction_count>=4 & amount>=300000 ~ "Agenda < 12",
      recency_days<=60 & transaction_count>=3 & amount>=200000 ~ "Agenda 12-24",
      recency_days<=90 & transaction_count>=2 & amount>=150000 ~ "Agenda 24-48",
      recency_days<=120 & transaction_count>=1 & amount>=35000  ~ "Agenda 48-72",
      TRUE ~ "Others"))


t1<-table(df$segment, df$agenda)
t1

a<-df[df$segment==df$agenda,]
nrow(a)/nrow(df)



