#====================packages
#instalasi dan pemanggilan paket yang diperlukan
install.packages("dplyr")
install.packages("readr")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("vars")
install.packages("readr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("caret")
install.packages("car")
install.packages("tseries")
install.packages("vars")
install.packages("tdyr")
install.packages("gbm")

library(dplyr)
library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(vars)
library(readr)
library(ggplot2)
library(lubridate)
library(caret)
library(car)
library(tseries)
library(vars)
library(tidyr)
library(gbm)

#====================dataset
#membaca file csv
arm_tj<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/jumlah_armada_tj.csv")
colnames(arm_tj)
head(arm_tj)

pen_lrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/jumlah_penumpang_lrt.csv")
colnames(pen_lrt)<-c("bulan","tahun","jumlah_penumpang_lrt")
colnames(pen_lrt)
head(pen_lrt)

pen_mrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/jumlah_penumpang_mrt.csv")
colnames(pen_mrt)<-c("bulan","tahun","jumlah_penumpang_mrt")
colnames(pen_mrt)
head(pen_mrt)

perj_lrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/jumlah_perjalanan_lrt.csv")
colnames(perj_lrt)<-c("bulan","tahun","jumlah_perjalanan_lrt")
colnames(perj_lrt)
head(perj_lrt)

perj_mrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/jumlah_perjalanan_mrt.csv")
colnames(perj_mrt)<-c("bulan","tahun","jumlah_perjalanan_mrt")
colnames(perj_mrt)
head(perj_mrt)

#lihat data test & samsub
samsub<-read.csv("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/sample_submision.csv")
head(samsub)
test_data<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/testing_jumlah_penumpang_tj.csv")

#data training (data y)
train_pen_tj<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/Raw Data/training_jumlah_penumpang_tj.csv")
head(train_pen_tj) #data 1/2015 sd 12/2023
tail(train_pen_tj)

#=================
#data x
#menggabungkan csv based bulan tahun
datax <- train_pen_tj %>%
  left_join(arm_tj, by = c("bulan","tahun")) %>%
  left_join(pen_lrt, by = c("bulan", "tahun")) %>%
  left_join(perj_lrt, by = c("bulan", "tahun")) %>%
  left_join(pen_mrt, by = c("bulan", "tahun")) %>%
  left_join(perj_mrt, by = c("bulan", "tahun"))

#lihat hasil penggabungan
head(datax) #data 1/2023 sd 5/2024
tail(datax)

#ubah missing values data x jika ada
datax<-datax %>%
  mutate(across(everything(), ~ifelse(is.na(.),0,.)))

#buat fitur datetime
datax$datetime <- as.Date(paste(datax$tahun,datax$bulan,"01",sep = "-"))
head(datax)

#data training
#menyiapkan dataset training dan test
head(train_pen_tj)
tail(train_pen_tj)

#menyiapkan data training & dataset
train_data<-datax %>%
  filter(datetime<as.Date("2024-01-01"))

head(train_data)
tail(train_data)

test_data<-test_data %>%
  left_join(arm_tj,by=c("bulan","tahun")) %>%
  left_join(pen_lrt,by=c("bulan","tahun")) %>%
  left_join(perj_lrt,by=c("bulan","tahun")) %>%
  left_join(pen_mrt,by=c("bulan","tahun")) %>%
  left_join(perj_mrt,by=c("bulan","tahun")) %>%
  mutate(datetime=as.Date(paste(tahun,bulan,"01",sep = "-")))
head(test_data)

#perbaiki dlu nilai missing di test data
test_data<-test_data %>%
  mutate(across(everything(),~ifelse(is.na(.), 0, .)))

#cek nama kolom train & test
colnames(train_data)
head(train_data)
print(names(train_data))

colnames(test_data)
head(test_data)

#pilih fitur x dan target y
features <- c("bulan","tahun","jumlah_armada_tj","jumlah_penumpang_lrt","jumlah_perjalanan_lrt",
              "jumlah_penumpang_mrt","jumlah_perjalanan_mrt")
target <- "jumlah_penumpang"

print(features)
print(target)

#training data model
set.seed(123)
gbm_model<-gbm(
  formula=as.formula(paste(target,"~",paste(features,collapse = "+"))),
  data = train_data,
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 5,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = TRUE
)

#iterasi terbaik dari cross validation
best_iter<-gbm.perf(gbm_model,method = "cv")

#prediksi di test data
predictions<-predict(gbm_model,test_data[features],n.trees = best_iter)

#buat data frame dari predictions
results<-test_data %>%
  mutate(predicted_jumlah_penumpang=predictions) %>%
  select(bulan, tahun, predicted_jumlah_penumpang)

print(results)

#evaluasi model performance dalam training data
train_predictions <- predict(gbm_model,train_data[features],n.trees = best_iter)
train_rmse<-sqrt(mean((train_data[[target]]-train_predictions)^2))
print(paste("Training MSE: ",train_rmse))

#save csv hasil jumlah_penumpang ke samsub
head(samsub)
colnames(samsub)<-c("bulan","jumlah_penumpang")
colnames(samsub)

print(test_data$predicted_jumlah_penumpang)

#gabungkan data2 dengan data1 berdasarkan bulan
samsub$jumlah_penumpang <- results$predicted_jumlah_penumpang

# Tampilkan hasil
print(samsub)

#simpan data frame ke file CSV
colnames(samsub)<-c("id","jumlah_penumpang")
head(samsub)
write.csv(samsub, file = "D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/sample_submision.csv", row.names = FALSE)
samsub<-read.csv("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar2_2 GBM/sample_submision.csv")
