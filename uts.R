setwd("C:/Users/USER/Documents/R")  
getwd()
#EXPLORING THE DATA
usedata = read.csv("house_price_regression_dataset.csv", sep = ";", header = TRUE)
View(usedata)

#Melihat struktur data
str(usedata)

#Melihat data yang kosong
colSums(is.na(usedata))

#Membersihkan data yang kosong
data1 <- na.omit(usedata)

#Menampilkan data yang baru
View(data1)

#Ringkasan statistik semua variabel
var(data1$Square_Footage)
sd(data1$Square_Footage)
range(data1$Square_Footage)
IQR(data1$Square_Footage)

var(data1$Num_Bedrooms)
sd(data1$Num_Bedrooms)
range(data1$Num_Bedrooms)
IQR(data1$Num_Bedrooms)

var(data1$Num_Bathrooms)
sd(data1$Num_Bathrooms)
range(data1$Num_Bathrooms)
IQR(data1$Num_Bathrooms)

var(data1$Year_Built)
sd(data1$Year_Built)
range(data1$Year_Built)
IQR(data1$Year_Built)

var(data1$Lot_Size)
sd(data1$Lot_Size)
range(data1$Lot_Size)
IQR(data1$Lot_Size)

var(data1$Garage_Size)
sd(data1$Garage_Size)
range(data1$Garage_Size)
IQR(data1$Garage_Size)

var(data1$Neighborhood_Quality)
sd(data1$Neighborhood_Quality)
range(data1$Neighborhood_Quality)
IQR(data1$Neighborhood_Quality)

var(data1$House_Price)
sd(data1$House_Price)
range(data1$House_Price)
IQR(data1$House_Price)

#Scatterplot semua variabel terhadap House_Price
library(ggplot2)

ggplot(data1, aes(Square_Footage, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(data1, aes(Num_Bedrooms, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(data1, aes(Num_Bathrooms, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(data1, aes(Year_Built, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(data1, aes(Lot_Size, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(data1, aes(Garage_Size, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(data1, aes(Neighborhood_Quality, House_Price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

#BUILDING FORMAL STATISTICAL MODELS
model <- lm(House_Price ~ Square_Footage + Num_Bedrooms + Num_Bathrooms +
              Year_Built + Lot_Size + Garage_Size + Neighborhood_Quality,
            data = data1)
summary(model)

#Korelasi seluruh variabel numerik (alternatif cor())
cor(data1[, c("Square_Footage", "Num_Bedrooms", "Num_Bathrooms",
              "Year_Built", "Lot_Size", "Garage_Size",
              "Neighborhood_Quality", "House_Price")])