library(tidyr)
library(stringr)
library(stringi)
library(dplyr)

# ------------------------------------------------------------------------------------------------------------------

# 3 Tien du li so lieu 

# 3.1 Doc du lieu tu file .csv

setwd("C:/codeR")
df <- read.csv("All_GPUs.csv")
View(df)

# Chon cac bien de hien thi

df <- df[ , c("Name", "Max_Power", "Memory_Bandwidth", "Memory_Speed", "Pixel_Rate", "Texture_Rate","Core_Speed")]
View(df)

# 3.2 Tien xu li du lieu

str(df)

# Tach du lieu thanh 2 phan: Gia tri va Don vi

# Bien Max_Power
df$Max_Power_Nums <- as.numeric(str_extract(df$Max_Power ,"\\d+\\.?\\d*"))
df$Max_Power_Units <- str_replace(df$Max_Power, "\\d+\\.?\\d*", "")
print(table(df$Max_Power_Units))

# Bien Memory_Bandwidth
df$Memory_Bandwidth_Nums <- as.numeric(str_extract(df$Memory_Bandwidth, "\\d+\\.?\\d*"))
df$Memory_Bandwidth_Units <- str_replace(df$Memory_Bandwidth, "\\d+\\.?\\d*", "")
print(table(df$Memory_Bandwidth_Units))

# Bien Memory_Speed
df$Memory_Speed_Nums <- as.numeric(str_extract(df$Memory_Speed, "\\d+\\.?\\d*"))
df$Memory_Speed_Units <- str_replace(df$Memory_Speed, "\\d+\\.?\\d*", "")
print(table(df$Memory_Speed_Units))

# Bien Pixel_Rate
df$Pixel_Rate_Nums <- as.numeric(str_extract(df$Pixel_Rate, "\\d+\\.?\\d*"))
df$Pixel_Rate_Units <- str_replace(df$Pixel_Rate, "\\d+\\.?\\d*", "")
print(table(df$Pixel_Rate_Units))

# Bien Texture_Rate
df$Texture_Rate_Nums <- as.numeric(str_extract(df$Texture_Rate, "\\d+\\.?\\d*"))
df$Texture_Rate_Units <- str_replace(df$Texture_Rate, "\\d+\\.?\\d*", "")
print(table(df$Texture_Rate_Units))

# Đong nhat don vi cua Memory_Bandwidth (MB/sec ->GB/sec)
# Kiem tra xem mau du lieu nao co don vi MB/sec
if ("MB/sec" %in% df$Memory_Bandwidth_Units) {
  # Chuyen doi tu MB/sec thanh GB/sec
  mb_indices <- df$Memory_Bandwidth_Units == "MB/sec"
  df$Memory_Bandwidth_Nums[mb_indices] <- df$Memory_Bandwidth_Nums[mb_indices] / 1024
  df$Memory_Bandwidth_Units[mb_indices] <- "GB/sec"
}
print(table(df$Memory_Bandwidth_Units))

# Bien Core_Speed
df$Core_Speed_Nums <- as.numeric(str_extract(df$Core_Speed ,"\\d+\\.?\\d*"))
df$Core_Speed_Units <- str_replace(df$Core_Speed, "\\d+\\.?\\d*", "")
print(table(df$Max_Power_Units))

head(df)

# 3.3 Xu li du lieu khiem khuyet

# Tao dataframe "dfNums" tu cac cot co gia tri so trong dataframe "df"
dfNums <- data.frame(Max_Power = df$Max_Power_Nums, 
                     Memory_Bandwidth = df$Memory_Bandwidth_Nums, 
                     Memory_Speed = df$Memory_Speed_Nums, 
                     Pixel_Rate = df$Pixel_Rate_Nums, 
                     Texture_Rate = df$Texture_Rate_Nums, 
                     Core_Speed = df$Core_Speed_Nums)
colnames(dfNums) <- c("Max_Power", "Memory_Bandwidth", "Memory_Speed", "Pixel_Rate", "Texture_Rate", "Core_Speed")
head(dfNums) 

# In so luong du lieu khiem khuyet trong moi cot 
print((colSums(is.na(dfNums))))

# In ti le du lieu khiem khuyet trong moi cot
print((colMeans(is.na(dfNums))))

# In cac thong so tong quan du lieu cua moi cot
print(summary(dfNums))

# Xu li du lieu

# Luoc bo cac hang co du lieu gia tri bang 0
df <- subset(df, !apply(df == 0, 1, any))

# Kiem tra kich thuoc cua dataframe sau khi xu li 
dim(df)

# Xu li du lieu khiem khuyet bang gia tri trung vi (median)
df$Max_Power_Nums[is.na(df$Max_Power_Nums)] <- median(df$Max_Power_Nums, na.rm = TRUE)
df$Memory_Bandwidth_Nums[is.na(df$Memory_Bandwidth_Nums)] <- median(df$Memory_Bandwidth_Nums, na.rm = TRUE)
df$Memory_Speed_Nums[is.na(df$Memory_Speed_Nums)] <- median(df$Memory_Speed_Nums, na.rm = TRUE)
df$Pixel_Rate_Nums[is.na(df$Pixel_Rate_Nums)] <- median(df$Pixel_Rate_Nums, na.rm = TRUE)
df$Texture_Rate_Nums[is.na(df$Texture_Rate_Nums)] <- median(df$Texture_Rate_Nums, na.rm = TRUE)
df$Core_Speed_Nums[is.na(df$Core_Speed_Nums)] <- median(df$Core_Speed_Nums, na.rm = TRUE)

# Cap nhat dataframe "dfNums" de su dung cho qua trinh thong ke mo ta
dfNums <- data.frame(df$Max_Power_Nums, df$Memory_Bandwidth_Nums, df$Memory_Speed_Nums, df$Pixel_Rate_Nums, df$Texture_Rate_Nums, df$Core_Speed_Nums)
head(dfNums)

# Chuyen doi cac bien sang dang logarit de han che sai so trong mo hinh

# Tao dataframe "dfNums2" tu cac cot co gia tri so trong dataframe "dfNums"
dfNums2 <- dfNums
View(dfNums2) 

# Chuyen du lieu tu cac cot thanh logarit
columns_to_log <- c("df.Max_Power_Nums", "df.Memory_Bandwidth_Nums", "df.Memory_Speed_Nums", 
                    "df.Pixel_Rate_Nums", "df.Texture_Rate_Nums", "df.Core_Speed_Nums")
dfNums2[, columns_to_log] <- log(dfNums2[, columns_to_log])

# Hien thi phan dau cua bo du lieu da xu li sang dan logarit
head(dfNums2)

# ------------------------------------------------------------------------------------------------------------------

# 4. Thong ke mo ta

# 4.1 Thong tin tong quat ve cac bien trong bo du lieu phan tich
print(summary(dfNums))

# 4.2 Cac bieu do - do thi

# Bieu do tan suat (histogram)
hist(dfNums$df.Max_Power_Nums, xlab = df$Max_Power_Units, main = 'Histogram of Max Power')
hist(dfNums$df.Memory_Bandwidth_Nums, xlab = df$Memory_Bandwidth_Units, main = 'Histogram of Memory Bandwidth')
hist(dfNums$df.Memory_Speed_Nums, xlab = df$Memory_Speed_Units, main = 'Histogram of Memory Speed')
hist(dfNums$df.Pixel_Rate_Nums, xlab = df$Pixel_Rate_Units, main = 'Histogram of Pixel Rate')
hist(dfNums$df.Texture_Rate_Nums, xlab = df$Texture_Rate_Units, main = 'Histogram of Texture Rate')
hist(dfNums$df.Core_Speed_Nums, xlab = 'Core Speed', main = 'Histogram of Core Speed')

# Bieu do phan tan (scatter plot)
pairs(dfNums, main = 'Scatterplot of data')

# Tinh ma tran tuong quan cho cac cot du lieu trong mau du lieu phan tich
print(cor(dfNums))

# ------------------------------------------------------------------------------------------------------------------

# 5. Thong ke suy dien

# Tao cac mo hinh hoi quy tuyen tinh voi bien phu thuoc Core_Speed, su dung data frame dfNums2 

  # Mo hinh 1 - Bien doc lap: Memory_Speed, Pixel_Rate, Texture_Rate, Release_Date
  Model1 <- lm(df.Core_Speed_Nums ~ df.Memory_Speed_Nums + df.Pixel_Rate_Nums + df.Texture_Rate_Nums, data = dfNums2)
  summary(Model1)
  
  # Mo hinh 2 - Bien doc lap: Max_Power, Memory_Speed, Texture_Rate, Release_Date
  Model2 <- lm(df.Core_Speed_Nums ~ df.Memory_Bandwidth_Nums + df.Memory_Speed_Nums + df.Pixel_Rate_Nums + df.Texture_Rate_Nums, data = dfNums2)
  summary(Model2)
  
  # Mo hinh 3 - Bien doc lap: Max_Power, Memory_Speed, Pixel_Rate, Texture_Rate, Release_Date
  Model3 <- lm(df.Core_Speed_Nums ~ df.Max_Power_Nums + df.Memory_Bandwidth_Nums + df.Memory_Speed_Nums + df.Pixel_Rate_Nums + df.Texture_Rate_Nums, data = dfNums2)
  summary(Model3)

# So sanh cac mo hinh hoi quy 
  
  # Anova
  anova(Model1,Model2)
  anova(Model2,Model3)
  
  # Do thi bieu thi sai so hoi quy residuals va gia tri du bao fitted values
  par(mfrow =c(2,2))
  plot(Model3)
  par(mfrow = c(1,1))

# Du doan (predict)

  # Cac bien trong du lieu
  Core_Speed=dfNums2$df.Core_Speed_Nums
  Memory_Bandwidth=dfNums2$df.Memory_Bandwidth_Nums
  Max_Power=dfNums2$df.Max_Power_Nums
  Memory_Speed=dfNums2$df.Memory_Speed_Nums
  Pixel_Rate=dfNums2$df.Pixel_Rate_Nums
  Texture_Rate=dfNums2$df.Texture_Rate_Nums

  
  predict_data <- data.frame(Max_Power,Memory_Bandwidth,Memory_Speed,Pixel_Rate,Texture_Rate)
  P<-lm(Core_Speed~.,data=predict_data)
  summary(P)
  
  # So sanh ket qua du doan va du lieu
  predict_cs = predict(P)
  p=data.frame(predict_cs,Core_Speed)
  p
  
  # Ve bieu do du doan
  x<- (1:100)
  y<-x
  kql = lm(y~x)
  par(mfrow=c(1,1))
  plot(p,xlab="predicted values",ylab="observed values",main=" Plot to predict Core Speed")
  abline(kql,col="red")
  
  # Kiem tra du doan
  new <- dfNums2[,c ("df.Max_Power_Nums", "df.Memory_Bandwidth_Nums", "df.Memory_Speed_Nums", "df.Pixel_Rate_Nums", "df.Texture_Rate_Nums" )]
  result <- predict(Model3, newdata = new, interval = "confidence")
  newDf <- data.frame(ifelse(abs(result[,"fit"]-dfNums2[,"df.Core_Speed_Nums"]) >= 0.5, FALSE, TRUE))
  colnames(newDf) <- "Good predict"
  apply(newDf,2,mean)