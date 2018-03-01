setwd("F:/INFO7390-ADS/Mid-term_Project/train.csv")

prudentialData <- read.csv(file="train.csv", header=TRUE, sep= ',')



##CATEGORICAL COLUMNS
cat.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
               paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
               "Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""),"Response")

#discrete
disc.names <- c("Id","Medical_History_1", "Medical_History_10", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                paste("Medical_Keyword_", 1:48, sep=""))


#continuous
cont.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                "Family_Hist_5")


prudential.cat <- prudentialData[, cat.names]

prudential.cont <- prudentialData[, cont.names]

prudential.disc <-prudentialData[, disc.names]


#removing columns with most missing values
prudential.disc <-prudential.disc[, -c(3,4,5,6)]

prudential.cont <-prudential.cont[, -c(11,13)]


#removing coulmns with categories more than 6

prudential.cat <-prudential.cat[, -c(2,3,7,12,25)]

# imputing missing values with mean for Continuous variables
prudential.cont$Employment_Info_1[is.na(prudential.cont$Employment_Info_1)] <- mean(prudential.cont$Employment_Info_1, na.rm = TRUE)

prudential.cont$Employment_Info_4[is.na(prudential.cont$Employment_Info_4)] <- mean(prudential.cont$Employment_Info_4, na.rm = TRUE)

prudential.cont$Employment_Info_6[is.na(prudential.cont$Employment_Info_6)] <- mean(prudential.cont$Employment_Info_6, na.rm = TRUE)

prudential.cont$Insurance_History_5[is.na(prudential.cont$Insurance_History_5)] <- mean(prudential.cont$Insurance_History_5, na.rm = TRUE)

prudential.cont$Family_Hist_2[is.na(prudential.cont$Family_Hist_2)] <- mean(prudential.cont$Family_Hist_2, na.rm = TRUE)

prudential.cont$Family_Hist_4[is.na(prudential.cont$Family_Hist_4)] <- mean(prudential.cont$Family_Hist_4, na.rm = TRUE)

# imputing missing values with mean for Discrete variables
prudential.disc$Medical_History_1[is.na(prudential.disc$Medical_History_1)] <- mean(prudential.disc$Medical_History_1, na.rm = TRUE)

#install.packages("ade4")
library(ade4)
df_cat <-acm.disjonctif(prudential.cat[c(1:55)])

response <-prudential.cat[, c(56)]

df_cat1 <- cbind(df_cat,response)

prud <- cbind(prudential.cont,prudential.disc,df_cat1)

write.csv(prud, file = "intermediateData.csv")


df_cat1$response

reg = lm(prud$response~., data = prud)
summary(reg)

prud1 <- prud[, c(1,2,3,4,5,10,11,13,15,16,19,22,25,28,29,31,32,33,35,38,39,40,42,44,46,47,50,51,52,54,56,59,62,64,66,68,71,72,77,81,83,85,87,89,92,99,101,111,113,121,131,134,136,139,146,160,162,171,177,181,189,198,200,202,205,208)]
prud_res=cbind(prud1,response)
reg1 = lm(prud_res$response~., data = prud_res)
summary(reg1)


prud2 <- prud1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,24,25,26,27,28,29,33,35,36,37,41,42,43,44,45,46,47,49,50,53,54,55,57,58,59,62,64,65,66)]
prud_res1=cbind(prud2,response)
reg2 = lm(prud_res1$response~., data = prud_res1)
summary(reg2)

## For reg2 , multiple R squared value is 0.2979

