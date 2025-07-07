
library(tidyverse)
library(ggplot2)    
library(mice)       
library(caret) 
library(dplyr)
library(tidyr)
library(zoo)


data <- read.csv("D:/Dataset(Updated)_MIdterm_sectoin(F).csv", na.strings = c("", "NA"))

str(data)

colSums(is.na(data))
top_bottom_data <- data %>% fill(gender, age, hypertension, smoking_history, bmi, .direction = 'down')
colSums(is.na(top_bottom_data))

colSums(is.na(data))
bottom_top_data <- data %>% fill(gender, age, hypertension, smoking_history, bmi, .direction = 'down')
colSums(is.na(bottom_top_data))

colSums(is.na(data))
data_cleaned <- na.omit(data)
colSums(is.na(data_cleaned))


newdata <- data
colSums(is.na(newdata))

find_mode <- function(x) {
  tbl <- table(x[!is.na(x)])
  names(tbl)[which.max(tbl)]
}

mode_gender <- find_mode(newdata$gender)  #find mode
newdata$gender[is.na(newdata$gender)] <- mode_gender #store mode data in empty value

newdata$gender[newdata$gender == "Femalee"] <- "Female" 
newdata$gender[newdata$gender == "Malee"] <- "Male"

mode_smoking <- find_mode(newdata$smoking_history)
newdata$smoking_history[is.na(newdata$smoking_history)] <- mode_smoking

mode_hypertension <- find_mode(newdata$hypertension)
newdata$hypertension[is.na(newdata$hypertension)] <- mode_hypertension

mode_heart <- find_mode(newdata$heart_disease)
newdata$heart_disease[is.na(newdata$heart_disease)] <- mode_heart

newdata$age <- as.numeric(as.character(newdata$age)) #data type conversion 
newdata$age[newdata$age < 0 | newdata$age > 120] <- NA
mean_age <- mean(newdata$age, na.rm = TRUE)
newdata$age[is.na(newdata$age)] <- round(mean_age)

newdata$bmi <- as.numeric(as.character(newdata$bmi))
newdata$bmi[newdata$bmi < 0] <- NA
mean_bmi <- mean(newdata$bmi, na.rm = TRUE)
newdata$bmi[is.na(newdata$bmi)] <- round(mean_bmi, 2)

mean_hba1c <- mean(newdata$HbA1c_level, na.rm = TRUE)
newdata$HbA1c_level[is.na(newdata$HbA1c_level)] <- round(mean_hba1c, 1)

newdata$blood_glucose_level <- as.numeric(gsub("[^0-9.]", "", as.character(newdata$blood_glucose_level)))
mean_bg <- mean(newdata$blood_glucose_level, na.rm = TRUE)
newdata$blood_glucose_level[is.na(newdata$blood_glucose_level)] <- round(mean_bg)


colSums(is.na(newdata))
newdata

missing_plot <- function(data) {
  missing_data <- data %>% is.na() %>% colSums()
  missing_df <- data.frame(
    Variable = names(missing_data),
    Count = missing_data
  )
  

  ggplot(missing_df, aes(x = reorder(Variable, Count), y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = Count), hjust = -0.2) +
    labs(title = "Missing Values by Variable", 
         x = "Variables", 
         y = "Count of Missing Values") +
    coord_flip() +
    theme_minimal()
}

md.pattern(data, plot = TRUE)
missing_plot(data)


detect_outlier <- function(dataframe, columns) {
for (col in columns) {
      if (is.numeric(dataframe[[col]])){
              Quantilel <- quantile(dataframe[[col]], probs = 0.25)
              Quantile3 <- quantile(dataframe[[col]], probs = 0.75)
              IQR <- Quantile3 - Quantilel
              outlier_flags <- dataframe[[col]] > Quantile3 + (IQR * 1.5) | dataframe[[col]] < Quantilel - (IQR * 1.5)
              outliers <- dataframe[[col]][outlier_flags]
              if (length(outliers) > 8) {
                    cat ("Outliers detected in column", col, "=\n")
                    print(outliers)
              } else {
                cat("No outliers detected in column", col, "\n")
                }
      } else {
          cat("Colunn", col, "is not numeric, skipped\n")
      }
    }
}
remove_outlier <- function (dataframe, columns){
  for(col in columns){
    if(is.numeric(dataframe[[col]])) {
      quantile1 <- quantile(dataframe[[col]], probs = 0.25)
      quantile3 <- quantile(dataframe[[col]], probs = 0.75)
      IQR <- quantile3 - quantile1
      dataframe <- dataframe[!(dataframe[[col]] > quantile3 + (IQR * 1.5) | dataframe[[col]] < quantile3 - (IQR * 1.5)),]
    }
  }
  return(dataframe)
}
detect_outlier(newdata, names(newdata))
without_outlierdata <- remove_outlier(newdata, names(newdata))
detect_outlier(without_outlierdata, names(without_outlierdata))


tempdata <- newdata
tempdata$gender<-factor(tempdata$gender,levels=c("Male","Female"),labels=c(1,2))
tempdata$heart_disease<-factor(tempdata$heart_disease,levels=c(1,0),labels=c("yes","no"))
tempdata


normalizedata <- without_outlierdata
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalizedata$age <- normalize(normalizedata$age)
normalizedata$bmi <- normalize(normalizedata$bmi)
normalizedata$HbA1c_level <- normalize(normalizedata$HbA1c_level)
normalizedata$blood_glucose_level <- normalize(normalizedata$blood_glucose_level)
normalizedata

write.csv(normalizedata, "D:/processed_diabetes_data.csv", row.names = FALSE)


duplicate_data <- newdata
duplicated(duplicate_data)
sum(duplicated(duplicate_data))
duplicate_data <- distinct(duplicate_data)
sum(duplicated(duplicate_data))


filtered_data <- subset(newdata, age > 79)
filtered_data


class_distribution <- table(newdata$diabetes)
print(class_distribution)
if (class_distribution[1] > class_distribution[2]) {
  majority <- filter(newdata, diabetes == 0)
  minority <- filter(newdata, diabetes == 1)
} else {
  majority <- filter(newdata, diabetes == 1)
  minority <- filter(newdata, diabetes == 0)
}

set.seed(123)
oversampled_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
oversampled_data <- bind_rows(majority, oversampled_minority)
table(oversampled_data$diabetes)
oversampled_data


undersampled_majority <- majority %>% sample_n(nrow(minority), replace = FALSE)
undersampled_data <- bind_rows(undersampled_majority, minority)
table(undersampled_data$diabetes)
undersampled_data


set.seed(123) #ki bujhai
split <- sample(1:nrow(newdata_copy), 0.8 * nrow(newdata_copy))
train_data <- newdata_copy[split, ]
test_data <- newdata_copy[-split, ]
train_data
test_data


aggregate(age ~ gender, data = newdata_copy, FUN = function(x) c(mean = mean(x), median = median(x), mode = find_mode(x)))


aggregate(age ~ hypertension, data = newdata_copy, FUN = function(x) c(mean = mean(x), median = median(x), mode = find_mode(x)))


spread_stats <- aggregate(age ~ gender, data = newdata_copy, 
                          FUN = function(x) c(
                            range = max(x) - min(x),
                            IQR = IQR(x),
                            var = var(x),
                            sd = sd(x)
                          ))
spread_stats

bmi_by_diabetes <- aggregate(bmi ~ diabetes, data = newdata_copy, 
                             FUN = function(x) c(mean = mean(x), 
                                                 median = median(x), 
                                                 sd = sd(x),
                                                 min = min(x),
                                                 max = max(x)
                                                 ))
bmi_by_diabetes
