library(dplyr)
library(tidyr)
library(ggplot2)


data_stroke <- read.csv("healthcare-dataset-stroke-data.csv")
data_stroke <- data_stroke %>% select(-id)

#Visualization for preprocessed data
# histogram with a density curve 

ggplot(data_stroke, aes(x = age)) +  
  
  geom_histogram(aes(y = ..count..), binwidth = 5, fill = "blue", color = "black", alpha = 0.5) + 
  
  # for density scaling
  geom_density(aes(y = ..count.. * 5), color = "blue", size = 1) +   
  
  labs(title = "Age Distribution", x = "Age", y = "Count") + 
  
  theme_minimal() 

#Heatmap
install.packages("reshape2")
library(reshape2)
#Select numerical columns for correlation analysis 
numerical_columns <- c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi", "stroke") 
data_numeric <- data_numeric %>% 
  mutate(across(everything(), ~ as.numeric(as.character(.))))

#  Compute the correlation matrix 
cor_matrix <- cor(data_numeric, use = "complete.obs") 

#  Melt the correlation matrix for ggplot 
cor_melt <- melt(cor_matrix) 

#  Plot heatmap 
heatmap <- ggplot(cor_melt, aes(Var1, Var2, fill = value)) + geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs(title = "Correlation Matrix", x = "", y = "") 

# Print heatmap 
print(heatmap) 


#Preprocessing the data
#One-hot encoding the 'smoking_status' column
data_stroke <- data_stroke %>% 
  mutate(smoking_status = gsub(" ","_", smoking_status))%>%
  mutate(across(smoking_status, ~ as.factor(.))) %>%
  pivot_wider(names_from = smoking_status, 
              values_from = smoking_status, 
              values_fn = length, 
              values_fill = 0)

#Removing age values below 0
data_stroke <- data_stroke %>% filter(age > 0)

#Rounding up ages
data_stroke$age <- ceiling(data_stroke$age)

#Creating age ranges
data_stroke <- data_stroke %>%
  mutate(age_range = case_when(
    age <= 10 ~ "0_10",
    age <= 20 ~ "11_20",
    age <= 30 ~ "21_30",
    age <= 40 ~ "31_40",
    age <= 50 ~ "41_50",
    age <= 60 ~ "51_60",
    age <= 70 ~ "61_70",
    age <= 80 ~ "71_80",
    age > 80 ~ "81_and_above"
  ))

#One-hot encoding the 'age_range' column
data_stroke <- data_stroke %>% 
  pivot_wider(names_from = age_range, 
              values_from = age_range, 
              values_fn = length, 
              values_fill = 0)

#One-hot encoding work type
data_stroke<- data_stroke %>%
  mutate(work_type = gsub("-","_", work_type))%>%
  mutate(across(work_type, ~ as.factor(.))) %>%
  pivot_wider(names_from = work_type,
              values_from = work_type,
              values_fn = length,
              values_fill = 0)
#Dropping the row of gender that has "other" assigned, as it is just one value and they've not gotten a stroke
data_stroke <- data_stroke %>% 
  filter(gender != "Other")

#Binary encoding gender. Female = 1, Male = 0
data_stroke<- data_stroke %>%
  mutate(gender = ifelse(gender =="Female",1, 0))

#Binary encoding Married column. Yes = 1, No=0
data_stroke<- data_stroke %>%
  mutate(ever_married = ifelse(ever_married =="Yes",1, 0))

#Binary encoding residence type. Urban= 1, Rural = 0
data_stroke<- data_stroke %>%
  mutate(Residence_type = ifelse(Residence_type =="Urban",1, 0))

#Checking the data type of BMI
str(data_stroke$bmi)

#Converting BMI to numeric type so we can find the mean
data_stroke$bmi <- as.numeric(data_stroke$bmi)

#Finding the mean/ average values of BMI depending on its gender
mean_bmi_gen<-data_stroke %>%
  group_by(gender) %>%
  summarise(mean_bmi = mean(bmi, na.rm = TRUE))

#Replacing the N/A BMI values with the mean values depending on its gender
data_stroke <- data_stroke %>%
  left_join(mean_bmi_gen, by = "gender") %>%  
  mutate(bmi = ifelse(is.na(bmi), mean_bmi, bmi)) %>%  
  select(-mean_bmi)

summary(data_stroke)
View(data_stroke)


#Under-sampling and SMOTE
library(smotefamily)      #For SMOTE
library(ROSE)             #For under sampling
library(caret)            #For data splitting and evaluation

set.seed(42)
index <- createDataPartition(data_stroke$stroke, p=0.7, list=FALSE)      #splitting 70% of data for training
train_data <- data_stroke[index, ]
test_data <- data_stroke[-index, ]

table(train_data$stroke)   #class distribution before sampling

#Applying SMOTE 
data_smote <- SMOTE(
  X = train_data[, -which(names(train_data) == "stroke")], 
  target = train_data$stroke, 
  K = 5, 
  dup_size = 3
)

#Combining synthetic data with the original data
smote_train_data <- data_smote$data
names(smote_train_data)[names(smote_train_data) == "class"] <- "stroke"

#Ensuring proper column names
colnames(smote_train_data) <- make.names(colnames(smote_train_data))

#Appling under sampling 
data_undersampled <- ovun.sample(
  stroke ~ .,
  data = smote_train_data,
  method = "under",
  N = 2 * sum(smote_train_data$stroke == 1)
)$data

#Checking the class distribution after under sampling
table(data_undersampled$stroke)

#Verifying column names of the under sampled data 
colnames(data_undersampled)

#Removing the X prefix from some of the columns of the under sampled data set 
colnames(data_undersampled) <- gsub("\\.", "-", gsub("^X", "", colnames(data_undersampled)))

#Rounding up to ensure binary values (0 or 1) for certain columns
rounding_values <- c(
  "gender", "hypertension","heart_disease","ever_married", "Residence_type","formerly_smoked", "never_smoked", "smokes", "Unknown",
  "61_70", "71_80", "41_50", "81_and_above", "51_60", "31_40",
  "0_10", "11_20", "21_30", "Private", "Self_employed",
  "Govt_job", "children", "Never_worked"
)
data_undersampled <- data_undersampled %>%
  mutate(across(all_of(rounding_values), ~ ifelse(. >= 0.5, 1, 0)))

#Rounding up the avg_glucose_level and BMI to 2 decimal points
data_undersampled <- data_undersampled %>%
  mutate(across(c(avg_glucose_level, bmi),~round(.x,2)))

#Rounding up ages after SMOTE
data_undersampled$age <- ceiling(data_undersampled$age)

#Reordering the data set, placing the age range columns first in ascending order, followed by others
age_order <-c("0_10","11_20","21_30","31_40","41_50","51_60","61_70","71_80","81_and_above") 
data_undersampled <- data_undersampled %>%
  select(all_of(age_order), everything())

cat("\nClass distribution in undersampled data:\n")
table(data_undersampled$stroke)

View(data_undersampled)

#Visualization for processed data
library(ggplot2)

#Visualizing class distribution after balancing data using bar-plot
ggplot(data_stroke, aes(x = factor(stroke))) + geom_bar(fill='skyblue') + 
  labs(title = "Class Distribution Before Balancing", x = "Stroke [0=No, 1=Yes]", y = "Count") +
  theme_minimal()

#Visualizing class distribution after balancing data using bar plot
ggplot(data_undersampled, aes(x=factor(stroke))) + geom_bar(fill='pink') +
  labs(title = "Class Distribution After Balancing", x = "Stroke [0=No, 1= Yes]", y = "Count") +
  theme_minimal()

#Visualizing class distribution before balancing data using pie chart
class_dist_before <- data_stroke %>%
  count(stroke) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(round(percentage, 1), "%"))

ggplot(class_dist_before, aes(x = "", y = percentage, fill = factor(stroke))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +  
  labs(title = "Class Distribution Before Balancing",
       fill = "Stroke [0=No, 1=Yes]") +
  theme_void() +
  theme(legend.position = "right")

#Visualizing class distribution after balancing data using pie chart
class_dist_after <- data_undersampled %>%
  count(stroke) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(round(percentage, 1), "%")) 

ggplot(class_dist_after, aes(x = "", y = percentage, fill = factor(stroke))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +  
  labs(title = "Class Distribution After Balancing",
       fill = "Stroke [0=No, 1=Yes]") +
  theme_void() +
  theme(legend.position = "right")


#Principle Component Analysis
num_columns <- data_undersampled %>% select(age,bmi,avg_glucose_level)
data_scaled <- scale(num_columns)    #scaling data before PCA

pca <- prcomp(data_scaled, center=TRUE, scale.=TRUE)   #performing PCA
summary(pca)


#Visualizing PC1 by stroke status using box plot
pc1 <- pca$x[,1]
boxplot_data <- data.frame(PC1=pc1, stroke=data_undersampled$stroke)

ggplot(boxplot_data, aes(x=factor(stroke), y=PC1)) + geom_boxplot(fill='lavender', color='black')+
  labs(title = "Boxplot of PC1 by Stroke Status", 
       x= "Stroke Status", y= "Principle Component 1") +
  theme_minimal()


#Stats of the box plot
boxplot_stats <- boxplot(PC1 ~ stroke, data = boxplot_data, plot = FALSE)
print(boxplot_stats)


#Converting the under sampled data set to csv file 
write.csv(data_undersampled,'data_stroke_undersampled.csv', row.names = FALSE)





