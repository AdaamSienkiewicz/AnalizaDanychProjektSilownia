library(ggplot2)
library(readr)
library(naniar)
library(visdat)
library(dplyr)

silownia <- read_csv("silownia_new.csv")

View(silownia)

#Zmiana nazw kolumn
colnames(silownia)[colnames(silownia) == "Weight (kg)"] <- "Weight_kg"
colnames(silownia)[colnames(silownia) == "Height (m)"] <- "Height_m"
colnames(silownia)[colnames(silownia) == "Session_Duration (hours)"] <- "Session_Duration_hours"
colnames(silownia)[colnames(silownia) == "Water_Intake (liters)"] <- "Water_Intake_liters"
colnames(silownia)[colnames(silownia) == "Workout_Frequency (days/week)"] <- "Workout_Frequency_daysweek"


## Odstające obserwacje
# Z-score
z_score_Age <- (na.omit(silownia$Age) - mean(na.omit(silownia$Age))) / sd(na.omit(silownia$Age))
z_score_Age
odstajace_Age <- sum(na.omit(abs(z_score_Age)) > 3)
odstajace_Age
# Dla zmiennej Age nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Weight <- (na.omit(silownia$Weight_kg) - mean(na.omit(silownia$Weight_kg))) / sd(na.omit(silownia$Weight_kg))
z_score_Weight
odstajace_Weight <- sum(na.omit(abs(z_score_Weight)) > 3)
odstajace_Weight
# Dla zmiennej Weight nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Height <- (na.omit(silownia$Height_m) - mean(na.omit(silownia$Height_m))) / sd(na.omit(silownia$Height_m))
z_score_Height
odstajace_Height <- sum(na.omit(abs(z_score_Height)) > 3)
odstajace_Height
# Dla zmiennej Height nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Max_BPM <- (na.omit(silownia$Max_BPM) - mean(na.omit(silownia$Max_BPM))) / sd(na.omit(silownia$Max_BPM))
z_score_Max_BPM
odstajace_Max_BPM <- sum(na.omit(abs(z_score_Max_BPM)) > 3)
odstajace_Max_BPM
# Dla zmiennej Max_BPM nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Avg_BPM <- (na.omit(silownia$Avg_BPM) - mean(na.omit(silownia$Avg_BPM))) / sd(na.omit(silownia$Avg_BPM))
z_score_Avg_BPM
odstajace_Avg_BPM <- sum(na.omit(abs(z_score_Avg_BPM)) > 3)
odstajace_Avg_BPM
# Dla zmiennej Avg_BPM nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Resting_BPM <- (na.omit(silownia$Resting_BPM) - mean(na.omit(silownia$Resting_BPM))) / sd(na.omit(silownia$Resting_BPM))
z_score_Resting_BPM
odstajace_Resting_BPM <- sum(na.omit(abs(z_score_Resting_BPM)) > 3)
odstajace_Resting_BPM
# Dla zmiennej Resting_BPM nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_SessDur <- (na.omit(silownia$Session_Duration_hours) - mean(na.omit(silownia$Session_Duration_hours))) / sd(na.omit(silownia$Session_Duration_hours))
z_score_SessDur
odstajace_SessDur <- sum(na.omit(abs(z_score_SessDur)) > 3)
odstajace_SessDur
# Dla zmiennej Session_Duration_hours nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Calories_Burned <- (na.omit(silownia$Calories_Burned) - mean(na.omit(silownia$Calories_Burned))) / sd(na.omit(silownia$Calories_Burned))
z_score_Calories_Burned
odstajace_Calories_Burned <- sum(na.omit(abs(z_score_Calories_Burned)) > 3)
odstajace_Calories_Burned
# Dla zmiennej Calories_Burned są 3 obserwacje, które różnią się o co najmniej 3 odchylenia od średniej, tj. są one obserwacjami odstającymi.

z_score_FatPer <- (na.omit(silownia$Fat_Percentage) - mean(na.omit(silownia$Fat_Percentage))) / sd(na.omit(silownia$Fat_Percentage))
z_score_FatPer
odstajace_FatPer <- sum(na.omit(abs(z_score_FatPer)) > 3)
odstajace_FatPer
# Dla zmiennej Fat_Percentage nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Water <- (na.omit(silownia$Water_Intake_liters) - mean(na.omit(silownia$Water_Intake_liters))) / sd(na.omit(silownia$Water_Intake_liters))
z_score_Water
odstajace_Water <- sum(na.omit(abs(z_score_Water)) > 3)
odstajace_Water
# Dla zmiennej Water_Intake_liters nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_WorkoutFreq <- (na.omit(silownia$Workout_Frequency_daysweek) - mean(na.omit(silownia$Workout_Frequency_daysweek))) / sd(na.omit(silownia$Workout_Frequency_daysweek))
z_score_WorkoutFreq
odstajace_WorkoutFreq <- sum(na.omit(abs(z_score_WorkoutFreq)) > 3)
odstajace_WorkoutFreq
# Dla zmiennej Workout_Frequency_daysweek nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_Experience <- (na.omit(silownia$Experience_Level) - mean(na.omit(silownia$Experience_Level))) / sd(na.omit(silownia$Experience_Level))
z_score_Experience
odstajace_Experience <- sum(na.omit(abs(z_score_Experience)) > 3)
odstajace_Experience
# Dla zmiennej Experience_level nie ma obserwacji, które różnią się o 3 odchylenia od średniej, tj. nie ma obserwacji odstających.

z_score_BMI <- (na.omit(silownia$BMI) - mean(na.omit(silownia$BMI))) / sd(na.omit(silownia$BMI))
z_score_BMI
odstajace_BMI <- sum(na.omit(abs(z_score_BMI)) > 3)
odstajace_BMI
# Dla zmiennej BMI jest 10 obserwacji, które różnią się o co najmniej 3 odchylenia od średniej, tj. są obserwacjami odstającymi.

#Dla zmiennej BMI jest 10 obserwacji odstających oraz dla zmiennej Calories_burned są 3 obserwacje odstające

#ANALIZA BRAKÓW DANYCH
vis_miss(silownia)
vis_dat(silownia)
gg_miss_var(silownia)

#Braki danych występują w kolumnie Workout_Type, Age oraz BMI
#Kolumna Age oraz BMI posiadają wartości liczbowe, a kolumna Workout_Type to ciąg znaków (wyraz)
#W Age braki stanowią 10%, w Workout_Type 15%, a w BMI 15 %

kolumny_z_NA <- silownia %>% 
                  select(Age, Workout_Type, BMI)

miss_case_table(silownia)
gg_miss_upset(silownia, nsets= 3)

# 631 wierszy ma 0 NA
# 287 wierszy ma 1 NA (115 wierszy w Workout_Type; 106 w BMI; 66 w Age)
# 52 wiersze ma 2 NA (21 wierszy mialo NA w Workout_Type i BMI; 20 wierszy w Age i BMI;
# 11 w Age i Workout_type)
# 3 wiersze maja 3 NA

ggplot(data = silownia, aes(x = Age, y = BMI)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

#Age i BMI - braki w kolumnie BMI były kompletnie niezalzne od kolumny Age; 
# braki w kolumnie Age raczej nie były zależne od kolumny BMI, jedynie troche wiecej
# brakow wystepowalo w nizszych wartsociach BMI

ggplot(data = kolumny_z_NA, aes(x = Workout_Type, y = BMI)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

#Workout_Type i BMI - braki w kolumnie BMI były kompletnie niezalzne od kolumny Workout_Type; 
# braki w kolumnie Workout_Type raczej nie były zależne od kolumny BMI, jedynie troche wiecej
# brakow wystepowalo w nizszych wartsociach BMI

ggplot(data = kolumny_z_NA, aes(x = Workout_Type, y = Age)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

#Workout_Type i Age - braki w obu kolumnach są kompletnie niezależne od siebie

# Zastąpienie braków NA w kolumnie BMI za pomocą wzoru na BMI - i te braki zastapimy wlasnie tym wzorem

silownia$BMI <- ifelse(
  is.na(silownia$BMI),  
  silownia$Weight_kg / (silownia$Height_m^2),  
  silownia$BMI )

# Imputacja metodą k-Nearest Neighbors (kNN) - poprawka
library(VIM)
dane_imputowane <- kNN(silownia, k = 3) # Imputacja z użyciem 3 najbliższych sąsiadów
print(dane_imputowane)
str(dane_imputowane)

vis_miss(dane_imputowane)
vis_dat(dane_imputowane)
gg_miss_var(dane_imputowane)

# IMPUTACJA Z PAKIETEM MICE
library(mice)

if (!is.factor(silownia$Workout_Type)) {
  silownia$Workout_Type <- factor(silownia$Workout_Type, levels = c("Yoga", "Cardio", "HIIT", "Strength"))
}

# Tworzenie wektora metod imputacji
metody <- make.method(silownia)

# Ustawienie metod imputacji tylko dla Age i Workout_Type
metody["Age"] <- "pmm"
metody["Workout_Type"] <- "polyreg"
metody["BMI"] <- "" 

# Tworzenie macierzy predyktorów
pred_mat <- make.predictorMatrix(silownia)
pred_mat["BMI", ] <- 0 # Wyłącz imputację dla BMI
pred_mat[, "BMI"] <- 1 # BMI jako predyktor dla innych kolumn

# Przeprowadzenie imputacji
silownia_imp <- mice(silownia, m = 5, method = metody, predictorMatrix = pred_mat, seed = 123)


lm_imp <- with(silownia_imp, lm(BMI ~ Weight_kg + Gender))
lm_pooled <- pool(lm_imp)

summary(lm_pooled, conf.int = TRUE, conf.level = 0.95)


stripplot(silownia_imp, BMI ~ Weight_kg | .imp, pch = 20, cex = 2)


silownia_mice <- complete(silownia_imp, action = 1)

#Imputacja z pakietem RPART

library(rpart)

silownia_rpart  <- silownia  %>%
  mutate(Workout_Type = case_when(
    Workout_Type == "Yoga" ~ 1,
    Workout_Type == "Cardio" ~ 2,
    Workout_Type == "HIIT" ~ 3,
    Workout_Type == "Strength" ~ 4,
    TRUE ~ as.numeric(Workout_Type) 
  ))

silownia_rpart <- silownia_rpart %>%
  mutate(Gender = case_when(
    Gender == "Male" ~ 1,
    Gender == "Female" ~ 2,
    TRUE ~ as.numeric(Gender) 
  ))


drzewo_decyzyjne1 <- rpart(Workout_Type ~ Age + BMI + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Gender + Experience_Level, data = silownia_rpart, method = "anova", na.action = na.exclude)

silownia_rpart$Workout_Type[is.na(silownia_rpart$Workout_Type)] <- predict(drzewo_decyzyjne1, newdata = silownia_rpart[is.na(silownia_rpart$Workout_Type), ])


drzewo_decyzyjne2 <- rpart(Age ~ BMI + Workout_Type + Gender + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Experience_Level, data = silownia_rpart, method = "anova", na.action = na.exclude)
silownia_rpart$Age[is.na(silownia_rpart$Age)] <- predict(drzewo_decyzyjne2, newdata = silownia_rpart[is.na(silownia_rpart$Age), ])

print(silownia_rpart)



#Imputacja hot-deck
dane_imputowanehotdeck <- hotdeck(silownia)
print(dane_imputowanehotdeck)


#WYBRALISMY METODE ...

#walidacja danych
packages <- c(
  "dplyr", "ggplot2", "rmdformats", "validate", "validatetools", 
  "dcmodify", "errorlocate", "deductive", "VIM", "simputation", 
  "lumberjack", "ISLR", "dlookr", "xts", "quantmod", "ROCR", 
  "DMwR", "Information", "scorecard"
)

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) { 
      install.packages(pkg)                    
      library(pkg, character.only = TRUE)      
    } else {
      library(pkg, character.only = TRUE)      
    }
  }
}
install_if_missing(packages)

library(dplyr)
library(ggplot2)
library(rmdformats)
library(validate)
library(validatetools)
library(dcmodify)
library(errorlocate)
library(deductive)
library(VIM)
library(simputation)
library(lumberjack)
library(ISLR) 
library(dlookr)

library(xts)
library(quantmod)
library(ROCR)
library(DMwR)
library(Information)
library(scorecard)

#walidacja Weight_kg

min_value <- min(silownia$`Weight_kg`, na.rm = TRUE)
max_value <- max(silownia$`Weight_kg`, na.rm = TRUE)

rules_Weight_kg <- validator(
  `Weight_kg` > 0,            
  `Weight_kg` >= 40,           
  `Weight_kg` <= 140,
  is.numeric(`Weight_kg`) == TRUE
)

cf <- confront(silownia, rules_Weight_kg, key="Weight_kg")
summary(cf)
barplot(cf, main="silownia")

#Height_m

min_value <- min(silownia$`Height_m`, na.rm = TRUE)
max_value <- max(silownia$`Height_m`, na.rm = TRUE)

rules_Height_m <- validator(
  `Height_m` > 0,            
  `Height_m` >= 1.30,           
  `Height_m` <= 2.15,
  is.numeric(`Height_m`) == TRUE
)

cf <- confront(silownia, rules_Height_m, key="Height_m")
summary(cf)
barplot(cf, main="silownia")

#Age

rules_Age <- validator(
  `Age` >= 0,            
  `Age` <= 110,           
  `Age` == floor(Age),
  is.numeric(`Age`) == TRUE
)

cf <- confront(silownia, rules_Age, key="Age")
summary(cf)
barplot(cf, main="silownia")

#Max_BPM - na podstawie wzoru Max BPM = 220 − Age

rules_Max_BPM <- validator(
  `Max_BPM` > 0, 
  if (Age >= 18 & Age <= 29) `Max_BPM` >= 100 & `Max_BPM` <= 200,
  if (Age >= 30 & Age <= 39) `Max_BPM` >= 95 & `Max_BPM` <= 190,
  if (Age >= 40 & Age <= 49) `Max_BPM` >= 90 & `Max_BPM` <= 180,
  if (Age >= 50 & Age <= 59) `Max_BPM` >= 85 & `Max_BPM` <= 170,
  if (Age >= 60) `Max_BPM` >= 80 & `Max_BPM` <= 160
)

cf <- confront(silownia, rules_Max_BPM, key="Max_BPM")
summary(cf)
barplot(cf, main="silownia")

#Zamiana błędnych pól na NA w Max_BPM

silownia <- replace_errors(silownia, rules_Max_BPM)
sum(is.na(silownia))

#I tu trzeba znów przeprowadzić imputację 

#Avg_BPM

rules_Avg_BPM <- validator(
  `Avg_BPM` > 0,
  if (Age >= 18 & Age <= 29) `Avg_BPM` >= 60 & `Avg_BPM` <= 120,
  if (Age >= 30 & Age <= 39) `Avg_BPM` >= 60 & `Avg_BPM` <= 115,
  if (Age >= 40 & Age <= 49) `Avg_BPM` >= 60 & `Avg_BPM` <= 110,
  if (Age >= 50 & Age <= 59) `Avg_BPM` >= 55 & `Avg_BPM` <= 105,
  if (Age >= 60) `Avg_BPM` >= 50 & `Avg_BPM` <= 100
)

cf <- confront(silownia, rules_Avg_BPM, key="Avg_BPM")
summary(cf)
barplot(cf, main="silownia")

#Zamiana błędnych pól na NA w Max_BPM

silownia <- replace_errors(silownia, rules_Avg_BPM)
sum(is.na(silownia))

#I tu ponownie trzeba znów przeprowadzić imputację 

#Resting_BPM

rules_Resting_BPM <- validator(
  `Resting_BPM` > 0,
  if (Age >= 18 & Age <= 29) `Resting_BPM` >= 50 & `Resting_BPM` <= 85,
  if (Age >= 30 & Age <= 39) `Resting_BPM` >= 55 & `Resting_BPM` <= 85,
  if (Age >= 40 & Age <= 49) `Resting_BPM` >= 55 & `Resting_BPM` <= 90,
  if (Age >= 50 & Age <= 59) `Resting_BPM` >= 60 & `Resting_BPM` <= 90,
  if (Age >= 60) `Resting_BPM` >= 60 & `Resting_BPM` <= 95
)

cf <- confront(silownia, rules_Resting_BPM, key="Resting_BPM")
summary(cf)
barplot(cf, main="silownia")

#Zamiana błędnych pól na NA w Resting_BPM

silownia <- replace_errors(silownia, rules_Resting_BPM)
sum(is.na(silownia))

#Imputacja 

#Fat_Percentage

rules_Fat_Percentage <- validator(
  `Fat_Percentage` > 0, 
  `Fat_Percentage` <= 50,
  if (Gender == "Male" & Age >= 18 & Age <= 39) `Fat_Percentage` >= 8 & `Fat_Percentage` <= 20,
  if (Gender == "Male" & Age >= 40 & Age <= 59) `Fat_Percentage` >= 11 & `Fat_Percentage` <= 22,
  if (Gender == "Male" & Age >= 60) `Fat_Percentage` >= 13 & `Fat_Percentage` <= 25,
  if (Gender == "Female" & Age >= 18 & Age <= 39) `Fat_Percentage` >= 21 & `Fat_Percentage` <= 33,
  if (Gender == "Female" & Age >= 40 & Age <= 59) `Fat_Percentage` >= 23 & `Fat_Percentage` <= 35,
  if (Gender == "Female" & Age >= 60) `Fat_Percentage` >= 24 & `Fat_Percentage` <= 36
)

cf <- confront(silownia, rules_Fat_Percentage, key="Fat_Percentage")
summary(cf)
barplot(cf, main="silownia")

#Zamiana błędnych pól na NA w Fat_Percentage

silownia <- replace_errors(silownia, rules_Fat_Percentage)
sum(is.na(silownia))

#Imputacja 

#BMI

rules_BMI <- validator(
  `BMI` > 0,                      
  `BMI` >= 10,
  `BMI` <= 60, #Zakres BMI
  abs(`BMI` - (`Weight_kg` / (`Height_m`^2))) < 0.1 # Spójność z masą i wzrostem(ze względu na wzór)
)

cf <- confront(silownia, rules_BMI, key="BMI")
summary(cf)
barplot(cf, main="silownia")

#Workout_Frequency_daysweek

rules_Workout_Frequency_daysweek <- validator(
  workout_days_week >= 0,
  workout_days_week <= 7  
)

cf <- confront(silownia, rules_Workout_Frequency_daysweek, key="Workout_Frequency_daysweek")
summary(cf)
barplot(cf, main="silownia")
