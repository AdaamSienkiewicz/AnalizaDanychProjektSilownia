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
silownia_kNN <- kNN(silownia, k = 3) # Imputacja z użyciem 3 najbliższych sąsiadów
print(silownia_kNN)
str(silownia_kNN)

vis_miss(silownia_kNN)
vis_dat(silownia_kNN)
gg_miss_var(silownia_kNN)

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
silownia_hotdeck <- hotdeck(silownia)
print(silownia_hotdeck)


#WYBRALISMY METODE HOTDECK do dalszej analizy
silownia_brudne_dane <- silownia_hotdeck
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

#walidacja 

#Age

rules_Age <- validator(
  `Age` >= 0,            
  `Age` <= 110,           
  `Age` == floor(Age),
  is.numeric(`Age`) == TRUE
)

cf1 <- confront(silownia_brudne_dane, rules_Age, key="Age")
summary(cf1)
barplot(cf1, main="silownia")

#Gender
rules_Gender <- validator(Gender %in% c("Female", "Male"))


cf2 <- confront(silownia_brudne_dane, rules_Gender, key="Gender")
summary(cf2)
barplot(cf2, main="silownia")


#Weight

rules_Weight_kg <- validator(
  `Weight_kg` > 0,            
  `Weight_kg` >= 40,           
  `Weight_kg` <= 140,
  is.numeric(`Weight_kg`) == TRUE
)

cf3 <- confront(silownia_brudne_dane, rules_Weight_kg, key="Weight_kg")
summary(cf3)
barplot(cf3, main="silownia")

#Height_m

rules_Height_m <- validator(
  `Height_m` > 0,            
  `Height_m` >= 1.30,           
  `Height_m` <= 2.15,
  is.numeric(`Height_m`) == TRUE
)

cf4 <- confront(silownia_brudne_dane, rules_Height_m, key="Height_m")
summary(cf4)
barplot(cf4, main="silownia")

#Max_BPM

rules_Max_BPM <- validator(
 `Max_BPM` >=110,
 `Max_BPM` <=210,
 is.numeric(`Max_BPM`) == TRUE
)

cf5 <- confront(silownia_brudne_dane, rules_Max_BPM, key="Max_BPM")
summary(cf5)
barplot(cf5, main="silownia")


#Avg_BPM

rules_Avg_BPM <- validator(
  `Avg_BPM` >=80,
  `Avg_BPM` <=180,
  is.numeric(`Avg_BPM`) == TRUE
)

cf6 <- confront(silownia_brudne_dane, rules_Avg_BPM, key="Avg_BPM")
summary(cf6)
barplot(cf6, main="silownia")

#Resting_BPM

rules_Resting_BPM <- validator(
  `Resting_BPM` >= 45,
  `Resting_BPM` <=130,
  is.numeric(`Resting_BPM`) == TRUE
)

cf7 <- confront(silownia_brudne_dane, rules_Resting_BPM, key="Resting_BPM")
summary(cf7)
barplot(cf7, main="silownia")

#Session_Duration_hours

rules_Session_Duration_hours <- validator(
  `Session_Duration_hours` > 0, 
  `Session_Duration_hours` <= 2,
  is.numeric(`Session_Duration_hours`) == TRUE
)

cf8 <- confront(silownia_brudne_dane, rules_Session_Duration_hours, key="Session_Duration_hours")
summary(cf8)
barplot(cf8, main="silownia")

#Calories_burned

rules_Calories_Burned <- validator(
  `Calories_Burned` > 0, 
  `Calories_Burned` <= 2000,
  is.numeric(`Calories_Burned`) == TRUE
)

cf9 <- confront(silownia_brudne_dane, rules_Calories_Burned, key="Calories_Burned")
summary(cf9)
barplot(cf9, main="silownia")

#Workout_Type

rules_Workout_Type <- validator(Workout_Type %in% c("Yoga", "Cardio", "HIIT","Strength"))


cf10 <- confront(silownia_brudne_dane, rules_Workout_Type, key="Workout_Type")
summary(cf10)
barplot(cf10, main="silownia")


#Fat_Percentage

rules_Fat_Percentage <- validator(
  `Fat_Percentage` > 0, 
  `Fat_Percentage` <= 50,
  is.numeric(`Fat_Percentage`) == TRUE
)

cf11 <- confront(silownia_brudne_dane, rules_Fat_Percentage, key="Fat_Percentage")
summary(cf11)
barplot(cf11, main="silownia")

#Water_Intake_litres

rules_Water_Intake_liters <- validator(
  `Water_Intake_liters` > 0, 
  `Water_Intake_liters` <= 4,
  is.numeric(`Water_Intake_liters`) == TRUE
)

cf12 <- confront(silownia_brudne_dane, rules_Water_Intake_liters, key="Water_Intake_liters")
summary(cf12)
barplot(cf12, main="silownia")

#Workout_Frequency_daysweek

rules_Workout_Frequency_daysweek <- validator(
  Workout_Frequency_daysweek >= 0,
  Workout_Frequency_daysweek <= 7,
  Workout_Frequency_daysweek %% 1 == 0
)

cf13 <- confront(silownia_brudne_dane, rules_Workout_Frequency_daysweek, key="Workout_Frequency_daysweek")
summary(cf13)
barplot(cf13, main="silownia")

#Experience_level

rules_Experience_Level <- validator(
  Experience_Level >= 0,
  Experience_Level <= 3,
  Experience_Level %% 1 == 0
)

cf14 <- confront(silownia_brudne_dane, rules_Experience_Level, key="Experience_Level")
summary(cf14)
barplot(cf14, main="silownia")

#BMI

rules_BMI <- validator(
  `BMI` > 0,                      
  `BMI` >= 10,
  `BMI` <= 60, #Zakres BMI
  abs(`BMI` - (`Weight_kg` / (`Height_m`^2))) < 0.1 # Spójność z masą i wzrostem(ze względu na wzór)
)

cf15 <- confront(silownia_brudne_dane, rules_BMI, key="BMI")
summary(cf15)
barplot(cf15, main="silownia")

#DANE SĄ CZYSTE I GOTOWE DO ANALIZY
Silownia_wykresy <- silownia_brudne_dane
