library(ggplot2)
library(readr)
library(naniar)
library(visdat)
library(dplyr)
library(skimr)

silownia <- read_csv("silownia_new.csv")

View(silownia)
skim(silownia)

#Zmiana nazw kolumn
colnames(silownia)[colnames(silownia) == "Weight (kg)"] <- "Weight_kg"
colnames(silownia)[colnames(silownia) == "Height (m)"] <- "Height_m"
colnames(silownia)[colnames(silownia) == "Session_Duration (hours)"] <- "Session_Duration_hours"
colnames(silownia)[colnames(silownia) == "Water_Intake (liters)"] <- "Water_Intake_liters"
colnames(silownia)[colnames(silownia) == "Workout_Frequency (days/week)"] <- "Workout_Frequency_daysweek"


#WYKAZANIE OBSERWACJI ODSTAJĄCYCH
# metoda Z-score
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

# Za pomocą metody z-score wykazano, że dla zmiennych BMI oraz Calories_burned są obserwacje, 
#które różnią się o co najmniej 3 odchylenia od średnich
#Dla zmiennej BMI jest 10 obserwacji odstających oraz dla zmiennej Calories_burned są 3 obserwacje odstające

# za pomocą wykresu ramkowego
#Age
boxplot(silownia$Age)
# Zmienna Age nie ma obserwacji odstających, które znajdują się o więcej niż 1,5 rozstępu ćwiartkowego 
#poniżej pierwszego kwartyla oraz powyżej trzeciego kwartyla

#Weight
boxplot(silownia$Weight_kg)
# Zmienna Weight ma odstające obserwacje (górne outliery), czyli obserwacje, które znajdują się o więcej
#niż 1,5 rozstępu ćwiartkowego powyżej trzeciego kwartyla

#Height
boxplot(silownia$Height_m)
# Zmienna Height nie ma obserwacji odstających

#Max BPM
boxplot(silownia$Max_BPM)
# Zmienna Max_BPM nie ma obserwacji odstających

#Avg BPM
boxplot(silownia$Avg_BPM) 
# Zmienna Avg_BPM nie ma obserwacji odstających

#Resting BPM
boxplot(silownia$Resting_BPM)
# Zmienna Resting BPM nie ma obserwacji odstających

#Session duration
boxplot(silownia$Session_Duration_hours)
# Zmienna Session duration nie ma obserwacji odstających

#Calories burned
boxplot(silownia$Calories_Burned)
# Zmienna Calories burned ma górne outliery, czyli obserwacje, które znajdują się o więcej
#niż 1,5 rozstępu ćwiartkowego powyżej trzeciego kwartyla

#Fat percentage
boxplot(silownia$Fat_Percentage)
# Zmienna Fat percentage nie ma obserwacji odstających 

#Water intake
boxplot(silownia$Water_Intake_liters)
# Zmienna Water intake nie ma obserwacji odstających 

#Workout frequency
boxplot(silownia$Workout_Frequency_daysweek)
# Zmienna Workout frequency nie ma obserwacji odstających 

#BMI
boxplot(silownia$BMI)
# W przypadku zmiennej BMI występują górne outliery, czyli obserwacje, które znajdują się o więcej
#niż 1,5 rozstępu ćwiartkowego powyżej trzeciego kwartyla

# Za pomocą wykresu ramkowego wykazano, że w przypadku zmiennych Weight, Calories_burned oraz BMI występują 
#górne outliery,czyli obserwacje, które znajdują się o więcej niż 1,5 rozstępu ćwiartkowego powyżej trzeciego kwartyla.
# Żadna zmienna natomiast nie ma ekstremalnych obserwacji odstających


#ANALIZA BRAKÓW DANYCH
vis_miss(silownia)
vis_dat(silownia)
gg_miss_var(silownia)
colSums(is.na(silownia))

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
   "Information", "scorecard"
)

install.packages("Information")
install.packages("scorecard")

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

install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

#Podczas wizualizacji skupiliśmy się na podkreśleniu najciekawszych zależności pomiędzy danymi

#Rozkład kobiet i mężczyzn
# Wykres pokazuje równomierny rozkład płci w analizowanym zbiorze danych. Widoczna jest zbliżona liczba kobiet i mężczyzn, z niewielką przewagą mężczyzn, około 500 osób w każdej grupie.

ggplot(Silownia_wykresy, aes(x = Gender, fill = Gender)) + 
  geom_bar(stat = "count", width = 0.6, color = "black", size = 0.5) +  
  scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +  
  labs(
    title = "Rozkład płci w zbiorze danych", 
    x = "Płeć", 
    y = "Liczba osób"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title.position = "panel"
    )

#Rozkład wieku w zależności od płci
#Wykres przedstawia rozkład wieku w zależności od podanej płci. Zauważyć można większy udział mężczyzn w grupie wiekowej 25-39 lat. Z kolei w grupie 40-55 lat przeważyła płeć żeńska.

ggplot(Silownia_wykresy, aes(Age, color = Gender, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Rozkład wieku w zależności od płci", x = "Wiek", y = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title.position = "panel"
  )

install.packages("psych")
library(psych)
library(stats)
opis_Age <- list( "Wiek" = 
                    list(
                      "Min"= ~ min(Age),
                      "Max"= ~ max(Age),
                      "Kwartyl dolny"= ~ quantile(Age,0.25),
                      "Mediana"= ~ round(median(Age),2),
                      "Kwartyl górny"= ~ quantile(Age,0.75),
                      "Średnia"= ~ round(mean(Age),2),
                      "Odch. std."= ~ round(sd(Age),2),
                      "IQR"= ~ round(IQR(Age),2),
                      "Odchylenie ćwiartkowe"=~round(IQR(Age)/2,2),
                      "Odch. std. w %"=~round((sd(Age)/mean(Age)),2),
                      "Odch. ćwiartkowe w %"=~round((IQR(Age)/median(Age)),2),
                      "Skośność"=~round(skew(Age),2),
                      "Kurtoza"=~round(kurtosi(Age),2)
                    ))
install.packages("qwraps2")
library(qwraps2)
tabela_Age <- summary_table(Silownia_wykresy, summaries = opis_Age, by = c("Gender"), markup = "plain")
tabela_Age <- gsub("~~", "", tabela_Age)
tabela_Age <- as.data.frame(tabela_Age)
tabela_Age <- tabela_Age[-1, ]  
print(tabela_Age)

library(kableExtra)
tabela_Age %>%
  knitr::kable(
    digits = 2,
    align = "lcc", 
    caption = "Statystyki opisowe zmiennej 'Wiek' w zależności od płci",
    col.names = c("Statystyka", "Kobiety", "Mężczyźni"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  add_header_above(c(" " = 1, "Płeć" = 2))
# Wyniki pokazują, że wiek kobiet i mężczyzn w próbie jest zbliżony, co też potwierdza analizę dokonaną na podstawie wykresu rozkładu wieku w zależności od płci. Średni wiek dla obu płci wynosi około 38 lat. Z kolei mediana dla kobiet to 40 lat, a dla mężczyzn 39 lat. Wiek minimalny to 18, a maksymalny 59 lat dla obu płci. Rozproszenie danych (odchylenie standardowe) jest podobne: 12,22 dla kobiet i 12,32 dla mężczyzn. Rozstęp międzykwartylowy (IQR) wynosi 21, co wskazuje na stabilność w centralnej części rozkładu. Rozkład wieku jest lekko ujemnie skośny, co oznacza niewielką przewagę starszych uczestników w próbie.

#Zależność BMI od wieku
#Wykres pokazuje, jakie wartości wskaźnika BMI osiągano dla danego wieku. Dodano także linię trendu, która została oszacowana na poziomie BMI = 25.

ggplot(Silownia_wykresy, aes(x = Age, y = BMI)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Wiek") +
  ylab("BMI") +
  ggtitle("Zależność BMI od wieku") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

opis_BMI <- list( "BMI" = 
                    list(
                      "Min"= ~ min(BMI),
                      "Max"= ~ max(BMI),
                      "Kwartyl dolny"= ~ quantile(BMI,0.25),
                      "Mediana"= ~ round(median(BMI),2),
                      "Kwartyl górny"= ~ quantile(BMI,0.75),
                      "Średnia"= ~ round(mean(BMI),2),
                      "Odch. std."= ~ round(sd(BMI),2),
                      "IQR"= ~ round(IQR(BMI),2),
                      "Odchylenie ćwiartkowe"=~round(IQR(BMI)/2,2),
                      "Odch. std. w %"=~round((sd(Age)/mean(BMI)),2),
                      "Odch. ćwiartkowe w %"=~round((IQR(Age)/median(BMI)),2),
                      "Skośność"=~round(skew(BMI),2),
                      "Kurtoza"=~round(kurtosi(BMI),2)
                    ))

library(qwraps2)

Silownia_wykresy$Age_Category <- cut(Silownia_wykresy$Age,
                                     breaks = c(18, 29, 39, 49, 59),
                                     labels = c("18-29", "29-39", "39-49", "49-59"),
                                     right = FALSE)

tabela_BMI <- summary_table(Silownia_wykresy, summaries = opis_BMI, by = c("Age_Category"), markup = "plain")
tabela_BMI <- gsub("~~", "", tabela_BMI)
tabela_BMI <- as.data.frame(tabela_BMI)
tabela_BMI <- tabela_BMI[-1, ] 
print(tabela_BMI)

library(kableExtra)

tabela_BMI %>%
  knitr::kable(
    digits = 2,
    align = "lcc", 
    caption = "Statystyki opisowe zmiennej 'BMI' w zależności od wieku",
    col.names = c("Statystyka", "18-29", "29-39", "39-49", "49-59"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  add_header_above(c(" " = 1, "Wiek" = 4))

# Na podstawie powyższych wyników można stwierdzić, że najwyższą średnią wartość BMI osiągnięto dla grupy wiekowej 18-29 lat, dla której największa jest też mediana BMI równa 24,96. Minimalna wartość BMI to 12,32 dla osób w wieku 39-49 lat, a maksymalna wynosi niespełna 50 dla tej samej grupy wiekowej. W grupie wiekowej 49-59 nastąpiło największe rozproszenie danych. Wyniosło ono 7,1. Rozstęp międzykwartylowy (IQR) osiąga wartości od 7,08 do 9,08, co wskazuje na stabilność w centralnej części rozkładu. Rozkład BMI jest lekko dodatnio skośny. Wartości Bmi większe od średniej są nieco bardziej rozproszone. Wartości kurtozy wskazują na to, że dane są bardziej rozproszone, a wyniki nie wykazują silnej koncentracji wokół średniej ani wielu skrajnych wartości.

#Czas trwania sesji a spalone kalorie 
#Wykres ten przedstawia zależność między czasem trwania treningu i spalonymi kaloriami. Im dłuższy trening, tym więcej spalonych kalorii

ggplot(Silownia_wykresy, aes(x = Session_Duration_hours, y = Calories_Burned)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", color = "orange", se = TRUE) +
  xlab("Czas trwania sesji (godziny)") +
  ylab("Spalone kalorie") +
  ggtitle("Czas trwania sesji a spalone kalorie") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

opis_Calories_Burned <- list( "Spalone kalorie" = 
                    list(
                      "Min"= ~ min(Calories_Burned),
                      "Max"= ~ max(Calories_Burned),
                      "Kwartyl dolny"= ~ quantile(Calories_Burned,0.25),
                      "Mediana"= ~ round(median(Calories_Burned),2),
                      "Kwartyl górny"= ~ quantile(Calories_Burned,0.75),
                      "Średnia"= ~ round(mean(Calories_Burned),2),
                      "Odch. std."= ~ round(sd(Calories_Burned),2),
                      "IQR"= ~ round(IQR(Calories_Burned),2),
                      "Odchylenie ćwiartkowe"=~round(IQR(Calories_Burned)/2,2),
                      "Odch. std. w %"=~round((sd(Calories_Burned)/mean(Calories_Burned)),2),
                      "Odch. ćwiartkowe w %"=~round((IQR(Calories_Burned)/median(Calories_Burned)),2),
                      "Skośność"=~round(skew(Calories_Burned),2),
                      "Kurtoza"=~round(kurtosi(Calories_Burned),2)
                    ))

library(qwraps2)

Silownia_wykresy$Duration_Category <- cut(Silownia_wykresy$Session_Duration_hours, 
                                          breaks = c(0, 1, 1.5, 2), 
                                          labels = c("0,5-1h", "1-1,5h", "1,5-2h"), 
                                          right = FALSE)

tabela_Calories_Burned <- summary_table(Silownia_wykresy, summaries = opis_Calories_Burned, by = c("Duration_Category"), markup = "plain")
tabela_Calories_Burned <- gsub("~~", "", tabela_Calories_Burned)
tabela_Calories_Burned <- as.data.frame(tabela_Calories_Burned)
tabela_Calories_Burned <- tabela_Calories_Burned[-1, ]  
print(tabela_Calories_Burned)

library(kableExtra)
tabela_Calories_Burned %>%
  knitr::kable(
    digits = 2,
    align = "lcc", 
    caption = "Statystyki opisowe zmiennej 'Spalone kalorie' w zależności od czasu trwania sesji",
    col.names = c("Statystyka", "0,5-1h", "1-1,5h", "1,5-2h"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  add_header_above(c(" " = 1, "Spalone kalorie" = 3))

#Średnie tętno a rodzaj treningu - w tym przypadku zrobiłem wykres pudełkowy

ggplot(Silownia_wykresy, aes(x = Workout_Type, y = Avg_BPM, fill = Workout_Type)) +
  geom_boxplot(alpha = 0.7) +
  xlab("Rodzaj treningu") +
  ylab("Średnie tętno (BPM)") +
  ggtitle("Średnie tętno a rodzaj treningu") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Spalone kalorie a waga

ggplot(Silownia_wykresy, aes(x = Weight_kg, y = Calories_Burned)) +
  geom_point(color = "purple", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  xlab("Waga (kg)") +
  ylab("Spalone kalorie") +
  ggtitle("Spalone kalorie a waga") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Zależność tętna spoczynkowego od wieku 
#Wstawiłem linię trendu wygładzoną, co pozwoliło uchwycić nieliniowe zależności między zmiennymi.
#Cień to przedział ufności, który wskazuje niepewność estymacji linii trendu. Szeroki cień oznacza większą niepewność w przewidywaniu.

ggplot(Silownia_wykresy, aes(x = Age, y = Resting_BPM)) +
  geom_point(color = "orange", size = 3, alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue", se = TRUE) +
  xlab("Wiek") +
  ylab("Tętno spoczynkowe (BPM)") +
  ggtitle("Zależność tętna spoczynkowego od wieku") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Procent tkanki tłuszczowej ciała a częśtotliwość treningów

ggplot(Silownia_wykresy, aes(x = Fat_Percentage, y = Workout_Frequency_daysweek)) +
  geom_point(color = "pink", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +
  xlab("Procent tkanki tłuszczowej ciała (%)") +
  ylab("Częstotliwość treningów (dni/tydzień)") +
  ggtitle("Zależność między procentem tłuszczu a częstotliwością treningów") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Spalone kaolorie a czas trwania sesji

ggplot(Silownia_wykresy, aes(x = Calories_Burned, y = Session_Duration_hours)) +
  geom_point(color = "steelblue", size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  xlab("Spalone kalorie") +
  ylab("Czas trwania sesji (godziny)") +
  ggtitle("Zależność między spalonymi kaloriami a czasem trwania sesji") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

#Wykres interaktywny - Zależność BMI od wieku, legenda to rodzaj treningu

install.packages("plotly")
library(plotly)

p1 <- ggplot(Silownia_wykresy, aes(x = Age, y = BMI, color = Workout_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Wiek") +
  ylab("BMI") +
  scale_color_discrete(name = "Rodzaj treningu") +
  ggtitle("Zależność BMI od wieku") +
  theme_light()

plotly::ggplotly(p1)

#Histogramy dla trzech rodzajów BPM - przykładowo zrobiłem dla tej zmiennej

ggplot(Silownia_wykresy, aes(x = Max_BPM)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogram dla Max_BPM") +
  xlab("Max_BPM") +
  ylab("Liczba obserwacji")

ggplot(Silownia_wykresy, aes(x = Avg_BPM)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogram dla Avg_BPM") +
  xlab("Avg_BPM") +
  ylab("Liczba obserwacji")

ggplot(Silownia_wykresy, aes(x = Resting_BPM)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogram dla Resting_BPM") +
  xlab("Resting_BPM") +
  ylab("Liczba obserwacji")


# Czas trwania sesji w zależności od typu treningu

ggplot(Silownia_wykresy, aes(x = Workout_Type, y = Session_Duration_hours, fill = Workout_Type)) +
  geom_violin() +
  labs(title = "Czas trwania sesji w zależności od typu treningu", x = "Typ treningu", y = "Czas trwania (godziny)")


# Średnie spalone kalorie według płci i poziomu doświadczenia

ggplot(Silownia_wykresy, aes(x = factor(Experience_Level), y = Calories_Burned, fill = Gender)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(title = "Średnie spalone kalorie według płci i poziomu doświadczenia", x = "Poziom doświadczenia", y = "Średnie spalone kalorie")


# Średnia liczba spalonych kalorii dla różnych typów treningu

ggplot(Silownia_wykresy, aes(x = Workout_Type, y = Calories_Burned, fill = Workout_Type)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Średnia liczba spalonych kalorii dla różnych typów treningu", x = "Typ treningu", y = "Średnie spalone kalorie")


# Częstotliwość treningów a czas trwania sesji

ggplot(Silownia_wykresy, aes(x = Workout_Frequency_daysweek, y = Session_Duration_hours, color = Experience_Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Częstotliwość treningów a czas trwania sesji", x = "Liczba sesji w tygodniu", y = "Czas trwania sesji (godziny)")


