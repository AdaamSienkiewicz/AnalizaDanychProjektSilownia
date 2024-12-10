library(ggplot2)
library(readr)
library(naniar)
library(visdat)
library(dplyr)

silownia <- read_csv("silownia_new.csv")

View(silownia)


vis_miss(silownia)
vis_dat(silownia)
gg_miss_var(silownia)

#Braki danych występują w kolumnie Workout_Type, Age oraz BMI
#Kolumna Age oraz BMI posiadają wartości liczbowe, a kolumna Workout_Type to ciąg znaków (wyraz)
#W Age braki stanowią 10%, w Workout_Type 15%, a w BMI 15 %

kolumny_z_NA <- silownia %>% 
                  select(Age, Workout_Type, BMI)

#Jakby ktos potrzebowal kodu do zmiany character na numeric
kolumny_z_NA  <- kolumny_z_NA  %>%
  mutate(Workout_Type = case_when(
    Workout_Type == "Yoga" ~ 1,
    Workout_Type == "Cardio" ~ 2,
    Workout_Type == "HIIT" ~ 3,
    Workout_Type == "Strength" ~ 4,
    TRUE ~ as.numeric(Workout_Type) # Pozostawienie NA
  ))

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

#Zmiana nazw kolumn
colnames(silownia)[colnames(silownia) == "Weight (kg)"] <- "Weight_kg"
colnames(silownia)[colnames(silownia) == "Height (m)"] <- "Height_m"
colnames(silownia)[colnames(silownia) == "Session_Duration (hours)"] <- "Session_Duration_hours"
colnames(silownia)[colnames(silownia) == "Water_Intake (liters)"] <- "Water_Intake_liters"
colnames(silownia)[colnames(silownia) == "Workout_Frequency (days/week)"] <- "Workout_Frequency_daysweek"

# Zastąpienie braków NA w kolumnie BMI za pomocą wzoru na BMI
silownia_z_BMI <- silownia
silownia_z_BMI$BMI <- ifelse(
  is.na(silownia_z_BMI$BMI),  
  silownia_z_BMI$Weight_kg / (silownia_z_BMI$Height_m^2),  
  silownia_z_BMI$BMI )

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

is.factor(silownia$Workout_Type)
silownia$Workout_Type <- factor(silownia$Workout_Type, levels = c("Yoga", "Cardio", "HIIT", "Strength"))

metody <- make.method(silownia)
metody["Age"] <- "pmm"
metody["Workout_Type"] <- "polyreg"
metody["BMI"] <- "pmm"

pred_mat <- quickpred(silownia, mincor = 0.25)
pred_mat
silownia_imp <- mice(silownia, m = 5, method = metody, predictorMatrix = pred_mat)

lm_imp <- with(silownia_imp, lm(BMI ~ Weight_kg + Gender))
lm_pooled <- pool(lm_imp)
summary(lm_pooled, conf.int = TRUE, conf.level = 0.95)

# Porównanie danych rzeczywistych z zimputowanymi
stripplot(silownia_imp, BMI ~ Weight_kg  | .imp, pch = 20, cex = 2)

silownia_mice <- complete(silownia_imp, action = 1)
head(silownia_mice)

summary(silownia)
summary(silownia_mice)


#Imputacja z pakietem RPART
install.packages("rpart")
library(rpart)

colnames(silownia)[colnames(silownia) == "Weight (kg)"] <- "Weight_kg"
colnames(silownia)[colnames(silownia) == "Height (m)"] <- "Height_m"
colnames(silownia)[colnames(silownia) == "Session_Duration (hours)"] <- "Session_Duration_hours"
colnames(silownia)[colnames(silownia) == "Water_Intake (liters)"] <- "Water_Intake_liters"
colnames(silownia)[colnames(silownia) == "Workout_Frequency (days/week)"] <- "Workout_Frequency_daysweek"

silownia  <- silownia  %>%
  mutate(Workout_Type = case_when(
    Workout_Type == "Yoga" ~ 1,
    Workout_Type == "Cardio" ~ 2,
    Workout_Type == "HIIT" ~ 3,
    Workout_Type == "Strength" ~ 4,
    TRUE ~ as.numeric(Workout_Type) 
  ))

silownia <- silownia %>%
  mutate(Gender = case_when(
    Gender == "Male" ~ 1,
    Gender == "Female" ~ 2,
    TRUE ~ as.numeric(Gender) 
  ))

drzewo_decyzyjne1 <- rpart(Workout_Type ~ Age + BMI + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Gender + Experience_Level, data = silownia, method = "anova", na.action = na.exclude)

silownia$Workout_Type[is.na(silownia$Workout_Type)] <- predict(drzewo_decyzyjne1, newdata = silownia[is.na(silownia$Workout_Type), ])
drzewo_decyzyjne2 <- rpart(BMI ~ Age + Workout_Type + Gender + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Experience_Level, data = silownia, method = "anova", na.action = na.exclude)
silownia$BMI[is.na(silownia$BMI)] <- predict(drzewo_decyzyjne2, newdata = silownia[is.na(silownia$BMI), ])

drzewo_decyzyjne3 <- rpart(Age ~ BMI + Workout_Type + Gender + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Experience_Level, data = silownia, method = "anova", na.action = na.exclude)
silownia$Age[is.na(silownia$Age)] <- predict(drzewo_decyzyjne3, newdata = silownia[is.na(silownia$Age), ])

print(silownia)

#Imputacja hot-deck
dane_imputowanehotdeck <- hotdeck(silownia)
print(dane_imputowanehotdeck)


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

#walidacja danych
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

#walidacja Weight (kg)

min_value <- min(silownia$`Weight (kg)`, na.rm = TRUE)
max_value <- max(silownia$`Weight (kg)`, na.rm = TRUE)

rules <- validator(
  `Weight (kg)` > 0,            
  `Weight (kg)` >= 40,           
  `Weight (kg)` <= 140           
)

cf <- confront(silownia, rules, key="Weight (kg)")
summary(cf)
barplot(cf, main="silownia")
