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

# Imputacja metodą k-Nearest Neighbors (kNN) - poprawka
library(VIM)
dane_imputowane <- kNN(silownia, k = 3) # Imputacja z użyciem 3 najbliższych sąsiadów
print(dane_imputowane)
str(dane_imputowane)

vis_miss(dane_imputowane)
vis_dat(dane_imputowane)
gg_miss_var(dane_imputowane)

# Imputacja z pakietem MICE
library(mice)

pred_mat <- quickpred(silownia, mincor = 0.25)

#Imputacja z pakietem RPART
install.packages("rpart")
library(rpart)

colnames(silownia)[colnames(silownia) == "Weight (kg)"] <- "Weight_kg"
colnames(silownia)[colnames(silownia) == "Height (m)"] <- "Height_m"
colnames(silownia)[colnames(silownia) == "Session_Duration (hours)"] <- "Session_Duration_hours"
colnames(silownia)[colnames(silownia) == "Water_Intake (liters)"] <- "Water_Intake_liters"
colnames(silownia)[colnames(silownia) == "Workout_Frequency (days/week)"] <- "Workout_Frequency_daysweek"

drzewo_decyzyjne1 <- rpart(BMI ~ Age + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Experience_Level, data = silownia, method = "anova", na.action = na.exclude)
silownia$BMI[is.na(silownia$BMI)] <- predict(drzewo_decyzyjne1, newdata = silownia[is.na(silownia$BMI), ])

drzewo_decyzyjne2 <- rpart(Age ~ BMI + Max_BPM + Weight_kg + Height_m + Avg_BPM + Resting_BPM + 
                             Session_Duration_hours + Calories_Burned + 
                             Fat_Percentage + Water_Intake_liters + 
                             Workout_Frequency_daysweek + Experience_Level, data = silownia, method = "anova", na.action = na.exclude)
silownia$Age[is.na(silownia$Age)] <- predict(drzewo_decyzyjne2, newdata = silownia[is.na(silownia$Age), ])

silownia$Workout_Type <- as.factor(silownia$Workout_Type)
silownia$Gender <- as.factor(silownia$Gender)

#Jak z Workout_Type?

drzewo_decyzyjne3 <- rpart(Workout_Type ~ Gender, data = silownia, method = "class", na.action = na.exclude)

silownia$Workout_Type[is.na(silownia$Workout_Type)] <- predict(drzewo_decyzyjne3, newdata = silownia[is.na(silownia$Workout_Type), ])
print(silownia)

#Imputacja hot-deck
dane_imputowanehotdeck <- hotdeck(silownia)
print(dane_imputowanehotdeck)
