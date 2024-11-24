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

library(VIM)

# Imputacja metodą k-Nearest Neighbors (kNN) - poprawa
dane_imputowane <- kNN(silownia, k = 3) # Imputacja z użyciem 3 najbliższych sąsiadów
print(dane_imputowane)
str(dane_imputowane)
vis_miss(dane_imputowane)
vis_dat(dane_imputowane)
gg_miss_var(dane_imputowane)

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