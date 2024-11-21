library(ggplot2)
library(readr)
library(naniar)
library(visdat)
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
