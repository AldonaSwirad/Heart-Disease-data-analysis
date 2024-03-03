dane <- read.csv("C:\\Users\\Aldona\\Documents\\GitHub\\Heart-Disease-data-analysis\\heart_2020_cleaned.csv", sep = ',', dec='.')

#barplot(height = dane$BMI, names.arg = dane$HeartDisease)
#title(ylab = "BMI", xlab = "Czy chory?", line = 2.5)
#l <- table(dane$BMI)
#length(l)
cardio <- dane[, c(1, 2)]
# byly chore
cardio_yes <- cardio[cardio$HeartDisease == "Yes", ]

# nie byly chore
cardio_no <- cardio[cardio$HeartDisease == "No", ]

#zaokrąglenie wartości pod agregację

nrow(cardio_yes)
cardio_yes$BMI <- round(cardio_yes$BMI)
nrow(cardio_yes)

nrow(cardio_no)
cardio_no$BMI <- round(cardio_no$BMI)
nrow(cardio_no)

count_yes <- as.data.frame(table(cardio_yes$BMI))
colnames(count_yes) <- c("Zaokraglenie", "Liczba_Wystapien")
count_no <- as.data.frame(table(cardio_no$BMI))
colnames(count_no) <- c("Zaokraglenie", "Liczba_Wystapien")

par(mfrow=c(1,2))
plot(count_yes$Zaokraglenie, count_yes$Liczba_Wystapien)
plot(count_no$Zaokraglenie, count_no$Liczba_Wystapien)

