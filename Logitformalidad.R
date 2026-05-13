url_zip <- "https://github.com/EduardoVelazquezBarragan/Modelo-Logit-para-la-Formalidad-Laboral-en-la-ENOE/blob/main/Datos%20ENOE.zip?raw=true"

# 1. Crear un archivo temporal
temp <- tempfile()

# 2. Descargar el archivo ZIP en el archivo temporal
download.file(url_zip, temp, mode = "wb")

# 3. Leer el CSV específico que está dentro del ZIP
# El nombre "SDEMT425.csv" debe coincidir exactamente con cómo se llama dentro del ZIP
enoe <- read.csv(unz(temp, "Datos ENOE/SDEMT425.csv"))
enoe

# 4. Eliminar el archivo temporal de la memoria
unlink(temp)


enoe<-enoe[enoe$clase2==1 & enoe$eda>=15,] #filtro a ocupados 

formal<-ifelse(enoe$emp_ppal==2,1,0) # 1 formal, 0 informal
escol<-enoe$anios_esc
edad<-enoe$eda
sexo <- factor(enoe$sex, levels = c(1,2), labels = c("Hombre","Mujer"))
# Mal: "2","3",...
# Bien:
enoe$region <- ifelse(enoe$cve_ent %in% c(2,3,5,8,10,19,25,26,28), "norte",
                      ifelse(enoe$cve_ent %in% c(1,6,11,14,16,18,22,24,32), "centro_norte",
                             ifelse(enoe$cve_ent %in% c(9,13,15,17,21,29), "centro", "sur")))
region <- factor(enoe$region, levels = c("norte","centro_norte","centro","sur"))
region

logit <- glm(formal ~ sexo + edad + escol + region,
            family = binomial)
summary(logit)

intervconf<-confint(logit,level=.95)
intervconf
tabla <- data.frame(
  efecto_pct = round((exp(coef(logit)) - 1) * 100, 2),
  ic_inf     = round((exp(intervconf[, 1]) - 1) * 100, 2),
  ic_sup     = round((exp(intervconf[, 2]) - 1) * 100, 2)
)

tabla <- tabla[-1, ]  # elimina la fila del intercepto
tabla

install.packages("ROCR")
library(ROCR)

#  Probabilidades predichas
prob <- predict(logit, type = "response")

#  Objeto ROCR
pred_rocr <- prediction(prob, formal)

# 3. Curva ROC y AUC
perf_rocr <- performance(pred_rocr, "tpr", "fpr")
auc_value <- performance(pred_rocr, "auc")@y.values[[1]]

# Graficar
plot(perf_rocr, colorize = TRUE, lwd = 2,
     main = paste("Curva ROC (AUC =", round(auc_value, 4), ")"))
abline(a = 0, b = 1, lty = 2, col = "grey")

#  Encontrar el umbral óptimo (más cercano a esquina superior izquierda)
sensibilidad <- perf_rocr@y.values[[1]]
fpr          <- perf_rocr@x.values[[1]]
umbrales     <- perf_rocr@alpha.values[[1]]

optimo <- which.min(sqrt((1 - sensibilidad)^2 + fpr^2))
cat("Umbral óptimo:", round(umbrales[optimo], 4), "\n")

# Con el umbral óptimo 
predicciones <- ifelse(prob >= umbrales[optimo], 1, 0)

# Matriz de confusión
matriz <- table(Prediccion = predicciones, Real = formal)
print(matriz)

# Accuracy
accuracy <- sum(diag(matriz)) / sum(matriz)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

VP <- matriz[2,2]  # Verdaderos Positivos (predijo formal, era formal)
VN <- matriz[1,1]  # Verdaderos Negativos (predijo informal, era informal)
FP <- matriz[2,1]  # Falsos Positivos
FN <- matriz[1,2]  # Falsos Negativos

# Métricas
sensibilidad  <- VP / (VP + FN)
especificidad <- VN / (VN + FP)
accuracy      <- (VP + VN) / sum(matriz)

cat("Sensibilidad:", round(sensibilidad * 100, 2), "%\n")
cat("Especificidad:", round(especificidad * 100, 2), "%\n")
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
