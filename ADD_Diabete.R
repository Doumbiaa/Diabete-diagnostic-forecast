# Charger les bibliothèques nécessaires
library(MASS)           # Pour LDA et QDA
library(caret)          # Pour la validation croisée et les métriques de performance
library(pROC)           # Pour la courbe ROC
library(ggplot2) 
library(parallelly)
library(pROC)
library(kernlab)
library(dplyr)
# Pour les visualisations
install.packages("caret")
install.packages("parallelly") # Necessaire pour caret
install.packages("pROC")
install.packages("biotools")




# Charger le dataset
data <- read.table(file.choose(), sep = ",", header =TRUE)
data
# Convertir la variable cible en facteur
data$Outcome <- as.factor(data$Outcome)

# Aperçu des données
summary(data)

dim(data)

##- Structure des donnees
str(data)

##- Nom des colonnes 
colnames(data)

##- Affichage des k premieriere ligne
head(data)

# Visualisation des données
pairs(data[,-9], col = data$Outcome)
ggplot(data, aes(x=Glucose, y=Insulin, color=Outcome)) + geom_point() + labs(title="Glucose vs Insulin")

sum(is.na(data))  # Vérifier s'il y a des NA
#- Nettoyage des données
cols_to_replace <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
#- les colonnes presentant des valeurs nulle 0,
#- on les remplace par la médaine pour éviter des biais 
# Boucle pour chaque colonne concernée
for (col in cols_to_replace) {
  median_value <- median(data[[col]][data[[col]] != 0], na.rm = TRUE)
  data[[col]][data[[col]] == 0] <- median_value
}
fix(data)
# Convertir la variable cible en facteur
data$Outcome <- as.factor(data$Outcome)

# Séparation des ensembles d'entraînement et de test
set.seed(42)
trainIndex <- createDataPartition(data$Outcome, p = 0.7, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Application de l'Analyse Discriminante Linéaire (LDA)
lda_model <- lda(Outcome ~ ., data = trainData)
lda_model
# Prediction des donnees 
lda_pred <- predict(lda_model, testData)$class

# Matrice de confusion et métriques de performance pour LDA
confusion_lda <- confusionMatrix(lda_pred, testData$Outcome)
confusion_lda

# Courbe ROC pour LDA
lda_prob <- predict(lda_model, testData)$posterior[,2]
roc_curve_lda <- roc(testData$Outcome, lda_prob)
plot(roc_curve_lda, main="Courbe ROC pour LDA", col="blue")
auc(roc_curve_lda)
legend("bottomright", legend="LDA", col="blue", lwd=2)

# Application de l'Analyse Discriminante Quadratique (QDA)
qda_model <- qda(Outcome ~ ., data = trainData)
qda_model
qda_pred <- predict(qda_model, testData)$class

# Matrice de confusion et métriques de performance pour QDA
confusion_qda <- confusionMatrix(qda_pred, testData$Outcome)
confusion_qda

# Courbe ROC pour QDA
qda_prob <- predict(qda_model, testData)$posterior[, 2]
roc_qda <- roc(testData$Outcome, qda_prob)
plot(roc_qda, main="Courbe ROC pour QDA", col="green", add=TRUE)
auc(roc_qda)
legend("bottomright", legend=c("LDA", "QDA"), col=c("blue", "green"), lwd=2)

# Appliquer le modèle KNN
library(class)
K <- 5 # Nombre de voisins
knn_pred <- knn(trainData[, -which(names(trainData) == "Outcome")], 
                testData[, -which(names(testData) == "Outcome")], 
                trainData$Outcome, k = K)
summary(knn_pred)

##- Verification du modele
table(testData[,"Outcome"],knn_pred)

confusion_knn <- confusionMatrix(knn_pred, testData$Outcome)
confusion_knn 

# Courbe ROC pour KNN (Utilisation des probabilités approximatives)
knn_prob <- as.numeric(knn_pred == "1")  # Transforme les prédictions en format binaire
roc_knn <- roc(testData$Outcome, knn_prob)
plot(roc_knn, main="Courbe ROC pour KNN", col="red", add=TRUE)
auc(roc_knn)
legend("bottomright", legend=c("LDA", "QDA", "KNN"), col=c("blue", "green", "red"), lwd=2)

##- La methode noyau 
library(kernlab)
##- Creation d'un modele d'analyse discriminante avec la methode des noyaux
kda_model <- ksvm(Outcome ~ ., data = trainData, type = "C-svc", kernel = "vanilladot", prob.model = TRUE)
##-Prediction sur la base test
kda_pred <-kernlab::predict(modele_kda, newdata = testData)
summary(kda_pred)

confusion_kda <- confusionMatrix(kda_pred, testData$Outcome)
confusion_knn 

# courbe ROC et AUC pour KDA
# Faire des prédictions probabilistes
kda_pred_prob <- predict(kda_model, newdata = testData, type = "probabilities")
roc_kda <- roc(testData$Outcome, kda_pred_prob[,2])  # kda_pred_prob[,2] pour la probabilité de la classe "1"
plot(roc_kda, main = "Courbe ROC pour le modèle KDA", col="yellow", add=TRUE)
auc(roc_kda)
legend("bottomright", legend=c("LDA", "QDA", "KNN", "KDA"), col=c("blue", "green", "red", "yellow"), lwd=2)

# Comparaison des résultats de l'AUC
lda_auc <- auc(roc_curve_lda)
qda_auc <- auc(roc_qda)
knn_auc <- auc(roc_knn)
kda_auc <- auc(roc_kda)

results <- data.frame(
  Model = c("LDA", "QDA", "KNN", "KDA"),
  AUC = c(lda_auc, qda_auc, knn_auc, kda_auc)
)

results

# Visualisation des résultats avec valeurs AUC 
ggplot(results, aes(x=reorder(Model, -AUC), y=AUC, fill=Model)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(AUC, 3)), vjust=-0.5, size=5) +  # Affiche les valeurs AUC sur les barres
  labs(title="Comparaison des modèles - AUC", y="AUC", x="Modèle") +
  theme_minimal()


## Comparer les performances des modèles avec un graphique
# Créer un data frame pour les métriques
metrics <- data.frame(
  Model = rep(c("LDA", "QDA", "KNN", "KDA"), each = 4),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1_Score"), times = 4),
  Value = c(
    confusion_lda$overall["Accuracy"],
    confusion_lda$byClass["Pos Pred Value"],
    confusion_lda$byClass["Sensitivity"],
    confusion_lda$byClass["F1"],
    
    confusion_qda$overall["Accuracy"],
    confusion_qda$byClass["Pos Pred Value"],
    confusion_qda$byClass["Sensitivity"],
    confusion_qda$byClass["F1"],
    
    confusion_knn$overall["Accuracy"],
    confusion_knn$byClass["Pos Pred Value"],
    confusion_knn$byClass["Sensitivity"],
    confusion_knn$byClass["F1"],
    
    confusion_kda$overall["Accuracy"],
    confusion_kda$byClass["Pos Pred Value"],
    confusion_kda$byClass["Sensitivity"],
    confusion_kda$byClass["F1"]
  )
)

# Barplot avec légende et proportions
ggplot(metrics, aes(x = Model, y = Value, fill = Metric)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) + 
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) + 
  scale_fill_manual(values = c("Accuracy" = "lightblue", 
                               "Precision" = "lightgreen", 
                               "Recall" = "lightcoral", 
                               "F1_Score" = "lightyellow")) + 
  labs(title = "Comparaison des performances des modèles", 
       x = "Modèles", 
       y = "Valeurs des métriques", 
       fill = "Métriques") + 
  theme_minimal()

# Validation definitive du modele
library(biotools)
# Sélectionner les variables explicatives
variables_explicatives <- data[, -which(names(data) == "Outcome")]

# Appliquer le test de Box's M
box_m_result <- boxM(variables_explicatives, data$Outcome)
box_m_result

# Exemple d'un nouvel individu (remplir avec des valeurs appropriées)
new_individual <- data.frame(
  Pregnancies = 1,
  Glucose = 120,         
  BloodPressure = 70,    
  SkinThickness = 25,    
  Insulin = 100,         
  BMI = 30,               
  DiabetesPedigreeFunction = 0.5,  # Exemple de valeur
  Age = 35
)
# Prédiction avec KDA
kda_pred_new <- predict(kda_model, new_individual)
# Transformer l'affichage en texte compréhensible
kda_pred_new_text <- ifelse(kda_pred_new == 1, "Diabétique", "Non diabétique")
print(paste("Prédiction KDA pour le nouvel individu : ", kda_pred_new_text))

new_diabetic <- data.frame(
  Pregnancies = 5,         # Nombre de grossesses 
  Glucose = 220,           # Très élevé
  BloodPressure = 100,     # Élevé
  SkinThickness = 40,      # Élevé
  Insulin = 250,           # Élevé
  BMI = 38,                # Obésité
  DiabetesPedigreeFunction = 1.5,  # Haut risque génétique
  Age = 50                # Âge avancé (facteur de risque)
)

# Prédiction avec le modèle KDA
kda_pred_diabetic <- predict(kda_model, new_diabetic)
# Transformer l'affichage en texte compréhensible
kda_pred_diabetic_text <- ifelse(kda_pred_diabetic == 1, "Diabétique", "Non diabétique")
print(paste("Prédiction KDA pour un individu fortement diabétique :", kda_pred_diabetic_text))

#Sauvegarde modele
saveRDS(kda_model, "diabetes_model.rds")
