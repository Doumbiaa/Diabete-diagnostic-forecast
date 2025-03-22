library(shiny)
library(kernlab)
library(shinythemes)

# Charger le modèle avec activation des probabilités
kda_model <- ksvm(Outcome ~ ., data = trainData, type = "C-svc", kernel = "vanilladot", prob.model = TRUE)

# Interface utilisateur
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Diabetes Diagnostic Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Pregnancies", "Nombre de grossesses :", value = 1, min = 0, max = 20),
      numericInput("Glucose", "Taux de glucose :", value = 120, min = 50, max = 250),
      numericInput("BloodPressure", "Pression artérielle :", value = 70, min = 40, max = 200),
      numericInput("SkinThickness", "Épaisseur de la peau :", value = 25, min = 0, max = 100),
      numericInput("Insulin", "Niveau d'insuline :", value = 100, min = 0, max = 900),
      numericInput("BMI", "Indice de masse corporelle (BMI) :", value = 30, min = 10, max = 50),
      numericInput("DiabetesPedigreeFunction", "Diabetes Pedigree Function :", value = 0.5, min = 0, max = 2),
      numericInput("Age", "Âge :", value = 35, min = 10, max = 100),
      actionButton("predict", "Prédire", class = "btn btn-primary"),
      h6('Auth: cheikh.doumbia@uadb.edu.sn'),
      tags$a("GitHub", href= "https://github.com/Doumbiaa/Diabete-diagnostic-forecast"),
      tags$a("Linkedin", href= "www.linkedin.com/in/cheikh-doumbia")
    ),
    
    mainPanel(
      h3("Résultat de la Prédiction :"),
      uiOutput("predictionBox"),
      h4("Score d'Appartenance :"),
      textOutput("scoreText"),
      uiOutput("scoreProgress")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Valeur réactive pour stocker la probabilité
  prob_diab <- reactiveVal(0)
  kda_pred_text <- reactiveVal("En attente...")
  
  observeEvent(input$predict, {
    req(input$Glucose, input$BMI, input$Age)  # Vérifie que les entrées sont valides
    
    # Création du nouvel individu avec les valeurs saisies
    new_individual <- data.frame(
      Pregnancies = input$Pregnancies,
      Glucose = input$Glucose,
      BloodPressure = input$BloodPressure,
      SkinThickness = input$SkinThickness,
      Insulin = input$Insulin,
      BMI = input$BMI,
      DiabetesPedigreeFunction = input$DiabetesPedigreeFunction,
      Age = input$Age
    )
    
    # Prédiction avec le modèle KDA + Probabilités
    kda_pred_prob <- predict(kda_model, new_individual, type = "probabilities")
    
    # Extraire la probabilité d'être diabétique
    prob <- round(kda_pred_prob[1, "1"] * 100, 2)  # Convertir en pourcentage
    prob_diab(prob)  # Met à jour la valeur réactive
    
    # Déterminer la classe
    kda_pred_text(ifelse(prob >= 50, "Diabétique", "Non diabétique"))
  })
  
  # Affichage du résultat
  output$predictionBox <- renderUI({
    color <- ifelse(prob_diab() >= 50, "red", "green")
    div(style = paste("color:white; background-color:", color, "; padding:10px; border-radius:5px;"),
        kda_pred_text())
  })
  
  # Affichage du score sous forme de texte
  output$scoreText <- renderText({ 
    paste("Probabilité d'être diabétique :", prob_diab(), "%") 
  })
  
  # Barre de progression pour le score
  output$scoreProgress <- renderUI({
    req(prob_diab())  # Vérifie que la valeur est disponible
    tagList(
      div(style = "width: 100%; background-color: #ddd; border-radius: 10px;",
          div(style = paste("width:", prob_diab(), "%; background-color: lightblue; 
                             text-align: center; padding: 5px; border-radius: 10px;"),
              paste(prob_diab(), "%"))
      )
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
