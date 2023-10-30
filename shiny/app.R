# Load the required packages
library(shiny)
library(data.table)
library(ggplot2)
library(corrplot)

# Preloading the data
dat <- fread("data/variables20000.csv.gz")

# Listing numeric variables
vars <- colnames(dat)[
  sapply(dat, class) %in% c("integer", "numeric")
]

# Show the correlation with is_fraud
corr_fraud <- sapply(vars, function(x) cor(dat[[x]], dat$is_fraud))

corr_fraud <- data.table(
  var  = vars,
  corr = corr_fraud,
  corr_abs = abs(corr_fraud)
)

# Sorting
corr_fraud <- corr_fraud[!is.na(corr)]
setorder(corr_fraud, -corr_abs)

# Keeping the top 100
vars <- corr_fraud$var[1:100]


# Define the UI
ui <- fluidPage(
  titlePanel("Variables for predicting fraud"),
  p("Author: George G. Vega Yon, Ph.D."),
  p("This shiny app (including the data processing), was generated on October 30th, between 2.10 PM and 4 PM."),
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Select a method:", choices = c("circle", "square", "ellipse", "number", "shade", "color")),
      selectInput("type", "Select a type:", choices = c("full", "lower", "upper")),
      sliderInput("sig.level", "Significance Level:", min = 0, max = 1, value = 0.05, step = 0.01),
      sliderInput(
        "corlevel", "Select corr with is_fraud:", min = 0, max = 1, value = 0.05, step = 0.01
      )
    ),
    mainPanel(
      p("The following is a figure showing the correlation between each of the possible predictions. Part of the shiny app was generated using GitHub co-pilot."),
      p("I would add more variables by including transformations like sqrt, 1/x, x^2 etc. Nevertheless, because of time, I just added the existing variables to the figure."),
      p("Both, the figure and the table, filter out observations based on the correlation level with is_fraud."),
      plotOutput("corrplot"),
      p("This table shows the correlation between predictors and is_fraud."),
      tableOutput("corrvars"),
      p("The data used in this figure was generated with the quarto file `data/variables.qmd`. It only includes a sample of 20,000 observations so that the app loads quickly.")
    )
  )
)

# Define the server
server <- function(input, output) {

  # Render the plot correlation with fraud
  output$corrplot <- renderPlot({

    # Subsetting
    which_vars <- corr_fraud[
      corr_abs >= input$corlevel, c(var)
    ]
    
    # idx <- which(colnames(correlation_dat) %in% which_vars)

    correlation_dat <- dat[, ..which_vars] |>
      as.matrix() |> cor(use = "pairwise.complete.obs")

    corrplot(
      correlation_dat,
      method = input$method,
      type = input$type, sig.level = input$sig.level, insig = "blank")
  })

  # Correlation table with fraud
  output$corrvars <- renderTable({
    corr_fraud[
      corr_abs >= input$corlevel,
      list("Variable" = var, "Cor" = corr) 
    ] 
  })
}

# Run the app
shinyApp(ui = ui, server = server)