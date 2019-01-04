# DFS Optimizer UI and Web Application
# by Fraser Walker

library(shiny)
library(tidyr)
library(data.table)
library(gurobi)
library(stringdist)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("dkInput", label = "DraftKings Salaries"),
      fileInput("fourInput", label = "4for4 Projections"),
      uiOutput("playerOutput"),
      uiOutput("actionButton")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Salaries", tableOutput(outputId = "salaries")),
                  tabPanel("Projections", tableOutput(outputId = "projections")),
                  tabPanel("Matching", tableOutput(outputId = "matching")),
                  tabPanel("Optimizer", verbatimTextOutput(outputId = "lineup"))
                  )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dk <- reactive({ # DK salaries dataframe
    
    if (is.null(input$dkInput)){
      return(NULL)
    }
    
    inFile <- input$dkInput
    read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE) %>%
      select(dkId = ID, 
             dkName = Name, 
             dkPosition = Position, 
             dkTeam = TeamAbbrev, 
             dkPpg = AvgPointsPerGame,
             dkSalary = Salary)
  })
  
  proj <- reactive({ # 4for4 projections dataframe
    
    if (is.null(input$fourInput)){
      return(NULL)
    }
    
    inFile <- input$fourInput
    read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  distMat <- reactive({
    stringdistmatrix(dk()$dkName, proj()$Player)
  })
  
  l <- reactive({
    apply(distMat(), 1, function(x)  which(x <= 5))
  })
  
  lt <- reactive({
    if (length(l()) == 0){
      return(NULL)
    } else {
      tmp <- l()
      for (i in 1:length(tmp)){
        if (length(tmp[[i]]) == 0){
          tmp[[i]] <- NA
        }
      }
    }
    transpose(tmp)
  })
  
 
  df <- reactive({
    df <- as.data.frame(lt(),
                  col.names = paste0("match", c(1:length(lt())))) %>%
      cbind(., dk()) %>%
      gather(., key = number, value = index, -starts_with("dk")) %>%
      filter(!is.na(index) |
               (is.na(index) & number == "match1")) %>%
      mutate(fourName = proj()$Player[index],
             fourPosition = proj()$Pos[index],
             fourTeam = proj()$Team[index],
             fourId = proj()$PID[index],
             fourProj = proj()$FFPts[index],
             stringDist = stringdist(dkName, fourName),
             similarity = stringsim(dkName, fourName),
             qualityScore = 1*(stringDist <= 4) +
               1*(dkPosition == fourPosition) +
               1*(dkTeam == fourTeam)
      ) %>%
      arrange(dkId) %>%
      filter(qualityScore >= 2) %>%
      group_by(fourId) %>%
      top_n(1, qualityScore) %>%
      top_n(1, similarity) %>%
      group_by(dkId) %>%
      top_n(1, qualityScore) %>%
      top_n(1, similarity) %>%
      bind_rows(., filter(dk(), dkPosition == "DST")) %>%
      mutate(
        qb = 1*(dkPosition == 'QB'),
        rb = 1*(dkPosition == 'RB'),
        wr = 1*(dkPosition == 'WR'),
        te = 1*(dkPosition == 'TE'),
        flex = 1*(dkPosition == 'RB' | dkPosition == 'WR'),
        dst = 1*(dkPosition == 'DST'),
        proj = if_else(is.na(fourProj) & dst == 1, dkPpg, fourProj)
      ) %>%
      ungroup() %>%
      select(dkName, proj, dkSalary, qb, rb, wr, te, flex, dst)
  })
  
  final <- reactive({
    if(input$action == 1){
      final <- df() %>%
        mutate(require = 1*(dkName %in% input$playerInput)) 
    }
  })
  
  name <- reactive ({
    final()$dkName
  })
  
  gurobiModel <- reactive({
    tmp1 <- final() 
   
    tmp <- as.matrix(tmp1[,-1]) %>%
      t()
    
    A <- tmp[-1,]
    obj <- tmp[1,]
    
    model <- list()
    
    model$A <- A
    model$obj <- obj
    model$modelsense <- 'max'
    
    model$rhs        <- c(50000,1,3,4,1,6,1
                          , length(input$playerInput) # 'require' optional for specific player(s)
    )
    model$sense      <- c('<', '=', '<', '<', '=', '=', '='
                          , '=' # 'require' optional for specific player(s)
    )
    model$vtype      <- 'B'
    
    result <- gurobi(model)
    
    result
  })
  
  output$playerOutput <- renderUI({
    if (is.null(input$dkInput) | is.null(input$fourInput)){
      return(NULL)
    }
    tmp <- df()
    selectizeInput(inputId = "playerInput",
                   label = "Require",
                   choices = list(
                     QB = select(filter(tmp, qb == 1), QB = dkName),
                     RB = select(filter(tmp, rb == 1), RB = dkName)
                   ),
                   multiple = TRUE)
  })
  
  output$actionButton <- renderUI({
    tmp <- input$playerInput
    actionButton(inputId = "action", label = "Run")
  })
  
  # observe({print(dim(mat()))})
  # observe({print(length(l()))})
   observe({print(length(input$playerInput))})
  # observe({print(input$action)})
  
  output$salaries <- renderTable({
    dk()
  })
  
  output$projections <- renderTable({
    proj()
  })
  
  output$matching <- renderTable({
    final()
  })
  
  output$lineup <- renderPrint({
    ind <- which(gurobiModel()$x > c(0.9))
    tmp <- name()
    tmp[ind]
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)