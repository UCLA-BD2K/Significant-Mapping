# load packages
library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Mapping Regions of Diseases with Statistically Significant Publication Frequencies", 
             windowTitle = "Regions vs Publications"), br(),

  sidebarLayout(
    sidebarPanel(
      radioButtons("disease", "Choose a disease system from the annotated set:",
                   c("All" = "All ACCRs",
                     "Cancer" = "Cancer ACCRs",
                     "Nervous" = "Nervous ACCRs",
                     "Cardiovascular" = "Cardiovascular ACCRs",
                     "Musculoskeletal and Rheumatological" = "Musculoskeletal and Rheumatological ACCRs",
                     "Digestive" = "Digestive ACCRs",
                     "Obstetrical and Gynecological" = "obst_gyn",
                     "Infectious" = "infectious",
                     "Respiratory Tract" = "resp",
                     "Hematological" = "hema",
                     "Kidney and Urological" = "kid_uro",
                     "Endocrine" = "endo",
                     "Oral and Maxillofacial" = "oral_max",
                     "Ophthalamological" = "oph",
                     "Otorhinolaryngological" = "otor",
                     "Skin" = "skin",
                     "Rare" = "rare",
                     "None: will search by phrase below" = "phrase",
                     "None: will search by PMID below" = "none")),
  
    textInput(inputId = "search",
              label = "Type your disease here:",
              value = "", 
              placeholder = "example: Tetralogy of Fallot"),
    
    textInput(inputId = "pmid",
              label = "Type all of your PMIDs here:",
              value = "", 
              placeholder = "example: 29767142 29762612 29757331 29755335"),
    
    textInput(inputId = "name",
              label = "Name the category of these case reports:",
              value = "", 
              placeholder = "example: Plasminogens, Cancers, etc"),
    
    radioButtons("decade_option", "Will you be mapping by decade?", c("No" = "no", "Yes" = "yes")),
    
    radioButtons("decade2", "Choose the decade you would like to map:", 
                 c("1950" = "1950", "1960" = "1960", "1970" = "1970", "1980" = "1980", 
                   "1990" = "1990", "2000" = "2000", "2010" = "2010")),
    
    radioButtons("mapping", "Choose an option for mapping:",
                 c("Raw Count" = "raw",
                   "Standardized by Proportion" = "standard"))),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text1"), #dataTableOutput("df"),
      plotlyOutput("map", width = "100%", height = "800px"), 
      fluidRow(
        column(width = 6, tableOutput("state_user")), 
        column(width = 1.5),
        column(width = 6, tableOutput("world_user")))
    )
  )
))
