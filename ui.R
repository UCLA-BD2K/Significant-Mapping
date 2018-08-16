# load packages
library(shiny)
library(plotly)
library(shinycssloaders)

shinyUI(fluidPage(
  titlePanel("Mapping Regions of Diseases with Statistically Significant Publication Frequencies", 
             windowTitle = "Regions vs Publications"), br(),
  
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput(inputId = "disease", label = "Choose a disease system from the annotated set:",
                         choices = c(
                           "Cancer" = "Cancer",
                           "Nervous" = "Nervous System Diseases",
                           "Cardiovascular" = "Cardiovascular Diseases",
                           "Musculoskeletal and Rheumatological" = "Musculoskeletal Diseases and Rheumatological Diseases",
                           "Digestive" = "Digestive System Diseases",
                           "Obstetrical and Gynecological" = "Obstetrical and Gynecological Diseases",
                           "Infectious" = "Infectious Diseases",
                           "Respiratory Tract" = "Respiratory Tract Diseases",
                           "Hematological" = "Hematologic Diseases",
                           "Kidney and Urological" = "Kidney Diseases and Urological Diseases",
                           "Endocrine" = "Endocrine System Diseases",
                           "Oral and Maxillofacial" = "Oral and Maxillofacial Diseases",
                           "Ophthalamological" = "Ophthalamological Diseases",
                           "Otorhinolaryngological" = "Otorhinolaryngological Diseases",
                           "Skin" = "Skin Diseases",
                           "Rare" = "Rare Diseases",
                           "All (containing any of the diseases listed above)" = "All ACCRs"),
                         selected = c("All (containing any of the diseases listed above)" = "All ACCRs")),
      
      
      radioButtons(inputId = "disease", label = "",
                   choices = c("None: will search by phrase below" = "phrase",
                               "None: will search by PMID below" = "none"), selected = character(0)),
      
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
      withSpinner(plotlyOutput("map", width = "100%", height = "800px")), 
      fluidRow(
        column(width = 6, tableOutput("state_user")), 
        column(width = 1.5),
        column(width = 6, tableOutput("world_user")))
    )
  )
))
