# load libraries
library(shiny)
library(dplyr)
library(leaflet)
library(ggmap)
library(rgdal)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(plotly)
library(grDevices)
library(stringr)
library(gridExtra)
library(DT)
library(rentrez)
library(XML)
library(stringr)
library(webshot) 
library(shinycssloaders)

# load datasets
load(file = "cleaned_accr.Rdata")
load(file = "country_all.Rdata")

# try new country method (unique from the world map)
#country_all <- data.frame(unique(world$region))
#names(country_all) <- "Var1"

load(file = "us_state_all.Rdata")
load(file = "location.Rdata")

load(file = "countries.Rdata")
load(file = "country_capitals.Rdata")
load(file = "major_global_cities.Rdata")
load(file = "US_cities.Rdata")
load(file = "usstates.Rdata")
load(file = "state_abs.Rdata")

state <- map_data(map = "state")
world <- map_data(map = "world")

shinyServer(function(input, output) {
  
  output$text1 <- renderText({
    if (input$disease != "phrase" || input$disease != "none")
    {
      # outputs names of all diseases selected 
      i <- 1
      paste("You have chosen: ")
      repeat{
        paste(input$disease[i], " and ")
        i <- i+1
        if(i > length(input$disease))
          break
      }
    }
    else if (input$disease == "phrase") # show phrase searching
    {
      paste("You have chosen: ", input$search)
    }
    else if (input$disease == "none") # show PMID search
    {
      paste("You have chosen: ", input$name)
    }
  })
  
  # subset dataframe based on user input
  subset_accr <- reactive({
    
    # subset by decade
    if (input$decade_option == "yes")
    {
      #dec <- as.factor(input$decade)
      #accr <- accr[accr$decade == dec, ]
      
      if (input$decade2 == "1950")
      {
        accr <- accr[accr$decade == 1950, ]
      }
      
      if (input$decade2 == "1960")
      {
        accr <- accr[accr$decade == 1960, ]
      }
      
      if (input$decade2 == "1970")
      {
        accr <- accr[accr$decade == 1970, ]
      }
      
      if (input$decade2 == "1980")
      {
        accr <- accr[accr$decade == 1980, ]
      }
      
      if (input$decade2 == "1990")
      {
        accr <- accr[accr$decade == 1990, ]
      }
      
      if (input$decade2 == "2000")
      {
        accr <- accr[accr$decade == 2000, ]
      }
      
      if (input$decade2 == "2010")
      {
        accr <- accr[accr$decade == 2010, ]
      }
    }
    
    if (all(input$disease == "All ACCRs"))
    {
      accr <- accr[ , 22:23]
    } 
    
    else{
    for (i in 1:length(input$disease)) {
      
      # gets one of the diseases selected 
      current_disease <- input$disease[i]
      
      # creates subset of previous data set with all case reports that contain that specific disease
      if(current_disease != "All ACCRs")
      accr <- accr[accr[,current_disease] == 1,]
    }
      
    # extracts country and state (is applicable) of all case reports that fit the requirements
    accr <- accr[, 22:23]
    }
    
    # return accr dataframe based on diseases selected
    return(accr)
  })
   
  # dataframe of user's PMIDs
  df <- reactive({
    
    # if (input$disease == "none")
    # {
    #   df <- data.frame(strsplit(input$pmid, " "))
    # }
    # 
    # # rename column
    # names(df) <- "PMID"
    # 
    # # which user based PMID indices are in the entire one
    # a <- which(location$PMID %in% df$PMID)
    # 
    # # return only user based PMID dataframe
    # final <- as.data.frame(location[a, ])
    
    if (input$disease == "none")
    {
      
      # number of articles for term searched
      # count <- str_count(input$pmid, " ") + 1
      
      # split string
      split <- strsplit(input$pmid, " ")
      
      # split into vector instead of list
      split <- as.vector(split[[1]])
      split <- as.numeric(split)
      split <- split[!is.na(split)]
      
      # empty vector that will soon contain locations
      location <- character()

      # get all location data 
      for (i in 1:length(split))
      {
        # get ID of each search
        test <- entrez_fetch(db = "pubmed", id = split[[i]], rettype = "XML")
        
        # convert to XML
        test_list <- XML::xmlToList(test)
        
        # retrieve location
        location <- c(location, test_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$AffiliationInfo$Affiliation)
      }
      
      # make into dataframe and prep for location extraction
      data <- as.data.frame(location)
      data$location <- tolower(data$location)
      
      data$country <- ""
      data$us_state <- ""
      
      # string match the countries
      data$country <- str_extract(data$location, paste0("\\b", countries$Name, "\\b", collapse = "|"))
      
      # string match the US states
      data$us_state <- str_extract(data$location, paste0("\\b", usstates$V1, "\\b", collapse = "|"))
      
      # string match the top US cities
      data$us_cities <- ifelse(is.na(data$us_state) , str_extract(data$location, paste0("\\b", US_cities$City, "\\b", collapse = "|")), "")
      data <- left_join(data, US_cities, by = c("us_cities" = "City"))
      data$us_state <- ifelse(is.na(data$us_state), data$State, data$us_state)
      data$State <- NULL
      
      # string match country capitals
      data$capitals <- ifelse(is.na(data$country) , str_extract(data$location, paste0("\\b", country_capitals$CapitalName, "\\b", collapse = "|")), "")
      data <- left_join(data, country_capitals, by = c("capitals" = "CapitalName"))
      data$country <- ifelse(is.na(data$country), data$CountryName, data$country)
      
      # string match the major global cities
      data$world_cities <- ifelse(is.na(data$country) , str_extract(data$location, paste0("\\b", major_global_cities$City, "\\b", collapse = "|")), "")
      data <- left_join(data, major_global_cities, by = c("world_cities" = "City"))
      data$country <- ifelse(is.na(data$country), data$Country, data$country)
      
      # string match usa state abbreviations
      cond <- is.na(data$us_state) & (is.na(data$country) | data$country == "United States")
      data$abs <- data$us_state
      data$abs[cond] <- str_extract(data$location[cond], paste0("\\b", state_abs$Abbreviation, "\\b", collapse = "|"))
      data <- left_join(data, state_abs, by = c("abs" = "Abbreviation"))
      data$us_state <- ifelse(is.na(data$us_state), data$State, data$us_state)
      
      # hardcode uk and usa abbreviations as well as mapping peking to China and others
      data$country <- ifelse(str_detect(string = data$location, pattern = "\\buk\\b"), "United Kingdom", data$country)
      data$country <- ifelse(grepl("\\busa\\b", data$location), "United States", data$country)
      data$country <- ifelse(grepl("peking", data$location), "China", data$country)
      data$us_state <- ifelse(grepl("stanford", data$location), "california", data$us_state)
      data$us_state <- ifelse(grepl("palo alto", data$location), "california", data$us_state)
      data$country <- ifelse(grepl("england", data$location), "United Kingdom", data$country)
      
      # vector of all US states
      states <- usstates$V1[1:50]
      
      # if state is in the USA, change country name to USA
      data$country <- ifelse(data$us_state %in% states, "United States", data$country)
      
      # make all countries and states title case
      data$country <- str_to_title(data$country)
      data$us_state <- str_to_title(data$us_state)
      
      # remove all unnecessary variables
      data <- data[ , -which(names(data) %in% c("us_cities", "State", "capitals", "CountryName", "world_cities", "Country", "abs"))]
    }
      
      # return dataframe
      return(data)
    })
  
  df2 <- reactive({
    
    if (input$disease == "phrase")
    {
      # number of articles for term copd
      count <- entrez_search(db = "pubmed", term = input$search)$count
      
      # set max to count
      id <- entrez_search(db = "pubmed", term = input$search, retmax = count)$ids
      
      # empty vector that will soon contain locations
      location <- character()
      
      # get all location data 
      for (i in 1:count)
      {
        # get ID of each search
        test <- entrez_fetch(db = "pubmed", id = id[i], rettype = "XML")
        
        # convert to XML
        test_list <- XML::xmlToList(test)
        
        # retrieve location
        location <- c(location, test_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$AffiliationInfo$Affiliation)
      }
      
      # make into dataframe and prep for location extraction
      data <- as.data.frame(location)
      data$location <- tolower(data$location)
      
      data$country <- ""
      data$us_state <- ""
      
      # string match the countries
      data$country <- str_extract(data$location, paste0("\\b", countries$Name, "\\b", collapse = "|"))
      
      # string match the US states
      data$us_state <- str_extract(data$location, paste0("\\b", usstates$V1, "\\b", collapse = "|"))
      
      # string match the top US cities
      data$us_cities <- ifelse(is.na(data$us_state) , str_extract(data$location, paste0("\\b", US_cities$City, "\\b", collapse = "|")), "")
      data <- left_join(data, US_cities, by = c("us_cities" = "City"))
      data$us_state <- ifelse(is.na(data$us_state), data$State, data$us_state)
      data$State <- NULL
      
      # string match country capitals
      data$capitals <- ifelse(is.na(data$country) , str_extract(data$location, paste0("\\b", country_capitals$CapitalName, "\\b", collapse = "|")), "")
      data <- left_join(data, country_capitals, by = c("capitals" = "CapitalName"))
      data$country <- ifelse(is.na(data$country), data$CountryName, data$country)
      
      # string match the major global cities
      data$world_cities <- ifelse(is.na(data$country) , str_extract(data$location, paste0("\\b", major_global_cities$City, "\\b", collapse = "|")), "")
      data <- left_join(data, major_global_cities, by = c("world_cities" = "City"))
      data$country <- ifelse(is.na(data$country), data$Country, data$country)
      
      # string match usa state abbreviations
      cond <- is.na(data$us_state) & (is.na(data$country) | data$country == "United States")
      data$abs <- data$us_state
      data$abs[cond] <- str_extract(data$location[cond], paste0("\\b", state_abs$Abbreviation, "\\b", collapse = "|"))
      data <- left_join(data, state_abs, by = c("abs" = "Abbreviation"))
      data$us_state <- ifelse(is.na(data$us_state), data$State, data$us_state)
      
      # hardcode uk and usa abbreviations as well as mapping peking to China and others
      data$country <- ifelse(str_detect(string = data$location, pattern = "\\buk\\b"), "United Kingdom", data$country)
      data$country <- ifelse(grepl("\\busa\\b", data$location), "United States", data$country)
      data$country <- ifelse(grepl("peking", data$location), "China", data$country)
      data$us_state <- ifelse(grepl("stanford", data$location), "california", data$us_state)
      data$us_state <- ifelse(grepl("palo alto", data$location), "california", data$us_state)
      data$country <- ifelse(grepl("england", data$location), "United Kingdom", data$country)
      
      # vector of all US states
      states <- usstates$V1[1:50]
      
      # if state is in the USA, change country name to USA
      data$country <- ifelse(data$us_state %in% states, "United States", data$country)
      
      # make all countries and states title case
      data$country <- str_to_title(data$country)
      data$us_state <- str_to_title(data$us_state)
      
      # remove all unnecessary variables
      data <- data[ , -which(names(data) %in% c("us_cities", "State", "capitals", "CountryName", "world_cities", "Country", "abs"))]
      
    }
    
    return(data)
    
  })
  
  
  
world_table <- reactive({
  
  # load subsetted dataframe
  accr <- subset_accr()
  
  # if doing via PMIDs
  if (input$disease == "none")
  {
    accr <- df()
  }
  
  # if doing via search
  if (input$disease == "phrase")
  {
    accr <- df2()
  }
  
  # tables of country and states
  country <- data.frame(table(accr$country))
  
  # add proportions
  country$prop <- country$Freq/sum(country$Freq)
  
  # join tables
  country_all <- left_join(country_all, country, by = "Var1")
  
  # add in zeroes if NA
  country_all$Freq.y <- ifelse(is.na(country_all$Freq.y), 0, country_all$Freq.y)
  country_all$prop.y <- ifelse(is.na(country_all$prop.y), 0, country_all$prop.y)

  # find and remove those countries that don't satisfy chi square conditions
  count <- numeric(0)
  
  for (i in 1:nrow(country_all))
  {
    a <- c(country_all$Freq.x[i], country_all$Freq.y[i])
    b <- c(sum(country_all$Freq.x), sum(country_all$Freq.y, na.rm = T)) # dim of each variables
    m <- matrix(c(a, b-a), ncol = 2)
    if (sum(chisq.test(m)$expected > 5) != 4)
    {
      count <- append(count, i)
    }
  }
  
  # only remove if mapping option is to standardize
  if (input$mapping == "standard")
  {
    country_all <- country_all[-count, ]
  }
  
  # find p-values for country
  for (i in 1:nrow(country_all))
  {
    # garbage p-value if raw mapping
    if (input$mapping == "raw")
    {
      country_all$pvalue[i] <- 0.005
    }
    else
    {
      # calculate raw occurrences
      prop_1 <- country_all[i, 2]
      prop_2 <- country_all[i, 4]
      
      # calculate p-value
      p <- prop.test(x = c(prop_1, prop_2), n = c((country_all[1, 2]/country_all[1, 3]), 
                                                  country_all[1, 4]/country_all[1, 5]), correct = TRUE)
      p <- p$p.value
      country_all$pvalue[i] <- p
    }
  }
  
  # countries: order by significant and increasing p-values
  country_all <- country_all[country_all$pvalue <= 0.05, ]
  country_all <- country_all[order(country_all$pvalue), ]
  
  # countries: find difference between proportions
  country_all$diffs <- country_all$prop.x - country_all$prop.y
  
  # countries: create factor differences
  for (i in 1:nrow(country_all))
  {
    if (country_all$diffs[i] < 0)
    {
      country_all$factor[i] <- -country_all$prop.y[i]/country_all$prop.x[i]
    }
    else if (country_all$diffs[i] > 0)
    {
      country_all$factor[i] <- country_all$prop.x[i]/country_all$prop.y[i]
    }
    else
    {
      country_all$factor[i] <- 1
    }
  }
  
  # countries: substitute infinite values
  for (i in 1:nrow(country_all))
  {
    if (country_all$factor[i] == Inf)
    {
      country_all$factor[i] <- country_all$prop.x[i]
    }
    if (country_all$factor[i] == -Inf)
    {
      country_all$factor[i] <- country_all$prop.y[i]
    }
  }
  
  # countries: make 1/factor prop for those "less than 1" and reverse scale so they are country_all positive
  country_all$factor <- ifelse(country_all$factor > 0, 1/country_all$factor, -country_all$factor)
  
  # countries: to deal with 0 cases
  country_all$factor <- ifelse(country_all$Freq.y == 0, 1/(country_all[1, 4]/country_all[1, 5]),
                               country_all$factor)

  # return table
  return(country_all)
})

world_user <- reactive({
  
  # load data
  country_all <- world_table()
  
  # remove statistical variables to be user friendly
  country_all <- country_all[ , c(1:5, 8)]
  
  # if using PMIDs
  if (input$disease == "none")
  {
    name <- input$name
  }
  
  # if searching
  else if (input$disease == "phrase")
  {
    name <- input$name
  }
  else
  {
    name <- input$disease
  }
  
  # rename variables
  names(country_all) <- c("Country", "All CR's Count", "All CR's Proportion", paste0(name, "'s Count"), 
                           paste0(name, "'s Proportion"), "Factor")
  
  # remove decimals
  country_all[ , 4] <- round(country_all[ , 4], 0)
  
  # order by factor if standardized
  if (input$mapping == "standard")
  {
    country_all <- country_all[order(-country_all$Factor), ]
  }
  
  # order by frequency if raw
  if (input$mapping == "raw")
  {
    country_all <- country_all[order(-country_all[4]), ]
  }

  
  # return table
  return(country_all)
})
  
df_world <- reactive({
    
  # load data
  country_all <- world_table()
  
  # world and state map data
  world <- map_data(map = "world")
  
  # fix UK error
  world$region <- str_replace_all(world$region, "UK", "United Kingdom")
  
  # remove Antarctica rip :(
  world <- world[world$region != "Antarctica", ]
  
  # fix Korea error
  country_all$Var1 <- str_replace_all(country_all$Var1, "Korea", "South Korea")
  
  # merge world with countries data
  names(country_all)[1] <- "region" # rename just like in the world df
  world <- left_join(world, country_all)
  
  # clean up
  names(world)[5] <- "Region"
  names(world)[7] <- "Frequency"
  
  # round the factors
  world$factor <- round(world$factor, 4)
  
  # return dataframe
  return(world)
  
  })
  
  state_table <- reactive({
    
    # load subsetted dataframe
    accr <- subset_accr()
    
    # if doing via PMIDs
    if (input$pmid == "none")
    {
      accr <- df()
    }
    
    # if doing via phrase
    if (input$pmid == "phrase")
    {
      accr <- df2()
    }
    
    # tables of country and states
    state <- data.frame(table(accr$us_state))
    
    # add proportions
    state$prop <- state$Freq/sum(state$Freq)
    
    # join tables
    us_state_all <- left_join(us_state_all, state, by = "Var1")
    
    # add in zeroes if NA only if mapping is standardized
    us_state_all$Freq.y <- ifelse(is.na(us_state_all$Freq.y), 0, us_state_all$Freq.y)
    us_state_all$prop.y <- ifelse(is.na(us_state_all$prop.y), 0, us_state_all$prop.y)
      
    # find and remove those states that don't satisfy chi square conditions
    count <- numeric(0)
    
    for (i in 1:nrow(us_state_all))
    {
      a <- c(us_state_all$Freq.x[i], us_state_all$Freq.y[i])
      b <- c(sum(us_state_all$Freq.x), sum(us_state_all$Freq.y, na.rm = T)) # dim of each variables
      m <- matrix(c(a, b-a), ncol = 2)
      if (sum(chisq.test(m)$expected > 5) != 4)
      {
        count <- append(count, i)
      }
    }
    
    # only remove states if want to standardize map
    if (input$mapping == "standard")
    {
      us_state_all <- us_state_all[-count, ]
    }
    
    # find p-values for states
    for (i in 1:nrow(us_state_all))
    {
      # garbage p-value if raw mapping
      if (input$mapping == "raw")
      {
        us_state_all$pvalue[i] <- 0.005
      }
      else
      {
        # calculate raw occurrences
        prop_1 <- us_state_all[i, 2]
        prop_2 <- us_state_all[i, 4]
        
        # calculate p-value
        p <- prop.test(x = c(prop_1, prop_2), n = c((us_state_all[1, 2]/us_state_all[1, 3]), 
                                                    us_state_all[1, 4]/us_state_all[1, 5]), correct = TRUE)
        p <- p$p.value
        us_state_all$pvalue[i] <- p
      }
    }
    
    # states: order by significant and increasing p-values
    us_state_all <- us_state_all[us_state_all$pvalue <= 0.05, ]
    us_state_all <- us_state_all[order(us_state_all$pvalue), ]
    
    # states: find difference between proportions
    us_state_all$diffs <- us_state_all$prop.x - us_state_all$prop.y
    
    # states: create factor differences
    for (i in 1:nrow(us_state_all))
    {
      if (us_state_all$diffs[i] < 0)
      {
        us_state_all$factor[i] <- -us_state_all$prop.y[i]/us_state_all$prop.x[i]
      }
      else if (us_state_all$diffs[i] > 0)
      {
        us_state_all$factor[i] <- us_state_all$prop.x[i]/us_state_all$prop.y[i]
      }
      else
      {
        us_state_all$factor[i] <- 1
      }
    }
    
    # states: substitute infinite values
    for (i in 1:nrow(us_state_all))
    {
      if (us_state_all$factor[i] == Inf)
      {
        us_state_all$factor[i] <- us_state_all$prop.x[i]
      }
      if (us_state_all$factor[i] == -Inf)
      {
        us_state_all$factor[i] <- us_state_all$prop.y[i]
      }
    }
    
    # states: make 1/factor prop for those "less than 1" and reverse scale so they are us_state_all positive
    us_state_all$factor <- ifelse(us_state_all$factor > 0, 1/us_state_all$factor, -us_state_all$factor)
    
    # states: to deal with 0 cases
    us_state_all$factor <- ifelse(us_state_all$Freq.y == 0, 1/(us_state_all[1, 4]/us_state_all[1, 5]),
                                  us_state_all$factor)
    
    # return table
    return(us_state_all)
  })
  
  state_user <- reactive({
    us_state_all <- state_table()
    
    # remove statistical variables to be user friendly
    us_state_all <- us_state_all[ , c(1:5, 8)]
    
    # if using PMIDs
    if (input$disease == "none")
    {
      name <- input$name
    }
    
    # if searching
    else if (input$disease == "phrase")
    {
      name <- input$name
    }
    else
    {
      name <- input$disease
    }
    
    # rename variables
    names(us_state_all) <- c("US State", "All CR's Count", "All CR's Proportion", paste0(input$disease, "'s Count"), 
                             paste0(name, "'s Proportion"), "Factor")

    # remove decimals
    us_state_all[ , 4] <- round(us_state_all[ , 4], 0)
    
    # order by factor if standardized
    if (input$mapping == "standard")
    {
      us_state_all <- us_state_all[order(-us_state_all$Factor), ]
    }
    
    # order by frequency if raw
    if (input$mapping == "raw")
    {
      us_state_all <- us_state_all[order(-us_state_all[4]), ]
    }
    
    # return table
    return(us_state_all)
  })
    
  df_state <- reactive({
    
    us_state_all <- state_table()
    
    # world and state map data
    state <- map_data(map = "state")
    
    # add alaska and hawaii
    alaska <- world[which(world$subregion == "Alaska"), ]
    alaska[ , 6] <- NA
    alaska$region <- "alaska"
    state <- rbind(alaska, state)
    alaska$group <- 100
    
    hawaii <- world[which(world$subregion == "Hawaii"), ]
    hawaii[ , 6] <- NA
    hawaii$region <- "hawaii"
    hawaii$group <- 99
    
    state <- rbind(hawaii, state)
    
    # merge state with US data
    us_state_all$Var1 <- tolower(us_state_all$Var1) # lowercase just like in the state df
    names(us_state_all)[1] <- "region" # rename just like in the state df
    state <- left_join(state, us_state_all)
    
    # clean up
    names(state)[5] <- "Region"
    names(state)[7] <- "Frequency"
    state$Region <- str_to_title(state$Region)
    
    # round the factors
    state$factor <- round(state$factor, 4)
    
    # return dataframe
    return(state)
  })
  
  map <- reactive({
    
  # load datasets
    state <- df_state()
    world <- df_world()

  # make map of world based on mapping option
    if (input$mapping == "standard")
    {
      p1 <- ggplot(data = world, aes(x = long, y = lat, Region = Region, fill = factor, group = group)) +
        geom_polygon(color = "white", show.legend = FALSE)
    }
    if (input$mapping == "raw")
    {
      # change 0 to NA so doesn't show on map
      world$Freq.y <- ifelse(world$Freq.y == 0, NA, world$Freq.y)
      
      p1 <- ggplot(data = world, aes(x = long, y = lat, Region = Region, fill = Freq.y, group = group)) +
        geom_polygon(color = "white", show.legend = FALSE)
    }

  # title of graph
  if (input$disease == "none")
  {
    name <- input$name
  }
  else if (input$disease == "phrase")
  {
    name <- input$search
  }
  else
  {
    name <- input$disease
  }
    
    # retitle by decade 
    if (input$decade_option == "yes")
    {
      if (input$decade2 == "1950")
      {
        name <- paste0(name, " in 1950")
      }
      
      if (input$decade2 == "1960")
      {
        name <- paste0(name, " in 1960")
      }
      
      if (input$decade2 == "1970")
      {
        name <- paste0(name, " in 1970")
      }
      
      if (input$decade2 == "1980")
      {
        name <- paste0(name, " in 1980")
      }
      
      if (input$decade2 == "1990")
      {
        name <- paste0(name, " in 1990")
      }
      
      if (input$decade2 == "2000")
      {
        name <- paste0(name, " in 2000")
      }
      
      if (input$decade2 == "2010")
      {
        name <- paste0(name, " in 2010")
      }
    }
    
    # combine map of world with US states map
    if (input$mapping == "standard")
    {
      p2 <- p1 + geom_polygon(data = state, color = "white" , aes(fill = factor)) + guides(fill = guide_legend()) +
        ggtitle(label = paste0("Regions whose Publication Frequency of ", name, 
                               " to \nAll Case Reports are Statistically Significant")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "", y = "") +
        scale_fill_continuous(low = "lightblue", high = "darkblue",
                              guide = "colorbar", na.value = "grey") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", colour = "white"),
              axis.line = element_line(colour = "white"),
              axis.ticks = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank())
    }
    if (input$mapping == "raw")
    {
      # change 0 to NA so it doesn't show up on map
      state$Freq.y <- ifelse(state$Freq.y == 0, NA, state$Freq.y)
      
      p2 <- p1 + geom_polygon(data = state, color = "white" , aes(fill = Freq.y)) + guides(fill = guide_legend()) +
        ggtitle(label = paste0("Raw Publication Frequency Count of ", name)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "", y = "") +
        scale_fill_continuous(low = "lightblue", high = "darkblue",
                              guide = "colorbar", na.value = "grey") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", colour = "white"),
              axis.line = element_line(colour = "white"),
              axis.ticks = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank())
    }
    
    # plot it!
    if (input$mapping == "standard")
    {
      a <- ggplotly(p2, tooltip = c("Region", "factor"))
    }
    if (input$mapping == "raw")
    {
      a <- ggplotly(p2, tooltip = c("Region", "Freq.y"))
    }
    
    # save image as an html
    htmlwidgets::saveWidget(as_widget(a), "graph.html")
    
  return(a)
})
  
  output$map <- renderPlotly({map()})
  
  output$state_user <- renderTable({state_user()})
  output$world_user <- renderTable({world_user()})
  
  # output$savePlot <- downloadHandler(
  #   filename = "file.png", 
  #   content = function(file) {
  #     plotly_IMAGE(patientCircleInput(), format = "png", out_file = file)
  #     plot = map()
  #     dev.off()
  #   }
  # )
  #output$df <- renderDataTable({df()})
  
})
