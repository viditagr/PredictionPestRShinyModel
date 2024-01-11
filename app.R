# Load the shiny package
library(shiny)
library(ggplot2)
library(tidyr)

# Define UI for application
ui <- fluidPage(
  titlePanel(HTML('<span style="font-size: 60px;">
            Crop-Pest Interaction Model</span>')),
  #Write text explaining how to use this app
  h4("This app is designed to help you predict the population of a pest
     over time. You can also apply chemical and biological control
     interventions to see how they affect the population."),
  h4("To use this app, select the pest type, crop-pest interaction,
      and temperature conditions. Then, select the chemical and/or
      biological control interventions you want to apply. Finally,
      click the 'Apply Intervention' button to see the results. 
      As you change the parameters, the plot will update automatically."),
  h6("Note: If you face any errors then please click the 'Clear' button
      and try again."),
  sidebarLayout(
    sidebarPanel(
      h1("Model Parameters"),
      selectInput("pestType", "What pest type will you be modeling?",
                  choices = c("\a", "Agronomic", "Horticultural", "Forest")),
      selectInput("interactionType",
                  "What crop-pest interaction do you want to model?",
                  choices = c(""), selected = NULL),
      selectInput("tempConditions",
                  "What are the temperature conditions?",
                  choices = c("\a", "Cold", "Average", "Hot")),
      h1("Interventions"),
      h4("Chemical Control"),
      selectInput("targetChem", "What life stage are we targeting?",
                  choices = c("\a", "Adults", "Immatures")),
      numericInput("chemEfficacy",
                   "Efficacy of Chemical Control: (between 0 and 1)",
                   value = "", min = 0, max = 1),
      dateInput("chemDate", "Date of Implementation: (MM/DD)", value = ""),
      selectInput("chemFrequency", "How many times is the chemical applied:",
                  choices = c("\a", "Once", "Weekly", "Monthly")),
      h4("Biocontrol"),
      selectInput("bioTarget", "What life stage are we targeting?",
                  choices = c("\a", "Generalist", "Specialist")),
      numericInput("bioEfficacy",
                   "Efficacy of Biocontrol: (between 0 and 1)",
                   value = "", min = 0, max = 1),
      dateInput("bioDate", "Date of Implementation: (MM/DD)", value = ""),
      actionButton("clear", "Clear"),
      actionButton("apply", "Apply Intervention")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("selectedImageUI1"),
      plotOutput("plot", height = "700px")
    )
  )
)


apply_chemical_control <- function(df, control_params) {
  #Extract the parameters
  insectPlant <- control_params$interactionType
  stage <- control_params$targetChem
  efficacy <- control_params$chemEfficacy
  date <- as.Date(control_params$chemDate)
  # Change the year to 1998 no matter what
  date <- as.Date(paste0("1998-", format(date, "%m-%d")))
  frequency <- control_params$chemFrequency
  print(paste("insectPlant", insectPlant))
  print(paste("stage", stage))
  print(paste("efficacy", efficacy))
  print(paste("date", date))
  print(paste("frequency", frequency))
  # Add code here to deal with the frequency later
  if (efficacy == "0") {
    return(df)
  }
  if (insectPlant == "Corn Rootworm on Corn" ||
        insectPlant == "Japanese Beetle on Apple" ||
        insectPlant == "Spotted Wing Drosophila on Strawberry") {
    if (stage == "Immatures") {
      if(df[df$Date == date, "Immatures"] == 0) {
        return(df)
      }
      if (frequency == "Once") {
        df[df$Date >= date, "Immatures"] <-
          df[df$Date >= date, "Immatures"] * 0.65 * efficacy
        df[df$Date >= date + 10, "Adults"] <-
          df[df$Date >= date + 10, "Adults"] * 0.65 * efficacy
        df[df$Date >= date + 20, "Eggs"] <-
          df[df$Date >= date + 20, "Eggs"] * 0.65 * efficacy
      }
      if (frequency == "Weekly") {
        for (i in seq(from = date, to = max(df$Date), by = 7)) {
          df[df$Date >= i, "Immatures"] <-
            df[df$Date >= i, "Immatures"] * 0.65 * efficacy
          df[df$Date >= i + 10, "Adults"] <-
            df[df$Date >= i + 10, "Adults"] * 0.65 * efficacy
          df[df$Date >= i + 20, "Eggs"] <-
            df[df$Date >= i + 20, "Eggs"] * 0.65 * efficacy
        }
      } else if (frequency == "Monthly") {
        for (i in seq(from = date, to = max(df$Date), by = 30)) {
          df[df$Date >= i, "Immatures"] <-
            df[df$Date >= i, "Immatures"] * 0.65 * efficacy
          df[df$Date >= i + 10, "Adults"] <-
            df[df$Date >= i + 10, "Adults"] * 0.65 * efficacy
          df[df$Date >= i + 20, "Eggs"] <-
            df[df$Date >= i + 20, "Eggs"] * 0.65 * efficacy
        }
      }
    } else if (stage == "Adults") {
      if(df[df$Date == date, "Adults"] == 0) {
        return(df)
      }
      if (frequency == "Once") {
        df[df$Date >= date, "Adults"] <-
          df[df$Date >= date, "Adults"] * 0.6 * efficacy
        df[df$Date >= date + 10, "Eggs"] <-
          df[df$Date >= date + 10, "Eggs"] * 0.6 * efficacy
        df[df$Date >= date + 20, "Immatures"] <-
          df[df$Date >= date + 20, "Immatures"] * 0.6 * efficacy
      }
      if (frequency == "Weekly") {
        for (i in seq(from = date, to = max(df$Date), by = 7)) {
          df[df$Date >= i, "Adults"] <-
            df[df$Date >= i, "Adults"] * 0.6 * efficacy
          df[df$Date >= date + 10, "Eggs"] <-
            df[df$Date >= date + 10, "Eggs"] * 0.6 * efficacy
          df[df$Date >= date + 20, "Immatures"] <-
            df[df$Date >= date + 20, "Immatures"] * 0.6 * efficacy
        }
      } else if (frequency == "Monthly") {
        for (i in seq(from = date, to = max(df$Date), by = 30)) {
          df[df$Date >= i, "Adults"] <-
            df[df$Date >= i, "Adults"] * 0.6 * efficacy
          df[df$Date >= date + 10, "Eggs"] <-
            df[df$Date >= date + 10, "Eggs"] * 0.6 * efficacy
          df[df$Date >= date + 20, "Immatures"] <-
            df[df$Date >= date + 20, "Immatures"] * 0.6 * efficacy
        }
      }
    }
  }
  if (insectPlant == "Aphids on Soybean" ||
        insectPlant == "Hemlock Woolly Adelgid") {
    if (df[df$Date == date, "Immatures"] == 0) {
      return(df)
    }
    if (frequency == "Once") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.6 * efficacy
      df[df$Date >= date, "Adults"] <-
        df[df$Date >= date, "Adults"] * 0.6 * efficacy
      df[df$Date >= date, "Eggs"] <-
        df[df$Date >= date, "Eggs"] * 0.6 * efficacy
    }
    if (frequency == "Weekly") {
      for (i in seq(from = date, to = max(df$Date), by = 7)) {
        df[df$Date >= i, "Immatures"] <-
          df[df$Date >= i, "Immatures"] * 0.6 * efficacy
        df[df$Date >= i, "Adults"] <-
          df[df$Date >= i, "Adults"] * 0.6 * efficacy
        df[df$Date >= i, "Eggs"] <-
          df[df$Date >= i, "Eggs"] * 0.6 * efficacy
      }
    } else if (frequency == "Monthly") {
      for (i in seq(from = date, to = max(df$Date), by = 30)) {
        df[df$Date >= i, "Immatures"] <-
          df[df$Date >= i, "Immatures"] * 0.6 * efficacy
        df[df$Date >= i, "Adults"] <-
          df[df$Date >= i, "Adults"] * 0.6 * efficacy
        df[df$Date >= i, "Eggs"] <-
          df[df$Date >= i, "Eggs"] * 0.6 * efficacy
      }
    }
  }
  if (insectPlant == "Emerald Ash Borer") {
    if (df[df$Date == date, "Immatures"] == 0) {
      return(df)
    }
    if (stage == "Immatures") {
      if (frequency == "Once") {
        df[df$Date >= date, "Immatures"] <-
          df[df$Date >= date, "Immatures"] * 0.6 * efficacy
        df[df$Date >= date, "Adults"] <-
          df[df$Date >= date, "Adults"] * 0.6 * efficacy
        df[df$Date >= date, "Eggs"] <-
          df[df$Date >= date, "Eggs"] * 0.6 * efficacy
      }
    }
    if (frequency == "Weekly") {
      for (i in seq(from = date, to = max(df$Date), by = 7)) {
        df[df$Date >= i, "Immatures"] <-
          df[df$Date >= i, "Immatures"] * 0.6 * efficacy
        df[df$Date >= i, "Adults"] <-
          df[df$Date >= i, "Adults"] * 0.6 * efficacy
        df[df$Date >= i, "Eggs"] <-
          df[df$Date >= i, "Eggs"] * 0.6 * efficacy
      }
    }
    if (frequency == "Monthly") {
      for (i in seq(from = date, to = max(df$Date), by = 30)) {
        df[df$Date >= i, "Immatures"] <-
          df[df$Date >= i, "Immatures"] * 0.6 * efficacy
        df[df$Date >= i, "Adults"] <-
          df[df$Date >= i, "Adults"] * 0.6 * efficacy
        df[df$Date >= i, "Eggs"] <-
          df[df$Date >= i, "Eggs"] * 0.6 * efficacy
      }
    }
  }
  return(df)
}

apply_biological_control <- function(df, control_params) {
  #Extract the parameters
  insectPlant <- control_params$interactionType
  target <- control_params$bioTarget
  efficacy <- control_params$bioEfficacy
  date <- as.Date(control_params$bioDate)
  # Change the year to 1998 no matter what
  date <- as.Date(paste0("1998-", format(date, "%m-%d")))
  print(paste("insectPlant", insectPlant))
  print(paste("stage", target))
  print(paste("efficacy", efficacy))
  print(paste("date", date))
  if (efficacy == "0" || efficacy == "") {
    return(df)
  }
  if (insectPlant == "Corn Rootworm on Corn") {
    if (target == "Generalist") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.65 * efficacy
      df[df$Date >= date + 10, "Adults"] <-
        df[df$Date >= date + 10, "Adults"] * 0.65 * efficacy
    } else if (target == "Specialist") {
      df[df$Date >= date, "Eggs"] <-
        df[df$Date >= date, "Eggs"] * 0.65 * efficacy
      df[df$Date >= date + 10, "Immatures"] <-
        df[df$Date >= date + 10, "Immatures"] * 0.65 * efficacy
      df[df$Date >= date + 20, "Adults"] <-
        df[df$Date >= date + 20, "Adults"] * 0.65 * efficacy
    }
  }
  if (insectPlant == "Aphids on Soybean" ||
      insectPlant == "Hemlock Woolly Adelgid") {
    if (target == "Generalist") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.65 * efficacy
      df[df$Date >= date, "Adults"] <-
        df[df$Date >= date, "Adults"] * 0.65 * efficacy
    } else if (target == "Specialist") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.65 * efficacy
      df[df$Date >= date + 10, "Adults"] <-
        df[df$Date >= date + 10, "Adults"] * 0.65 * efficacy
    }
  }
  if (insectPlant == "Japanese Beetle on Apple") {
    if (target == "Generalist") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.6 * efficacy
      df[df$Date >= date + 10, "Adults"] <-
        df[df$Date >= date + 10, "Adults"] * 0.6 * efficacy
    } else if (target == "Specialist") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.65 * efficacy
      df[df$Date >= date + 10, "Adults"] <-
        df[df$Date >= date + 10, "Adults"] * 0.65 * efficacy
    }
  }
  if (insectPlant == "Spotted Wing Drosophila on Strawberry") {
    if (target == "Generalist") {
      df[df$Date >= date, "Adults"] <-
        df[df$Date >= date, "Adults"] * 0.6 * efficacy
    } else if (target == "Specialist") {
      df[df$Date >= date, "Adults"] <-
        df[df$Date >= date, "Adults"] * 0.6 * efficacy
      df[df$Date >= date + 10, "Immatures"] <-
        df[df$Date >= date + 10, "Immatures"] * 0.6 * efficacy
    }
  }
  if(insectPlant == "Emerald Ash Borer") {
    if (target == "Specialist") {
      df[df$Date >= date, "Immatures"] <-
        df[df$Date >= date, "Immatures"] * 0.6 * efficacy
      df[df$Date >= date, "Adults"] <-
        df[df$Date >= date, "Adults"] * 0.6 * efficacy
    }
  }
  return(df)
}

# Define server logic
server <- function(input, output, session) {

  observeEvent(input$pestType, {
    if (input$pestType == "Agronomic") {
      updateSelectInput(session, "interactionType",
                        choices = c("\a", "Corn Rootworm on Corn",
                                    "Aphids on Soybean"))
    } else if (input$pestType == "Horticultural") {
      updateSelectInput(session, "interactionType",
                        choices = c("\a", "Japanese Beetle on Apple",
                                    "Spotted Wing Drosophila on Strawberry"))
    } else if (input$pestType == "Forest") {
      updateSelectInput(session, "interactionType",
                        choices = c("\a", "Emerald Ash Borer",
                                    "Hemlock Woolly Adelgid"))
    }
  })

  observeEvent(input$interactionType, {
    pest_type <- input$interactionType
    print("FLAG")
    if(pest_type == "Corn Rootworm on Corn"){
      output$selectedImageUI1 <- renderUI({
        tagList(
          div(style = "display: inline-block; width: 300px; 
              height: 320px; margin-right: 10px;",
            img(src = "images/rootworm_immature.png",
                width = "100%", height = "100%",
                alt = "Corn Rootworm Immature", contentType = "image/png"),
            p("Corn Rootworm Immature")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/rootworm_adult.png",
                width = "100%", height = "100%",
                alt = "Corn Rootworm Adult", contentType = "image/png"),
            p("Corn Rootworm Adult")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/rootworm_crop_corn.png",
                width = "100%", height = "100%",
                alt = "Corn Plant", contentType = "image/png"),
            p("Corn Plant")
          )
        )
      })
    }
    if(pest_type == "Japanese Beetle on Apple"){
      output$selectedImageUI1 <- renderUI({
        tagList(
          div(style = "display: inline-block; width: 300px;
              height: 320px; margin-right: 10px;",
            img(src = "images/beetle_immature.png",
                width = "100%", height = "100%", alt = "Beetle Immature",
                contentType = "image/png"),
            p("Beetle Immature")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/beetle_adult.png", width = "100%",
                height = "100%", alt = "Beetle Adult",
                contentType = "image/png"),
            p("Beetle Adult")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/beetle_crop_apple.png", width = "100%",
                height = "100%", alt = "Apple Plant",
                contentType = "image/png"),
            p("Apple Plant")
          )
        )
      })
    }
    if(pest_type == "Emerald Ash Borer"){
      output$selectedImageUI1 <- renderUI({
        tagList(
          div(style = "display: inline-block; width: 300px;
              height: 320px; margin-right: 10px;",
            img(src = "images/borer_immature.png", width = "100%",
                height = "100%", alt = "Borer Immature",
                contentType = "image/png"),
            p("Borer Immature")
          ),
          div(style = "display: inline-block; width: 300px;
              height: 320px; margin-right: 10px;",
            img(src = "images/borer_adult.png", width = "100%",
                height = "100%", alt = "Borer Adult",
                contentType = "image/png"),
            p("Borer Adult")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/borer_crop_ash.png", width = "100%",
                height = "100%", alt = "Ash Plant",
                contentType = "image/png"),
            p("Ash Plant")
          )
        )
      })
    }
    if(pest_type == "Hemlock Woolly Adelgid"){
      output$selectedImageUI1 <- renderUI({
        tagList(
          div(style = "display: inline-block; width: 300px; height: 320px; margin-right: 10px;",
            img(src = "images/hwa_immature.png", width = "100%", height = "100%", alt = "HWA Immature", contentType = "image/png"),
            p("Hemlock Woolly Adelgid Immature")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px; margin-right: 10px;",
            img(src = "images/hwa_adult.png", width = "100%", height = "100%", alt = "HWA Adult", contentType = "image/png"),
            p("Hemlock Woolly Adelgid Adult")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/hwa_crop_hemlock.png", width = "100%", height = "100%", alt = "Hemlock Plant", contentType = "image/png"),
            p("Hemlock Tree")
          )
        )
      })
    }
    if(pest_type == "Aphids on Soybean"){
      output$selectedImageUI1 <- renderUI({
        tagList(
          div(style = "display: inline-block; width: 300px; height: 320px; margin-right: 10px;",
            img(src = "images/soybean_immature.png", width = "100%", height = "100%", alt = "Soybean Aphid Immature", contentType = "image/png"),
            p("Aphid Immature")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px; margin-right: 10px;",
            img(src = "images/soybean_adult.png", width = "100%", height = "100%", alt = "Soybean Aphid Adult", contentType = "image/png"),
            p("Aphid Adult")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/soybean_crop_soybean.png", width = "100%", height = "100%", alt = "Soybean Plant", contentType = "image/png"),
            p("Soybean Plant")
          )
        )
      })
    }
    if(pest_type == "Spotted Wing Drosophila on Strawberry"){
      output$selectedImageUI1 <- renderUI({
        tagList(
          div(style = "display: inline-block; width: 300px; height: 320px; margin-right: 10px;",
            img(src = "images/swd_immature.png", width = "100%", height = "100%", alt = "Spotted Wing Drosophila Immature", contentType = "image/png"),
            p("Spotted Wing Drosophila Immature")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px; margin-right: 10px;",
            img(src = "images/swd_adult.png", width = "100%", height = "100%", alt = "Spotted Wing Drosophila Adult", contentType = "image/png"),
            p("Spotted Wing Drosophila Adult")
          ),
          div(style = "display: inline-block; width: 300px; height: 320px;",
            img(src = "images/swd_crop_strawberry.png", width = "100%", height = "100%", alt = "Strawberry Plant", contentType = "image/png"),
            p("Strawberry Plant")
          )
        )
      })
    }
  })


  observeEvent(input$clear, {
    updateSelectInput(session, "pestType", selected = "\a")
    updateSelectInput(session, "interactionType", selected = "\a")
    updateSelectInput(session, "tempConditions", selected = "\a")
    updateSelectInput(session, "targetChem", selected = "\a")
    updateNumericInput(session, "chemEfficacy", value = "")
    updateDateInput(session, "chemDate", value = Sys.Date())
    updateSelectInput(session, "chemFrequency", selected = "\a")
    updateSelectInput(session, "bioTarget", selected = "\a")
    updateNumericInput(session, "bioEfficacy", value = "")
    updateDateInput(session, "bioDate", value = Sys.Date())
    output$plot <- renderPlot(NULL)  # Remove the plot
    output$selectedImageUI1 <- renderUI({ NULL })
  })
  data <- eventReactive(input$apply, {
    # Add some railings here to protect the fill in the blanks,
    # and making sure that we are getting the right stuff in
    req(input$interactionType)  # Ensure the input is not NULL
    req(input$tempConditions)
    interaction_type <- input$interactionType
    filename <- paste0(interaction_type, ".csv")  # Construct the filename
    filename <- paste0("data/", filename)
    df <- read.csv(filename, header = TRUE)  # Read the data
    if(input$tempConditions == "Cold") {
      subset(df, select = c(1, 2, 3, 4, ncol(df)))
    } else if (input$tempConditions == "Average") {
      subset(df, select = c(5, 6, 7, 8, ncol(df)))
    } else if (input$tempConditions == "Hot") {
      subset(df, select = c(9, 10, 11, 12, ncol(df)))
    }
  })
  observeEvent(input$apply, {
    output$plot <- renderPlot({
      df <- data()
      req(df)
      #Convert date column
      df[, 1] <- as.Date(df[, 1])
      colnames(df)[1] <- "Date"  # Assign the column name 'Date'
      colnames(df)[2] <- "Eggs"
      colnames(df)[3] <- "Immatures"
      colnames(df)[4] <- "Adults"
      #Change the values of some Biocontrol columns to 0
      df[df$Date <= as.Date("1998-03-15") |
           df$Date >= as.Date("1998-10-15"), "Biocontrol"] <- NA
      # Convert the data frame to long format
      # All the transformation code here from chemical control
      # and biocontrol interventions
      chem_control_params <- list(interactionType = input$interactionType,
                                  targetChem = input$targetChem,
                                  chemEfficacy = input$chemEfficacy,
                                  chemDate = input$chemDate,
                                  chemFrequency = input$chemFrequency)
      if (input$targetChem != "\a" && input$chemFrequency != "\a") {
        df <- apply_chemical_control(df, chem_control_params)
      }

      bio_control_params <- list(interactionType = input$interactionType,
                                 bioTarget = input$bioTarget,
                                 bioEfficacy = input$bioEfficacy,
                                 bioDate = input$bioDate)
      if (input$bioTarget != "\a") {
        df <- apply_biological_control(df, bio_control_params)
      }

      df_long <- tidyr::pivot_longer(df, -1,
                                     names_to = "Population",
                                     values_to = "Value")
      df_long <- df_long[!(df_long$Population == "Biocontrol" &
                             input$bioTarget == "\a"), ]
      ggplot(df_long, aes(x = Date, y = Value,
                          colour = Population)) +
        geom_line(size = 1.5) +
        scale_y_continuous(limits = c(0, 125)) +
        scale_x_date(date_labels = "%m-%d") +  # Add this line
        theme_bw() +
        labs(x = "Date", y = "Number of Individuals",
             title = "Predictions of Pest Population") +
        theme(text = element_text(size = 20))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)