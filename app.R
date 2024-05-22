library(shiny)
library(tidyverse)
library(stringr)
library(stringdist)
library(shinyjs)
library(shinyWidgets)
library(emojifont)
library(highcharter)
library(bslib)
library(shinyalert)

allLyrics = read_csv("WATERPARKS_LYRICS_FULL.csv")
allAlbums = sort(unique(allLyrics$album))

ui = page_navbar(title = strong("Waterparks Lyrics Guessing Game"), 
                 window_title = "Waterparks Lyrics Guessing Game",
                 shinyjs::useShinyjs(), 
                 includeCSS("www/styles.css"),
                 # tags$head(tags$script(HTML(jscode))),
                 setBackgroundImage(src = "waterparks_bg_black.jpg"),
                 br(),
                 h1(strong(class = "main-header", "Welcome to ", "Waterparks Lyrics Guessing Game!")),
                 layout_sidebar(
                   class = "layout", 
                   sidebar = sidebar(
                     width = 600, 
                     open = "open",
                     p("Lyrics range from", strong(em("Airplane Conversations")), " to ", 
                       strong(em("INTELLECTUAL PROPERTY")), ", including the 2020 demo album."),
                     h4(strong("How to play"), emojifont::emoji("guitar")), 
                     p("Choose your game mode, then click the Generate button to generate your lyrics. 
                       I recommend closing this instructions panel after you've 
                       read it (there's a little arrow at the bottom right)!"),
                     p(class = "important-note", 
                       strong("IMPORTANT: Capitalization does ", em("not"), 
                              " matter, and neither do minor spelling errors. 
                              Do NOT include parentheses for the demo album; 
                              e.g. Glitter Times (1997 DEMO) should just be 
                              Glitter Times.")),
                     p("Points:"), 
                     tags$ul(
                       tags$li("Correct guess (easy): +2 point"), 
                       tags$li("Correct guess (medium): +6 points"),
                       tags$li("Correct guess (hard): +8 points"),
                       tags$li("Incorrect guess (any mode): -2 points"), 
                       tags$li("Hint: -1 point"),
                       tags$li("Giving up: -2 points")
                     ),
                     p("There are three hints available for 
                       each set of lyrics."), 
                     tags$ul(
                       tags$li("Hint 1: gives the album that the song is from"), 
                       tags$li("Hint 2: gives the line immediately preceding the 
                               given set of lyrics"), 
                       tags$li("Hint 3: gives the line immediately after the 
                               given set of lyrics")
                     ), 
                     p(strong("That's it! Have fun!")),
                     hr(), 
                     h4(strong("Details")), 
                     p("Made with love by Jasmine Xu - contact me at 
                       jasminexu@utexas.edu or @jasminex.21 on Instagram!"), 
                     p("For those interested, the GitHub repository for this 
                       project can be accessed ", a("here", 
                                                    href = "https://github.com/jasminex21/waterparks_lyrics_game")),
                     p("Background photo by @jawn on Twitter/Instagram")
                   ), 
                   
                   # the main panel where all the guessing goes on
                   class = "mainPanel",
                   layout_sidebar(
                     sidebar = sidebar(
                       width = 350,
                       position = "right", 
                       open = T, 
                       h4(strong("Game Statistics")), 
                       div(
                         textOutput("totalRounds"), 
                         textOutput("totalCorrect"), 
                         textOutput("pointsProp"), 
                         textOutput("currentStreak"), 
                         span(style = "color: green", textOutput("totalPoints")), 
                         style = "font-weight: bold"
                       ),
                     ), 
                     
                     fluidRow( 
                       column(8, selectInput("mode", 
                                             label = "Select game mode", 
                                             choices = c("Easy (entire section, e.g. chorus)" = 1, 
                                                         "Medium (2 lines)" = 2, 
                                                         "Hard (1 line)" = 3), 
                                             selected = 1)), 
                       column(3, textInput("seed",
                                           label = "Set seed (optional)",
                                           placeholder = "e.g. 21 or 2013"), 
                              actionButton("submitSeed", 
                                           "Submit"))),
                     
                     fluidRow(
                       div(
                       checkboxGroupInput(
                         "albums", 
                         label = "Generate lyrics from: ",
                         choices = allAlbums,
                         selected = allAlbums, 
                         inline = T, 
                         width = "100%"
                       ))),
                     column(4, actionButton("generateButton", 
                                            "Generate!", 
                                            class = "btn btn-sm")),
                     h4(strong("What song are these lyrics from?")),
                     div(class = "randGenDiv", br(), htmlOutput("randGenerated"), br()),
                     # hr(), 
                     tagAppendAttributes(
                       textInput("guess", 
                                 label = "Enter your guess", 
                                 placeholder = "e.g. 21 Questions or Rare"), 
                       `data-proxy-click` = "submit"), 
                     column(12, 
                            actionButton("submit",
                                         label = "Submit"),
                            actionButton("hintButton",
                                         label = "Hint"),
                            actionButton("giveUpButton",
                                         label = "Give up"), 
                            br(), br(),
                            htmlOutput("guessFeedback"),
                            htmlOutput("printHint"),
                            htmlOutput("printAnswer"),
                     ), 
                   ),
                   
                 )
                 
)

server = function(input, output, session) {
  
  # setting a different seed each time the game is reloaded
  set.seed(round(as.numeric(Sys.time())))
  
  observeEvent(input$submitSeed, {
    seed = as.numeric(input$seed)
    if (!is.na(seed)) {
      set.seed(seed)
      shinyjs::disable("submitSeed")
    }
    else {
      shinyalert("Please enter a valid numerical seed, or leave this blank!")
    }
  })
  
  rounds = reactiveValues(roundValue = 0)
  hints = reactiveValues(hintsCount = 0)
  streaks = reactiveValues(streak = 0)
  
  buttonPressed = eventReactive(input$generateButton, {
    shinyjs::disable("generateButton")
    shinyjs::enable("hintButton")
    shinyjs::enable("submit")
    shinyjs::enable("giveUpButton")
    
    # keeping only the selected albums
    allLyrics = allLyrics %>%
      filter(album %in% input$albums)
    
    if (nrow(allLyrics) == 0) {
      shinyalert("At least one album must be selected. Please try again!")
      shinyjs::disable("submit")
      shinyjs::disable("giveUpButton")
      shinyjs::disable("hint")
      shinyjs::enable("generateButton")
      NULL
    }
    
    else {
      rounds$roundValue = rounds$roundValue + 1
      
      randNum = floor(runif(1, min = 1, max = nrow(allLyrics)))
      
      if (input$mode == 3) {
        allLyrics[randNum,]
      }
      else if (input$mode == 2) {
        # randSection = allLyrics$section[randNum]
        if (allLyrics$song_title[randNum + 1] == allLyrics$song_title[randNum]) {
          start = randNum
          end = randNum + 1
        }
        else {
          start = randNum - 1
          end = randNum
        }
        allLyrics[start:end,]
      }
      else if (input$mode == 1) {
        randSection = allLyrics$section[randNum]
        
        end = randNum
        while (allLyrics$section[end + 1] == randSection & allLyrics$song_title[randNum] == allLyrics$song_title[end + 1]) {
          end = end + 1
        }
        start = randNum
        while (allLyrics$section[start - 1] == randSection & allLyrics$song_title[randNum] == allLyrics$song_title[start - 1]) {
          start = start - 1
        }
        allLyrics[start:end,]
      }}
  })
  
  counter = reactiveValues(counterValue = 0)
  corrects = reactiveValues(correctsValue = 0)
  availPoints = reactiveValues(availValue = 0)
  
  output$randGenerated = renderUI({
    HTML(paste(buttonPressed()$lyrics, collapse = "<br/>"))
  })
  
  wantHint = eventReactive(input$hintButton, {
    counter$counterValue = counter$counterValue - 1
    hints$hintsCount = hints$hintsCount + 1
    
    df = buttonPressed()
    
    if (hints$hintsCount == 1) {
      hintMessage = df$album[1]
    }
    # give the NEXT line
    else if (hints$hintsCount == 2) {
      # get the ending index
      lastLine = tail(df, n = 1)
      end = which((allLyrics$lyrics == lastLine$lyrics) &
                    (allLyrics$line == lastLine$line) &
                    (allLyrics$album == lastLine$album) &
                    (allLyrics$song_title_corrected == lastLine$song_title_corrected))
      
      if (lastLine$song_title_corrected == allLyrics[end + 1,]$song_title_corrected) {
        hintMessage = allLyrics$lyrics[end + 1]
      }
      
      else {
        hintMessage = "NA"
      }
    }
    # give the PREVIOUS line
    else if (hints$hintsCount == 3) {
      
      # get the start index
      firstLine = df[1,]
      start = which((allLyrics$lyrics == firstLine$lyrics) &
                      (allLyrics$line == firstLine$line) &
                      (allLyrics$album == firstLine$album) &
                      (allLyrics$song_title_corrected == firstLine$song_title_corrected))
      
      if (firstLine$song_title_corrected == allLyrics[start - 1,]$song_title_corrected) {
        hintMessage = allLyrics$lyrics[start - 1]
      }
      
      else {
        hintMessage = "NA"
      }
      
      shinyjs::disable("hintButton")
    }
    
    return(hintMessage)
  })
  
  secButtonPressed = eventReactive(input$submit, {
    req(input$guess)
    # reset hints
    # hints$hintsCount = 0
    
    guess = str_trim(input$guess)
    titleLength = nchar(buttonPressed()$song_title_corrected[1])
    allowedDiff = ceiling(0.33 * titleLength)
    if (stringdist(str_to_lower(buttonPressed()$song_title_corrected[1]), 
                   str_to_lower(guess)) <= allowedDiff) {
      corrects$correctsValue = corrects$correctsValue + 1
      # add 1 to streak
      streaks$streak = streaks$streak + 1
      # reset hints
      hints$hintsCount = 0
      shinyjs::enable("generateButton")
      shinyjs::disable("giveUpButton")
      shinyjs::disable("hintButton")
      shinyjs::reset("guess")
      shinyjs::disable("submit")
      if (input$mode == 1) {
        counter$counterValue = counter$counterValue + 2
        availPoints$availValue = availPoints$availValue + 2
      }
      else if (input$mode == 2) {
        counter$counterValue = counter$counterValue + 6
        availPoints$availValue = availPoints$availValue + 6
      }
      else {
        counter$counterValue = counter$counterValue + 8
        availPoints$availValue = availPoints$availValue + 8
      }
      
      HTML(paste(tags$span(style = "color:green;font-size:larger;font-weight:800", 
                           tags$strong("Correct!")),
                 br(),  
                 em(strong(buttonPressed()$song_title_corrected[1])),
                 ", ", 
                 strong(em(buttonPressed()$section[1])),
                 ", from the album ", 
                 em(strong(buttonPressed()$album[1])), 
                 br(), br(), sep = ""), 
           .noWS = "outside")
    }
    # answered incorrectly
    else {
      # reset streak to 0
      streaks$streak = 0
      counter$counterValue = counter$counterValue - 2
      if (input$mode == 1) {
        availPoints$availValue = availPoints$availValue + 2
      }
      else if (input$mode == 2) {
        availPoints$availValue = availPoints$availValue + 6
      }
      else {
        availPoints$availValue = availPoints$availValue + 8
      }
      HTML(paste0(tags$span(style = "color: red;font-size:larger", '"', input$guess, '"', strong(" is incorrect")), 
                  br(), 
                  div("Try again, you can do it! (otherwise, you can give up)", 
                      style = "color:white;"), 
                  br()), 
           .noWS = "outside")
    }
  })
  output$guessFeedback = renderUI({
    secButtonPressed()
  })
  output$printHint = renderUI({
    
    hintMessage = wantHint()
    
    if (hints$hintsCount == 1) {
      HTML(paste0(tags$span(style = "color:orange;", tags$strong("Hint: ")),
                  "these lyrics come from the album ",
                  tags$em(tags$strong(hintMessage)),
                  br(), br()))
    }
    
    else if (hints$hintsCount == 2) {
      HTML(paste0(tags$span(style = "color:orange;", tags$strong("Hint: ")),
                  'the next line of this song is "', 
                  tags$em(tags$strong(hintMessage)),
                  '"', 
                  br(), br()))
    }
    
    else if (hints$hintsCount == 3) {
      HTML(paste0(tags$span(style = "color:orange;", tags$strong("Hint: ")),
                  'the previous line of this song is "',
                  tags$em(tags$strong(hintMessage)), 
                  '"',
                  br(), br()))
    }
  })
  # gave up
  wantAnswer = eventReactive(input$giveUpButton, {
    counter$counterValue = counter$counterValue - 2
    # reset streak to 0
    streaks$streak = 0
    shinyjs::disable("submit")
    shinyjs::enable("generateButton")
    shinyjs::disable("giveUpButton")
    shinyjs::disable("hintButton")
    shinyjs::reset("guess")
    # reset hints
    hints$hintsCount = 0
    
    addRow = data.frame(
      album = buttonPressed()$album[1], 
      status = "Gave up"
    )
    
    buttonPressed()$song_title_corrected[1]
  })
  output$printAnswer = renderUI({
    # shinyjs::show("printAnswer")
    HTML(paste0(tags$span(style = "color: red", tags$strong("Answer: ")), 
                "the correct answer was ", 
                tags$em(tags$strong(wantAnswer())), 
                ", ", 
                tags$strong(buttonPressed()$section[1]), 
                br(), br()))
  })
  output$totalRounds = renderText({
    paste0("Current round: ", rounds$roundValue)
  })
  output$totalCorrect = renderText({
    percentage = round((corrects$correctsValue / rounds$roundValue) * 100, digits = 2)
    paste0("Accuracy", emojifont::emoji("white_check_mark"), ": ", corrects$correctsValue, "/", rounds$roundValue, 
           " (", percentage, "%)")
  })
  output$totalPoints = renderText({
    paste0("Total points: ", counter$counterValue)
  })
  output$pointsProp = renderText({
    propPerc = round((counter$counterValue / availPoints$availValue) * 100, digits = 2)
    paste0("Points out of possible: ", counter$counterValue, "/", availPoints$availValue, " (", propPerc, "%)")
  })
  output$currentStreak = renderText({
    paste0("Current streak", emojifont::emoji("fire"), ": ", streaks$streak)
  })
  observeEvent(input$submit, {
    shinyjs::show("guessFeedback")
  })
  observeEvent(input$generateButton, {
    shinyjs::hide("guessFeedback")
    shinyjs::hide("printHint")
    shinyjs::hide("printAnswer")
  })
  observeEvent(input$hintButton, {
    shinyjs::show("printHint")
  })
  observeEvent(input$giveUpButton, {
    shinyjs::show("printAnswer")
  })
}

shinyApp(ui = ui, server = server)