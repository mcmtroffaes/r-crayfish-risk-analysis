library(shiny)
library(shinyFiles)

attributes <- data.frame(
  name = c(
    "impact",
    "longevity",
    "feasibility",
    "cost"
  ),
  description = c(
    "Biotic impact of the intervention on all species in the system.", # (except from the invasive alien species who is the target of the intervention).",
    "Duration of negative biotic impact.",
    "Extent to which an intervention is easy to carry out in practice.",
    "Monetary costs associated with an intervention."
  ),
  stringsAsFactors = FALSE
)

outcomes <- data.frame(
  name = c(
    rep("impact", 3),
    rep("longevity", 3),
    rep("feasibility", 4),
    rep("cost", 4)
  ),
  level = c(rep(3:1, 2), rep(4:1, 2)),
  short_description = c(
    "minor impact on some",
    "major impact on some",
    "major impact on most",
    "month",
    "1 year",
    "> 1 years",
    "no obstacles",
    "minor obstacles",
    "some controversy",
    "large controversy",
    "< 50k",
    "50-250k",
    "250-500k",
    "> 500k"
  ),
  description = c(
    "Some species are negatively affected, but this does not have any impact on the viability of their populations and the invasive alien species is not present in the system.",
    "Some of the species in the system are negatively affected or that the majority of species are affected but not with any impact on the viability of their populations, and the invasive alien species is not present in the system.",
    "Majority of the species in the system are negatively affected or the invasive alien species is present in the system.",
    "Duration of negative biotic impacts up to a month.",
    "Duration of negative biotic impacts up to 1 year.",
    "Duration of negative biotic impacts for more than 1 years.",
    "No major obstacles in carrying out the method.",
    "Some obstacles to carry out the method, but these are possible to overcome in the current legislation and policy.",
    "Method is controversial and it requires a lot of preparatory work to be possible to carry out.",
    "Large controversy about the method and it may be in conflict with current legislation or policy.",
    "Between 0 and 50 000 SEK.",
    "Between 50 000 and 250 000 SEK.",
    "Between 250 000 and 500 000 SEK.",
    "More than 500 000 SEK."
  ),
  stringsAsFactors = FALSE
)

ui_outcomes <- list(
  h2("Descriptions"),
  "Below you find a list of all attributes, along with a description of the different levels that we will consider for these.",
  lapply(1:nrow(attributes), function(k) {
    list(
      h3(attributes$name[k]),
      attributes$description[k],
      tableOutput(paste("tabledesc", k, sep = ""))
    )
  })
)

ui_pairs <- list(
  h2("Pair Selection"),
  "Later, you will be asked to compare these five attributes at a low and high level. On this page, we will identify which pairs of levels you find most comfortable with comparing. A default selection has been made, if you are happy with this, you can leave this as is.",
  fluidRow(
    column(6, h3("Better")),
    column(6, h3("Worse"))
  ),
  lapply(1:nrow(attributes), function(k) {
    attr_name <- attributes$name[k]
    attr_vals <- outcomes$short_description[outcomes$name == attr_name]
    attr_ranks <- outcomes$level[outcomes$name == attr_name]
    good <- max(attr_ranks)
    bad <- good - 1
    id1 <- paste("choice1", k, sep = "")
    id2 <- paste("choice2", k, sep = "")
    fluidRow(
      column(
        6,
        radioButtons(
          id1, h4(attr_name),
          choiceNames = attr_vals,
          choiceValues = attr_ranks,
          selected = good
        )
      ),
      column(
        6,
        radioButtons(
          id2, h4(attr_name),
          choiceNames = attr_vals,
          choiceValues = attr_ranks,
          selected = bad
        )
      )
    )
  })
)

ui_worst <- list(
  h2("Worst Combined Outcome"),
  sidebarLayout(
    sidebarPanel(
      "On the right you see a list of combined outcomes. Which of these combined outcomes do you judge to be the worst?",
      radioButtons(
        "worst", NULL,
        choices = 1:nrow(attributes)
      )
    ),
    mainPanel(
      tableOutput("tableworst")
    )
  )
)

ui_ranges_explanation <- list(
  h2("Random Outcomes"),
  "For any two outcomes, say A and B, we can consider a random outcome, for example:",
  tableOutput("tableabmixture"),
  "meaning that with 35% chance we will get A",
  "and with 65% chance we will get B.",
  h2("Elicitation Through Comparison With Random Outcomes"),
  "Say that we now consider three outcomes, A, B, and C,",
  "where A is the worst outcome and C is the best outcome.",
  "Then, from the following random outcomes:",
  fluidRow(
    column(3, tableOutput("tableacmixture1")),
    column(3, tableOutput("tableacmixture2")),
    column(3, tableOutput("tableacmixture3")),
    column(3, tableOutput("tableacmixture4"))
  ),
  "clearly you will prefer B to the left-most outcome (which is equivalent to the worst outcome A),",
  "and you will prefer the right-most outcome to B (which is equivalent to the best outcome C).",
  "Considering this random outcome as a function of the chance p associated with the best outcome C:",
  tableOutput("tableacmixturep"),
  "as we increase p from 0, there will be a largest value for p for which you prefer B to this random outcome.",
  "Similarly, as we decrease p from 100, there will be a smallest value for p for which you prefer this random outcome to B.",
  "We are interested in eliciting these values from you, for specific outcomes listed in the final tab."
)

ui_ranges <- list(
  h2("Preferences"),
  p("If you inspect the three outcomes on each row, the random outcome on the left (red) should be worse to you than the outcome in the middle (orange), which in turn should be worse to you than the outcome on the right (green)."),
  p("What we ask you to do now is for you to adjust the ranges on the sliders as tightly as possible ensuring that you fully agree with the preferences encoded by the colours, that is, tighten the range so that you still agree that red is not better than orange and orange is not better than green."),
  hr(),
  verticalLayout(
    lapply(
      1:nrow(attributes), function(k) {
        list(
          fluidRow(
            column(3, sliderInput(
              paste("sliderprob", k, sep = ""), paste("Chance for gamble", k),
              min = 0, max = 100, step = 1, value = c(0, 100)
            )),
            tags$style(paste("#tablegamble1", k, sep = ""), "{background-color:#ff8888;}"),
            tags$style(paste("#tablegamble2", k, sep = ""), "{background-color:#ffbb00;}"),
            tags$style(paste("#tablegamble3", k, sep = ""), "{background-color:#88ff88;}"),
            column(3, tableOutput(paste("tablegamble1", k, sep = ""))),
            column(3, tableOutput(paste("tablegamble2", k, sep = ""))),
            column(3, tableOutput(paste("tablegamble3", k, sep = "")))
          ),
          hr()
        )
      }
    )
  )
)

ui_save <- list(
  h2("Save Results To File"),
  p("Click the button below to save the results to a file."),
  shinySaveButton("save", "Save", "Save as...",
    filetype = list(csv = "csv")
  )
)

ui <- fluidPage(
  titlePanel("Marmorkreb Elicitation"),
  tabsetPanel(
    tabPanel("Descriptions", ui_outcomes),
    tabPanel("Pair Selection", ui_pairs),
    tabPanel("Worst Combined Outcome", ui_worst),
    tabPanel("Random Outcomes: Introduction", ui_ranges_explanation),
    tabPanel("Random Outcomes: Elicitation", ui_ranges),
    tabPanel("Save Results", ui_save)
  )
)

server <- function(input, output, session) {
  observe({
    for (k in 1:nrow(attributes)) {
      # ensure worse < better
      attr_ranks <- outcomes$level[outcomes$name == attributes$name[k]]
      id1 <- paste("choice1", k, sep = "")
      id2 <- paste("choice2", k, sep = "")
      val1 <- as.numeric(input[[id1]])
      val2 <- as.numeric(input[[id2]])
      if (val1 <= val2) val2 <- max(val1 - 1, min(attr_ranks))
      if (val1 <= val2) val1 <- min(val2 + 1, max(attr_ranks))
      updateRadioButtons(session, id1, selected = val1)
      updateRadioButtons(session, id2, selected = val2)
      # ensure unused slider for worst gamble is indicated
      worst <- input[["worst"]]
      gambleid <- paste("sliderprob", k, sep = "")
      range <- input[[gambleid]]
      if (k != worst) {
        updateSliderInput(session, gambleid,
          label = paste("Gamble", k), value = range
        )
      } else {
        updateSliderInput(session, gambleid,
          label = paste("Gamble", k, "(not used)"), value = range
        )
      }
    }
    # save file if need be
    shinyFileSave(input, "save", roots = c(wd = "."), session = session)
    fileinfo <- parseSavePath(roots = c(wd = "."), input$save)
    if (nrow(fileinfo) > 0) {
      data <- data.frame(input = c("pair1", "pair2", "worst", "slider1", "slider2"))
      worst <- input[["worst"]]
      for (k in 1:nrow(attributes)) {
        id1 <- paste("choice1", k, sep = "")
        id2 <- paste("choice2", k, sep = "")
        val1 <- as.numeric(input[[id1]])
        val2 <- as.numeric(input[[id2]])
        gambleid <- paste("sliderprob", k, sep = "")
        probrange <- input[[gambleid]]
        data[[paste("attr", k, sep = "")]] <- c(val1, val2, k == worst, probrange[1], probrange[2])
      }
      write.csv(data, as.character(fileinfo$datapath), row.names = FALSE)
    }
  })
  # draw outcome description tables
  # note: use lapply to get around lazy evaluation
  lapply(
    1:nrow(attributes),
    function(k) {
      name <- attributes$name[k]
      output[[paste("tabledesc", k, sep = "")]] <- renderTable(
        outcomes[outcomes$name == name, c("short_description", "description")]
      )
    }
  )
  # draw worst outcome table
  output$tableworst <- renderTable({
    dataworst <- list(option = sapply(1:nrow(attributes), paste))
    for (k in 1:nrow(attributes)) {
      id1 <- paste("choice1", k, sep = "")
      id2 <- paste("choice2", k, sep = "")
      val1 <- as.numeric(input[[id1]])
      val2 <- as.numeric(input[[id2]])
      attr_name <- attributes$name[[k]]
      dataworst[[attr_name]] <- sapply(
        1:nrow(attributes), function(k2) {
          if (k == k2) rank <- val2 else rank <- val1
          outcomes$short_description[
            outcomes$name == attr_name & outcomes$level == rank
          ]
        }
      )
    }
    dataworst
  })
  # mixtures
  output$tableabmixture <- renderTable(
    data.frame(chance = as.integer(c(35, 65)), outcome = c("A", "B"))
  )
  output$tableacmixture1 <- renderTable(
    data.frame(chance = as.integer(c(100, 0)), outcome = c("A", "C"))
  )
  output$tableacmixture2 <- renderTable(
    data.frame(chance = as.integer(c(70, 30)), outcome = c("A", "C"))
  )
  output$tableacmixture3 <- renderTable(
    data.frame(chance = as.integer(c(30, 70)), outcome = c("A", "C"))
  )
  output$tableacmixture4 <- renderTable(
    data.frame(chance = as.integer(c(0, 100)), outcome = c("A", "C"))
  )
  output$tableacmixturep <- renderTable(
    data.frame(chance = c("100-p", "p"), outcome = c("A", "C"))
  )
  # gambles
  lapply(
    1:nrow(attributes), function(k) {
      update_gamble_table_1 <- function(result, k, v) {
        id1 <- paste("choice1", k, sep = "")
        id2 <- paste("choice2", k, sep = "")
        val1 <- as.numeric(input[[id1]])
        val2 <- as.numeric(input[[id2]])
        vals <- c(val1, val2)
        attr_name <- attributes$name[[k]]
        result[[attributes$name[k]]] <-
          outcomes$short_description[
            outcomes$name == attr_name & outcomes$level == vals[v]
          ]
        result
      }
      update_gamble_table_2 <- function(result, k, v1, v2) {
        id1 <- paste("choice1", k, sep = "")
        id2 <- paste("choice2", k, sep = "")
        val1 <- as.numeric(input[[id1]])
        val2 <- as.numeric(input[[id2]])
        vals <- c(val1, val2)
        attr_name <- attributes$name[[k]]
        result[[attributes$name[k]]] <- c(
          outcomes$short_description[
            outcomes$name == attr_name & outcomes$level == vals[v1]
          ],
          outcomes$short_description[
            outcomes$name == attr_name & outcomes$level == vals[v2]
          ]
        )
        result
      }
      output[[paste("tablegamble1", k, sep = "")]] <- renderTable({
        sliderid <- paste("sliderprob", k, sep = "")
        range <- input[[sliderid]]
        result <- data.frame(
          chance = as.integer(c(100 - range[1], range[1]))
        )
        k2 <- as.numeric(input[["worst"]])
        result <- update_gamble_table_2(result, k2, 2, 1)
        result <- update_gamble_table_2(result, k, 1, 1)
        result
      })
      output[[paste("tablegamble2", k, sep = "")]] <- renderTable({
        sliderid <- paste("sliderprob", k, sep = "")
        range <- input[[sliderid]]
        result <- list()
        k2 <- as.numeric(input[["worst"]])
        result <- update_gamble_table_1(result, k2, 1)
        if (k != k2) result <- update_gamble_table_1(result, k, 2)
        result
      })
      output[[paste("tablegamble3", k, sep = "")]] <- renderTable({
        sliderid <- paste("sliderprob", k, sep = "")
        range <- input[[sliderid]]
        result <- data.frame(
          chance = as.integer(c(100 - range[2], range[2]))
        )
        k2 <- as.numeric(input[["worst"]])
        result <- update_gamble_table_2(result, k2, 2, 1)
        result <- update_gamble_table_2(result, k, 1, 1)
        result
      })
    }
  )
}

# print outcomes as table for the paper
#library(xtable)
#print(xtable(outcomes))

shinyApp(ui = ui, server = server)
