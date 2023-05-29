
library(shiny)
library(baseballr)
library(tidyverse)
library(gt)
library(gtExtras)
library(patchwork)
library(ggplot2)
library(ggrepel)



# Data frame for all player names & IDs----------------------------------------

# All files are from chadwick bureau and can be found at: 
# https://github.com/chadwickbureau/register
# Combining all files into one, in order to connect player names to their mlbam ID

  people.0 <- read.csv("NameFiles/people-0.csv")
  people.1 <- read.csv("NameFiles/people-1.csv")
  people.2 <- read.csv("NameFiles/people-2.csv")
  people.3 <- read.csv("NameFiles/people-3.csv")
  people.4 <- read.csv("NameFiles/people-4.csv")
  people.5 <- read.csv("NameFiles/people-5.csv")
  people.6 <- read.csv("NameFiles/people-6.csv")
  people.7 <- read.csv("NameFiles/people-7.csv")
  people.8 <- read.csv("NameFiles/people-8.csv")
  people.9 <- read.csv("NameFiles/people-9.csv")
  people.a <- read.csv("NameFiles/people-a.csv")
  people.b <- read.csv("NameFiles/people-b.csv")
  people.c <- read.csv("NameFiles/people-c.csv")
  people.d <- read.csv("NameFiles/people-d.csv")
  people.e <- read.csv("NameFiles/people-e.csv")
  people.f <- read.csv("NameFiles/people-f.csv")


  # Binding together all above data frames and cleaning
  Names <- rbind(people.0, people.1, people.2, people.3, people.4, people.5,
               people.6, people.7, people.8, people.9, people.a, people.b,
               people.c, people.d, people.e, people.f) %>%
  filter(pro_played_last > 2014) %>% 
    filter(key_mlbam > 0) %>%
  select(key_mlbam, name_first, name_last, pro_played_last) %>% na.omit()

  
  # Column for full names
  Names$Full <- paste(Names$name_first, Names$name_last, sep = " ")

# -----------------------------------------------------------------------------


  
# Coloring for pitch types in HV plot-------------------------------------------
  
TMcolors <- c("4-Seam Fastball" = "black",
              "Cutter" = "purple",
              "Sinker" = "#E50E00",
              "Slider" = "#4595FF",
              "Sweeper" = "#002FAD",
              "Slurve" = "#D38B31",
              "Changeup" = "#009A09",
              "Split-Finger" = "#0FB16E",
              "Curveball" = "orange",
              "Knuckle Curve" = "orange",
              "Screwball" = "#ECE400",
              "Forkball" = "#00F9AC")

# ------------------------------------------------------------------------------
  
  
  
  
  
  
# UI----------------------------------------------------------------------------

ui <- fluidPage(
  headerPanel("MLB Pitcher Statcast Data/Analysis"),
  sidebarPanel(
    textInput("name", label = "Pitcher Name(First Last)"),
    # selectizeInput("names", choices = NULL),
    actionButton("go", "Enter"),
    width = 3),
  sidebarPanel(
    dateRangeInput("Date", label = "Date Range(yyyy-mm-dd)",
                   format = "yyyy-mm-dd",
                   start = "2023-01-01", end = "2023-12-31",
                   min = "2015-03-01"),
    radioButtons("side", "Batter Side",
                 choices = c("All", "Right", "Left"), selected = "All"),
    width = 3
  ),
  mainPanel(
    h3("HV Plot"),
    h5("Pitcher's POV"),
    plotOutput("HV"),
    h3("Heatmap"),
    h5("Catcher's POV"),
    plotOutput("Heatmap"),
    h3("Data by pitch type"),
    gt_output("Table"),
    h3("Plate Discipline"),
    gt_output("Table2")
  )
)
# ------------------------------------------------------------------------------





# Server------------------------------------------------------------------------

server <- function(input, output, session){
  
  
  
  # For selectize input(not yet working properly)
    # updateSelectizeInput(session, "names", label = NULL, multiple = FALSE,
                         # choices = unique(Names$Full), server = TRUE)

  
  # Splitting full name input into first and last names
  # firstname <- reactive(sapply(strsplit(input$name, " "),
  #                                       function(x) x[1]))
  # lastname <- reactive(sapply(strsplit(input$name, " "),
  #                               function(x) x[length(x)]))
  
  
  
  # Making date input into reactive function
  Date1 <- reactive(input$Date[1])
  Date2 <- reactive(input$Date[2])
  
  
  # Pulling MLBAM ID of player
  ID <- eventReactive(input$go, Names[Names$Full == input$name, 1])
  
  
  # Use mlbam_id and dates to load all baseball savant data
  
  # Pulling baseball savant data and also filtering the data 
  # based on batter side input (pitcher facing RHH/LHH)
  
  dataset <- reactive({
      if(input$side == "Right"){
        scrape_statcast_savant_pitcher(start_date = Date1(),
                                       end_date = Date2(),
                                       pitcherid = ID()) %>%
          mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
          filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
                 !pitch_name == "", !pitch_name == "Other", stand == "R") %>% mutate(kzone = ifelse(c(plate_x >= -0.71 & plate_x <= 0.71 & 
                                                                                plate_z >= 1.5 & plate_z <= 3.5), 1, 0))
  } else if(input$side == "Left"){
    scrape_statcast_savant_pitcher(start_date = Date1(),
                                   end_date = Date2(),
                                   pitcherid = ID()) %>%
      mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
      filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
             !pitch_name == "", !pitch_name == "Other", stand == "L") %>% mutate(kzone = ifelse(c(plate_x >= -0.71 & plate_x <= 0.71 & 
                                                                            plate_z >= 1.5 & plate_z <= 3.5), 1, 0))
  } else {
    scrape_statcast_savant_pitcher(start_date = Date1(),
                                   end_date = Date2(),
                                   pitcherid = ID()) %>%
      mutate(pfx_x = -pfx_x*12) %>% mutate(pfx_z = pfx_z*12) %>%
      filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
             !pitch_name == "", !pitch_name == "Other") %>% mutate(kzone = ifelse(c(plate_x >= -0.71 & plate_x <= 0.71 & 
                                                            plate_z >= 1.5 & plate_z <= 3.5), 1, 0))
  }
})
  
  # Creating a seperate dataframe containing average pitch movement(used for p2)
  means <- reactive(dataset() %>% group_by(pitch_name) %>% summarize(
    "avgHorz" = mean(pfx_x),
    "avgVert" = mean(pfx_z),
    label = paste("(", round(avgVert, 1), "in ,", round(avgHorz, 1), "in)")
  ))
  
  # Creating HV plot using dataset dataframe/reactive function
  
  p1 <- reactive(ggplot(data = dataset(), aes(pfx_x, pfx_z)) +
    geom_point(aes(color = pitch_name)) +
    geom_segment(x=-30, xend=30, y=0, yend=0, color = "black") +
    geom_segment(x=0, xend=0, y=-30, yend=30, color = "black") +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(x = "Horizontal Movement(in.)", y = "Induced Vertical Movement(in.)") +
    scale_colour_manual(values = TMcolors)
  )
    
  
  # HV plot with average movement and ellipses
  p2 <- reactive(ggplot(data = dataset(), aes(pfx_x, pfx_z, color = pitch_name)) + 
    geom_point(alpha = 0.3) +
    geom_segment(x=-30, xend=30, y=0, yend=0, color = "black") + 
    geom_segment(x=0, xend=0, y=-30, yend=30, color = "black") +
    geom_point(data = means(), aes(avgHorz, avgVert, fill = pitch_name), 
               size = 5, alpha = 1, shape = 21, color = "black", ) +
    geom_label_repel(data = means(), aes(avgHorz, avgVert, label = label),
                       box.padding = unit(4, "lines")) +
    labs(x = "Horizontal Movement(in.)", y = "Induced Vertical Movement(in.)") + 
    stat_ellipse(data = dataset(), aes(pfx_x, pfx_z, color = pitch_name, fill = pitch_name),
                 geom = "polygon", alpha = 0.5, level = 0.9, type = "t",
                 linetype = "dashed") +
    scale_fill_manual(values = TMcolors) + 
    scale_color_manual(values = TMcolors) + 
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25))
  )
  
  
  # Putting p1 and p2 side by side as one plot
  output$HV <- renderPlot(wrap_plots(p1(), p2()))
  
    # Heatmap plot
  output$Heatmap <- renderPlot(
    ggplot(data = dataset(), aes(plate_x, plate_z), na.rm = TRUE) + 
    facet_wrap(~ pitch_name, nrow = 1) +
    geom_density_2d_filled(na.rm = TRUE, contour_var = "ndensity", 
                           show.legend = FALSE, bins = 8) +
    coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
    geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "gray") + 
    geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "gray") + 
    geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "gray") + 
    geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "gray")
  )
  

  # Table visuals
  # Pitch metrics
    output$Table <- render_gt(dataset() %>% group_by(pitch_name) %>%
    summarize(
      UsagePct = round(100*(n()/nrow(dataset())), 1),                      # usage rate
      AvgVelo = round(mean(release_speed, na.rm = TRUE),1),            # average velo
      "Velo Range (max / min)" = paste(round(max(release_speed),1),    # Max and Min Velo
                                       round(min(release_speed),1), sep = " / "),
      AvgSpinRate = round(mean(release_spin_rate, na.rm = TRUE), 0),   # avg spin rate
      BU = round((AvgSpinRate/AvgVelo), 1),                            # Bauer Units
      "Avg Vert Break" = round(mean(pfx_z, na.rm = TRUE),1),           # avg vert break
      "Avg Horz Break" = round(mean(pfx_x, na.rm = TRUE),1),           # avg horz break
      wOBA = round((sum(woba_value, na.rm = TRUE)/
                      sum(woba_denom, na.rm = TRUE)), 3),              # wOBA
      "Whiff%" = round(100*(sum(description == "swinging_strike")/     # Whiff Rate
                              sum(description == "swinging_strike",
                                  description == "foul",
                                  description == "hit_into_play")), 1),
      "HH%" = round(100*(sum(launch_speed >= 95 &                      # Hard Hit Rate
                               description == "hit_into_play"
                             , na.rm = TRUE) /
                           sum(description == "hit_into_play", na.rm = TRUE)), 1)
    ) %>% arrange(desc(UsagePct)) %>% gt() %>% gt_theme_538())
    
    
    # Plate discipline stats
    output$Table2 <- render_gt(dataset() %>% group_by(pitch_name) %>%
      summarize( 
        UsagePct = round(100*(n()/nrow(dataset())), 1),
        "Zone%" = round(100*(sum(kzone == 1)/n()),1),
        "Chase%" = round(100*(sum(kzone == 0 & 
                                    c(description == "swinging_strike", description == "foul",
                                      description == "hit_into_play"))/
                                sum(kzone == 0)),1),
        "Whiff%" = round(100*(sum(description == "swinging_strike")/
                                sum(description == "swinging_strike",
                                    description == "foul",
                                    description == "hit_into_play")), 1),
        "InZoneWhiff%" = round(100*sum(kzone == 1 & description == "swinging_strike")
                               /sum(kzone == 1 & c(description == "swinging_strike", 
                                                   description == "foul",
                                                   description == "hit_into_play")), 1),
        "CalledStrike%" = round(100*(sum(description == "called_strike")/nrow(dataset())),1),
        "CSW%" = round(100*((sum(description == "called_strike") + 
                               sum(description == "swinging_strike"))/n()),1)
      ) %>% arrange(desc(UsagePct)) %>% gt() %>% gt_theme_538())

}
# ------------------------------------------------------------------------------
  

shinyApp(ui = ui, server = server)