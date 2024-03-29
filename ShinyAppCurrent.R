# Load in all necessary packages
library(shiny)
library(baseballr)
library(tidyverse)
library(gt)
library(gtExtras)
library(patchwork)
library(ggplot2)
library(ggrepel)
library(scales)


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
  tabsetPanel(tabPanel(
    "Movement & Metrics",
    h3("HV Plot"),
    h5("Pitcher's POV"),
    plotOutput("HV"),
    h3("Data by pitch type"),
    dataTableOutput("Table"),
    h3("Plate Discipline"),
    dataTableOutput("Table2"),
    h3("Quality of Contact"),
    dataTableOutput("table3")),
  tabPanel("Heatmaps",
   h3("Heatmap, All pitches"),
    h5("Pitcher's POV"),
    plotOutput("Heatmap"),
    h3("Heatmap, Whiffs"),
    plotOutput("Heatmap2"),
    plotOutput("Heatmap2b"),
    h3("Heatmap, Hard Hit Balls"),
    plotOutput("Heatmap3"),
    plotOutput(("Heatmap3b"))),
  tabPanel("Release Point",
  h3("Release Point Characteristics"),
  h5("Home Plate View"),
  gt_output("ReleaseTable"),
  plotOutput("ReleasePlot"),
  plotOutput("ReleasePlot2"))
  ))
)
# ------------------------------------------------------------------------------





# Server------------------------------------------------------------------------

  
  

  
  
server <- function(input, output, session){
  
  
  
  
  # Loading in player data from Baseball Savant---------------------------------
  
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
          mutate(pfx_x = -pfx_x*12, plate_x = -plate_x) %>% mutate(pfx_z = pfx_z*12) %>%
          filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
                 !pitch_name == "", !pitch_name == "Other", stand == "R") %>% mutate(kzone = ifelse(c(plate_x >= -0.71 & plate_x <= 0.71 & 
                                                                                plate_z >= 1.5 & plate_z <= 3.5), 1, 0)) %>% 
          filter(game_type == "R" | game_type == "F" | game_type == "D" | 
                game_type == "L" | game_type == "W")
  } else if(input$side == "Left"){
    scrape_statcast_savant_pitcher(start_date = Date1(),
                                   end_date = Date2(),
                                   pitcherid = ID()) %>%
      mutate(pfx_x = -pfx_x*12, plate_x = -plate_x) %>% mutate(pfx_z = pfx_z*12) %>%
      filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
             !pitch_name == "", !pitch_name == "Other", stand == "L") %>% mutate(kzone = ifelse(c(plate_x >= -0.71 & plate_x <= 0.71 & 
                                                                            plate_z >= 1.5 & plate_z <= 3.5), 1, 0)) %>% 
      filter(game_type == "R" | game_type == "F" | game_type == "D" | 
             game_type == "L" | game_type == "W")
  } else {
    scrape_statcast_savant_pitcher(start_date = Date1(),
                                   end_date = Date2(),
                                   pitcherid = ID()) %>%
      mutate(pfx_x = -pfx_x*12, plate_x = -plate_x) %>% mutate(pfx_z = pfx_z*12) %>%
      filter(!pitch_name == "Intentional Ball", !pitch_name == "Pitch Out",
             !pitch_name == "", !pitch_name == "Other") %>% mutate(kzone = ifelse(c(plate_x >= -0.71 & plate_x <= 0.71 & 
                                                            plate_z >= 1.5 & plate_z <= 3.5), 1, 0)) %>% 
      filter(game_type == "R" | game_type == "F" | game_type == "D" | 
             game_type == "L" | game_type == "W")
  }
})
  
  # ----------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  # Movement & Metrics Tab------------------------------------------------------
  
  # Creating a seperate dataframe containing average pitch movement(used for p2)
  means <- reactive(dataset() %>% group_by(pitch_name) %>% summarize(
    "avgHorz" = mean(pfx_x, na.rm = TRUE),
    "avgVert" = mean(pfx_z, na.rm = TRUE),
    label = paste("(", round(avgVert, 1), "in ,", round(avgHorz, 1), "in)")
  ))
  
  # Creating HV plot using dataset dataframe/reactive function
  
  p1 <- reactive(ggplot(data = dataset(), aes(pfx_x, pfx_z)) +
    geom_segment(x=-30, xend=30, y=0, yend=0, color = "black") +
    geom_segment(x=0, xend=0, y=-30, yend=30, color = "black") +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
    geom_point(aes(color = pitch_name)) +
    labs(x = "Horizontal Movement(in.)", y = "Induced Vertical Movement(in.)") +
    scale_color_manual(values = TMcolors) + theme_light()
  )
    
  
  # HV plot with average movement and ellipses
  p2 <- reactive(ggplot(data = dataset(), aes(pfx_x, pfx_z, color = pitch_name)) + 
    # geom_point(alpha = 0.3) +
    geom_segment(x=-30, xend=30, y=0, yend=0, color = "black") + 
    geom_segment(x=0, xend=0, y=-30, yend=30, color = "black") +
    labs(x = "Horizontal Break(in.)", y = "Induced Vertical Break(in.)") + 
    stat_ellipse(data = dataset(), aes(pfx_x, pfx_z, color = pitch_name, fill = pitch_name),
                 geom = "polygon", alpha = 0.5, level = 0.9, type = "t",
                 linetype = "dashed") +
      geom_label_repel(data = means(), aes(avgHorz, avgVert, label = label),
                       box.padding = unit(4, "lines")) +
      geom_point(data = means(), aes(avgHorz, avgVert, fill = pitch_name), 
                 size = 5, alpha = 1, shape = 21, color = "black", ) +
    scale_fill_manual(values = TMcolors) + 
    scale_color_manual(values = TMcolors) + 
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) + theme_light()
  )
  
  
  # Putting p1 and p2 side by side as one plot
  output$HV <- renderPlot(wrap_plots(p1(), p2()), width = 1200, height = 400)
  
  
  # Tables
  
  # Pitch metrics
  output$Table <- renderDataTable(dataset() %>% group_by(pitch_name) %>%
                              summarize(
                                Pitches = n(),
                                UsagePct = percent(n()/nrow(dataset()), accuracy = .1),          # usage rate
                                "Avg Velo
                                (mph)" = round(mean(release_speed, na.rm = TRUE),1),            # average velo
                                "Velo Range (max / min)" = paste(round(max(release_speed, na.rm = TRUE),1),    # Max and Min Velo
                                                                 round(min(release_speed, na.rm = TRUE),1), sep = " / "),
                                "Avg Spin Rate
                                (rpm)" = round(mean(release_spin_rate, na.rm = TRUE), 0),   # avg spin rate
                                BU = round((mean(release_spin_rate, na.rm = TRUE)/
                                            mean(release_speed, na.rm = TRUE)), 1),                            # Bauer Units
                                "Avg Induced 
                                Vert. Break
                                (Inches)" = round(mean(pfx_z, na.rm = TRUE),1),           # avg vert break
                                "Avg Horz. Break
                                (Inches)" = round(mean(pfx_x, na.rm = TRUE),1)            # avg horz break
                              ) %>% arrange(desc(Pitches)),
                    options = list(dom = 't', columnDefs = list(list(targets = 0, visible = TRUE))) )
  
  
  # Plate discipline stats
  output$Table2 <- renderDataTable(dataset() %>% group_by(pitch_name) %>%
                               summarize( 
                                 Pitches = n(),
                                 UsagePct = percent(n()/nrow(dataset()), accuracy = .1),
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
                               ) %>% arrange(desc(Pitches)),
                options = list(dom = 't', columnDefs = list(list(targets = 0, visible = TRUE)))   )
  
  
  # Quality of contact/results stats
  
  BIP <- reactive(dataset() %>% filter(description == "hit_into_play"))
  
  output$table3 <- renderDataTable(BIP() %>% group_by(pitch_name) %>%
                               summarize( 
                                 BBE = n(),
                                 "Hard Hit %" = percent(sum(launch_speed >= 95 &                      # Hard Hit Rate
                                                       description == "hit_into_play"
                                                     , na.rm = TRUE)/
                                                   sum(description == "hit_into_play", na.rm = TRUE), accuracy = .1),
                                 "Avg Exit Velo
                                 (mph)" = round(mean(launch_speed, na.rm = TRUE), 1),
                                 "Avg Launch Angle
                                 (degrees)" = round(mean(launch_angle, na.rm = TRUE), 1),
                                 BABIP = round((sum(events == "single" | events == "double" | events == "triple")/
                                                sum(events == "single" | events == "double" | events == "triple",
                                                    events == "field_out" | events == "force_out" | events == "grounded_into_double_play",
                                                    events == "sac_fly" | events == "field_error")), 3)
                               ) %>% arrange(desc(BBE)),
                      options = list(dom = 't', columnDefs = list(list(targets = 0, visible = TRUE))) )
  
  # ----------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  # Heatmaps Tab----------------------------------------------------------------
    # Heatmap plot of all pitches
  output$Heatmap <- renderPlot(
    ggplot(data = dataset(), aes(plate_x, plate_z), na.rm = TRUE) + 
    facet_wrap(~ pitch_name, nrow = 1) +
    # geom_density_2d_filled(na.rm = TRUE, contour_var = "ndensity", 
    #                        show.legend = FALSE, bins = 40) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, show.legend = FALSE) +
    scale_fill_gradientn(colours = c("blue", "white", "red")) +
    coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
    geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "gray") + 
    geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "gray") + 
    geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "gray") + 
    geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "gray") +
    labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location") +
      theme_bw(),
  width = 1200)
  
  # Heatmap for whiffs
  
  # Creating separate dataset with only whiffs, instead of all pitches
  whiffs <- reactive(dataset() %>% filter(description == "swinging_strike"))
  
  # Heatmap of whiffs, with density plot
  output$Heatmap2 <- renderPlot(
    ggplot(data = whiffs(), aes(plate_x, plate_z), na.rm = TRUE) + 
      facet_wrap(~ pitch_name, nrow = 1) +
      # geom_density_2d_filled(na.rm = TRUE, contour_var = "ndensity", 
      #                        show.legend = FALSE, bins = 40) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, show.legend = FALSE) +
      scale_fill_gradientn(colours = c("blue", "white", "red")) +
      coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
      geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "gray") + 
      geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "gray") + 
      geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "gray") + 
      geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "gray") +
      labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location") +
      theme_bw(),
  width = 1200)
  
  # Heatmap of whiffs with dot plot(geom_point)
  output$Heatmap2b <- renderPlot(
    ggplot(data = whiffs(), aes(plate_x, plate_z), na.rm = TRUE) + 
      facet_wrap(~ pitch_name, nrow = 1) +
      geom_point(alpha = 0.65, shape = 21, color = "black", fill = "grey", 
                 size = 2.5, na.rm = TRUE) +
      coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
      geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "black") + 
      geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "black") + 
      geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "black") + 
      geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "black") +
      labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location") +
      theme_bw(),
    width = 1200)
  
  
  # Heatmap of hard hit balls
  
  # Dataset with all hard hit balls
  HardHitBalls <- reactive(dataset() %>% filter(launch_speed >= 95))
  
  # Heatmap of hard hit balls, with density plot
  output$Heatmap3 <- renderPlot(
    ggplot(data = HardHitBalls(), aes(plate_x, plate_z), na.rm = TRUE) + 
      facet_wrap(~ pitch_name, nrow = 1) +
      # geom_density_2d_filled(na.rm = TRUE, contour_var = "ndensity", 
      #                        show.legend = FALSE, bins = 40) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, show.legend = FALSE) +
      scale_fill_gradientn(colours = c("blue", "white", "red")) +
      coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
      geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "gray") + 
      geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "gray") + 
      geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "gray") + 
      geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "gray") +
      labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location") +
      theme_bw(),
    width = 1200)
  
  # Heatmap of hard hit balls, with dot plot(geom_point)
  output$Heatmap3b <- renderPlot(
    ggplot(data = HardHitBalls(), aes(plate_x, plate_z), na.rm = TRUE) + 
      facet_wrap(~ pitch_name, nrow = 1) +
      geom_point(alpha = 0.65, shape = 21, color = "black", fill = "grey",
                 size = 2.5, na.rm = TRUE) +
      coord_equal(xlim= c(-2,2), ylim = c(-1,5)) + 
      geom_segment(x=-0.71, xend=0.71, y=3.5, yend=3.5, col = "black") + 
      geom_segment(x=-0.71, xend=0.71, y=1.5, yend=1.5, col = "black") + 
      geom_segment(x=-0.71, xend=-0.71, y=1.5, yend=3.5, col = "black") + 
      geom_segment(x=0.71, xend=0.71, y=1.5, yend=3.5, col = "black") +
      labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location") +
      theme_bw(),
    width = 1200)
  
  # ----------------------------------------------------------------------------
  
  
  
  
  
  
  
 
    
    # Release Point Tab---------------------------------------------------------
    
    # Creating a dataframe with average release point to label release point on plot
    ReleaseMean <- reactive(dataset() %>% summarize(
      RelSide = mean(release_pos_x, na.rm = TRUE),
      RelHeight = mean(release_pos_z, na.rm = TRUE),
      label = paste("(", round(RelSide, 1), "ft ,", round(RelHeight, 1), "ft)")
    ))
  
  
  ReleaseMeanPitchTypes <- reactive(dataset() %>% group_by(pitch_name)%>% summarize(
    RelSide = mean(release_pos_x, na.rm = TRUE),
    RelHeight = mean(release_pos_z, na.rm = TRUE),
    label = paste("(", round(RelSide, 1), "ft ,", round(RelHeight, 1), "ft)")
  ))
    
    
    
    # Home plate view release point plot
    plot1 <- reactive(ggplot(dataset(), aes(release_pos_x, release_pos_z)) + 
      geom_point(aes(color = pitch_name), alpha = 0.3) +
      geom_point(data = ReleaseMean(), aes(RelSide, RelHeight), size = 5, shape = 21, fill = "grey", color = "black") +
      geom_label_repel(data = ReleaseMean(), aes(RelSide, RelHeight, label = label),
                       box.padding = unit(4, "lines")) +
      coord_equal(xlim = c(-5, 5), ylim = c(0.3, 8)) +
      labs(x = "Horizontal Release Point", y = "Vertical Release Point") +
      scale_color_manual(values = TMcolors) + theme_light())
    
    plot2 <- reactive(ggplot(dataset(), aes(release_pos_x, release_pos_z)) + 
      geom_point(data = ReleaseMeanPitchTypes(), aes(RelSide, RelHeight, fill = pitch_name), size = 5, shape = 21, color = "black") +
      geom_label_repel(data = ReleaseMeanPitchTypes(), aes(RelSide, RelHeight, label = label),
                       box.padding = unit(4, "lines")) +
      coord_equal(xlim = c(-5, 5), ylim = c(0.3, 8)) +
      labs(x = "Horizontal Release Point", y = "Vertical Release Point") +
      scale_color_manual(values = TMcolors) + 
      scale_fill_manual(values = TMcolors) + theme_light())
    
    output$ReleasePlot <- renderPlot(wrap_plots(plot1(), plot2()), width = 1200, height = 400)
    
    # Zoomed in release point, showing each pitch type
    output$ReleasePlot2 <- renderPlot(ggplot(dataset(), aes(release_pos_x, release_pos_z)) + 
      stat_ellipse(aes(color = pitch_name, fill = pitch_name), geom = "polygon",
                       alpha = 0.3, level = 0.9, type = "t", linetype = "dashed") +
      geom_point(aes(color = pitch_name), alpha = 0.75) +
      labs(x = "Horizontal Release Point", y = "Vertical Release Point") +
      scale_color_manual(values = TMcolors) +
      scale_fill_manual(values = TMcolors) + theme_light())
    
    # Table showing release metrics & effective velocity
    output$ReleaseTable <- render_gt(dataset() %>% group_by(pitch_name) %>% summarize(
      Pitches = n(),
      "Usage%" = percent(n()/nrow(dataset()), accuracy = .1),
      "Rel
      Side
      (Feet)" = round(mean(release_pos_x, na.rm = TRUE), 1),
      "Rel
      Height
      (Feet)" = round(mean(release_pos_z, na.rm = TRUE), 1),
      "Extension
      (Feet)" = round(mean(release_extension, na.rm = TRUE), 1),
      "Effective
      Velocity
      (mph)" = round(mean(effective_speed, na.rm = TRUE), 1),
      "Velocity
      (mph)" = round(mean(release_speed, na.rm = TRUE), 1)
      ) %>% arrange(desc(Pitches)) %>% gt() %>% gt_theme_538())
}
# ------------------------------------------------------------------------------
  
  
  
  
  # ----------------------------------------------------------------------------

  
shinyApp(ui = ui, server = server)
