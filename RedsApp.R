
#setwd("/Users/daveyount/Desktop/Baseball/RedsHackathon/Data/")

#rsconnect::setAccountInfo(name='reds', token='8C429337F7C61482DEC27F3582BE94CE', secret='7YaVHSoEVKM5VwOkxmjViirq51QV+i/B2iupY7zc')

# load packages

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggpubr)
library(gridExtra)
library(janitor)
library(plotly)
library(paletteer)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(shinythemes)
library(shiny)
library(rsconnect)
#rsconnect::deployApp('/Users/daveyount/Desktop/Baseball/RedsHackathon/Data')
#deployApp()

# Import data / CSV & Remove Extra Column "V1"

fangraphs=fread("fangraphs_season_level.csv")
#fangraphs <- subset(fangraphs, select = -c(V1))

statcast=fread("savant_pitch_level.csv")
#statcast <- subset(statcast, select = -c(V1))

preimputed=fread("preimputation_fangraphs.csv")
#preimputed <- subset(preimputed, select= -c(V1))

statcast <- statcast %>%
  mutate(Date = game_date,
         Pitcher = player_name,
         PitcherTeam = case_when(
           inning_topbot == 'Top' ~ home_team,
           inning_topbot == 'Bot' ~ away_team,
           .default = ''
         ))


statcast <- statcast[statcast$pitch_name != ""]
statcast <- statcast[statcast$pitch_name != "Eephus"]
statcast <- statcast[statcast$pitch_name != "Pitch Out"]
statcast <- statcast[statcast$pitch_name != "Other"]
statcast <- statcast[statcast$pitch_name != "Screwball"]
statcast <- statcast[statcast$pitch_name != 'Knuckleball']

table(statcast$pitch_name)

statcast$pitch_name[statcast$pitch_name == "Knuckle Curve"] <- "Curveball"
statcast$pitch_name[statcast$pitch_name == "Slow Curve"] <- "Curveball"
statcast$pitch_name[statcast$pitch_name == "Slurve"] <- "Curveball"
statcast$pitch_name[statcast$pitch_name == "Split-Finger"] <- "Splitter"
statcast$pitch_name[statcast$pitch_name == "Forkball"] <- "Splitter"
statcast$pitch_name[statcast$pitch_name == "4-Seam Fastball"] <- "Fastball"
# inning_topbot, home_team, away_team



fangraphs <- fangraphs[fangraphs$PitcherTeam != "- - -"]
unique(fangraphs$PitcherTeam)
# write.csv(fangraphs,"fangraphs_season_level.csv")

# Converts Character Date into Proper Format

fangraphs$Date <- mdy(fangraphs$Date)


unique(fangraphs$PitcherTeam)

### Link to a list of various Shiny Inputs we can use

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

colnames(statcast)

# Start of the UI - Part 1 of App Structure

ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; justify-content: flex-start;",
    img(src = "reds.png", style = "max-height: 40px; margin-right: 10px;", id = "nav-logo"),
    tags$span("Big Red Machine Learning App", style="font-weight: bold; font-family: 'Calibri', sans-serif; color: #FFFFFF;")
  ),
  
  theme = shinytheme("flatly"),
  
  tags$head(tags$style(HTML('
    /* Ensure that the navbar retains its red background */
    .navbar, .navbar-default {
      background-color: #C6011F !important;
    }

    /* Style for the logo and title alignment */
    #nav-logo {
      max-height: 40px;
      margin-right: 10px;
    }

    /* Style for the navbar-brand to ensure it has the correct font and does not change on hover */
    .navbar-brand {
      font-family: "Calibri", sans-serif;
      font-weight: bold;
      color: #FFFFFF !important;
    }
    .navbar-brand:hover {
      color: #FFFFFF !important; /* Keeps the color white on hover */
      background-color: transparent !important; /* Avoid any background change on hover */
    }
    
    /* Style for the navbar-nav links */
    .navbar-default .navbar-nav>li>a {
      color: #000000 !important; /* Text color */
      background-color: transparent !important; /* Background color */
      font-family: "Calibri", sans-serif; /* Font family */
      font-weight: bold; /* Font weight */
    }
    .navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {
      color: #FFFFFF !important; /* Text color on hover */
      background-color: #A50017 !important; /* Background color on hover */
    }

    /* Style for the active navbar-nav link */
    .navbar-default .navbar-nav>.active>a {
      background-color: #FFFFFF !important; /* Background color for the active link */
      color: #000000 !important; /* Text color for the active link */
    }
    .navbar-default .navbar-nav>.active>a:hover {
      background-color: #D3D3D3 !important; /* Background color on hover for the active link */
      color: #000000 !important; /* Text color on hover for the active link */
    }
    
    /* Additional custom styles can go here */
  '))),
  
  
  tabPanel("Pitchers",
           
           sidebarLayout(
             
             sidebarPanel(
               
               
               
               selectInput("PitcherTeam", label = "Team",
                           
                           choices = levels(as.factor(statcast$PitcherTeam))),
               
               selectInput("Pitcher", label = "Pitcher",
                           
                           choices = levels(as.factor(statcast$Pitcher))),
               
               dateRangeInput("Date", label = "Date Range",
                              start = min(statcast$Date),
                              end = max(statcast$Date),
                              min = min(statcast$Date),
                              max = max(statcast$Date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               
               checkboxGroupInput("Pitch", label = "Pitch Type(s)", 
                                  choices = levels(as.factor(statcast$pitch_name))),
               
               (img(src = "redsanalytics.png", width = 100)),
               
               
               width = 2),
             
             
             mainPanel(
               
               fluidRow(splitLayout(cellwidths = c("50%", "50%"), plotOutput("Strike_Zone"), plotOutput("Percentiles"))),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(DTOutput("Percentiles_Data")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Velo_Chart")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Spray_Chart")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Heat_Map"))
               
             )),
           
  ),
  
  tags$footer(
    style = "background-color: #C6011F; color: white; text-align: center; padding: 10px; font-family: 'Calibri', sans-serif; font-weight: bold; width: 100%",
    "This App was created by Dave Yount, Jack Hinde, Jacob Weber, Anthony Vicinanzo, and Shane Hauck"
  )
  
)



# Start of the Server - Part 2 of App Structure

server = function(input, output, session) {
  
  # Input Reactions -- Pitcher Tab
  
  
  # Pitchers Based on PitcherTeam    
  
  observeEvent(
    input$PitcherTeam,
    updateSelectInput(session,
                      "Pitcher", "Pitcher",
                      choices = levels(factor(filter(statcast,
                                                     PitcherTeam == isolate(input$PitcherTeam))$Pitcher))))
  
  
  # Date Range Based on When Pitcher Threw
  
  observeEvent(
    input$Pitcher,
    updateDateRangeInput(session,
                         "Date", "Date Range",
                         start = min(statcast$Date),
                         end = max(statcast$Date)))
  
  
  # Pitch Types Based on Pitcher
  
  observeEvent(
    input$Pitcher,
    updateCheckboxGroupInput(session,
                             "Pitch", "Pitch Type(s)",
                             choices = levels(factor(filter(statcast,
                                                            Pitcher == isolate(input$Pitcher))$pitch_name))))
  
  
  
  
  
  
  
  # Start of Outputs (Plots and Data Tables)
  
  # Strike_Zone
  
  output$Strike_Zone <- renderPlot({
    
    statcast%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% input$Pitch, c(Date >= input$Date[1] & Date <= input$Date[2])) %>%
      ggplot(statcast, mapping = aes(x=plate_x, y= plate_z)) +
      geom_point(aes(color = pitch_name),size = 3, alpha=0.5) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "brown",
                                    Splitter = "purple", Knuckleball = "cyan", Forkball = "pink", Sweeper = "gold"))+
      geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
      geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
      geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
      geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
      
      
      geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
      
      
      geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
      geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none") + 
      xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Strike Zone")
    
    
  })
  
  # Percentiles
  
  output$Percentiles <- renderPlot({
    
    statcast <- statcast%>%filter(c(Date >= input$Date[1] & Date <= input$Date[2]))  
    
    statcast$abs_pfx_x <- abs(statcast$pfx_x)
    statcast$abs_pfx_z <- abs(statcast$pfx_z)
    
    # Rounding to Single Decimal Point
    
    Statcast_MaxMetrics <- statcast[, .(
      'Max Velo' = max(release_speed, na.rm = TRUE),
      'Max Spin' = max(release_spin_rate, na.rm = TRUE),
      'Max H Break' = max(abs_pfx_x, na.rm = TRUE),
      'Max V Break' = max(abs_pfx_z, na.rm = TRUE)),
      by=.(PitcherTeam, Pitcher, pitch_name)]
    
    Statcast_MaxMetrics <- Statcast_MaxMetrics[order(Statcast_MaxMetrics$PitcherTeam, Statcast_MaxMetrics$Pitcher, Statcast_MaxMetrics$pitch_name)]
    
    Statcast_Usage <- statcast %>%
      group_by(PitcherTeam, Pitcher, pitch_name) %>%
      tally()
    Pitcher_Pitches <- statcast %>% 
      group_by(PitcherTeam, Pitcher) %>% 
      tally()
    
    Statcast_Usage <- merge(Statcast_Usage, Pitcher_Pitches, by=c('PitcherTeam', 'Pitcher'))
    
    Statcast_Usage$'Usage %' <- round(Statcast_Usage$n.x / Statcast_Usage$n.y, 4)
    
    Statcast_Usage$'Usage %' <- Statcast_Usage$'Usage %' * 100
    
    Statcast_MaxMetrics$'Usage %' <- Statcast_Usage$'Usage %'
    
    Statcast_MaxMetrics$'Max Velo' <- round(Statcast_MaxMetrics$'Max Velo', digits = 1)
    Statcast_MaxMetrics$'Max Spin' <- round(Statcast_MaxMetrics$'Max Spin', digits = 0)
    Statcast_MaxMetrics$'Max H Break' <- round(Statcast_MaxMetrics$'Max H Break', digits = 1)
    Statcast_MaxMetrics$'Max V Break' <- round(Statcast_MaxMetrics$'Max V Break', digits = 1)
    
    Statcast_MaxMetrics$'Max Spin'[Statcast_MaxMetrics$'Max Spin' < 0] <- 0
    Statcast_MaxMetrics$'Max H Break'[Statcast_MaxMetrics$'Max H Break' < 0] <- 0
    Statcast_MaxMetrics$'Max V Break'[Statcast_MaxMetrics$'Max V Break' < 0] <- 0
    
    get_Percentiles <- function(pitch_type) {
      Pitch_Type <- Statcast_MaxMetrics[Statcast_MaxMetrics$pitch_name == pitch_type]
      
      Pitch_Type$MaxVelo_percentile <- Pitch_Type$`Max Velo`
      Pitch_Type$MaxVelo_percentile <- round(Pitch_Type$MaxVelo_percentile, digits = 2)
      Pitch_Type$MaxSpin_percentile <- Pitch_Type$`Max Spin`
      Pitch_Type$MaxSpin_percentile <- round(Pitch_Type$MaxSpin_percentile, digits = 2)
      Pitch_Type$MaxHBreak_percentile <- Pitch_Type$`Max H Break`
      Pitch_Type$MaxHBreak_percentile <- round(Pitch_Type$MaxHBreak_percentile, digits = 2)
      Pitch_Type$MaxVBreak_percentile <- Pitch_Type$`Max V Break`
      Pitch_Type$MaxVBreak_percentile <- round(Pitch_Type$MaxVBreak_percentile, digits = 2)
      
      Pitch_Type$MaxVelo_ranking <- rep(0, nrow(Pitch_Type))
      Pitch_Type$MaxSpin_ranking <- rep(0, nrow(Pitch_Type))
      Pitch_Type$MaxHBreak_ranking <- rep(0, nrow(Pitch_Type))
      Pitch_Type$MaxVBreak_ranking <- rep(0, nrow(Pitch_Type))
      
      Pitch_Type$MaxVelo_ranking[order(Pitch_Type$MaxVelo_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxVelo_percentile <-  1 - ((Pitch_Type$MaxVelo_ranking) / max(Pitch_Type$MaxVelo_ranking, na.rm = TRUE))
      Pitch_Type$MaxVelo_percentile <- round(Pitch_Type$MaxVelo_percentile, digits = 2)
      Pitch_Type$MaxVelo_percentile <- Pitch_Type$MaxVelo_percentile*100
      
      Pitch_Type$MaxSpin_ranking[order(Pitch_Type$MaxSpin_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxSpin_percentile <-  1 - ((Pitch_Type$MaxSpin_ranking) / max(Pitch_Type$MaxSpin_ranking, na.rm = TRUE))
      Pitch_Type$MaxSpin_percentile <- round(Pitch_Type$MaxSpin_percentile, digits = 2)
      Pitch_Type$MaxSpin_percentile <- Pitch_Type$MaxSpin_percentile*100
      
      Pitch_Type$MaxHBreak_ranking[order(Pitch_Type$MaxHBreak_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxHBreak_percentile <-  1 - ((Pitch_Type$MaxHBreak_ranking) / max(Pitch_Type$MaxHBreak_ranking, na.rm = TRUE))
      Pitch_Type$MaxHBreak_percentile <- round(Pitch_Type$MaxHBreak_percentile, digits = 2)
      Pitch_Type$MaxHBreak_percentile <- Pitch_Type$MaxHBreak_percentile*100
      
      Pitch_Type$MaxVBreak_ranking[order(Pitch_Type$MaxVBreak_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxVBreak_percentile <-  1 - ((Pitch_Type$MaxVBreak_ranking) / max(Pitch_Type$MaxVBreak_ranking, na.rm = TRUE))
      Pitch_Type$MaxVBreak_percentile <- round(Pitch_Type$MaxVBreak_percentile, digits = 2)
      Pitch_Type$MaxVBreak_percentile <- Pitch_Type$MaxVBreak_percentile*100
      
      return(Pitch_Type)
    }
    
    Statcast_Percentiles <- get_Percentiles("Fastball")
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Changeup"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Curveball"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Slider"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Sinker"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Cutter"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Splitter"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Sweeper"))
    
    # Low
    
    All_Data_Low=subset(Statcast_Percentiles, select = c(1:3))
    
    All_Data_Low <- All_Data_Low[!duplicated(All_Data_Low)]
    
    colnames(All_Data_Low)
    
    All_Data_Low$pitch_name <- "Low"
    All_Data_Low$`Max Velo` <- 0
    All_Data_Low$`Max Spin` <- 0
    All_Data_Low$`Max H Break` <- 0
    All_Data_Low$`Max V Break` <- 0
    All_Data_Low$`Usage %` <- 1
    
    All_Data_Low$MaxVelo_percentile <- -5
    All_Data_Low$MaxVelo_ranking <- 20
    All_Data_Low$MaxSpin_percentile <- -5
    All_Data_Low$MaxSpin_ranking <- -5
    All_Data_Low$MaxHBreak_percentile <- -5
    All_Data_Low$MaxHBreak_ranking <- 20
    All_Data_Low$MaxVBreak_percentile <- -5
    All_Data_Low$MaxVBreak_ranking <- 20
    
    # High
    
    All_Data_High=subset(Statcast_Percentiles, select = c(1:3))
    
    All_Data_High <- All_Data_High[!duplicated(All_Data_High)]
    
    colnames(All_Data_High)
    
    All_Data_High$pitch_name <- "High"
    All_Data_High$`Max Velo` <- 100
    All_Data_High$`Max Spin` <- 100
    All_Data_High$`Max H Break` <- 100
    All_Data_High$`Max V Break` <- 100
    All_Data_High$`Usage %` <- 1
    
    All_Data_High$MaxVelo_percentile <- 105
    All_Data_High$MaxVelo_ranking <- 1
    All_Data_High$MaxSpin_percentile <- 105
    All_Data_High$MaxSpin_ranking <- 1
    All_Data_High$MaxHBreak_percentile <- 105
    All_Data_High$MaxHBreak_ranking <- 1
    All_Data_High$MaxVBreak_percentile <- 105
    All_Data_High$MaxVBreak_ranking <- 1
    
    # Rbind to Combine Low and High
    
    LowHigh <- rbind(All_Data_Low, All_Data_High)
    
    # Rbind to Combine LowHigh with Statcast_Percentiles
    
    Statcast_Percentiles <- rbind(Statcast_Percentiles, LowHigh)
    
    # Max Velo
    
    MaxVelo <- Statcast_Percentiles%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% Statcast_Percentiles$pitch_name[Statcast_Percentiles$'Usage %' >= 5]) %>%
      ggplot(Statcast_Percentiles, mapping = aes(x= MaxVelo_percentile, y= pitch_name, colour = (MaxVelo_percentile))) +
      geom_line() + geom_point(size = 5)  +
      ggtitle("Max Velo") + xlim(0, 100) + ylim(Statcast_Percentiles$pitch_name[(Statcast_Percentiles$PitcherTeam == input$PitcherTeam) & (Statcast_Percentiles$Pitcher == input$Pitcher) & (Statcast_Percentiles$'Usage %' >= 0.1) & !(Statcast_Percentiles$pitch_name %in% c("High", "Low"))]) +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = pitch_name, yend = pitch_name), color = "#9b9b9b", size = 0.5) +
      geom_point(aes(x = 0, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 50, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 100, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = MaxVelo_percentile, y = pitch_name, fill = MaxVelo_percentile), pch = 21, color = "black", size = 6) +
      geom_text(aes(label=MaxVelo_percentile),hjust=.5, vjust=.4, color = "White",
                size = 4)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, high = "#cc0000", mid = "#ffffff", low = "#2952a3",
                           na.value = "grey50") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    # Max Spin
    
    MaxSpin <- Statcast_Percentiles%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% Statcast_Percentiles$pitch_name[Statcast_Percentiles$'Usage %' >= 5]) %>%
      ggplot(Statcast_Percentiles, mapping = aes(x= MaxSpin_percentile, y= pitch_name, colour = (MaxSpin_percentile))) +
      geom_line() + geom_point(size = 5)  +
      ggtitle("Max Spin") + xlim(0, 100) + ylim(Statcast_Percentiles$pitch_name[(Statcast_Percentiles$PitcherTeam == input$PitcherTeam) & (Statcast_Percentiles$Pitcher == input$Pitcher) & (Statcast_Percentiles$'Usage %' >= 0.1) & !(Statcast_Percentiles$pitch_name %in% c("High", "Low"))]) +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = pitch_name, yend = pitch_name), color = "#9b9b9b", size = 0.5) +
      geom_point(aes(x = 0, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 50, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 100, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = MaxSpin_percentile, y = pitch_name, fill = MaxSpin_percentile), pch = 21, color = "black", size = 6) +
      geom_text(aes(label=MaxSpin_percentile),hjust=.5, vjust=.4, color = "White",
                size = 4)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, high = "#cc0000", mid = "#ffffff", low = "#2952a3") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    # Max H Break
    
    MaxHBreak <- Statcast_Percentiles%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% Statcast_Percentiles$pitch_name[Statcast_Percentiles$'Usage %' >= 5]) %>%
      ggplot(Statcast_Percentiles, mapping = aes(x= MaxHBreak_percentile, y= pitch_name, colour = (MaxHBreak_percentile))) +
      geom_line() + geom_point(size = 5)  +
      ggtitle("Max H Break") + xlim(0, 100) + ylim(Statcast_Percentiles$pitch_name[(Statcast_Percentiles$PitcherTeam == input$PitcherTeam) & (Statcast_Percentiles$Pitcher == input$Pitcher) & (Statcast_Percentiles$'Usage %' >= 0.1) & !(Statcast_Percentiles$pitch_name %in% c("High", "Low"))]) +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = pitch_name, yend = pitch_name), color = "#9b9b9b", size = 0.5) +
      geom_point(aes(x = 0, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 50, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 100, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = MaxHBreak_percentile, y = pitch_name, fill = MaxHBreak_percentile), pch = 21, color = "black", size = 6) +
      geom_text(aes(label=MaxHBreak_percentile),hjust=.5, vjust=.4, color = "White",
                size = 4)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, high = "#cc0000", mid = "#ffffff", low = "#2952a3") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    # Max V Break
    
    MaxVBreak <- Statcast_Percentiles%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% Statcast_Percentiles$pitch_name[Statcast_Percentiles$'Usage %' >= 5]) %>%
      ggplot(Statcast_Percentiles, mapping = aes(x= MaxVBreak_percentile, y= pitch_name, colour = (MaxVBreak_percentile))) +
      geom_line() + geom_point(size = 5)  +
      ggtitle("Max V Break") + xlim(0, 100) + ylim(Statcast_Percentiles$pitch_name[(Statcast_Percentiles$PitcherTeam == input$PitcherTeam) & (Statcast_Percentiles$Pitcher == input$Pitcher) & (Statcast_Percentiles$'Usage %' >= 0.1) & !(Statcast_Percentiles$pitch_name %in% c("High", "Low"))]) +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = pitch_name, yend = pitch_name), color = "#9b9b9b", size = 0.5) +
      geom_point(aes(x = 0, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 50, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = 100, y = pitch_name), color = "#9b9b9b", size = 3) +
      geom_point(aes(x = MaxVBreak_percentile, y = pitch_name, fill = MaxVBreak_percentile), pch = 21, color = "black", size = 6) +
      geom_text(aes(label=MaxVBreak_percentile),hjust=.5, vjust=.4, color = "White",
                size = 4)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, high = "#cc0000", mid = "#ffffff", low = "#2952a3") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    ggarrange(
      MaxVelo, MaxSpin, MaxHBreak, MaxVBreak, nrow = 2, ncol = 2)
  })
  
  
  
  
  # Percentiles_Data
  
  output$Percentiles_Data <- renderDT({
    
    statcast <- statcast%>%filter(c(Date >= input$Date[1] & Date <= input$Date[2]))  
    
    statcast$abs_pfx_x <- abs(statcast$pfx_x)
    statcast$abs_pfx_z <- abs(statcast$pfx_z)
    
    # Rounding to Single Decimal Point
    
    Statcast_MaxMetrics <- statcast[, .(
      'Max Velo' = max(release_speed, na.rm = TRUE),
      'Max Spin' = max(release_spin_rate, na.rm = TRUE),
      'Max H Break' = max(abs_pfx_x, na.rm = TRUE),
      'Max V Break' = max(abs_pfx_z, na.rm = TRUE)),
      by=.(PitcherTeam, Pitcher, pitch_name)]
    
    Statcast_MaxMetrics <- Statcast_MaxMetrics[order(Statcast_MaxMetrics$PitcherTeam, Statcast_MaxMetrics$Pitcher, Statcast_MaxMetrics$pitch_name)]
    
    Statcast_Usage <- statcast %>%
      group_by(PitcherTeam, Pitcher, pitch_name) %>%
      tally()
    Pitcher_Pitches <- statcast %>% 
      group_by(PitcherTeam, Pitcher) %>% 
      tally()
    
    Statcast_Usage <- merge(Statcast_Usage, Pitcher_Pitches, by=c('PitcherTeam', 'Pitcher'))
    
    Statcast_Usage$'Usage %' <- round(Statcast_Usage$n.x / Statcast_Usage$n.y, 4)
    
    Statcast_MaxMetrics$'Usage %' <- Statcast_Usage$'Usage %' * 100
    
    Statcast_MaxMetrics$'Max Velo' <- round(Statcast_MaxMetrics$'Max Velo', digits = 1)
    Statcast_MaxMetrics$'Max Spin' <- round(Statcast_MaxMetrics$'Max Spin', digits = 0)
    Statcast_MaxMetrics$'Max H Break' <- round(Statcast_MaxMetrics$'Max H Break', digits = 1)
    Statcast_MaxMetrics$'Max V Break' <- round(Statcast_MaxMetrics$'Max V Break', digits = 1)
    
    Statcast_MaxMetrics$'Max Spin'[Statcast_MaxMetrics$'Max Spin' < 0] <- 0
    Statcast_MaxMetrics$'Max H Break'[Statcast_MaxMetrics$'Max H Break' < 0] <- 0
    Statcast_MaxMetrics$'Max V Break'[Statcast_MaxMetrics$'Max V Break' < 0] <- 0
    
    get_Percentiles <- function(pitch_type) {
      Pitch_Type <- Statcast_MaxMetrics[Statcast_MaxMetrics$pitch_name == pitch_type]
      
      Pitch_Type$MaxVelo_percentile <- Pitch_Type$`Max Velo`
      Pitch_Type$MaxVelo_percentile <- round(Pitch_Type$MaxVelo_percentile, digits = 2)
      Pitch_Type$MaxSpin_percentile <- Pitch_Type$`Max Spin`
      Pitch_Type$MaxSpin_percentile <- round(Pitch_Type$MaxSpin_percentile, digits = 2)
      Pitch_Type$MaxHBreak_percentile <- Pitch_Type$`Max H Break`
      Pitch_Type$MaxHBreak_percentile <- round(Pitch_Type$MaxHBreak_percentile, digits = 2)
      Pitch_Type$MaxVBreak_percentile <- Pitch_Type$`Max V Break`
      Pitch_Type$MaxVBreak_percentile <- round(Pitch_Type$MaxVBreak_percentile, digits = 2)
      
      Pitch_Type$MaxVelo_ranking <- rep(0, nrow(Pitch_Type))
      Pitch_Type$MaxSpin_ranking <- rep(0, nrow(Pitch_Type))
      Pitch_Type$MaxHBreak_ranking <- rep(0, nrow(Pitch_Type))
      Pitch_Type$MaxVBreak_ranking <- rep(0, nrow(Pitch_Type))
      
      Pitch_Type$MaxVelo_ranking[order(Pitch_Type$MaxVelo_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxVelo_percentile <-  1 - ((Pitch_Type$MaxVelo_ranking) / max(Pitch_Type$MaxVelo_ranking, na.rm = TRUE))
      Pitch_Type$MaxVelo_percentile <- round(Pitch_Type$MaxVelo_percentile, digits = 2)
      Pitch_Type$MaxVelo_percentile <- Pitch_Type$MaxVelo_percentile*100
      
      Pitch_Type$MaxSpin_ranking[order(Pitch_Type$MaxSpin_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxSpin_percentile <-  1 - ((Pitch_Type$MaxSpin_ranking) / max(Pitch_Type$MaxSpin_ranking, na.rm = TRUE))
      Pitch_Type$MaxSpin_percentile <- round(Pitch_Type$MaxSpin_percentile, digits = 2)
      Pitch_Type$MaxSpin_percentile <- Pitch_Type$MaxSpin_percentile*100
      
      Pitch_Type$MaxHBreak_ranking[order(Pitch_Type$MaxHBreak_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxHBreak_percentile <-  1 - ((Pitch_Type$MaxHBreak_ranking) / max(Pitch_Type$MaxHBreak_ranking, na.rm = TRUE))
      Pitch_Type$MaxHBreak_percentile <- round(Pitch_Type$MaxHBreak_percentile, digits = 2)
      Pitch_Type$MaxHBreak_percentile <- Pitch_Type$MaxHBreak_percentile*100
      
      Pitch_Type$MaxVBreak_ranking[order(Pitch_Type$MaxVBreak_percentile, decreasing = TRUE)] <- 1:nrow(Pitch_Type)
      Pitch_Type$MaxVBreak_percentile <-  1 - ((Pitch_Type$MaxVBreak_ranking) / max(Pitch_Type$MaxVBreak_ranking, na.rm = TRUE))
      Pitch_Type$MaxVBreak_percentile <- round(Pitch_Type$MaxVBreak_percentile, digits = 2)
      Pitch_Type$MaxVBreak_percentile <- Pitch_Type$MaxVBreak_percentile*100
      
      return(Pitch_Type)
    }
    
    Statcast_Percentiles <- get_Percentiles("Fastball")
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Changeup"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Curveball"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Slider"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Sinker"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Cutter"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Splitter"))
    Statcast_Percentiles <- rbind(Statcast_Percentiles, get_Percentiles("Sweeper"))  
    
    Max_Ranks <- Statcast_Percentiles %>%
      group_by(pitch_name) %>%
      summarize(Max_Velo_Rank = max(MaxVelo_ranking),
                Max_Spin_Rank = max(MaxSpin_ranking),
                Max_HBreak_Rank = max(MaxHBreak_ranking),
                Max_VBreak_Rank = max(MaxVBreak_ranking))
    
    Statcast_Percentiles <- Statcast_Percentiles %>%
      inner_join(Max_Ranks, by=c('pitch_name'='pitch_name'))
    
    Statcast_Percentiles <- Statcast_Percentiles %>%
      mutate(Max_Velo_Rank = paste0(MaxVelo_ranking, "/", Max_Velo_Rank),
             Max_Spin_Rank = paste0(MaxSpin_ranking, "/", Max_Spin_Rank),
             Max_HBreak_Rank = paste0(MaxHBreak_ranking, "/", Max_HBreak_Rank),
             Max_VBreak_Rank = paste0(MaxVBreak_ranking, "/", Max_VBreak_Rank)
      )
    
    TMP <- Statcast_Percentiles%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% Statcast_Percentiles$pitch_name[Statcast_Percentiles$'Usage %' >= 5]) %>%
      select(pitch_name, 'Max Velo', 'Max Spin', 'Max H Break', 'Max V Break', 'Usage %', Max_Velo_Rank, Max_Spin_Rank, Max_HBreak_Rank, Max_VBreak_Rank)
    
    datatable(TMP, caption = htmltools::tags$caption( style = 'caption-side: top; 
                                                  text-align: center; color:black; font-size:200% ; font-weight:bold;',
                                                      'Percentiles Data'), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1), `border-left` = "solid 1px") %>% formatStyle(c(7), `border-right` = "solid 1px")
    
    
  })
  
  
  
  # Velo_Chart
  
  output$Velo_Chart <- renderPlot({
    
    statcast%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% input$Pitch, c(Date >= input$Date[1] & Date <= input$Date[2])) %>%
      ggplot(statcast, mapping = aes(x=pitch_number_appearance, y= release_speed, colour = pitch_name)) +
      geom_smooth(method="lm", se=FALSE) + geom_point(alpha=0.5) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "brown",
                                    Splitter = "purple", Knuckleball = "cyan", Forkball = "pink", Sweeper = "gold")) +
      ggtitle("Velocity / Pitch") +
      xlab("Pitch") + ylab("Velocity") + theme(
        plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
      geom_hline(yintercept = seq(from=70, to=100, by = 5), alpha=0.5)
    
  })
  
  output$Spray_Chart <- renderPlot({
    
    BBE_statcast <- statcast[!(is.na(hc_x) | is.na(hc_y))]
    
    BBE_statcast <- BBE_statcast %>%
      mutate(hit_x = ((hc_x - 130)/130)*315,
             hit_y = ((225 - hc_y)/225)*450)
    
    LHB_Spray_Chart <- BBE_statcast %>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% input$Pitch, 
             c(Date >= input$Date[1] & Date <= input$Date[2]),
             stand == "L") %>%
      ggplot(BBE_statcast, mapping = aes(x = hit_x, y = hit_y)) +
      geom_point(aes(color = pitch_name),size = 3, alpha=0.5) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "brown",
                                    Splitter = "purple", Knuckleball = "cyan", Forkball = "pink", Sweeper = "gold"))+
      xlim(-295,295)+ylim(0,450)+
      geom_segment(x = 0, xend = -315, y = 0, yend = 315, size = 1.2)+
      geom_segment(x = 0, xend = 315, y = 0, yend = 315, size = 1.2) +
      geom_curve(x = -315, xend = 315, y = 315, yend = 315, curvature = -.35, size = 1.2)+
      geom_curve(x = -90, xend = 90, y =88, yend = 88, curvature = -.45, size = 1.2)+
      coord_fixed() + theme_bw()+
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 16))+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) + ggtitle("Spray Chart - LHB")
    
    RHB_Spray_Chart <- BBE_statcast %>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher,
             pitch_name %in% input$Pitch, 
             c(Date >= input$Date[1] & Date <= input$Date[2]),
             stand == "R") %>%
      ggplot(BBE_statcast, mapping = aes(x = hit_x, y = hit_y)) +
      geom_point(aes(color = pitch_name),size = 3, alpha=0.5) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "brown",
                                    Splitter = "purple", Knuckleball = "cyan", Forkball = "pink", Sweeper = "gold"))+
      xlim(-295,295)+ylim(0,450)+
      geom_segment(x = 0, xend = -315, y = 0, yend = 315, size = 1.2)+
      geom_segment(x = 0, xend = 315, y = 0, yend = 315, size = 1.2) +
      geom_curve(x = -315, xend = 315, y = 315, yend = 315, curvature = -.35, size = 1.2)+
      geom_curve(x = -90, xend = 90, y =88, yend = 88, curvature = -.45, size = 1.2)+
      coord_fixed() + theme_bw()+
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 16))+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) + ggtitle("Spray Chart - RHB")
    
    ggarrange(
      LHB_Spray_Chart, RHB_Spray_Chart, nrow = 1, ncol = 2)
  })
  
  # Heat_Map
  
  output$Heat_Map <- renderPlot({
    
    # Creates A Color Palette, Reversed (Blue/Less Frequent to Red/More Frequent) With 16 Different Shades
    heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(16)
    
    # Shows The Color Scale
    heat_colors_interpolated %>% scales::show_col()
    
    # Filter By Pitcher
    Pitcher_statcast <- statcast%>%
      filter(PitcherTeam == input$PitcherTeam,
             Pitcher == input$Pitcher)
    
    Pitcher_Usage <- Pitcher_statcast %>%
      group_by(PitcherTeam, Pitcher, pitch_name) %>%
      tally()
    Pitcher_Pitches <- Pitcher_statcast %>% 
      group_by(PitcherTeam, Pitcher) %>% 
      tally()
    
    Pitcher_Usage <- merge(Pitcher_Usage, Pitcher_Pitches, by=c('PitcherTeam', 'Pitcher'))
    
    Pitcher_Usage$'Usage %' <- round((Pitcher_Usage$n.x / Pitcher_Usage$n.y), 4)
    Pitcher_Usage$'Usage %' <- Pitcher_Usage$'Usage %' * 100
    
    Pitcher_Usage <- Pitcher_Usage[order(Pitcher_Usage$'Usage %', decreasing=TRUE),]
    
    Top_3_pitch_name <- Pitcher_Usage$pitch_name[1:min(3, length(Pitcher_Usage$'Usage %'))]
    Top_3_Usage <- Pitcher_Usage$'Usage %'[1:min(3, length(Pitcher_Usage$'Usage %'))]
    
    # Add in Parameters for Strike Zone / Plate
    Left <- -8.5/12
    Right <- 8.5/12
    Bottom <- 18.29/12
    Top <- 44.08/12
    
    # This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
    Width <- (Right - Left) / 3
    Height <- (Top - Bottom) / 3
    
    # Graph, Filtered By Pitch Type == "Fastball"
    
    # Replace the "Fastball" with input$... to Make it Reactive
    # Pitcher_statcast <- Pitcher_statcast %>% filter(pitch_name %in% Top_3_Pitches)
    
    Pitch1 <- ggplot(filter(Pitcher_statcast, pitch_name == Top_3_pitch_name[1]), mapping = aes(x=plate_x, y= plate_z)) +
      stat_density2d_filled()  +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      # The Box (Bottom, Top, Left, Right)
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      
      # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
      
      xlim(-3,3) + ylim(0, 5)  + ggtitle(paste0(Top_3_pitch_name[1]), paste0(Top_3_Usage[1], "%")) +
      theme(
        legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    if (length(Top_3_pitch_name) > 1) {
      Pitch2 <- ggplot(filter(Pitcher_statcast, pitch_name == Top_3_pitch_name[2]), mapping = aes(x=plate_x, y= plate_z)) +
        stat_density2d_filled()  +
        scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
        # The Box (Bottom, Top, Left, Right)
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        
        # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
        
        xlim(-3,3) + ylim(0, 5)  + ggtitle(paste0(Top_3_pitch_name[2]), paste0(Top_3_Usage[2], "%")) +
        theme(
          legend.position = "none",
          plot.title = element_text(color = "black", size = 15, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    }
    
    if (length(Top_3_pitch_name) > 2) {
      Pitch3 <- ggplot(filter(Pitcher_statcast, pitch_name == Top_3_pitch_name[3]), mapping = aes(x=plate_x, y= plate_z)) +
        stat_density2d_filled()  +
        scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
        # The Box (Bottom, Top, Left, Right)
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        
        # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
        
        xlim(-3,3) + ylim(0, 5)  + ggtitle(paste0(Top_3_pitch_name[3]), paste0(Top_3_Usage[3], "%")) +
        theme(
          legend.position = "none",
          plot.title = element_text(color = "black", size = 15, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    }
    
    if (length(Top_3_pitch_name) == 3) {
      ggarrange(
        Pitch1, Pitch2, Pitch3, nrow = 1, ncol = 3)
    }
    else if (length(Top_3_pitch_name) == 2) {
      ggarrange(
        Pitch1, Pitch2, nrow = 1, ncol = 2)
    }
    else {
      ggarrange(
        Pitch1, nrow = 1, ncol = 1)
    }
    
  })
  
}


# ShinyApp - Part 3 of App Structure


shinyApp(ui = ui, server = server)