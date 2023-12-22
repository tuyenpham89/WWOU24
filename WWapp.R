
library(shiny)
library(zoo)
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(limma)      # lmFit, etc -- fitting many models
library(ggpubr)
library(forecast)
library(ggplot2)
library(AER)
library(dynlm)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(cardidates)
library(ggpmisc)
library(shinythemes)
library(openxlsx)
library(shinyBS)
library(shinyWidgets)
library(openxlsx)
library(writexl)
library(readxl)
library(data.table)
library(shinydashboard)

#Import data
read.csv(
  "C:\\OHIO UNIVERSITY PROJECTS\\Covid_Waste Water\\R_Code 2023_2024_Report\\WW20222023.csv"
) %>%
  select(
    sample_collect_date, sample_collect_time,
    sample_id, pcr_target_avg_conc
  ) -> WW20222023

read.csv(
  "C:\\OHIO UNIVERSITY PROJECTS\\Covid_Waste Water\\R_Code 2023_2024_Report\\WW20232024.csv"
) %>%
  select(
    sample_collect_date, sample_collect_time,
    sample_id, pcr_target_avg_conc
  ) -> WW20232024

cov19 <- rbind(WW20222023,WW20232024)

#Filter for dorm sites
cov19 %>%
  filter(
    grepl("TP", sample_id)
  ) %>%
  separate(
    sample_id ,
    into = c("tp", "waste"),
    sep = "_"
  ) %>%
  mutate(mydate = as.Date(sample_collect_date)) -> covdf 

#get rid of TP16
covdf <- covdf[covdf$tp != "TP16",]

#Get all dorm average
Covid_dorm_avg <- aggregate(pcr_target_avg_conc ~ mydate, covdf, mean)
Covid_dorm_avg$yvar <- log10(Covid_dorm_avg$pcr_target_avg_conc)

#Get 8 most current data points for all individual dorm
covdf %>%
  group_by(tp) %>%
  arrange(
    desc(mydate)
  ) %>%
  slice(1:8) -> mydf

#Get the number of days after first day of collection (tvar), and log 10 of ww concentration (yvar)
mydf %>%
  group_by(tp) %>%
  mutate(
    mindate = min(sample_collect_date),
    tvar = as.numeric(
      difftime(
        sample_collect_date,
        mindate,
        units = "days"
      )
    ),
    yvar = log10(as.numeric(pcr_target_avg_conc))
  ) -> mydf

#Get rid of 0 observations
mydf$yvar[mydf$yvar==-Inf] <- NA


#Get 8 most current data points for dorm AVERAGE
Covid_dorm_avg %>%
  arrange(
    desc(mydate)
  ) %>%
  slice(1:8) -> Covid_dorm_avg_8p

#Get the number of days after first day of collection (tvar), and log 10 of ww concentrtion (yvar)
Covid_dorm_avg_8p %>%
  mutate(
    mindate = min(mydate),
    tvar = as.numeric(
      difftime(
        mydate,
        mindate,
        units = "days"
      )
    ),
    yvar = log10(as.numeric(pcr_target_avg_conc))
  ) -> Covid_dorm_avg_8p
###DONE WITH DATA PREP###



#########################################################
ui <- fluidPage(
  sidebarLayout(position="right",
                sidebarPanel(
                  dateInput("single_date", "Select a single date to view all data and trend after the selected date", value=mean(Covid_dorm_avg$mydate), min=min(Covid_dorm_avg$mydate), max=max(Covid_dorm_avg$mydate), format="mm/dd/yyyy"),
                  dateRangeInput("range_date", "Select a range of dates to view data and trend between the range", start=min(Covid_dorm_avg$mydate), end=max(Covid_dorm_avg$mydate), min=min(Covid_dorm_avg$mydate), max=max(Covid_dorm_avg$mydate),format="mm/dd/yyyy")
                ),
                mainPanel(plotOutput("plot1"),plotOutput("plot2"),plotOutput("plot3"),plotOutput("plot4")))
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$plot1 <- renderPlot({
    date = as.Date(input$single_date,"%m/%d/%Y")
    Covid_dorm_avg_new1 <- Covid_dorm_avg[Covid_dorm_avg$mydate>=date,]
    Covid_dorm_avg_new1 %>%  
      ggplot(
        aes(
          x = mydate,
          y = log10(as.numeric(pcr_target_avg_conc))
        )
      ) +
      geom_smooth(linewidth=0.5,method = "loess", formula = y ~ x) +
      geom_point() +
      scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%d/%y") +
      labs(
        x = "Collection Date",
        y = "log10 Average Concentration"
      )+
      ggtitle(paste("Average of all campus sites after", as.character(date,"%m/%d/%Y")))
  })
  
  output$plot2 <- renderPlot({
    date = as.Date(input$single_date,"%m/%d/%Y")
    Covid_dorm_avg_new4 <- Covid_dorm_avg[Covid_dorm_avg$mydate>=date,]
    Covid_dorm_avg_new4 %>%  
      ggplot(
        aes(
          x = mydate,
          y = log10(as.numeric(pcr_target_avg_conc))
        )
      ) +
      geom_smooth(linewidth=0.5,method = "lm", formula = y ~ x) +
      geom_point() +
      scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%d/%y") +
      labs(
        x = "Collection Date",
        y = "log10 Average Concentration"
      )+
      stat_poly_eq(aes(label = paste(..rr.label..)), 
                   label.x.npc = "left", label.y.npc = 0.15,
                   formula = y~x, parse = TRUE, size = 4)+
      stat_fit_glance(method = 'lm',
                      method.args = list(formula = y~x),
                      geom = 'text',
                      aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                      label.x.npc = 'left', label.y.npc = 0.35, size = 4)+
      ggtitle(paste("The trend of all campus sites' average after", as.character(date,"%m/%d/%Y")))
  })
  
  output$plot3 <- renderPlot({
    start_date = as.Date(input$range_date[1],"%m/%d/%Y")
    end_date = as.Date(input$range_date[2],"%m/%d/%Y")
    Covid_dorm_avg_new2 <- Covid_dorm_avg[Covid_dorm_avg$mydate>=start_date & Covid_dorm_avg$mydate<=end_date,]
    Covid_dorm_avg_new2 %>%  
      ggplot(
        aes(
          x = mydate,
          y = log10(as.numeric(pcr_target_avg_conc))
        )
      ) +
      geom_smooth(linewidth=0.5,method = "loess", formula = y ~ x) +
      geom_point() +
      scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%d/%y") +
      labs(
        x = "Collection Date",
        y = "log10 Average Concentration"
      )+
      ggtitle(paste("Average of all campus sites between", as.character(start_date,"%m/%d/%Y"), "and", as.character(end_date,"%m/%d/%Y")))
  })
  
  output$plot4 <- renderPlot({
    start_date = as.Date(input$range_date[1],"%m/%d/%Y")
    end_date = as.Date(input$range_date[2],"%m/%d/%Y")
    Covid_dorm_avg_new3 <- Covid_dorm_avg[Covid_dorm_avg$mydate>=start_date & Covid_dorm_avg$mydate<=end_date,]
    Covid_dorm_avg_new3 %>%  
      ggplot(
        aes(
          x = mydate,
          y = log10(as.numeric(pcr_target_avg_conc))
        )
      ) +
      geom_smooth(linewidth=0.5,method = "lm", formula = y ~ x) +
      geom_point() +
      scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%d/%y") +
      labs(
        x = "Collection Date",
        y = "log10 Average Concentration"
      )+
      stat_poly_eq(aes(label = paste(..rr.label..)), 
                   label.x.npc = "left", label.y.npc = 0.15,
                   formula = y~x, parse = TRUE, size = 4)+
      stat_fit_glance(method = 'lm',
                      method.args = list(formula = y~x),
                      geom = 'text',
                      aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                      label.x.npc = 'left', label.y.npc = 0.35, size = 4)+
      ggtitle(paste("The trend of all campus sites' average between", as.character(start_date,"%m/%d/%Y"), "and", as.character(end_date,"%m/%d/%Y")))
  })
}

shinyApp(ui = ui, server = server)
