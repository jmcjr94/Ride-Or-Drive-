require(DT)
require(ggplot2)
require(ggthemes)
#require(hrbrthemes)
require(leaflet)
require(markdown)
require(raster)
require(RColorBrewer)
require(scales)
require(shiny)
require(shinythemes)
#require(showtext)
require(tidyverse)


# font_add_google(name = "Roboto Condensed", family = "Roboto Condensed",
#                 regular.wt = 400, bold.wt = 700)
# hrbrthemes::import_roboto_condensed()
options("scipen" = 100, "digits" = 4)
options(encoding = "UTF-8")
# system('fc-cache -f ~/.fonts/Roboto_Condensed') 

BarChart <- function(data, month) {
  data_to_plot <- filter(data, month_names == month)
  n_to_plot <- data_to_plot$UberRides
  
  p <- ggplot(data_to_plot) +
    geom_bar(mapping = aes(x = as.Date(day), y = n_to_plot* 30 / 10000, fill = as.factor(rain)), stat = "identity") +
    geom_line(mapping = aes(x = as.Date(day),
                            y = as.double(Temp_Avg),
                            colour = "Temperature", group = 1)) +
    scale_color_manual(name = "", values=c("Temperature" = "Black")) +
    scale_x_date(name = "Date") +
    theme_few() +
#    theme_ipsum_rc() +
    scale_y_continuous(expression("Average temperature ("~degree~"F)"),
                       sec.axis = sec_axis(~.* 10000 / 30, name = "Uber rides on that day", 
                                           labels = comma)) +
        guides(fill = guide_legend(title = "Rain?"), colour =  guide_legend(title = "")) +
    geom_text(aes(x = as.Date(day), y = 10, label = weekDay, angle = 90),
              position = position_stack(vjust = 0.55), family =  "Roboto Condensed") +
    labs(title = "Riders On The Storm", 
         subtitle = "The more it rains, the more New Yorkers take Ubers") +
    scale_fill_manual(values = c("#fdae61", "#3288bd"))
  
p <-  p + theme(
    plot.title = element_text(size = 26),
    plot.subtitle =  element_text(size = 20),
    axis.title.y = element_text(size = 14),
    axis.title.y.right = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_blank()
  )
  return(p)
}

lmFromData <- function(data, dependent_variable) {
  ols <- lm(dependent_variable ~ weekend + rain + Precipitation + Temp_Avg, data)
  ols_summary <- summary(ols)
  ols_summary <- ols_summary$coefficients
  ols_summary <- as.tibble(rownames_to_column(as.data.frame(ols_summary)))
  ols_summary$`Pr(>|t|)` <- substr(ols_summary$`Pr(>|t|)`, 1, 5)
  ols_summary[,2:4] <- round(ols_summary[,2:4], 2)
  ols_summary <- rename(ols_summary, Variable = rowname, "T-value" = `t value`) 
  ols_summary$Variable <- c("(Intercept)", "Weekend - Yes", 
                            "Rain - Yes", "Precipitation", "Average temp.")
  return(ols_summary)
}

plotWeekDayUber <- function(data) {
  p <-  ggplot(data, aes(x = hour, y = avg)) +
    geom_bar(stat = "identity", fill = "#b2182b") +
    facet_wrap(~ day_f, scales = "free_x", nrow = 2) +
    scale_x_discrete(labels = c("01" = "1-5am", 
                                "05" = "5-9am",
                                "09" = "9-1pm",
                                "13" = "1-5pm",
                                "17" = "5-9pm",
                                "21" = "9-1am")) +
    scale_y_continuous(labels = comma) +
    theme_few() +
#   theme_ipsum_rc() +
    labs(title = "NYC Uber Rides",
         subtitle = "April 2014 - September 2014",
         y = "Average Number of Rides")
  
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.6, size = 10),
                 plot.title = element_text(size = 26),
                 axis.title.x = element_blank(),
                 strip.text = element_text(size = 16),
                 axis.title.y = element_text(size = 14),
                 axis.text = element_text(size = 12)
                 )
  return(p)
}

plotWeekDayMTA <- function(data) {
  p <- ggplot(data, aes(x = hour, y = avg)) +
    geom_bar(stat = "identity", fill = "#2166ac") +
    facet_wrap(~ day_f, scales = "free_x", nrow = 2) +
    scale_x_discrete(labels = c("01" = "1-5am", 
                                "05" = "5-9am",
                                "09" = "9-1pm",
                                "13" = "1-5pm",
                                "17" = "5-9pm",
                                "21" = "9-1am")) +
    scale_y_continuous(labels = comma) +
    theme_few() +
    #   theme_ipsum_rc() +
    labs(title = "NYC-MTA Entrance Swipes",
         subtitle = "April 2014 - September 2014",
         y = "Average Number of Rides")
  
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.6, size = 10),
                 plot.title = element_text(size = 26),
                 axis.title.x = element_blank(),
                 strip.text = element_text(size = 16),
                 axis.title.y = element_text(size = 14),
                 axis.text = element_text(size = 12)
  )
  return(p)
}

plotWeekDayCompare <- function(data) {
  p <- ggplot(data, aes(x = hour, y = avg, fill = data)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ day_f, scales = "free_x", nrow = 2) +
    scale_x_discrete(labels = c("01" = "1-5am", 
                                "05" = "5-9am",
                                "09" = "9-1pm",
                                "13" = "1-5pm",
                                "17" = "5-9pm",
                                "21" = "9-1am")) +
    scale_y_continuous(labels = comma) +
    theme_few() +
    #   theme_ipsum_rc() +
    labs(title = "NYC Subway vs. Uber Trips",
         subtitle = "April 2014 - September 2014",
         y = "Average Number of Rides") +
    scale_fill_manual(values = c("#2166ac", "#b2182b"))
  
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.6, size = 10),
                 plot.title = element_text(size = 26),
                 axis.title.x = element_blank(),
                 strip.text = element_text(size = 16),
                 axis.title.y = element_text(size = 14),
                 axis.text = element_text(size = 12),
                 legend.title = element_blank()
  )
  return(p)
}

lmIncomeRent <- function(data, dependent_variable) {
  ols <- lm(dependent_variable ~ inc2015 + rnt2015 + twts_nr, data)
  ols_summary <- summary(ols)
  ols_summary <- ols_summary$coefficients
  ols_summary <- as.tibble(rownames_to_column(as.data.frame(ols_summary)))
  ols_summary$`Pr(>|t|)` <- substr(ols_summary$`Pr(>|t|)`, 1, 5)
  ols_summary[,2:4] <- round(ols_summary[,2:4], 2)
  ols_summary <- rename(ols_summary, Variable = rowname, "T-value" = `t value`) 
  ols_summary$Variable <- c("(Intercept)", "Median income (2015)", 
                            "Median rent (2015)", "Tweets (normalised)")
  return(ols_summary)
}
