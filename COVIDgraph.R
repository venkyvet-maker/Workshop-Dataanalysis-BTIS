
#only with frequency curve:
library(dplyr)
library(tidyr)
library(ggplot2)

x<-read.csv("overall wave r data.csv")
x$months <- factor(x$months, levels = unique(x$months))
p<-ggplot(x, aes(x = months, y = total, color = Legend, group = Legend)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("red", "green", "black", "purple")) +
  labs(title = "Overall COVID-19 ", 
       x = "Months (Mar-2020 to Aug-2024) ",
       y = "Population (numbers)") +
  theme_classic() +
  theme(plot.title = element_text(color = "blue", size = 20, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#symbols with curve:
library(dplyr)
library(tidyr)
library(ggplot2)
x<-read.csv("overall wave r data.csv")
x$months <- factor(x$months, levels = unique(x$months))
P<-ggplot(x, aes(x = months, y = total, color = Legend, group = Legend)) +
  geom_point(aes(shape = Legend), size = 3) +
  geom_line(aes(linetype = Legend)) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid", "dotdash")) +
  scale_colour_manual(values = c("red", "green", "black", "purple")) +
  labs(title = "Overall COVID-19 ", 
       x = "Months (Mar-2020 to Aug-2024) ",
       y = "Population (numbers)") +
  theme_classic() +
  theme(plot.title = element_text(color = "blue", size = 20, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P


SCRIPTS for graph with 4 waves:
  library(ggplot2)
library(lubridate)
#setwd("D:/SECOND SEMESTER/BST 673 - STATISTICAL COMPUTING/COVID PROJECT/COVID WAVES")
x <- read.csv("overall wave r data.csv")
x$months <- dmy(paste("01", x$months, sep = "-"))
p <- ggplot(x, aes(x = months, y = total, color = Legend, group = Legend)) +
  geom_point(aes(shape = Legend), size = 3) +
  geom_line(aes(linetype = Legend)) +
  annotate("rect", xmin = as.Date("2020-05-31"), xmax = as.Date("2021-02-20"),
           ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.4) + 
  annotate("rect", xmin = as.Date("2021-03-19"), xmax = as.Date("2021-12-24"),
           ymin = -Inf, ymax = Inf, fill = "lightpink", alpha = 0.4) + 
  annotate("rect", xmin = as.Date("2021-12-31"), xmax = as.Date("2022-04-04"),
           ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2022-06-23"), xmax = as.Date("2022-11-29"),
           ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.4) +
  annotate("text", x = as.Date("2020-11-01"), y = max(x$total), label = "1st Wave", color  = "black",hjust = 0.6,angle = 0,vjust = -0.8) + 
  annotate("text", x = as.Date("2021-08-01"), y = max(x$total), label = "2nd Wave", color = "black",hjust = 0.5,angle = 0, vjust = -0.8) +
  annotate("text", x = as.Date("2022-02-01"), y = max(x$total), label = "3rd Wave", color = "black",hjust = 0.4,angle = 0, vjust = -0.7) +
  annotate("text", x = as.Date("2022-10-01"), y = max(x$total), label = "4th Wave", color  = "black",hjust = 0.6,angle = 0,vjust = -0.8) + 
  scale_linetype_manual(values = c("dotted", "dashed", "solid", "dotdash")) +
  scale_colour_manual(values = c("red", "green", "black", "purple")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Overall COVID-19", 
       x = "Months (Mar-2020 to Aug-2024)", 
       y = "Population (numbers)") +
  theme_classic() +
  theme(plot.title = element_text(color = "blue", size = 20, face = "bold", hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#script for graph marked with critical days:
  library(ggplot2)
library(lubridate)
#setwd("D:/SECOND SEMESTER/BST 673 - STATISTICAL COMPUTING/COVID PROJECT/COVID WAVES")
x <- read.csv("overall wave r data.csv")
x$months <- dmy(paste("01", x$months, sep = "-"))
p <- ggplot(x, aes(x = months, y = total, color = Legend, group = Legend)) +
  geom_point(aes(shape = Legend), size = 3) +
  geom_line(aes(linetype = Legend)) +
  annotate("rect", xmin = as.Date("2020-05-31"), xmax = as.Date("2021-02-20"),
           ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.4) + 
  annotate("rect", xmin = as.Date("2021-03-19"), xmax = as.Date("2021-12-24"),
           ymin = -Inf, ymax = Inf, fill = "lightpink", alpha = 0.4) + 
  annotate("rect", xmin = as.Date("2021-12-31"), xmax = as.Date("2022-04-04"),
           ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2022-06-23"), xmax = as.Date("2022-11-29"),
           ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-05-31"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-07-27"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2021-02-20"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2021-03-19"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2021-05-21"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2021-12-24"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2021-12-31"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-01-22"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-04-04"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-06-23"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-07-07"), linetype = "longdash", color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-11-29"), linetype = "longdash", color = "blue", linewidth = 1) +
  annotate("text", x = as.Date("2020-05-31"), y = max(x$total), label = "above 1000", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2020-07-27"), y = max(x$total), label = "peak", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2021-02-20"), y = max(x$total), label = "lowest", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2021-03-19"), y = max(x$total), label = "above 1000", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2021-05-21"), y = max(x$total), label = "peak", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2021-12-24"), y = max(x$total), label = "lowest", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2021-12-31"), y = max(x$total), label = "above 1000", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2022-01-22"), y = max(x$total), label = "peak", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2022-04-04"), y = max(x$total), label = "lowest", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2022-06-23"), y = max(x$total), label = "above 1000", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2022-07-07"), y = max(x$total), label = "peak", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2022-11-29"), y = max(x$total), label = "lowest", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date("2020-11-01"), y = max(x$total), label = "1st Wave", color  = "black", hjust = 0.6, angle = 0, vjust = -0.8) + 
  annotate("text", x = as.Date("2021-08-01"), y = max(x$total), label = "2nd Wave", color = "black", hjust = 0.5, angle = 0, vjust = -0.8) +
  annotate("text", x = as.Date("2022-02-01"), y = max(x$total), label = "3rd Wave", color = "black", hjust = 0.4, angle = 0, vjust = -0.7) +
  annotate("text", x = as.Date("2022-10-01"), y = max(x$total), label = "4th Wave", color  = "black", hjust = 0.6, angle = 0, vjust = -0.8) + 
  scale_linetype_manual(values = c("dotted", "dashed", "solid", "dotdash")) +
  scale_colour_manual(values = c("red", "green", "black", "purple")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Overall COVID-19", 
       x = "Months (Mar-2020 to Aug-2024)", 
       y = "Population (numbers)") +
  theme_classic() +
  theme(plot.title = element_text(color = "blue", size = 20, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
