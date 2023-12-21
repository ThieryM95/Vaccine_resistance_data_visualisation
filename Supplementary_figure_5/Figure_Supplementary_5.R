###############################################################################
# Code to visualize Figure Supplement 5                                       #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Panel 1
##########

# Load the data
Output_data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_5A.csv", header = T)

# Define time of deployment
TIME_1 <- Output_data$Survey[Output_data$nEPIVaccinations_3 >= 1][1] / 73
TIME_2 <- Output_data$Survey[Output_data$nEPIVaccinations_5 >= 1][1] / 73

# Plot
PLOT_AS1 <- 
  ggplot(Output_data) +
  geom_line(aes(x = Survey / 73, y = Incidence), size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "Number of clinical cases\n per 1000 children under 5 years",
                     lim = c(0, 30),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years)",
                     lim = c(28, 37),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = TIME_1,
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = TIME_1 - 1,
             linetype = "dashed",
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = TIME_1 - 1,
                   y = 20,
                   xend = TIME_1, 
                   yend = 20),
    color = "#2C85B2",
    size = 3 / ggplot2::.pt / constant,
    arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = TIME_1,
                   y = 20,
                   xend = TIME_1 - 1,
                   yend = 20),
               color = "#2C85B2",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_vline(xintercept = TIME_2,
             color = "#FF50FF",
             size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = TIME_2 + 1,
             linetype = "dashed",
             color = "#FF50FF",
             size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = TIME_2 + 1,
                   y = 20,
                   xend = TIME_2,
                   yend = 20),
               color = "#FF50FF",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = TIME_2,
                   y = 20,
                   xend = TIME_2 + 1,
                   yend = 20),
               color = "#FF50FF",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm")))

# Panel 2
##########
Output_data_2 <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_5B.csv", header = T)

# Define time deployment
TIME_3 <- Output_data_2$Survey[Output_data_2$nEPIVaccinations_2 >= 1][1] / 73
TIME_4 <- Output_data_2$Survey[Output_data_2$nMassVaccinations_4 >= 1] / 73

PLOT_AS2 <- 
  ggplot(Output_data_2) +
  geom_line(aes(x = Survey / 73, y = Incidence), size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "Number of clinical cases\n per 1000 children under 5 years",
                     lim = c(0, 50),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years)",
                     lim = c(28, 37),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = TIME_1,
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = TIME_1 - 1,
             linetype = "dashed",
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = TIME_3 - 1,
                   y = 40,
                   xend = TIME_3,
                   yend = 40),
               color = "#2C85B2",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = TIME_3,
                   y = 40,
                   xend = TIME_3 - 1,
                   yend = 40),
               color = "#2C85B2",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_vline(xintercept = TIME_4,
             color = "#FF50FF",
             size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = TIME_4[4] + 1,
                   y = 40,
                   xend = TIME_4[4],
                   yend = 40),
               color = "#FF50FF",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = TIME_4[4], 
                   y = 40,
                   xend = TIME_4[4] + 1,
                   yend = 40),
               color = "#FF50FF",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))

PLOT5 <- plot_grid(PLOT_AS1, PLOT_AS2, ncol = 1, nrow = 2, labels = c("A", "B"), label_size = 18 / constant, label_fontface = 2, scale = 0.99)



ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_5.pdf", plot = PLOT5, width = 12, height = 12, device = "pdf", units = "cm", dpi = 300)
