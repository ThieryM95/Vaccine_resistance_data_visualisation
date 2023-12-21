###############################################################################
# Code to visualize Figure Supplement 6                                       #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# load the data
Output_data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_6.csv", header = T)

# Define time of MDA
TIME_1 <- Output_data$Survey[Output_data$nMassVaccinations_6 >= 1] / 73

# Plot
PLOT_A <- 
  ggplot(Output_data) +
  geom_line(aes(x = Survey / 73, y = Incidence), size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "Number of clinical cases\n per 1000 individuals",
                     lim = c(0, 25),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years)",
                     lim = c(25, 37),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = TIME_1[1:2] - 2,
             linetype = "dashed",
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = TIME_1,
             linetype = "dashed", 
             color = "#FF50FF", 
             size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = TIME_1[1],
                   y = 20, xend = TIME_1[2],
                   yend = 20),
               color = "#FF50FF",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = TIME_1[2],
                   y = 20,
                   xend = TIME_1[1],
                   yend = 20),
               color = "#FF50FF",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes( x = TIME_1[1] - 2,
                    y = 20,
                    xend = TIME_1[2] - 2,
                    yend = 20),
               color = "#2C85B2",
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = TIME_1[2] - 2,
                   y = 20,
                   xend = TIME_1[1] - 2, 
                   yend = 20),
               color = "#2C85B2", 
               size = 3 / ggplot2::.pt / constant,
               arrow = arrow(length = unit(0.5, "cm")))

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_6.pdf", plot = PLOT_A, width = 12, height = 6, device = "pdf", units = "cm", dpi = 300)
