###############################################################################
# Code to visualsise Figure Supplement 2                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# Pannel A
##########

# Load the data
Output_data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_2A.csv", header = T)

# Define time of vaccination
TIME_1 <- Output_data$Survey[Output_data$nMassVaccinations_6 >= 1] / 73

PLOT_AS1 <- 
  ggplot(Output_data) +
  geom_line(aes(x = Survey / 73 - 30, y = simulatedEIR),
            size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "EIR \n(inoculations per person per month)",
                     lim = c(0, 6),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years)",
                     lim = c(-1, 7),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = TIME_1 - 30,
             linetype = "dashed",
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  annotate("rect", xmin = TIME_1[1] - 30, xmax = 7, ymin = -Inf, ymax = +Inf, alpha = .5, fill = "grey")


# Pannel B
##########

# Load the data
Output_data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_2B.csv", header = T)

# Define time of vaccination
TIME_1 <- Output_data$Survey[Output_data$nMassVaccinations_6 >= 1] / 73

PLOT_AS2 <- 
  ggplot(Output_data) +
  geom_line(aes(x = Survey / 73 - 30, y = simulatedEIR),
            size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "EIR \n(inoculations per person per month)",
                     lim = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years)",
                     lim = c(-1, 7),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = TIME_1 - 30,
             linetype = "dashed",
             color = "#2C85B2",
             size = 3 / ggplot2::.pt / constant) +
  annotate("rect", xmin = TIME_1[1] - 30, xmax = 7, ymin = -Inf, ymax = +Inf, alpha = .5, fill = "grey")


# Merge the two panel
PLOT_S2 <- plot_grid(PLOT_AS1, PLOT_AS2, ncol = 1, nrow = 2, labels = c("A", "B"), label_size = 18 / constant, label_fontface = 2, scale = 0.99)

# Save the figure
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_2.pdf", plot = PLOT_S2, width = 12, height = 12, device = "pdf", units = "cm", dpi = 300)
