###############################################################################
# Code to visualize Figure Supplement 1A                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# load package
library("rlang")
library("ggh4x")
library("ggplot2")
library('plyr')

# Define palletter

pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")

# Panel A
##########

# Load the data
Output_data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_1A.csv", header = T)

# Define time of vaccination
TIME_1 <- Output_data$Survey[Output_data$nEPIVaccinations_3 >= 1][1] / 73
TIME_2 <- Output_data$Survey[Output_data$nEPIVaccinations_5 >= 1][1] / 73

# Plot
constant <- 2.5

PLOT_AS1 <- 
  ggplot(Output_data) +
  geom_line(aes(x = Survey / 73 - 30, y = simulatedEIR), 
            size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "EIR \n(inoculations per person per month)",
                     lim = c(0, 4),
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
  geom_vline(xintercept = TIME_2 - 30,
             linetype = "dashed",
             color = "#FF50FF",
             size = 3 / ggplot2::.pt / constant) +
  annotate("rect",
            xmin = TIME_2 - 30,
            xmax = 7,
            ymin = -Inf,
            ymax = +Inf,
            alpha = .5,
            fill = "grey")

# Pannel B
##########

# Load the figure
Output_data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_1B.csv", header = T)

# Define the time
TIME_1 <- Output_data$Survey[Output_data$nEPIVaccinations_2 >= 1][1] / 73
TIME_2 <- Output_data$Survey[Output_data$nMassVaccinations_4 >= 1] / 73
TIME_3 <- Output_data$Survey[Output_data$nMassVaccinations_7 >= 1][1] / 73

# Plot the result
PLOT_AS2 <- 
  ggplot(Output_data) +
  geom_line(aes(x = Survey / 73 - 30, y = simulatedEIR),
            size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "EIR \n(inoculations per person per month)",
                     lim = c(0, 15),
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
  geom_vline(xintercept = TIME_2 - 30,
             linetype = "dashed",
             color = "#FF50FF",
             size = 3 / ggplot2::.pt / constant) +
  annotate("rect",
           xmin = TIME_3 - 30,
           xmax = 7,
           ymin = -Inf,
           ymax = +Inf,
           alpha = .5,
           fill = "grey")


# Merge the two panel
PLOT_S1 <- plot_grid(PLOT_AS1, PLOT_AS2, ncol = 1, nrow = 2, labels = c("A", "B"), label_size = 18 / constant, label_fontface = 2, scale = 0.99)

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_1.pdf", plot = PLOT_S1, width = 12, height = 12, device = "pdf", units = "cm",  dpi = 300)