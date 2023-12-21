###############################################################################
# Code to visualize Figure Supplement 21                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load the data
data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_21.csv", header = T)


# Define label
A.labs <- c("Low access to treatment", "High access to treatment")
names(A.labs) <- c("0.1", "0.4")

# define constant for plot
constant <- 2.5

# Remove data with midel access to treatment
data_2 <- data[data$Access != 0.2, ]

# Plot
PLOT_A <- 
  ggplot(data_2, aes(x = eir, y = Proportion_infection, fill = Population)) +
  geom_area() +
  facet_nested(. ~ Access, labeller = labeller(Access = A.labs)) +
  theme_bw() +
  xlim(0, 500) +
  scale_fill_manual(name = "Proportion of infections that are among:",
                    values = c("#CC6677", "#88CCEE"),
                    breaks = c("Adults", "Children"),
                    labels = c("Adults", "Children")) +
  labs(title = "", x = "EIR (inoculations per person per years)", y = "Porportion (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold"))   +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(legend.position = "top")

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_21.pdf", plot = PLOT_A, width = 15, height = 10, device = "pdf", units = "cm", dpi = 300)
