###############################################################################
# Code to visualize Figure Supplement 21                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load the data
Scenario_liste_2 <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_27.csv", header = T)

# Panel A
#########

# Define range for X and Y axis
eir <- seq(from = 5, to = 500, length = 20)
Degree_resistance <- seq(from = 0, to = 1, length = 5)

# Creat a new dataset
DATA <- merge(as.data.frame(eir), as.data.frame(Degree_resistance))
DATA$Probability_extinction <- 100

# Loop to estimate the probability of extinction in the defined range of X and Y values
for (i in 1:(length(eir) - 1)) {
  for (j in 1:(length(Degree_resistance) - 1)) {
    DATA$Probability_extinction[DATA$eir == eir[i] & DATA$Degree_resistance == Degree_resistance[j]] <-
      mean(Scenario_liste_2$extinct[Scenario_liste_2$eir >= eir[i] & 
                                    Scenario_liste_2$eir <= eir[i + 1] &
                                    Scenario_liste_2$Degree_resistance >= Degree_resistance[j] &
                                    Scenario_liste_2$Degree_resistance <= Degree_resistance[j + 1]])
  }
}

# Define the last range
DATA <- DATA[DATA$eir != max(DATA$eir), ]
DATA <- DATA[DATA$Degree_resistance != max(DATA$Degree_resistance), ]

# Define constant
constant <- 2.5

# Plot
PLOT_1 <-
  ggplot(DATA, aes(x = eir, 
                   y = Degree_resistance, 
                   fill = Probability_extinction)) +
  geom_tile() +
  ylab("Degree of resistance") +
  xlab("EIR (inoculations \nper person per year)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, 
                                  hjust = 0.5, 
                                  face = "bold")) +
  ggtitle(label = "") +
  theme(legend.text = element_text(size = 16 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  scale_fill_gradient(name = "Probability of extinction:", 
                      low = "#132B43", 
                      high = "#56B1F7", 
                      limits = c(0, 0.18)) +
  theme(legend.position = "none")


# Panel B
###########

# Define range for X and Y axis
x <- seq(from = 0.5, to = 1, length = 10)
y <- seq(from = 0.5, to = 5, length = 10)

# Creat a new dataset
DATA <- merge(as.data.frame(x), as.data.frame(y))
DATA$Probability_extinction <- 100

# Loop to estimate the probability of extinction in the defined range of X and Y values
for (i in 1:(length(x) - 1)) {
  for (j in 1:(length(y) - 1)) {
    DATA$Probability_extinction[DATA$x == x[i] & DATA$y == y[j]] <-
      mean(Scenario_liste_2$extinct[Scenario_liste_2$initialEfficacy >= x[i] &
                                      Scenario_liste_2$initialEfficacy <= x[i + 1] &
                                      Scenario_liste_2$half_life >= y[j] &
                                      Scenario_liste_2$half_life <= y[j + 1]])
  }
}

# Define the last range
DATA <- DATA[DATA$x != max(DATA$x), ]
DATA <- DATA[DATA$y != max(DATA$y), ]

# Plot
PLOT_2 <-
  ggplot(DATA,  aes(x = x, 
                    y = y, 
                    fill = Probability_extinction)) +
  geom_tile() +
  ylab("Vaccine half-life (years)") +
  xlab("Initial vaccine efficacy (%)\n") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  ggtitle(label = "") +
  theme(legend.text = element_text(size = 16 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  scale_fill_gradient(name = "Probability of extinction:",
                      low = "#132B43",
                      high = "#56B1F7",
                      limits = c(0, 0.18)) +
  theme(legend.position = "none")


# Panel C
#########

# Define range for X and Y axis
x <- seq(from = 0.6, to = 1, length = 10)
y <- seq(from = 0.04, to = 0.5, length = 10)


# Creat a new dataset
DATA <- merge(as.data.frame(x), as.data.frame(y))
DATA$Probability_extinction <- 100

# Loop to estimate the probability of extinction in the defined range of X and Y values
for (i in 1:(length(x) - 1)) {
  for (j in 1:(length(y) - 1)) {
    DATA$Probability_extinction[DATA$x == x[i] & DATA$y == y[j]] <-
      mean(Scenario_liste_2$extinct[Scenario_liste_2$Coverage >= x[i] &
                                      Scenario_liste_2$Coverage <= x[i + 1] &
                                      Scenario_liste_2$Access >= y[j] &
                                      Scenario_liste_2$Access <= y[j + 1]])
  }
}

# Define the last range
DATA <- DATA[DATA$x != max(DATA$x), ]
DATA <- DATA[DATA$y != max(DATA$y), ]

# Plot
PLOT_3 <-
  ggplot(DATA, aes(x = x, 
                   y = y, 
                   fill = Probability_extinction)) +
  geom_tile() +
  ylab("Access to treatment (%)") +
  xlab("Vaccine coverage (%)\n") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold")) +
  ggtitle(label = "") +
  theme(legend.text = element_text(size = 16 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  scale_fill_gradient(name = "Probability of extinction:",
                      low = "#132B43",
                      high = "#56B1F7",
                      limits = c(0, 0.18)) +
  theme(legend.position = "none")


# Get the legend
LEG <- 
  ggplot(DATA, aes(x = x, 
                   y = y, 
                   fill = Probability_extinction * 100)) +
  geom_tile() +
  ylab("Access to treatment") +
  xlab("Coverage") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5, 
                                  face = "bold")) +
  ggtitle(label = "") +
  theme(legend.text = element_text(size = 16 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  scale_fill_gradient(name = "Probability of extinction (%):",
                      low = "#132B43",
                      high = "#56B1F7",
                      limits = c(0, 18))

# Legend
LEG <- get_legend(LEG)

# Merge all panel
PLOT_ALL <- plot_grid(PLOT_1, PLOT_2, PLOT_3, LEG, ncol = 4, nrow = 1, labels = c("A", "B", "C", ""), label_size = 18 / constant, label_fontface = 2, scale = 0.99, rel_heights  = c(7, 7, 7, 1))

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_27.pdf", plot = PLOT_ALL, width = 17, height = 5, device = "pdf", units = "cm", dpi = 300)
