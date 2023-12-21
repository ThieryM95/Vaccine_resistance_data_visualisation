###############################################################################
# Code to visualize Figure Supplement 26A                                     #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load the data
Prediction_table <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_26A.csv", header = T)

# Defin label
S.labs <- c("Seasonal deployment", "Perennial deployment")
names(S.labs) <- c("Seasonal", "Perennial")

B.labs <- c("With drug", "Without drug")
names(B.labs) <- c(0, 1)

Ac.labs <- c("Low treatment acess (25%)", "High treatment access (60%)")
names(A.labs) <- c(0.1, 0.3)

H.labs <- c("Half-life = 1 year", "Half-life = 1.5 years")
names(H.labs) <- c(1, 1.5)

V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("AIV", "BSV")

A.labs <- c("Children", "Whole population", "Adults")
names(A.labs) <- c("Children", "0.75", "18")


# Select the data
Prediction_table_2 <- Prediction_table[Prediction_table$half_life == 1, ]
Prediction_table_2 <- Prediction_table_2[Prediction_table_2$eir == 5, ]
Prediction_table_2 <- Prediction_table_2[Prediction_table_2$Access == 0.1, ]
Prediction_table_2$Seasonality[Prediction_table_2$Seasonality == "Perenial"] <- "Perennial"

# Define the level
Prediction_table_2$Seasonality <- as.factor(Prediction_table_2$Seasonality)
Prediction_table_2$Age <- factor(Prediction_table_2$Age, levels = c("Children", "18", "0.75"))
Prediction_table_2$Blood_clerance <- as.factor(Prediction_table_2$Blood_clerance)
Prediction_table_2$eir <- as.factor(Prediction_table_2$eir)
Prediction_table_2$Vaccine_type <- factor(Prediction_table_2$Vaccine_type, levels = c("AIV", "BSV"))

# Plot
constant <- 2.5
pd <- position_dodge(0.15)

PLOT_26A <- 
  ggplot(data = Prediction_table_2) +
  geom_errorbar(aes(x = Degree_resistance,
                    ymin = time_UP,
                    ymax = time_LOW,
                    color = Seasonality,
                    shape = Blood_clerance),
                width = 1 / constant,
                position = pd) +
  geom_point(aes(x = Degree_resistance,
                 y = time_0.1,
                 color = Seasonality,
                 shape = Blood_clerance),
             size = 5 / constant, 
             stroke = 2.5 / constant, 
             position = pd) +
  facet_nested(. ~ Age + Vaccine_type, labeller = labeller(Vaccine_type = V.labs, Age = A.labs)) +
  scale_color_manual(name = "Deployment:",
                     values = c("#CC6677", "#88CCEE"),
                     breaks = c("Seasonal", "Perennial"),
                     labels = c("Seasonal", "Perennial")) +
  scale_shape_manual(values = c(22, 24),
                     name = "Combined with drug:",
                     breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  theme_bw() +
  ylim(0, 155) +
  xlim(0, 1.05) +
  labs(title = "", 
       x = "Degree of resistance", 
       y = "Time needed by the resistant genotype to spread\n from a frequency in inoculations of 1% to 100% (years)") +
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
  theme(legend.position = "none")
