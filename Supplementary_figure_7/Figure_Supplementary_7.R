###############################################################################
# Code to visualize Figure Supplement 7                                       #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# Package
library(ggpattern)

# Load data
data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_7.csv", header = T)

# Creat labels for each constrain factor
C.labs <- c("Coverage reduction\nat booster = 0%", "Coverage reduction\nat booster = 20%")
names(C.labs) <- c("0", "0.2")
S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")
V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("AIV", "BSV")
D.labs <- c("Seasonal deployment without\n drugs given with boosters", "Seasonal deployment with\n drugs given with boosters", "Perennial deployment", "Perennial deployment \n(larger half-life range)")
names(D.labs) <- c("Seasonal", "Seasonal + drugs", "Perennial + short half-life", "Perennial")

# Define the break for the y axis
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# Select the first order indices
data_2 <- data[data$Effect == "First",]

# Define if impact is negative or positive
data_2$Impact <- 0
data_2$Impact[data_2$Factor == "Access"] <- 1
data_2$Impact <- as.factor(data_2$Impact)

# Select setting to visualize
data_2$Factor <- factor(data_2$Factor[data_2$Setting == "Spread_sesonality2_Coverage_reduced_0" &
                                      data_2$Vaccine_Type == "AIV" &
                                      data_2$SPAQ == "0"  &
                                      data_2$half_life_short == "0"], 
                        levels = data_2$Factor[order(data_2$First[data_2$Effect == "First" &
                                                                  data_2$Setting == "Spread_sesonality2_Coverage_reduced_0"  &
                                                                  data_2$Vaccine_Type == "AIV"  &
                                                                  data_2$SPAQ == "0" & data_2$half_life_short == "0"])])


data_2 <- data_2[data_2$deployment == "Seasonal" | data_2$deployment == "Seasonal + drugs" | data_2$deployment == "Perennial + short half-life", ]

data_2$deployment <- factor(data_2$deployment, levels = c("Seasonal", "Seasonal + drugs", "Perennial + short half-life"))

# Visualize the results
constant <- 2.5
PA <-
  ggplot(data_2, aes(x = Factor, y = First, fill = Impact, pattern = Seasonality)) +
  geom_bar(stat = "identity",
           color = "black",
           width = 0.6) +
  facet_nested(deployment * Coverage_reduced ~ Vaccine_Type,
               labeller = labeller(Vaccine_Type = V.labs,
                                   deployment = D.labs,
                                   Coverage_reduced = C.labs)) +
  scale_x_discrete(labels = c("Degree of resistance\nto the vaccine",
                              "Vaccine half-life (years)",
                              "EIR (inoculations per \n person per year)",
                              "Vaccine coverage (%)",
                              "Access to treatment (%)",
                              "Initial vaccine efficacy (%)",
                              "Decay of vaccine efficacy\nat the second dose"),
    breaks = c("Degree_resistance",
               "half_life", 
               "eir",
               "Coverage",
               "Access",
               "initialEfficacy",
               "decay_efficacy")) +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Relative importance on spread") +
  scale_fill_manual(name = "Relationship:",
                    breaks = c("0", "1"),
                    labels = c("Positive", "Negative"),
                    values = c("#2C85B2", "#B26F2C")) +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold")) +
  ggtitle(label = "") +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 18 / constant,
                                color = "black",
                                face = "bold")) +
  coord_flip() +
  theme(legend.position = "top")


# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_7.pdf", plot = PA, width = 14, height = 20, device = "pdf", units = "cm", dpi = 300)
