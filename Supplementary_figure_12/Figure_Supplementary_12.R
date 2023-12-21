###############################################################################
# Code to visualize Figure Supplement 12                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# load the data
data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_12.csv", header = T)

# Creat labels for each constrain factor
C.labs <- c("Coverage reduction = 0%", "Coverage reduction = 20%")
names(C.labs) <- c("0", "0.2")
S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")
A.labs <- c("Whole population", "Adult")
names(A.labs) <- c("0.75", "18")
B.labs <- c("No drug deployed with vaccine", "Drug deployed with vaccine")
names(B.labs) <- c("0", "1")

# Define the break for the y axis
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# Select first order indices
data_2 <- data[data$Effect == "First",]

# Order the level of each factors
data_2$Factor <- factor(data_2$Factor[data_2$Setting == "Spread_sesonality2_Coverage_reduced_0_Age_0.75_Blood_coverage_0"], levels = data_2$Factor[order(data_2$First[data_2$Effect == "First" &
                                                                                                                                                         data_2$Setting == "Spread_sesonality2_Coverage_reduced_0_Age_0.75_Blood_coverage_0"])])

# Define which factor had a negative impacts
data_2$Impact <- 0
data_2$Impact[data_2$Factor == "eir"] <- 1
data_2$Impact <- as.factor(data_2$Impact)

# Define level of factors
data_2$Seasonality <- factor(data_2$Seasonality, levels = c("sesonality2", "sesonality1"))

# Constant
constant <- 2.5

# Visualise
pd <- position_dodge(0.05)
PLOT <- 
  ggplot(data_2, aes(x = Factor, y = First, fill = Impact)) +
  geom_bar(stat = "identity", 
           color = "black", 
           width = 0.6, 
           position = pd) +
  facet_nested(Blood_coverage + Seasonality ~ Age + Coverage_reduced,
               labeller = labeller(Age = A.labs,
                                   Seasonality = S.labs,
                                   Blood_coverage = B.labs,
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
  scale_fill_manual(
    name = "Relationship",
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
    strip.text.y = element_text(
      size = 18 / constant,
      color = "black",
      face = "bold")) +
  coord_flip() +
  theme(legend.position = "top")


# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_12.pdf", plot = PLOT, width = 17.5, height = 17.5, device = "pdf", units = "cm", dpi = 300)
