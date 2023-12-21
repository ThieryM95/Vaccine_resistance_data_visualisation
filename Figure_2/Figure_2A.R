###############################################################################
# Code to visualize Figure Supplement 2A                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# load package
library("rlang")
library("ggh4x")
library("ggplot2")
library('plyr')

# Define color palletter

pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")
# load the data
data <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_2A.csv", header = T)

# transform the constrained variable into factor
data$Seasonality <- factor(data$Seasonality, levels = c("sesonality2", "sesonality1"))
data$Age <- factor(data$Age, levels = c("Children", "18", "0.75"))
data$Vaccine_Type <- factor(data$Vaccine_Type, levels = c("AIV", "BSV"))

# Creat labels for each constrain factor
C.labs <- c("Coverage reduction = 0%", "Coverage reduction = 20%")
names(C.labs) <- c("0", "0.2")
S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")
A.labs <- c("Children", "Whole population", "Adults")
names(A.labs) <- c("Children", "0.75", "18")
B.labs <- c("No drug deployed with vaccine", "Drug deployed with vaccine")
names(B.labs) <- c("0", "1")
V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("AIV", "BSV")

# define the break for the y axis
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# Constant
constant <- 2

# Plot
PLOT_A <- 
  ggplot(data_2, 
         aes(x = Factor,
             y = First, 
             fill = Impact)) +
  geom_bar(stat = "identity",
           color = "black",
           width = 0.6,
           position = pd) +
  facet_nested(Seasonality ~ Age + Vaccine_Type, 
               labeller = labeller(Vaccine_Type = V.labs,
                                   Age = A.labs,
                                   Seasonality = S.labs)) +
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
  scale_fill_manual(name = "Relationship",
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
  theme(strip.text.x = element_text(size = 18 / constant, 
                                    color = "black", 
                                    face = "bold"),
  strip.text.y = element_text(size = 18 / constant, 
                                color = "black", 
                                face = "bold")) +
  coord_flip() +
  theme(legend.position = "top")
