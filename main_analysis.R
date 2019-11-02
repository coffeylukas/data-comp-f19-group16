#
#
# Data Comp 2019
# --------------------------
setwd("~/Documents/School/VT/DataCompFall19")

library(tidyverse)
library(ggdark)
library(scales)

# General Hospital Data
general <- read_csv("data/Hospital_Revised_FlatFiles/Hospital\ General\ Information.csv")

# By State death and complications
deaths.hospital <- read_csv("data/Hospital_Revised_FlatFiles/Complications and Deaths - Hospital.csv")
deaths.state <- read_csv("data/Hospital_Revised_FlatFiles/Complications and Deaths - State.csv")
patient.rating <- read_csv("data/Hospital_Revised_FlatFiles/HCAHPS - State.csv")
HAI <- read_csv("data/Hospital_Revised_FlatFiles/Healthcare Associated Infections - State.csv")

glimpse(deaths.state)
unique(deaths.state$`State`)
unique.measures <- unique(deaths.state$`Measure ID`)

# AMI scores by state
AMI.measures <- deaths.state %>%
  filter(`Measure ID` == unique.measures[1]) %>%
  glimpse()

AMI.measures %>%
  arrange(desc(`Number of Hospitals Worse`)) %>%
  head(10)

# -----------------------------------------------------------------------------------
# State bar plot
# -----------------------------------------------------------------------------------
ggplot(deaths.state, aes(x = `Measure Name`)) +
  geom_bar(aes(fill = State)) +
  coord_flip()


# -----------------------------------------------------------------------------------
# Scores per hospital
# -----------------------------------------------------------------------------------

# worst average scores per state
cleaned.scores <- deaths.hospital %>%
  mutate(
    Score = na_if(Score, "Not Available")
  ) %>%
  drop_na(Score)
  
average.state.scores <- cleaned.scores %>%
  group_by(
    State
  ) %>%
  summarize(
    mean = mean(as.numeric(Score), na.rm = T)
  ) %>%
  arrange(desc(mean))

ggplot(average.state.scores, aes(x = State, y = mean)) +
  geom_bar(aes(fill = mean), stat = 'identity') + 
  coord_flip() +
  theme_minimal()

# worst scores for each metric
average.metric.scores <- cleaned.scores %>%
  group_by(
    `Measure Name`
  ) %>%
  summarize(
    mean = mean(as.numeric(Score), na.rm = T)
  ) %>%
  arrange(desc(mean))

glimpse(cleaned.scores)

states <- list(state.abb = state.name)







scores <- deaths.hospital %>%
  mutate(
      Score = na_if(Score, "Not Available")
    ) %>%
  drop_na(Score) %>%
  select(Score)

scores$Score %>%
  as.character() %>%
  as.numeric() %>%
  is.na() %>%
  which()
  
  
glimpse(scores)

unique(scores$`Compared to National`)

ggplot(scores, aes(x = `Compared to National`)) +
  geom_bar() +
  coord_flip()




# -----------------------------------------------------------------------------------
# Creating graphs
# -----------------------------------------------------------------------------------
maxnum <- max(as.character(as.numeric(general$`Hospital overall rating`)), na.rm = T)
highest <- general[ which(general$`Hospital overall rating` == maxnum), ]
average <- mean(as.numeric(as.character(general$`Hospital overall rating`)), na.rm = T)

nevada.table <- data.frame(Category = c('Nevada', 'Average', 'Highest'),
                    Rating = c(1.96, average, 5))
nevada.table
nevada.comparison <- ggplot(nevada.table, aes(x = Category, y = Rating)) +
  geom_bar(stat = "identity", fill = hue_pal()(2)[2], color = "black") +
  theme_minimal() +
  labs(title = "Comparing Nevada to the Highest and Average State Ratings")
  
# hospital boxplots
nevada.hospitals <- general %>%
  filter(State == 'NV')

nevada.rating <- ggplot(nevada.hospitals, aes(x = `Hospital overall rating`)) +
  geom_bar(fill = hue_pal()(2)[2], color = "black") + 
  theme_minimal() +
  labs(title = "Comparison of Hospital Ratings Within Nevada")

ggsave(filename = "poster/nevada-comparison.pdf",
       plot = nevada.comparison,
       width = 8, height = 4.5, units = "in")

ggsave(filename = "poster/nevada-rating.pdf",
       plot = nevada.rating,
       width = 8, height = 4.5, units = "in")


HAI.nevada <- HAI %>%
  filter(State == 'NV',
         `Measure ID` == 'HAI_1_SIR' |
           `Measure ID` == 'HAI_2_SIR' |
           `Measure ID` == 'HAI_3_SIR' |
           `Measure ID` == 'HAI_4_SIR' |
           `Measure ID` == 'HAI_5_SIR' |
           `Measure ID` == 'HAI_6_SIR')

mean(as.numeric(HAI.nevada$Score))


deaths.nevada <- deaths.state %>%
  filter(State == 'NV')













### HCAHPS
hcahps <- read_csv("data/Hospital_Revised_FlatFiles/HCAHPS\ -\ Hospital.csv")
summary(hcahps)
hcahps <- hcahps[!(hcahps$`Patient Survey Star Rating`=="Not Applicable"),]
hcahps$`HCAHPS Question`
findid <- hcahps[which(hcahps$`HCAHPS Question`==levels(hcahps$`HCAHPS Question`)[j])[1],1]
clear(hcahps,10,12)
### OUtpatients Imaging
imag <- read.csv("Outpatient Imaging Efficiency - Hospital.csv")
summary(imag)
imag <- imag[!(imag$Score=="Not Applicable"),]
clear(imag,10,11)








