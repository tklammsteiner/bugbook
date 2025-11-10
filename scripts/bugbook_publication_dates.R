library(readxl)
library(tidyverse)
library(scales)
library(ggExtra)


chapters <- read_excel("bugbook_data.xlsx", sheet = "chapters")

main <- chapters %>% 
  #mutate(Label = ifelse(Chapter == 0, paste0("Editorial\n", First_Author),
  #                      paste0("Chapter ", Chapter, "\n", First_Author))) %>%
  mutate(Label = ifelse(Chapter == 0, paste0("Editorial"),
                        paste0("Chapter ", Chapter))) %>% 
  select(Chapter, Label, Received, Accepted, Published_online) %>% 
  pivot_longer(cols = 3:5, names_to = "Status", values_to = "Date") %>% 
  mutate(Status = factor(Status, levels = c("Received", "Accepted", "Published_online")),
         Date = as.Date(Date, "%Y-%m-%d", tz = "UTC")) %>% 
  group_by(Label) %>% 
  ggplot(aes(x = reorder(Label, desc(Chapter)), y = Date)) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  #scale_color_manual(values = c("Received" = "#9E2A2B", "Accepted" = "#2C5F5D", "Published_online" = "#E09F3E")) +
  scale_color_manual(values = c("#9E2A2B", "#2C5F5D", "#E09F3E"),
                     labels = c("Received", "Accepted", "Online")) +
  scale_y_date(date_breaks = "months", date_labels = "%b %y", ) +
  labs(x = NULL,
       y = NULL) +
  ggpubr::theme_pubclean() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

# Add marginal density plot
ggMarginal(main, type = "density", margins = "x", 
           groupColour = TRUE, groupFill = TRUE)

