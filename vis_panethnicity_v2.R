
##Load required packages
if (!requireNamespace("pacman")) install.packages('pacman')
library(pacman)
packages<-c("tidyverse","lubridate","glue","quanteda","dplyr",
            "haven","here","magrittr","ggplot2", "gridExtra", "cowplot","forcats","grid")
p_load(packages,character.only = TRUE)

new_df <- read.csv("fulltext_cleaned.csv")

## construct the sub-ethnic mentions
new_df <- new_df %>% 
  mutate(
    asian=str_count(alltext,regex("Asia",ignore_case = T)), #Asian American, Asian Americans,Asian, Asians, Asian-, Asian-Americans, Asia
    # east asian ethnicity
    east_term =str_count(alltext,regex("East Asia|East-Asia",ignore_case = T)),
    chinese=str_count(alltext,regex("Chinese",ignore_case = T)),
    hk=str_count(alltext,regex("Hong Kong|hongkong",ignore_case = T)),
    jp=str_count(alltext,regex("Japanese",ignore_case = T)), 
    kr=str_count(alltext,regex("Korean",ignore_case = T)),
    mg=str_count(alltext,regex("mongolian|mongol",ignore_case = T)),
    ryky=str_count(alltext,regex("ryukyuan|Lewchewan|Loochooan",ignore_case = T)),
    tw=str_count(alltext,regex("Taiwanese",ignore_case = T)),
    tb=str_count(alltext,regex("Tibetan",ignore_case = T)),
    #east asian country
    china=str_count(alltext,regex("china|prc|hong kong|macau|macao",ignore_case = T)),
    japan=str_count(alltext,regex("Japan(?!ese)",ignore_case = T)), 
    nkorea=str_count(alltext,regex("North Korea(?!n)",ignore_case = T)),
    skorea=str_count(alltext,regex("South Korea(?!n)",ignore_case = T)),
    mongolia=str_count(alltext,regex("Mongolia(?!n)",ignore_case = T)),
    okinawa=str_count(alltext,regex("okinawa",ignore_case = T)),
    taiwan=str_count(alltext,regex("Taiwan(?!ese)|tai wan|roc",ignore_case = T)),
    tibet=str_count(alltext,regex("Tibet(?!an)",ignore_case = T)),
    #south asian ethnicity
    south_term=str_count(alltext,regex("South Asia|South-Asia",ignore_case = T)),
    bangla=str_count(alltext,regex("Bangladeshi|Bengali|bangalee",ignore_case = T)),
    bhutanese =str_count(alltext,regex("Bhutanese",ignore_case = T)),
    ind=str_count(alltext,regex("indian(?!-caribbean)",ignore_case = T)),
    indocari=str_count(alltext,regex("Indo-Caribbean|Indian-Caribbean",ignore_case = T)),
    indofuji=str_count(alltext,regex("Fiji Indian|Indo-Fijian",ignore_case = T)),
    maldivian=str_count(alltext,regex("Maldivian",ignore_case = T)),
    nepalese=str_count(alltext,regex("Nepalese|Nepali",ignore_case = T)),
    srilk=str_count(alltext,regex("Sri Lankan",ignore_case = T)),
    pakistani=str_count(alltext,regex("Pakistani",ignore_case = T)),
    #south asian countries
    india=str_count(alltext,regex("india(?!n)",ignore_case = T)),
    bangladesh=str_count(alltext,regex("Bangladesh(?!i)",ignore_case = T)),
    bhutan=str_count(alltext,regex("Bhutan(?!ese)",ignore_case = T)),
    nepal=str_count(alltext,regex("Nepal(?!i)",ignore_case = T)),
    maldives=str_count(alltext,regex("Maldives",ignore_case = T)),
    pakistan=str_count(alltext,regex("Pakistan(?!i)",ignore_case = T)),
    srilanka=str_count(alltext,regex("Sri Lanka(?!n)",ignore_case = T)),
    
    #south east ethnicity
    southeast_term=str_count(alltext,regex("Southeast Asia|South-east Asia|South east Asia|Southeastern Asia",ignore_case = T)), 
    burmese=str_count(alltext,regex("Burmese",ignore_case = T)),
    Filipino=str_count(alltext,regex("Filipino|Philipino",ignore_case = T)),
    hmong=str_count(alltext,regex("Hmong",ignore_case = T)),
    indonesian=str_count(alltext,regex("Indonesian",ignore_case = T)),
    iumien=str_count(alltext,regex("Iu Mien|indochinese",ignore_case = T)),
    laotian=str_count(alltext,regex("Laotian",ignore_case = T)),
    malaysian=str_count(alltext,regex("Malaysian",ignore_case = T)),
    thai=str_count(alltext,regex("Thai(?!land)|Siamese",ignore_case = T)),
    sgp=str_count(alltext,regex("Singaporean",ignore_case = T)),
    viet=str_count(alltext,regex("Vietnamese|Viet(?!nam)",ignore_case = T)),
    cmb=str_count(alltext,regex("Cambodian|khmer",ignore_case = T)),
    
    #south east countries
    Brunei=str_count(alltext,regex("Brunei",ignore_case = T)),
    Cambodia=str_count(alltext,regex("Cambodia(?!n)",ignore_case = T)),
    EastTimor=str_count(alltext,regex("East Timor",ignore_case = T)),
    Indonesia=str_count(alltext,regex("Indonesia(?!n)",ignore_case = T)),
    laos=str_count(alltext,regex("Laos",ignore_case = T)),
    Malaysia=str_count(alltext,regex("Malaysia(?!n)",ignore_case = T)),
    Myanmar=str_count(alltext,regex("Myanmar",ignore_case = T)),
    Philippines=str_count(alltext,regex("Philippines",ignore_case = T)),
    Singapore=str_count(alltext,regex("Singapore(?!an)",ignore_case = T)),
    Thailand=str_count(alltext,regex("Thailand",ignore_case = T)),
    Vietnam=str_count(alltext,regex("Vietnam(?!ese)",ignore_case = T))
  )

new_df <- new_df %>% 
  # each region's ethnicity and country, by region, I mean south, east, south east Asia
  mutate(
    eth_east=rowSums(select(., "chinese","hk","jp","kr","mg","ryky","tw","tb")),
    eth_south=rowSums(select(., "bangla","bhutanese","ind","indocari","indofuji","maldivian","nepalese","srilk","pakistani")),
    eth_southeast=rowSums(select(.,"burmese","Filipino","hmong","indonesian","iumien","laotian","malaysian","thai","sgp","viet","cmb")),
    ctr_east=rowSums(select(.,"china","japan","nkorea","skorea","mongolia","okinawa","taiwan","tibet")),
    ctr_south=rowSums(select(.,"india","bangladesh","bhutan","nepal","maldives","pakistan","srilanka")),
    ctr_southeast=rowSums(select(.,"Brunei","Cambodia","EastTimor","Indonesia","laos","Malaysia","Myanmar","Philippines","Singapore","Thailand","Vietnam"))
  ) %>%
  # combine ethnicity and country together for three regions
  mutate(
    east=rowSums(select(., "eth_east","ctr_east")),
    south=rowSums(select(., "eth_south","ctr_south")),
    southeast=rowSums(select(.,"eth_southeast","ctr_southeast")),
  ) %>%
  # not mention any country or ethnicity
  mutate(
    asian_as_a_whole=rowSums(select(., "eth_east","ctr_east","eth_south","ctr_south","eth_southeast","ctr_southeast","east_term","south_term","southeast_term"))
  )

# create a dataset for replication
new_df_rep <- new_df %>%
  select(-c("X.2", "X.1", "Author", "Publication.info", "Abstract", "Links", "Full.text",
            "Subject", "Title", "Publication.title", "Publication.year", "Publication.date",
            "Section", "Publisher", "Place.of.publication", "Country.of.publication",
            "Publication.subject", "ISSN", "Source.type", "Language.of.publication",
            "Document.type", "ProQuest.document.ID", "Document.URL", "Copyright",
            "Last.updated", "Database", "Location", "Identifier...keyword", "newspaper"))

colnames(new_df_rep)
write.csv(new_df_rep, "new_df_replication.csv", row.names = FALSE)

# plot1: create a variable sub_ethnicity, include both country mentions and ethnicity mentions
# only east, south, southeast or multi
new_df$sub_ethnicity <- as.factor(ifelse(new_df$east >= 1 & new_df$south==0 & new_df$southeast==0, "East Asian",
                                         ifelse(new_df$south >= 1 & new_df$east== 0 & new_df$southeast==0, "South Asian",
                                                ifelse(new_df$southeast >= 1 & new_df$east== 0 & new_df$south==0, "Southeast Asian", 
                                                       ifelse(new_df$southeast == 0 & new_df$east== 0 & new_df$south==0,  "No Specific", "Multiple"
                                                       ))))
)

table(new_df$sub_ethnicity)

regionbyy <-new_df %>% 
  group_by(Publication.year,sub_ethnicity) %>% 
  summarise(number = n()) 

#Convert long-formatted data into wide
regionbyy_wide <- regionbyy %>% pivot_wider(names_from = sub_ethnicity, values_from = number)

# welch t test
t.test(regionbyy_wide$`South Asian`, regionbyy_wide$`East Asian`, paired = FALSE, var.equal = FALSE)

# t = -8.3154, p-value = 2.575e-10
# mean of x mean of y 
# 3.69697  40.29268 


# plot1: Figure 1 main
regionbyy$sub_ethnicity <- factor(regionbyy$sub_ethnicity, 
                                  levels = c("East Asian", "South Asian", "Southeast Asian", "No Specific", "Multiple"))

plot1 <- regionbyy %>%
  ggplot(aes(x = Publication.year, y = number, color = sub_ethnicity, linetype = sub_ethnicity, size = sub_ethnicity)) +
  geom_line() +
  xlab("Year") + 
  ylab("Number of News Articles") +
  labs(color = "Sub-ethnicity", linetype = "Sub-ethnicity", size = "Sub-ethnicity") +
  scale_color_manual(values = c("East Asian" = "#992813", "South Asian" = "#62787F", "Southeast Asian" = "#FACA78", "No Specific" = "#5e5e5e", "Multiple" = "#d0d0d0")) +
  scale_linetype_manual(values = c("East Asian" = "solid", "South Asian" = "dashed", "Southeast Asian" = "dotdash", "No Specific" = "dotted", "Multiple" = "longdash")) +
  scale_size_manual(values = c("East Asian" = 0.5, "South Asian" = 0.5, "Southeast Asian" = 0.5, "No Specific" = 0.4, "Multiple" = 0.4)) +
  theme(legend.position = "none", plot.title = element_text(size=8), axis.text = element_text(size=6), axis.title = element_text(size=6)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        panel.border = element_blank())

plot1
ggsave("f1_main.eps", plot1, width=150, height=60, units="mm")

# 992813, 62787F,CAAD9D, C38458

# proportion instead of absolute mentions
# plot2
new_df$sub_ethnicity2 <- as.factor(ifelse(new_df$east >= 1, "East Asian",
                                          ifelse(new_df$south >= 1, "South Asian",
                                                 ifelse(new_df$southeast >= 1, "Southeast Asian", 
                                                        "No Specific"
                                                 )))
)

table(new_df$sub_ethnicity2)

regionbyy_2 <-new_df %>% 
  group_by(Publication.year,sub_ethnicity2) %>% 
  summarise(number = n()) 

total_per_year <- new_df %>%
  group_by(Publication.year) %>%
  summarise(total = n())

merged_data <- merge(total_per_year, regionbyy_2, by = "Publication.year")

# fill missing Southeast/ south with 0
years <- data.frame(Publication.year = seq(min(merged_data$Publication.year), max(merged_data$Publication.year), by = 1))
ethnicities <- data.frame(sub_ethnicity2 = unique(merged_data$sub_ethnicity))
complete_grid <- expand_grid(years, ethnicities)
complete_data <- complete_grid %>%
  left_join(merged_data, by = c("Publication.year", "sub_ethnicity2")) %>%
  replace_na(list(number = 0))

complete_data_with_total <- complete_data %>%
  left_join(total_per_year, by = "Publication.year")

complete_data_with_total <- complete_data_with_total %>%
  mutate(proportion = number / total.y) 

complete_data_with_total$sub_ethnicity2 <- factor(complete_data_with_total$sub_ethnicity2, 
                                                  levels = c("East Asian", "South Asian", "Southeast Asian", "No Specific", "Multiple"))

plot2 <- complete_data_with_total %>%
  ggplot(aes(x = Publication.year, y = proportion, color = sub_ethnicity2, linetype = sub_ethnicity2, size = sub_ethnicity2)) +
  geom_line() +
  xlab("Year") + 
  ylab("Proportion of News Articles") +
  labs(color = "Sub_ethnicity", linetype = "Sub_ethnicity", size = "Sub_ethnicity") +
  scale_color_manual(values = c("East Asian" = "#992813", "South Asian" = "#62787F", "Southeast Asian" = "#FACA78", "No Specific" = "#5e5e5e", "Multiple" = "#d0d0d0")) +
  scale_linetype_manual(values = c("East Asian" = "solid", "South Asian" = "dashed", "Southeast Asian" = "dotdash", "No Specific" = "dotted", "Multiple" = "longdash")) +
  scale_size_manual(values = c("East Asian" = 0.5, "South Asian" = 0.5, "Southeast Asian" = 0.5, "No Specific" = 0.4, "Multiple" = 0.4)) +
  theme(legend.position = "none", plot.title = element_text(size=8), axis.text =element_text(size=6),axis.title =element_text(size=6))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        panel.border = element_blank())
plot2
ggsave("f1_si.eps", plot2, width=150, height=70, units="mm")


# plot3: create a variable sub_ethnicity, include only ethnicity mentions
# only east, south, southeast or multi
new_df$sub_ethnicity1 <- as.factor(ifelse(new_df$eth_east >= 1 & new_df$eth_south==0 & new_df$eth_southeast==0, "East Asian",
                                          ifelse(new_df$eth_south >= 1 & new_df$eth_east== 0 & new_df$eth_southeast==0, "South Asian",
                                                 ifelse(new_df$eth_southeast >= 1 & new_df$eth_east== 0 & new_df$eth_south==0, "Southeast Asian", 
                                                        ifelse(new_df$eth_southeast == 0 & new_df$eth_east== 0 & new_df$eth_south==0,  "No Specific", "Multiple"
                                                        ))))
)

table(new_df$sub_ethnicity1)

regionbyy2 <-new_df %>% 
  group_by(Publication.year,sub_ethnicity1) %>% 
  summarise(number = n()) 

regionbyy2

regionbyy2$sub_ethnicity1 <- factor(regionbyy2$sub_ethnicity1, 
                                  levels = c("East Asian", "South Asian", "Southeast Asian", "No Specific", "Multiple"))


plot3 <- regionbyy2 %>%
  ggplot(aes(x = Publication.year, y = number, color = sub_ethnicity1, linetype = sub_ethnicity1, size = sub_ethnicity1)) +
  geom_line() +
  xlab("Year") + 
  ylab("Number of News Articles") +
  labs(color = "Sub-ethnicity", linetype = "Sub-ethnicity", size = "Sub-ethnicity") +
  scale_color_manual(values = c("East Asian" = "#992813", "South Asian" = "#62787F", "Southeast Asian" = "#FACA78", "No Specific" = "#5e5e5e", "Multiple" = "#d0d0d0")) +
  scale_linetype_manual(values = c("East Asian" = "solid", "South Asian" = "dashed", "Southeast Asian" = "dotdash", "No Specific" = "dotted", "Multiple" = "longdash")) +
  scale_size_manual(values = c("East Asian" = 0.5, "South Asian" = 0.5, "Southeast Asian" = 0.5, "No Specific" = 0.4, "Multiple" = 0.4)) +
  theme(legend.position = "none", plot.title = element_text(size=8), axis.text =element_text(size=6),axis.title =element_text(size=6))+  
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        panel.border = element_blank())
plot3
ggsave("f2_si.eps", plot3, width=150, height=60, units="mm")

# plot4
allmulti <-new_df %>% 
  subset(sub_ethnicity  == "Multiple")

sum_mentions <- allmulti %>%
  group_by(Publication.year) %>%
  summarise(subregion1_total = mean(east),
            subregion2_total = mean(south),
            subregion3_total = mean(southeast))

# Create a line plot to visualize the total mentions over the years for each subregion
plot4 <- ggplot(sum_mentions, aes(x = Publication.year)) +
  geom_line(aes(y = subregion1_total, color = "East Asian", linetype = "East Asian"), size = 0.5) +
  geom_line(aes(y = subregion2_total, color = "South Asian", linetype = "South Asian"), size = 0.4) +
  geom_line(aes(y = subregion3_total, color = "Southeast Asian", linetype = "Southeast Asian"), size = 0.4) +
  labs(x = "Year",
       y = "Average Mentions in News Article",
       color = "Sub-ethnicity", linetype = "Sub-ethnicity", size = "Sub-ethnicity") +
  scale_color_manual(values = c("East Asian" = "#992813", "South Asian" = "#62787F", "Southeast Asian" = "#FACA78")) +
  scale_linetype_manual(values = c("East Asian" = "solid", "South Asian" = "dashed", "Southeast Asian" = "dotdash", "No Specific" = "dotted", "Multiple" = "longdash"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        panel.border = element_blank())
plot4
ggsave("f3_si.eps", plot4, width=150, height=70, units="mm")


