# load libraries
library(tidyverse)
library(rvest)
library(RSelenium)
library(V8)
library(htmlunit)
library(cronR)
library(emayili)
library(magrittr)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(readODS)
library(readxl)
library(RColorBrewer)

# define url as object
url <- "https://www.gov.uk/government/statistics/agricultural-price-indices"

# read url as html object
h1 <- read_html(url)

# extract nodes from html object
p1 <- h1 %>% html_nodes("script") %>% html_text()

#scrape latest API .ods filename from p1[8]
pattern1 <- "[a-z]{5}://[a-z]{6}.[a-z]{10}.[a-z]{7}.[a-z]{3}.[a-z]{2}/[a-z]{10}/[a-z]{7}/[a-z]{6}/[a-z]{7}/[a-z]{10}_[a-z]{4}/[a-z]{4}/\\d\\d\\d\\d\\d\\d/[A-Z]{3}-[a-z]{14}-\\d\\d[a-z]{3}\\d\\d.[a-z]{3}"
API_latest_ods <- str_extract(p1[9], pattern1)

#download latest API monthly .ods dataset & change decimals to two
download.file(API_latest_ods, "~/projects/agri_ss/latest_API.ods", method = "wget")

# extraction of various prices paid by/to farmers (indexed to annual 2015 prices = 100)
all_data <- (read_ods("/home/eric/projects/agri_ss/latest_API.ods", sheet = "Outputs_monthly_2011_onwards", col_names = FALSE))
all_data <- all_data[4:ncol(all_data)]
# colnames(all_data) <- slice(all_data, 7)
date <- slice(all_data, 7)
date <- as.character(as.list(date))
date[length(date)] <- substring(date[length(date)], 1, 6)
animals_for_slaughter_or_export <- as.numeric(slice(all_data, 58))
animal_products <- as.numeric(slice(all_data, 73))
cooking_apples <- as.numeric(slice(all_data, 46))
onions_brown <- as.numeric(slice(all_data, 40))
wheat_for_bread <- as.numeric(slice(all_data, 16))

# extraction of monthly inputs - straight fertilizer [21] costs, animal feed stuffs [36], vetinary services [34], motor fuels [18], seeds [13](indexed to annual 2015 prices = 100)
all_data2 <- read_ods("/home/eric/projects/agri_ss/latest_API.ods", sheet = "Inputs_monthly_2011_onwards", col_names = FALSE)
all_data2 <- all_data2[4:ncol(all_data2)]
seeds <- as.numeric(slice(all_data2, 13))
motor_fuels <- as.numeric(slice(all_data2, 18))
vetinary_services <- as.numeric(slice(all_data2, 34))
animal_feed_stuffs <- as.numeric(slice(all_data2, 36))
straight_fertilizer <- as.numeric(slice(all_data2, 21))

# plot preparation for of all monthly input/output prices
selection1 <- bind_rows("date" = date, "animals_for_slaughter_or_export" = animals_for_slaughter_or_export, "animal_products" = animal_products, "cooking_apples" = cooking_apples, "onions_brown" = onions_brown, "wheat_for_bread" = wheat_for_bread, "seeds" = seeds, "motor_fuels" = motor_fuels, "vetinary_services" = vetinary_services, "animal_feedstuffs" = animal_feed_stuffs, "straight_fertilizer" = straight_fertilizer)
selection1 <- selection1 %>% pivot_longer(-date, names_to = "product", values_to = "API")
selection1$date <- as.Date.character(parse_date_time(selection1$date, "%m-%y"), origin = 1970-01-01)
#UNUSED selection1$date <- parse_date_time(selection1$date, "%m-%y")
selection1 %>% ggplot(aes(x=date, y=c(animals_for_slaughter_or_export, animal_products, cooking_apples, onions_brown, wheat_for_bread, seeds, motor_fuels, vetinary_services, animal_feed_stuffs, straight_fertilizer))) +
    scale_x_date() + theme_economist() + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(0.5,"cm"),legend.position = c(0.29, 0.85))+ xlab("Jan 2011 to Present") + ylab("API - indexed for 2015 = 100") + geom_line(mapping = aes(linetype = product, color = product))

#UNUSED selection1 %>% ggplot(aes(date, product, fill = API)) +
  geom_tile(color = "grey50") +
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  geom_vline(xintercept="2016-07-01 UTC", col = "blue") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), 
        legend.position="bottom", 
        text = element_text(size = 8)) +
  ggtitle("API_inputs_and_outputs") + 
  ylab("") + xlab("")

#UNUSED dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")
#rooted raster ONS plot - avoids the really high API values dominating the plot
selection1plot <- ggplot(selection1, aes(date, product)) +
    geom_raster(aes(fill = API)) + geom_vline(xintercept = selection1$date[661], col="red")
#save plot
ggsave("~/projects/raster_plot_ONS/10_rooted_API_inputs_outputs_raster_plot.png", plot = selection1plot)   
# set up of email alert
email_eric <- envelope()
email_eric <- email_eric %>% from("oedipusatcolonussheffield@gmail.com") %>% to("eric210bohun@gmail.com")
email_eric <- email_eric %>% subject("UK Agrcultural Price Indicators - Raster plot for 10 inputs & outputs")
email_eric <- email_eric %>% text("rooted UK farming inputs & outputs raster plot")
email_eric <- email_eric %>% attachment("~/projects/raster_plot_ONS/10_rooted_API_inputs_outputs_raster_plot.png")
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "oedipusatcolonussheffield@gmail.com",
               password = "XXXXXX")
smtp(email_eric, verbose = TRUE)
  
