# Some exploratory plots on customer carton breakouts
# Load required packages

pkgs = c("data.table", "ggplot2", "dplyr", "viridis")
lapply(pkgs, require, character.only = TRUE)
rm(pkgs)


# Read in customer data csv

customer = fread("customer.csv")

# Adjust column header names
setnames(customer, c(names(customer)), c(tolower(names(customer))))


# Sum up carton details to get number of SKUs and total units per carton
# Create chart specifically for Footwear (non-full case)

# Enter customer name for chart
customer_name = "" 

customer %>% 
  group_by(pkt, ctn, division, pack_code, date_invoiced) %>%
  summarise(units = sum(packed_units),
            skus = n_distinct(sku)) %>%
  filter(division == 'FW' & !(pack_code %in% c('FC', ''))) %>%
  ggplot(aes(x = units, fill = pack_code)) +
  geom_histogram(color = "black", binwidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(xlim = c(0, 20)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = paste0(customer_name, " Footwear Cartons")) +
  xlab("Units Per Carton") +
  ylab("Number of Cartons") +
  guides(fill = guide_legend(title = "Pack Code")) +
  theme_minimal()



# See differences between cartons going through Active Pick and Distrib areas
customer_sum %>%
  filter(division == 'FW' & !(pack_code %in% c('FC', ''))) %>%
  group_by(pack_code) %>%
  summarise(tot_units = sum(units),
            num_cartons = n_distinct(ctn)) %>%
  mutate(freq = num_cartons / sum(num_cartons))
