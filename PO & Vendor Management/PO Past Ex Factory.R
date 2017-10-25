# Required list of packages
pkgs = c("data.table", "dplyr", "ggplot2", "opsanalysis")
lapply(pkgs, require, character.only = TRUE)
rm(pkgs)

# Read in dataset
pos = fread("ex factory.csv", colClasses = "character")

# Adjust column headers
format_col_label(pos)

# Remove EA/PAA from qty
pos[ , qty := sub("[[:space:]][A-Za-z]+", "", purchase.order.qty)]

# replace "#" in ibd creation date with NAs
pos[ , ibd.creation.date := sub("#", NA, ibd.creation.date)][ , past.ex.factory := current.ship.date < ibd.creation.date]


# Generate list of POs past Ex-Factory date
po_list = pos %>% 
  filter(past.ex.factory == TRUE) %>% 
  select(purchasing.document.number)


# Summarise total qty for POs past Ex-Factory

pos %>%
  filter(purchasing.document.number %in% po_list$purchasing.document.number) %>%
  group_by(purchasing.document.number, purchase.document.order.date) %>%
  summarise(units = sum(qty, na.rm = TRUE))
