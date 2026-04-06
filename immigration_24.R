getwd()
library(readxl)
mois_24 <- read_excel("input/nationalite_mois.xlsx")

library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)

mois_24_cl <- mois_24 %>% 
  slice(-c(1, 30, 31, 32, 33)) %>% #to remove rows use slice(-c(1,2,3)); 
#to remove columns use select(-c(1,2,3)) or select(-c("column name"))
  #select(-c(7, 12)) %>% 
  row_to_names(row_number = 1) %>%  #takes row 1 and makes it the header.
  rename(pays = 1) %>% 
  pivot_longer(-pays, names_to = "mois", values_to = "nombre") %>%  #-pays excludes the column pays from pivoting.
#It tells R: ➔ "pivot all columns EXCEPT pays."
  mutate(nombre = ifelse(is.na(nombre), 0, nombre)) #replace NA with 0

top10 <- mois_24_cl %>% 
  mutate(nombre = as.numeric(nombre)) %>%  # Convert 'nombre' to numeric
  group_by(pays) %>% 
  summarise(nombre = sum(nombre)) %>%
  arrange(desc(nombre)) %>% 
  slice_head(n = 10) # Select top 10 countries

mois_24_top10 <- mois_24_cl %>% 
  filter(pays %in% top10$pays) %>% 
  mutate(
    mois = as.factor(mois),       # Convert 'mois' to a factor
    nombre = as.numeric(nombre),  # Convert 'nombre' to numeric
    pays = as.factor(pays)        # Optionally convert 'pays' to a factor
  )



str(mois_24_top10)

# Specify the path where you want to save the CSV file
file_path <- "output/mois_24_top10.csv"

# Save the data frame as a CSV file
write.csv(mois_24_top10, file_path, row.names = FALSE)


# Create a stacked area chart
library(ggplot2)


ggplot(mois_24_top10, aes(x = mois, y = nombre, fill = pays)) +
  geom_area(position = 'stack') #need to group_by mois to get the sum per month


#########################


# Demandes de protection internationale par mois en 2025
demandes_25 <- data.frame(
  mois = factor(
    c("janv.", "févr.", "mars", "avr.", "mai", "juin",
  "juil.", "août", "sept.", "oct.", "nov.", "déc."),
  levels = c("janv.", "févr.", "mars", "avr.", "mai", "juin", #convert 'mois' to a factor with the desired order
             "juil.", "août", "sept.", "oct.", "nov.", "déc.")
  ),
  demandes = c(188, 170, 136, 128, 171, 106, 150, 130, 154, 184, 132, 119)
)



ggplot(demandes_25, aes(x = mois, y = demandes)) +
  geom_col(fill = "#DF002C", alpha = 0.6) + #the alpha parameter controls the transparency of the geometric objects (like bars, points, lines, etc.). It takes values between 0 (completely transparent) and 1 (completely opaque)
  coord_radial(start = 17 * (pi / 180),
    inner.radius = 0.09 # optional
  ) +
  labs(
    title = "Nombre de personnes ayant introduit une demande \nde protection internationale (2025)",
    #subtitle = "Luxembourg, 2025",
    x = "",
    y = "Demandes"
  ) +
  theme_minimal(base_size = 11) + #base_size argument adjusts the font size; the default base_size is 11.
  theme(panel.grid.major = element_line(color = "#B9B9B8", linetype = "solid", size = 0.2))


#Transferts Dublin

library(readxl)
transferts_Dublin <- read_excel("input/transferts_Dublin.xlsx")

library(janitor)
Dublin <- transferts_Dublin %>% 
  select(-1) %>% #use select to remove columns, slice to remove rows
  slice(-20) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  {
    colnames(.)[2:13] <- 1:12     # Rename columns 2:13 to 1:12. The {} block allows you to perform operations that don't return a value in the traditional sense (like assigning column names).
  .
    } %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%   # Replace NA with 0 only in numeric columns. The first column contains character (text) data, and replace_na() is trying to replace NA values with 0, which is a numeric value. need to handle numeric and character columns separately.
  pivot_longer(
    cols = 2:13,  # Columns to pivot (currently named 1:12)
    names_to = "mois",  # New column for month identifiers
    values_to = "value"  # New column for the values
  ) %>% 
  mutate(mois = recode(mois,
                       "1" = "janv.", "2" = "févr.", "3" = "mars",
                       "4" = "avr.", "5" = "mai", "6" = "juin",
                       "7" = "juil.", "8" = "août", "9" = "sept.",
                       "10" = "oct.", "11" = "nov.", "12" = "déc.")) %>%
  mutate(mois = factor(mois, levels = c("janv.", "févr.", "mars", "avr.", "mai", "juin",
                                        "juil.", "août", "sept.", "oct.", "nov.", "déc."))) %>% 
  mutate(pays_de_destination = factor(pays_de_destination, levels = sort(unique(pays_de_destination), decreasing = TRUE))) #Sort the data frame by country (A to Z descending)


#Visualized as a heatmap - countries sorted by total, with total row/column
library(tidyverse)

# Sort countries by annual total (ascending) so highest appears at top of chart
country_order <- Dublin |>
  group_by(pays_de_destination) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
  arrange(total) |>
  pull(pays_de_destination) |>
  as.character()

y_limits <- c("Total", country_order)       # Total at bottom, smallest → largest going up
x_limits <- c(levels(Dublin$mois), "Total") # months then Total at right

# Build total row (by month), total column (by country), and corner cell
total_by_mois <- Dublin |>
  group_by(mois) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(pays_de_destination = "Total")

total_by_pays <- Dublin |>
  group_by(pays_de_destination) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(mois = factor("Total", levels = x_limits),
         pays_de_destination = as.character(pays_de_destination))

corner <- tibble(
  mois = factor("Total", levels = x_limits),
  pays_de_destination = "Total",
  value = sum(Dublin$value, na.rm = TRUE)
)

totals <- bind_rows(total_by_mois, total_by_pays, corner) |>
  mutate(
    mois = factor(as.character(mois), levels = x_limits),
    pays_de_destination = factor(pays_de_destination, levels = y_limits)
  )

Dublin_plot <- Dublin |>
  mutate(
    mois = factor(as.character(mois), levels = x_limits),
    pays_de_destination = factor(as.character(pays_de_destination), levels = y_limits)
  )

ggplot() +
  geom_tile(data = Dublin_plot,
            aes(x = mois, y = pays_de_destination, fill = value),
            color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "D", name = NULL) +
  geom_tile(data = totals,
            aes(x = mois, y = pays_de_destination),
            fill = "#78909C", color = "white", linewidth = 0.3) +
  geom_text(data = totals,
            aes(x = mois, y = pays_de_destination, label = value),
            size = 2.6, color = "white", fontface = "bold") +
  geom_hline(yintercept = 1.5, color = "white", linewidth = 1.2) +
  geom_vline(xintercept = 12.5, color = "white", linewidth = 1.2) +
  scale_y_discrete(limits = y_limits) +
  scale_x_discrete(limits = x_limits) +
  labs(
    title = "Transferts en application du règlement Dublin III",
    subtitle = "Personnes transférées vers des États membres (2025)",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, -6, 0),
    legend.text = element_text(size = 7)
  ) +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 0.35))


#In a circular format - too many rows/impossible to read
ggplot(Dublin, aes(x=mois, y=pays_de_destination, fill = value))+
  geom_tile()+
  geom_hline(
    yintercept = seq(0.5, 17.5, by = 1),
    color = "white", linewidth = 0.5
  )+
  scale_fill_viridis_c(option = "B", name = "Personnes")+
  coord_radial(
    start = .15,
    # add inner whole
    inner.radius = 0.07,
    expand = FALSE
  )+
  labs(
    title = "Transferts en application du règlement Dublin III en 2025",
    subtitle = "Personnes transférées vers des Etats membres",
    x = "",
    y = ""
  ) +
  theme_minimal()+
  theme(legend.position = "right")





  

