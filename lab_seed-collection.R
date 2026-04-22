# Seed collection

source("https://inkaverse.com/setup.r")
url <- "https://docs.google.com/spreadsheets/d/16mHDNySMacwoNtHqV05WDhkL7Liq40Ox-XcIEcmajXo/edit?gid=466718038#gid=466718038"
gs <- as_sheets_id(url)

fb <- gs %>% 
  range_read("seeds")

font <- c("Courgette", "Tillana")

huito_fonts(font)

label <- fb %>% 
  rename("scientific.name" = "Nombre científico", "name" = "Nombre común") %>% 
  mutate(barcode = paste(number, gsub("(\\w+\\s+\\w+).*", "\\1", scientific.name), sep = "_")) %>% 
  mutate(across(barcode, ~gsub(" ", "-", .))) %>% 
  mutate(name = paste0('"', name, '"')) %>% 
  label_layout(size = c(5, 3)
               , border_color = "darkgreen"
               ) %>% 
  include_image(
    value = "https://www.untrm.edu.pe/assets/images/untrmazul.png"
    , size = c(2.2, 1.9)
    , position = c(3.1, 2.6)
    ) %>% 
  include_image(
    value = "https://huito.inkaverse.com/img/flozano.png"
    , size = c(0.7, 0.7)
    , position = c(4.6, 2.6)
    ) %>% 
  include_barcode(
     value = "barcode"
     , size = c(2, 2)
     , position = c(1, 2)
     ) %>% 
  include_text(value = "Orden:"
               , position = c(2, 2.1)
               , size = 6
               , color = "black"
                 , opts = list(hjust = 0)
               ) %>% 
  include_text(value = "Orden"
               , position = c(2.5, 1.8)
               , size = 6
               , color = "black"
                 , opts = list(hjust = 0)
               ) %>% 
  include_text(value = "Familia:"
               , position = c(2, 1.5)
               , size = 6
               , color = "black"
                 , opts = list(hjust = 0)
               ) %>% 
  include_text(value = "Familia"
               , position = c(2.5, 1.2)
               , size = 6
               , color = "black"
                 , opts = list(hjust = 0)
               ) %>% 
  include_text(value = "scientific.name"
               , position = c(2.5, 0.8)
               , size = 9
               , color = "black"
                 , font[1] 
               ) %>% 
  include_text(value = "name"
               , position = c(2.5, 0.3)
               , size = 9
               , color = "black"
                 , font[2]
               )

label %>% label_print()

# label %>% label_print(mode = "c")



