#libraries
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(plotly)
library(geodata)
library(sf)
library(dplyr)
library(leaflet)
library(terra)
library(readxl)
library(janitor)
library(ggplot2)
library(scales)
library(bs4Dash)
library(highcharter)
library(writexl)
library(echarts4r)
library(shinyjs)
library(stringr)
library(tidyr)
library(prophet)
library(purrr)
library(rsconnect)
#=========================================================================
#Load Data
Countries<- read_xlsx("www/countries.xlsx", .name_repair = "minimal")
monthly_exports<- read_xlsx("www/countries.xlsx", sheet = 2, .name_repair = "minimal")
rwanda_map <- read_sf("www/rwanda_map.shp")
Export_Data <- read_xlsx("www/Export_Data.xlsx")
top_commodities <- read_xlsx("www/countries.xlsx", sheet = 3, .name_repair = "minimal")
Export_Income_Forecast<- read.csv("www/Export_Income_Forecast.csv")
Re_Export_Forecast <- read.csv("www/Re_Export_Forecast.csv")
contribution<- read_xlsx("www/countries.xlsx", sheet = "Sheet1")
 continents<- read_xlsx("www/countries.xlsx", sheet = 4, .name_repair = "minimal")
custom_colors <- c("darkgreen", "darkgrey", "grey", "#38211E", "#92340b")
#=========================================================================
# ======================================================
# Scrapping NBR ----
scrape_nbr <- function() {
  library(rvest)
  library(dplyr)
  library(chromote)
  library(readr)
  
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  
  url <- "https://www.bnr.rw/exchangeRate"
  b$Page$navigate(url)
  b$Page$loadEventFired(timeout_ = 20000)
  Sys.sleep(10)
  
  page <- b$DOM$getDocument()
  html <- b$DOM$getOuterHTML(nodeId = page$root$nodeId)[["outerHTML"]]
  page_html <- read_html(html)
  
  forex_data <- page_html %>%
    html_element("table") %>%
    html_table()
  
  forex_data <- forex_data %>%
    mutate(scraped_at = Sys.Date())
  
  save_path <- "www/NBR_rates.csv"
  
  if (file.exists(save_path)) {
    old_data <- read_csv(save_path, show_col_types = FALSE)
    combined <- bind_rows(old_data, forex_data) %>%
      distinct()
    write_csv(combined, save_path)
  } else {
    write_csv(forex_data, save_path)
  }
  
  return(forex_data)
}
#=============Scraping + Load Cached File ==========================
save_path <- "www/NBR_rates.csv"
today <- Sys.Date()

if (!file.exists(save_path)) {
  forex_data <- scrape_nbr()
} else {
  existing <- readr::read_csv(save_path, show_col_types = FALSE)
  
  if (!(today %in% existing$scraped_at)) {
    forex_data <- scrape_nbr()
  } else {
    forex_data <- existing
  }
}
#=========================================================================
#Setting Prophet 
Category_Export<- read_xlsx("www/Category_Export_2020.xlsx")
Category_Export<- Category_Export %>%
  pivot_longer(
    cols = -Category,
    names_to = "Quarter",
    values_to = "Export_Value"
  )
#Changing to Date format
Category_Export <- Category_Export %>%
  mutate(ds = case_when(
    str_detect(Quarter, "Q1") ~ paste0(substr(Quarter, 1, 4), "-01-01"),
    str_detect(Quarter, "Q2") ~ paste0(substr(Quarter, 1, 4), "-04-01"),
    str_detect(Quarter, "Q3") ~ paste0(substr(Quarter, 1, 4), "-07-01"),
    str_detect(Quarter, "Q4") ~ paste0(substr(Quarter, 1, 4), "-10-01")
  )) %>%
  mutate(ds = as.POSIXct(ds, tz = "UTC"))
#Using prophet
# Forecast function for one category
forecast_category <- function(df, category_name) {
  df_cat <- df %>%
    filter(Category == category_name) %>%
    select(ds, y = Export_Value) %>%
    mutate(ds = as.POSIXct(ds, tz = "UTC"))  # ✅ enforce tz
  
  m <- prophet(df_cat, n.changepoints = 5)
  future <- make_future_dataframe(m, periods = 6, freq = "quarter")
  forecast <- predict(m, future)
  
  forecast %>%
    select(ds, yhat) %>%
    mutate(
      ds = as.POSIXct(ds, tz = "UTC"),  # ✅ ensure same tz for join/mutate
      Category = category_name,
      type = ifelse(ds <= max(df_cat$ds), "Observed", "Forecast")
    )
}
# Apply to all categories
categories <- unique(Category_Export$Category)

forecast_df <- map_df(categories, ~ forecast_category(Category_Export, .x))
#Selecting top 5
top5_Categories <- Category_Export %>%
  group_by(Category) %>%
  summarise(total = sum(Export_Value)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(Category)

plot_df <- forecast_df %>% filter(Category %in% top5_Categories)
forecast_only <- plot_df %>%
  filter(type == "Forecast")
#Formatting plot_df
plot_df <- plot_df %>%
  mutate(
    ds = as.character(format(ds, "%Y-Q%q")),  # convert quarters to string labels
    yhat = round(yhat, 2),                    # round values to 2 decimals
    type = factor(type, levels = c("Observed", "Forecast"))
  )
#==============================================
ui <- navbarPage(
  # ---------------- Header with Logo + Tabs (right) ----------------
  title = div(
    style = "display: flex; align-items: center; width: 100%;",
    # Logo 
    div(
      img(src = "logo.jpg", height = "60px"),
      style = "margin-right: 120px;margin-top: -5px" 
    )
  ),
  
  windowTitle = "Rwanda Export Dashboard",
  
  # ---------------- Overview ----------------
  tabPanel("Overview",
           # Hero Section full-width ----
           fluidRow(
             column(
               width = 12,
               div(
                 class = "hero", id = "hero-section",
                 style = "
    position: fixed;
    top: 80px; /* same as your header height */
    left: 0;
    width: 100%;
    height: 315px;
    color: white;
    text-align: center;
    display: flex;
    flex-direction: column;
    justify-content: center;
    background-image: url('background1.jpg');
    background-size: cover;
    background-position: center;
    overflow: hidden;
    z-index: 1500; /* below header, above content */
    transition: background-image 1s ease-in-out;
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
  ",
                 div(class = "clouds"),
                 h1("RWANDA EXPORT PLATFORM", 
                    style = "font-size: 48px; font-weight: 900; text-transform: uppercase; color: white; text-shadow: 3px 3px 8px rgba(0,0,0,0.7); margin-bottom: 10px;"),
                 h3("Data for Export Growth",
                    style = "font-size: 22px; font-weight: lighter; color: white; text-shadow: 2px 2px 6px rgba(0,0,0,0.6); margin-top: 0;"),
               )
             )
           ),
           tags$div(style = "height: 370px;"),
           # Info boxes below ----
           br(),
           fluidRow(
             # Total Export Revenue Box
             column(
               width = 4,
               div(style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d); 
                padding: 20px; 
                border-radius: 10px; 
                text-align: center;
                color: white;
                box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                height: 120px;
                display: flex;
                flex-direction: column;
                justify-content: center;",
                   tags$h3("$ 3,041.6 M", style = "margin: 0; font-size: 22px; font-weight: bold;"),
                   tags$p("Export Revenue 2024", style = "margin: 5px 0; font-size: 12px;"),
                   tags$p("31.58% Increase From 2023", style = "margin: 5px 0; font-size: 12px; color: #90EE90;"),
                   tags$hr(style = "border-color: #4CAF50; margin: 10px 0;"),
                   tags$p("Total Export Revenue", style = "margin: 0; font-size: 10px; font-weight: bold;")
               )
             ),
             
             # Export Revenue From Top Exports Box
             column(
               width = 2,
               div(style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d); 
                padding: 20px; 
                border-radius: 10px; 
                text-align: center;
                color: white;
                box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                height: 120px;
                display: flex;
                flex-direction: column;
                justify-content: center;",
                   tags$h3("$ 441.8 M", style = "margin: 0; font-size: 22px; font-weight: bold;"),
                   tags$p("Export Revenue 2024", style = "margin: 5px 0; font-size: 12px;"),
                   tags$p("9.4% Increase From 2023", style = "margin: 5px 0; font-size: 12px; color: #90EE90;"),
                   tags$hr(style = "border-color: #4CAF50; margin: 10px 0;"),
                   tags$p("Principal Exports", style = "margin: 0; font-size: 10px; font-weight: bold;")
               )
             ),
             
             # Ordinary Exports Revenue Box
             column(
               width = 2,
               div(style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d); 
                padding: 20px; 
                border-radius: 10px; 
                text-align: center;
                color: white;
                box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                height: 120px;
                display: flex;
                flex-direction: column;
                justify-content: center;",
                   tags$h3("$ 392.5 M", style = "margin: 0; font-size: 22px; font-weight: bold;"),
                   tags$p("Export Revenue 2024", style = "margin: 5px 0; font-size: 12px;"),
                   tags$p("5.7% Increase From 2023", style = "margin: 5px 0; font-size: 12px; color: #90EE90;"),
                   tags$hr(style = "border-color: #4CAF50; margin: 10px 0;"),
                   tags$p("Ordinary Export Revenue", style = "margin: 0; font-size: 10px; font-weight: bold;")
               )
             ),
             
             # Re_Exports Revenue Box
             column(
               width = 2,
               div(style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d); 
                padding: 20px; 
                border-radius: 10px; 
                text-align: center;
                color: white;
                box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                height: 120px;
                display: flex;
                flex-direction: column;
                justify-content: center;",
                   tags$h3("$ 698.3 M", style = "margin: 0; font-size: 22px; font-weight: bold;"),
                   tags$p("Export Revenue 2024", style = "margin: 5px 0; font-size: 12px;"),
                   tags$p("6.9% Increase From 2023", style = "margin: 5px 0; font-size: 12px; color: #90EE90;"),
                   tags$hr(style = "border-color: #4CAF50; margin: 10px 0;"),
                   tags$p("Re_Export Revenue", style = "margin: 0; font-size: 10px; font-weight: bold;")
               )
             ),
             
             # Adjustment Box
             column(
               width = 2,
               div(style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d); 
                padding: 20px; 
                border-radius: 10px; 
                text-align: center;
                color: white;
                box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                height: 120px;
                display: flex;
                flex-direction: column;
                justify-content: center;",
                   tags$h3("$ 1,509.1 M", style = "margin: 0; font-size: 22px; font-weight: bold;"),
                   tags$p("Export Revenue 2024", style = "margin: 5px 0; font-size: 12px;"),
                   tags$p("70.7% Increase From 2023", style = "margin: 5px 0; font-size: 12px; color: #90EE90;"),
                   tags$hr(style = "border-color: #4CAF50; margin: 10px 0;"),
                   tags$p("Adjustments", style = "margin: 0; font-size: 10px; font-weight: bold;")
               )
             )
            
             
           ),
           #--- Trending News Section ---
           br(),
           fluidRow(
             column(
               width = 12,
               tags$h2("LATEST TRADE NEWS", 
                       style = "text-align:center; font-weight:bold; margin-bottom:20px; color:#021a0e; text-decoration: underline;"),
               
               # News carousel container
               tags$div(
                 id = "news-carousel",
                 class = "news-carousel",
                 
                 # All articles ----
                 tags$div(class = "news-slide",
                          tags$img(src = "article1.jpg", class = "news-img"),
                          tags$a("Read more →", href = "https://www.newtimes.co.rw/article/29759/news/economy/rwandas-trade-deficit-falls-by-125-in-quarter-2", 
                                 target = "_blank", style = "color:#2d5a2d; font-weight:bold;")
                 ),
                 
                 tags$div(class = "news-slide",
                          tags$img(src = "article2.jpg", class = "news-img"),
                          tags$a("Read more →", href = "https://www.newtimes.co.rw/article/30933/news/economy/tanzania-unrest-disrupts-rwanda-bound-cargo", 
                                 target = "_blank", style = "color:#2d5a2d; font-weight:bold;")
                 ),
                 
                 tags$div(class = "news-slide",
                          tags$img(src = "article3.jpg", class = "news-img"),
                          tags$a("Read more →", href = "https://www.newtimes.co.rw/article/29888/news/business/new-rule-who-can-get-paid-in-foreign-currency-in-rwanda", 
                                 target = "_blank", style = "color:#2d5a2d; font-weight:bold;")
                 ),
                 
                 tags$div(class = "news-slide",
                          tags$img(src = "article4.jpg", class = "news-img"),
                          tags$a("Read more →", href = "https://www.newtimes.co.rw/article/30069/opinions/africas-trade-future-in-a-fractured-global-economy", 
                                 target = "_blank", style = "color:#2d5a2d; font-weight:bold;")
                 ),
                 
                 tags$div(class = "news-slide",
                          tags$img(src = "article5.jpg", class = "news-img"),
                          tags$a("Read more →", href = "https://www.newtimes.co.rw/article/30259/video/what-next-for-us-africa-trade-after-expiry-of-agoa", 
                                 target = "_blank", style = "color:#2d5a2d; font-weight:bold;")
                 ),
                 
                 # Navigation buttons
                 tags$button("❮", id = "prev-news", class = "news-btn left"),
                 tags$button("❯", id = "next-news", class = "news-btn right")
               )
             )
           ),
           
           # CSS & JS ----
           tags$style(HTML("
 .news-carousel {
  position: relative;
  overflow: hidden;
  max-width: 1100px;
  margin: auto;
 }
 .news-slide {
  flex: 0 0 32%;
  background: #fff;
  border-radius: 10px;
  margin: 0 1%;
  box-shadow: 0 4px 8px rgba(0,0,0,0.15);
  padding: 10px;
  text-align: left;
  transition: transform 0.3s ease;
 }
 .news-slide:hover {
  transform: translateY(-5px);
 }
 .news-img {
  width: 100%;
  border-radius: 10px;
 }
 .news-wrapper {
  display: flex;
  transition: transform 0.5s ease;
 }
 .news-btn {
  position: absolute;
  top: 45%;
  transform: translateY(-50%);
  background-color: rgba(0,0,0,0.3);
  color: white;
  border: none;
  font-size: 30px;
  padding: 8px 14px;
  cursor: pointer;
  border-radius: 50%;
  z-index: 10;
 }
 .news-btn.left { left: 5px; }
 .news-btn.right { right: 5px; }
 .news-btn:hover { background-color: rgba(0,0,0,0.6); }
 #news-carousel-inner {
  display: flex;
  transition: transform 0.6s ease;
 }
  ")),
           
           tags$script(HTML("
 document.addEventListener('DOMContentLoaded', function() {
  const slides = Array.from(document.querySelectorAll('.news-slide'));
  const container = document.createElement('div');
  container.id = 'news-carousel-inner';
  slides.forEach(s => container.appendChild(s));
  document.getElementById('news-carousel').insertBefore(container, document.getElementById('prev-news'));

  let currentIndex = 0;
  const totalSlides = slides.length;
  const visibleSlides = 3;
  const slideWidth = slides[0].offsetWidth + 20;

  function updateSlides() {
    container.style.transform = `translateX(${-currentIndex * slideWidth}px)`;
  }

  document.getElementById('next-news').addEventListener('click', () => {
    if (currentIndex < totalSlides - visibleSlides) currentIndex++;
    updateSlides();
  });
  document.getElementById('prev-news').addEventListener('click', () => {
    if (currentIndex > 0) currentIndex--;
    updateSlides();
  });
 });
  "))
           ,
           # ----------------- Adding ticker ------------------------------------
           br(),
           fluidRow(
             column(
               width = 12,
               div(class = "ticker-container",
                   div(class = "ticker-text",
                       "This website presents Rwanda’s trade and export data with key sections: Overview, Analytical Dashboard, Currency Insights, and Export Performance — helping users visualize and understand export trends effectively."
                   )
               )
             )
           ),
           # CSS for ticker
           tags$head(
             tags$style(HTML("
    .ticker-container {
      width: 100%;
      overflow: hidden;
      background: linear-gradient(135deg, #1e3c1e, #2d5a2d);
      border-radius: 10px;
      padding: 6px 0;
      margin-top: 8px;
    }
    .ticker-text {
      display: inline-block;
      white-space: nowrap;
      animation: ticker 120s linear infinite;
      font-size: 20px;
      font-weight: normal;
      color: white;
    }
    @keyframes ticker {
      0% { transform: translateX(100vw); }
      100% { transform: translateX(-100%); }
    }
  "))
           ),
           # CSS & JS for rotating background ----
           tags$head(
             tags$style(HTML("
               .hero {
                 position: relative;
                 height: 350px;
                 color: white;
                 text-align: center;
                 display: flex;
                 flex-direction: column;
                 justify-content: center;
                 background-image: url('background1.jpg');
                 background-size: cover;
                 background-position: center;
                 margin: 0; 
                 padding: 0;
                 width: 100%;
                 transition: background-image 1s ease-in-out;
               }
               .hero h1 {
                 font-size: 48px;
                 font-weight: 900;
                 text-transform: uppercase;
                 color: white;
                 text-shadow: 3px 3px 8px rgba(0,0,0,0.7);
                 margin-bottom: 10px;
               }
               .hero h3 {
                 font-size: 22px;
                 font-weight: lighter;
                 text-transform: lowercase;
                 color: white;
                 text-shadow: 2px 2px 6px rgba(0,0,0,0.6);
                 margin-top: 0;
               }
             ")),
             tags$script(HTML("
               var images = ['background1.jpg','background2.jpg'];
               var index = 0;
               setInterval(function(){
                 index = (index + 1) % images.length;
                 document.getElementById('hero-section').style.backgroundImage = 'url(' + images[index] + ')';
               }, 10000); // 10 seconds
             "))
           )
  ),
  
  #-------------------About Us-----------------------
  
  tabPanel(
    "About",
    div(
      style = "
    position: fixed;
    top: 80px; /* same as header height */
    left: 0;
    width: 100%;
    background-image: url('4to.png');
    background-size: cover;
    background-position: center;
    padding: 50px;
    text-align: center;
    overflow: hidden;
    z-index: 1500; /* below header, above charts */
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
  ",
      h2("ABOUT US: NDABAGA TEAM",
         style = "font-weight: bold; color: #0E402D; z-index: 2; position: relative;"),
      
      tags$style("
    .clouds {
      position: absolute;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background: url('clouds.png') repeat-x;
      animation: drift 60s linear infinite;
      opacity: 0.3;
      z-index: 1;
    }
    @keyframes drift {
      0% { background-position: 0 0; }
      100% { background-position: -1000px 0; }
    }
  "),
      div(class = "clouds")
    ),
    
    # Spacer to push the rest of the content below the fixed banner
    tags$div(style = "height: 220px;"),
    
    # ----------- PAGE STYLE -----------
    tags$style(HTML("
    .about-card {
      background: #ffffff;
      border-left: 5px solid #0E402D;   /* Forest green */
      padding: 20px 25px;
      border-radius: 10px;
      box-shadow: 0 3px 8px rgba(0,0,0,0.12);
      margin-bottom: 25px;
    }
    
    .about-title {
      color: #0E402D;
      font-weight: 700;
      margin-bottom: 15px;
    }

    .value-tag {
      background: #0E402D;
      color: white;
      padding: 6px 14px;
      border-radius: 20px;
      margin-right: 8px;
      font-size: 14px;
      font-weight: 600;
      display: inline-block;
    }

    /* Contact Cards */
    .contact-card {
      background: #0E402D;
      color: white;
      padding: 20px;
      border-radius: 12px;
      text-align: center;
      box-shadow: 0 3px 10px rgba(0,0,0,0.18);
    }

    .contact-icon {
      font-size: 28px;
      margin-bottom: 8px;
    }
  ")),
    
    # ----------- PAGE TITLE -----------
    fluidRow(
      
      # LEFT CARD
      column(
        width = 6,
        div(
          class = "about-card",
          h3("Problem Statement", class = "about-title"),
          p("Rwanda’s export sector has made remarkable progress in value, quality, and global visibility over recent years. However, the market remains heavily dominated by traditional commodities such as coffee and tea, limiting broader diversification. Although global trade data is available, it is vast and difficult for policymakers, SMEs, and young innovators to interpret. This creates a gap in identifying emerging high-potential products and forecasting future opportunities—highlighting the need for a clear, data-driven intelligence system to support strategic export growth.")
        )
      ),
      # ----------- VISION + MISSION + VALUES -----------
      column(
        width = 6,
        div(
          class = "about-card",
          
          # Vision + Mission Row
          fluidRow(
            column(
              6,
              h3("Vision", class = "about-title"),
              p("To build a future where Rwanda becomes a globally competitive,
           diversified, and innovation-driven export economy empowered by 
           big data, youth participation, and intelligent forecasting.")
            ),
            column(
              6,
              h3("Mission", class = "about-title"),
              p("To equip Rwanda with a data-driven intelligence system that
           analyzes global trade patterns, predicts export demand using
           machine learning, and delivers actionable insights for
           policymakers, SMEs, and youth entrepreneurs.")
            )
          ),
          
          br(),
          
          # ----- VALUES CENTERED -----
          div(style = "text-align:center;",
              h3("Our Core Values", class = "about-title"),
              div(
                span(class = "value-tag", "Accuracy"),
                span(class = "value-tag", "Innovation"),
                span(class = "value-tag", "Excellence")
              )
          )
        )
      )
    ),
    
    br(),
    tags$h3(
      "REACH OUT TO US", 
      style = "color:#0E402D; font-weight:700; margin-bottom:20px; text-align:center;"
    ),
    
    # ----------- CONTACT BOXES -----------
    fluidRow(
      tags$style(HTML("
  .contact-card {
    background: #ffffff;
    color: #0E402D;
    padding: 20px;
    border-radius: 12px;
    text-align: center;
    box-shadow: 0 3px 10px rgba(0,0,0,0.12);
    border: 1px solid #0E402D33;
  }

  .contact-icon {
    font-size: 28px;
    margin-bottom: 8px;
    color: #0E402D;
  }
")),
      column(
        width = 4,
        div(
          class = "contact-card",
          div(icon("phone"), class = "contact-icon"),
          strong("For inquiries"), br(),
          "+ (250) 783757180"
        )
      ),
      
      column(
        width = 4,
        div(
          class = "contact-card",
          div(icon("envelope"), class = "contact-icon"),
          strong("Email us"), br(),
          tags$a(href = "https://mail.google.com/mail/u/0/?tab=rm&ogbl#inbox?compose=new", "gaellemuhimpundu@gmail.com", target = "_blank",
                 style = "color: #0E402D;"),
        )
      ),
      
      column(
        width = 4,
        div(
          class = "contact-card",
          div(icon("map-marker-alt"), class = "contact-icon"),
          strong("Address"), br(),
          "Kigali, Rwanda"
        )
      )
    )
  ),   
  
  # ---------------- Analysis Dashboard ----------------
  tabPanel("Analytical Dashboard",
           # Dashboard Title with Background Image + Clouds ----
           div(
             style = "
    position: fixed;
    top: 80px; /* same as header height */
    left: 0;
    width: 100%;
    background-image: url('Tea.png');
    background-size: cover;
    background-position: center;
    padding: 50px;
    text-align: center;
    overflow: hidden;
    z-index: 1500; /* below header, above charts */
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
  ",
             h2("INTERACTIVE ANALYTICAL DASHBOARD",
                style = "font-weight: bold; color: white; z-index: 2; position: relative;"),
             
             tags$style("
    .clouds {
      position: absolute;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background: url('clouds.png') repeat-x;
      animation: drift 60s linear infinite;
      opacity: 0.3;
      z-index: 1;
    }
    @keyframes drift {
      0% { background-position: 0 0; }
      100% { background-position: -1000px 0; }
    }
  "),
             div(class = "clouds")
           ),
           
           # Spacer to push the rest of the content below the fixed banner
           tags$div(style = "height: 220px;"),
           
           # Row 1: Big Line Chart ----
           tabItem(
             tabName = "dashboard",
             shiny::fluidRow(
               bs4Dash::box(
                 title = div("TOTAL EXPORT REVENUE 2000 TO 2024",
                             style = "text-align: center; width: 100%;"),
                 width = 12,
                 status = "olive",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 highchartOutput("Ex_lchart"),
                 tags$p("Source: MINECOFIN", style = "text-align: right; font-size: 12px; margin: 5px;")  
               )
             )
           ),
           tags$div(style = "height: 2px; background-color: darkgreen; margin: 10px 0;"),# Separation
           # Row 2: Two Side-by-Side Charts ----
           fluidRow(
             box(
               title = "2024 Export Trend", 
               width = 6,                   
               solidHeader = FALSE,           
               status = "success", 
               height = "300px",    
               selectInput(
                 inputId = "commodity_choice",   
                 label = "Select Commodity",
                 choices = c("Total_Export", "Coffee", "Tea", "Pyrethrum", "Minerals_Combined","Cassiterite","Coltan","Wolfram","Other_Export","Hides & Skin"),  # match your column names
                 selected = "Total_Export"
               ),
               highchartOutput("monthly_export_2024", height = "230px"),
               tags$p("Source: MINECOFIN", style = "text-align: right; font-size: 12px; margin: 5px;")
             ),
             # Box with dropdown + chart
             box(
               title = "Top Destination Countries",
               width = 6,
               solidHeader = FALSE,
               status = "warning",
               height = "350px",
               
               # Inline layout for Year + Quarter selectors
               div(
                 style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px;",
                 selectInput(
                   inputId = "year_choice",
                   label = NULL,
                   choices = c("2023", "2024", "2025"),
                   selected = "2025",
                   width = "50%"
                 ),
                 uiOutput("quarter_selector", width = "50%")
               ),
               
               highchartOutput("top5_countries", height = "250px"),
               tags$p("Source: NISR", 
                      style = "text-align: right; font-size: 12px; margin: 5px;")
             )
           ),
           # Row 3: Two Side-by-Side Charts ----
           fluidRow(
             box(
               title = "2024 Top Export Commodities",
               width = 6,
               solidHeader = FALSE,
               status = "info",
               height = "300px",
               highchartOutput("top_export_treemap", height = "250px"),
               tags$p("Source: NISR", style = "text-align: right; font-size: 12px; margin: 5px") 
             ),
             box(
               title = "Contribution of Trade on GDP",
               width = 6,
               solidHeader = FALSE,
               status = "danger",
               height = "300px",
               highchartOutput("contribution_plot", height = "100%"),  # reduced from 230px
               div(
                 tags$p(tags$b(tags$i("Source: MINECOFIN"))),
                 style = "text-align: right; font-size: 12px; margin: 5px;"
               )
             )
           ),
           tags$div(style = "height: 2px; background-color: darkgreen; margin: 10px 0;"),# Separation
           # Row 4: Map + District Table ----
           fluidRow(
             bs4Dash::box(
               title = div("MAIN SOURCES OF EXPORTS", 
                           style = "text-align: center; width: 100%;"),
               width = 8,
               solidHeader = FALSE,
               status = "primary",
               height = "400px",
               leafletOutput("Export_map", height = 350)
             ),
             bs4Dash::box(
               title = div("DISTRICTS' MAIN EXPORTS", 
                           style = "text-align: center; width: 100%;"),
               width = 4,
               solidHeader = FALSE,
               status = "secondary",
               height = "400px",
               DT::DTOutput("district_table", height = "350px")
             )
           )
  ),
  # ---------------- Data Projection----------------
  tabPanel("Next Opportunity",
           fluidPage(
             # 🔝 Top Banner with Background Image and Transparent Title
             div(
               style = "
    position: fixed;
    top: 80px; /* height of your global header */
    left: 0;
    width: 100%;
    background-image: url('Top_Banner.jpg');
    background-size: cover;
    background-position: center;
    padding: 50px 20px;
    text-align: center;
    overflow: hidden;
    z-index: 1500; /* below header, above content */
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
  ",
               tags$h1(
                 "NEXT BIG EXPORT OPPORTUNITIES",
                 style = "
      background-color: rgba(0, 60, 0, 0.6);  /* darker green overlay */
      color: white;
      padding: 10px 25px;
      border-radius: 8px;
      font-weight: bold;
      font-size: 28px;
      display: inline-block;
      position: relative;
    "
               )
             ),
             
             # Spacer to push the rest of the tab content below the fixed banner
             tags$div(style = 'height: 220px;')
             ,
             # 🌍 Main Section with World Map Background (transparent green overlay)
             div(
               style = "
               background-image: url('world.png'); 
               background-size: cover;
               background-position: center;
               padding: 30px;
               min-height: 600px;
               border-radius: 12px;
               background-color: rgba(0, 60, 0, 0.6); /* transparent green overlay */
               background-blend-mode: overlay;
             ",
               # First Row (two boxes)
               fluidRow(
                 column(
                   width = 6,
                   box(
                     title = tags$div(
                       list(
                         tags$div(
                           "Projected Export Commodities",
                           style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d);
               color: white;
               text-align: center;
               font-weight: bold;
               font-size: 18px;
               width: 100%;
               padding: 10px;
               border-radius: 12px;"
                         )
                       )
                     ),
                     width = 12,
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     status = "success",
                     height = "250px",
                     highchartOutput("top_commodities_chart", height = "100%")
                   ),
                   tags$div(
                     "We feed Prophet Algorithm data from 2020Q1 to make the above predictions.",
                     style = "text-align: center;
             font-size: 14px;
             color: white;
             font-style: italic;
             margin-top: 10px;
             animation: fadeIn 2s ease-in-out;"
                   ),
                   tags$style(HTML("
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(10px); }
      to { opacity: 1; transform: translateY(0); }
    }
  "))
                   
                 ),
                 column(
                   width = 6,
                     box(
                       title = tags$div( "Projected Re-export Growth",
                                         style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d);
                                                color: white;
                                                text-align: center;
                                                font-weight: bold;
                                                font-size: 18px;
                                                position: relative;
                                                width: 100%;
                                                padding: 10px;
                                                border-radius: 12px;
                                       ")
                       ,
                       width = 12,
                       solidHeader = TRUE,
                       collapsible = FALSE,
                       status = "success",
                       height = "250px",
                       highchartOutput("forecast_plot", height = "100%")
                     ),
                   tags$div(
                     "We feed Linear Regression model data from 2023Q1 to make the above predictions.",
                     style = "text-align: center;
             font-size: 14px;
             color: white;
             font-style: italic;
             margin-top: 10px;
             animation: fadeIn 2s ease-in-out;"
                   ),
                   tags$style(HTML("
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(10px); }
      to { opacity: 1; transform: translateY(0); }
    }
  "))
                   )
               ),
                 # Second Row (one centered box)
               fluidRow(
                 column(width = 3), # empty space left
                 column(
                   width = 6,
                   box(
                     title = tags$div("Projected Total Export Revenue",
                                      style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d);
                                                color: white;
                                                text-align: center;
                                                font-weight: bold;
                                                font-size: 18px;
                                                position: relative;
                                                width: 100%;
                                                padding: 10px;
                                                border-radius: 12px;
                                       "),
                     width = 12,
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     status = "warning",
                     height = "250px",
                    highchartOutput("forecast_plot_1", height = "100%")
                   ),
                   tags$div(
                     "We feed Linear Regression model data from 2023Q1 to make the above predictions.",
                     style = "text-align: center;
             font-size: 14px;
             color: white;
             font-style: italic;
             margin-top: 10px;
             animation: fadeIn 2s ease-in-out;"
                   ),
                   tags$style(HTML("
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(10px); }
      to { opacity: 1; transform: translateY(0); }
    }
  "))
                 ),
                 column(width = 3) # empty space right
               )
             )
           )
  ),
  
  #------------------ Market Linker Dashboard-------------------------
  
  tabPanel(
    "Market Linker",
    
    tags$iframe(
      src = "https://market-and-export-finder.vercel.app/",
      style = "width:100%; height:800px; border:none;"
    )
  ),
  
  

  # ---------------- Forex Converter ----------------
  tabPanel(
    "Forex Converter",
    fluidPage(
      # Banner (title + subtitle inside same banner)
      div(
        style = "
    position: fixed;
    top: 80px; /* height of global header */
    left: 0;
    width: 100%;
    background-image: url('forex_bg.png');
    background-size: cover;
    background-position: center;
    padding: 50px 20px;
    text-align: center;
    overflow: hidden;
    z-index: 1500; /* below navbar, above content */
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
  ",
        
        # Main title
        tags$h1(
          "FOREX CONVERTER",
          style = "
      background-color: rgba(0, 60, 0, 0.6);
      color: white;
      padding: 10px 25px;
      border-radius: 8px;
      font-weight: bold;
      font-size: 28px;
      display: inline-block;
      position: relative;
      margin-bottom: 6px;
      cursor: pointer;
    "
        ),
        
        # Subtitle / link
        tags$a(
          href = 'https://www.bnr.rw/exchangeRate', target = '_blank',
          'Powered by BNR Daily Exchange Rates',
          style = "
      color: rgb(0,10,0);
      font-weight: bold;
      font-size: 12px;
      margin-top: 0;
      display: block;
      cursor: pointer;
    "
        )
      ),
      
      # Spacer to prevent overlap with fixed banner
      tags$div(style = 'height: 220px;')
      ,
      
      br(),
      tags$div(style = "height: 2px; background-color: darkgreen; margin: 10px 0;"), # Separation
      
      # Controls: foreign currency + amount
      fluidRow(
        column(
          width = 6,
          selectizeInput(
            inputId = "foreign_currency",
            label = "Select Foreign Currency:",
            choices = unique(forex_data$Currency),
            selected = "USD",
            options = list(placeholder = 'Type to search')
          )
        ),
        column(
          width = 6,
          textInput(
            inputId = "amount_input",
            label = "Enter Amount in Rwf:",
            value = formatC(1000, big.mark = ",", format = "f", digits = 0)
          )
        )
      ),
      
      # Rate type radio buttons
      fluidRow(
        column(
          width = 12,
          br(),
          radioButtons(
            inputId = "rate_type",
            label = "Rate Type:",
            choices = c("Buying Rate", "Selling Rate"),
            selected = "Selling Rate",
            inline = TRUE
          )
        )
      ),
      
      br(),
      
      # Converted output + last update
      fluidRow(
        column(
          width = 12,
          tags$h4("Converted Amount:", align = "center"),
          tags$div(
            textOutput("converted_amount"),
            style = "text-align: center; font-size: 18px; color: darkgreen; font-weight: 600;"
          ),
          tags$br(),
          tags$p(textOutput("last_update"), style = "text-align:center; color: #555;")
        )
      ),
      
      br(),
      
      # Searchable, scrollable table
      tags$head(
        tags$style(HTML("
        table.dataTable thead th {
          background-color: #006400 !important;
          color: white !important;
        }
        table.dataTable td {
          text-align: center;
        }
      ")),
        
        # JS to auto-format the amount input with commas as user types
        tags$script(HTML("
        $(document).on('input', '#amount_input', function(){
          var raw = $(this).val().replace(/,/g,'');
          if(raw === '') return;
          raw = raw.replace(/[^0-9.\\-]/g,'');
          if(raw === '') return;
          var n = Number(raw);
          if(isNaN(n)) return;
          $(this).val(n.toLocaleString('en-US'));
        });
      "))
      ),
      
      fluidRow(
        column(
          width = 12,
          DT::DTOutput("forex_table")
        )
      )
    )
  ),

  
  
  # ---------------- Footer ----------------
  footer <- div(
    style = "background: linear-gradient(135deg, #1e3c1e, #2d5a2d); color: white; padding: 20px; margin-top: 30px;",
    fluidRow(
      # Left: Important Links
      column(
        width = 4,
        div(
          tags$h4("Important Links", style = "font-weight: bold;"),
          tags$ul(
            tags$li(tags$a(href = "https://www.naeb.gov.rw/", target = "_blank", "NAEB", style = "color: white;")),
            tags$li(tags$a(href = "https://rdb.rw/", target = "_blank", "RDB", style = "color: white;"))
          )
        )
      ),
      
      # Middle: Contact Us Section
      column(
        width = 4,
        align = "center",
        tags$img(
          src = "nisr_logo.jpg",
          height = "60px",
          style = "margin-bottom: 10px;"
        ),
        tags$p("NISR BIG DATA HACKATHON 2025",
               style = "margin-top: 5px; font-weight: bold; font-size: 16px;")
      ),
      
      # Right: About Us
      column(
        width = 4,
        tags$h4("Our Team:  NDABAGA", style = "font-weight: bold;"),
        tags$ul(
          tags$li(tags$a(href = "https://www.linkedin.com/in/raissa-ishimwe-69332b241", target = "_blank", "Raissa ISHIMWE", style = "color: white;")),
          tags$li(tags$a(href = "https://www.linkedin.com/in/gaelle-muhimpundu-0551732a3", target = "_blank", "Gaelle MUHIMPUNDU", style = "color: white;"))
        )
      )
    ),
    
    # Add a subtle bottom line
    tags$hr(style = "border-top: 1px solid #ccc; width: 80%; margin: 20px auto; opacity: 0.5;"),
    div(
      style = "text-align: center; font-size: 13px; color: #ffffff; font-weight: bold;",
      "© 2025 Rwanda Export Data Dashboard | All rights reserved."
    )
  ),
  # ---------------- Custom CSS ----------------
  header = tags$head(
    tags$style(HTML("
    /* Fix the top header */
    .navbar {
      position: fixed !important;
      top: 0;
      left: 0;
      right: 0;
      z-index: 2000; /* above everything else */
      background-color: white; /* ensure solid background */
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    }
    
    /* Add padding to body to avoid overlap */
    body {
      padding-top: 80px; /* match your navbar height */
    }

    /* Existing header adjustments */
    .navbar-nav {
      margin-right: auto !important;
    }
    .navbar {
      min-height: 80px;
    }
    .navbar-nav > li > a {
      line-height: 50px;
      padding-top: 15px;
      padding-bottom: 15px;
    }
  "))
  )
)

server <- function(input, output, session) {
  output$Ex_lchart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_add_series(
        data = Export_Data,
        type = "line",
        hcaes(x = years, y = total_exports_millions_us_dollars),
        name = "Export Income (M USD)",
        color = "#0073C2FF"
      ) %>%
      hc_xAxis(
        title = list(text = "Year")
      ) %>%
      hc_yAxis(
        title = list(text = "Amount (Millions USD)"),
        labels = list(format = "{value:,.0f}") # comma formatting
      ) %>%
      hc_tooltip(
        pointFormat = "Year: {point.x}<br>Exports: {point.y:,.2f} M USD"
      )
  })
  # Render Leaflet Map
  output$Export_map <- renderLeaflet({
    pal <- colorFactor(
      palette = c("darkgray","brown","#411900","white","darkgreen"),
      domain = rwanda_map$product
    )
    
    leaflet(rwanda_map, options = leafletOptions(
      zoomControl = FALSE,
      dragging = FALSE,
      scrollWheelZoom = FALSE,
      doubleClickZoom = FALSE
    )) %>%
      addTiles(group = "Background") %>%
      addPolygons(
        fillColor = ~pal(product),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          bringToFront = TRUE
        ),
        label = ~paste0(NAME_2, ": ", product),
        popup = ~paste("<b>District:</b>", NAME_2, "<br>",
                       "<b>Main Export:</b>", product)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~product,
                title = "Export Product") %>%
      setView(lng = 30.06, lat = -1.94, zoom = 8)
  })
  #DISTRICT TABLE===============================================================
  output$district_table <- DT::renderDT({
    rwanda_map %>%
      sf::st_drop_geometry() %>%   # drops geometry column
      dplyr::select(District = NAME_2, `Main Export` = product)
  },
  options = list(
    pageLength = 5,      # show 5 rows by default
    lengthMenu = c(5, 10, 20), # allow switching between 5, 10, 20
    autoWidth = TRUE,
    dom = 'ftip'   # f = search, t = table, i = info, p = pagination
  ),
  rownames = FALSE
  )
  #TOP COUNTRIES================================================================
  # Dynamic quarter options based on year selection
  observeEvent(input$year_choice, {
    quarters <- switch(input$year_choice,
                       "2023" = c("2023Q1", "2023Q2", "2023Q3", "2023Q4"),
                       "2024" = c("2024Q1", "2024Q2", "2024Q3", "2024Q4", "2024_TOTAL"),
                       "2025" = c("2025Q1", "2025Q2")
    )
    
    updateSelectInput(
      session,
      "quarter_choice",
      choices = quarters,
      selected = ifelse(input$year_choice == "2025", "2025Q2", quarters[1])
    )
  })
  
  # Render quarter dropdown UI
  output$quarter_selector <- renderUI({
    selectInput(
      inputId = "quarter_choice",
      label = NULL,
      choices = c("2025Q1", "2025Q2"),  # default initial options
      selected = "2025Q2"
    )
  })
  
  # Render highchart
  output$top5_countries <- renderHighchart({
    req(input$quarter_choice)
    
    df_top5 <- Countries %>%
      dplyr::select(Country, !!sym(input$quarter_choice)) %>%
      dplyr::arrange(desc(!!sym(input$quarter_choice))) %>%
      dplyr::slice_head(n = 5)
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_add_series(
        data = df_top5[[input$quarter_choice]],
        name = "Exports (Million USD)",
        color = "#0073C2FF"
      ) %>%
      hc_xAxis(categories = df_top5$Country) %>%
      hc_yAxis(title = list(text = "Exports (Million USD)")) %>%
      hc_plotOptions(
        bar = list(
          dataLabels = list(enabled = TRUE, format = "{point.y:,.0f}")
        )
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.y:,.2f}</b> Million USD")
  })
  #TOP COMMODITY ============================================================
  output$top_commodities_chart <- renderHighchart({
  req(plot_df)

  highchart() %>%
    hc_chart(type = "line", backgroundColor = "#afdec4") %>%
    hc_title(text = "Forecasted and Observed Export Values by Category",
             style = list(color = "#003300", fontSize = "18px", fontWeight = "bold")) %>%
    hc_xAxis(
      categories = unique(plot_df$ds),
      title = list(text = "Quarter", style = list(color = "#003300", fontWeight = "bold")),
      labels = list(style = list(color = "#003300"))
    ) %>%
    hc_yAxis(
      title = list(text = "Export Value (Million USD)", style = list(color = "#003300", fontWeight = "bold")),
      labels = list(style = list(color = "#003300"))
    ) %>%
    hc_plotOptions(
      series = list(
        lineWidth = 2,
        marker = list(enabled = TRUE, radius = 3),
        animation = list(duration = 1000)
      )
    ) %>%
    hc_tooltip(
      backgroundColor = "#f4fff8",
      borderColor = "#003300",
      style = list(color = "#003300", fontWeight = "bold"),
      pointFormat = "<b>{series.name}</b><br>Quarter: {point.category}<br>Value: {point.y:.2f} Million USD"
    ) %>%
    hc_add_theme(hc_theme_flat())

  # Loop over categories to add observed and forecast lines
  categories <- unique(plot_df$Category)
  chart <- highchart() %>%
    hc_chart(type = "line", backgroundColor = "#afdec4") %>%
    hc_title(text = "Forecasted and Observed Export Values by Category",
             style = list(color = "#003300", fontSize = "18px", fontWeight = "bold")) %>%
    hc_xAxis(
      categories = unique(plot_df$ds),
      title = list(text = "Quarter", style = list(color = "#003300", fontWeight = "bold")),
      labels = list(style = list(color = "#003300"))
    ) %>%
    hc_yAxis(
      title = list(text = "Export Value (Million USD)", style = list(color = "#003300", fontWeight = "bold")),
      labels = list(style = list(color = "#003300"))
    ) %>%
    hc_tooltip(
      backgroundColor = "#f4fff8",
      borderColor = "#003300",
      style = list(color = "#003300", fontWeight = "bold"),
      pointFormat = "<b>{series.name}</b><br>Quarter: {point.category}<br>Value: {point.y:.2f} Million USD"
    ) %>%
    hc_plotOptions(
      series = list(
        lineWidth = 2,
        marker = list(enabled = TRUE, radius = 3),
        animation = list(duration = 1000)
      )
    ) %>%
    hc_add_theme(hc_theme_flat())

  for (cat in categories) {
    for (tp in c("Observed", "Forecast")) {
      sub_df <- plot_df %>% filter(Category == cat, type == tp)
      if (nrow(sub_df) > 0) {
        chart <- chart %>%
          hc_add_series(
            data = sub_df$yhat,
            name = paste(cat, "-", tp),
            type = "line",
            dashStyle = ifelse(tp == "Forecast", "ShortDash", "Solid"),
            color = custom_colors[[cat]]
          )
      }
    }
  }

  chart
})

      #====================Monthly==== 
  output$monthly_export_2024 <- renderHighchart({
    req(input$commodity_choice)  # ensures input is available
    
    selected_data <- monthly_exports[[input$commodity_choice]]
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_add_series(
        data = selected_data,
        name = paste(input$commodity_choice, "(Million USD)"),
        color = "#2E8B57"
      ) %>%
      hc_xAxis(
        categories = monthly_exports$Month,
        title = list(text = "Month")
      ) %>%
      hc_yAxis(
        title = list(text = paste(input$commodity_choice, "(Million $)"))
      ) %>%
      hc_plotOptions(
        line = list(
          dataLabels = list(enabled = FALSE, format = "{point.y:,.2f}")
        )
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.y:,.2f}</b> Million USD")
  })
     #==========TOP_COMMODITIES=============================================
  output$top_export_treemap <- renderHighchart({
    # Define custom colors
    custom_colors <- c("darkgreen", "darkgrey", "grey", "#38211E", "#92340b")
    
    # Prepare data for treemap
    hc_data <- top_commodities %>%
      mutate(
        name = Commodity,
        value = `2024_Values`,
        color = rep(custom_colors, length.out = n())
      ) %>%
      select(name, value, color)
    
    highchart() %>%
      hc_chart(type = "treemap") %>%
      hc_add_series(
        data = list_parse(hc_data),
        type = "treemap",
        layoutAlgorithm = "squarified",
        allowDrillToNode = TRUE,
        dataLabels = list(enabled = TRUE, format = "{point.name}")
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.value:,.2f} Million USD")
  })
  #==============PIE Chart of Continents========================================
  output$contribution_plot <- renderHighchart({
    
    df <- data.frame(
      Year = c("2019/20","2020/21","2021/22","2022/23","2023/24"),
      Contribution = c(0.0836, 0.0856, 0.10, 0.100, 0.111)
    )
    
    forest_green <- "#0073C2FF"
    
    # Trend line values (linear regression)
    trend_vals <- fitted(lm(Contribution ~ seq_along(Contribution), df))
    
    highchart() %>%
      hc_xAxis(
        categories = df$Year,
        title = list(text = "Year")
      ) %>%
      hc_yAxis(
        title = list(text = "Contribution to GDP (%)"),
        labels = list(format = "{value}%")
      ) %>%
      
      # Bars in forest green
      hc_add_series(
        name = "Contribution to GDP",
        data = round(df$Contribution * 100, 2),   # convert to percentages
        type = "column",
        color = forest_green
      ) %>%
      
      # Trend line
      hc_add_series(
        name = "Trend",
        data = round(trend_vals * 100, 2),        # convert trend to %
        type = "line",
        color = "#1b8552",
        lineWidth = 3
      ) %>%
      
      
      hc_tooltip(
        pointFormat = "<b>{point.y}%</b>"
      ) %>%
      
      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            format = "{y}%",
            style = list(fontSize = "12px", fontWeight = "bold")
          )
        )
      ) %>%
      
      hc_exporting(enabled = TRUE)
  })
  # FOREX Converter
  output$forex_table <- DT::renderDT({
    forex_data %>%
      dplyr::select(Currency, !!rlang::sym(input$rate_type)) %>%
      dplyr::rename(Rate = !!rlang::sym(input$rate_type)) %>%
      DT::datatable(
        rownames = FALSE,
        filter = "top",   # adds search boxes
        options = list(
          pageLength = 5,
          lengthChange = FALSE,   # hide selector
          autoWidth = TRUE,
          scrollY = "300px",      # scrollable height
          scrollCollapse = TRUE,
          paging = TRUE,
          dom = 'tip'             # only table, input, pagination
        )
      ) %>%
      DT::formatStyle(
        columns = c("Currency", "Rate"),
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = names(.),   # apply to header
        target = "row",
        backgroundColor = DT::styleEqual(names(forex_data), rep("#06402b", length(names(forex_data)))),
        color = "white"
      )
  })
  
  # --- safe rate type reactive ----
  safe_rate_col <- reactive({
    rt <- input$rate_type
    if (is.null(rt) || rt == "") rt <- "Selling Rate"
    rt
  })
  
  # --- table for display ----
  forex_display <- reactive({
    req(forex_data)
    col <- safe_rate_col()
    
    if (!col %in% names(forex_data)) {
      stop(paste0("Rate column '", col, "' not found in forex_data."))
    }
    
    forex_data %>%
      dplyr::select(Currency, Rate = !!rlang::sym(col))
  })
  
  # --- DT table output ----
  output$forex_table <- DT::renderDT({
    dat <- forex_display()
    
    DT::datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 5,
        lengthChange = FALSE,
        autoWidth = TRUE,
        scrollY = "220px",
        scrollCollapse = TRUE,
        paging = TRUE,
        dom = 'tip'
      )
    )
  })
  
  # --- last update text ----
  output$last_update <- renderText({
    paste0("Last update 8AM (", format(Sys.Date(), "%B %d, %Y"), ")")
  })
  
  # --- conversion: RWF → Foreign (FIXED) ----
  output$converted_amount <- renderText({
    req(input$foreign_currency, input$amount_input, safe_rate_col())
    
    amt_raw <- gsub(",", "", input$amount_input)
    amt_num <- suppressWarnings(as.numeric(amt_raw))
    if (is.na(amt_num)) return("Please enter a valid number.")
    
    rate_col_name <- safe_rate_col()
    
    # Use distinct() to ensure only one row per currency
    row <- forex_data %>% 
      dplyr::filter(Currency == input$foreign_currency) %>%
      dplyr::distinct(Currency, .keep_all = TRUE)
    
    if (nrow(row) == 0) {
      return(paste0("Currency '", input$foreign_currency, "' not found."))
    }
    
    rate <- as.numeric(row[[rate_col_name]])
    if (is.na(rate)) return("Rate not available for chosen currency.")
    
    # FIXED: Convert RWF to foreign currency (RWF / rate = foreign currency)
    # If 1$ = 1450 RWF, then 50,000 RWF = 50000 / 1450 = 34.5$
    converted <- amt_num / rate
    
    paste0(
      formatC(amt_num, format = "f", big.mark = ",", digits = 2),
      " RWF = ",
      formatC(round(converted, 2), format = "f", big.mark = ",", digits = 2),
      " ", input$foreign_currency
    )
  })
  # Re_Export forecast
  output$forecast_plot <- renderHighchart({
    req(Re_Export_Forecast)
    quarters <- unique(Re_Export_Forecast$Quarter)
    
    observed <- Re_Export_Forecast %>%
      dplyr::filter(type == "Observed")
    
    forecast <- Re_Export_Forecast %>%
      dplyr::filter(type == "Forecast")
    
    highchart() %>%
      hc_chart(backgroundColor = "#afdec4") %>%
      hc_xAxis(
        categories = quarters,
        labels = list(rotation = 45, style = list(color = "#003300"))
      ) %>%
      hc_yAxis(
        title = list(text = "Export Value (Million USD)", style = list(color = "#003300")),
        gridLineWidth = 0,
        labels = list(style = list(color = "#003300"))
      ) %>%
      # Observed series
      hc_add_series(
        name = "Observed",
        data = purrr::map_dbl(quarters, ~ {
          val <- observed$Exports[observed$Quarter == .x]
          if (length(val) == 0) NA else val
        }),
        type = "line",
        color = "darkgreen",
        marker = list(radius = 4, symbol = "circle"),
        lineWidth = 2
      ) %>%
      # Forecast series
      hc_add_series(
        name = "Forecast",
        data = purrr::map_dbl(quarters, ~ {
          val <- forecast$Exports[forecast$Quarter == .x]
          if (length(val) == 0) NA else val
        }),
        type = "line",
        color = "#1071cc",
        marker = list(radius = 4, symbol = "circle"),
        lineWidth = 2,
        dashStyle = "ShortDot"
      ) %>%
      hc_legend(
        align = "right",
        verticalAlign = "middle",
        layout = "vertical",
        backgroundColor = "#afdec4",
        itemStyle = list(color = "#003300", fontWeight = "bold")
      ) %>%
      hc_tooltip(
        backgroundColor = "#f4fff8",
        borderColor = "#003300",
        style = list(color = "#003300", fontWeight = "bold"),
        pointFormat = "<b>{series.name}</b>: {point.y:.2f} Million USD<br/>Quarter: {point.category}"
      ) %>%
      hc_plotOptions(series = list(animation = list(duration = 1000)))
  })
  #Export_Income forecast
  output$forecast_plot_1 <- renderHighchart({
    req(Export_Income_Forecast)
    
    # Keep your already ordered quarters
    quarters <- unique(Export_Income_Forecast$Quarter)
    
    observed <- Export_Income_Forecast %>%
      dplyr::filter(type == "Observed")
    
    forecast <- Export_Income_Forecast %>%
      dplyr::filter(type == "Forecast")
    
    highchart() %>%
      hc_chart(backgroundColor = "#afdec4") %>%
      hc_xAxis(
        categories = quarters,
        labels = list(rotation = 45, style = list(color = "#003300"))
      ) %>%
      hc_yAxis(
        title = list(text = "Export Value (Million USD)", style = list(color = "#003300")),
        gridLineWidth = 0,
        labels = list(style = list(color = "#003300"))
      ) %>%
      # Observed series
      hc_add_series(
        name = "Observed",
        data = purrr::map_dbl(quarters, ~ {
          val <- observed$Exports[observed$Quarter == .x]
          if (length(val) == 0) NA else val
        }),
        type = "line",
        color = "darkgreen",
        marker = list(radius = 4, symbol = "circle"),
        lineWidth = 2
      ) %>%
      # Forecast series
      hc_add_series(
        name = "Forecast",
        data = purrr::map_dbl(quarters, ~ {
          val <- forecast$Exports[forecast$Quarter == .x]
          if (length(val) == 0) NA else val
        }),
        type = "line",
        color = "#1071cc",
        marker = list(radius = 4, symbol = "circle"),
        lineWidth = 2,
        dashStyle = "ShortDot"
      ) %>%
      hc_legend(
        align = "right",
        verticalAlign = "middle",
        layout = "vertical",
        backgroundColor = "#afdec4",
        itemStyle = list(color = "#003300", fontWeight = "bold")
      ) %>%
      hc_tooltip(
        backgroundColor = "#f4fff8",
        borderColor = "#003300",
        style = list(color = "#003300", fontWeight = "bold"),
        pointFormat = "<b>{series.name}</b>: {point.y:.2f} Million USD<br/>Quarter: {point.category}"
      ) %>%
      hc_plotOptions(series = list(animation = list(duration = 1000)))
  })
  # Projected Top Export Commoditie with highcharter
  output$top_commodities_chart <- renderHighchart({
    req(forecast_only)
    
    all_categories <- unique(forecast_only$Category)
    default_palette <- RColorBrewer::brewer.pal(8, "Dark2")
    
    # Map each category to a color from the palette
    color_map <- setNames(
      default_palette[seq_along(all_categories) %% length(default_palette) + 1],
      all_categories
    )
    
    chart <- highchart() %>%
      hc_chart(type = "line", backgroundColor = "#afdec4") %>%
      hc_title(text = NULL) %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(
        categories = unique(forecast_only$ds),
        gridLineWidth = 0,
        title = list(text = "Quarter", style = list(color = "#003300", fontWeight = "bold")),
        labels = list(style = list(color = "#003300"))
      ) %>%
      hc_yAxis(
        gridLineWidth = 0,
        title = list(text = "Export Value (Million $)", style = list(color = "#003300", fontWeight = "bold")),
        labels = list(style = list(color = "#003300"))
      ) %>%
      hc_tooltip(
        backgroundColor = "#f4fff8",
        borderColor = "#003300",
        style = list(color = "#003300", fontWeight = "bold"),
        pointFormat = "<b>{series.name}</b><br>Quarter: {point.category}<br>Value: {point.y:.2f} Million USD"
      ) %>%
      hc_plotOptions(
        series = list(
          lineWidth = 2,
          marker = list(enabled = TRUE, radius = 3),
          animation = list(duration = 1000)
        )
      ) %>%
      hc_add_theme(hc_theme_flat())
    
    # Add each forecast series
    for (cat in all_categories) {
      sub_df <- forecast_only %>% filter(Category == cat)
      if (nrow(sub_df) > 0) {
        chart <- chart %>%
          hc_add_series(
            data = sub_df$yhat,
            name = cat,
            type = "line",
            dashStyle = "ShortDash",
            color = color_map[[cat]]
          )
      }
    }
    
    chart
  })
}

shinyApp(ui, server)