# Global Health Analytics Dashboard
# Student: Alve Reza Sazim
# Course: Data Vis with R
# Assignment 3

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(leaflet)
library(sf)
library(maps)
library(mapdata)
library(viridis)
library(shinydashboard)
library(shinyWidgets)

SCREEN_WIDTH <- 1920
SCREEN_HEIGHT <- 1000

# Load and process data
data <- read.csv("lifeexpectancy_processed.csv", stringsAsFactors = FALSE)

# Get world map data
world_map <- map_data("world")

# Country name mapping for consistency
country_mapping <- c(
  "United States of America" = "USA",
  "United Kingdom of Great Britain and Northern Ireland" = "UK",
  "Russian Federation" = "Russia",
  "Republic of Korea" = "South Korea",
  "Viet Nam" = "Vietnam",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Iran (Islamic Republic of)" = "Iran",
  "United Republic of Tanzania" = "Tanzania",
  "Syrian Arab Republic" = "Syria",
  "Democratic People's Republic of Korea" = "North Korea",
  "Congo" = "Republic of the Congo",
  "Côte d'Ivoire" = "Ivory Coast",
  "Republic of Moldova" = "Moldova",
  "Czechia" = "Czech Republic",
  "Lao People's Democratic Republic" = "Laos",
  "The former Yugoslav republic of Macedonia" = "Macedonia",
  "Trinidad and Tobago" = "Trinidad",
  "Brunei Darussalam" = "Brunei",
  "Cabo Verde" = "Cape Verde"
)

# Prepare data for analysis
country_health_stats <- data %>%
  group_by(country) %>%
  summarise(
    lifeexpectancy = round(mean(lifeexpectancy, na.rm = TRUE), 1),
    adultmortality = round(mean(adultmortality, na.rm = TRUE), 1),
    infant_mortality_rate = round(mean(infant_mortality_rate, na.rm = TRUE), 1),
    bmi = round(mean(bmi, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  mutate(region = ifelse(country %in% names(country_mapping), 
                         country_mapping[country], 
                         country))

# Merge with map data
map_data_merged <- world_map %>%
  left_join(country_health_stats, by = "region")

# Time series data
health_intervention_data <- data %>%
  group_by(year) %>%
  summarise(
    hepatitisb = round(mean(hepatitisb, na.rm = TRUE), 1),
    measles = round(mean(measles, na.rm = TRUE), 0),
    polio = round(mean(polio, na.rm = TRUE), 1),
    diphtheria = round(mean(diphtheria, na.rm = TRUE), 1),
    hivaids = round(mean(hivaids, na.rm = TRUE), 2),
    .groups = 'drop'
  )

development_comparison <- data %>%
  group_by(year, status) %>%
  summarise(avg_life_exp = round(mean(lifeexpectancy, na.rm = TRUE), 1), .groups = 'drop')

bmi_health_analysis <- data %>%
  filter(!is.na(bmi_category) & bmi_category != "Unknown" & bmi_category != "") %>%
  group_by(year, bmi_category) %>%
  summarise(
    avg_life_exp = round(mean(lifeexpectancy, na.rm = TRUE), 1),
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 5)

# UI with navbar
ui <- navbarPage(
  title = "Global Health Analytics",
  id = "navbar",
  theme = NULL,
  
  # Custom CSS
  tags$head(
    tags$style(HTML(paste0("
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        overflow: hidden;
        background: #f5f5f5;
      }
      
      /* Navbar styling */
      .navbar {
        height: 50px !important;
        min-height: 50px !important;
        background-color: #3c8dbc !important;
        border: none !important;
        margin-bottom: 0 !important;
      }
      
      .navbar-brand {
        height: 50px !important;
        line-height: 50px !important;
        padding: 0 15px !important;
        font-size: 18px !important;
        font-weight: 600 !important;
        color: white !important;
      }
      
      .navbar-nav > li > a {
        height: 50px !important;
        line-height: 50px !important;
        padding: 0 15px !important;
        color: white !important;
        font-size: 14px !important;
        transition: background-color 0.3s ease;
      }
      
      .navbar-nav > li > a:hover,
      .navbar-nav > li > a:focus {
        background-color: rgba(255,255,255,0.1) !important;
        color: white !important;
      }
      
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover,
      .navbar-nav > .active > a:focus {
        background-color: rgba(255,255,255,0.2) !important;
        color: white !important;
      }
      
      /* Content area styling */
      .tab-content {
        height: ", SCREEN_HEIGHT - 50, "px !important;
        overflow: hidden !important;
      }
      
      .tab-pane {
        height: 100% !important;
        overflow-y: auto !important;
        overflow-x: hidden !important;
        padding: 15px !important;
      }
      
      /* Typography */
      h2 {
        font-size: 26px;
        font-weight: 300;
        margin-bottom: 8px;
        color: #333;
      }
      
      h3 {
        font-size: 18px;
        font-weight: 400;
        margin: 15px 0 10px 0;
        color: #555;
      }
      
      p {
        font-size: 13px;
        line-height: 1.5;
        color: #666;
        margin-bottom: 10px;
      }
      
      strong {
        font-weight: 600;
        color: #444;
      }
      
      /* Page container */
      .page-container {
        height: 100%;
        display: flex;
        flex-direction: column;
        padding: 0 15px;
        overflow-y: auto;
        overflow-x: hidden;
      }

      .viz-container {
        flex: 1;
        height: 100%;
        margin: 0;
        padding: 10px;
        background: white;
        border-radius: 6px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        transition: all 0.3s ease;
        overflow: hidden;
      }

      /* DataTable styling */
      .dataTables_wrapper {
        font-size: 13px;
        height: 100%;
        overflow: hidden;
      }
      
      .dataTables_scrollBody {
        border: 1px solid #e0e0e0;
        overflow-y: auto !important;
        overflow-x: auto !important;
      }
      
      table.dataTable thead th {
        background: #fafafa;
        font-weight: 600;
        text-align: left;
        padding: 10px !important;
      }
      
      /* Control groups */
      .control-group {
        display: flex;
        gap: 20px;
        align-items: center;
        margin-bottom: 15px;
      }
      
      .control-group label {
        font-weight: 500;
        color: #555;
        min-width: 120px;
      }
      
      .selectize-control {
        flex: 1;
        max-width: 300px;
      }
      
      /* Scrollbar styling */
      ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
    ")))
  ),
  
  # Page 1: Data Exploration
  tabPanel(
    "Exploring The Data",
    value = "exploration",
    div(class = "page-container", style = "display: flex; flex-direction: row; gap: 30px; align-items: center; justify-content: space-between;",
        # Left Column - Text Content
        div(style = "width: 40%; padding: 10px; overflow-y: auto; max-height: 100%;",
            h3("A few days ago..."),
            p(em("I got a call from home. My grandmother was sick. Really sick. She is about 75 years old and quite weak because of diabetes, bone decay and all the hard jobs she has done when she was younger. However, when I was in Hong Kong on my transit, I got a phone call that she is now in the ICU under life support. By the time I arrived, her condition was critical. She came back this time. This whole scenario deeply moved me. That's why the question I am looking for the answer to is...")),
            
            h2("What Determines How Long We Live?"),
            p("Due to the crazy advancement of medical science, the life expectancy around the whole world is more than ever. People are living longer and healthier lives. All of the epidemics and diseases are at the lowest in history. Diseases once considered fatal are now treatable or preventable, and healthcare access has expanded across both developed and developing nations. However, despite the overall progress, disparities still exist. The aim of this dashboard is to get to know what are the things that have been improved and also the main causes and reasons of different factors contributing to the improvement. The importance of this exploration to get to know about the key factors of life expectancy and the treatment or preventive measures we are taking, is paramount."),
            
            h3("Understanding the Data Interface"),
            p("Understanding the health outcomes is really important. The right side dataset does not only represent numbers. It represents countless efforts of a lot of doctors, nurses, scientists etc. The dataset on the right has different columns with different data in it. The dataset is sourced from WHO and UN. This dataset has been used to make the next page(s) visualizations and to tell the whole story. Not all the columns are shown there. Only the most important ones. However the dataset can be filtered with different factors to have a proper look."),
            
            
            p("With filters for country, development status, time range, and life expectancy bands, users can isolate specific factors and compare across regions. This targeted analysis helps uncover correlations and disparities often hidden in large datasets."),
            
            p("The visualized metrics include:"),
            tags$ul(style = "font-size: 13px; line-height: 1.5; margin-left: 20px; columns: 2; -webkit-columns: 2; -moz-columns: 2;",
                    tags$li("Life Expectancy"),
                    tags$li("Adult & Infant Mortality"),
                    tags$li("Education Levels"),
                    tags$li("GDP per Capita"),
                    tags$li("Alcohol Use"),
                    tags$li("BMI"),
                    tags$li("Vaccination Coverage"),
                    tags$li("Measles Incidence")
            ),
            
            p("Whether one is investigating a country's progress or contrasting multiple regions, this interface offers the flexibility needed for meaningful analysis and gives a proper idea of what the condition was of that country throughout the years. This interface enables users to explore these indicators over time, revealing patterns of improvement and areas needing urgent attention. Whether one is investigating the aftermath of a healthcare policy, tracking progress towards the Sustainable Development Goals, or assessing the impact of education on mortality, this dashboard offers an evidence-based view. It bridges the gap between data and action, giving policymakers, researchers, and concerned citizens the tools to advocate for meaningful change. Ultimately, understanding how and why we live longer, healthier lives is not just a scientific quest—it's a moral imperative. It reminds us that behind every data point lies a real person, a real family, and a real story worth telling.")
        ),
        
        # Right Column - Data Table
        div(style = "width: 55%; display: flex; align-items: center; justify-content: center;",
            div(class = "viz-container", style = "width: 100%; overflow-x: auto;",
                h3("Data Exploration Interface"),
                p("Filter and explore the raw dataset by country, development status, and key health indicators:"),
                fluidRow(style = "margin-bottom: 5px;",
                         column(3, selectInput("filter_country", "Country:", 
                                               choices = c("All", unique(data$country)), 
                                               selected = "All", width = "100%")),
                         column(3, selectInput("filter_status", "Development Status:", 
                                               choices = c("All", "Developed", "Developing"), 
                                               selected = "All", width = "100%"))
                ),
                
                DTOutput("data_table", height = "450px")
            )
        )
    )
  ),
  
  # Page 2: World Map
  tabPanel(
    "Global Perspective",
    value = "map",
    div(class = "page-container",
        h2("Health Outcomes Are Not Evenly Distributed"),
        
        fluidRow(
          column(
            width = 5,
            p("This map-based visualization presents a global perspective on four key health indicators: Life Expectancy, Adult Mortality, Child Mortality, and Average BMI. The goal is to show how geography, wealth, and infrastructure play a vital role in human longevity."),
            
            div(style = "margin: 15px 0;",
                selectInput("map_indicator", "Select Health Indicator:",
                            choices = list(
                              "Life Expectancy (Years)" = "lifeexpectancy",
                              "Adult Mortality (per 1000)" = "adultmortality",
                              "Child Mortality (per 1000)" = "infant_mortality_rate",
                              "Average BMI" = "bmi"
                            ),
                            selected = "lifeexpectancy", width = "100%")
            ),
            
            h3("Visualization Explanation"),
            p("Each map uses a choropleth color scale, where darker shades represent higher values. Toggle between the four metrics to compare regional patterns."),
            p(em("This visual representation helps uncover the intersection of public health and geopolitics. Nations with stable governance, robust healthcare infrastructure, and higher investment in education often score better in life expectancy and BMI metrics. Meanwhile, regions affected by conflict or economic disparity tend to experience higher mortality rates.")),
            p(em("The stark differences highlighted in this visualization align closely with Sustainable Development Goal 3 (Good Health and Well-being), emphasizing the need for equitable access to health services globally. This map not only reveals disparities but also serves as a call for targeted interventions.")),
            
            h3("Observations and Patterns"),
            tags$ul(style = "font-size: 13px; line-height: 1.5;",
                    tags$li(tags$strong("Life Expectancy:"), " Life expectancy tends to be highest in regions such as Western Europe, Canada, Australia, and parts of East Asia. In these areas, people frequently live beyond 80 years due to robust healthcare systems, widespread vaccination, and access to clean environments and quality education. Conversely, in regions such as Central and Sub-Saharan Africa, life expectancy remains significantly lower. This disparity often stems from limited access to healthcare, poor sanitation, malnutrition, and the impact of political instability. The data highlights that while medical advancements are widespread, the benefits are not equally distributed across the globe."),
                    tags$li(tags$strong("Adult Mortality:"), " Adult mortality rates are noticeably higher in countries experiencing war, poverty, or fragile healthcare infrastructures. In many low-income regions, adults face higher risks of death from treatable conditions due to lack of access to emergency medical services or early diagnostics. In contrast, developed countries show significantly lower adult mortality, with most deaths occurring at older ages and often due to chronic conditions. These differences emphasize the critical importance of access to timely and affordable healthcare in determining adult survival outcomes."),
                    tags$li(tags$strong("Child Mortality:"), " High child mortality rates continue to be a challenge in economically disadvantaged areas. The data shows that regions with limited access to maternal health services, clean water, and immunizations report a much higher number of infant deaths. On the other hand, countries with strong primary healthcare networks have achieved significant reductions in child mortality over the past decade. This illustrates the direct impact that basic public health interventions—such as vaccinations and clean delivery practices—can have on saving young lives."),
                    tags$li(tags$strong("BMI:"), " The global distribution of Body Mass Index (BMI) reveals a growing divide. In wealthier nations, average BMI levels tend to be higher, often reflecting dietary patterns high in processed foods and reduced physical activity. Emerging economies are beginning to follow similar trends, especially in urban settings. Interestingly, some developing countries face a dual burden: undernutrition in rural areas and rising obesity in cities. This duality underscores how nutrition-related health outcomes are shaped not only by income, but also by education, food access, and urbanization.")
            ),
            
            p("These maps highlight both progress and persistent inequality. Even as global life expectancy increases, many countries are still burdened by preventable deaths.")
          ),
          
          column(
            width = 7,
            div(class = "viz-container", style = "height: 100%; min-height: 600px; padding: 10px;",
                plotlyOutput("world_map", height = "550px")
            )
          )
        )
    )
  ),
  
  # Page 3: Temporal Trends
  tabPanel(
    "Present Day Comparison",
    value = "trends",
    div(class = "page-container",
        h2("A Timeline of Transformation"),
        p("How have things changed over time? This final tab zooms in on longitudinal trends. Explore the relationship between BMI and longevity, the closing development gap, and the impact of public health interventions."),
        
        fluidRow(
          column(5,
                 div(class = "control-group", style = "margin-bottom: 15px;",
                     selectInput("trend_analysis", "Analysis Focus:",
                                 choices = list(
                                   "Disease Prevention Progress" = "disease",
                                   "Development Gap Analysis" = "dev_vs_developing",
                                   "BMI and Longevity Relationship" = "bmi_life"
                                 ),
                                 selected = "disease", width = "100%"),
                     
                     conditionalPanel(
                       condition = "input.trend_analysis == 'disease'",
                       selectInput("intervention_type", "Health Intervention:",
                                   choices = list(
                                     "Hepatitis B Vaccination" = "hepatitisb",
                                     "Measles Control" = "measles",
                                     "Polio Eradication" = "polio",
                                     "Diphtheria Prevention" = "diphtheria",
                                     "HIV/AIDS Response" = "hivaids"
                                   ),
                                   selected = "hepatitisb", width = "100%")
                     )
                 ),
                 
                 div(style = "margin-top: 10px;",
                     h3("Visualization Breakdown"),
                     tags$ul(style = "font-size: 13px; line-height: 1.5;",
                             tags$li(tags$strong("BMI vs Life Expectancy:"), " Shows that individuals with a 'Normal' BMI live shorter lives than those classified as overweight — reflecting a paradox observed in many epidemiological studies."),
                             tags$li(tags$strong("Developed vs Developing Nations:"), " While life expectancy is rising globally, the gap between rich and poor countries remains wide — narrowing only slowly."),
                             tags$li(tags$strong("Vaccination Trends:"), " The increase in coverage for hepatitis B, polio, and measles maps almost directly to improvements in life expectancy.")
                     ),
                     
                     h3("Analysis"),
                     p("The data trends presented in this dashboard provide strong evidence of global health improvements over the 15-year period, particularly in areas like disease prevention and life expectancy. It is encouraging to observe steady progress in vaccination coverage, with Hepatitis B, polio, and measles showing significant increases over time. These improvements demonstrate the positive impact of coordinated global health initiatives and public health policies, especially in low- and middle-income countries. However, the gap between developed and developing nations remains evident. While both groups show upward trends in life expectancy, developing countries consistently lag behind. This suggests that although medical advancements are becoming more accessible, structural issues such as poverty, limited infrastructure, and unequal resource distribution continue to limit health outcomes for many populations. The data highlights the importance of sustained investment in healthcare access, education, and local health systems. One particularly interesting observation emerged from the BMI and life expectancy relationship. Contrary to popular assumptions, individuals with a 'normal' BMI sometimes showed lower average life expectancy than those in the 'overweight' category. This outcome supports findings from other epidemiological research and highlights the complex nature of health data, where simple classifications do not always reflect broader lived realities. It also suggests that factors like chronic disease, nutrition quality, and healthcare access play important roles beyond BMI values alone. Overall, this analysis reinforced my understanding that data visualization is more than just a technical tool—it is a way of uncovering human stories. By translating large datasets into accessible visuals, we can gain insights into real-world issues and begin to identify where change is most needed. This assignment has helped me appreciate how health data can drive more informed, compassionate, and equitable decision-making."),
                     
                     
                 )
          ),
          
          column(7,
                 div(class = "viz-container", style = "height: 650px;",
                     plotlyOutput("trends_chart", height = "100%")
                 )
          )
        ),
        div(style = "margin-top: 5px; width: 100%;",
            h3("Conclusion"),
            p("Ten years ago, critical care like life support, ICU admission, or even timely diagnosis would have been out of reach for families like mine—financially, technologically, and geographically. Today, my grandmother's survival was not just a miracle, but a testament to how far medical science and public health have come. We are witnessing a revolution—where once-impossible treatments are becoming routine, and where hope is no longer a privilege of the wealthy."),
            p("She is still here because the systems around her improved—because researchers pushed boundaries, because data illuminated needs, and because someone, somewhere, used insights like those in this dashboard to plan smarter interventions. It made me realize that behind every row in a dataset is a life that could be saved. That's why I built this dashboard—not just to analyze numbers, but to recognize lives, like hers, that deserve a chance.")
        )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data
    
    if (input$filter_country != "All") {
      df <- df %>% filter(country == input$filter_country)
    }
    
    if (input$filter_status != "All") {
      df <- df %>% filter(status == input$filter_status)
    }
    df
  })
  
  
  # Data table output
  output$data_table <- renderDT({
    df <- filtered_data() %>%
      select(
        Country = country,
        Year = year,
        Status = status,
        `Life Expectancy` = lifeexpectancy,
        `Adult Mortality` = adultmortality,
        `Infant Mortality` = infant_mortality_rate,
        `GDP per Capita` = gdp,
        `Education (Years)` = schooling,
        `BMI` = bmi,
        `Alcohol Consumption` = alcohol,
        `Hepatitis B Coverage` = hepatitisb,
        `Measles Cases` = measles
      )
    
    datatable(
      df,
      options = list(
        scrollY = "350px",
        scrollX = TRUE,
        paging = FALSE,
        searching = TRUE,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-left', targets = 0:2),
          list(className = 'dt-center', targets = 3:11)
        )
      ),
      class = 'compact stripe hover',
      rownames = FALSE,
      filter = 'top'
    ) %>%
      formatRound(columns = c('Life Expectancy', 'GDP per Capita', 'BMI',
                              'Alcohol Consumption', 'Education (Years)'), digits = 1) %>%
      formatPercentage(columns = c('Hepatitis B Coverage'), digits = 0)
  })
  
  
  # World map with zoom capability
  output$world_map <- renderPlotly({
    selected_metric <- input$map_indicator
    
    # Color configuration - fixed ranges for all data
    color_config <- switch(selected_metric,
                           "lifeexpectancy" = list(
                             colors = c("white", "#2d8f2d"),
                             title = "Life Expectancy<br>(Years)",
                             range = c(40, 90)
                           ),
                           "adultmortality" = list(
                             colors = c("white", "#d32f2f"),
                             title = "Adult Mortality<br>(per 1000)",
                             range = c(0, 700)
                           ),
                           "infant_mortality_rate" = list(
                             colors = c("white", "#ff5722"),
                             title = "Infant Mortality<br>(per 1000)",
                             range = c(0, 120)
                           ),
                           "bmi" = list(
                             colors = c("white", "#7b1fa2"),
                             title = "Average BMI<br>(kg/m²)",
                             range = range(map_data_merged$bmi, na.rm = TRUE)
                           )
    )
    plot_data <- map_data_merged %>%
      mutate(display_label = paste0(
        region, "<br>",
        switch(input$map_indicator,
               "lifeexpectancy" = "Life Expectancy: ",
               "adultmortality" = "Adult Mortality: ",
               "infant_mortality_rate" = "Infant Mortality: ",
               "bmi" = "Average BMI: "),
        round(get(input$map_indicator), 1)
      ))
    
    # Create plot with border styling
    p <- ggplot(plot_data, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = selected_metric, text = "display_label"), color = "#333333", size = 0.2) +
      scale_fill_gradient(
        low = color_config$colors[1], 
        high = color_config$colors[2],
        limits = color_config$range,
        na.value = "#e0e0e0",
        name = color_config$title
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.background = element_rect(fill = "white", color = "#cccccc", size = 1),
        panel.background = element_rect(fill = "#f5f5f5", color = NA)
      ) +
      coord_fixed(1.3)
    
    ggplotly(p, tooltip = "text", height = 550) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = TRUE
      ) %>%
      config(
        displayModeBar = FALSE  # This disables all toolbar buttons
      )
  })
  
  # Trends chart
  output$trends_chart <- renderPlotly({
    req(input$trend_analysis)
    
    # Create the base plot based on analysis type
    plot_result <- if (input$trend_analysis == "disease") {
      req(input$intervention_type)
      selected_intervention <- input$intervention_type
      
      y_label <- switch(selected_intervention,
                        "hepatitisb" = "Hepatitis B Coverage (%)",
                        "measles" = "Measles Cases",
                        "polio" = "Polio Coverage (%)",
                        "diphtheria" = "Diphtheria Coverage (%)",
                        "hivaids" = "HIV/AIDS Deaths (per 100,000)"
      )
      
      plot_ly(health_intervention_data, 
              x = ~year, 
              y = ~get(selected_intervention),
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = '#2e7d32', width = 3),
              marker = list(color = '#2e7d32', size = 8),
              hovertemplate = paste0("<b>", y_label, ": %{y}</b><br>Year: %{x}<extra></extra>")) %>%
        layout(
          xaxis = list(title = "Year", gridcolor = "#f0f0f0"),
          yaxis = list(title = "Life Expectancy (Years)", gridcolor = "#f0f0f0"),
          plot_bgcolor = "#fafafa",
          paper_bgcolor = "white",
          legend = list(x = 0.02, y = 0.98),
          margin = list(l = 60, r = 20, t = 20, b = 60),
          hovermode = 'closest'
        )
      
    } else {
      # BMI analysis
      bmi_colors <- c("Normal" = "#4caf50", "Underweight" = "#ff9800",
                      "Overweight" = "#2196f3", "Obese" = "#9c27b0")
      
      plot_data <- plot_ly()
      
      for (bmi_cat in names(bmi_colors)) {
        category_data <- bmi_health_analysis %>% filter(bmi_category == bmi_cat)
        
        if (nrow(category_data) > 0) {
          plot_data <- plot_data %>% add_trace(
            data = category_data,
            x = ~year,
            y = ~avg_life_exp,
            name = bmi_cat,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = bmi_colors[[bmi_cat]], width = 3),
            marker = list(color = bmi_colors[[bmi_cat]], size = 8)
          )
        }
      }
      
      plot_data %>%
        layout(
          xaxis = list(title = "Year", gridcolor = "#f0f0f0"),
          yaxis = list(title = "Life Expectancy (Years)", gridcolor = "#f0f0f0"),
          plot_bgcolor = "#fafafa",
          paper_bgcolor = "white",
          legend = list(x = 0.02, y = 0.98),
          margin = list(l = 60, r = 20, t = 20, b = 60),
          hovermode = 'closest'
        )
    }
    
    # Apply config to the final plot
    plot_result %>% config(displayModeBar = FALSE)
  })
}

# Run the app
shinyApp(ui = ui, server = server) 