library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(RColorBrewer)
library(DT)
library(bslib)

#data
spotify_year <- read_csv("data_by_year.csv", show_col_types = FALSE)
songs_raw    <- read_csv("spotify_dataset.csv", show_col_types = FALSE)
genres_raw   <- read_csv("data_w_genres.csv", show_col_types = FALSE)

#Viz 1 Prep
feature_choices <- c("acousticness", "danceability", "energy", 
                     "instrumentalness", "liveness", "speechiness", 
                     "tempo", "valence", "popularity")

spotify_scaled <- spotify_year %>%
  mutate(year = as.numeric(year)) %>%
  mutate(across(all_of(feature_choices), ~ as.numeric(scale(.))))

ranked_features <- spotify_year %>%
  summarise(across(all_of(setdiff(feature_choices, "popularity")),
                   ~ cor(.x, popularity, use = "complete.obs"))) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "correlation") %>%
  mutate(abs_corr = abs(correlation)) %>%
  arrange(desc(abs_corr)) %>%
  pull(feature)
ranked_features <- c(ranked_features, "popularity")

dark2_colors <- brewer.pal(8, "Dark2")
feature_colors <- c(
  acousticness = dark2_colors[1], danceability = dark2_colors[2],
  energy = dark2_colors[3], instrumentalness = dark2_colors[4],
  liveness = dark2_colors[5], speechiness = dark2_colors[6],
  tempo = dark2_colors[7], valence = "#000000", popularity = "#808080"
)

#Viz 2 Prep
songs_q2 <- songs_raw %>%
  select(id, name, artists, year, popularity, valence, energy, danceability) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    year = as.numeric(year),
    decade_num = floor(year / 10) * 10,
    decade = paste0(decade_num, "s"),
    upbeat_score = (valence + energy + danceability) / 3
  ) %>%
  filter(!is.na(year), !is.na(valence), !is.na(energy), !is.na(danceability))

decade_summary <- songs_q2 %>%
  group_by(decade_num, decade) %>%
  summarise(
    avg_valence = mean(valence, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_upbeat_score = mean(upbeat_score, na.rm = TRUE),
    n_songs = n(), .groups = "drop"
  ) %>% arrange(decade_num)

events_by_decade <- tribble(
  ~decade_num, ~event, ~description,
  1920, "Jazz Age / Roaring Twenties", "Economic growth, nightlife, and jazz contributed to more lively music.",
  1930, "Great Depression", "Widespread economic struggles influenced the themes and tones listeners connected with.",
  1940, "World War II", "The uncertainty and patriotism of the war shaped entertainment and emotional tone.",
  1950, "Postwar optimism", "Prosperity and youth culture fueled energetic, fast-paced rock and roll.",
  1960, "Civil Rights era", "Social movements and cultural experimentation influenced the message and sound.",
  1970, "Disco and dance culture", "Dance-focused styles became highly popular, spiking energy levels.",
  1980, "MTV era / synth-pop", "Music videos and electronic production shaped a brighter, polished pop sound.",
  1990, "Grunge and alternative", "Introspective/darker styles became prominent, lowering positivity measures.",
  2000, "Digital music era", "Digital production and genre blending changed mainstream sound.",
  2010, "Streaming era", "Online discovery influenced the mood, style, and structure of popular songs.",
  2020, "TikTok virality", "Short-form trends shaped catchy, repetitive, and immediately engaging songs."
)

#Viz 3 Prep
df_v3 <- songs_raw %>%
  mutate(tier = case_when(
    popularity >= 75 ~ "High (75–100)",
    popularity >= 40 ~ "Mid (40–74)",
    TRUE             ~ "Low (0–39)"
  ))
v3_features <- c("danceability", "energy", "acousticness", "valence", "speechiness", "liveness", "instrumentalness")
v3_labels   <- c("Danceability", "Energy", "Acousticness", "Valence", "Speechiness", "Liveness", "Instrumentalness")

tier_avgs <- df_v3 %>%
  group_by(tier) %>%
  summarise(across(all_of(v3_features), mean, na.rm = TRUE), .groups = "drop") %>%
  arrange(factor(tier, levels = c("High (75–100)", "Mid (40–74)", "Low (0–39)")))

tier_colors <- c("High (75–100)" = "#E7298AFF", "Mid (40–74)" = "#66A61EFF", "Low (0–39)" = "#E6AB02FF")

#Viz 4 Prep
df_long <- genres_raw %>%
  mutate(genres = str_remove_all(genres, "\\[|\\]|\\'")) %>%
  separate_rows(genres, sep = ",\\s*") %>%
  mutate(genres = str_trim(genres)) %>%
  filter(genres != "")


ui <- navbarPage(
  title = "Echoes of an Era: The Anatomy of Popular Music",
  theme = bs_theme(
    bootswatch = "journal", 
    primary = "#1DB954", 
    base_font = font_google("Merriweather"),
    heading_font = font_google("Playfair Display")
  ),

  tabPanel("Data Story",
           fluidRow(
             column(8, offset = 2,
                    h1("Echoes of an Era: How History Shapes the Sound of Popular Music", style="font-weight: bold; margin-bottom: 20px;"),
                    h4("By Team 10", style="color: gray; margin-bottom: 30px;"),
                    
                    p(class="lead", "Music is intensely personal, yet undeniably collective. A song that defines a summer for one generation might feel completely foreign to the next. But when we aggregate the listening habits of millions over nearly a century, a fascinating narrative emerges. We set out to investigate a core question: Is there a formula for a hit song, and how has the 'DNA' of popular music evolved alongside human history?"),
                    
                    p("To answer this, we analyzed over 160,000 tracks from Spotify's database, spanning from the roaring 1920s to the viral hits of 2020. Our findings show that the qualities of top songs are far ffrom stale, reflecting our cultural trends."),
                    
                    h3("The Shifting Landscape of Emotion", style="margin-top: 30px;"),
                    p("Our first major inquiry was tracing musical characteristics over time. Do songs become more upbeat from decade to decade? The data presents a compelling, non-linear story. If we examine the composite 'Upbeat Score' (an average of valence, energy, and danceability) over the last 100 years, we see the profound impact of global events."),
                    
                    # Story Viz 1: The 100-year trend
                    div(style = "margin: 30px 0; padding: 15px; border-left: 4px solid #1DB954; background-color: #f9f9f9;",
                        plotlyOutput("story_plot_1", height = "350px"),
                        p(em("Above: The historical trajectory of 'Upbeat' music. Hover over the line to see exact values for specific years."), style = "text-align: center; font-size: 0.9em; margin-top: 10px;")
                    ),
                    
                    p("The positiveness of music correlates strongly with historical events. The somber tones and lower energy during the Great Depression gave way to the energetic postwar optimism of the 1950s. We observed a significant spike during the 1970s disco era. Yet, average positivity plummeted during the 1990s as introspective grunge and alternative rock dominated the airwaves. Today, driven by short-form virality on platforms like TikTok, the upbeat score has violently rebounded."),
                    
                    h3("The Anatomy of a Modern Hit", style="margin-top: 30px;"),
                    p("But what traits do the most popular songs share right now? When we isolate the data by popularity tier, a stark contrast emerges between the songs that dominate the charts and the songs that fade into obscurity."),
                    
                    div(style = "margin: 30px 0; padding: 15px; border-left: 4px solid #E7298A; background-color: #f9f9f9;",
                        plotlyOutput("story_plot_2", height = "350px"),
                        p(em("Above: A direct comparison of core audio features between top-tier hits (score 75-100) and bottom-tier tracks (score 0-39)."), style = "text-align: center; font-size: 0.9em; margin-top: 10px;")
                    ),
                    
                    p("As the data shows, 'High' popularity tracks practically abandon high acousticness and instrumentalness. Instead, they index massively in danceability and energy. The modern listener demands immediate engagement, a heavy beat, and a clear, dominant vocal presence."),
                    
                    h3("The Genre Monopolies & Conclusion", style="margin-top: 30px;"),
                    p("Finally, examining the top genres reveals a highly concentrated landscape. Pop, Modern Rock, and Urban Contemporary artists command massive shares of overall popularity. The artists sitting at the top of these genres are artists whose catalogs perfectly align with the high-energy, high-danceability matrix we identified."),
                    
                    p(strong("The Takeaway:"), " The formula for a hit song is not a static set of rules; it is a living reflection of our culture, technology, and collective mood. However, if you want to write a chart-topper song today, the data is clear: turn up the energy, and make sure we can dance to it."),
                    
                    p(em("Ready to dive deeper? Click the 'Interactive Exploration' tab at the top of the page to investigate these trends yourself using our interactive tools."))
             )
           )
  ),

  tabPanel("Interactive Exploration",
           tabsetPanel(
             
             # VIZ 1
             tabPanel("1. Features Over Time",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Track Characteristics"),
                          p("See how the makeup of music changes in a specific year."),
                          sliderInput("v1_year", "Select Year:", 
                                      min = min(spotify_scaled$year, na.rm = TRUE),
                                      max = max(spotify_scaled$year, na.rm = TRUE),
                                      value = 1980, step = 1, sep = ""),
                          checkboxGroupInput("v1_features", "Select Characteristics:", 
                                             choices = ranked_features, selected = ranked_features[1:4]),
                          hr(),
                          div(style = "background-color: #eef8f1; padding: 10px; border-radius: 5px;",
                              strong("How to explore this chart:"),
                              tags$ul(
                                tags$li("Drag the slider to shift the data to a different year."),
                                tags$li("Check/uncheck boxes to add or remove features from the comparison."),
                                tags$li("Hover your mouse over the bars to see the exact standardized numerical values.")
                              )
                          )
                        ),
                        mainPanel(
                          plotlyOutput("v1_barPlot", height = "500px"),
                          h5("Data Snapshot:"),
                          verbatimTextOutput("v1_summaryText")
                        )
                      )
             ),
             
             # VIZ 2
             tabPanel("2. Historical Trends & Top Songs",
                      fluidRow(
                        column(7,
                               selectInput("v2_features", "Choose features to compare (select multiple):",
                                           choices = c("Valence" = "avg_valence", "Energy" = "avg_energy",
                                                       "Danceability" = "avg_danceability", "Upbeat score" = "avg_upbeat_score"),
                                           selected = c("avg_valence", "avg_energy", "avg_danceability", "avg_upbeat_score"),
                                           multiple = TRUE, width = "100%"),
                               plotlyOutput("v2_trend_plot", height = "500px"),
                               
                               # Interactive Instructions
                               div(style = "background-color: #eef8f1; padding: 10px; border-radius: 5px; margin-top: 15px;",
                                   strong("How to explore this chart:"),
                                   tags$ul(
                                     tags$li("Use the dropdown menu above the chart to add or remove emotional features."),
                                     tags$li(strong("Interactive Click: "), "Click directly on any dot on the line graph. This will automatically update the right panel with the Historical Events and Top 10 Songs for that specific decade!")
                                   )
                               )
                        ),
                        column(5,
                               wellPanel(
                                 h4("Decade Context"),
                                 uiOutput("v2_selected_decade_info"),
                                 hr(),
                                 h5("Historical Events"),
                                 DTOutput("v2_events_table"),
                                 hr(),
                                 h5("Top 10 Most Popular Songs"),
                                 DTOutput("v2_top_songs_table")
                               )
                        )
                      )
             ),
             
             # VIZ 3
             tabPanel("3. Popularity Radar",
                      sidebarLayout(
                        sidebarPanel(
                          h4("The Anatomy of Popularity"),
                          p("Compare the audio features of songs based on their overall popularity tier."),
                          checkboxGroupInput("v3_tiers", "Select Tiers to Compare:",
                                             choices = c("High (75–100)", "Mid (40–74)", "Low (0–39)"),
                                             selected = c("High (75–100)", "Low (0–39)")),
                          hr(),
                          # Interactive Instructions
                          div(style = "background-color: #eef8f1; padding: 10px; border-radius: 5px;",
                              strong("How to explore this chart:"),
                              tags$ul(
                                tags$li("Check the boxes above to overlay 'High', 'Mid', and 'Low' popularity songs on top of each other."),
                                tags$li("Hover your mouse over the points on the radar web to see the exact average score for a specific audio trait (like Danceability)."),
                                tags$li("Click a tier name in the legend to hide/show it temporarily.")
                              )
                          )
                        ),
                        mainPanel(
                          plotlyOutput("v3_radar_plot", height = "600px")
                        )
                      )
             ),
             
             # VIZ 4
             tabPanel("4. Genre Share Sunburst",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Genre & Artist Breakdown"),
                          p("Explore how specific artists contribute to the overall popularity of the top genres."),
                          sliderInput("v4_top_n", "Number of Top Genres to Show:",
                                      min = 3, max = 15, value = 10, step = 1),
                          hr(),
                          # Interactive Instructions
                          div(style = "background-color: #eef8f1; padding: 10px; border-radius: 5px;",
                              strong("How to explore this chart:"),
                              tags$ul(
                                tags$li("Use the slider to increase or decrease the number of genres analyzed in the wheel."),
                                tags$li(strong("Hovering: "), "Hover over the inner circle to see a genre's overall share. Hover over the outer wedges to see individual artist popularity."),
                                tags$li(strong("Zooming: "), "Click on an inner genre slice (like 'Pop') to zoom in and expand that specific genre. Click the center circle to zoom back out.")
                              )
                          )
                        ),
                        mainPanel(
                          plotlyOutput("v4_sunburst_plot", height = "700px")
                        )
                      )
             )
           )
  )
)

server <- function(input, output, session) {
  
  #Data Story Server Plots
  output$story_plot_1 <- renderPlotly({
    trend_data <- spotify_year %>%
      mutate(upbeat = (valence + energy + danceability) / 3, year = as.numeric(year)) %>%
      arrange(year)
    
    plot_ly(data = trend_data, x = ~year, y = ~upbeat, type = "scatter", mode = "lines+markers",
            line = list(color = "#1DB954", width = 3), marker = list(size = 4, color = "#191414"),
            hovertemplate = "Year: %{x}<br>Upbeat Score: %{y:.2f}<extra></extra>") %>%
      layout(title = "100 Years of Emotion: The Average Upbeat Score (1921-2020)",
             xaxis = list(title = "Year"), yaxis = list(title = "Upbeat Score (0 to 1)"),
             margin = list(t = 40, b = 40))
  })
  
  output$story_plot_2 <- renderPlotly({
    high_low <- tier_avgs %>% 
      filter(tier %in% c("High (75–100)", "Low (0–39)")) %>%
      select(tier, danceability, energy, acousticness) %>%
      pivot_longer(cols = -tier, names_to = "Feature", values_to = "Score") %>%
      mutate(Feature = str_to_title(Feature))
    
    plot_ly(data = high_low, x = ~Feature, y = ~Score, color = ~tier, type = "bar",
            colors = c("High (75–100)" = "#E7298A", "Low (0–39)" = "#E6AB02"),
            hovertemplate = "%{x}<br>Score: %{y:.2f}<extra></extra>") %>%
      layout(title = "The Hit Formula: Comparing High vs. Low Popularity Tracks",
             xaxis = list(title = "Audio Feature"), yaxis = list(title = "Average Score"),
             barmode = 'group', margin = list(t = 40, b = 40))
  })
  
  # viz 1 Server
  v1_data <- reactive({
    req(input$v1_features)
    spotify_scaled %>%
      filter(year == input$v1_year) %>%
      select(year, all_of(input$v1_features)) %>%
      pivot_longer(cols = -year, names_to = "feature", values_to = "value") %>%
      mutate(feature = factor(feature, levels = input$v1_features))
  })
  
  output$v1_barPlot <- renderPlotly({
    df <- v1_data()
    plot_ly(data = df, x = ~feature, y = ~value, color = ~feature, colors = feature_colors, type = "bar",
            hovertemplate = paste("Year: ", input$v1_year, "<br>Feature: %{x}<br>Standardized Value: %{y:.2f}<extra></extra>")) %>%
      layout(title = paste("Standardized Music Characteristics in", input$v1_year),
             xaxis = list(title = "Characteristic"), yaxis = list(title = "Standardized Value"))
  })
  
  output$v1_summaryText <- renderText({
    df <- v1_data()
    paste(apply(df, 1, function(row) paste0(row["feature"], ": ", round(as.numeric(row["value"]), 2))), collapse = "\n")
  })
  
  # Viz 2 Server
  get_labels <- function(feature_names) {
    sapply(feature_names, function(f) {
      case_when(f == "avg_valence" ~ "Valence", f == "avg_energy" ~ "Energy",
                f == "avg_danceability" ~ "Danceability", f == "avg_upbeat_score" ~ "Upbeat score")
    })
  }
  
  selected_decade <- reactiveVal(min(decade_summary$decade_num, na.rm = TRUE))
  
  observeEvent(event_data("plotly_click", source = "decade_plot"), {
    click <- event_data("plotly_click", source = "decade_plot")
    if (!is.null(click)) selected_decade(click$key[1])
  })
  
  output$v2_trend_plot <- renderPlotly({
    req(length(input$v2_features) > 0)
    plot_data <- decade_summary %>%
      select(decade_num, decade, n_songs, all_of(input$v2_features)) %>%
      pivot_longer(cols = all_of(input$v2_features), names_to = "feature", values_to = "value") %>%
      mutate(feature_label = get_labels(feature))
    
    plot_ly(data = plot_data, x = ~decade_num, y = ~value, color = ~feature_label, 
            type = "scatter", mode = "lines+markers", source = "decade_plot", key = ~decade_num,
            text = ~paste0("Decade: ", decade, "<br>", feature_label, ": ", round(value, 3), "<br>Songs: ", n_songs),
            hoverinfo = "text", line = list(width = 3), marker = list(size = 8)) %>%
      layout(title = list(text = "Historical Emotion: Musical Features by Decade", x = 0.02),
             xaxis = list(title = "Decade", tickmode = "array", tickvals = decade_summary$decade_num, ticktext = decade_summary$decade),
             yaxis = list(title = "Average Value", range = c(0, 1)),
             legend = list(title = list(text = "Feature")))
  })
  
  output$v2_selected_decade_info <- renderUI({
    dec <- selected_decade()
    row <- decade_summary %>% filter(decade_num == dec)
    req(nrow(row) > 0)
    tagList(
      h5(row$decade[1], style="color: #1DB954; font-weight: bold;"),
      p(strong("Total Songs Analyzed: "), format(row$n_songs[1], big.mark=","))
    )
  })
  
  output$v2_events_table <- renderDT({
    dec <- selected_decade()
    tbl <- events_by_decade %>% filter(decade_num == dec) %>% select(`Event/Trend` = event, Description = description)
    datatable(tbl, rownames = FALSE, options = list(dom = "t", autoWidth = TRUE, pageLength = 5))
  })
  
  output$v2_top_songs_table <- renderDT({
    dec <- selected_decade()
    top_songs <- songs_q2 %>%
      filter(decade_num == dec) %>%
      arrange(desc(popularity)) %>%
      head(10) %>%
      select(Track = name, Artist = artists, Year = year, Popularity = popularity) %>%
      mutate(Artist = str_remove_all(Artist, "\\[|\\]|\\'")) 
    
    datatable(top_songs, rownames = FALSE, options = list(dom = "tp", pageLength = 5, autoWidth = TRUE))
  })
  
  # Viz 3 Server
  output$v3_radar_plot <- renderPlotly({
    req(input$v3_tiers)
    active_tiers <- tier_avgs %>% filter(tier %in% input$v3_tiers)
    
    fig <- plot_ly(type = "scatterpolar", fill = "toself")
    if(nrow(active_tiers) > 0){
      for (i in seq_len(nrow(active_tiers))) {
        tier_name <- active_tiers$tier[i]
        vals <- as.numeric(active_tiers[i, v3_features])
        fig <- fig %>% add_trace(
          r = c(vals, vals[1]), theta = c(v3_labels, v3_labels[1]), name = tier_name,
          line = list(color = tier_colors[tier_name], width = 3),
          marker = list(color = tier_colors[tier_name], size = 7),
          fillcolor = paste0(tier_colors[tier_name], "33"),
          hovertemplate = paste0("<b>", tier_name, "</b><br>%{theta}: <b>%{r:.2f}</b><extra></extra>")
        )
      }
    }
    fig %>% layout(
      title = "The Blueprint of a Hit: Average Audio Features by Popularity Tier",
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 1)))
    )
  })
  
  # Viz 4 Server
  output$v4_sunburst_plot <- renderPlotly({
    req(input$v4_top_n)
    
    top_n_genres <- df_long %>%
      count(genres, sort = TRUE) %>%
      slice_head(n = input$v4_top_n) %>%
      pull(genres)
    
    avg_pop <- df_long %>%
      filter(genres %in% top_n_genres) %>%
      group_by(genres, artists) %>%
      summarise(avg_pop = mean(popularity, na.rm = TRUE), .groups = "drop")
    
    top_artists <- avg_pop %>%
      arrange(genres, desc(avg_pop)) %>%
      group_by(genres) %>%
      mutate(rank = row_number()) %>%
      ungroup() %>%
      filter(rank <= 5) %>%
      mutate(genres = str_trunc(genres, 15)) 
    
    artist_rows <- top_artists %>%
      transmute(ids = paste(genres, artists, sep = " - "), labels = artists,
                parents = genres, values = round(avg_pop), avg = round(avg_pop))
    
    genre_rows <- artist_rows %>%
      group_by(parents) %>%
      summarise(values = sum(values), avg = round(mean(values)), .groups = "drop") %>%
      transmute(ids = parents, labels = parents, parents = "music", values = values, avg = avg)
    
    genre_rows <- genre_rows %>% mutate(pct = round(values / sum(values) * 100, 1))
    artist_rows <- artist_rows %>% left_join(genre_rows %>% select(ids, pct), by = c("parents" = "ids"))
    
    root_row <- tibble(ids = "music", labels = "Music", parents = "", values = sum(genre_rows$values), avg = NA, pct = 100)
    sunburst_df <- bind_rows(root_row, genre_rows, artist_rows)
    
    dark2 <- c("#E41A1C","#D95F02","#E6AB02","#66A61E",
               "#1B9E77","#377EB8","#7570B3","#E7298A",
               "#A6761D","#666666")
    
    final_colors <- rep(dark2, length.out = nrow(genre_rows))
    genre_labels <- genre_rows$labels
    color_map    <- setNames(final_colors, genre_labels)
    
    sunburst_df <- sunburst_df %>%
      mutate(
        color = case_when(
          ids == "music" ~ "#CCCCCC", 
          labels %in% names(color_map) ~ color_map[labels], 
          TRUE ~ color_map[parents]
        ),
        tooltip_text = paste0("Avg Popularity: ", avg, "<br>Share: ", pct, "%")
      )
    
    plot_ly(data = sunburst_df, ids = ~ids, labels = ~labels, parents = ~parents, values = ~values,
            type = "sunburst", branchvalues = "total", marker = list(colors = ~color),
            customdata = ~tooltip_text, hovertemplate = "<b>%{label}</b><br>%{customdata}<extra></extra>") %>%
      layout(
        title = paste("Genre Market Share: Top Artists inside", input$v4_top_n, "Leading Genres"),
        margin = list(l = 10, r = 10, b = 10, t = 40)
      )
  })
}

shinyApp(ui = ui, server = server)
