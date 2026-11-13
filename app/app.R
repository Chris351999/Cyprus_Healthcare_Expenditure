# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT : app/app.R
# PURPOSE: Simple, story-driven dashboard: where does Cyprus's
#          health money go, and what changed with GeSY?
# DATA   : app/data/*.csv (copied there by run_all.R)
# RUN    : shiny::runApp("app")
# ============================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)


# ============================================================
# 1. DATA
# ============================================================

# Works both from the project folder and from inside app/
data_dir <- if (file.exists("data/public_private.csv")) "data" else "app/data"
rd <- function(file) read_csv(file.path(data_dir, file), show_col_types = FALSE)

public_private  <- rd("public_private.csv")
oop_by_care     <- rd("oop_by_care.csv")
spending_care   <- rd("spending_by_care.csv")
health_gdp      <- rd("health_gdp.csv")
health_real     <- rd("health_real.csv")
eu_spending_all <- rd("eu_spending_all.csv")
eu_outcomes     <- rd("eu_outcomes.csv")
unmet           <- rd("unmet_needs.csv")
did             <- rd("did_results.csv")
verdict         <- rd("verdict.csv")
citizen         <- rd("citizen_per_person.csv")
household       <- rd("household_burden.csv")
flows_fp        <- rd("flows_financing_provider.csv")
flows_pc        <- rd("flows_provider_care.csv")


# ============================================================
# 2. COLOURS AND HELPERS
# ============================================================

blue   <- "#2a78d6"   # pooled: taxes & GeSY
orange <- "#eb6834"   # private: out-of-pocket
red    <- "#d6293e"   # Cyprus in EU charts
navy   <- "#0f2742"
grey   <- "#9aa3af"

group_cols <- c("Hospital care"            = "#2a78d6",
                "Outpatient care"          = "#eb6834",
                "Medicines & devices"      = "#1baf7a",
                "Labs, prevention & admin" = "#eda100",
                "Long-term & home care"    = "#7a5fd1")

care_groups <- c("Hospital care"       = "Hospital care",
                 "Outpatient care"     = "Outpatient care",
                 "Home care"           = "Long-term & home care",
                 "Long-term care"      = "Long-term & home care",
                 "Lab tests & imaging" = "Labs, prevention & admin",
                 "Prevention"          = "Labs, prevention & admin",
                 "Administration"      = "Labs, prevention & admin",
                 "Medicines"           = "Medicines & devices",
                 "Medical devices"     = "Medicines & devices")

sankey_cols <- c("GeSY" = blue, "Government" = "#8fb6ee", "Out-of-pocket" = orange,
                 "Voluntary insurance" = grey, "Employers" = "#c3c9d1",
                 "Non-profit" = "#d9dde3", "Foreign payers" = "#e9ecef")

rating_cols <- c("Success" = "#1baf7a", "Concern" = "#eb6834",
                 "Mixed"   = "#eda100", "Unclear" = "#8a94a6")

# Grey band for the GeSY roll-out (2019-2020)
gesy_band <- list(type = "rect", xref = "x", yref = "paper", x0 = 2018.5, x1 = 2020.5,
                  y0 = 0, y1 = 1, fillcolor = "#94a3b8", opacity = 0.13,
                  line = list(width = 0), layer = "below")
gesy_label <- list(x = 2019.5, y = 1, xref = "x", yref = "paper", text = "GeSY",
                   showarrow = FALSE, yanchor = "bottom", font = list(size = 11, color = "#64748b"))

# One clean look for every chart
clean <- function(p, ytitle = "", band = FALSE, suffix = "", prefix = "", hover = "x unified") {
  p %>%
    layout(shapes      = if (band) list(gesy_band) else list(),
           annotations = if (band) list(gesy_label) else list(),
           xaxis  = list(title = "", showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE),
           yaxis  = list(title = ytitle, gridcolor = "#eef1f5", zeroline = FALSE, fixedrange = TRUE,
                         ticksuffix = suffix, tickprefix = prefix, rangemode = "tozero"),
           legend = list(orientation = "h", x = 0, y = -0.12, font = list(size = 12)),
           hovermode  = hover,
           hoverlabel = list(bgcolor = "white", bordercolor = "#e5e7eb"),
           font   = list(family = "Inter, sans-serif", size = 12, color = "#374151"),
           margin = list(l = 10, r = 10, t = 25, b = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
    config(displayModeBar = FALSE)
}

# Simple line chart: one line per column of a wide table
lines_chart <- function(d, cols, suffix = "", prefix = "", digits = 0) {
  p <- plot_ly()
  for (nm in names(cols)) {
    p <- add_trace(p, x = d$year, y = d[[nm]], name = nm, type = "scatter",
                   mode = "lines+markers",
                   line = list(color = cols[[nm]], width = 3.5, shape = "spline", smoothing = 0.6),
                   marker = list(color = cols[[nm]], size = 6),
                   hovertemplate = paste0(nm, ": ", prefix, "%{y:,.", digits, "f}", suffix,
                                          "<extra></extra>"))
  }
  p
}

hex_alpha <- function(hex, a = 0.35) {
  v <- col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", v[1, ], v[2, ], v[3, ], a)
}

fmt_eur <- function(x) paste0("€", format(round(x), big.mark = ","))
val     <- function(d, yr, col) d[[col]][d$year == yr]


# ============================================================
# 3. HEADLINE NUMBERS
# ============================================================

pooled_18 <- round(val(public_private, 2018, "public_share"))
pooled_24 <- round(val(public_private, 2024, "public_share"))
oop_18    <- val(citizen, 2018, "oop_per_person")
oop_24    <- val(citizen, 2024, "oop_per_person")
gdp_24    <- val(health_gdp, 2024, "che_pct_gdp")
poor      <- unmet %>% filter(country == "Cyprus", reason == "Too expensive", income == "Poorest 20%")
poor_14   <- val(poor, 2014, "value")
poor_last <- poor$value[poor$year == max(poor$year)]


# ============================================================
# 4. THEME
# ============================================================

theme <- bs_theme(
  version = 5, bg = "#f6f8fb", fg = "#1f2937", primary = blue,
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  "navbar-bg" = navy
) %>%
  bs_add_rules("
    .container-fluid { max-width: 1280px; }
    .card { background:#fff; border:none; border-radius:18px;
            box-shadow:0 1px 2px rgba(16,24,40,.05), 0 6px 16px rgba(16,24,40,.05); }
    .card-header { background:transparent; border-bottom:none; padding:18px 20px 0;
                   font-weight:700; font-size:1.05rem; }
    .card-header small { display:block; font-weight:400; color:#6b7280; font-size:.85rem; margin-top:2px; }
    .card-footer { background:transparent; border-top:none; color:#9ca3af; font-size:.78rem; padding:0 20px 14px; }
    .hero { background:linear-gradient(120deg,#0f2742 0%,#1d4f91 60%,#2a78d6 100%); color:#fff;
            border-radius:22px; padding:38px 42px; margin:6px 0 4px; }
    .hero .eyebrow { text-transform:uppercase; letter-spacing:.12em; font-size:.75rem; opacity:.75; }
    .hero h1 { font-weight:800; letter-spacing:-.025em; font-size:2.1rem; margin:8px 0 10px; }
    .hero p { font-size:1.08rem; opacity:.9; max-width:820px; margin:0; }
    .kpi { padding:20px 22px; }
    .kpi .icon { width:42px; height:42px; border-radius:12px; display:flex; align-items:center;
                 justify-content:center; font-size:1.25rem; margin-bottom:12px; }
    .kpi .value { font-size:2rem; font-weight:800; letter-spacing:-.02em; line-height:1.1; }
    .kpi .label { color:#374151; font-weight:600; margin-top:4px; }
    .kpi .sub { color:#9ca3af; font-size:.85rem; }
    .chapter { margin:26px 2px 6px; }
    .chapter h3 { font-weight:800; letter-spacing:-.02em; margin-bottom:2px; }
    .chapter p { color:#6b7280; margin:0; }
    .finding { display:flex; gap:14px; padding:10px 0; border-bottom:1px solid #f1f3f6; }
    .finding:last-child { border-bottom:none; }
    .finding .n { flex:0 0 28px; height:28px; border-radius:50%; background:#eaf2fd; color:#2a78d6;
                  font-weight:700; font-size:.85rem; display:flex; align-items:center; justify-content:center; }
    .pill { padding:3px 12px; border-radius:999px; font-size:.76rem; font-weight:600; color:#fff; }
    .table td, .table th { padding:12px 10px; border-color:#f1f3f6; }
  ")

# A KPI tile: big number, label, small note
kpi <- function(value, label, sub, icon, colour) {
  card(class = "kpi",
       div(class = "icon", style = paste0("background:", hex_alpha(colour, .12), ";color:", colour),
           bs_icon(icon)),
       div(class = "value", value),
       div(class = "label", label),
       div(class = "sub", sub))
}

chapter <- function(title, text) div(class = "chapter", h3(title), p(text))


# ============================================================
# 5. USER INTERFACE
# ============================================================

ui <- page_navbar(
  title = tags$span(bs_icon("heart-pulse-fill"), " Cyprus Health Spending"),
  theme = theme,
  fillable = FALSE,
  
  # ---------------- 1. OVERVIEW ----------------
  nav_panel(
    "Overview", icon = bs_icon("house"),
    
    div(class = "hero",
        div(class = "eyebrow", "Cyprus · Health accounts 2010-2024"),
        h1("Who pays for health care in Cyprus, and what did GeSY change?"),
        p("In two years, GeSY turned Cyprus from the EU country where patients paid the most
           out of their own pocket into one with EU-level protection. It did not make care cheaper.
           It changed who pays, and how.")),
    
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      kpi(paste0(pooled_24, "%"), "paid by taxes & GeSY", paste0("was ", pooled_18, "% in 2018"),
          "bank2", blue),
      kpi(fmt_eur(oop_24), "out-of-pocket per person", paste0("was ", fmt_eur(oop_18), " in 2018 (2024 €)"),
          "wallet2", orange),
      kpi(paste0(poor_last, "%"), "of the poorest skip care due to cost", paste0("was ", poor_14, "% in 2014"),
          "people-fill", "#1baf7a"),
      kpi(paste0(format(round(gdp_24, 1), nsmall = 1), "%"), "of GDP spent on health", "EU average: about 10%",
          "pie-chart-fill", "#7a5fd1")
    ),
    
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Who pays for every €100 of health care?",
                       tags$small("Share of current health expenditure, 2010-2024")),
           plotlyOutput("ov_who", height = "380px"),
           card_footer("Source: CYSTAT. Grey band = GeSY roll-out (June 2019 outpatient, June 2020 hospitals).")),
      card(card_header("In five lines"),
           card_body(
             div(class = "finding", div(class = "n", "1"),
                 div(tags$b("Who pays flipped: "), "pooled share 42% → 79%.")),
             div(class = "finding", div(class = "n", "2"),
                 div(tags$b("Biggest change in the EU: "), "out-of-pocket fell 6× more than in any other country.")),
             div(class = "finding", div(class = "n", "3"),
                 div(tags$b("Fairer access: "), "the poor no longer skip care because of cost.")),
             div(class = "finding", div(class = "n", "4"),
                 div(tags$b("Not cheaper: "), "with contributions, people pay about the same, for 38% more care.")),
             div(class = "finding", div(class = "n", "5"),
                 div(tags$b("Gaps remain: "), "outpatient and long-term care, hospital-centred growth."))))
    )
  ),
  
  # ---------------- 2. PATIENTS ----------------
  nav_panel(
    "Patients", icon = bs_icon("person-heart"),
    
    chapter("What changed for patients",
            "Less to pay at the doctor and the pharmacy, but contributions every month."),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("What patients still pay themselves",
                       tags$small("% of spending paid out-of-pocket, 2018 vs 2024")),
           plotlyOutput("pt_oop", height = "380px"),
           card_footer("Hospital care = inpatient + day care. Source: CYSTAT.")),
      card(card_header("Paid when sick vs paid by everyone",
                       tags$small("Per person per year, 2024 euros")),
           plotlyOutput("pt_person", height = "380px"),
           card_footer("Pooled = taxes + GeSY contributions."))
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("What the average person pays directly",
                       tags$small("Out-of-pocket + private insurance + own GeSY contribution")),
           plotlyOutput("pt_burden", height = "360px"),
           card_footer("Own contribution is an estimate (31% of contributions are paid by employees and pensioners).")),
      card(card_header("Did cost stop people seeing a doctor?",
                       tags$small("% who skipped care because it was too expensive, Cyprus")),
           plotlyOutput("pt_access", height = "360px"),
           card_footer("Source: Eurostat EU-SILC. The fall started in 2015, before GeSY."))
    )
  ),
  
  # ---------------- 3. THE MONEY ----------------
  nav_panel(
    "The money", icon = bs_icon("cash-stack"),
    
    chapter("Where the money goes",
            "Spending almost doubled, and most of the new money went to hospitals."),
    
    card(card_header(class = "d-flex justify-content-between align-items-start",
                     div("Spending by type of care", tags$small("2010-2024, current prices")),
                     radioButtons("mn_view", NULL, inline = TRUE,
                                  choices = c("€ million" = "eur", "Share %" = "pct"))),
         plotlyOutput("mn_area", height = "400px"),
         card_footer("Source: CYSTAT 2160012E.")),
    
    layout_columns(
      col_widths = c(7, 5),
      card(card_header(class = "d-flex justify-content-between align-items-start",
                       div("Money flows", tags$small("Who pays → who provides → type of care")),
                       div(style = "width:120px",
                           selectInput("mn_year", NULL, choices = 2024:2010, selected = 2024))),
           plotlyOutput("mn_sankey", height = "430px"),
           card_footer("Hover over a flow to see the amount (EUR million).")),
      card(card_header("Spending per person",
                       tags$small("With and without inflation")),
           plotlyOutput("mn_person", height = "430px"),
           card_footer("Real growth 2018-2024: +46%. Deflated with HICP."))
    )
  ),
  
  # ---------------- 4. EUROPE ----------------
  nav_panel(
    "Europe", icon = bs_icon("globe-europe-africa"),
    
    chapter("Cyprus in Europe",
            "From the highest out-of-pocket share to the EU average, while spending less than most."),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Out-of-pocket share, 2018 → 2023",
                       tags$small("Every EU country, sorted by size of change")),
           plotlyOutput("eu_arrows", height = "620px"),
           card_footer("Source: Eurostat hlth_sha11_hf.")),
      card(card_header("More money, better results?",
                       tags$small("Spending per person vs treatable deaths, 2023")),
           plotlyOutput("eu_scatter", height = "620px"),
           card_footer("Each dot is a country. Dashed line = trend. Not proof of cause and effect."))
    )
  ),
  
  # ---------------- 5. VERDICT ----------------
  nav_panel(
    "Verdict", icon = bs_icon("clipboard-check"),
    
    div(class = "hero",
        div(class = "eyebrow", "Was GeSY good for the population?"),
        h1("Fairer, not cheaper"),
        p("GeSY protects patients from large bills and closed the income gap in access.
           It did not reduce the total cost, and gaps remain in long-term care,
           outpatient costs, efficiency and waiting times.")),
    
    chapter("The GeSY effect",
            "Change in Cyprus minus change in Greece, Malta and Portugal (2016-18 vs 2022-24)."),
    uiOutput("vd_cards"),
    
    chapter("Scorecard", "Thirteen findings against the WHO health-system goals."),
    card(uiOutput("vd_table"),
         card_footer("WHO, HIO, Politis and ILO items come from published sources, not from this analysis."))
  ),
  
  # ---------------- 6. ABOUT ----------------
  nav_panel(
    "About", icon = bs_icon("info-circle"),
    
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("About"),
           card_body(
             p("An analysis of current health expenditure in Cyprus, 2010-2024, before and after the
                General Healthcare System (GeSY), built in R."),
             tags$p(tags$b("Data: "), "CYSTAT health accounts (7 tables, SHA 2011), Eurostat (GDP, population,
                    prices, EU health spending, mortality, EU-SILC), HIO annual report 2023."),
             tags$p(tags$b("Methods: "), "2021 gaps filled with accounting identities (checked against CYSTAT);
                    inflation removed with HICP; difference-in-differences vs Greece, Malta, Portugal."),
             tags$p(tags$b("Limitations: "), "breaks in series (2015, 2021); 2023-2024 provisional;
                    GeSY's hospital phase coincided with COVID-19; descriptive, not causal proof."),
             p(class = "text-muted", "Analysis by Christos Fakontis."))),
      card(card_header("Download the data"),
           card_body(
             selectInput("dl", NULL, choices = c(
               "Who pays"                  = "public_private",
               "Out-of-pocket by care"     = "oop_by_care",
               "Spending by type of care"  = "spending_care",
               "Per person"                = "citizen",
               "Household burden"          = "household",
               "Money flows"               = "flows_fp",
               "EU comparison"             = "eu_spending_all",
               "Difference-in-differences" = "did",
               "Scorecard"                 = "verdict")),
             downloadButton("dl_btn", "Download CSV", class = "btn-primary w-100"))))
  ),
  
  nav_spacer(),
  nav_item(tags$a(bs_icon("github"), " Code", href = "   https://github.com/Chris351999/Cyprus_Healthcare_Expenditure", target = "_blank"))
)


# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ---------------- OVERVIEW ----------------
  
  output$ov_who <- renderPlotly({
    d <- public_private %>%
      transmute(year, "Taxes & GeSY" = public_share, "Out-of-pocket & private" = private_share)
    lines_chart(d, c("Taxes & GeSY" = blue, "Out-of-pocket & private" = orange),
                suffix = "%", digits = 0) %>%
      clean("", band = TRUE, suffix = "%") %>%
      layout(yaxis = list(range = c(0, 100)))
  })
  
  # ---------------- PATIENTS ----------------
  
  output$pt_oop <- renderPlotly({
    d <- oop_by_care %>%
      filter(year %in% c(2018, 2024), care_name != "Administration") %>%
      select(year, care_name, oop_share) %>%
      pivot_wider(names_from = year, values_from = oop_share, names_prefix = "y") %>%
      arrange(y2024)
    
    plot_ly(d) %>%
      add_segments(x = ~y2018, xend = ~y2024, y = ~care_name, yend = ~care_name,
                   line = list(color = "#dfe4ea", width = 6), showlegend = FALSE, hoverinfo = "none") %>%
      add_markers(x = ~y2018, y = ~care_name, name = "2018 (before GeSY)",
                  marker = list(color = grey, size = 14),
                  hovertemplate = "<b>%{y}</b><br>2018: %{x:.0f}%<extra></extra>") %>%
      add_markers(x = ~y2024, y = ~care_name, name = "2024",
                  marker = list(color = blue, size = 14),
                  hovertemplate = "<b>%{y}</b><br>2024: %{x:.0f}%<extra></extra>") %>%
      clean("", hover = "closest", suffix = "%") %>%
      layout(yaxis = list(categoryorder = "array", categoryarray = d$care_name,
                          title = "", ticksuffix = "   ", showgrid = FALSE),
             xaxis = list(showgrid = TRUE, gridcolor = "#eef1f5", ticksuffix = "%", range = c(0, 105)))
  })
  
  output$pt_person <- renderPlotly({
    d <- citizen %>%
      transmute(year, "Paid by everyone (pooled)" = pooled_per_person,
                "Paid when sick (out-of-pocket)" = oop_per_person)
    lines_chart(d, c("Paid by everyone (pooled)" = blue,
                     "Paid when sick (out-of-pocket)" = orange), prefix = "€") %>%
      clean("", band = TRUE, prefix = "€")
  })
  
  output$pt_burden <- renderPlotly({
    d <- household %>%
      mutate(lbl = if_else(year == min(year), paste(year, "· before GeSY"),
                           paste(year, "· with GeSY")))
    parts <- list("Out-of-pocket"         = list(col = "oop_pp",         fill = orange),
                  "Private insurance"     = list(col = "insurance_pp",   fill = "#c3c9d1"),
                  "Own GeSY contribution" = list(col = "own_contrib_pp", fill = blue))
    p <- plot_ly()
    for (nm in names(parts)) {
      v <- d[[parts[[nm]]$col]]
      p <- add_trace(p, x = d$lbl, y = v, type = "bar", name = nm,
                     marker = list(color = parts[[nm]]$fill),
                     text = ifelse(v > 40, fmt_eur(v), ""), textposition = "inside",
                     insidetextfont = list(color = "white", size = 13),
                     hovertemplate = paste0(nm, ": €%{y:,.0f}<extra></extra>"))
    }
    p %>%
      add_annotations(x = d$lbl, y = d$direct_total_pp, yshift = 16, showarrow = FALSE,
                      text = paste0("<b>", fmt_eur(d$direct_total_pp), "</b>"),
                      font = list(size = 15, color = navy)) %>%
      clean("", prefix = "€") %>%
      layout(barmode = "stack", bargap = 0.5)
  })
  
  output$pt_access <- renderPlotly({
    d <- unmet %>%
      filter(country == "Cyprus", reason == "Too expensive",
             income %in% c("Poorest 20%", "Richest 20%")) %>%
      select(year, income, value) %>%
      pivot_wider(names_from = income, values_from = value)
    lines_chart(d, c("Poorest 20%" = orange, "Richest 20%" = blue), suffix = "%", digits = 1) %>%
      clean("", band = TRUE, suffix = "%")
  })
  
  # ---------------- THE MONEY ----------------
  
  output$mn_area <- renderPlotly({
    d <- spending_care %>%
      mutate(group = care_groups[care_name]) %>%
      group_by(year, group) %>%
      summarise(value = sum(value_meur), .groups = "drop")
    if (input$mn_view == "pct") {
      d <- d %>% group_by(year) %>% mutate(value = 100 * value / sum(value)) %>% ungroup()
    }
    suf <- if (input$mn_view == "pct") "%" else ""
    pre <- if (input$mn_view == "pct") "" else "€"
    
    p <- plot_ly()
    for (g in names(group_cols)) {
      dd <- filter(d, group == g)
      p <- add_trace(p, x = dd$year, y = dd$value, name = g, type = "scatter", mode = "lines",
                     stackgroup = "one", line = list(width = 0.5, color = group_cols[[g]]),
                     fillcolor = hex_alpha(group_cols[[g]], 0.85),
                     hovertemplate = paste0(g, ": ", pre, "%{y:,.0f}", suf,
                                            if (suf == "") " m" else "", "<extra></extra>"))
    }
    p %>% clean("", band = TRUE, suffix = suf, prefix = pre)
  })
  
  output$mn_sankey <- renderPlotly({
    f <- bind_rows(flows_fp, flows_pc) %>% filter(year == as.numeric(input$mn_year))
    nodes <- unique(c(f$from, f$to))
    ncol  <- unname(sankey_cols[nodes]); ncol[is.na(ncol)] <- "#64748b"
    src   <- match(f$from, nodes) - 1
    plot_ly(type = "sankey", arrangement = "snap", valueformat = ",.0f", valuesuffix = " m€",
            node = list(label = nodes, color = ncol, pad = 14, thickness = 14, line = list(width = 0)),
            link = list(source = src, target = match(f$to, nodes) - 1, value = f$value_meur,
                        color = hex_alpha(ncol[src + 1], 0.30))) %>%
      layout(font = list(family = "Inter, sans-serif", size = 11),
             margin = list(l = 5, r = 5, t = 10, b = 5), paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displayModeBar = FALSE)
  })
  
  output$mn_person <- renderPlotly({
    d <- health_real %>%
      transmute(year, "2024 prices" = che_per_capita_real, "Current prices" = che_per_capita)
    lines_chart(d, c("2024 prices" = blue, "Current prices" = "#c3c9d1"), prefix = "€") %>%
      clean("", band = TRUE, prefix = "€")
  })
  
  # ---------------- EUROPE ----------------
  
  output$eu_arrows <- renderPlotly({
    d <- eu_spending_all %>%
      filter(indicator == "Out-of-pocket share (%)", year %in% c(2018, 2023)) %>%
      select(country, year, value) %>%
      pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
      filter(!is.na(y2018), !is.na(y2023)) %>%
      mutate(change = y2023 - y2018) %>%
      arrange(desc(change))
    end_col <- ifelse(d$country == "Cyprus", red, "#7c8796")
    
    plot_ly(d) %>%
      add_segments(x = ~y2018, xend = ~y2023, y = ~country, yend = ~country,
                   line = list(color = "#dfe4ea", width = 3), showlegend = FALSE, hoverinfo = "none") %>%
      add_markers(x = ~y2018, y = ~country, name = "2018",
                  marker = list(color = "#dfe4ea", size = 8),
                  hovertemplate = "<b>%{y}</b> 2018: %{x:.1f}%<extra></extra>") %>%
      add_markers(x = ~y2023, y = ~country, name = "2023",
                  marker = list(color = end_col, size = ifelse(d$country == "Cyprus", 14, 9)),
                  customdata = ~change,
                  hovertemplate = "<b>%{y}</b> 2023: %{x:.1f}% (%{customdata:+.1f} pts)<extra></extra>") %>%
      clean("", hover = "closest", suffix = "%") %>%
      layout(showlegend = FALSE,
             yaxis = list(categoryorder = "array", categoryarray = d$country, title = "",
                          tickfont = list(size = 11), ticksuffix = "  ", showgrid = FALSE),
             xaxis = list(showgrid = TRUE, gridcolor = "#eef1f5", ticksuffix = "%"))
  })
  
  output$eu_scatter <- renderPlotly({
    sp  <- eu_spending_all %>% filter(indicator == "Health spending per person (PPS)", year == 2023) %>%
      select(geo, country, spending = value)
    out <- eu_outcomes %>% filter(indicator == "Treatable mortality (per 100,000)", year == 2023) %>%
      select(geo, deaths = value)
    d   <- inner_join(sp, out, by = "geo") %>% filter(!is.na(spending), !is.na(deaths))
    fit   <- lm(deaths ~ log(spending), data = d)
    trend <- tibble(spending = seq(min(d$spending), max(d$spending), length.out = 100))
    trend$deaths <- predict(fit, newdata = trend)
    lab <- d %>% filter(country %in% c("Cyprus", "Greece", "Malta", "Portugal"))
    
    plot_ly() %>%
      add_lines(data = trend, x = ~spending, y = ~deaths, line = list(color = "#cbd2dc", dash = "dash"),
                hoverinfo = "none", showlegend = FALSE) %>%
      add_markers(data = d, x = ~spending, y = ~deaths, text = ~country, showlegend = FALSE,
                  marker = list(color = ifelse(d$country == "Cyprus", red, "#c3c9d1"),
                                size  = ifelse(d$country == "Cyprus", 18, 11),
                                line  = list(color = "white", width = 1.5)),
                  hovertemplate = "<b>%{text}</b><br>%{x:,.0f} PPS per person<br>%{y:.0f} treatable deaths per 100,000<extra></extra>") %>%
      add_text(data = lab, x = ~spending, y = ~deaths, text = ~country, textposition = "top center",
               textfont = list(size = 12, color = ifelse(lab$country == "Cyprus", red, "#4b5563")),
               showlegend = FALSE, hoverinfo = "none") %>%
      clean("Treatable deaths per 100,000", hover = "closest") %>%
      layout(xaxis = list(title = "Health spending per person (PPS)", showgrid = TRUE, gridcolor = "#eef1f5"))
  })
  
  # ---------------- VERDICT ----------------
  
  output$vd_cards <- renderUI({
    cards <- lapply(seq_len(nrow(did)), function(i) {
      card(card_header(did$indicator[i]),
           plotlyOutput(paste0("did_", i), height = "230px"),
           card_footer(HTML(sprintf("<span style='color:#1f2937;font-size:.95rem'><b>GeSY effect: %+.1f points</b></span>",
                                    did$did_estimate[i]))))
    })
    do.call(layout_column_wrap, c(list(width = 1/3, fill = FALSE), cards))
  })
  
  lapply(seq_len(nrow(did)), function(i) {
    output[[paste0("did_", i)]] <- renderPlotly({
      r <- did[i, ]
      plot_ly(x = c("Cyprus", "Greece, Malta, Portugal"),
              y = c(r$cyprus_change, r$comparators_change), type = "bar",
              marker = list(color = c(red, "#c3c9d1")),
              text = sprintf("%+.1f", c(r$cyprus_change, r$comparators_change)),
              textposition = "outside", cliponaxis = FALSE, hoverinfo = "none") %>%
        clean("", hover = FALSE) %>%
        layout(yaxis = list(rangemode = "normal", zeroline = TRUE, zerolinecolor = "#cbd2dc",
                            showticklabels = FALSE, showgrid = FALSE),
               bargap = 0.55)
    })
  })
  
  output$vd_table <- renderUI({
    tags$table(
      class = "table align-middle mb-0",
      tags$tbody(lapply(seq_len(nrow(verdict)), function(i) {
        v <- verdict[i, ]
        tags$tr(
          tags$td(style = "width:110px",
                  tags$span(class = "pill", style = paste0("background:", rating_cols[[v$rating]]), v$rating)),
          tags$td(tags$div(tags$b(v$dimension)), tags$div(v$finding)),
          tags$td(class = "text-end text-muted", style = "width:180px", tags$small(v$source)))
      }))
    )
  })
  
  # ---------------- DOWNLOAD ----------------
  
  datasets <- list(public_private = public_private, oop_by_care = oop_by_care,
                   spending_care = spending_care, citizen = citizen, household = household,
                   flows_fp = flows_fp, eu_spending_all = eu_spending_all,
                   did = did, verdict = verdict)
  
  output$dl_btn <- downloadHandler(
    filename = function() paste0("cyprus_health_", input$dl, ".csv"),
    content  = function(file) write_csv(datasets[[input$dl]], file)
  )
}


# ============================================================
# 7. RUN
# ============================================================

shinyApp(ui, server)
