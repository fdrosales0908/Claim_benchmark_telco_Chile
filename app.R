# app.R
# =========================================================
# Shiny App: Text Mining Reclamos (Scraping + NLP + Bigrams)
# =========================================================

library(shiny)
library(shinycssloaders)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(tidyr)
library(tibble)
library(readr)
library(rvest)
library(lubridate)
library(ggplot2)
library(tidytext)
library(stopwords)
library(igraph)
library(ggraph)

# --- opcionales (si no están, desactivamos features) ---
has_spacyr <- requireNamespace("spacyr", quietly = TRUE)

# =========================
# 1) Scraper functions
# =========================
scrape_page <- function(url_base, page_number) {
  url <- if (page_number == 0) url_base else paste0(url_base, "?page=", page_number)
  
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) NULL
  )
  if (is.null(page)) return(tibble())
  
  rows <- page %>% html_nodes("table tbody tr")
  if (length(rows) == 0) return(tibble())
  
  purrr::map_df(rows, function(row) {
    celdas <- row %>% html_nodes("td")
    if (length(celdas) < 3) return(NULL)
    
    fecha <- celdas[1] %>% html_text(trim = TRUE)
    enlace_nodo <- celdas[2] %>% html_node("a")
    if (length(enlace_nodo) == 0) return(NULL)
    
    asunto <- enlace_nodo %>% html_text(trim = TRUE)
    link_rel <- enlace_nodo %>% html_attr("href")
    link <- paste0("https://www.reclamos.cl", link_rel)
    id <- str_extract(link, "\\d+$")
    estado <- celdas[3] %>% html_text(trim = TRUE)
    
    tibble(id = id, fecha = fecha, asunto = asunto, estado = estado, link = link)
  })
}

scrape_company <- function(company_name, url_base, n_pages = 15, pause_sec = 0.6) {
  pages <- 0:(n_pages - 1)
  out <- map_df(pages, function(p) {
    Sys.sleep(pause_sec)
    df <- scrape_page(url_base, p)
    if (!is.null(df) && nrow(df) > 0) df <- mutate(df, page = p)
    df
  })
  if (is.null(out) || nrow(out) == 0) return(tibble())
  out %>% mutate(empresa = company_name, .before = 1)
}

# =========================
# 2) NLP helpers
# =========================
get_stopwords <- function() {
  stop_es <- stopwords::stopwords("es")
  stop_dom <- c(
    "reclamo","problema","consulta","respuesta",
    "empresa","servicio","plan","linea","telefono",
    "cliente","caso","solicitud","wom","claro","entel","movistar","chile",
    "hacer","decir","decirme","informar","contar","comentar",
    "pedir","solicitar","responder","esperar","seguir","volver",
    "querer","poder","tener","dar","dejar","poner","ver","saber",
    "situacion","tema","asunto","solucion","atencion","usuario","persona",
    "dia","dias","mes","meses","año","años","hoy","ayer","ahora","momento","fecha",
    "mucho","poco","varios","varias","siempre","nunca","ademas","igual","mismo","solo","tambien","todo","toda",
    "porque","cuando","donde","como","entonces","pero","ya",
    "won"
  )
  list(stop_es = stop_es, stop_dom = stop_dom)
}

get_diccionario_norm <- function() {
  tibble::tribble(
    ~from,      ~to,
    "sexi",     "sexy",
    "sexyy",    "sexy",
    "sexxy",    "sexy",
    "glamourr", "glamour"
  )
}

normalize_dates <- function(base_reclamos) {
  base_reclamos %>%
    mutate(
      fecha_txt = fecha %>%
        as.character() %>%
        str_trim() %>%
        str_replace_all("[./]", "-") %>%
        str_replace_all("\\s+", " "),
      fecha_date_4 = suppressWarnings(dmy(fecha_txt)),
      fecha_date_2 = suppressWarnings(dmy(fecha_txt, truncated = 2)),
      fecha_date   = coalesce(fecha_date_4, fecha_date_2),
      fecha_date   = if_else(!is.na(fecha_date) & year(fecha_date) < 100,
                             update(fecha_date, year = 2000 + year(fecha_date)),
                             fecha_date)
    ) %>%
    select(-fecha_date_4, -fecha_date_2)
}

make_tokens <- function(base_reclamos2, diccionario_norm, stop_es, stop_dom) {
  base_reclamos2 <- base_reclamos2 %>%
    mutate(
      row_id  = row_number(),
      empresa = str_to_lower(empresa),
      mes     = floor_date(fecha_date, "month"),
      anio    = year(fecha_date)
    )
  
  tokens_clean <- base_reclamos2 %>%
    filter(!is.na(fecha_date)) %>%
    select(row_id, empresa, mes, anio, fecha_date, asunto) %>%
    mutate(asunto = tidyr::replace_na(asunto, "")) %>%
    mutate(
      asunto = str_to_lower(asunto),
      asunto = stringi::stri_trans_general(asunto, "Latin-ASCII"),
      asunto = str_replace_all(asunto, "[[:punct:]]+", " ")
    ) %>%
    tidytext::unnest_tokens(word, asunto) %>%
    mutate(
      word = str_to_lower(word),
      word = stringi::stri_trans_general(word, "Latin-ASCII"),
      word = str_replace_all(word, "[0-9]+", ""),
      word = str_replace_all(word, "[[:punct:]]+", ""),
      word = str_squish(word)
    ) %>%
    filter(str_detect(word, "^[[:alpha:]]+$"), nchar(word) > 2) %>%
    left_join(diccionario_norm, by = c("word" = "from")) %>%
    mutate(word = coalesce(to, word)) %>%
    select(-to) %>%
    filter(!word %in% c(stop_es, stop_dom))
  
  list(base = base_reclamos2, tokens = tokens_clean)
}

lemmatize_spacy <- function(tokens_clean) {
  df_words <- tokens_clean %>%
    distinct(word) %>%
    transmute(doc_id = row_number(), text = word)
  
  lemma_map <- spacyr::spacy_parse(df_words, lemma = TRUE, pos = TRUE) %>%
    select(doc_id, token, lemma, pos)
  
  lemma_map2 <- lemma_map %>%
    transmute(
      word  = str_to_lower(token),
      lemma = str_to_lower(lemma),
      pos   = pos
    ) %>%
    distinct(word, .keep_all = TRUE) %>%
    filter(pos %in% c("NOUN","ADJ","VERB")) %>%
    filter(!lemma %in% c("", "ser", "estar", "haber"))
  
  tokens_clean %>%
    left_join(lemma_map2 %>% select(word, lemma, pos), by = "word") %>%
    mutate(word_lemma = if_else(!is.na(lemma) & lemma != "", lemma, word)) %>%
    select(row_id, empresa, mes, anio, fecha_date, word, word_lemma, pos)
}

make_base_lemma_mes <- function(tokens_lemma_ok) {
  tokens_lemma_ok %>%
    filter(!is.na(mes), anio %in% c(2023, 2024, 2025),
           !is.na(word_lemma), word_lemma != "") %>%
    count(empresa, anio, mes, word_lemma, name = "n") %>%
    group_by(empresa, anio, mes) %>%
    mutate(total_mes = sum(n),
           pct_mes = n / total_mes) %>%
    ungroup()
}

make_bigrams <- function(tokens_lemma_ok, diccionario_norm) {
  tokens_big <- tokens_lemma_ok %>%
    mutate(
      empresa = str_to_lower(empresa),
      anio = if_else(!is.na(anio), anio, year(mes))
    ) %>%
    filter(anio %in% c(2023, 2024, 2025)) %>%
    transmute(row_id, empresa, mes, anio, word_lemma = str_to_lower(word_lemma)) %>%
    filter(!is.na(word_lemma), word_lemma != "", str_detect(word_lemma, "^[[:alpha:]]+$")) %>%
    left_join(diccionario_norm, by = c("word_lemma" = "from")) %>%
    mutate(word_norm = if_else(!is.na(to) & to != "", to, word_lemma)) %>%
    select(-to) %>%
    group_by(row_id) %>%
    mutate(pos = row_number()) %>%
    ungroup()
  
  bigrams <- tokens_big %>%
    arrange(row_id, pos) %>%
    group_by(row_id, empresa, mes, anio) %>%
    mutate(word2 = lead(word_norm)) %>%
    ungroup() %>%
    filter(!is.na(word2), word2 != "", word_norm != word2) %>%
    mutate(bigram = paste(word_norm, word2)) %>%
    select(row_id, empresa, mes, anio, bigram)
  
  bigrams_freq <- bigrams %>%
    count(row_id, empresa, mes, anio, bigram, name = "freq")
  
  list(bigrams = bigrams, bigrams_freq = bigrams_freq)
}

get_dic_categorias <- function() {
  tribble(
    ~categoria,                        ~patron,
    "Suscripción no deseada",          "(suscrib|suscrip|alta\\s*.*suscrip|cobro\\s*.*suscrip)",
    "Contenido adulto",                "(glamour|sexy|hotzone|porno|adult|hot|plati|pienzzo)",
    "Cobro indebido",                  "(cobro|cobraron|cargo|factur|cargar|cobran)",
    "Bloqueo / Restricción",           "(bloque|restric|inhabilit|no\\s*habilit|portabil)",
    "Acceso / App",                    "(\\bapp\\b|web|acceso|clave|login|cuenta|sesion|ingres)",
    "Atención / Callcenter / Visita",  "(call|ejecutiv|atenci|soporte|sucursal|visita|visit|tecn)",
    "Internet / red",                  "(wifi|wi\\s*fi|antena|internet|fibra|senal|señal|red|corte|caid|conex|veloc)",
    "Precio / alza",                   "(precio|alza|recarg|prorr|cobro\\s*extra|tarifa|valor)",
    "Llamadas",                        "(llamad|insis|acoso|hostig|spam)",
    "Contrato / Equipo",               "(reembolso|demora|prepago|plan|sim|chip|contrat|fraud|compra|garant|devoluci|entrega|env)"
  ) %>% mutate(prioridad = row_number())
}

assign_categories <- function(bigrams_freq) {
  dic_categorias <- get_dic_categorias()
  
  bigrams_cat_1 <- bigrams_freq %>%
    tidyr::crossing(dic_categorias) %>%
    filter(str_detect(bigram, patron)) %>%
    group_by(row_id, empresa, mes, anio, bigram) %>%
    slice_min(order_by = prioridad, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(row_id, empresa, mes, anio, bigram, freq, categoria)
  
  bigrams_cat_final <- bigrams_freq %>%
    left_join(bigrams_cat_1, by = c("row_id","empresa","mes","anio","bigram","freq")) %>%
    mutate(categoria = if_else(is.na(categoria), "Otros", categoria))
  
  patron_intencion_suscripcion <- paste0(
    "(",
    "no\\s*autor|sin\\s*consent|desconoz|no\\s*reconoz|",
    "nunca\\s*solicit|jam[aá]s\\s*solicit|",
    "me\\s*activ|activaron|alta\\s*autom|servicio\\s*activo|",
    "me\\s*inscrib|inscribieron|",
    "cargo\\s*realiz|cobro\\s*realiz|cobro\\s*no\\s*correspond|",
    "me\\s*cobraron|cobran\\s*sin|",
    "no\\s*contrat|no\\s*solicit|no\\s*ped[ií]|",
    "desconozco\\s*servicio|",
    "me\\s*llego\\s*cobro|aparec(e|io)\\s*cobro|",
    "se\\s*activo\\s*solo",
    ")"
  )
  
  bigrams_cat_final %>%
    mutate(
      categoria = case_when(
        categoria == "Otros" & str_detect(bigram, patron_intencion_suscripcion) ~ "Suscripción no deseada",
        TRUE ~ categoria
      )
    )
}

make_cooc_edges <- function(bigrams) {
  bigrams %>%
    separate(bigram, into = c("w1", "w2"), sep = " ", remove = FALSE) %>%
    filter(!is.na(w1), !is.na(w2), w1 != w2) %>%
    count(empresa, w1, w2, name = "n", sort = TRUE)
}

ggraph_plot_safe <- function(edges_df, emp, min_n = 5, top_n = 60) {
  edges <- edges_df %>%
    filter(empresa == emp, n >= min_n) %>%
    arrange(desc(n)) %>%
    slice_head(n = top_n) %>%
    mutate(n = as.numeric(n)) %>%
    filter(!is.na(n), n > 0) %>%
    select(w1, w2, n)
  
  validate(need(nrow(edges) >= 2, paste0("Muy pocos enlaces para ", emp, ". Baja min_n o sube top_n.")))
  
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
  validate(need(igraph::ecount(g) >= 1 && igraph::vcount(g) >= 2, paste0("Grafo sin enlaces suficientes en ", emp)))
  
  cl <- tryCatch(igraph::cluster_louvain(g, weights = igraph::E(g)$n), error = function(e) NULL)
  igraph::V(g)$community <- if (!is.null(cl)) as.integer(igraph::membership(cl)) else as.integer(igraph::components(g)$membership)
  
  ggraph::ggraph(g, layout = "fr") +
    ggraph::geom_edge_link(aes(width = n), alpha = 0.6) +
    ggraph::geom_node_point(aes(color = factor(community)), size = 4) +
    ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    ggraph::scale_edge_width(range = c(0.3, 2)) +
    guides(color = "none", width = "none") +
    labs(title = paste("Red de co-ocurrencias –", emp)) +
    theme_void()
}

# =========================
# 3) UI
# =========================
ui <- fluidPage(
  titlePanel("Text Mining Reclamos (Reclamos.cl) – Shiny"),
  sidebarLayout(
    sidebarPanel(
      h4("Fuente de datos"),
      radioButtons(
        "data_mode", "¿Cómo cargas datos?",
        choices = c("Scrapear ahora" = "scrape", "Subir CSV" = "upload"),
        selected = "upload"
      ),
      
      conditionalPanel(
        condition = "input.data_mode == 'upload'",
        fileInput("csv_file", "Sube base_reclamos.csv", accept = c(".csv")),
        checkboxInput("guess_delim", "Intentar leer con readr (rápido)", TRUE)
      ),
      
      conditionalPanel(
        condition = "input.data_mode == 'scrape'",
        checkboxGroupInput(
          "empresas_sel", "Empresas",
          choices = c("entel","movistar","wom","claro"),
          selected = c("entel","movistar","wom","claro")
        ),
        numericInput("n_pages", "N páginas por empresa", value = 15, min = 1, max = 50),
        sliderInput("pause_sec", "Pausa entre páginas (seg)", min = 0, max = 3, value = 0.6, step = 0.1),
        actionButton("run_scrape", "▶ Ejecutar scraping", class = "btn-primary"),
        br(), br(),
        checkboxInput("save_cache", "Guardar cache RDS (recomendado)", TRUE)
      ),
      
      hr(),
      h4("NLP / Visualización"),
      checkboxInput("do_spacy", "Lemmatizar con spaCy (spacyr)", TRUE),
      helpText("Si spacyr/spaCy no está instalado, se desactivará solo y usará fallback."),
      selectInput("empresa_plot", "Empresa para red", choices = c("entel","movistar","wom","claro"), selected = "entel"),
      sliderInput("min_edge", "Red: min frecuencia enlace", min = 1, max = 20, value = 5, step = 1),
      sliderInput("top_edge", "Red: top enlaces", min = 20, max = 200, value = 60, step = 10)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vista previa",
                 tableOutput("tbl_preview") %>% withSpinner()),
        
        tabPanel("Serie mensual",
                 plotOutput("p_time", height = 420) %>% withSpinner()),
        
        tabPanel("Top lemas",
                 plotOutput("p_top_lemmas", height = 520) %>% withSpinner()),
        
        tabPanel("Dolores",
                 plotOutput("p_dolores_freq", height = 520) %>% withSpinner(),
                 plotOutput("p_dolores_pct", height = 520) %>% withSpinner()),
        
        tabPanel("Heatmap lemas",
                 plotOutput("p_heat_lemmas", height = 520) %>% withSpinner()),
        
        tabPanel("Bigramas",
                 plotOutput("p_top_bigrams", height = 520) %>% withSpinner()),
        
        tabPanel("Categorías",
                 plotOutput("p_cat_anio", height = 520) %>% withSpinner(),
                 plotOutput("p_suscrip_mes", height = 520) %>% withSpinner(),
                 plotOutput("p_heat_cat", height = 520) %>% withSpinner()),
        
        tabPanel("Red co-ocurrencias",
                 plotOutput("p_network", height = 650) %>% withSpinner()),
        
        tabPanel("Descargar",
                 downloadButton("dl_csv", "Descargar base_reclamos (CSV)"),
                 downloadButton("dl_rds", "Descargar objetos (RDS)"))
      )
    )
  )
)

# =========================
# 4) Server
# =========================
server <- function(input, output, session) {
  
  spacy_ready <- reactiveVal(FALSE)
  
  # Intentar inicializar spaCy SOLO si el usuario lo pide
  observeEvent(input$do_spacy, {
    if (!isTRUE(input$do_spacy)) return()
    
    if (!has_spacyr) {
      showNotification("spacyr no está instalado. Se desactiva lematización (fallback).", type = "warning")
      updateCheckboxInput(session, "do_spacy", value = FALSE)
      spacy_ready(FALSE)
      return()
    }
    
    if (spacy_ready()) return()
    
    ok <- tryCatch({
      spacyr::spacy_initialize(model = "es_core_news_sm")
      TRUE
    }, error = function(e) {
      showNotification(
        paste0("Error spacy_initialize: ", e$message, " | Se desactiva lematización (fallback)."),
        type = "error",
        duration = 10
      )
      updateCheckboxInput(session, "do_spacy", value = FALSE)
      FALSE
    })
    
    spacy_ready(ok)
  }, ignoreInit = TRUE)
  
  # --- data store ---
  base_raw <- reactiveVal(NULL)
  
  # --- Load by upload ---
  observeEvent(input$csv_file, {
    req(input$data_mode == "upload")
    f <- input$csv_file$datapath
    validate(need(!is.null(f), "Sube un CSV."))
    
    df <- tryCatch({
      if (isTRUE(input$guess_delim)) {
        readr::read_csv(f, show_col_types = FALSE)
      } else {
        read.csv(f, stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      showNotification(paste("No pude leer el CSV:", e$message), type = "error")
      NULL
    })
    
    base_raw(df)
  })
  
  # --- Scrape ---
  observeEvent(input$run_scrape, {
    req(input$data_mode == "scrape")
    validate(need(length(input$empresas_sel) > 0, "Selecciona al menos 1 empresa."))
    
    empresas_all <- tribble(
      ~empresa,   ~url_base,
      "entel",    "https://www.reclamos.cl/taxonomy/term/1",
      "movistar", "https://www.reclamos.cl/empresa/movistar",
      "wom",      "https://www.reclamos.cl/empresa/wom",
      "claro",    "https://www.reclamos.cl/empresa/claro"
    )
    empresas <- empresas_all %>% filter(empresa %in% input$empresas_sel)
    
    withProgress(message = "Scrapeando reclamos.cl", value = 0, {
      n <- nrow(empresas)
      out <- pmap_df(empresas, function(empresa, url_base) {
        incProgress(1/n, detail = paste("Empresa:", empresa))
        scrape_company(empresa, url_base, n_pages = input$n_pages, pause_sec = input$pause_sec)
      })
      base_raw(out)
      
      if (isTRUE(input$save_cache) && !is.null(out) && nrow(out) > 0) {
        try(saveRDS(out, file = "cache_base_reclamos.rds"), silent = TRUE)
      }
    })
  })
  
  # --- Preview table ---
  output$tbl_preview <- renderTable({
    df <- base_raw()
    req(df)
    head(df, 20)
  })
  
  # --- Core pipeline ---
  pipeline <- reactive({
    df <- base_raw()
    req(df)
    
    validate(need(all(c("empresa","fecha","asunto") %in% names(df)), "Tu base debe tener columnas: empresa, fecha, asunto."))
    
    diccionario_norm <- get_diccionario_norm()
    sw <- get_stopwords()
    base_reclamos2 <- normalize_dates(df)
    
    tok <- make_tokens(base_reclamos2, diccionario_norm, sw$stop_es, sw$stop_dom)
    tokens_clean <- tok$tokens
    base2 <- tok$base
    
    # Lemmatización SAFE
    if (isTRUE(input$do_spacy) && has_spacyr && spacy_ready()) {
      tokens_lemma_ok <- lemmatize_spacy(tokens_clean)
    } else {
      tokens_lemma_ok <- tokens_clean %>%
        mutate(word_lemma = word, pos = NA_character_) %>%
        select(row_id, empresa, mes, anio, fecha_date, word, word_lemma, pos)
    }
    
    base_lemma_mes <- make_base_lemma_mes(tokens_lemma_ok)
    big_obj <- make_bigrams(tokens_lemma_ok, diccionario_norm)
    bigrams <- big_obj$bigrams
    bigrams_freq <- big_obj$bigrams_freq
    bigrams_cat_final <- assign_categories(bigrams_freq)
    cooc_edges <- make_cooc_edges(bigrams)
    
    list(
      base = base2,
      tokens_clean = tokens_clean,
      tokens_lemma_ok = tokens_lemma_ok,
      base_lemma_mes = base_lemma_mes,
      bigrams = bigrams,
      bigrams_freq = bigrams_freq,
      bigrams_cat_final = bigrams_cat_final,
      cooc_edges = cooc_edges
    )
  })
  
  # =========================
  # Plots
  # =========================
  output$p_time <- renderPlot({
    obj <- pipeline()
    base2 <- obj$base
    
    reclamos_mes <- base2 %>%
      filter(!is.na(fecha_date)) %>%
      mutate(mes = floor_date(fecha_date, unit = "month")) %>%
      count(empresa, mes, name = "n_reclamos") %>%
      mutate(anio = year(mes))
    
    ggplot(
      reclamos_mes %>% filter(anio %in% c(2023, 2024, 2025)),
      aes(x = mes, y = n_reclamos, color = empresa, group = empresa)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~ anio, scales = "free_x", ncol = 1) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(
        title = "Cantidad de reclamos por mes y empresa (2023–2025)",
        x = "Mes", y = "N° reclamos", color = "Empresa"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "bottom")
  })
  
  output$p_top_lemmas <- renderPlot({
    obj <- pipeline()
    tokens_lemma_ok <- obj$tokens_lemma_ok
    
    top_n <- 15
    top_lemas_empresa <- tokens_lemma_ok %>%
      filter(!is.na(word_lemma), word_lemma != "") %>%
      count(empresa, word_lemma, sort = TRUE) %>%
      group_by(empresa) %>%
      slice_max(n, n = top_n, with_ties = FALSE) %>%
      ungroup()
    
    ggplot(top_lemas_empresa,
           aes(x = tidytext::reorder_within(word_lemma, n, empresa), y = n, fill = empresa)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ empresa, scales = "free") +
      coord_flip() +
      tidytext::scale_x_reordered() +
      labs(title = paste0("Top ", top_n, " lemas por empresa"),
           x = NULL, y = "Frecuencia") +
      theme_minimal()
  })
  
  output$p_dolores_freq <- renderPlot({
    obj <- pipeline()
    base_lemma_mes <- obj$base_lemma_mes
    
    dolores <- c("suscribir","suscripcion","cobrar","cobro","cargo","factura","baja","bloquear","robar","estafa")
    serie_dolores <- base_lemma_mes %>% filter(word_lemma %in% dolores)
    
    ggplot(serie_dolores, aes(x = mes, y = n, color = word_lemma)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ empresa, scales = "free_y") +
      scale_x_date(date_labels = "%Y-%m") +
      labs(title = "Evolución mensual de lemas clave por empresa",
           x = "Mes", y = "Frecuencia", color = "Lemma") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$p_dolores_pct <- renderPlot({
    obj <- pipeline()
    base_lemma_mes <- obj$base_lemma_mes
    
    dolores <- c("suscribir","suscripcion","cobrar","cobro","cargo","factura","baja","bloquear","robar","estafa")
    serie_dolores <- base_lemma_mes %>% filter(word_lemma %in% dolores)
    
    ggplot(serie_dolores, aes(x = mes, y = pct_mes, color = word_lemma)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ empresa, scales = "free_y") +
      scale_x_date(date_labels = "%Y-%m") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(title = "Evolución mensual (proporción) de lemas clave por empresa",
           x = "Mes", y = "% del vocabulario mensual", color = "Lemma") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$p_heat_lemmas <- renderPlot({
    obj <- pipeline()
    base_lemma_mes <- obj$base_lemma_mes
    
    topK <- 5
    top_lemmas <- base_lemma_mes %>%
      group_by(word_lemma) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      slice_max(n, n = topK, with_ties = FALSE) %>%
      pull(word_lemma)
    
    heat_df <- base_lemma_mes %>% filter(word_lemma %in% top_lemmas)
    
    ggplot(heat_df, aes(x = mes, y = word_lemma, fill = n)) +
      geom_tile() +
      facet_grid(empresa ~ anio, scales = "free_x", space = "free_x") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(title = paste0("Heatmap (Top ", topK, " lemas globales) por mes y empresa (2023–2025)"),
           x = "Mes", y = "Lema", fill = "Frecuencia") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank())
  })
  
  output$p_top_bigrams <- renderPlot({
    obj <- pipeline()
    bigrams <- obj$bigrams
    
    top10_bigrams_emp <- bigrams %>%
      count(empresa, bigram, sort = TRUE, name = "n") %>%
      group_by(empresa) %>%
      slice_max(n, n = 10, with_ties = FALSE) %>%
      ungroup()
    
    ggplot(top10_bigrams_emp,
           aes(x = tidytext::reorder_within(bigram, n, empresa), y = n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~ empresa, scales = "free_y") +
      tidytext::scale_x_reordered() +
      labs(
        title = "Top 10 bigramas por empresa (asunto lematizado/normalizado)",
        x = NULL, y = "Frecuencia"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$p_cat_anio <- renderPlot({
    obj <- pipeline()
    bigrams_cat_final <- obj$bigrams_cat_final
    
    cat_empresa_anio <- bigrams_cat_final %>%
      filter(anio %in% c(2023, 2024, 2025)) %>%
      group_by(empresa, anio, categoria) %>%
      summarise(n = sum(freq), .groups = "drop")
    
    ggplot(cat_empresa_anio, aes(x = factor(anio), y = n, fill = categoria)) +
      geom_col() +
      facet_wrap(~ empresa, ncol = 2, scales = "free_y") +
      labs(
        title = "Categorías de reclamo (bigrams) por empresa y año",
        x = "Año", y = "Frecuencia (suma de bigramas)", fill = "Categoría"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$p_suscrip_mes <- renderPlot({
    obj <- pipeline()
    bigrams_cat_final <- obj$bigrams_cat_final
    
    suscrip_mes <- bigrams_cat_final %>%
      filter(categoria == "Suscripción no deseada", anio %in% c(2023, 2024, 2025)) %>%
      group_by(empresa, mes) %>%
      summarise(n = sum(freq), .groups = "drop")
    
    ggplot(suscrip_mes, aes(x = mes, y = n, color = empresa)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~ empresa, scales = "free_y", ncol = 1) +
      scale_x_date(date_labels = "%Y-%m") +
      labs(
        title = "Evolución mensual: Suscripción no deseada",
        subtitle = "Incluye reclasificación semántica desde 'Otros'",
        x = "Mes", y = "Frecuencia"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  output$p_heat_cat <- renderPlot({
    obj <- pipeline()
    bigrams_cat_final <- obj$bigrams_cat_final
    
    heat_df <- bigrams_cat_final %>%
      filter(anio %in% c(2023, 2024, 2025)) %>%
      group_by(empresa, mes, categoria) %>%
      summarise(n = sum(freq), .groups = "drop")
    
    ggplot(heat_df, aes(x = mes, y = categoria, fill = n)) +
      geom_tile() +
      facet_wrap(~ empresa, ncol = 2, scales = "free_x") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      labs(
        title = "Heatmap: categorías de reclamo por mes y empresa",
        x = "Mes", y = "Categoría", fill = "Frecuencia"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ✅ AQUÍ está lo que querías: la red funciona para TODAS (según selector)
  output$p_network <- renderPlot({
    obj <- pipeline()
    edges <- obj$cooc_edges
    req(edges)
    ggraph_plot_safe(edges, input$empresa_plot, min_n = input$min_edge, top_n = input$top_edge)
  })
  
  # =========================
  # Downloads
  # =========================
  output$dl_csv <- downloadHandler(
    filename = function() paste0("base_reclamos_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- base_raw()
      req(df)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$dl_rds <- downloadHandler(
    filename = function() paste0("pipeline_objects_", Sys.Date(), ".rds"),
    content = function(file) {
      obj <- pipeline()
      saveRDS(obj, file = file)
    }
  )
}

shinyApp(ui, server)
