# 📊 Shiny App – TELCO Complaints Text Mining (Chile)

🔗 **Live Shiny Application:**  
https://latency-fraud-analysis.shinyapps.io/claim_reclamos/

This Shiny app analyzes **customer complaints from Reclamos.com** related to the main telecom companies in Chile (**Entel, Claro, Movistar, WOM**).  
It is a **Data Analytics + Compliance portfolio project**, focused on understanding risk signals, recurring issues, and consumer pain points through **web scraping and text mining**.

---

## 🧠 Project Summary (Context)

The goal of this project is to explore complaints reported on Reclamos.com for the main TELCO companies in Chile.

- Data was collected from the first **15 pages** of the website (around **6000 complaints** total).
- For analysis, the project includes about **1500 complaints per company** (Entel, Claro, Movistar, WOM).
- These companies represent almost all the TELCO market in Chile (according to SUBTEL industry information).

Using **R**, the project applies:
- **Web scraping** to collect complaint text,
- **Text mining / NLP** to clean and normalize language,
- **Exploratory visual analysis** to find patterns and main pain points.

This analysis does **not** aim to say which company has more complaints.  
The goal is to understand the **content** of complaints and identify **common topics** that can be useful for monitoring, compliance, and consumer protection.

---

## 🎯 Research Questions (NLP Focus)

This app uses NLP to answer questions like:

- What are the **main pain points** in TELCO complaints?
- Where do patterns appear for **billing issues, subscriptions, technical problems**, etc.?
- Are there **text differences by company**?

---

## ✨ Main Features

- Complaint text cleaning and normalization (basic NLP pipeline)
- Word frequency analysis
- Top words / lemmas by company
- Main pain points view (topic-style grouping / keyword signals)
- Heatmap of frequent terms (lemmas)
- Bigrams (common word pairs)
- Co-occurrence network (word relationship graph)
- Interactive filters and exploration in Shiny

---

## 📸 Application Screenshots (in app order)

### 1) Overview
![Overview](screenshots/overview.png)

### 2) Top Lemma
![Top Lemma](screenshots/top_lemma.png)

### 3) Main Pain
![Main Pain](screenshots/main_pain.png)

### 4) Heatmap (Lemmas)
![Heatmap Lemmas](screenshots/heatmap_lemmas.png)

### 5) Bigrams
![Bigrams](screenshots/bigrama.png)

### 6) Co-occurrences Network
![Co-occurrences](screenshots/co_occurrencias.png)

---

## 🔒 Data Privacy Notice (Important)

This repository is designed to be **public and safe**.

✅ The repo **does NOT include sensitive or private data**.  
✅ If you use real complaint data, you should store it locally and exclude it with `.gitignore`.

Suggested approach:
- Keep data inside a `data/` folder (local only)
- Add `data/` to `.gitignore`
- Use a small **example dataset** if you want to show structure

---

## 📁 Project Structure (Example)

```text
claim_reclamos/
├── app.R                       # Shiny app entry point (or ui.R + server.R)
├── R/                          # Helper functions (cleaning, NLP, plots)
├── data/                       # Local data (ignored in GitHub)
├── screenshots/                # Images used in README
├── rsconnect/                  # ShinyApps.io deployment files (optional)
├── README.md                   # Project description
├── .gitignore                  # Prevent sensitive files from upload
└── claim_reclamos.Rproj        # RStudio project file
