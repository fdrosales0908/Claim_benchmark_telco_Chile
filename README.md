# 📊 Shiny App – TELCO Complaints Text Mining (Chile)

🔗 **Live Shiny Application:**  
https://latency-fraud-analysis.shinyapps.io/reclamos/

This Shiny app analyzes **customer complaints published on Reclamos.cl** related to the main telecom companies in Chile (**Entel, Claro, Movistar, WOM**).

**Reclamos.cl** is an independent Chilean digital platform that works as a public forum where consumers can:
- publish complaints,
- express dissatisfaction,
- and request solutions for negative experiences with companies.

The platform acts as a public reputation space and, in some cases, as a mediator between consumers and companies for small disputes.

This project is part of my **Data Analytics and Compliance portfolio**, with a focus on **consumer protection, monitoring, and text-based risk analysis**.

---

## 🧠 Project Summary (Context)

The objective of this project is to explore and analyze **qualitative information** from telecom-related complaints using **web scraping and text mining techniques in R**.

- Complaints were collected from the first **15 pages** of Reclamos.cl.
- The final dataset includes around **6,000 complaints** published between **2023 and 2025**.
- For text analysis, approximately **1,500 complaints per company** were analyzed (Entel, Claro, Movistar, WOM).
- Together, these companies represent **more than 90% of the Chilean telecom market**, according to SUBTEL industry data.

Text mining techniques were applied to identify **common topics, recurring problems, and main user pain points**.

---

## 🎯 Research Questions (NLP Focus)

This application uses **Natural Language Processing (NLP)** to answer questions such as:

- What are the **main pain points** mentioned in telecom complaints?
- Where do patterns appear related to **billing issues, subscriptions, or technical problems**?
- Are there **differences in complaint topics by company**?

⚠️ This analysis **does not aim to compare the volume of complaints between companies**, but rather to understand **what users are complaining about**.

---

## 🧩 Methods and Tools

Using **R**, the project applies:

- **Web scraping** to collect complaint texts
- **Text cleaning and normalization**
- **Lemmatization using spaCy (spacyr)**
- Word frequency analysis
- Exploratory text mining and visualization
- Comparison of topics across companies

---

## ✨ Main Features

- Cleaning and normalization of complaint texts
- Word and lemma frequency analysis
- Top lemmas by telecom company
- Identification of main pain points
- Heatmap of frequent lemmas
- Bigram analysis (common word pairs)
- Co-occurrence network (word relationships)
- Interactive exploration using Shiny

---

## 📊 Key Results and Insights

- **All companies** show a high presence of complaints related to **incorrect or unfair charges**.
- **Entel and Claro** present a strong pattern of complaints related to **subscriptions**, often linked to specific services such as *Glamour Sexy* (Entel) and *Hotzone* (Claro), which appear to be adult content services.
- For **Movistar**, complaints are mainly related to **home internet and fiber service issues**.
- For **WOM**, the analysis highlights problems related to **internet quality and signal coverage**.

These findings reflect **common consumer pain points** that may be relevant for **monitoring, compliance, and risk analysis**.

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

## 🔒 Data Privacy Notice

This repository is designed to be **public and safe**.

- The project **does NOT include sensitive or personal data**.
- If real complaint data is used, it should be stored **locally** and excluded using `.gitignore`.

Recommended approach:
- Keep raw data inside a `data/` folder (local only)
- Add `data/` to `.gitignore`
- Share only code, documentation, and safe visual outputs

---

## 📁 Project Structure

```text
claim_reclamos/
├── app.R                       # Shiny app entry point
├── data/                       # Local data (not included in GitHub)
├── screenshots/                # Images used in the README
├── rsconnect/                  # ShinyApps.io deployment files (optional)
├── README.md                   # Project documentation
├── .gitignore                  # Prevents uploading sensitive files
└── claim_reclamos.Rproj        # RStudio project file

