# Startup OS

> Plateforme SaaS tout-en-un pour startups — Shiny + shinydashboard + SQLite.

Startup OS est un **Operating System pour startups** : un espace unique pour piloter le produit, les projets, les tâches, les clients et l'activité de l'équipe. Ce dépôt contient le **MVP fonctionnel** (v0.1) prêt à lancer.

---

## Sommaire

- [Aperçu](#aperçu)
- [Stack technique](#stack-technique)
- [Arborescence](#arborescence)
- [Installation](#installation)
- [Lancement](#lancement)
- [Fonctionnalités du MVP](#fonctionnalités-du-mvp)
- [Base de données](#base-de-données)
- [Architecture](#architecture)
- [Personnalisation](#personnalisation)
- [Roadmap](#roadmap)
- [Contribuer](#contribuer)
- [Licence](#licence)

---

## Aperçu

Startup OS vise à remplacer la dispersion d'outils (Notion, Trello, HubSpot, Excel…) par une seule application :

- **Product & Development** — Projects, Tasks, Roadmap, Backlog, Releases
- **Business** — CRM, Sales, Marketing, Customers
- **Collaboration** — Client Portal, Client Requests, Documents
- **Team** — Collaborators, Objectives
- **Strategy** — KPIs, OKRs, Business Overview
- **System** — Settings

L'approche UI est **minimaliste** (`Less is more`) : beaucoup d'espace blanc, peu de couleurs, typographie claire, cartes sobres. L'objectif est de ressembler à un **SaaS B2B moderne**, pas à un dashboard R classique.

> **MVP v0.1** — Le périmètre livré inclut : Dashboard, Projects, Tasks, Customers, Activity Center. Le module AI Assistant est volontairement exclu de cette première version.

---

## Stack technique

| Domaine            | Packages                                             |
| ------------------ | ---------------------------------------------------- |
| Application        | `shiny`, `shinydashboard`, `shinydashboardPlus`      |
| UI / UX            | `shinyWidgets`, `shinyjs`, `bslib`, `fresh`          |
| Tableaux & charts  | `DT` (ou `reactable`), `plotly`, `ggplot2`           |
| Validation         | `shinyvalidate`                                      |
| Base de données    | `DBI`, `RSQLite`, `pool`                             |
| Data manipulation  | `dplyr`                                              |

**Base de données** : SQLite, stockée dans `data/startup_os.sqlite`. Le pool est géré par `pool` — **aucune nouvelle connexion à chaque action**.

---

## Arborescence
