# An optional custom script to run before Hugo builds your site.
# You can delete it if you do not need it.

library(hexSticker)
library(glue)
library(googlesheets4)
library(ComplexHeatmap)
library(dplyr)
library(circlize)
library(grDevices)
gs4_deauth()

file.copy(from = "C:/Users/araul/Documents/GitHub/CV/Rau_Andrea_CV.pdf",
          to = "C:/Users/araul/Documents/GitHub/andrea/content/about/Rau_Andrea_CV.pdf", overwrite=TRUE)
file.copy(from = "C:/Users/araul/Documents/GitHub/CV/visual-CV/Rau_Andrea_visual-CV.png",
          to = "C:/Users/araul/Documents/GitHub/andrea/content/about/Rau_Andrea_visual-CV.png", overwrite=TRUE)

## Graphics --------------------------------------------------------------------
remake_hex <- FALSE
if(remake_hex) {
    base <- "C:/Users/araul/Documents/GitHub/andrea/content/project/"
    ## Applications project hex
    imgurl <- paste0(base, "applications/applications_illustrate.png")
    sticker(imgurl, package="", p_size=20, s_x=1, s_y=1, s_width=.7,
            filename=paste0(base, "applications/featured-hex.png"),
            h_fill="white", h_color="grey90")
    ## Integration project hex
    imgurl <- paste0(base, "integration/integrative_illustrate.png")
    sticker(imgurl, package="", p_size=20, s_x=1, s_y=.9, s_width=.95,
            filename=paste0(base, "integration/featured-hex.png"),
            h_fill="white", h_color="grey90")
    ## Network project hex
    imgurl <- paste0(base, "networks/networks_example.png")
    sticker(imgurl, package="", p_size=20, s_x=1, s_y=.95, s_width=.75,
            filename=paste0(base, "networks/featured-hex.png"),
            h_fill="white", h_color="grey90")
}

set.seed(1234)
mat1 = matrix(runif(60, max = 3, min = 1), 6, 10)
mat1 = rbind(mat1, matrix(runif(60, max = 2, min = 0), 6, 10))
mat2 = matrix(runif(60, max = 3, min = 1), 6, 10)
mat2 = rbind(mat2, matrix(runif(60, max = 2, min = 0), 6, 10))
le = sample(letters[1:3], 12, replace = TRUE)
ind = sample(12, 12)
mat1 = mat1[ind, ]
mat2 = mat2[rev(ind), ]
le = le[ind]
col_fun1 = colorRamp2(c(0, 2), c("white", "#00a3a6"))
col_fun2 = colorRamp2(c(0, 2), c("white", "#275662"))
col_fun3 = structure(c("#ed6e6c", "#f6b6b5", "white"),
                     names = letters[1:3])

ht1 = Heatmap(mat1, show_heatmap_legend = FALSE, show_column_names = FALSE,
              show_row_names=FALSE, col=col_fun1)
ht2 = Heatmap(mat2[,1:6], show_heatmap_legend = FALSE, show_column_names = FALSE,
              show_row_names=FALSE, col=col_fun2)
ht3 = Heatmap(le, show_heatmap_legend = FALSE, show_column_names = FALSE,
              show_row_names=FALSE, col= col_fun3)
png("C:/Users/araul/Documents/GitHub/andrea/static/img/featured.png",
    width = 600, height = 400)
ht1 + ht2 + ht3
dev.off()

jpeg("C:/Users/araul/Documents/GitHub/andrea/content/blog/sidebar-listing.jpg",
    width = 500, height = 150)
ht1[1:5,] + ht2[1:5,]
dev.off()

jpeg("C:/Users/araul/Documents/GitHub/andrea/content/collection/featured-sidebar.jpg",
     width = 500, height = 150)
ht1[6:10,] + ht2[6:10,]
dev.off()

## CV stuff automation ---------------------------------------------------------
remake_cv <- TRUE
if(remake_cv) {
    id <- "1szi9SA4sKl9Fmh-MWrepS1M9T36-HLvMSpyW9RJh-ws"

    publications <- googlesheets4::range_read(googlesheets4::as_sheets_id(id),
                                              sheet="publications")
    software <- googlesheets4::range_read(googlesheets4::as_sheets_id(id),
                                          sheet="software")
    advising <- googlesheets4::range_read(googlesheets4::as_sheets_id(id),
                                          sheet="advising")
    talks <- googlesheets4::range_read(googlesheets4::as_sheets_id(id),
                                       sheet="talks") %>%
        filter(date > "2022-01-31" | event %in% c("JOBIM 2020", "EuroBioc 2019"))

    ## Research ----------------------------------------------------------------
    tmp <- publications %>% filter(type %in% c("article")) %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link, url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
'---
date: "{Sys.Date()}"
draft: false
excerpt:
subtitle: ""
title: Peer-reviewed articles
weight: 1
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/writing/articles/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Books ----------------------------------------------------------------
    tmp <- publications %>%
        filter(type %in% c("dissertation", "book", "book chapter")) %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link, url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(url_slides_full =
                       ifelse(is.na(url_slides), "",
                              paste0("[:speaking_head:](", url_slides, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
draft: false
excerpt:
subtitle: ""
title: Books, book chapters & dissertations
weight: 2
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/writing/books/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")


    ## Preprints ----------------------------------------------------------------
    tmp <- publications %>%
        filter(type %in% c("other", "submitted", "techreport", "conference")) %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link, url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = ifelse(is.na(journal), "", paste0("*",journal,"*"))) %>%
        mutate(details_full = ifelse(is.na(details), ".",
                                     ifelse(details == "Submitted", "Submitted",
                                            paste0(", ", details)))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
draft: false
excerpt:
subtitle: ""
title: Preprints, submitted articles, and technical reports
weight: 3
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/writing/preprints/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")


    ## Software ----------------------------------------------------------------
    tmp0 <- software %>%
        mutate(full1 = glue("1. **{title}**: {description} [:link:]({url_software})
                            "),
                            full2 = glue("1. **{title}**: {description} [:globe_with_meridians:]({url_software})
                                         "))
    tmp <- tmp0 %>%
        filter(repo != "Shiny web app") %>%
        mutate(full = ifelse(repo == "Shiny web app", full2, full1)) %>%
        mutate(full = as_glue(full)) %>%
        select(full) %>%
        unclass()
    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
draft: false
excerpt:
subtitle:
title: Software packages
weight: 1
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/coding/packages/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Shiny apps ----------------------------------------------------------------
    tmp <- tmp0 %>%
        filter(repo == "Shiny web app") %>%
        mutate(full = ifelse(repo == "Shiny web app", full2, full1)) %>%
        mutate(full = as_glue(full)) %>%
        select(full) %>%
        unclass()
    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
draft: false
excerpt:
subtitle:
title: Shiny apps
weight: 2
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/coding/shiny/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## PhD Advising ----------------------------------------------------------------
    tmp <- advising %>%
        filter(type %in% c("PhD", "Postdoc", "3-month PhD Erasmus+ Learning Mobility")) %>%
        mutate(cosupervision_full = ifelse(is.na(cosupervision), "", paste0("(with ", cosupervision, ")"))) %>%
        mutate(full = glue("- **{name}** ({year}, {type}): {title} {cosupervision_full}")) %>%
        select(full) %>%
        unclass()
    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
draft: false
excerpt: (Co-)supervision of doctoral and postdoctoral research.
subtitle:
title: PhD + Postdoc supervision
weight: 1
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/advising/phd/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Master's Advising ----------------------------------------------------------------
    tmp <- advising %>%
        filter(type %in% c("M2 internship", "M1 internship",
                           "M1 STV/EM-ABG internship", "L2 internship", "6-month CDD")) %>%
        mutate(cosupervision_full = ifelse(is.na(cosupervision), "", paste0("(with ", cosupervision, ")"))) %>%
        mutate(full = glue("- **{name}** ({year}, {type}): {title} {cosupervision_full}")) %>%
        select(full) %>%
        unclass()
    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
excerpt:
subtitle: ""
title: Research interns + short-term contract supervision
weight: 2
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/advising/masters/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Committee Advising ----------------------------------------------------------------
    tmp <- advising %>%
        filter(grepl("committee", type))  %>%
        mutate(full = glue("- **{name}** ({year}): {type}")) %>%
        select(full) %>%
        unclass()
    tmp2 <- glue(
        '---
date: "{Sys.Date()}"
excerpt:
subtitle: ""
title: Advisory and evaluation Committee membership
weight: 3
---

{paste0(tmp$full, collapse = "\n")}')
    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/collection/advising/committees/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Talks -------------------------------------------------------------------
    for(i in 1:nrow(talks)) {
        choose <- talks[i,]
        create_name <- paste0(choose$date, "-", choose$location) %>%
            gsub(pattern=" ", replacement="-") %>%
            gsub(pattern=",", replacement="")
        suppressWarnings(dir.create(paste0("C:/Users/araul/Documents/GitHub/andrea/content/talk/",
                          create_name)))
        choose <- choose %>%
            mutate(date_full = ifelse(is.na(time_start), date,
                                      paste0(date,"T", time_start, ":00")),
                   date_end_full = ifelse(is.na(time_end), date,
                                      paste0(date,"T", time_end, ":00")),
                   abstract_full = ifelse(is.na(abstract), "", abstract),
                   event_url_full = ifelse(is.na(event_url), "", event_url),
                   # featured = ifelse(i < 5, "true", "false"),
                   featured = "true",
                   embed_full = ifelse(is.na(embed), "", embed),
                   slides = ifelse(is.na(url_slides), "",
                                   glue(
'
- icon: presentation-screen
  icon_pack: fa-regular
  name: slides
  url: {url_slides}
')),
                   video = ifelse(is.na(url_video), "",
                                  glue(
'
- icon: video
  icon_pack: fas
  name: video
  url: {url_video}
')))
        tmp2 <- glue(
'---
author: Andrea Rau
date: "{choose$date_full}"
date_end: "{choose$date_end_full}"
categories:
    - {choose$type}
draft: false
excerpt: {choose$abstract_full}
title: {choose$title}
subtitle: {choose$event}
location: {choose$location}
layout: single
event_url: {choose$event_url_full}
featured: {choose$featured}
links:
{choose$video}
{choose$slides}
---

{choose$embed_full}

')
        write.table(tmp2,
                    file = paste0("C:/Users/araul/Documents/GitHub/andrea/content/talk/",
                    create_name, "/index.md"),
                    quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")


    }
}

## Project automation ----------------------------------------------------------
remake_projects <- TRUE
if(remake_projects) {

    id <- "1szi9SA4sKl9Fmh-MWrepS1M9T36-HLvMSpyW9RJh-ws"
    ### Applications -----------------------------------------------------------
    publications <- googlesheets4::range_read(googlesheets4::as_sheets_id(id),
                                              sheet="publications")
    animal <- publications %>% filter(grepl("applications_animal", project))
    plant <- publications %>% filter(grepl("applications_plant", project))
    human <- publications %>% filter(grepl("applications_human", project))

    tmp_animal <- animal %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link,
               url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp_plant <- plant %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link,
               url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp_human <- human %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link,
               url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
'---
author: Andrea Rau
categories:
- projects
date: "{Sys.Date()}"
draft: false
excerpt: Applications in animal, plant, and human genomics
layout: single
subtitle:
tags:
- hugo-site
title: Applications in animal, plant, and human genomics
---

My methodological research is strongly motivated by applications in human, animal, and plant
genomics. I have made strong efforts to contribute analyses and develop methods
that can provide useful solutions in these areas.

---

### Animal genomics applications
{paste0(tmp_animal$full, collapse = "\n")}

### Plant genomics applications
{paste0(tmp_plant$full, collapse = "\n")}

### Human genomics applications
{paste0(tmp_human$full, collapse = "\n")}')

    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/project/applications/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Multi-omic  -----------------------------------------------------------
    software <- googlesheets4::range_read(googlesheets4::as_sheets_id(id),
                                          sheet="software")
    software_integration <- software %>%
        filter(project == "integration") %>%
        mutate(full = glue("1. **{title}**: {description} [:link:]({url_software})
                            "))

    integration <- publications %>% filter(grepl("integration", project))
    tmp_integration <- integration %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link,
               url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
'---
author: Andrea Rau
categories:
- projects
date: "{Sys.Date()}"
draft: false
excerpt: Combining multiple data views for holistic systems biology insights.
layout: single
subtitle:
tags:
- hugo-site
title: Multi-omic data integration
---

### Related software

{paste0(software_integration$full, collapse = "\n")}

![](hex_integrative.png)

### Related publications

{paste0(tmp_integration$full, collapse = "\n")}')

    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/project/integration/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Network  -----------------------------------------------------------
    software_networks <- software %>%
        filter(project == "networks") %>%
        mutate(full = glue("1. **{title}**: {description} [:link:]({url_software})
                            "))

    networks <- publications %>% filter(grepl("networks", project))
    tmp_networks <- networks %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link,
               url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
        '---
author: Andrea Rau
categories:
- projects
date: "{Sys.Date()}"
draft: false
excerpt: Systems biology approaches to identify dependency networks.
layout: single
subtitle:
tags:
- hugo-site
title: Network inference
---

### Related software

{paste0(software_networks$full, collapse = "\n")}

### Related publications

{paste0(tmp_networks$full, collapse = "\n")}')

    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/project/networks/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")

    ## Coexpression  -----------------------------------------------------------
    software_rnaseq <- software %>%
        filter(project == "rnaseq") %>%
        mutate(full = glue("1. **{title}**: {description} [:link:]({url_software})
                            "))

    rnaseq <- publications %>% filter(grepl("rnaseq", project))
    tmp_rnaseq <- rnaseq %>%
        arrange(desc(year)) %>%
        select(year, title, authors, journal, details, doi,
               url_pdf, url_preprint, url_code, url_software, url_custom1_link,
               url_slides) %>%
        mutate(authors_bf = sub(authors, pattern="Rau, A.",
                                replacement="**Rau, A.**"),
               journal_it = paste0("*",journal,"*")) %>%
        mutate(details_full = ifelse(is.na(details), ".", paste0(", ", details))) %>%
        mutate(doi_full =
                   ifelse(is.na(doi), ".",
                          paste0(". ", "[:link:](https://dx.doi.org/",doi,")"))) %>%
        mutate(url_pdf_full =
                   ifelse(is.na(url_pdf), "",
                          paste0("[:page_facing_up:](", url_pdf, ")"))) %>%
        mutate(url_preprint_full =
                   ifelse(is.na(url_preprint), "",
                          paste0("[:arrows_counterclockwise:](", url_preprint, ")"))) %>%
        mutate(url_code_full =
                   ifelse(is.na(url_code), "",
                          paste0("[:input_numbers:](", url_code, ")"))) %>%
        mutate(url_software_full =
                   ifelse(is.na(url_software), "",
                          paste0("[:computer:](", url_software, ")"))) %>%
        mutate(link_full =
                   ifelse(is.na(url_custom1_link), "",
                          paste0("[:globe_with_meridians:](", url_custom1_link, ")"))) %>%
        mutate(url_slides_full =
                   ifelse(is.na(url_slides), "",
                          paste0("[speaking_head:](", url_slides, ")"))) %>%
        mutate(full =
                   glue("1. {authors_bf} ({year}) {title}. {journal_it}{details_full}{doi_full}{url_pdf_full}{url_preprint_full}{url_code_full}{url_software_full}{url_slides_full}

                        ")) %>%
        select(full) %>%
        unclass()

    tmp2 <- glue(
        '---
author: Andrea Rau
categories:
- projects
date: "{Sys.Date()}"
draft: false
excerpt: Co-expression analysis for RNA sequencing data
layout: single
subtitle:
tags:
- hugo-site
title: RNA-seq co-expression
---

### Related software

{paste0(software_rnaseq$full, collapse = "\n")}

### Related publications

{paste0(tmp_rnaseq$full, collapse = "\n")}')

    write.table(tmp2,
                file = "C:/Users/araul/Documents/GitHub/andrea/content/project/coexpression/index.md",
                quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding = "utf8")
}
