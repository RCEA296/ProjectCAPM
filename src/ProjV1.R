---
title: "Tsunami-Prediction"
author: "Angel Ortiz; Rayane Cascon"
date: 'UC3M, 2025'
output:
  html_document:
    css: my-theme.css
    theme: cerulean
    highlight: tango
    number_sections: no
    toc: no
    toc_depth: 1
editor_options:
  chunk_output_type: console
---

```{r global_options, include =TRUE, echo =FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```


Libraries
```{r}
library("dplyr")
```

```{r}
tsunami_data = read.csv("src/data/TsunamiData.csv", header = TRUE)
```





