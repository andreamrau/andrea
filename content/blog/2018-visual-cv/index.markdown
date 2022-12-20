---
author: Andrea Rau
categories:
- Tips and tricks
date: "2018-10-31"
draft: false
excerpt: Using the VisualResume package to create a visual CV.
layout: single
subtitle: ... a nice addition to a text-based CV
title: Creating a visual CV
---


This is a short post to provide details on how I created the visual CV that is included on my homepage. I got the idea for doing this from a [tweet](https://twitter.com/dataandme/status/1052527057496940544) from the awesome [Mara Averick](https://maraaverick.rbind.io/about/) about an R package called [*VisualResume*](https://github.com/ndphillips/VisualResume) by [Nathaniel Phillips](http://ndphillips.github.io/):

<blockquote class="twitter-tweet" data-lang="fr"><p lang="en" dir="ltr">OMG, I love this! (I miss Breaking Bad so much)<br>📦 &quot;VisualResume: An R package for creating a visual resume&quot; by <a href="https://twitter.com/YaRrrBook?ref_src=twsrc%5Etfw">@YaRrrBook</a> <a href="https://t.co/ZNtbrU87Y4">https://t.co/ZNtbrU87Y4</a> <a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">#rstats</a> <a href="https://t.co/8KbI06QYKq">pic.twitter.com/8KbI06QYKq</a></p>&mdash; Mara Averick (@dataandme) <a href="https://twitter.com/dataandme/status/1052527057496940544?ref_src=twsrc%5Etfw">17 octobre 2018</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Once *VisualResume* is installed from GitHub (via *devtools*) and loaded, I just modified the Walter White example, resized the plot directly in the RStudio plot window, and exported to PNG.


```r
library(VisualResume)
VisualResume::VisualResume(
  titles.left = c("","Andrea Rau, PhD",  ""),
  titles.right = c("", "", ""),
  titles.left.cex = c(1,  4, 1),
  timeline.labels = c("Education", "Work experience"),
  timeline = data.frame(title = c("St. Olaf College", "Purdue University", 
                                  "Purdue University", "Inria", "INRA", "UWM", "UWM",
                                  "Mayo Clinic", "Walgreens"),
                        sub = c("BA, Mathematics & French", "MS, Applied Statistics", 
                                "PhD, Statistics", "Post-doc", 
                                "Research Scientist", "Visiting Scholar", 
                                "AgreenSkills+ Fellow", "Intern", "Intern"),
                        start = c(2001.75, 2005.75, 2007.75, 2010.75, 2011.75, 
                                  2016.58, 2018783, 2005, 2006.4),
                        end = c(2005.42, 2007.74, 2010.67, 2011.75, 2020, 2016.75, 
                                2019.5, 2005.15, 2006.7),
                        side = c(1, 1, 1, 0, 0, 0, 0, 0, 0)),
  milestones = data.frame(title = c("BA", "MS", "PhD"),
                          sub = c("Math/\nFrench", "Applied\nStatistics", 
                                  "Statistics"),
                          year = c(0, 0, 0)),
  events = data.frame(year = c(2009.9, 2015.5, 2017.75, 2018.5),
                      title = c("First R package on CRAN (ebdbNet)",
                                "BioBayes book on Bayesian statistics published",
                                "HDR in Applied Mathematics (University d'Evry-Val-d'Essonne)",
                                "First Shiny web application published (EDGE in TCGA)")),
  interests = list("R" = c(rep("coseq", 4), "HTSCluster",
                           rep("HTSFilter", 3), rep("EDGE in TCGA", 2)), 
                   "statistics" = c(rep("mixture models", 6), 
                                    rep("clustering", 3), rep("networks", 5), 
                                    rep("Bayesian", 3)),
                   "genomics" = c(rep("multi-omics", 8), rep("ATAC-seq", 2), 
                                  rep("co-expression", 6), rep("networks", 3))),
  year.steps = 1
)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="1440" />
