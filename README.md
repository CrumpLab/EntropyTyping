# EntropyTyping

This is an open and reproducible project being written in public on Github. The entire project is written in R Markdown, including .rmd files for analysis, and .rmd files to compile the paper in APA format using papaja (for .pdf), and this R Markdown website. In principle the source code and open data contained in this project repository should be sufficient for compiling and reproducing the paper.

## PUBLIC DRAFT IN PROGRESS
(Everything subject to change, all changes recorded by git)

This draft is currently being displayed as an R Markdown website: [https://crumplab.github.io/EntropyTyping/index.html](https://crumplab.github.io/EntropyTyping/index.html)

This repository contains:
- `EntropyTyping.proj` is the R-studio project file
- `mturk.txt.zip` is the raw data
- the files for compiling the paper are:
  - `Entropy_typing_draft.Rmd` is the paper written using the papaja package
  - `Entropy_typing_draft.pdf` is the .pdf version of this paper
  - `Entropy_typing_analysis.r` contains the analysis scripts for the paper. These scripts are linked to in the `Entropy_typing_draft.Rmd`
  - `r-references.bib` contains the bibliography file
  - `the_data.Rdata` contains the data used in the present analysis
- the `_history` folder contains additional analysis files made by the authors throughout the course of the project
- `index.Rmd`, `_site.yml`, `webpaper.css` contain files for compiling this website. The website files are saved in the `docs` folder, and served on the web using the github pages option for this repository.
      
- The [issues tab of the github repository](https://github.com/CrumpLab/EntropyTyping/issues) contains threaded discussions we had (are having) about this project

