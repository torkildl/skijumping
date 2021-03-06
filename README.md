# Skijumping

This repository contains files for a paper by Leiv Rønneberg and myself on nationalistic (and previously, home field) biases in skijumping competitions. The paper DOI is https://doi.org/10.1080/16138171.2020.1792628 and it was published in European Journal of Sport and Society.

The data acquisition and preparation process is a little messy. First, we downloaded PDFs with results from several seasons in the late 2000s from FIS by hand. The results were partly hand-punched and partly copied and reviewed for accuracy by us and assistants. They were used to produce the first version of the paper.  The original PDFs and resulting data set in `.csv` form are available in the repository.

In the revision process, we decided to obtain new data and scraped FIS' website for PDFs using automated procedures. These procedures are documented in this repository. Both R scripts, original PDFs, and analysis code files are saved in the repository, and the work using this season should be 100% replicable (as long as FIS' website remains the same) and mostly replicable when using the already downloaded PDFs.


