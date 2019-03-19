# PlotsOfDifferences
A Shiny App for comparison of samples

### About PlotsOfDifferences

The PlotsOfDifferences Shiny app plots the data, statistics and (optional) differences to enable the comparison of (experimental) conditions. The philosophy of the approach is that plotting the raw data (instead of a summary) improves transparency and interpretation [this blog](http://thenode.biologists.com/leaving-bar-five-steps/)). To further facilitate the comparison, summary statistics (mean, median, boxplot) and inferential statistics (confidence intervals) can be added. The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha). Finally, differences between central values (mean or median) can be quantified, reflecting effect size. To this end, a reference condition ('control') must be defined, which can be changed by the user. The 95%CI of the difference per condition is calculated from 1000 bootstrap samples (their distribution is shown in the difference plot) of the statistic (mean of median).
A table with summary and inferential statistics is provided on a separate tab. There is an option to add a p-value which is calculated by a [randomization test](https://www.uvm.edu/~dhowell/StatPages/Randomization%20Tests/Random2Sample/twoindependentsamples.html).


Bootstrapping is used to calculate the (asymmetric) 95% CI of medians as well as the differences. More information in [this blog](http://thenode.biologists.com/a-better-bar/education/)

The p-values that are (optionally) listed in the summary table is the output of a [randomization test.](https://www.pmrjournal.org/article/S1934-1482(17)30168-5/abstract)

The data can be supplied in spreadsheet/long format (e.g. by copy-pasting from excel) or in tidy format. For more information on the conversion of spreadsheet data to tidy data see [this blog](http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/).

The differences plot can be saved as a PDF file, which can be opened and edited with Adobe Illustrator to allow for fine adjustments of the lay-out.

More information can be found in [the preprint](http://biorxiv.org/cgi/content/short/578575v1)


### Running the App

A shiny server running the app is available: [https://huygens.science.uva.nl/PlotsOfDifferences/](https://huygens.science.uva.nl/PlotsOfDifferences/)

The app can also run locally from R/Rstudio:

Give it a quick try by running it directly from Github. In the command line (in R or Rstudio) type
shiny::runGitHub('PlotsOfDifferences', 'JoachimGoedhart')

Or download it to use it offline:

-download the app.R, geom_flat_violin.R and csv files with example data.

-Run RStudio and load app.R

-Select 'Run All' (shortcut is command-option-R on a Mac) or click on "Run App" (upper right button on the window)

This should launch a web browser with the Shiny app.
Note that the app depends on several R packages that need to be installed (shiny, ggplot2, dplyr, tidyr, readr, magrittr, ggbeeswarm, readxl, DT, RCurl, gridExtra, shinycssloaders). It also uses geom_flat_violin.R (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)


### Credits

PlotsOfData is inspired by [BoxPlotR](http://shiny.chemgrid.org/boxplotr/). See [this link](https://www.nature.com/articles/nmeth.2813) for background information on boxplots.  
The code for the shiny app is partially derived from [ggplotGUI](https://github.com/gertstulp/ggplotgui) by [Gert Stulp](https://www.gertstulp.com)  
The colorblind safe palettes were developed by [Paul Tol](https://personal.sron.nl/~pault/).  
The display of bootstrap distributions is inspired by [work of Adam Claridge-Chang and colleagues (2017)](https://www.nature.com/articles/nmeth.4148)

PlotsOfData is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart)) and Marten Postma

### Example output

![alt text](https://github.com/JoachimGoedhart/PlotsOfDifferences/blob/master/DifferencesPlot_example1.png "Output")


