# Shiny app

To execute this app, use `make` on the command line. Alternatively, you can run the following command in R:

```
Rscript --vanilla -e 'shiny::runApp(".")'
```

The app depends on the dataset `data/variables20000.csv.gz` that is generated by the quarto document in `data/variables.qmd`.