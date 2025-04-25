
# circos.triangulate <img src="images/circos-triangulate-icon.png" align="right" height="138"/></a>

`circos.triangulate` is build from the
[`Circlize`](https://github.com/jokergoo/circlize) package to allow for
easy visualisation of large numbers of protein analyses, with
triangulated data in mind, allowing each different source to be
displayed as a different track. Automatic resizing occurs to ensure
tracks are displayed clearly, with custom options such as generating
odds ratios where a beta and standard error column are given, chosing
which set of data is displayed as the primary (1st) track and dealing
with missing data.

To ensure the expected outcome data should be formatted in a
`data.frame` as in the example below.

    #>   protein disease           method           b          se track_id   segment
    #> 1       a  cancer  Direct Measures  0.89072846 0.007767377        3 segment 1
    #> 2       b  cancer  Direct Measures  0.53127996 0.019442063        3 segment 2
    #> 3       a  cancer Reverse MR (IVW)  0.01320296 0.032406131        1 segment 1
    #> 4       b  cancer Reverse MR (IVW)  0.07656988 0.002151979        1 segment 2
    #> 5       b  cancer Forward MR (IVW)  0.03387963 0.015050333        2 segment 2
    #> 6       a  cancer Forward MR (IVW) -0.46365763 0.002417289        2 segment 1
    #> 7       a  cancer   Other Analysis -0.86554400 0.043312867        4 segment 1
    #> 8       b  cancer   Other Analysis  0.83199648 0.015875112        4 segment 2

To generate a `circos.triangulate` plot follow the example below:

``` r
# install.packages("remotes")
remotes::install_github("lisamhobson/circos.triangulate.git")

circos_protein_plot(circos_data = sample_data, # data.table containing all of the data to plot
                    total_track_number = 4, # total number of tracks to plot (can be less than sources of data if only plotting subset)
                    track_id_column = "track_id", # name of column containing variable to determine which track e.g. method / data source
                    protein_column = "protein", # column containing names of proteins
                    beta_column = "b", # column containing beta value 
                    se_column = "se", # column containing standard error to generate error bars
                    primary_track = "1", # variable from "track_id_column" to determine which track is first to plot and used to generate names
                    segment_names_column = "segment", # column containing factor to subset data on
                    # optional
                    odds_ratios = TRUE, # bolean value, whether to generate and plot odds ratios from beta and se (default = FALSE)
                    error_bar_ends = TRUE, # bolean value, whether to add ends to error bars (default = T)
                    custom_pallet = c("#FEC98DFF", "#FD9567FF", "#F1605DFF", "#CD4071FF") # provide custom colour pallet, supports viridis or custom vector
                    )
```

# <img src="images/example_circos.png" align="centre"/>

## Future Updates

- Colour coded key for data sources

- Zoomed segment plot
