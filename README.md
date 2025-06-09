
# circos.triangulate <img src="images/circos-triangulate-icon.png" align="right" height="138"/></a>

`circos.triangulate` is build from the
[`Circlize`](https://github.com/jokergoo/circlize) package to allow for
easy visualisation of large numbers of protein analyses, with
triangulated data in mind, allowing each different source to be
displayed as a different track. Automatic resizing occurs to ensure
tracks are displayed clearly, with custom options such as generating
odds ratios where a beta and standard error column are given, choosing
which set of data is displayed as the primary (1st) track and dealing
with missing data.

To ensure the expected outcome data should be formatted in a
`data.frame` as in the example below.

    #>   protein disease           method             b           se track_id
    #> 1       a  cancer  Direct Measures -0.7103975601 0.0235400403        3
    #> 2       b  cancer  Direct Measures -0.1249684612 0.0002788665        3
    #> 3       a  cancer Reverse MR (IVW) -0.0924579104 0.0337596620        1
    #> 4       b  cancer Reverse MR (IVW)  0.0009113503 0.0080741077        1
    #> 5       a  cancer Forward MR (IVW) -0.5297028850 0.0066467277        2
    #> 6       b  cancer Forward MR (IVW) -0.3933940993 0.0234348884        2
    #> 7       a  cancer   Other Analysis  1.5098517407 0.0195729367        4
    #> 8       b  cancer   Other Analysis  1.6779766828 0.0067239027        4
    #>     segment
    #> 1 segment 1
    #> 2 segment 2
    #> 3 segment 1
    #> 4 segment 2
    #> 5 segment 1
    #> 6 segment 2
    #> 7 segment 1
    #> 8 segment 2

To generate a `circos.triangulate` plot follow the example below:

``` r
# install.packages("remotes")
remotes::install_github("lisamhobson/circos.triangulate", force=T)

circos_protein_plot(circos_data = sample_data, # data.table containing all of the data to plot
                    total_track_number = 4, # total number of tracks to plot (can be less than sources of data if only plotting subset)
                    track_id_column = "track_id", # name of column containing variable to determine which track e.g. method / data source
                    protein_column = "protein", # column containing names of proteins
                    beta_column = "b", # column containing beta value 
                    se_column = "se", # column containing standard error to generate error bars
                    primary_track = "1", # variable from "track_id_column" to determine which track is first to plot and used to generate names
                    segment_names_column = "segment", # column containing factor to subset data on
                    # optional
                    odds_ratios = TRUE, # boolean value, whether to generate and plot odds ratios from beta and se (default = FALSE)
                    error_bar_ends = TRUE, # boolean value, whether to add ends to error bars (default = T)
                    custom_pallet = c("#FEC98DFF", "#FD9567FF", "#F1605DFF", "#CD4071FF") # provide custom colour pallet, supports viridis or custom vector
                    )
```

# <img src="images/example_circos.png" align="centre"/>

## Future Updates

- Colour coded key for data sources

- Zoomed segment plot
