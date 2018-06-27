## Check the Demas well arrangement

require(devtools)
require(lattice)
load_all()




## This material is moving into ../vignettes/data_input.Rmd
## once complete, this file should be deleted...


test_platelayout = list(n_well = 6,
                        wells = paste0("w", 1:6),
                        n_well_r = 2,
                        n_well_c = 3,
                        layout = c(3, 2),
                        n_elec_r = 8,
                        n_elec_c = 8,
                        xlim = c(-100, 7200),
                        ylim = c(0, 6000),
                        spacing = 200,
                        corr_breaks = 0
                        )

add_plateinfo("test-6well", test_platelayout)
times = system.file("extdata/textreader/demas.times", package="meaRtools")
pos = system.file("extdata/textreader/demas.pos", package="meaRtools")
s = read_spikelist_text(times, pos, array="test-6well")
.plot_mealayout(s$layout, use_names = TRUE, cex=0.3)



## This takes the average across the whole plate
.plot_meanfiringrate(s, main = "Mean Firing Rate by Plate (Hz)")

## TODO -- fix this up.
##.channel_plot_by_well(s, resp = "meanfiringrate",
##                      resp_label = "Mean Firing Rate (Hz)")

s <- calculate_isis(s)
.plot_isis_by_plate(s)

## .plot_isis_by_electrode(s) ## does nothing?



s = get_num_ae(s)
.plot_mean_firingrate_by_electrode(s)
