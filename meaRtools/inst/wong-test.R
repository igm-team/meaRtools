require(devtools)
require(lattice)
load_all()




hex_platelayout = list(n_well = 1,
                        wells = paste0("w", "1"),
                        n_well_r = 1,
                        n_well_c = 1,
                        layout = c(1, 1),
                        n_elec_r = 8,
                        n_elec_c = 8,
                        xlim = c(-400, 400),
                        ylim = c(-400, 400),
                        spacing = 50,
                        corr_breaks = 0
                        )

add_plateinfo("hex-1well", hex_platelayout)
times = system.file("extdata/textreader/wong1993_p0.times", package="meaRtools")
pos = system.file("extdata/textreader/wong1993_p0.pos", package="meaRtools")
s = read_spikelist_text(times, pos, array="hex-1well")
.plot_mealayout(s$layout, use_names = TRUE, cex=0.3)
.plot_meanfiringrate(s, main = "Mean Firing Rate by Plate (Hz)")

get_plateinfo("hex-1well")
