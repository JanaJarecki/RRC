source('fig_setup.R') # Figure setup

# Run this line if you have not fit the network before!
# source('1.2-risk-return-paraxos.R') # First fit the networks

# Make a list with plits
plots <- lapply(1:length(corlist), function(z) {
  plot_net(corlist[[z]]
    , type = "contemporaneous"
    , Title = paste0(letters[z], '. ', vars[z])
    , mlvar = TRUE
    , alpha = 0.0125)})

png('../figures/fig3.png', width = 1600, height = 550, units = "px", res = 220, , pointsize = 12)
par(mfrow = c(1, 4), family = "Roboto Condensed", oma = c(3,0,0,0))
for (i in 1:4) {
  plot(plots[[i]])
}
mtext('Note: Perceived values z-standardized', side = 1, outer = TRUE, line = 2, cex = .7, family = "Roboto Condensed", font = 1, adj = 1)
dev.off();
dev.off();