

subMuni <- function(df, munis) {
    df <- subset(df, Municipality %in% munis)
}

plotHomicide <- function(df, sub, title) {
    df <- subset(df, variable == sub & Year)
    df <- ddply(df, .(Municipality), transform,
                       order = value[Year == 2007])
    df$Municipality <- with(df, reorder(Municipality, -order))
    ggplot(df, aes(Year, value, group = Municipality,
                   color = Municipality)) +
        geom_line(size = 1.2) +
        scale_x_continuous(limits = c(1990, 2010)) +
        scale_y_continuous(limits = c(0, max(df$value))) +
        ylab("homicide rate") +
        opts(title = title)
}

addLabels <- function(p) direct.label(p, "last.points")

savePlot <- function(p, filename, width = 640, height = 480,
                     AA = FALSE){
    if(AA) {
        Cairo(file = filename, width=width, height=height)
        print(p)
        dev.off()
    } else {
        print(p)
        dev.print(png, file = filename, width=width, height=height)
    }
}

plotM <- function(df, sub, title, filename)
    savePlot(addLabels(plotHomicide(df, sub, title)), filename)

