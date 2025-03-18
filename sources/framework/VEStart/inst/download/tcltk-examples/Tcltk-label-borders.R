library(tcltk)

create_bordered_label <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Bordered Label")

  # Create a label with a raised border
  raised_label <- tklabel(tt, text = "Raised Border", borderwidth = 2, relief = "raised")
  tkgrid(raised_label, row = 0, column = 0, padx = 10, pady = 10)

  # Create a label with a sunken border
  sunken_label <- tklabel(tt, text = "Sunken Border", borderwidth = 2, relief = "sunken")
  tkgrid(sunken_label, row = 1, column = 0, padx = 10, pady = 10)

  # Create a label with a groove border
  groove_label <- tklabel(tt, text = "Groove Border", borderwidth = 2, relief = "groove")
  tkgrid(groove_label, row = 2, column = 0, padx = 10, pady = 10)

  # Create a label with a ridge border
  ridge_label <- tklabel(tt, text = "Ridge Border", borderwidth = 2, relief = "ridge")
  tkgrid(ridge_label, row = 3, column = 0, padx = 10, pady = 10)

  # Create a label with a solid border
  solid_label <- tklabel(tt, text = "Solid Border", borderwidth = 2, relief = "solid")
  tkgrid(solid_label, row = 4, column = 0, padx = 10, pady = 10)

  # Create a label with a flat border
  flat_label <- tklabel(tt, text = "Flat Border", borderwidth = 2, relief = "flat")
  tkgrid(flat_label, row = 5, column = 0, padx = 10, pady = 10)
}

create_bordered_label()