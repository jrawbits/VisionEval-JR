library(tcltk)

# Create the main window
tt <- tktoplevel()
tkwm.title(tt, "Checkbox and Radio Button Example")

# Checkboxes
frame_checkboxes <- tkframe(tt)
tkpack(frame_checkboxes, fill = "x", padx = 10, pady = 5) # grid geometry manager, use sticky="ew" to make the widget take up all horizontal room.

checkbox_vars <- lapply(1:3, function(i) tclVar(0))
checkbox_labels <- c("Option A", "Option B", "Option C")
checkboxes <- lapply(1:3, function(i) tkcheckbutton(frame_checkboxes, text = checkbox_labels[i], variable = checkbox_vars[[i]]))

for (i in 1:3) {
  tkpack(checkboxes[[i]], anchor = "w")
}

# Radio buttons
frame_radiobuttons <- tkframe(tt)
tkpack(frame_radiobuttons, fill = "x", padx = 10, pady = 5)

radio_var <- tclVar("1") # Default selection
radio_labels <- c("Radio 1", "Radio 2", "Radio 3")
radiobuttons <- lapply(1:3, function(i) tkradiobutton(frame_radiobuttons, text = radio_labels[i], variable = radio_var, value = as.character(i)))

for (i in 1:3) {
  tkpack(radiobuttons[[i]], anchor = "w")
}

# Button to display selected values
button_frame <- tkframe(tt)
tkpack(button_frame, fill = "x", padx = 10, pady = 10)

display_values <- function() {
  checkbox_values <- sapply(checkbox_vars, tclvalue)
  radio_value <- tclvalue(radio_var)

  message <- paste("Checkboxes:", paste(checkbox_labels[as.logical(as.numeric(checkbox_values))], collapse = ", "),
                   "\nRadio Button:", radio_labels[as.numeric(radio_value)])

  tkmessageBox(message = message, icon = "info", title = "Selected Values")
}

display_button <- tkbutton(button_frame, text = "Display Selected Values", command = display_values)
tkpack(display_button)

# Run the tcltk event loop
tkwait.window(tt)
