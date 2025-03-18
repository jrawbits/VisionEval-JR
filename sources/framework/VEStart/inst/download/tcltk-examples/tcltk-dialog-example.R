library(tcltk)

create_dialog <- function() {
  # Create a top-level window
  dialog <- tktoplevel()
  tkwm.title(dialog, "Find assets in Github repository")
  
  # Create a frame for padding
  frame <- tkframe(dialog)
  tkpack(frame)
  
  # Create labels and entry fields
  label1 <- tklabel(frame, text = "Github User:")
  entry1 <- tkentry(frame)
  
  label2 <- tklabel(frame, text = "Github Repository:")
  entry2 <- tkentry(frame)
  
  # Pack labels and entries with ample margins
  tkpack(label1, entry1, padx = 10, pady = 10)
  tkpack(label2, entry2, padx = 10, pady = 10)
  
  # Create a variable to store the result
  result <- list()
  
  # Function to handle OK button press
  on_ok <- function() {
    result <<- list(User=tclvalue(tkget(entry1)), Repository=tclvalue(tkget(entry2)))
    tkdestroy(dialog)
  }
  
  # Create OK and Cancel buttons
  ok_button <- tkbutton(frame, text = "OK", command = on_ok)
  cancel_button <- tkbutton(frame, text = "Cancel", command = function() tkdestroy(dialog))
  
  # Pack buttons
  tkpack(ok_button, cancel_button, padx = 10, pady = 10)
  tkpack(cancel_button, padx = 10, pady = 10)
  
  # Wait for the dialog to close
  tkwait.window(dialog)
  
  return(result)
}

# Example of how to call the dialog and get results
result <- create_dialog()
print(result)
