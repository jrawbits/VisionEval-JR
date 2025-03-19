library(tcltk)

get_two_inputs <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Enter Two Values")

  frame <- tkframe(tt,padx=5,pady=5)
  tkgrid(frame,sticky="ew")
  tkgrid.columnconfigure(tt,0,weight=1)

  # First input
  tkgrid(tklabel(frame, text = "Value 1:"), column = 0, row = 0, sticky = "w", padx = 5, pady = 5)
  value1_var <- tclVar("")
  value1_entry <- tkentry(frame, textvariable = value1_var)
  tkgrid(value1_entry, column = 1, row = 0, sticky = "we", padx = 5, pady = 5)

  # Second input
  tkgrid(tklabel(frame, text = "Value 2:"), column = 0, row = 1, sticky = "w", padx = 5, pady = 5)
  value2_var <- tclVar("")
  value2_entry <- tkentry(frame, textvariable = value2_var)
  tkgrid(value2_entry, column = 1, row = 1, sticky = "we", padx = 5, pady = 5)

  env <- new.env()

  # OK and Cancel buttons
  onOK <- function() {
    val1 <- tclvalue(value1_var)
    val2 <- tclvalue(value2_var)
    if (val1 != "" && val2 != "") {
      assign("result",c(val1, val2),envir=env)  # Store in global result
      tkdestroy(tt)
    } else {
      tkmessageBox(title = "Error", message = "Please enter both values.", icon = "error")
    }
  }

  onCancel <- function() {
    assign("result",NULL,envir=env)  # Store in global result
    tkdestroy(tt)
  }

  ok_button <- tkbutton(frame, text = "OK", command = onOK)
  cancel_button <- tkbutton(frame, text = "Cancel", command = onCancel)

  tkgrid(ok_button, column = 0, row = 2, pady = 10)
  tkgrid(cancel_button, column = 1, row = 2, pady = 10)
  tkgrid.columnconfigure(frame, 1, weight = 1) #Make the second column expandable.
  tkwait.window(tt) #Wait for the window to be destroyed.

  return(env$result)
}

# Example usage:
user_inputs <- get_two_inputs()

if (!is.null(user_inputs)) {
  print("User inputs:")
  print(user_inputs)
} else {
  print("User cancelled.")
}
