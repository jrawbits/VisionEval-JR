library(tcltk)

dialog.env <- new.env()

# Dialog function to get a text input
get_text_input <- function(parent_window) {
  tt <- tktoplevel(parent = parent_window) #make the dialog a child of the main window
  tkwm.title(tt, "Enter Text")

  input_var <- tclVar("")
  input_entry <- tkentry(tt, textvariable = input_var)
  tkpack(input_entry, padx = 10, pady = 10)

  onOK <- function() {
    input_value <- tclvalue(input_var)
    if (input_value != "") {
      assign("dialog_result", input_value, envir = dialog.env ) # parent.frame())
      tkdestroy(tt)
    } else {
      tkmessageBox(title = "Error", message = "Please enter a value.", icon = "error")
    }
  }

  onCancel <- function() {
    assign("dialog_result", NULL, envir = dialog.env ) # parent.frame())
    tkdestroy(tt)
  }

  ok_button <- tkbutton(tt, text = "OK", command = onOK)
  cancel_button <- tkbutton(tt, text = "Cancel", command = onCancel)
  tkpack(ok_button, cancel_button, padx = 10, pady = 10, side = "left")

  tkwait.window(tt) #Wait for the window to close
}

# Main GUI function
create_main_gui <- function() {
  main_window <- tktoplevel()
  tkwm.title(main_window, "Main GUI")

  result_label <- tklabel(main_window, text = "Result: (None)")
  tkpack(result_label, padx = 10, pady = 10)

  open_dialog_button <- tkbutton(main_window, text = "Open Dialog", command = function() {
    get_text_input(main_window) #Pass the parent window

    if (!is.null(result <- get("dialog_result", dialog.env))) {
      tkconfigure(result_label, text = paste("Result:", result))
    } else {
      tkconfigure(result_label, text = "Result: (Cancelled)")
    }
  })

  tkpack(open_dialog_button, padx = 10, pady = 10)

  tkwait.window(main_window) #Wait for the window to close
}

# Run the GUI
create_main_gui()
ls(dialog.env)
print(dialog.env$result)
