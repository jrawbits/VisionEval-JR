# Load the tcltk package
library(tcltk)

# Dialog values to update
message_text <- tclVar("Not displayed")
question_text <- tclVar("Question Not Answered")
name_text <- tclVar("No Input Yet")
file_text <- tclVar("No File Yet")
directory_text <- tclVar("No Directory yet")

# Example 1: Simple Message Box
show_message <- function() {
  tkmessageBox(title = "Information", message = "This is a simple message box.", icon = "info")
  tclvalue(message_text) <- paste("Displayed message at:",format(Sys.time(),"%H:%M:%S"))
  tcl("update")
}

# Example 2: Yes/No Question Box
ask_question <- function() {
  response <- tkmessageBox(title = "Question", message = "Do you want to continue?", icon = "question", type = "yesno")
  if (as.character(response) == "yes") {
    msg <- "User clicked Yes"
  } else {
    msg <- "User clicked No"
  }
  tclvalue(question_text) <- msg
}

# Example 3: Entry Dialog
get_input <- function(parent_window) {
  tt <- tktoplevel(parent = parent_window)
  tkwm.title(tt, "Enter your name")

  tkgrid(
    tklabel(tt,text="Enter your name"),
    column = 0, row = 0, padx = 5, pady = 5, columnspan = 2
  )

  name_var <- tclVar("")
  entry <- tkentry(tt, textvariable = name_var)
  tkgrid(entry, column = 0, row = 1, columnspan = 2, sticky = "w", padx = 5, pady = 5)

  onOK <- function() {
    name <- tclvalue(name_var)
    if (name != "") {
      tclvalue(name_text) <- name
      tkdestroy(tt)
    } else {
      tkmessageBox(title = "Error", message = "Please enter a name.", icon = "error")
    }
  }

  onCancel <- function() { tkdestroy(tt) }

  ok_button <- tkbutton(tt, text = "OK", command = onOK)
  cancel_button <- tkbutton(tt,text="Cancel",command=onCancel)

  tkgrid(ok_button, column = 0, row = 3, padx = 5, pady = 5)
  tkgrid(cancel_button, column = 1, row = 3, padx = 5, pady = 5)
}

# Example 4: File Selection Dialog
select_file <- function() {
  file_path <- tclvalue(tkgetOpenFile())
  tclvalue(file_text) <- if (file_path != "") {
    file_path
  } else {
    "No file selected."
  }
}

# Example 5: Directory Selection Dialog
select_directory <- function() {
  dir_path <- tclvalue(tkchooseDirectory())
  tclvalue(directory_text) <- if (dir_path != "") {
    dir_path
  } else {
    "No directory selected."
  }
}

# Example 6: A simple GUI with buttons to trigger the dialogs
create_gui <- function(max_width=400) {
  tt <- tktoplevel()
  tkwm.title(tt, "Tcl/Tk Sub-Dialog Examples")
  tkwm.maxsize(tt, max_width, 10000) # we don't expect to expand vertically

  # Actions to gather information
  message_button <- tkbutton(tt, text = "Show Message", command = show_message)
  question_button <- tkbutton(tt, text = "Ask Question", command = ask_question)
  input_button <- tkbutton(tt, text = "Get Input",
    command = function() {
      get_input(tt)
    }
  )
  file_button <- tkbutton(tt, text = "Select File", command = select_file)
  directory_button <- tkbutton(tt, text = "Select Directory", command = select_directory)

  # Display the buttons
#  tkpack(message_button, question_button, input_button, file_button, directory_button, pady = 5)
  tkgrid(message_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)
  tkgrid(question_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)
  tkgrid(input_button, column = 0, row = 2, sticky = "e", padx = 5, pady = 5)
  tkgrid(file_button, column = 0, row = 3, sticky = "e", padx = 5, pady = 5)
  tkgrid(directory_button, column = 0, row = 4, sticky = "e", padx = 5, pady = 5)

  # Display the values set by the buttons
  tkgrid(
    tklabel(tt,textvariable=message_text, borderwidth = 2, relief = "groove"),
    column = 1, row = 0, sticky="w", padx = 5, pady = 5
  )
  tkgrid(
    tklabel(tt,textvariable=question_text, borderwidth = 2, relief = "groove"),
    column = 1, row = 1, sticky="w", padx = 5, pady = 5
  )
  tkgrid(
    tklabel(tt,textvariable=name_text, borderwidth = 2, relief = "groove"),
    column = 1, row = 2, sticky="w", padx = 5, pady = 5
  )
  tkgrid(
    tklabel(tt,textvariable=file_text, borderwidth = 2, relief = "groove"),
    column = 1, row = 3, sticky="w", padx = 5, pady = 5
  )
  tkgrid(
    tklabel(tt,textvariable=directory_text, borderwidth = 2, relief = "groove"),
    column = 1, row = 4, sticky="w", padx = 5, pady = 5
  )

  # OK and Cancel buttons
  onOK <- function() {
    tkdestroy(tt)
  }

  onCancel <- function() {
    tkdestroy(tt)
  }

  ok_button <- tkbutton(tt, text = "OK", command = onOK)
  cancel_button <- tkbutton(tt, text = "Cancel", command = onCancel)

  tkgrid(ok_button, column = 0, row = 5, pady = 10)
  tkgrid(cancel_button, column = 1, sticky="w", row = 5, pady = 10)
  tkgrid.columnconfigure(tt, 1, weight = 1) #Make the second column expandable.

  tkwait.window(tt)
  return(
    c(
      Message=tclvalue(message_text),
      Question=tclvalue(question_text),
      Name=tclvalue(name_text),
      File=tclvalue(file_text),
      Directory=tclvalue(directory_text)
    )
  )
}

# Run the GUI
create_gui(max_width=800)
