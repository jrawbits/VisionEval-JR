# Load the tcltk package
library(tcltk)

# TODO: get the various approaches to be more generic (a la the get_input / name function, where the
# destination variable is a parameter)

# Then start figuring out what needs to be in each of the dialogs...q

setup.dialog <- function(max_width=800) {
  # Dialog values to update
  message_text <- tclVar("Not displayed")
  question_text <- tclVar("Question Not Answered")
  name_text <- tclVar("No Name Yet")
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
  get_input <- function(parent_window,tcl_dest_var) {
    tt <- tktoplevel(parent = parent_window)
    tkwm.title(tt, "Enter your name")

    tkgrid(
      tklabel(tt,text="Enter your name: "),
      column = 0, row = 0, padx = 5, pady = 5, sticky = "e"
    )

    name_var <- tclVar("")
    entry <- tkentry(tt, textvariable = name_var)
    tkgrid(entry, column = 1, row = 0, columnspan = 2, sticky = "w", padx = 5, pady = 5)

    onOK <- function() {
      name <- tclvalue(name_var)
      if (name != "") {
        tclvalue(tcl_dest_var) <- name
        tkdestroy(tt)
      } else {
        tkmessageBox(title = "Error", message = "Please enter a name.", icon = "error")
      }
    }

    onCancel <- function() { tkdestroy(tt) }

    ok_button <- tkbutton(tt, text = "OK", command = onOK)
    cancel_button <- tkbutton(tt,text="Cancel",command=onCancel)

    tkgrid(ok_button, column = 0, row = 3, padx = 5, pady = 5, sticky="e")
    tkgrid(cancel_button, column = 1, row = 3, padx = 5, pady = 5, sticky="w")
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
        get_input(tt,name_text)
      }
    )
    file_button <- tkbutton(tt, text = "Select File", command = select_file)
    directory_button <- tkbutton(tt, text = "Select Directory", command = select_directory)

    # Display the buttons
    tkgrid(message_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)
    tkgrid(question_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)
    tkgrid(input_button, column = 0, row = 2, sticky = "e", padx = 5, pady = 5)
    tkgrid(file_button, column = 0, row = 3, sticky = "e", padx = 5, pady = 5)
    tkgrid(directory_button, column = 0, row = 4, sticky = "e", padx = 5, pady = 5)

    # Display the values set by the buttons in label widgets  
    message_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    message_label <- tklabel(message_frame,textvariable=message_text, justify="left")
    tkpack(message_label,anchor="w",padx=5,pady=5)
    tkgrid(message_frame, column = 1, row = 0, sticky="ew", padx = 5, pady = 5)

    question_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    question_label <- tklabel(question_frame,textvariable=question_text, justify="left")
    tkpack(question_label,anchor="w",padx=5,pady=5)
    tkgrid(question_frame, column = 1, row = 1, sticky="ew", padx = 5, pady = 5)

    name_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    name_label <- tklabel(name_frame,textvariable=name_text, justify="left")
    tkpack(name_label,anchor="w",padx=5,pady=5)
    tkgrid(name_frame, column = 1, row = 2, sticky="ew", padx = 5, pady = 5)

    file_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    file_label <- tklabel(file_frame,textvariable=file_text, justify="left")
    tkpack(file_label,anchor="w",padx=5,pady=5)
    tkgrid(file_frame, column = 1, row = 3, sticky="ew", padx = 5, pady = 5)

    directory_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    directory_label <- tklabel(directory_frame,textvariable=directory_text, justify="left")
    tkpack(directory_label,anchor="w",padx=5,pady=5)
    tkgrid(directory_frame, column = 1, row = 4, sticky="ew", padx = 5, pady = 5)

    # OK and Cancel buttons
    onOK <- function() {
      tkdestroy(tt)
    }

    onCancel <- function() {
      # TODO: revert to original values
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
  create_gui(max_width=max_width)
}
