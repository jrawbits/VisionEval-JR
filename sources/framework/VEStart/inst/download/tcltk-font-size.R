library(tcltk)

tt <- tktoplevel()
label <- tklabel(tt, text = "Hello, World!")
tkpack(label)

# Increase the font size to 16 points
tkconfigure(label, font = "-family {Helvetica} -size 16")

library(tcltk)

tt <- tktoplevel()
button <- tkbutton(tt, text = "Click Me")
tkpack(button)

# Increase the font size and make it bold
tkconfigure(button, font = "-family {Times} -size 14 -weight bold")

library(tcltk)

tt <- tktoplevel()
entry <- tkentry(tt)
tkpack(entry)

# Increase the font size of the entry
tkconfigure(entry, font = "-family {Courier} -size 12")

library(tcltk)

tt <- tktoplevel()
listbox <- tklistbox(tt)
tkinsert(listbox, "end", "Item 1", "Item 2", "Item 3")
tkpack(listbox)

# Increase the font size of the listbox
tkconfigure(listbox, font = "-family {Arial} -size 13")

library(tcltk)

tt <- tktoplevel()
myFont <- tkfont.create(family = "Helvetica", size = 18, weight = "bold")

label1 <- tklabel(tt, text = "Label 1")
label2 <- tklabel(tt, text = "Label 2")
tkpack(label1)
tkpack(label2)

tkconfigure(label1, font = myFont)
tkconfigure(label2, font = myFont)