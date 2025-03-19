require(tcltk)
tt <- tktoplevel()

#Scrollbars
scr.d <- tkscrollbar(
  tt, repeatinterval=4,
  command=function(...) {
    tkyview(t.d,...)
    tkyview(t.m,...)
    tkyview(t.s,...)
    tkyview(t.a,...)
  }
)

#Listboxes
t.d <- tklistbox(tt, selectmode="browse",yscrollcommand=function(...) tkset(scr.d,...), width=20,background="white", exportselection=0)
t.m <- tklistbox(tt, selectmode="Single",
                 yscrollcommand=function(...) tkset(scr.d,...), width=20, background="white", exportselection=0)
t.s <- tklistbox(tt,selectmode="Single",
                 yscrollcommand=function(...) tkset(scr.d,...), width=30, background="white", exportselection=0)
t.a <- tklistbox(tt,selectmode="Single",
                 yscrollcommand=function(...) tkset(scr.d,...), width=35, background="white", exportselection=0)

#Place them on the window
tkgrid(tklabel(tt,text="Select subject property:"))
tkgrid(tklabel(tt,text=""))
tkgrid(t.d, scr.d, t.m, t.s, t.a)

tkgrid.configure(scr.d,rowspan=4,sticky="nsw")

#Filling up the listboxes
for (i in 1:100)
{
  tkinsert(t.d, "end", i)
}

for (i in letters)
{
  tkinsert(t.m, "end", i)
}

for (i in rnorm(100))
{
  tkinsert(t.s, "end", i)
}

for (i in letters)
{
  tkinsert(t.a, "end", i)
}

tkwait.window(tt)
