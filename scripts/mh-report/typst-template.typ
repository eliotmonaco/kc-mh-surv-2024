#let template(
  title: "Title",
  author: "Author",
  body,
) = {
  // Metadata
  set document(title: title, author: author)

  // Formatting & layout
  set text(font: "Calibri", size: 12pt)

  set page(
    margin: (x: .5in, y: .5in),
    footer: context {
      if counter(page).get().first() > 1 [ // footer only after p1
        #upper(title)
        #h(1fr)
        #counter(page).display()
      ]
    }
  )

  // Title page
  page(background: rect(fill: navy, width: 100%, height: 3in))[
    #set text(fill: white, weight: "bold")
    #set align(center)
    #set block(width: 6in)
    #v(1fr)
    #grid(
      columns: 1, rows: 1,
      block[
        #text(size: 32pt)[#title] \
        #text(size: 24pt)[#author]
      ]
    )
    #v(1fr)
  ]

  // Table of contents
  show outline: it => {
    show heading: set align(center)
    it
  }

  outline(indent: n => n * 1.2em)

  pagebreak()

  // Document body
  body
}

#let rectwhite(
  body,
  height: auto,
  width: 100%
) = {
  rect(
    stroke: rgb("#dddddd") + 3pt,
    inset: 10pt,
    width: width,
    height: height,
    [#body]
  )
}

#let rectgrey(
  body,
  height: auto,
  width: 100%
) = {
  rect(
    stroke: rgb("#dddddd") + 3pt,
    fill: rgb("#dddddd"),
    inset: 10pt,
    width: width,
    height: height,
    [#body]
  )
}



