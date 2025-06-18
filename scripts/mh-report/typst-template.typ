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
    margin: (x: 0.5in, y: 0.5in),
    footer: context {
      if counter(page).get().first() > 1 [ // footer only after p1
        #upper(title)
        #h(1fr)
        #counter(page).display()
      ]
    }
  )

  // Title page
  page(background: [#image("banner.png", width: 100%)])[
    #v(1fr)
    #block(width: 100%)[
      #place(center + horizon, dy: -10pt)[
        #text(fill: white, size: 20pt, weight: "bold")[
          #title \
          #author
        ]
      ]
    ]
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



