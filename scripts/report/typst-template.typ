#let template(
  title: "Title",
  subtitle: "Subtitle",
  author: "Author",
  body,
) = {
  // METADATA

  set document(title: title, author: author)

  // FORMATTING & LAYOUT

  set text(font: "Lato", size: 12pt)

  set page(
    margin: (x: 1in, y: 1in),
    columns: 1,
    footer: context {
      if counter(page).get().first() > 1 [ // footer only after p1
        #set text(10pt)
        #upper(title) | #upper("2024 Report")
        #h(1fr)
        #counter(page).display()
      ]
    },
    footer-descent: 50%
  )

  set list(indent: .25in)

  set enum(indent: .25in)

  // FIGURES & TABLES

  show figure.caption: set text(size: 10pt)

  show figure.where(
    kind: table
  ): set figure.caption(position: top)

  set table(stroke: none)

  // TITLE PAGE

  page(
    background: rect(
      fill: rgb("#318CCC"),
      stroke: none,
      width: 100%,
      height: 2.5in,
      inset: 0pt
    )[
      #align(bottom)[#image("img/fountain.svg", height: 2in)]
    ]
  )[
    #set text(fill: white, weight: "bold")
    #set align(center + horizon)
    #set block(width: 6.5in)
    #grid(
      columns: 1, rows: 1,
      block[
        #v(.3in)
        #text(size: 30pt)[#title] \
        #v(.01in)
        #text(size: 26pt)[2024 Report] \  // no quarto/pandoc support yet for
        #v(.1in)                          // the yaml param subtitle
        #text(size: 16pt)[#author.replace("\\", "")]
      ]
    )
  ]

  // TABLE OF CONTENTS

  show outline: it => {
    show heading: set align(center)
    it
  }

  outline(
    depth: 3,
    indent: n => n * 1.2em
  )

  pagebreak()

  // HEADINGS

  show heading.where(level: 1): it => block(
    width: 100%,
    fill: rgb("#3A80BF")
  )[
    #set text(fill: white, size: 17pt)
    #pad(top: 5pt, bottom: 5pt, upper(it))
  ]

  show heading.where(level: 2): it => block(
    width: 100%,
    fill: rgb("#F4C134")
  )[
    #set text(fill: black, size: 15.5pt)
    #pad(top: 5pt, bottom: 5pt, it)
  ]

  show heading.where(level: 3): it => block(
    width: 100%,
    fill: rgb("#8DBD3f")
  )[
    #set text(fill: white, size: 14pt)
    #pad(top: 5pt, bottom: 5pt, it)
  ]

  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    it
  }

  // DOCUMENT BODY

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
    stroke: rgb("#eeeeee") + 3pt,
    fill: rgb("#eeeeee"),
    inset: 10pt,
    width: width,
    height: height,
    [#body]
  )
}

#let rectclear(
  body,
  height: auto,
  width: 100%
) = {
  rect(
    stroke: none,
    fill: none,
    inset: 0pt,
    width: width,
    height: height,
    [#body]
  )
}

#let figblock = {
  block.with(
    width: 100%,
    breakable: false,
    above: 30pt,
    below: 30pt
  )
}


