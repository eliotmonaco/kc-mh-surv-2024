#show: doc => template(
  $if(title)$ title: "$title$", $endif$

  $if(author)$ author: "$author$", $endif$

  doc,
)

