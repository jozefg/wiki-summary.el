## wiki-summary.el

[![MELPA](http://melpa.org/packages/wiki-summary-badge.svg)](http://melpa.org/#/wiki-summary)

Grab the Wikipedia summary for a term and display the result in a
buffer. Inspired by [WikiTerm][wikiterm].

Currently this exports two functions:

 * `wiki-summary` which will interactively prompt you for the string
of the page title and gives back the information in a buffer
`*wiki-summary*`

 * `wiki-summary-insert` which will interactively prompt you for the string
of the page title and insert (or barf if read only) the summary into the current buffer at point.

You can specify a language encoding (as per the URL for that language) with `M-x customize-variable` and `wiki-summary-language-string`.

Terms with no direct matches are converted into queries and a list of potential terms is offered to the user. To include snippet text with these potential matches, you can toggle the `wiki-summary-showsnippet` variable.

[wikiterm]: https://gist.github.com/thedouglenz/193defdb711e0e54d68a
