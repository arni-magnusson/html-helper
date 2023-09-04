# html-helper-mode 4.3.0 (2023-09-05)

* Improved the indentation engine so it indicates that the buffer has not been
  modified, if that is the case.

* Removed keybindings that were violating Emacs major mode conventions.

* Removed archaic timestamp utilities.




# html-helper-mode 4.2.0 (2023-09-05)

* Improved indentation when tags `<li>`, `<dt>`, `<dd>`, `<th>`, and `<td>` have
  attributes. Added indentation support for tags `<thead>` and `<tbody>`.

* Improved indentation when tags `<dl>`, `<ul>`, `<ol>`, `<menu>`, `<dir>`, and
  `<tr>` have attributes. Added indentation support for tags `<form>`,
  `<style>`, and `<div>`.

* Added indentation support for curly braces `{}`.

* Changed default `html-helper-item-continue-indent` to 2.

* Changed default `html-helper-search-limit` to 20000.

* Added syntax highlighting of `/*` CSS comments `*/`.

* Changed syntax highlighting of `<title>` and `<h1>` entries to be the same as
  `<h2>`, `<h3>`, and `<h4>`.

* Changed syntax highlighting of `<form>`, `<img>`, and `<input>` to be as
  generic tags.




# html-helper-mode 4.1.1 (2023-09-01)

* Formatted code to 80 cols.




# html-helper-mode 4.1.0 (2023-09-01)

* Changed `font-lock-reference-face` to `font-lock-constant-face` to revive
  syntax highlighting in Emacs 29.1.

* Removed obsolete XEmacs support.




# html-helper-mode 4.0.1 (2023-09-01)

* Removed obsolete `html-helper-emacs18` support.

* Removed obsolete `hilit19` support.

* Removed misleading `(provide 'html-mode)` feature.

* Removed ambiguous `(run-hooks 'html-load-hook)`. Use `html-helper-load-hook`
  instead.




# html-helper-mode 4.0.0 (2023-09-01)

* Cosmetic changes in code and comments.

* Same functionality as version 2.19.1.1.

* Maintainer is Arni Magnusson.




# help-helper-mode 2.19.1.1 (1998-08-06)

* Starting point for 2023 development.

* Maintainer is Nelson Minar.
