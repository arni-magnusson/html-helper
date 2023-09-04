;;; html-helper-mode.el --- Major mode for editing HTML files

;; Copyright (C) 1994-1997 Nelson Minar
;; Copyright (C) 1998-2022 Nelson Minar and Ulrik Dickow
;; Copyright (C) 2023-     Nelson Minar, Ulrik Dickow, and Arni Magnusson

;; Author:     Nelson Minar
;; Maintainer: Arni Magnusson
;; Keywords:   languages
;; URL:        https://github.com/arni-magnusson/html-helper

(defconst html-helper-mode-version "4.3.0" "HTML Helper Mode version number.")

;;; Commentary:

;; `html-helper-mode' makes it easier to write HTML documents. This mode
;; handles inserting HTML codes in a variety of ways (keybindings, menus,
;; completion in the buffer). It also supports indentation, skeletons for new
;; documents, and a variety of other things.
;;
;; Installation:
;;
;; Add this line in your .emacs:
;;   (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; To invoke `html-helper-mode' automatically on .html files, do this:
;;   (add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))
;;
;; Configuration:
;;
;; See the "user variables" section. There are variables you want to configure,
;; particularly `html-helper-address-string' and `html-helper-use-expert-menu'.

;; Acknowledgements:
;;
;; Ulrik Dickow <dickow@nbi.dk> for the font-lock code.
;; Arni Magnusson <thisisarni@gmail.com> for updating the 1998 code to modern
;;   Emacs and HTML.
;; David Kagedal <davidk@lysator.liu.se> for the tempo code which forms the core
;;   of the HTML insertion, as well as the HTML+ tags.
;; Marc Hedlund <march@europa.com> for general encouragement and many helpful
;;   suggestions, especially with HTML 2.0 compliance and form design.
;; Denis Howe <dbh@doc.ic.ac.uk> for adding browse-url support.
;; Everyone who submitted a version of menus, 16 in all!
;; Marc Andreessen <marca@mcom.com> for writing the original `html-mode'.

;;; Code:

;; 1  Preamble

(require 'tempo)
(require 'easymenu)

;; 2  User variables

(defvar html-helper-address-string ""
  "*The default author string of each file.")

(defvar html-helper-use-expert-menu nil
  "*If not nil, then use the full HTML menu.")

(defvar html-helper-build-new-buffer t
  "*If not nil, then insert `html-helper-new-buffer-strings' for new buffers.")

(defvar html-helper-htmldtd-version
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"
  "*Version of HTML DTD you're using.")

(defvar html-helper-user-menu nil
  "*Extra items to put in the HTML expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus).")

(defvar html-helper-basic-offset 2
  "*Basic indentation size used for list indentation.")

(defvar html-helper-item-continue-indent 2
  "*Indentation of lines that follow a <li> item.")

(defvar html-helper-never-indent nil
  "*If not nil, the indentation code for html-helper is turned off.")

(defvar html-helper-mode-hook nil
  "*Hook run when `html-helper-mode' is started.")

(defvar html-helper-load-hook nil
  "*Hook run when `html-helper-mode' is loaded.")

(defvar html-helper-new-buffer-template
  '(html-helper-htmldtd-version
    "<html> <head>\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "<h1>" p "</h1>\n\n"
    p
    "\n\n<hr>\n"
    "<address>" html-helper-address-string "</address>\n"
    "\n</body> </html>\n")
  "*Template for new buffers.
Inserted by `html-helper-insert-new-buffer-strings' if
`html-helper-build-new-buffer' is set to t")

(defvar html-helper-types-to-install
  '(anchor list header logical phys textel entity image head form)
  "*List of tag types to install when `html-helper-mode' is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

;; 3  Syntax table and abbrev table

(defvar html-helper-mode-syntax-table nil
  "Syntax table for html-helper.")

(if (not html-helper-mode-syntax-table)
    (progn
      (setq html-helper-mode-syntax-table
            (make-syntax-table text-mode-syntax-table))
      (modify-syntax-entry ?<  "(>  " html-helper-mode-syntax-table)
      (modify-syntax-entry ?>  ")<  " html-helper-mode-syntax-table)
      (modify-syntax-entry ?\" ".   " html-helper-mode-syntax-table)
      (modify-syntax-entry ?\\ ".   " html-helper-mode-syntax-table)
      (modify-syntax-entry ?'  "w   " html-helper-mode-syntax-table)))

(defvar html-helper-mode-abbrev-table nil
  "Abbrev table used while in `html-helper-mode'.")
(define-abbrev-table 'html-helper-mode-abbrev-table ())

;; 4  Keymap and menu

(defvar html-helper-mode-map (make-sparse-keymap)
  "Keymap for html-helper.")

(defvar html-helper-mode-menu nil
  "Menu for html-helper. Clobbered and rebuilt by `html-helper-install-menu'.")

;; html-helper-mode has a concept of "type" of tags. Each type is a list of tags
;; that all go together in one keymap and one menu. Types can be added to the
;; system after html-helper has been loaded, briefly by doing
;; html-helper-add-type-to-alist, then html-helper-install-type,
;; then html-helper-add-tag (for each tag) then html-helper-rebuild-menu. See
;; the mode documentation for more detail.

;; 5  Accessor functions for html-helper-type-alist

(defconst html-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `html-helper-add-type-to-alist'.")

(defun html-helper-keymap-for (type)
  "Accessor function for alist: for TYPE, return keymap or nil."
  (nth 0 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-key-for (type)
  "Accessor function for alist: for TYPE, return keybinding or nil."
  (nth 1 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-for (type)
  "Accessor function for alist: for TYPE, return menu or nil."
  (nth 2 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-string-for (type)
  "Accessor function for alist: for TYPE, return menustring or nil."
  (nth 3 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (html-helper-menu-string-for type)
        (eval (html-helper-menu-for type))))

(defun html-helper-add-type-to-alist (type)
  "Add a TYPE specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq html-helper-type-alist (cons type html-helper-type-alist)))

;; Types provided by html-helper-mode
(mapcar 'html-helper-add-type-to-alist
        '((entity  . (nil nil html-helper-entity-menu
                          "Insert Character Entities"))
          (textel  . (nil nil html-helper-textel-menu
                          "Insert Text Elements"))
          (head    . (html-helper-head-map
                      "\C-c\C-b" html-helper-head-menu
                      "Insert Structural Elements"))
          (header  . (html-helper-header-map
                      "\C-c\C-t" html-helper-header-menu
                      "Insert Headers"))
          (anchor  . (html-helper-anchor-map
                      "\C-c\C-a" html-helper-anchor-menu
                      "Insert Hyperlinks"))
          (logical . (html-helper-logical-map
                      "\C-c\C-s" html-helper-logical-menu
                      "Insert Logical Styles"))
          (phys    . (html-helper-phys-map
                      "\C-c\C-p" html-helper-phys-menu
                      "Insert Physical Styles"))
          (list    . (html-helper-list-map
                      "\C-c\C-l" html-helper-list-menu
                      "Insert List Elements"))
          (form    . (html-helper-form-map
                      "\C-c\C-f" html-helper-form-menu
                      "Insert Form Elements"))
          (image   . (html-helper-image-map
                      "\C-c\C-i" html-helper-image-menu
                      "Insert Inlined Images"))))

;; Once html-helper-mode is aware of a type, it can then install the type:
;; arrange for keybindings, menus, etc.

(defconst html-helper-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

(defun html-helper-install-type (type)
  "Install a new tag TYPE: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with `html-helper-add-type-to-alist'."
  (setq html-helper-installed-types (cons type html-helper-installed-types))
  (let ((keymap (html-helper-keymap-for type))
        (key (html-helper-key-for type))
        (menu (html-helper-menu-for type))
        (menu-string (html-helper-menu-string-for type)))
    (and key
         (progn
           (set keymap nil)
           (define-prefix-command keymap)
           (define-key html-helper-mode-map key keymap)))
    (and menu
         (progn
           (set menu nil)))))

;; Install the default types
(mapcar 'html-helper-install-type html-helper-types-to-install)

;; Special mode keys
(mapcar
 (function (lambda (l) (define-key html-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;; Extra commands that HTML helper supports that aren't insertions
(defvar html-helper-mode-functions-map nil
  "Keymap for extra html-helper functions.")
(define-prefix-command 'html-helper-mode-functions-map)
(define-key html-helper-mode-map "\C-c\C-z"
            'html-helper-mode-functions-map)

;; Indentation keys - only rebind these if the user wants indentation
(if (not html-helper-never-indent)
    (progn
      (define-key html-helper-mode-map "\t"   'html-helper-indent-command)
      (define-key html-helper-mode-map "\C-m" 'newline-and-indent      )))

;; Browse URL stuff
(if (fboundp 'browse-url-of-file)
    (define-key html-helper-mode-functions-map "v" 'browse-url-of-file))
(if (and (boundp 'browse-url-browser-function)
         (fboundp browse-url-browser-function))
    (define-key html-helper-mode-functions-map "u" browse-url-browser-function))

;; 6  Basic tags

(defvar html-helper-tempo-tags nil
  "List of tags used in completion.")

(defun html-helper-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately Emacs Lisp is forgiving."
  (let* ((s (copy-sequence input-string))
         (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
          (aset s l ?\-))
      (setq l (1- l)))
    (concat "html-" (downcase s))))

(defun html-helper-add-tag (l)
  "Add a new tag to `html-helper-mode'.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(html-helper-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
         (keymap (html-helper-keymap-for type))
         (menu (html-helper-menu-for type))
         (key (nth 1 l))
         (completer (nth 2 l))
         (name (nth 3 l))
         (tag (nth 4 l))
         (doc (nth 5 l))
         (command (tempo-define-template (html-helper-string-to-symbol name)
                                         tag completer doc
                                         'html-helper-tempo-tags)))
    (if (null (memq type html-helper-installed-types))     ; type loaded?
        t                                                  ; no, do nothing.
      (if (stringp key)                                    ; bind key somewhere?
          (if keymap                                       ; special keymap?
              (define-key (eval keymap) key command)       ; t:   bind to prefix
            (define-key html-helper-mode-map key command)) ; nil: bind to global
        t)
      (if menu                                             ; is there a menu?
          (set menu                                        ; good, cons it in
               (cons (vector name command t) (eval menu))))
      )))

;; Backwards compatability
(fset 'html-helper-add-cookie 'html-helper-add-tag)

;; 7  HTML tags

;; Order here is significant: within a tag type, menus and mode help go in the
;; reverse order of what you see here. Sorry about that, it's not easy to fix.

(mapcar
 'html-helper-add-tag
 '(
   ;; Entities
   (entity "\C-c#"  "&#"     "Ascii Code"        ("&#" (r "Ascii: ") ";"))
   (entity "\C-c\"" "&quot;" "Quotation mark"    ("&quot;"))
   (entity "\C-c$"  "&reg;"  "Registered"        ("&reg;"))
   (entity "\C-c@"  "&copy;" "Copyright"         ("&copy;"))
   (entity "\C-c-"  "&shy;"  "Soft Hyphen"       ("&shy;"))
   (entity "\C-c "  "&nbsp;" "Nonbreaking Space" ("&nbsp;"))
   (entity "\C-c&"  "&amp;"  "Ampersand"         ("&amp;"))
   (entity "\C-c>"  "&gt;"   "Greater Than"      ("&gt;"))
   (entity "\C-c<"  "&lt;"   "Less Than"         ("&lt;"))

   ;; Logical styles
   (logical "b" "<blockquote>" "Blockquote"
            ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c" "<code>" "Code" ("<code>" (r "Code: ") "</code>"))
   (logical "x" "<samp>" "Sample" ("<samp>" (r "Sample code") "</samp>"))
   (logical "r" "<cite>" "Citation" ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k" "<kbd>" "Keyboard Input" ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v" "<var>" "Variable" ("<var>" (r "Variable: ") "</var>"))
   (logical "d" "<dfn>" "Definition" ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a" "<address>" "Address" ("<address>" r "</address>"))
   (logical "e" "<em>" "Emphasized" ("<em>" (r "Text: ") "</em>"))
   (logical "s" "<strong>" "Strong" ("<strong>" (r "Text: ") "</strong>"))
   (logical "p" "<pre>" "Preformatted" ("<pre>" (r "Text: ") "</pre>"))

   ;; Physical styles
   (phys "s" "<strike>" "Strikethru" ("<strike>" (r "Text: ") "</strike>"))
   (phys "u" "<u>"      "Underline"  ("<u>"      (r "Text: ") "</u>"))
   (phys "i" "<i>"      "Italic"     ("<i>"      (r "Text: ") "</i>"))
   (phys "b" "<b>"      "Bold"       ("<b>"      (r "Text: ") "</b>"))
   (phys "f" "<tt>"     "Fixed"      ("<tt>"     (r "Text: ") "</tt>"))

   ;; Headers
   (header "6" "<h6>" "Header 6" ("<h6>" (r "Header: ") "</h6>"))
   (header "5" "<h5>" "Header 5" ("<h5>" (r "Header: ") "</h5>"))
   (header "4" "<h4>" "Header 4" ("<h4>" (r "Header: ") "</h4>"))
   (header "3" "<h3>" "Header 3" ("<h3>" (r "Header: ") "</h3>"))
   (header "2" "<h2>" "Header 2" ("<h2>" (r "Header: ") "</h2>"))
   (header "1" "<h1>" "Header 1" ("<h1>" (r "Header: ") "</h1>"))

   ;; Forms
   (form "o" "<option>" "Option"
         (& "<option>" > ))
   (form "v" "<option value" "Option with Value"
         (& "<option value=\"" (r "Value: ") "\">" >))
   (form "s" "<select" "Selections"
         ("<select name=\"" (p "Name: ") "\">\n<option>" > "\n</select>")
         "<select")
   (form "z" "<input" "Reset Form"
         ("<input type=\"RESET\" value=\"" (p "Reset button text: ") "\">"))
   (form "b" "<input" "Submit Form"
         ("<input type=\"SUBMIT\" value=\"" (p "Submit button text: ") "\">"))
   (form "i" "<input" "Image Field"
         ("<input type=\"IMAGE\" name=\"" (p "Name: ") "\" src=\""
          (p "Image URL: ") "\">"))
   (form "h" "<input" "Hidden Field"
         ("<input type=\"HIDDEN\" name=\"" (p "Name: ") "\" value=\""
          (p "Value: ") "\">"))
   (form "p" "<textarea" "Text Area"
         ("<textarea name=\"" (p "Name: ") "\" rows=\"" (p "Rows: ")
          "\" cols=\"" (p "Columns: ") "\">" r "</textarea>"))
   (form "c" "<input" "Checkbox"
         ("<input type=\"CHECKBOX\" name=\"" (p "Name: ") "\">"))
   (form "r" "<input" "Radiobutton"
         ("<input type=\"RADIO\" name=\"" (p "Name: ") "\">"))
   (form "t" "<input" "Text Field"
         ("<input type=\"TEXT\" name=\"" (p "Name: ") "\" size=\"" (p "Size: ")
          "\">"))
   (form "f" "<form" "Form"
         ("<form action=\"" (p "Action: ") "\" method=\"" (p "Method: ")
          "\">\n</form>\n"))

   ;; Lists
   (list "t" "<dt>" "Definition Item"
         (& "<dt>" > (p "Term: ") "\n<dd>" > (r "Definition: ")))
   (list "l" "<li>" "List Item"
         (& "<li>" > (r "Item: ")))
   (list "r" "<dir>" "DirectoryList"
         (& "<dir>" > "\n<li>" > (r "Item: ") "\n</dir>" >))
   (list "m" "<menu>" "Menu List"
         (& "<menu>" > "\n<li>" > (r "Item: ") "\n</menu>" >))
   (list "o" "<ol>" "Ordered List"
         (& "<ol>" > "\n<li>" > (r "Item: ") "\n</ol>" >))
   (list "d" "<dl>" "Definition List"
         (& "<dl>" > "\n<dt>" > (p "Term: ") "\n<dd>" > (r "Definition: ")
            "\n</dl>" >))
   (list "u" "<ul>" "Unordered List"
         (& "<ul>" > "\n<li>" > (r "Item: ") "\n</ul>" >))

   ;; Anchors
   (anchor "n" "<a name=" "Link Target"
           ("<a name=\"" (p "Anchor name: ") "\">" (r "Anchor text: ") "</a>"))
   (anchor "l" "<a href=" "Hyperlink"
           ("<a href=\"" (p "URL: ") "\">" (r "Anchor text: ") "</a>"))

   ;; Graphics
   (image "a" nil "Aligned Image"
          ("<img align=\"" (r "Alignment: ") "\" src=\"" (r "Image URL: ")
           "\">"))
   (image "i" "<img src=" "Image"
          ("<img src=\"" (r "Image URL: ") "\">"))
   (image "e" "<img align=" "Aligned Image With Alt. Text"
          ("<img align=\"" (r "Alignment: ") "\" src=\"" (r "Image URL: ")
           "\" alt=\"" (r "Text URL: ") "\">"))
   (image "t" "<img alt=" "Image With Alternate Text"
          ("<img alt=\"" (r "Text URL: ") "\" src=\"" (r "Image URL: ") "\">"))

   ;; Text elements
   (textel "\C-c="    nil "Horizontal Line" (& "<hr>\n"))
   (textel "\C-c\C-m" nil "Line Break"      ("<br>\n"))
   (textel "\e\C-m"   nil "Paragraph"       ("<p>\n"))

   ;; Head elements
   (head "H" "<head>" "Head"
         ("<head>\n" "</head>\n"))
   (head "B" "<body>" "Body"
         ("<body>\n" "</body>\n"))
   (head "i" "<isindex>" "Isindex"
         ("<isindex>\n"))
   (head "n" "<nextid>" "Nextid"
         ("<nextid>\n"))
   (head "h" "<meta http-equiv=" "HTTP Equivalent"
         ("<meta http-equiv=\"" (p "Equivalent: ") "\" content=\""
          (r "Content: ") "\">\n"))
   (head "m" "<meta name=" "Meta Name"
         ("<meta name=\"" (p "Name: ") "\" content=\"" (r "Content: ") "\">\n"))
   (head "l" "<link" "Link"
         ("<link href=\"" p "\">"))
   (head "b" "<base" "Base"
         ("<base href=\"" r "\">"))
   (head "t" "<title>" "Title"
         ("<title>" (r "Document title: ") "</title>"))))

;; 8  Smart insert item

;; There are two different kinds of items in HTML - those in regular lists <li>
;; and those in dictionaries <dt>..<dd>. This command will insert the
;; appropriate one depending on context.

(defun html-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  (interactive "*P")
  (let ((case-fold-search t))
    (if
        (save-excursion
          (re-search-backward
           "<li>\\|<dt>\\|<ul>\\|<ol>\\|<dd>\\|<menu>\\|<dir>\\|<dl>" nil t)
          (looking-at "<dt>\\|<dl>\\|<dd>"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-list-item arg))))

;; Special keybindings in the prefix maps (not in the list of tags)
(and (boundp 'html-helper-list-map)
     (define-key html-helper-list-map "i" 'html-helper-smart-insert-item))

;; And, special menu bindings
(and (boundp 'html-helper-list-menu)
     (setq html-helper-list-menu
           (cons '["List Item" html-helper-smart-insert-item t]
                 html-helper-list-menu)))

;; 9  Menu support

;; Menus are built for easymenu. html-helper-add-tag builds submenus based on
;; tag type, the expert menu code lumps them together into one list and calls
;; easy-menu-define.

(defvar html-helper-novice-menu
  '("HTML"
    ["Insert Paragraph" tempo-template-html-paragraph t]
    ["Insert Hyperlink" tempo-template-html-hyperlink t]
    ["Insert Big Header" tempo-template-html-header-2 t]
    ["Insert Unordered List" tempo-template-html-unordered-list t]
    ["Insert List Item" html-helper-smart-insert-item t]
    ["Insert Inlined Image" tempo-template-html-image-with-alternate-text t]
    ["Turn on Expert Menu" html-helper-toggle-expert-menu t])
  "Menu for novices, only installed if `html-helper-use-expert-menu is nil'.")

(defun html-helper-menu nil
  "Return the proper menu, based on `html-helper-use-expert-menu'."
  (if html-helper-use-expert-menu
      (html-helper-expert-menu)
    html-helper-novice-menu))

(defun html-helper-rebuild-menu nil
  "Rebuild and install the HTML menu (using `easy-menu-define').
If `html-helper-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (html-helper-menu)))
    (easy-menu-remove menu)
    (easy-menu-define html-helper-mode-menu-symbol
      html-helper-mode-map "HTML menus" menu)
    (easy-menu-add menu html-helper-mode-map)))

(defun html-helper-toggle-expert-menu (&optional arg)
  "Toggle full HTML menus. Optional ARG acts like minor-mode args."
  (interactive "P")
  (setq html-helper-use-expert-menu
        (if (null arg) (not html-helper-use-expert-menu)
          (> (prefix-numeric-value arg) 0)))
  (html-helper-rebuild-menu))

;; If `browse-url-of-file' loaded, add this in the novice menu
(if (fboundp 'browse-url-of-file)
    (setq html-helper-novice-menu
          (append html-helper-novice-menu
                  (list ["Load This Buffer in Browser" browse-url-of-file t]))))

;; Expert menus: consed up out of html-helper-installed-types
(defun html-helper-expert-menu ()
  "This menu is based on the current value of `html-helper-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; First, reset this so we can call this again and again
  (setq html-helper-mode-menu nil)

  ;; Cons in the toggle of the menu
  (setq html-helper-mode-menu
        (cons '["Turn on Novice Menu"
                html-helper-toggle-expert-menu t]
              html-helper-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq html-helper-mode-menu
        (append html-helper-user-menu html-helper-mode-menu))

  ;; Now cons in the `browse-url-of-file' functions
  (if (fboundp 'browse-url-of-file)
      (setq html-helper-mode-menu
            (cons '["Load this Buffer in Browser" browse-url-of-file t]
                  html-helper-mode-menu)))
  (if (and (boundp 'browse-url-browser-function)
           (fboundp browse-url-browser-function))
      (setq html-helper-mode-menu
            (cons (vector "Browse URL at point" browse-url-browser-function t)
                  html-helper-mode-menu)))

  ;; Now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
               (setq html-helper-mode-menu
                     (cons (html-helper-normalized-menu-for type)
                           html-helper-mode-menu))))
   html-helper-installed-types)

  ;; Now tack on our name
  (setq html-helper-mode-menu (cons "HTML" html-helper-mode-menu))
  html-helper-mode-menu)

(html-helper-rebuild-menu)

;; 10 Context guessing

;; Guess where we are in indented lists based on the last list token. It would
;; be much better to try to match </ul> to <ul>, and </ol> to <ol> etc, but that
;; is pretty unwieldy and slow. Note, we make select/option look like a list
;; structure too, so indentation works. This is a bit weird, but it's ok.

(defvar html-helper-any-list-item-start
  ;; Distinguish between <li> and <link>
  "<li[ >]\\|<dt\\|<dd\\|<option\\|<th\\|<td\\|<thead\\|<tbody")
(defvar html-helper-any-list-item-end
  "</li>\\|</dt>\\|</dd>\\|</th>\\|</td>\\|</thead>\\|</tbody>")
(defvar html-helper-any-list-start
  "<dl\\|<ul\\|<ol\\|<menu\\|<dir\\|<form\\|<select\\|<table\\|<tr\\|<style\\|\
<div\\|{")
(defvar html-helper-any-list-end
  "</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</form>\\|</select>\\|</table>\\|\
</tr>\\|</style>\\|</div>\\|}")
(defvar html-helper-any-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
          html-helper-any-list-start
          html-helper-any-list-end
          html-helper-any-list-item-start
          html-helper-any-list-item-end))
(defvar html-helper-indentation-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
          html-helper-any-list-start
          html-helper-any-list-end
          html-helper-any-list-item-start))
(defvar html-helper-search-limit 20000
  "Limit on how far back we search.")

(defun html-helper-context-symbol ()
  "Return the symbol the last match (against `html-helper-any-list') found."
  (cond ((match-beginning 1) 'list-start)
        ((match-beginning 2) 'list-end)
        ((match-beginning 3) 'item-start)
        ((match-beginning 4) 'item-end)
        (t 'error)))

(defun html-helper-guess-prev-context ()
  "Figure out the last list-type tag before point relevant to indentation.
Returns 'item-start if the last list tag is a list item start
        'start      if the last list tag is the start of a list
        'end        if the last list tag is the end of a list.
Ignores list item ends, because those aren't reliable for indentation."
  (save-excursion
    (let* ((lim (max (point-min) (- (point) html-helper-search-limit)))
           (context (if (re-search-backward html-helper-indentation-list lim t)
                        (html-helper-context-symbol)
                      nil)))
      (cons context (current-indentation)))))

(defun html-helper-print-prev-context ()
  "Show last tag before point relevant to indentation."
  (interactive)
  (message "%s" (html-helper-guess-prev-context)))

;; 11 Indentation

(defvar html-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun html-helper-indent-command ()
  "Command for indenting HTML to the appropriate column.
Calls `html-helper-indent' which tries to examine how many levels down
in nested lists we are and does the appropriate indentation.'
See also `html-helper-basic-offset', `html-helper-item-continue-indent',
and `html-helper-never-indent'."
  (interactive)
  (html-helper-indent))

;; Some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

(defun html-helper-indent ()
  "Indentation workhorse function."
  (if (not html-helper-never-indent)
      (progn
        (let ((m (point-marker))
              (bol (progn (beginning-of-line) (point))))

          ;; Unindent the line
          (delete-region (point) (progn (back-to-indentation) (point)))

          (let* ((where (html-helper-guess-prev-context))
                 (prev-context (car where))
                 (this-context nil)
                 (previ (cdr where))
                 (newi (cond
                        ((eq prev-context 'list-end) previ)
                        ((eq prev-context 'item-start) previ)
                        ((eq prev-context 'list-start)
                         (+ previ html-helper-basic-offset))
                        (t previ))))

            ;; newi is set to the basic indentation, now adjust indentation
            ;; based on what the current line is.
            (if (looking-at html-helper-any-list)
                (progn
                  (setq this-context (html-helper-context-symbol))
                  (cond
                   ;; Item start or end and last line was a list-end: go back
                   ((and
                     (or (eq this-context 'item-start)
                         (eq this-context 'item-end))
                     (eq prev-context 'list-end))
                    (setq newi (- newi html-helper-item-continue-indent)))

                   ;; End of list and last line was an end: go backwards twice
                   ((and (eq this-context 'list-end)
                         (eq prev-context 'list-end))
                    (setq newi (- newi html-helper-item-continue-indent
                                  html-helper-basic-offset)))

                   ;; Any other end of list? Indent negative
                   ((and (eq this-context 'list-end))
                    (setq newi (- newi html-helper-basic-offset)))

                   ;; Start of list and last line beginning of item, go forwards
                   ((and (eq this-context 'list-start)
                         (eq prev-context 'item-start))
                    (setq newi (+ newi html-helper-item-continue-indent)))))

              ;; Default: no special case, indent forward for text
              (cond
               ;; Last line an item? Beginning of continued item - go forward
               ((eq prev-context 'item-start)
                (setq newi (+ newi html-helper-item-continue-indent)))))

            (if html-helper-print-indent-info
                (message
                 "Last Context: %s, This Context: %s, Previous: %s New: %s"
                 prev-context this-context previ newi))

            ;; Just in case
            (if (< newi 0)
                (setq newi 0))
            (indent-to newi newi)

            ;; Adjust point to where it was before, or at start of indentation
            (goto-char (marker-position m))
            (if (< (current-column) (current-indentation))
                (back-to-indentation)))))))

;; 12 Completion finder for tempo

(defvar html-helper-completion-finder "\\(\\(<\\|&\\).*\\)\\="
  "Passed to `tempo-use-tag-list', used to find tags to complete.")

;; 13 Insert new buffer strings

(tempo-define-template "html-skeleton" html-helper-new-buffer-template
                       nil
                       "Insert a skeleton for a HTML document")

(defun html-helper-insert-new-buffer-strings ()
  "Insert `html-helper-new-buffer-strings'."
  (tempo-template-html-skeleton))

;; 14 Main function

(defun html-helper-mode ()
  "Mode for editing HTML documents.

The main function `html-helper-mode' provides a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, skeletons for new documents, and lots of
other neat features.

\\{html-helper-mode-map}
Written by Nelson Minar."
  (interactive)
  (kill-all-local-variables)

  (use-local-map html-helper-mode-map)
  (setq local-abbrev-table html-helper-mode-abbrev-table)
  (set-syntax-table html-helper-mode-syntax-table)

  (setq mode-name "HTML helper")
  (setq major-mode 'html-helper-mode)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(html-helper-font-lock-keywords t t))

  (setq comment-start "<!-- "
        comment-end " -->"
        comment-start-skip "<!--[ \t]*"
        comment-column 0
        indent-line-function 'html-helper-indent)

  (tempo-use-tag-list 'html-helper-tempo-tags html-helper-completion-finder)

  (if (and html-helper-build-new-buffer (zerop (buffer-size)))
      (html-helper-insert-new-buffer-strings))

  (easy-menu-add (html-helper-menu) html-helper-mode-map)

  (run-hooks 'text-mode-hook)
  (run-hooks 'html-mode-hook)
  (run-hooks 'html-helper-mode-hook))

;; 15 Patterns for font-lock

;; We make an effort on handling nested tags intelligently
(defvar html-helper-bold-face 'bold
  "Face used as bold. Typically `bold'.")
(defvar html-helper-italic-face 'italic
  "Face used as italic. Typically `italic'.")
(defvar html-helper-underline-face 'underline
  "Face used as underline. Typically `underline'.")

(defvar html-helper-font-lock-keywords
  (let (;; Names of tags to boldify
        (bword "\\(b\\|title\\|h[1-4]\\|strong\\)\\([ \t\n]+[^>]+\\)?")
        ;; Names of tags to italify
        (iword "\\(address\\|cite\\|em\\|i\\|var\\)\\([ \t\n]+[^>pa]+\\)?")
        ;; Regexp to match shortest sequence that surely isn't a bold end
        ;; We simplify a bit by extending "</strong>" to "</str.*"
        ;; Do similarly for non-italic and non-title ends
        (not-bend (concat "\\([^<]\\|<\\([^/]\\|/\\([^bhs]\\|"
                          "b[^>]\\|"
                          "title[^>]\\|"
                          "h\\([^1-4]\\|[1-4][^>]\\)\\|"
                          "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
        (not-iend (concat "\\([^<]\\|<\\([^/]\\|/\\([^aceiv]\\|"
                          "a\\([^d]\\|d[^d]\\)\\|"
                          "c\\([^i]\\|i[^t]\\)\\|"
                          "e\\([^m]\\|m[^>]\\)\\|"
                          "i[^>]\\|"
                          "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
        (not-tend (concat "\\([^<]\\|<\\([^/]\\|/\\([^ht]\\|"
                          "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list
     ;; First fontify the text of a HREF anchor, it may be overridden later
     ;; Anchors in headings will be made bold, for instance
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>" 1 font-lock-constant-face t)
     ;; Tag pairs like <b>...</b>
     ;; Cunning repeated fontification to handle common cases of overlap
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>") 3
           'html-helper-bold-face t)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>") 3
           'html-helper-italic-face t)
     ;; Bold simple --- first fontify bold regions with no tags inside
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>") 3
           'html-helper-bold-face t)
     ;; Any tag, general rule, just after bold/italic stuff
     '("\\(<[^>]*>\\)" 1 font-lock-type-face t)
     ;; Underline is rarely used, only handle it when no tags inside
     '("<u>\\([^<]*\\)</u>" 1 html-helper-underline-face t)
     '("</a>" 0 font-lock-keyword-face t)
     '("<a\\b[^>]*>" 0 font-lock-keyword-face t)
     '("=[ \t\n]*\\(\"[^\"]+\"\\)" 1 font-lock-string-face t)
     ;; Large-scale structure keywords
     ;; "<html>" "<body>" "<head>" "<form>"
     '("</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)>" 0
       font-lock-variable-name-face t)
     ;; HTML special characters
     '("&[^;\n]*;" 0 font-lock-string-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside
     '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>" 0 font-lock-comment-face t)
     ;; /* CSS comments */
     '("/\\*.*\\*/" 0 font-lock-comment-face t)
     ;; Comment declarations according to the HTML 2.0 spec at
     ;; https://www.w3.org/MarkUp/html-spec/html-spec_3.html
     ;; Usually <!-- ... -->, but also the single, complete declaration
     ;; <!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >
     '("<!\\(--\\([^-]\\|-[^-]\\)*--\\s-*\\)*>" 0 font-lock-comment-face t)))
  "Additional expressions to highlight in HTML helper mode.")

(provide 'html-helper-mode)
(run-hooks 'html-helper-load-hook)

;;; html-helper-mode.el ends here
