(in-package :static-site)

;; Configuration and bookmarks remain the same
(defparameter *config*
  '(:image-path "static/office.gif"
    :image-name "office.gif"
    :image-alt "Coffee"))

(defparameter *bookmarks*
  '((:category "DEV"
     :links ((:title "gh"
              :url "https://github.com"
              :icon "github")
             (:title "gl"
              :url "https://gitlab.com"
              :icon "gitlab")
             (:title "lbs"
              :url "https://lobste.rs/"
              :icon "rss")
             (:title "hs"
              :url "https://news.ycombinator.com/"
              :icon "rss")
             (:title "cld"
              :url "https://claude.ai"
              :icon "bot")))
    (:category "CPP"
     :links ((:title "ref"
              :url "https://cppreference.com"
              :icon "book-open")
             (:title "quick"
              :url "https://quickref.me/cpp"
              :icon "zap")
             (:title "comp"
              :url "https://godbolt.org"
              :icon "cpu")
             (:title "boost"
              :url "https://www.boost.org/doc/"
              :icon "rocket")))
    (:category "LISP"
     :links ((:title "ql"
              :url "https://www.quicklisp.org/beta/"
              :icon "package")
             (:title "pcl"
              :url "https://gigamonkeys.com/book/"
              :icon "book")
             (:title "hp"
              :url "http://www.lispworks.com/documentation/HyperSpec/Front/"
              :icon "book-open")
             (:title "clhs"
              :url "https://clhs.lisp.se/"
              :icon "library")))
    (:category "NIX"
     :links ((:title "pkgs"
              :url "https://search.nixos.org/packages"
              :icon "snowflake")
             (:title "opt"
              :url "https://nixos.org/manual/nix/stable/"
              :icon "snowflake")
             (:title "hm"
              :url "https://nix-community.github.io/home-manager/options.html"
              :icon "home")))))

;; Updated light coffee theme
(defparameter *theme*
  '(:bg "#F5F1EE"              ; Light cream background
    :fg "#4A3B32"              ; Dark roast text
    :bg-alt "#EBE5E0"          ; Light roast accent
    :fg-alt "#8B7355"          ; Medium roast text
    :bg-active "#E6DCD4"       ; Pressed state
    :border "#D4C8BE"))

(defun generate-css ()
  (cl-css:css
   `((body
      :font-family "JetBrains Mono, monospace"
      :margin 0
      :padding 0
      :background ,(getf *theme* :bg)
      :color ,(getf *theme* :fg)
      :overflow "hidden"
      :height "100vh"
      :display "flex"
      :padding "2rem")
     (".main-container"
      :display "flex"
      :margin "auto"
      :gap "3rem"
      :width "100%"
      :max-width "1200px"
      :align-items "stretch")
     (".image-container"
      :flex "0 0 450px"            ; Fixed width
      :border-radius "1.5rem"      ; Slightly larger radius
      :overflow "hidden"
      :display "flex"
      :padding "1rem"              ; Padding for frame
      :background ,(getf *theme* :bg-alt)
      :box-shadow "0 2px 20px rgba(0,0,0,0.08)")
     (".image-container img"
      :width "100%"
      :height "100%"
      :object-fit "cover"
      :border-radius "0.75rem"     ; Slightly smaller than container
      :flex "1 1 auto")
     (".content-container"
      :flex "1"
      :display "flex"
      :flex-direction "column"
      :height "100%")
     (".search-box"
      :margin-bottom "2rem")
     ("#search-input"
      :width "100%"
      :padding "0.75rem 1rem"
      :font-size "1rem"
      :border ,(format nil "1px solid ~A" (getf *theme* :border))
      :border-radius "0.5rem"
      :background ,(getf *theme* :bg-alt)
      :color ,(getf *theme* :fg)
      :box-sizing "border-box")
     ("#search-input:focus"
      :outline "none"
      :background ,(getf *theme* :bg-active))
     (".categories"
      :display "flex"
      :flex-direction "column"
      :gap "1.5rem")
     (".category"
      :background ,(getf *theme* :bg-alt)
      :border-radius "0.5rem"
      :padding "1.5rem")
     (".category-title"
      :color ,(getf *theme* :fg-alt)
      :margin "0 0 1rem 0"
      :font-size "0.875rem"
      :text-transform "uppercase"
      :letter-spacing "0.05em"
      :font-weight "600")
     (".links"
      :display "flex"
      :flex-direction "row"     ; Horizontal layout
      :flex-wrap "wrap"         ; Allow wrapping
      :gap "0.75rem")          ; Space between links
     (".link"
      :display "inline-flex"
      :align-items "center"
      :gap "0.75rem"
      :padding "0.5rem 0.75rem"
      :text-decoration "none"
      :color ,(getf *theme* :fg)
      :background ,(getf *theme* :bg)
      :border-radius "0.25rem"
      :transition "all 0.2s")
     (".link:hover"
      :background ,(getf *theme* :bg-active))
     (".link i"
      :color ,(getf *theme* :fg-alt)
      :font-size "1.25rem"        ; Slightly larger for Lucide
      :width "1.5rem"             ; Increased width for better spacing
      :height "1.5rem"            ; Match width for perfect squares
      :display "inline-flex"
      :align-items "center"
      :justify-content "center")
     (".link div[class^='icon-']"  ; Target all icon-* classes
      :color ,(getf *theme* :fg-alt)
      :font-size "1.25rem"
      :width "1.5rem"
      :height "1.5rem"
      :display "inline-flex"
      :align-items "center"
      :justify-content "center")
     ;; Basic icon styling
     (".link div[class^='icon-']"
      :font-size "1.25rem"
      :width "1.5rem"
      :height "1.5rem"
      :display "inline-flex"
      :align-items "center"
      :justify-content "center"
      :transition "color 0.2s ease-in-out")

     ;; Default icon color
     (".link div[class^='icon-']"
      :color ,(getf *theme* :fg-alt))

     ;; Category-specific colors
     (".category:nth-of-type(1) .link div[class^='icon-']"  ; DEV
      :color "#4A9EED")

     (".category:nth-of-type(2) .link div[class^='icon-']"  ; CPP
      :color "#00599C")

     (".category:nth-of-type(3) .link div[class^='icon-']"  ; LISP
      :color "#C065DB")

     (".category:nth-of-type(4) .link div[class^='icon-']"  ; GO
      :color "#00ADD8")

     (".category:nth-of-type(5) .link div[class^='icon-']"  ; PY
      :color "#3776AB")

     (".category:nth-of-type(6) .link div[class^='icon-']"  ; NIX
      :color "#5277C3")

     (".category:nth-of-type(7) .link div[class^='icon-']"  ; NEWS
      :color "#FF6600")

     (".category:nth-of-type(8) .link div[class^='icon-']"  ; AI
      :color "#00A67D")

     ;; Hover effect - brighten icons
     (".link:hover div[class^='icon-']"
      :filter "brightness(1.2)"))))

;; HTML generation with a few adjustments for image sizing
(defun generate-html ()
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:meta :charset "UTF-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1.0")
      (:title "Startpage")
      (:link :rel "stylesheet" :href "styles.css")
      (:link :rel "stylesheet"
             :href "https://cdn.jsdelivr.net/npm/lucide-static@0.293.0/font/lucide.min.css")
      (:link :rel "stylesheet"
             :href "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;600&display=swap")
      (:script :src "script.js"))
     (:body
      (:div :class "main-container"
            (:div :class "image-container"
                  (:img :src (getf *config* :image-name)
                        :alt (getf *config* :image-alt)))
            (:div :class "content-container"
                  (:div :class "search-box"
                        (:form :id "search-form"
                               :action "javascript:void(0);"
                               (:input :type "text"
                                       :id "search-input"
                                       :placeholder "Brew up a search...")))
                  (:div :class "categories"
                        (dolist (category *bookmarks*)
                          (:div :class "category"
                                (:h2 :class "category-title"
                                     (getf category :category))
                                (:div :class "links"
                                      (dolist (link (getf category :links))
                                        (:a :class "link"
                                            :href (getf link :url)
                                            :target "_blank"
                                            (:div :class (format nil "icon-~A" (getf link :icon)))
                                            (:span (getf link :title))))))))))))))


(defun generate-js ()
  (ps:ps
    ;; Helper functions
    (ps:defun prevent-browser-default (e)
      (when (ps:@ e cancelable)
        ((ps:@ e prevent-default))
        ((ps:@ e stop-propagation))))

    (ps:defun perform-search (query)
      (when (and query (> (ps:@ query length) 0))
        (let ((url (+ "https://startpage.com/search?q="
                     ((ps:@ window encode-u-r-i-component) query))))
          (setf (ps:@ window location href) url))))

    (ps:defun is-in-search ()
      (equal (ps:@ document active-element id) "search-input"))

    (ps:defun clear-and-blur-search ()
      (let ((search ((ps:@ document get-element-by-id) "search-input")))
        (when search
          (setf (ps:@ search value) "")
          ((ps:@ search blur)))))

    ;; Event handlers
    (ps:defun handle-key (e)
      (let ((is-ctrl (ps:@ e ctrl-key))
            (key (ps:@ e key)))
        (cond
          ;; Ctrl-S to focus search
          ((and is-ctrl (= key "s"))
           (prevent-browser-default e)
           (let ((search ((ps:@ document get-element-by-id) "search-input")))
             ((ps:@ search focus))
             ((ps:@ search select))))

          ;; Ctrl-G to exit search
          ((and is-ctrl (= key "g"))
           (prevent-browser-default e)
           (clear-and-blur-search)))))

    (ps:defun handle-search-submit (e)
      (prevent-browser-default e)
      (let* ((input ((ps:@ document get-element-by-id) "search-input"))
             (query (and input (ps:@ input value))))
        (perform-search query)))

    (ps:defun handle-search-keydown (e)
      (let ((key (ps:@ e key)))
        (cond
          ((= key "Enter")
           (let ((query (ps:@ e target value)))
             (prevent-browser-default e)
             (perform-search query)))

          ;; Allow Ctrl-G to exit search
          ((and (ps:@ e ctrl-key) (= key "g"))
           ((ps:@ e target blur))))))

    ;; Initialization
    (ps:defun init ()
      ;; Add favicon
      (let ((favicon ((ps:@ document create-element) "link")))
        (setf (ps:@ favicon rel) "icon")
        (setf (ps:@ favicon type) "image/x-icon")
        (setf (ps:@ favicon href) "favicon.png")
        ((ps:@ document head append-child) favicon))

      ;; Set up search handlers
      (let* ((form ((ps:@ document get-element-by-id) "search-form"))
             (input ((ps:@ document get-element-by-id) "search-input")))

        (when input
          (setf (ps:@ input onkeydown)
                (lambda (e)
                  (handle-search-keydown e))))

        (when form
          (setf (ps:@ form onsubmit)
                (lambda (e)
                  (handle-search-submit e))))

        ;; Add global key handler for Ctrl-S
        (setf (ps:@ document onkeydown) handle-key)))

    (setf (ps:@ window onload) init)))

(defun generate-site ()
  (ensure-directories-exist "output/")
  ;; Copy image
  (alexandria:copy-file (getf *config* :image-path)
                        (merge-pathnames (getf *config* :image-name)
                                         "output/")
                        :if-to-exists :supersede)
  ;; Copy favicon
  (alexandria:copy-file "static/favicon.png"
                        (merge-pathnames "favicon.png" "output/")
                        :if-to-exists :supersede)
  ;; Generate other files
  (with-open-file (stream "output/index.html"
                          :direction :output
                          :if-exists :supersede)
    (write-string (generate-html) stream))
  (with-open-file (stream "output/styles.css"
                          :direction :output
                          :if-exists :supersede)
    (write-string (generate-css) stream))
  (with-open-file (stream "output/script.js"
                          :direction :output
                          :if-exists :supersede)
    (write-string (generate-js) stream))
  (format t "~&Static site generated in output/~%"))
