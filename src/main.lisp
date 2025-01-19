(in-package :static-site)

;; Configuration and bookmarks remain the same
(defparameter *config*
  '(:image-path "static/office.gif"
    :image-name "office.gif"
    :image-alt "Coffee"))

(defparameter *bookmarks*
  '((:category "DEV"
     :links ((:title "/gh/"
              :url "https://github.com"
              :icon "fab fa-github")
             (:title "/gl/"
              :url "https://gitlab.com"
              :icon "fab fa-gitlab")
             (:title "/bb/"
              :url "https://bitbucket.org"
              :icon "fas fa-book")
             (:title "/cpp/"
              :url "https://cppreference.com"
              :icon "fas fa-code")))
    (:category "NIX"
     :links ((:title "/pkgs/"
              :url "https://search.nixos.org/packages"
              :icon "fas fa-snowflake")
             (:title "/opt/"
              :url "https://nixos.org/manual/nix/stable/"
              :icon "fas fa-snowflake")))
    (:category "HM"
     :links ((:title "/opt/"
              :url "https://nix-community.github.io/home-manager/options.html"
              :icon "fas fa-home")))))

;; Updated light coffee theme
(defparameter *theme*
  '(:bg "#F5F1EE"              ; Light cream background
    :fg "#4A3B32"              ; Dark roast text
    :bg-alt "#EBE5E0"          ; Light roast accent
    :fg-alt "#8B7355"          ; Medium roast text
    :bg-active "#E6DCD4"       ; Pressed state
    :border "#D4C8BE"))        ; Subtle borders

(defun generate-css ()
  (cl-css:css
   `((body
      :font-family "JetBrains Mono, monospace"
      :margin 0
      :padding 0
      :background ,(getf *theme* :bg)
      :color ,(getf *theme* :fg)
      :min-height "100vh"
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
      :font-size "1rem"          ; Slightly smaller for FA icons
      :width "1.2rem"            ; Fixed width for alignment
      :text-align "center"))))

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
             :href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css")
      (:link :rel "stylesheet"
             :href "https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css")
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
                        (:input :type "text"
                               :id "search-input"
                                :placeholder "Brew up a search..."))
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
                                            (:i :class (getf link :icon))  ; Use the full class directly
                                            (:span (getf link :title))))))))))))))

(defun generate-js ()
  (ps:ps
    (ps:defun init ()
      (let ((input (ps:chain document (get-element-by-id "search-input"))))
        (when input
          ;; Handle "/" shortcut
          (setf (ps:@ document 'onkeydown)
                (lambda (e)
                  (when (and (= (ps:@ e 'key) "/")
                            (not (equal (ps:@ document 'active-element) input)))
                    (ps:chain e (prevent-default))
                    (ps:chain input (focus)))))

          ;; Handle search input events
          (setf (ps:@ input 'onkeydown)
                (lambda (e)
                  (cond
                    ((= (ps:@ e 'key) "Escape")
                     (ps:chain input (blur)))
                    ((= (ps:@ e 'key) "Enter")
                     (let ((query (ps:@ input 'value)))
                       (when (> (ps:@ query 'length) 0)
                         (ps:chain e (prevent-default))
                         (setf (ps:@ window 'location 'href)
                               (+ "https://startpage.com/search?q="
                                  (encode-u-r-i query))))))))))))

    (setf (ps:@ window 'onload) init)))

(defun generate-site ()
  (ensure-directories-exist "output/")
  (alexandria:copy-file (getf *config* :image-path)
                       (merge-pathnames (getf *config* :image-name)
                                      "output/")
                       :if-to-exists :supersede)
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
