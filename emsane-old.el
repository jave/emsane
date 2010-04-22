;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emsane.el -- emacs sane frontend

;;sane is "scanner acces now easy", scanner support available for free
;; operating systems

;; run scanadf(from the sane package) with various presets

;; Author: Joakim Verona, (C) FSF 2009, GPL

;;emsane concepts:
;; preset  - a list of sections
;; section - a list of scanner and postprocessing settings

;;a section definition can contain:
;, - paper size; expressed as an ISO size, or width x height
;; - front or duplex scan
;; - color/lineart
;; - resolution
;; - file name pattern
;; - postprocessing options:
;;   - unpaper options, or dont unpaper
;;   - convert to jpg/djvu etc

;; all the above properties can be defined in a scanjob description
;; as property lists. "ask" can be used to prompt for a value

;; - section list

;; example sections: front-matter, body, cover, etc
;;   - a section has a property list that overrides the overall job property list
;;   - a section has a human readable name and a short form used in the file name
;;  this convention is aimed at keeping image file names and book page numbers in sync

;; - startpage

;; the above value are useful if you want to continue an interrupted job

;;TODO
;;- guard against file overwrites when scanning


;;trivia:

;;I have scanned something like 20K pages with this package. This
;;doesnt mean the package is bug-free or perfect, just that I know the
;;quirks well.  I wrote emsane.el when other solutions didnt work
;;well. in particular i needed to have quick access to a large number
;;of presets for various different document types such as magazines, books etc.

;;emsane.el is complemented by other emacs packages such as dired, and
;;some of my own packages such as dired-sequence.el, and a patch to
;;the emacs core to use imagemagick for image display, so djvu files
;;can be shown in emacs.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;code

;; properties specific for your scanner,
;; a fujitsu fi-5120c in my case

(defcustom emsane-root-directory  "~/my_scans" "where to put scanjobs")

;;TODO i should probably use eieio objects instead since i need to
;;support several scanners and stuff
(require 'eieio)

(defclass emsane-scanner-c (eieio-named)
  ((scanwidth :initarg :scanwidth
    :documentation "physical width of scanner")
   (device :initarg :device
    :documentation "SANE device string of scanner")
   (options :initarg :options
    :documentation "options always used for this device")
  )
  "class describing a SANE scanner")

(setq emsane-scanner-fujitsu (emsane-scanner-c "fujitsu"
                                               :scanwidth 216
                                               :device "fujitsu:fi-5120Cdj:173978"
                                               ;;--df-X options are for double feed detection particular for the fujitsu backend
                                               :options '( "--df-action=Stop" 
                                                           "--df-skew=yes"
                                                           "--df-thickness=yes"
                                                           "--df-length=yes")))
(setq emsane-scanner-test (emsane-scanner-c "test"
                                            :device "test"
                                            :scanwidth 216
                                            :options '("--test-picture"
                                                       "Color pattern")))

(setq emsane-scanner emsane-scanner-fujitsu)
;;TODO deuglify

;;http://en.wikipedia.org/wiki/Paper_size
;;                 ISO paper sizes
(defvar emsane-paper-sizes
  '(("A0"   (841 . 1189))
    ("A1"   (594 . 841))
    ("A2"   (420 . 594))
    ("A3"   (297 . 420))
    ("A4"   (210 . 297))
    ("A5"   (148 . 210))
    ("A6"   (105 . 148))
    ("A7"   (74 . 105))
    ("A8"   (52 . 74))
    ("A9"   (37 . 52))
    ("A10"  (26 . 37))
    ("B0"   (1000 . 1414))
    ("B1"   (707 . 1000))
    ("B2"   (500 . 707))
    ("B3"   (353 . 500))
    ("B4"   (250 . 353))
    ("B5"   (176 . 250))
    ("B6"   (125 . 176))
    ("B7"   (88 . 125))
    ("B8"   (62 . 88))
    ("B9"   (44 . 62))
    ("B10"  (31 . 44))
    ("C0"   (917 . 1297))
    ("C1"   (648 . 917))
    ("C2"   (458 . 648))
    ("C3"   (324 . 458))
    ("C4"   (228 . 324))
    ("C5"   (162 . 229))
    ("C6"   (114 . 162))
    ("C7"   (81 . 114.9))
    ("C8"   (57 . 81))
    ("C9"   (40 . 57))
    ("C10"  (28 . 40))
    ("105x180"  (105 . 180))))


;;TODO eieio instead of this
(defvar emsane-presets
  '(book (book-body book-misc book-cover book-color-inlay)
    topas (topas-body topas-cover) ;;a magazine with a particular layout
    pox (pox-body)
    tm (tm-body)
    epix (epix-body)
    epixhw (epixhw-body)    
    epixa4 (epixa4-body)
    epix287 (epix287-body)            
    a4hiresgray (a4hiresgray)
    a4bwfront (     a4bwfront)
    mb(mb)
    )
    "a preset is a list of sections"
)


;;TODO eieio
(defvar emsane-section-presets
  `( book-body ;;BW djvu for book contents
     (:size (ask ,(mapcar (lambda (x) (car x)) emsane-paper-sizes) t) ;;"ask" is a special value, means read from minibuffer
            :source duplex
            :mode lineart
            :image-type djvu
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")
     book-misc ;;BW djvu for book index etc
     (:size (ask ,(mapcar (lambda (x) (car x)) emsane-paper-sizes) t) ;;"ask" is a special value, means read from minibuffer
            :source duplex
            :mode lineart
            :image-type djvu
            :resolution 300
            :file-pattern "00-%04d"
            :unpaper "")     
     book-cover;; color for the cover, only scan front
     (:size (ask ,(mapcar (lambda (x) (car x)) emsane-paper-sizes) t)
            :source front
            :mode color
            :image-type djvu-color
            :resolution 300
            :file-pattern "cover-%04d"
            :unpaper "")
     book-color-inlay;; color for inlay, duplex
     (:size (ask ,(mapcar (lambda (x) (car x)) emsane-paper-sizes) t)
            :source duplex
            :mode color
            :image-type djvu-color
            :resolution 300
            :file-pattern "inlay-%04d"
            :unpaper "")

     topas-body ;;grayscale jpeg
     (:size "200 x 295" 
            :source duplex
            :mode gray
            :image-type jpg-gray
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")
     topas-cover ;;color jpeg
     (:size "200 x 295" 
            :source front
            :mode color
            :image-type jpg-color
            :resolution 300
            :file-pattern "cover-%04d"
            :unpaper "")

     pox-body ;; jpeg color
     (:size "205 x 275" 
            :source duplex
            :mode color
            :image-type jpg-color-hw
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")

     tm-body ;; jpeg color
     (:size "200 x 275" 
            :source duplex
            :mode color
            :image-type jpg-color-hw
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")


     epix-body ;; jpeg color
     (:size "205 x 275" 
            :source duplex
            :mode color
            :image-type jpg-color
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")

     epixhw-body ;; jpeg color by hw in scanner
     (:size "210 x 295" 
            :source duplex
            :mode color
            :image-type jpg-color-hw
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")

     epix287-body ;; jpeg color
     (
      :size "210 x 287"
            :source duplex
            :mode color
            :image-type jpg-color-hw
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")
     
     
     a4hiresgray ;; jpeg gray
     (:size "A4" 
            :source front
            :mode gray
            :image-type jpg-gray
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")     

     a4bwfront ;; a4 bw bunch only front to djvu
     (:size "A4" 
            :source front
            :mode lineart
            :image-type djvu
            :resolution 300
            :file-pattern "01-%04d"
            :unpaper "")
     mb ;;for månblad 
     (:size "A4"
            :source duplex
            :mode gray
            :image-type pnm
            :resolution 300
;;            :file-pattern "%04d"
            :file-pattern "01-%04d" ;;TODO these cant be freely named due to a regexp in the line matcher
            :unpaper emsane-unpaper-args-mb)

     )
  
    "a section is a list of properties used when scanning")

(defun emsane-unpaper-args-mb (filename)
  "a special unpaper argument creator for when scanning booklets made of A4 sheets
stapled together and folded. (when scanning remove staples and unfold of course)
"
  (let*
      ((evenpage (evenp emsane-next-pagenumber))
       (angle (if evenpage;;we are postprocessing so pagenumber is current
                  "90" "-90"))
       (p1 (if evenpage "a" "b") ) ;; we need to swap out page num sequence for odd/even pages
       (p2 (if evenpage "b" "a") )
       (cmd (list  "--overwrite"
;;                   "--size" "a4"
                   "--layout" "double"
                   "--output-pages" "2"
                   "--pre-rotate"  angle
                   "--output-file-sequence" (concat filename "-" p1 ".pnm") (concat  filename "-" p2 ".pnm")
                   "--input-file-sequence" filename )))
    (message (format "unpaper args:%s" cmd))
    cmd))

(defun emsane-get-preset (section property)
  "get preset PROPERTY for SECTION."
  (plist-get  (plist-get  emsane-section-presets  section)  property))

(defun emsane-get-value (section property)
    "get preset PROPERTY for SECTION, or ask if undefined."
    (let ((value (emsane-get-preset section property))
          (choices nil)
          (recall nil))
      (cond
       ((or (null value) (and (listp value)(eq (car value) 'ask)))
        (setq recall (caddr value));;if t recall old value
        (setq choices (cadr value)) ;;TODO this enables completion, which is nice, for paper sizes we want both constans and free sizes though
        (cond
         ((and recall (emsane-get-last-value property))
          (setq value (emsane-get-last-value property)))
         (t
          (setq value (completing-read (format "enter %s: " property choices) choices))
          (emsane-put-last-value property value)))))
    value))

(defun emsane-put-last-value (property value)
  "Store PROPERTY, VALUE in emsane-current-recall.
This plist is used to remember values that have been entered
earlier using the 'ask' feature."
  (setq emsane-current-recall (plist-put emsane-current-recall property value))
  )

(defun emsane-get-last-value (property)
  "Get PROPERTY in emsane-current-recall."  
  (plist-get emsane-current-recall property)
  )

      
(defun emsane-get-paper-size (size-string)
  "Return a size cons from SIZE-STRING.
SIZE-STRING is either an ISO paper size \"A4\" or a string like \"210 x 297\" (A4 in mm)."
  (let ((size (cadr (assoc (upcase size-string) emsane-paper-sizes))))
    (if size
        size
      (if (string-match "\\([0-9]*\\)[ ]*x[ ]*\\([0-9]*\\)" size-string)
          (cons (string-to-number (match-string 1 size-string))
                (string-to-number (match-string 2 size-string))))
    )))


(defun emsane-prepare-buffer ()
  "prepare the *scanadf* scan job buffer"
    (let*
      ((buffer (get-buffer-create "*scanadf*"))) ;;TODO support multiple scan job buffers
    (pop-to-buffer buffer)
    (erase-buffer)
    (emsane-mode)
    buffer
    ))

(defun emsane-start (buffer preset scanjob-id)
  "Start up a new scanjob given PRESET and SCANJOB-ID.
Prompt the user to feed the scanner."
  (interactive
   (list
    (emsane-prepare-buffer)
    (read (completing-read "preset:"
                           (mapcar (lambda (x) (format "%s" x))
                                   emsane-presets )
                           nil
                           t
                           (format "%s" emsane-current-preset)))
    ;;TODO im confused about atoms and strings obviously...
    (read-string "scanjob id:"  emsane-current-scanjob-id)
    ;;TODO complete id:s from contents of emsane-root-directory
   ))
  (unless buffer   (emsane-prepare-buffer))
  (setq emsane-current-scanjob-id scanjob-id)
  (setq emsane-current-preset preset)
  (setq emsane-current-section (car (plist-get emsane-presets preset)))
  (emsane-set-mode-line)
  )


(defun emsane-get-next-section()
  "fetch the next section from the preset"
  (let
      ((mylist (mapcar (lambda (x) (format "%s" x))
                       (plist-get emsane-presets emsane-current-preset))))
    (while (not (equal (format "%s" emsane-current-section) (car mylist) ))
      (setq mylist (cdr mylist)))
    (format "%s" (cadr mylist)))
  )

(defun emsane-set-section ()
  "Jump to next section in scan preset."
  (interactive)
  (let* ((sections (mapcar (lambda (x) (format "%s" x)) ;;TODO this atom list -> string list transform doesnt seem right
                          (plist-get emsane-presets emsane-current-preset)))
         (next-section (emsane-get-next-section))

         )

    ;;TODO if next-section is nil, were at the end and should do something useful
    ;;start a new scan with same preset but with new job-id?
        (setq emsane-current-section
              (read (completing-read (format "section[%s]: " next-section)
                                     sections
                                     nil nil nil nil
                                     next-section)))

      
      (emsane-setpage 1)
      (emsane-set-mode-line)
  ))

;;mode-line-format
;;force-mode-line-update
;;mode-line-buffer-identification

(defun emsane-set-mode-line ()
  (setq mode-line-buffer-identification
	(nconc (propertized-buffer-identification "%b")
               (list 
                (format " %s [%s %s] %s"
                    emsane-current-scanjob-id
                    emsane-current-preset
                    emsane-current-section
                    emsane-next-pagenumber)))))


(defun emsane-section (&optional section  startcount-info  scanjob-id)
  "Start a scan using the pretest SECTION with SCANJOB-ID.
STARTCOUNT-INFO sets the start page  num.
If SCANJOB-ID is nil, use the default value in the buffer.
"
  (interactive)
    (insert (format "id:%s preset:%s section:%s recall:\n"
                    emsane-current-scanjob-id
                    emsane-current-preset
                    emsane-current-section
                    emsane-current-recall))
  (unless section
    (setq section emsane-current-section))
  (unless scanjob-id
    (setq scanjob-id emsane-current-scanjob-id))
  
  (let ((size (emsane-get-paper-size (emsane-get-value section :size)))
        (startcount (cond
                     ((null startcount-info) emsane-next-pagenumber)
                     ((>= 1 startcount-info) startcount-info))))
    (emsane-scanadf emsane-scanner
                          scanjob-id
                          (emsane-get-value section :source)
                          (emsane-get-value section :mode)
                          (emsane-get-value section :resolution)
                          (car size)
                          (cdr size)
                          (emsane-get-value section :file-pattern)
                          startcount
                          )))


(defun emsane-get-scandir (scanjob-id)
  "return dir used to store scans for SCANJOB-ID."
  (let 
      ((dir (concat (expand-file-name emsane-root-directory) "/" scanjob-id "/")))
    ;;mkdir dir if not exists TODO not here, there should be a prepare-scanjob defun
    ;;TODO presets should be able to override scandir(so magazines can wind up in their own dir for example)
    (mkdir dir t)
    dir))


(defun emsane-get-scanjob-dir ()
  (emsane-get-scandir emsane-current-scanjob-id)
  )


(defun emsane-scanadf (scanner
                       scanjob-id 
                       source mode resolution
                       paperwidth paperheight
                       filepattern startcount )
  "Calls scanadf with a number of parameters."
  (let*
      
      ((scanwidth (oref scanner scanwidth))
       (topleft (- (/ scanwidth 2) (/ paperwidth 2))) ;;TODO this is for croping from center, which maybe not all adf scanners need
       (imgtype (emsane-get-value emsane-current-section :image-type))

       ;; only if hw supports it and only if jpg-color-hw option
       ;;normaly sane 1.0.20 must be recompiled to get get this
       (compression-options (if (equal imgtype 'jpg-color-hw)
                                (list "--compression" "JPEG")))

       (page-height (if (not (equal (oref scanner :object-name) "test"))
                         (list "--page-height" (number-to-string paperheight))));;TODO again scanner specific, in this case a lambda definition seems needed
       (args `("scanadf" "*scanadf*"
         "scanadf"
         "--device-name" ,(oref scanner :device)
         "--source"
         ,(if (equal (oref scanner :object-name) "test") "Automatic Document Feeder"
             (if (eq source 'duplex)
                 "ADF Duplex"
               "ADF Front") ;;TODO refactor ;;TODO we need aliases not renames(duplex should be an alias of ADF Duplex, for a particular scanner)
           )
         "--mode" ,(symbol-name  mode);;TODO Color or Gray and depth options for test
         ,@compression-options
         ,@(oref scanner :options)
         "--resolution"  ,(number-to-string resolution)
         "--output-file" ,(concat (emsane-get-scanjob-dir)
                                 filepattern ".scan") ;;TODO refactor
         "--start-count" ,(number-to-string startcount) 
         
         "-l" ,(number-to-string topleft)
         "-t" ,(number-to-string 0)
         "-x" ,(number-to-string paperwidth)

         ;;TODO figure out how to handleheight better.
         ;; y is height of scan-area
         ;; page-height is height of scanner physical scan area, but this is less than max by default.
         ;;y <= paperheight
         "-y" ,(number-to-string paperheight)
         ,@page-height
         ))
       (scan-process
        (eval `(start-process
         ,@args
         ))))
    (set-process-sentinel scan-process 'emsane-sentinel)
    (set-process-filter scan-process 'emsane-filter)
    ))

(defvar emsane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 	  'emsane-section)
    (define-key map "n" 	  'emsane-set-section)
    (define-key map "p" 	  'emsane-setpage)
    (define-key map "d" 	  'emsane-dired)
    (define-key map "s" 	  'emsane-start)
    ;;(define-key map "a" 	  'emsane-again)
    map)
    "keymap for emsane-mode.")


(define-derived-mode emsane-mode fundamental-mode
  "emsane-mode"
  "scanner frontend mode"
  (set (make-local-variable 'emsane-current-preset) nil)  
  (set (make-local-variable 'emsane-current-section) nil)
  (set (make-local-variable 'emsane-next-pagenumber) 1)  
  (set (make-local-variable 'emsane-current-scanjob-id) nil)
  (set (make-local-variable 'emsane-current-recall) ())  
  (emsane-set-mode-line)
  )


(defun emsane-dired ()
  "Dired the current scan project."
  (interactive)
  (dired (emsane-get-scanjob-dir))
  )


(defun emsane-setpage (pagenum) ;;TODO there are other settings than pagenum that might need to be overriden, refactor(size)
  "Set next scan to begin with PAGENUM in the filename."
  (interactive (list (read-number "Pagenumber of next scan: "  emsane-next-pagenumber)))
  (setq emsane-next-pagenumber pagenum)
  (emsane-set-mode-line)
  );;TODO is this the right way to set a buffer local variable? 


(defun emsane-sentinel (scan-process msg)
  "Called when scanadf is finished."
  (message (format "scan finished with code:%s in buffer %s" msg (process-buffer scan-process))))


;;TODO react on these type of strings:
;; Scanned document 00-0001 ;;start post-processing
;; scanadf: sane_start: Document feeder jammed
;; Document feeder jammed
;; scan finished?
(defun emsane-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc)))
          )
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        ;;break up string in lines and handle each
        (mapcar 'emsane-line-handler (split-string string "\n" t))
        (insert (format "filter:<<%s>> <<%s>>\n" ;;TODO should be possible to visit image files in the scanadf buffer!
                        (substring string 0 -1)
                        emsane-next-pagenumber))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun emsane-line-handler(string)
  (let
      ((filename nil))
    (cond
     ((string-match "Scanned document \\(.*\\)" string)
      (setq filename (match-string 1 string))

      (emsane-postprocess filename)

      (string-match "\\([09a-zA-Z]*\\)-\\([0-9]*\\)\.scan" filename) ;;TODO this must match the page naming expression of the current section
      (setq emsane-next-pagenumber
            (+ 1 (string-to-number (match-string 2 filename))))
      
      (emsane-set-mode-line)
      ))))

(defun emsane-postprocess2(command filename &optional delete-scan)
  (let*
      ((filenamebase (substring filename 0 -5));;TODO this removes ".scan" but needs to be more robust
       (postprocess-command      (if delete-scan
         (format (concat command "&& rm %s") filename filenamebase filename);;TODO  handle rm at lisp level instead
         (format command  filename filenamebase )       
       )))
    (insert postprocess-command);;DEBUG
    (call-process-shell-command
     postprocess-command
     nil 0)))

(defun emsane-postprocess(filename)
  "Postprocess file FILENAME sccording to current section settings"
  (let*
      ((imgtype (emsane-get-value emsane-current-section :image-type))
       (unpaper (emsane-get-value emsane-current-section :unpaper)));;TODO
    (insert (format "postprocessing:%s to %s\n" filename imgtype))
    (cond
     ((eq imgtype 'djvu)
      (emsane-postprocess2
       "cjb2 -lossy %s %s.djvu" filename  t))
     ((eq imgtype 'djvu-color)
      (emsane-postprocess2
       "c44 %s %s.djvu" filename t))
     ((eq imgtype 'jpg-gray)
      (emsane-postprocess2
       "convert -type Grayscale %s %s.jpg"  filename t))
     ((eq imgtype 'jpg-color)
      (emsane-postprocess2
       "convert %s %s.jpg"  filename  t))
     ((eq imgtype 'jpg-color-hw)
      (emsane-postprocess2
       "mv %s %s.jpg"  filename  nil))
     ((eq imgtype 'pnm)
      (emsane-postprocess2
       ""  filename  nil))     ;;leave it as .scan TODO
     
     (t
      (emsane-postprocess2
       (concat "convert %s %s." imgtype)  filename )))
    (if (functionp unpaper)
      (eval `(call-process "unpaper" nil 0 nil ,@(funcall unpaper filename)));;TODO since unpaper is slow, this job should be queued somehow
      )
    )
  
  )


;;
(defun emsane-start-test ()
  (interactive)
  (let
      ()
    (setq emsane-scanner "test");;a test device in sane
    (emsane-start nil 'topas "scan-test")
  ))
;(setq emsane-scanner "sane-test");;for testing withouth an actual scanner
(provide 'emsane)