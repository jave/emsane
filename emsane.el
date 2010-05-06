;; emsane.el -- Emacs SANE frontend

;;; Commentary:
;;

;; Emsane is an Emancs frontend for SANE.  Emsane runs scanadf with
;; various jobs, to make it easy to scan books, magazines etc.

;; SANE is an acronym for "scanner acces now easy", scanner support
;; available for free operating systems, see
;; http://www.sane-project.org/

;; Author: Joakim Verona, (C) FSF 2009, 2010, GPL

;; Some interesting Emsane features:
;; - convenient Emacs interface to scanning
;; - handles one or several scanners
;; - use different scanner settings for different sections of a book
;; - the power of tramp lets you run scanners remotely over ssh
;; - dired your scans
;; - postprocess scans:
;;   - format conversions, to djvu for instance
;;   - unpaper
;; - multi-scanner mode, scan different sections of a book with different scanners to speed up scanning

;; Quick usage:
;; - Define scanners and jobs in a file, "emsane-config.el"
;; for instance. See sample.
;; - load "emsane" and "emsane-config"
;; - start scanning with "emsane-scan-start
;; - check out the keybindings, but mostly you use "return" and "n" while scanning.

;; Some Emsane concepts:
;; scanner - a set of options describing a scanner
;; section - a list of scanner and postprocessing settings
;; job  - a list of sections

;; A section definition can contain:
;; - paper size; expressed as an alias(ISO sizes or common book sizes), or width x height
;; - front or duplex scan
;; - color/lineart
;; - resolution
;; - file name pattern
;; - postprocessing options

;; emsane.el is complemented by other Emacs packages such as dired,
;; and some of my own packages such as dired-sequence.el, and a patch
;; to the Emacs core to optionaly use imagemagick for image display,
;; so djvu files can be shown in Emacs.


;;TODO terminology cleanup/code cleanup
;; - find better wording for:
;;   - "section" and "jobs" or at least be consistent
;;   - multi-scan
;; - use "name" consistently for references, use object references rather than objects in some places
;;   - scanjob-id -> scanjob-name ??
;;   - some defs take scanner as arg, should be scanner-name?

;;; Code:

(require 'eieio)
(require 'emsane-postop)
(require 'emsane-query)

(defcustom emsane-root-directory
  "~/my_scans"
  "Where to put jobs."
  :group 'emsane)


(defcustom emsane-notify-command
  "aplay /usr/share/sounds/linphone/rings/bigben.wav"
  "Command to execute when scanner needs attention."
  :group 'emsane)

(defvar emsane-paper-sizes
  "A list of paper sizes. Its also possible to add local size aliases to a section job,
 for instance for magazines which come in different sizes." )

(defvar emsane-iso-paper-sizes
  '(("a0"   (841 . 1189))
    ("a1"   (594 . 841))
    ("a2"   (420 . 594))
    ("a3"   (297 . 420))
    ("a4"   (210 . 297))
    ("a5"   (148 . 210))
    ("a6"   (105 . 148))
    ("a7"   (74 . 105))
    ("a8"   (52 . 74))
    ("a9"   (37 . 52))
    ("a10"  (26 . 37))
    ("b0"   (1000 . 1414))
    ("b1"   (707 . 1000))
    ("b2"   (500 . 707))
    ("b3"   (353 . 500))
    ("b4"   (250 . 353))
    ("b5"   (176 . 250))
    ("b6"   (125 . 176))
    ("b7"   (88 . 125))
    ("b8"   (62 . 88))
    ("b9"   (44 . 62))
    ("b10"  (31 . 44))
    ("c0"   (917 . 1297))
    ("c1"   (648 . 917))
    ("c2"   (458 . 648))
    ("c3"   (324 . 458))
    ("c4"   (228 . 324))
    ("c5"   (162 . 229))
    ("c6"   (114 . 162))
    ("c7"   (81 . 114.9))
    ("c8"   (57 . 81))
    ("c9"   (40 . 57))
    ("c10"  (28 . 40)))
  "ISO paper sizes from http://en.wikipedia.org/wiki/Paper_size.")

(defvar emsane-book-paper-sizes
  '(("Folio"         ( 382 . 305 ))
    ("Quarto"        ( 305 . 241.5 ))
    ("Octavo"        ( 228 . 152.5 ))
    ("Duodecimo"     ( 187 . 127))
    ("Twelvemo"      ( 187 . 127))
    ("Sextodecimo"   ( 171.5 . 101.5 ))
    ("Sixteenmo"     ( 171.5 . 101.5 ))
    ("Octodecimo"    ( 165 . 101.5))
    ("Eighteenmo"    ( 165 . 101.5))
    ("Trigesimo-secundo"( 140 . 90))
    ("Thirty-twomo"  ( 140 . 90))
    ("Sexagesimo-quarto"( 76 . 50))
    ("Sixty-fourmo"  ( 76 . 50)))
  "Book sizes http://en.wikipedia.org/wiki/Book_size")


(setq emsane-paper-sizes (append emsane-iso-paper-sizes emsane-book-paper-sizes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro to define named and tracked classes



(defmacro emsane-declare-instance-get (mname &optional fn-doc list-doc)
  "declares getter and instance lists"
  `(progn
     (defvar ,(intern (concat "emsane-" (symbol-name mname) "-list"))
       nil
       ,(concat "Tracker symbol for instances of " (symbol-name mname) "."))
     (defvar ,(intern (concat "emsane-" (symbol-name mname) "-history"))
       nil
       ,(concat "History symbol for instances of " (symbol-name mname) "."))
     (defun ,(intern (concat "emsane-" (symbol-name mname) "-get")) (name)
       ,(concat "Get " (symbol-name mname) " NAME from its tracker.")
       (let* ((instance-get-rv (emsane-instance-tracker-find name 'object-name
                                                             ',(intern (concat "emsane-" (symbol-name mname) "-list")))))
         (assert (not (null instance-get-rv)) nil  "object with key %s not found" name)
         instance-get-rv)
       )))



(emsane-declare-instance-get section);;inherits section interface
(emsane-declare-instance-get job);;TODO should inherit section interface?
(emsane-declare-instance-get query)
(emsane-declare-instance-get scanner)

(defun emsane-instance-tracker-find (key slot list-symbol)
  (eieio-instance-tracker-find key slot list-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eieio class definitions

;;TODO emsane-tracker isnt a stellar name, since we are named, tracked, and instance-inherited together
(defclass emsane-tracker (eieio-named
                          eieio-instance-tracker
                          ;;eieio-instance-inheritor;;TODO dont think i need this anymore
                          )
  (())
  "emsane-tracker works similar to eieio-instance-tracker, but it is also named, and the name works like a primary key.
there can only be one emsane-tracker object with a particular name.")

(defmethod initialize-instance ((this emsane-tracker)
                                &rest slots)
  "make sure only 1 object with a particular name on the tracker list."
  (let*
      ((sym (oref this tracking-symbol))
       (already-existing (object-assoc (oref this :object-name) :object-name (symbol-value sym))))
    (if already-existing (delete-instance already-existing))
    (call-next-method)
    ))

(defmethod clone ((obj emsane-tracker) &rest params)
  "enable instance tracking also for clones."
  ;;we want clone for eieio-instance-inheritor to execute
  ;;also clone doesnt do object init, so so it explicitly
  (assert (stringp (car params)) nil "1st arg to clone must be a new name!") 
  (let ((theclone (call-next-method))) 
    (initialize-instance theclone)
    theclone))


(defclass emsane-scanner (emsane-tracker)
  ((tracking-symbol :initform 'emsane-scanner-list)
   (scanwidth :initarg :scanwidth
              :documentation "physical width of scanner")
   (device :initarg :device
           :documentation "SANE device string of scanner")
   (options :initarg :options
            ;;:accessor emsane-get-options
            :documentation "options always used for this device")
   (image-type-options :initarg :image-type-options :initform nil
                       :documentation "an assoc list of options for a particular image type"
                       )
   (sources :initarg :sources
            :documentation "scanner sources (duplex,simplex, etc)")
   (modes :initarg :modes
          :documentation "scanner modes")
   ;;a couple of scanner quirk flags
   (topleft-adf :initarg :topleft-adf
                :initform t
                :documentation "t if scanner has centered adf, affects topleft calculation")
   (needs-pageheight :initarg :needs-pageheight
                     :initform nil
                     :documentation "t if scanner needs --page-height flag")
   (inhibit-adf :initarg :inhibit-adf
                :initform nil
                :documentation "t if scanner is a flatbed which scans forever withouth this flag")
   )
  "class describing a SANE scanner")


(defclass emsane-job (emsane-tracker   ;; store instantiated objects in a list
                      )
  ((tracking-symbol :initform 'emsane-job-list)
   (section-list :initarg :section-list
                 :accessor emsane-job-get-section-list
                 :documentation "get the list of section jobs")
   (job-id-template :initarg :job-id-template
                    :initform nil
                    :documentation "template when creating job id")
   ))



(defmethod emsane-read-job-id ((this emsane-job))
  "Prompt for a job id."
  (let
      ((template (oref this job-id-template))
       ;;(default-id (if (boundp 'emsane-current-job-id) emsane-current-job-id ""))
       )
    (if (null template)
        (read-string "job id:"
                     ;;TODO
                     ;;- complete id:s from contents of emsane-root-directory
                     ;; this should probably not go here, rather in a "emsane-scan-continue" function or something
                     ;;- [ and ] for example, are not allowed in job ids, guard against.
                     ;;- convert " " to "_" in jobids for convenience
                     ;;- GUID jobid sugestion when you dont have time to figure one out,
                     ;;also combine with some general xattr tagging interface?
                     nil
                     'emsane-job-id-history
                     ;;default-id
                     )
      (eval `(format (car template) ,@(emsane-read-values template))))))

;;TODO
;;"size" is the currently the most convoluted of the attributes, but the other ones might also come to use the same machinery
;;i would like to expand the current scheme to a chain. "size" (as an example) is queried for each "section" object in the chain
;; a section would realy only be an interface for a  bunch of settings. the chain would handle several buffers at once, with local overrides.
;;these are the cases:
;;- multi-scan buffers, several scan buffers with the same settings, a scan buffer might have local overrides
;;- several independent scan buffers
;;- conveniently allow recall of last entered local  value, but allow to re-read the size setting if needed

;;list of slot that needs this handling:
;;- size. a book can have a cover that is of a different size to the body, for hardbacks
;;- start-page. in particular i want a way to force asking start-page for particular scan buffers in multi-scan mode

;;slots that might need this:   
;;- resolution. some pages might need higher resolution
;;- mode. maybe only a couple of pages are color
;;- image-type. maybe a mostly djvu document sometimes needs some other type
;;- source. Might be convenient to override as duplex sometimes, in a mostly simplex document


;;maybe not. these slots might be entirely different cases
;;- scanner, file-pattern, operation-list

(defclass emsane-section-interface ()
  ((size :initarg :size
         ;;TODO :initform is autoquoted! the lambda is used as a workaround, but this syntax is going away in eieio
         ;;TODO this is supposed to be an interface so wtf do i have a lot of initforms here at all?
         ;;         :initform (lambda () (emsane-query-paper-size "paper-size" :prompt "Paper size" :values emsane-paper-sizes))
         :documentation "paper size")
   (start-page :initarg :start-page
               ;;               :initform (lambda ()    (emsane-query-integer "startpage" :prompt "Start-page"))
               :documentation "section start page")
   
   (scanner :initarg :scanner
            ;;            :initform (lambda () (emsane-query-object "scanners" :prompt "Scanner" :object-type 'scanner ))
            :documentation "scanner name")   
   (source :initarg :source
           ;;           :initform (lambda () (emsane-query-atom "sources" :prompt "Source"  :values '(duplex simplex )))
           :accessor emsane-section-get-source
           :documentation "scanner source(duplex,simplex, etc)")   
   (mode :initarg :mode
         ;;         :initform (lambda () (emsane-query-atom "modes" :prompt "Mode" :values '(lineart color )))
         :accessor emsane-section-get-mode
         :documentation "scanner mode(lineart,color etc)")
   (resolution :initarg :resolution
               ;;               :initform (lambda () (emsane-query-integer "resolution" :prompt "Resolution"))
               :accessor emsane-section-get-resolution
               :documentation "scan resolution in dpi")
   (file-pattern :initarg :file-pattern
                 ;;                 :initform (lambda () (emsane-query-string  "file-pattern" :prompt "File pattern" :require-match nil))
                 :accessor emsane-section-get-file-pattern
                 :documentation "how to name the files")
   (image-type :initarg :image-type
               ;;               :initform (lambda () (emsane-query-atom "image-types"  :prompt "Image type"  :values '(djvu jpeg )))
               :accessor emsane-section-get-image-type
               :documentation "image type(djvu,jpeg)")
   (operation-list :initarg :operation-list :initform nil
                   :documentation "A list of operations to be used for every image scanned in this section")   
   )
  :abstract t )



(defclass emsane-section (emsane-tracker   ;; store instantiated objects in a list(needs special clone override)
                          emsane-section-interface
                          )
  ((tracking-symbol :initform 'emsane-section-list)
   (parent :initarg :parent :initform emsane-the-section-defaults
           :initform nil
           :documentation "parent object. this instances slots overrides the parent slots.")
   )
  "class describing a section")

;;TODO parent got confused it seems

;;;value class
(defclass emsane-section-value (emsane-section-interface)
  ((parent :initarg :parent
           :initform nil
           :documentation "parent object. this instances slots overrides the parent slots."))
  )
(defvar emsane-the-section-defaults
  (emsane-section-value "the-section-defaults"
                        :size (emsane-query-paper-size "paper-size" :prompt "Paper size" :values emsane-paper-sizes)
                        :start-page   (emsane-query-integer "startpage" :prompt "Start-page")
                        :scanner (emsane-query-object "scanners" :prompt "Scanner" :object-type 'scanner )
                        :source  (emsane-query-atom "sources" :prompt "Source"  :values '(duplex simplex ))
                        :mode    (emsane-query-atom "modes" :prompt "Mode" :values '(lineart color ))
                        :resolution   (emsane-query-integer "resolution" :prompt "Resolution")
                        :file-pattern (emsane-query-string  "file-pattern" :prompt "File pattern" :require-match nil)
                        :image-type   (emsane-query-atom "image-types"  :prompt "Image type"  :values '(djvu jpeg ))
                        :parent nil
                        )
  "default values for section slots")
(defclass emsane-process-state ()
  (;;per job
   (postop-queue :initarg :postop-queue)
   (job :initarg :job)
   (job-id :initarg :job-id)
   ;;per buffer
   (section :initarg :section)
   (section-overide :initarg :section-overide :initform nil)   
   (subsection :initarg :subsection)
   (next-pagenumber :initarg :next-pagenumber)
   )
  )

;;TODO stopgap global recall object again...
;; global state i dont want
(defvar emsane-query-recall nil)
(defun emsane-query-recall-reset ()
  (setq emsane-query-recall nil))

;;;;;;;;;;;;;;;;;;;;;;;
(defmethod emsane-handle-slot ((this emsane-section-interface) slot)
  (if (slot-boundp this slot)
      (let*
          ((sv (slot-value this slot))
           (query-result nil))
        
        (if (and (object-p sv) (object-of-class-p sv emsane-query))
            ;;if this exact query already was answered, return the previous value
            (if (assq sv emsane-query-recall)
                (cdr (assq sv emsane-query-recall))
              ;;otherwise ask the question and store it
              (progn
                (setq query-result (emsane-do-query sv))
                (setq emsane-query-recall (append (list (cons sv query-result)) emsane-query-recall))
                query-result
                )
              )
          sv))
    (progn
      (emsane-handle-slot (cond
                           ((object-p (oref this :parent)) (oref this :parent))
                           ((symbolp (oref this :parent)) (eval (oref this :parent)))
                           (t (error "needed parent, but wasnt object or symbol"))
                           )
                          
                          slot));;ask the parent for the value


    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;; getters for section-interface slots
(defmethod emsane-get-size ((this emsane-section-interface)  &optional sizes)
  "accessor for a sections size slot, supports prompting and recall"
  ;;each slot supporting prompting and recall should look somewhat like this
  (unless sizes (setq sizes emsane-paper-sizes))
  (emsane-parse-paper-size (emsane-handle-slot this 'size) sizes))

(defmethod emsane-get-start-page ((this emsane-section-interface))
  "accessor for a sections start-page slot, supports prompting and recall"
  (let*
      ((startnum (emsane-handle-slot this 'start-page)))
    (cond
     ((eq 'continue startnum)  emsane-next-pagenumber)
     (t startnum))))

(defmethod emsane-get-file-pattern ((this emsane-section-interface))
  (let*
      ((pattern (emsane-handle-slot this 'file-pattern)))
    (if (functionp pattern)
        (funcall pattern)
      pattern
      )))

;;TODO refactoe get-source and get-mode
(defmethod emsane-get-source ((this emsane-section-interface))
  (let*
      ((scanner (emsane-get-scanner this)))
    (emsane-source-dealias scanner  (emsane-handle-slot this 'source))))

(defmethod emsane-get-mode ((this emsane-section-interface))
  (let*
      ((scanner (emsane-get-scanner this)))
    (emsane-mode-dealias scanner  (emsane-handle-slot this 'mode))
    ))

(defmethod emsane-get-scanner ((this emsane-section-interface))
  ;;TODO getters that return objects can store object name referenses internaly
  ;;this should be generalized, otoh theres only get-scanner atm that actualy needs it
  (let* ((scanner (emsane-handle-slot this 'scanner)))
    (cond ((object-p scanner) scanner)
          ((stringp scanner) (emsane-scanner-get scanner))
          (t (error "get-scanner on %s should return object or string but doesnt" this)))))


;; dealiases.  
;;TODO probably needs support for more than 1 flag per mode/source/whatever
;;like image-type-options?

(defmethod emsane-source-dealias ((this emsane-scanner) source-alias)
  (let ((source-dealias-rv  (cadr (assoc source-alias (oref this sources)))))
    (assert (not (null source-dealias-rv)) nil "null not allowed for alias")
    source-dealias-rv))

(defmethod emsane-mode-dealias ((this emsane-scanner) mode-alias)
  (let ((mode-dealias-rv (cadr (assoc mode-alias (oref this modes)))))
    (assert (not (null mode-dealias-rv)) nil "null not allowed for alias")
    mode-dealias-rv))


(defmethod emsane-get-buffer-create ((this emsane-scanner))
  (let*
      ((buffer (get-buffer-create (format "*Emsane %s*" (oref this object-name)))))
    (with-current-buffer buffer
      ;;dont reset locals if already in correct mode
      (unless (eq major-mode 'emsane-mode) (emsane-mode)))
    buffer))



(defmacro emsane-default-getter (slot)
  "declare a base version of a slot getter"
  `(defmethod ,(intern (concat "emsane-get-" (symbol-name slot))) ((this emsane-section-interface))
     (emsane-handle-slot this ',slot)))

(emsane-default-getter resolution)
(emsane-default-getter image-type)


(defmethod emsane-get-options ((this emsane-scanner) section)
  (let* ((options (oref this :options))
         (image-type-options (car (emsane-get-image-type-options this
                                                                 (oref section :image-type)))))
    (append options image-type-options)))


(defmethod  emsane-get-image-type-options ((this emsane-scanner) image-type)
  (cdr (assoc image-type (oref this :image-type-options))))

(defmethod  emsane-get-actual-image-type ((this emsane-scanner) image-type)
  "Give the actual image type a scanner produces, for a given image type."
  (let ((actual-img-type  (cadr (emsane-get-image-type-options this image-type))))
    (unless actual-img-type (setq actual-img-type 'pnm))
    actual-img-type))


(defmethod emsane-get-section-names ((this emsane-job))
  (emsane-job-get-section-list this))

(defmethod emsane-next-section ((this emsane-job) current-section)
  "fetch the next logical section from the job"
  (let
      ((mylist (emsane-get-section-names this)))
    (while (not (equal (oref current-section object-name) (car mylist) ))
      (setq mylist (cdr mylist)))
    (emsane-section-get (cadr mylist))))

(defmethod emsane-get-sections ((this emsane-job))
  "return a list of section job objects(rather than a list of strings)"
  (mapcar (lambda (x) (emsane-section-get x))
          (oref this section-list)))

(defmethod emsane-get-root-directory ((this emsane-job))
  ;;TODO figure out some convenient way to override this, to place similar scans in the same dir
  emsane-root-directory )




(defun emsane-set-default-scanner (scanner)
  "set the default scanner in the buffer. the actual scanner used
might still be modified by section settings"
  (interactive
   ;;TODO check if scanner is consisten with job etc, (obj or name reference?)
   (list (emsane-ask-scanner)))
  (setq emsane-current-default-scanner scanner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun emsane-setup-from-other-buffer (buffer newpagenum newscanner)
;;   "copy settings from another emsane buffer BUFFER.
;; This is useful when you work with several scanner buffers. Therefore the
;; scanner setting in the other buffer is not copied, nor the pagenum.
;; These are asked for
;; "
;;   (interactive
;;    (list (emsane-ask-buffer)
;;          (emsane-ask-pagenum)
;;          (emsane-ask-scanner))
;;    )
;;   (let ((a) 
;;         (b) 
;;         (d))
;;     (save-excursion
;;       (set-buffer buffer)
;;       ;;copy locals. 
;;       (setq a emsane-current-job) 
;;       (setq b emsane-current-section) 
;;       (setq d emsane-current-job-id)
;;       )
;;     (setq emsane-current-job a) 
;;     (setq emsane-current-section b) 
;;     (setq emsane-current-job-id d)
;;     ;;scanner and pagenum must be handled specially
;;     (setq emsane-next-pagenumber newpagenum)
;;     (setq emsane-current-default-scanner newscanner)

;;     (emsane-set-mode-line)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod emsane-get-job-dir  ((this emsane-process-state))
  "Return directory used to store scans.
Parent directories are created if needed."
  (let
      ((dir (concat (expand-file-name (emsane-get-root-directory (oref this :job)))
                    "/"
                    (oref this :job-id)
                    "/")))
    (mkdir dir t)
    dir))


(defun emsane-set-section (&optional section)
  "Set section. if SECTION is nil, prompt for one."
  (interactive)
  (if section (setq emsane-current-section section)
    (let* ((next-section (if emsane-current-section
                             (emsane-next-section emsane-current-job
                                                  emsane-current-section)))
           (nextname (if next-section (oref next-section object-name) "")))

      ;;TODO if next-section is nil, were at the end and should do something useful
      ;;- start a new scan with same job but with new job-id? same as (emsane-again) so redundant?
      ;;- just rewind and suggest the 1st section?
      (setq section-name (completing-read (format "section[%s]: " nextname) ;;TODO should go to emsane-query
                                          (emsane-get-section-names emsane-current-job)
                                          nil nil nil nil
                                          nextname))
      (setq emsane-current-section (emsane-section-get section-name))))
  (emsane-set-page  (emsane-get-start-page emsane-current-section))
  (emsane-set-mode-line))


(defun emsane-parse-paper-size (size-string sizes)
  "Return a size cons from SIZE-STRING.
SIZE-STRING is either an ISO paper size \"A4\" or a string like \"210 x 297\" (A4 in mm)."
  (if (consp size-string) size-string
    (let ((size (if (listp sizes) (cadr (assoc (downcase size-string) sizes)))))
      (if size
          size
        (if (string-match "\\([0-9]*\\)[ ]*x[ ]*\\([0-9]*\\)" size-string)
            (cons (string-to-number (match-string 1 size-string))
                  (string-to-number (match-string 2 size-string))))))))



(defvar emsane-job-history)
(defvar emsane-job-id-history)
(defvar emsane-scanner-history)



(defun emsane-set-page (pagenum)
  "Set next scan to begin with PAGENUM in the filename."
  (interactive (list (emsane-ask-pagenum)))
  (setq emsane-next-pagenumber pagenum)
  (emsane-set-mode-line))

;; (defun emsane-set-mode-line ()
;;   "Update the modeline with the current job, pagenumber, etc."
;;   (setq mode-line-buffer-identification
;;         (nconc (propertized-buffer-identification "%b")
;;                (list
;;                 (format " %s [%s %s] %s"
;;                         emsane-current-job-id
;;                         (if emsane-current-job (oref emsane-current-job object-name) "no job")
;;                         (if emsane-current-job (oref emsane-current-section object-name) "no section")
;;                         emsane-next-pagenumber)))))

;;TODO have a look at using force-mode-line-update
(defun emsane-set-mode-line (section)
  "Update the modeline with the current job, pagenumber, etc."
  (setq mode-line-buffer-identification
        (nconc (propertized-buffer-identification "%b")
               (list
                (format " %s [%s %s] %s"
                        "?" ;;emsane-current-job-id
                        "?" ;;(if emsane-current-job (oref emsane-current-job object-name) "no job")
                        (oref section object-name)
                        "?";;emsane-next-pagenumber
                        )))))



;; (defun emsane-scan-start (buffer-or-name
;;                           job-name
;;                           job-id
;;                           &optional
;;                           scanner-name
;;                           start-section-name)
;;   "Start up a new job.
;; Prompt the user to feed the scanner.
;; Argument BUFFER-OR-NAME is the scanadf scan buffer.
;; Argument JOB-NAME is the name of the job to use.
;; JOB-ID is used to identify  the scan."
;;   (interactive
;;    (let*
;;        ((buffer-name
;;          (if current-prefix-arg
;;              (read-buffer "emsane buffer: "
;;                           (generate-new-buffer-name "*emsane*")) ;;TODO emsane-query
;;            "*emsane*"))
;;         ;;TODO this vX scheme was hurried and ugly
;;         (v2 (emsane-ask-job))
;;         (v3 (emsane-read-job-id (emsane-job-get v2))))
;;      (list buffer-name v2 v3)))
;;   (cond
;;    ((bufferp buffer-or-name)
;;     (switch-to-buffer buffer-or-name))
;;    ((stringp buffer-or-name)
;;     (emsane-prepare-buffer buffer-or-name))
;;    (t (error "Invalid buffer-or-name")))

;; ;;  (setq emsane-current-job-id job-id)
;; ;;  (setq emsane-current-job (emsane-job-get job-name))
;; ;;  (setq emsane-current-default-scanner (if scanner-name  (emsane-scanner-get scanner-name) emsane-default-scanner))

;;   (unless start-section-name (setq start-section-name (oref (car (emsane-get-sections (emsane-job-get job-name))) :object-name)))
;;   (emsane-set-section (emsane-section-get start-section-name))
;; ;;  (emsane-last-section-slots-reset)
;;   (emsane-set-mode-line))

;; (defun emsane-scan-again ()
;;   "Make a new scan, reuse as much settings as possible, except for the job-id."
;;   (interactive)
;;   (emsane-scan-start
;;    (current-buffer);;TODO verify somehow
;;    (oref emsane-current-job :object-name)
;;    (emsane-read-job-id emsane-current-job)))

;; (defun emsane-scan-section-again ()
;;   "rescan current section, basically just reset the pagenum"
;;   (interactive)
;;   (emsane-set-section  (emsane-section-get (oref emsane-current-section :object-name))))

(defconst emsane-scan-file-suffix ".scan")


;; (defun emsane-scan-section (&optional section  startcount-info  job-id)
;;   (interactive)
;;   (unless section (setq section emsane-current-section))
;;   (unless job-id (setq job-id emsane-current-job-id))
;;   (emsane-scan section emsane-the-postop-queue (current-buffer) )   )



(defun emsane-scan-start (job job-id &optional start-section section-overide)
  "start a new scan job."
  ;;piece together an emsane-process-state
  ;;start the scan
  (interactive
   (let*
       ((job (emsane-job-get (emsane-do-query (emsane-query-object "gimmejob" :prompt "job" :object-type 'job))))
        (job-id (emsane-read-job-id job))
        (start-section  (car (emsane-get-sections job))))
     (list job job-id start-section)))
  (unless start-section (setq start-section (car (emsane-get-sections job))))
  (let*
      ((state (emsane-process-state job-id :job-id job-id :job job
                                    :postop-queue nil
                                    :section start-section
                                    :section-overide section-overide
                                    ))
       (q  (emsane-postop-queue job-id
                                :default-directory (emsane-get-job-dir state)
                                :process-buffer (get-buffer-create (format "*emsane postop %s*" job-id))))
       )
    (oset state :postop-queue q)
    (emsane-query-recall-reset)
    (emsane-scan state)))

(defun emsane-scan-continue (state)
  (interactive (list emsane-current-process-state))
  (unless state (setq state emsane-current-process-state))
  (emsane-scan state)
  )

(defmethod emsane-scan ((this emsane-process-state)
                        &optional buffer the-sentinel the-filter)
  ;;TODO
  ;;- guard against file overwrites when scanning. ask user if overwriting is what she really wants.
  ;;  (not sure, overwriting has proven convenient)
  ;;- if a scan process is already running in buffer signal error and stop
  ;;the scanner buffer used is figured out from the scanner name by default
  (unless buffer (setq buffer (emsane-get-buffer-create (emsane-get-scanner (oref this :section)))))
  (unless the-sentinel (setq the-sentinel 'emsane-sentinel))
  (unless the-filter (setq the-filter 'emsane-filter))
  (save-excursion
    (set-buffer buffer)
    (if (emsane-process-running) (error "scanner process already running in this buffer"))
    (setq emsane-current-process-state this) ;;TODO is this the right place really?
    (let*
        ((postop-queue (oref this :postop-queue))
         (section (if (oref this :section-overide)  (progn (oset (oref this :section-overide)  :parent section) (oref this :section-overide) ) (oref this :section)))
         (job-dir (oref postop-queue :default-directory)) ;;TODO cleanup these bindings a bit, they happened due to refactoring
         (dummy-dirok (assert (equal (substring job-dir -1) "/") nil "dir must end with /"));;It took a lot of time before I realized this is necessary
         (default-directory  job-dir)
         (scanner (emsane-get-scanner section))
         (options (emsane-get-options scanner section))
         (dealiased-source (emsane-get-source section))
         (resolution (emsane-get-resolution section))
         (dealiased-mode (emsane-get-mode section))
         (file-pattern (emsane-get-file-pattern section))
         ;;TODO unify :next-pagenumber and :start-page. it can possibly be stored in :section-overide
         (startcount  (if (slot-boundp state :next-pagenumber) (oref state :next-pagenumber) (emsane-get-start-page section)))
         (imgtype (emsane-get-image-type section))
         (size (emsane-get-size section))
         (paperwidth (car size))
         (paperheight (cdr size))
         (topleft1 (- (/ (oref scanner :scanwidth) 2)
                      (/ paperwidth 2)))
         (topleft (if (oref scanner :topleft-adf)
                      topleft1
                    0))
         ;;TODO this is for croping from center, which maybe not all adf scanners need.
         ;;for flatbeds, it would be: paperwidth
         
         ;;(imgtype (emsane-section-get-image-type emsane-current-section))

         ;; only if hw supports it and only if jpg-color-hw option
         ;;normaly sane 1.0.20 must be recompiled to get get this

         ;; funnily the "fujitsu" requires --page-height, while the "brother" "test" and "epson" fails with it.
         (page-height (if  (oref scanner :needs-pageheight) 
                          (list "--page-height" (number-to-string paperheight))))
         (args `("scanadf" ,buffer "scanadf"
                 "--device-name" ,(oref scanner :device)
                 ,@(if dealiased-source (list "--source" dealiased-source))
                 "--mode" ,dealiased-mode;;(emsane-mode-dealias scanner  mode)
                 ,@options;;(emsane-get-options scanner  emsane-current-section) ;;scanner specific options
                 "--resolution"  ,(number-to-string resolution)
                 "--output-file" ,(concat file-pattern emsane-scan-file-suffix) 
                 "--start-count" ,(number-to-string startcount)
                 ,@(if (oref scanner :inhibit-adf) (list "--end-count" (number-to-string startcount) ))
                 "-l" ,(number-to-string topleft)
                 "-t" ,(number-to-string 0)
                 "-x" ,(number-to-string paperwidth)

                 ;;TODO figure out how to handle height better.
                 ;; y is height of scan-area
                 ;; page-height is height of scanner physical scan area, but this is less than max by default.
                 ;;y <= paperheight
                 "-y" ,(number-to-string paperheight)
                 ,@page-height))
         (dbg-process
          (format "scan command: %s\n" args
                  (mapconcat (lambda (x) x) (cddr args) " ")
                  
                  ))
         ;;TODO verify the type of each element of arg. no nil:s for instance
         ;;(all-not-nil  (mapconcat (lambda (x) (stringp x)) (cddr args) " "))
         (scan-process
          (apply 'start-file-process
                 args
                 )))
      (insert dbg-process)
      (insert (format "job-dir:%s\n" job-dir))
      (set-process-sentinel scan-process the-sentinel)
      (set-process-filter scan-process the-filter)
      (process-put scan-process 'emsane-process-state
                   this)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode setup


(defvar emsane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m"        'emsane-scan-continue)
    (define-key map "s"           'emsane-scan-start)

    ;;TODO
    (define-key map "a"           'emsane-scan-again)
    (define-key map "q"           'emsane-scan-quit)
    (define-key map "r"           'emsane-scan-section-again)

    (define-key map "n"           'emsane-set-section)

    (define-key map "p"           'emsane-set-page)
    (define-key map "d"           'emsane-dired)
    map)
  "Keymap for `emsane-mode'.")

(define-derived-mode emsane-mode fundamental-mode
  "emsane-mode"
  "scanner frontend mode"
  (set (make-local-variable 'emsane-current-process-state) nil) 
  ;;(emsane-set-mode-line)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired support

(defun emsane-dired ()
  "Dired the current scan project."
  (interactive)
  (dired (emsane-get-job-dir emsane-current-job)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process control support


(defun emsane-process-running ()
  "check if a process is running in the current buffer"
  ;;the code is a little hard to read. (process-status) returns a symbol or throws an error
  ;; - if the symbol is 'run, there is a running symbol, the other symbols I assume to mean
  ;; - if an 
  (condition-case nil
      (if (equal 'run (process-status nil)) t nil)
    (error nil)))



(defun emsane-scan-quit ()
  "quit running scan process"
  (interactive)
  (if (emsane-process-running) 
      (delete-process (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;subsection support

(defun emsane-set-subsection (subsection)
  "set subsection counter"
  (interactive (list (emsane-ask-subsection)))
  (setq emsane-subsection subsection)
  (emsane-set-page 1)
  (emsane-set-mode-line))

(defun emsane-subsection-filepattern ()
  "used in configs when you want a subsection counter"
  ;;TODO support section id
  (concat  (format "01%02d" emsane-subsection) "-%04d" ))

(defmacro emsane-subsection-filepattern-lambda (section)
  `(lambda ()   (concat  (format "%02d%02d" ,section  emsane-subsection) "-%04d" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scanner process sentinel and filter


(defun emsane-sentinel (scan-process msg)
  "Called when scanadf is finished.
Argument SCAN-PROCESS is the previously created scanadf process.
Argument MSG is the exit code."
  (call-process-shell-command emsane-notify-command nil 0)
  (message (format "scan finished with code:%s in buffer %s" msg (process-buffer scan-process))))

;; TODO Also react on these type of strings:
;; Scanned document 00-0001 ;;start post-processing
;; scanadf: sane_start: Document feeder jammed
;; Document feeder jammed
;; scan finished?


(defun emsane-filter (proc string)
  "Filters scanadf output.
Argument PROC scanadf process.
Argument STRING output from scanadf."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc)))
          (state (process-get proc 'emsane-process-state)))
      (save-excursion
        (assert (not (null state)) nil "state cant be nil")
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        ;;break up string in lines and handle each
        (mapcar (lambda (line) (emsane-line-handler line
                                               state
                                               )) (split-string string "\n" t))
        (insert (format "filter:<<%s>>\n" ;;TODO should be possible to visit image files in the scanadf buffer!
                        (substring string 0 -1)
                        ))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun emsane-line-handler (string state)
  ;;TODO is a method really
  "Process a single line STRING of scanadf output."
  (let*
      ((filename nil)
       (tx nil)
       (section (oref state :section))
       (postop-queue (oref state :postop-queue))
       (op-list (oref section :operation-list))
       )
    ;;TODO refactor this: - cond looks strange - factor out string match code from action code
    (cond
     ((string-match "Scanned document \\(.*\\)" string) ;;a string emitted by scanadf
      (setq filename (match-string 1 string))
      (emsane-line-default-postop filename)
      (string-match (concat "\\([0-9a-zA-Z]*\\)-\\([0-9]*\\)" emsane-scan-file-suffix) filename) ;;this must match the scanned page, which must have a .scan suffix
      (oset state  :next-pagenumber
            (+ 1 (string-to-number (match-string 2 filename))))
      ;;(emsane-set-mode-line section) ;;TODO update modeline in some intelligent way
      )
     (t );;TODO do something if line didnt match
     )))

(defun emsane-line-default-postop (filename)
  ;;TODO uhm... this is tightly coupled to emsane-line-handler through dynamic scope lite bindings. dunno how that happened actually
  ;;begin tx
  (setq tx (emsane-postop-transaction "tx"))
  (emsane-postop-setenv tx 'SCANFILE filename)
  (emsane-postop-setenv tx 'SCANFILEBASE (substring filename 0 ( -(length emsane-scan-file-suffix))))
  (if (null op-list) ;;nil currently means do a default conversion
      (progn
        ;;push a default op:s for now. these converts and deletes original.
        ;;since ops are pushed delete goes before convert which is harder to read TODO add prepend operation
        (emsane-postop-push tx (emsane-postop-lisp-operation "op"
                                                             :operation-lambda
                                                             (lambda (tx q) (delete-file (emsane-postop-getenv tx 'SCANFILE)))))
        (emsane-postop-push tx (emsane-mkpostop-convert section)))
    ;;TODO else push the op-list, assume "dust" now EXPERIMENTAL
    (mapcar (lambda (x)
              (emsane-postop-push tx x))
            (reverse (emsane-mkpostop-dustdetect)))
    )

  (emsane-postop-push postop-queue tx)
  ;;end tx
  (emsane-postop-go postop-queue)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup the postprocess queue
;; postprocessing support

;; ;;TODO only 1 queue atm. if you have a octo core or something youd like more queues
;; (setq emsane-the-postop-queue  (emsane-postop-queue "transaction_queue"
;;                                                     :default-directory nil ;;must be initialized to run
;;                                                     :process-buffer (get-buffer-create "*emsane postop*")
;;                                                     :error-hooks    (list (lambda () (message "postop q error hook called"))) ))

(defvar emsane-image-type-suffixes
  '((jpg-gray jpg) (jpg-color jpg)(djvu-color djvu)))


(defun emsane-mk-conversion-command (filenamebase current-suffix type desired-type)
  "produce a command line string to convert FILENAME from TYPE to DESIRED-TYPE.
FILENAME is currently assumed have a .scan suffix"
  (let*
      ((filename (concat filenamebase current-suffix))
       ;;(filenamebase (substring filename 0 ( -(length emsane-scan-file-suffix))));;this removes the ".scan" suffix
       (replacesuffix (cadr (assoc desired-type emsane-image-type-suffixes)))
       (suffix (if replacesuffix replacesuffix desired-type)))
    (cond
     ((equal desired-type type )
      (format
       "cp %s %s.%s"  filename filenamebase suffix));;cp is for concistency, TODO find more efficient way
     ((eq desired-type 'djvu)
      (format
       "cjb2 -lossy %s %s.%s" filename  filenamebase suffix))
     ((eq desired-type 'djvu-color)
      (format
       "c44 %s %s.%s" filename  filenamebase suffix))
     ((eq desired-type 'jpg-gray)
      (format
       "convert -type Grayscale %s %s.%s"  filename  filenamebase suffix))
     ((eq desired-type 'jpg-color)
      (format
       "convert %s %s.%s"  filename  filenamebase suffix))
     (t
      (format
       "convert %s %s.%s"   filename  filenamebase suffix)))))


(defun emsane-mkpostop-convert (section)
  "mk conversion postop"
  (let*
      ((imgtype (emsane-get-image-type section))
       (convcmd (emsane-mk-conversion-command
                 "${SCANFILEBASE}" emsane-scan-file-suffix
                 (emsane-get-actual-image-type (emsane-get-scanner section) imgtype)
                 imgtype)))
    (emsane-postop-simple-shell-operation
     "op1"
     :operation-shell-command convcmd)))

(defun emsane-image-size (image-file)
  "Return the size of IMAGE-FILE as a cons."
  ;;TODO I snipped this from "dragbox.el" so it might be reusable
  (with-current-buffer (get-buffer-create "*imagemagic identify*")
    (erase-buffer)
    (call-process "identify" nil "*imagemagic identify*" nil "-verbose" image-file) ;; "-ping" sometimes segfaults for me
    (goto-char (point-min))
    (re-search-forward "Geometry: \\([0-9]+\\)x\\([0-9]+\\)")
    (cons (string-to-number (match-string 1))
          (string-to-number (match-string 2)))))

(setq emsane-dust-area-height 30)
;;150px should be 1/2 inch at 300 dpi = 1.27 cm
;;turns out only 30px is suitable for this measurment! 30px = 1/10 inch = 2.54/10 cm
(defun emsane-mkpostop-dustdetect ()
  "detect dust adf scanner glass and quit"
  ;;- preop: add 150px to scan area somehow(just hardcode for now)
  ;;- postop:
  ;;- split image in 2 parts, "image.jpg" and "dust.jpg", at 150px from bottom
  ;;- "dust dust.jpg" is added to tx, if it fails, transaction fails, job fails
  ;;- normal postprocessing of "image" (emsane-mkpostop-convert)
  (list
   (emsane-postop-lisp-operation "size" :operation-lambda  (lambda (tx q)
                                                             (emsane-postop-setenv tx
                                                                                   'CROP_AT_Y
                                                                                   (- (cdr (emsane-image-size (emsane-postop-getenv tx 'SCANFILE)))
                                                                                      emsane-dust-area-height))))
   (emsane-postop-simple-shell-operation "crop-dust"  :operation-shell-command "convert +repage -crop +0+${CROP_AT_Y} ${SCANFILE} ${SCANFILE}.dust" )
   (emsane-postop-simple-shell-operation "crop-img"   :operation-shell-command (format "convert +repage -crop 0x0+0-%s ${SCANFILE} ${SCANFILE}"
                                                                                       emsane-dust-area-height) )
   (emsane-mkpostop-convert )
   (emsane-postop-simple-shell-operation "dust"  :operation-shell-command "dust ${SCANFILE}.dust"))
  )

;;;TODO this is from a previous version of postop code, so it needs to be converted
(defun emsane-unpaper-args-mb (filename)
  "An unpaper argument creator for booklets made from folded A4 sheets.
When scanning remove staples and unfold.  FILENAME is the file to unpaper."
  ;;TODO this isnt tested lately
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


(provide 'emsane)

;;; emsane.el ends here

