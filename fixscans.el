;;20191229 - fiddle with scans

;;  /flib/[ebooks]/[myscans]

;; - pack dir to cbz
;;   -0 = only store, because zipping jpegs is dumb and slow
;;   how do you do something for each marked file?
(defun fscans-dired-pack-to-cbz ( dirs)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "pack the dir as cbz"
  (-map (lambda (dir)
          (call-process-shell-command (format "zip -0 -r '%s.cbz' '%s'" dir dir) nil "*fscans*"))
        dirs
        )
  
  )

;;(fscans-dired-pack-to-cbz '("a" "b"))
;;(fscans-dired-pack-to-cbz "a" "b")

;; - pack dir to djvu book
;;   djvm -c ../rasrisk.djvu *.djvu

(defun fscans-dired-pack-to-djvm (dir)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     files))
  "pack the dir as djvm"
  (call-process-shell-command (format "djvm -c  '%s.djvu' '%s'/*.djvu" dir dir) nil "*fscans*")
  )


;; - verify the packed scan with evince

;; - send the packed scan to calibre

;; - delete the packed dir, and packed scan

;; - done, so delete memory of ongoing process


;; - calibre atm not support djvu covers, so until plugin exists
;; convert x.djvu x.png

;; all-the-icons-dired is slow with scan dirs, so remove it
(remove-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;find . -type d -exec basename '{}' ';'|sort|uniq -d|wc -l
;;cd advancedlinuxprogramming ; zip -r ../advancedlinuxprogramming.htmlz .; cd ..

;; to fix multipart zips.
;; the numbers arent uniform, fill out to 3 digits, also add part number
;; remove redundant comercial text files
;; tried using % R in dired, ^[0-9][0-9][0-9].jpg to 001-\&
;; is cumbersome but kinda works
;; did "world without end" this way

;; send to calibre
(defun fscans-dired-send-to-calibre (files)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "calibre"
  (-map (lambda (file)
          (call-process-shell-command (format "calibre  '%s'" file) nil "*fscans*"))
        files
        )
  
  )

(defun fscans-dired-send-to-gimp (files)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "gimp"
  (-map (lambda (file)
          (start-file-process-shell-command "gimp"  "*fscans*" (format "gimp %s" file) ))
        files
        )
  
  )

;;both djvm and tjvu could be the same command depending on filtype of arg
(defun fscans-dired-convert-to-djvu (files)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "gimp"
  (-map (lambda (file)
          (call-process-shell-command
           (format "convert %s %s.pnm && c44 %s.pnm %s.djvu" file file file file)
           nil "*fscans*"))
        
        files))
  
(defun fscans-dired-unpack (files)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "unpack"
  (-map (lambda (file)
          (call-process-shell-command
           (format "aunpack '%s'" file)
           nil "*fscans*"))
        
        files))
    


;;(start-file-process-shell-command "gimp"  "*fscans*" (format "gimp %s" ""))

;; hydra


(defhydra hydra-fixscans ()
  "
_z_: cbz
_d_: djvm
_c_: calibre
_e_: edit in gimp
_p_: to djvu
"
  ("z" fscans-dired-pack-to-cbz)
  ("d" fscans-dired-pack-to-djvm)
  ("c" fscans-dired-send-to-calibre)
  ("e" fscans-dired-send-to-gimp)
  ("p" fscans-dired-convert-to-djvu)
  ("u" fscans-dired-unpack)    
  )


(define-key dired-mode-map "z" (function hydra-fixscans/body)  )
;; convert various cover png to djvu
;; convert x.png x.pnm && c44 x.pnm

;;rotate djvu file
;;djvused tst.djvu -e 'select 1; set-rotation -1; save-page tst2.djvu' 
;; for x in *djvu; do djvused $x -e "select 1; set-rotation -1; save-page $x" ;done

;;calibredb
;;calibredb add --username joakim --password calibrekn4rk  --with-library "http://192.168.210.1:8085/#calibre" -s 2000+ 2000+*
;;sadly this doesnt add djvu covers

;;regexp dired rename
;;\([0-9]+\)-\([0-9]+\).djvu
;;\101-\2.djvu

(defun fixscans-dired-rename ()
  (interactive)
  (let ((rename-regexp-query t);;doesnt work
        (dired-mode-hook nil));;maybe hooks are slowing us down?
    (dired-do-rename-regexp "\\([0-9]+\\)-\\([0-9]+\\).djvu" "\\101-\\2.djvu")
    (dired-do-rename-regexp "cover-\\([0-9]+\\).djvu" "0000-\\1.djvu")
    )
  )


 
;;
(defun fixscans-dired-renamenumerically (files)

  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "rename numerically"
  (-map-indexed (lambda (index file)
                  ;;dired-rename-file didnt work for some reason
                  (rename-file file
                               (format "0000-%04d.djvu" (1+ index) )
                                     nil)
                  )
        files
        )
  )

(defun fixscans-dired-renamenumerically-2 (files)

  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "rename numerically"
  (let ((section (read-string "section:"))
        (offset (read-number "offset:")))
    (dired-sort-other "-alv")
    (-map-indexed (lambda (index file)
                    ;;dired-rename-file didnt work for some reason
                    ;;TODO the file extension should be kept from the original file name
                    (rename-file file
                                 (format "%s-%04d.%s" section
                                         (+ offset index)
                                         (file-name-extension file))
                                 nil)
                    )
                  files
                  ))
  )

(defun fixscans-dired-join-dirs (files)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list files)))
  "copy dir contents to a new dir"
  (let ((destination  (read-file-name "new dir name:"))) 
    (dired-create-directory destination)
    (-map (lambda ( file)
            (f-copy-contents (format "%s" file) destination )
            )
          files
          ))

  )

;;renaming the hotcomic scans this way doesnt work, you need to sort dired like this:
;; ls |sort -n
;; in dired, c-u s, and add -v flag
