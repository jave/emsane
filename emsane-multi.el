(require 'emsane)
;; (C) FSF 2010, Joakim Verona, part of the emsane package

;;Here we deal with using several scanners to scan a single book, magazine etc.

(emsane-declare-instance-get multi           nil "list of multi scanner configurations.")
(emsane-declare-instance-get multi-scan-conf nil "list of scanner configurations to be used in multi  scanner setups.")

(defclass emsane-multi  (emsane-tracker)
  ((tracking-symbol :initform 'emsane-multi-list)
   (scanner-list :initarg :scanner-list
                 :documentation "list of scanner buffer setups in this setup")
   (job :initarg :job
        :documentation "job, book, for instance"))
  "list of scanner configurations")

(defclass emsane-multi-scan-conf  (emsane-tracker
                                   emsane-section-interface)
  ((tracking-symbol :initform 'emsane-multi-scan-conf-list)
   (start-section :initarg :start-section  :documentation "which section to start with within the job")))



(defun emsane-multi-scan (multi-conf job-id)
  "Scan a single project with multiple scanners. Each scanner
will scan different sections of the material."
  ;;TODO
  ;;- each buffer is scanner centric, map to a scanner
  ;;- keymap to jump quickly to each scanner buffer(1,2,3...)
  ;;- a multi scan def, is a list of scanners, start section and pagenums for each new emsane buffer
  ;;  book-multi, fujitsu1:book-body, fujitsu2:book-body, brother:cover-simplex
  ;;- reset and reuse all old scan buffers
  (interactive (let*
                   ;;TODO this turned out rather horrible... bindings are duplicated below refactor
                   ((v1 (emsane-do-query (emsane-query-object "multiscan" :prompt "multi-scan" :object-type 'multi)))
                    (v2 (emsane-multi-get v1));;(emsane-ask-job-id job) ;;TODO refactor job-id prompt
                    (v3 (emsane-read-job-id (emsane-job-get (oref v2 :job))))
                    )
                 (list v1 v3)))
  (let*
      ((multi (emsane-multi-get multi-conf))
       (scanner-list (oref multi :scanner-list))
       (job (oref multi :job)))
    (mapcar (lambda (scanner)
              (emsane-scan-start
                  (oref scanner :start-section)

                  job
                  job-id
                                 )
              ) scanner-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO functions for handling a list of multi-scans in sequence,
;;so that starting a new job is quick

(setq emsane-the-multi-batch
      '(("book-multi" "titel1" "a4" )
        ("book-multi" "titel2" "a3" )))
(setq emsane-the-multi-batch-index 0)


(defun emsane-multi-batch-next ()
  (interactive)
  (let* ((setting (nth emsane-the-multi-batch-index emsane-the-multi-batch)))
    (setq emsane-last-section-slots (emsane-section "dummy" :size (cdr setting))) ;;TODO refactor
    (emsane-multi-scan (car setting) (cdr setting))
    (setq emsane-the-multi-batch-index (+ 1 emsane-the-multi-batch-index ))))


(provide 'emsane-multi)