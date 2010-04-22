;;unit tests for emsane, using ert.el, available from ELPA

(require 'ert)
(require 'emsane)
(require 'emsane-postop)

;; properties specific for the "test" virtual scan device SANE provides
;;(setq emsane-scanner-list nil)
(emsane-scanner "test"
                :device "test"
                :scanwidth 216
                ;;:mode ;;TODO Color or Gray and depth options for test
                :modes '((color "Color")
                         (gray "Gray"))
                
                :options '("--test-picture"
                           "Color pattern")
                :sources '((duplex  "Automatic Document Feeder"))
                :image-type-options nil
                )


(deftest emsane-postop-queue-push-pop ()
  ;;simple push/pop test
  (let*
      ((q  (emsane-postop-queue "tq"))
       (pushed 'a)
       (push-r (emsane-postop-push q pushed))
       (poped (emsane-postop-pop q)))
    (should (equal pushed poped))))


(deftest emsane-postop-donext-safe ()
  ;;donext shouldnt crash, or recurse unexpectedly
  (let*
      ((q  (emsane-postop-queue "tq"))
       (emsane-postop-donext q)
       (emsane-postop-donext q))))

(deftest emsane-postop-donext-once ()
  ;;make a queue, push a tx with 1 op, verify it executed
  (let*
      ((q  (emsane-postop-queue "tq"))
       (tx (emsane-postop-transaction "tx"))
       (op (emsane-postop-lisp-operation "op"
                                         :operation-lambda
                                         (lambda (tx q) (message "im an op and im ok")
                                           ;;(oset tx :result 'ok)
                                           ))))
    (emsane-postop-push tx op)
    (emsane-postop-push q tx)
    (oset q :default-directory "/tmp")
    (emsane-postop-go q)
    ;;(assert-equal  'ok (oref tx :result))
    ))

(deftest emsane-postop-donext-shell ()
  ;;make a queue, push a shell tx, verify it executed
  (let*
      ((q  (emsane-postop-queue "tq"))
       (tx (emsane-postop-transaction "tx"))
       (op1 (emsane-postop-simple-shell-operation "op1"
                                                  :operation-shell-command
                                                  "x=w00t && echo $x$x"))
       (op2 (emsane-postop-simple-shell-operation "op2"
                                                  :operation-shell-command
                                                  "echo $TESTVAR ... $TOASTVAR")))
    (emsane-postop-push tx op1)
    (emsane-postop-push tx op2)
    (emsane-postop-setenv tx 'TESTVAR "W00T")
    (emsane-postop-setenv tx 'TOASTVAR "AAH")
    (emsane-postop-push q tx)
    (oset q :process-buffer "*emsane postop*")
    (oset q :default-directory  "/tmp")
    ;;(oset tx :result "w00t")
    (emsane-postop-go q)
    ;;w00tw00t should appear in the postproc buf
    ))

(deftest emsane-postop-tx-env ()
  ;;test that transaction environments behave
  (let*
      ((tx (emsane-postop-transaction "tx")))
    (emsane-postop-setenv tx 'key 'cool) ;;strings arent good keys
    (should (equal 'cool (emsane-postop-getenv tx 'key)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;value chain tests
;;- use oref to realy acces the internal value, no conversion or value chaining
;;- use handle-slot to access the value chain, but no other special
;;handling
;;- use getter to access value chain and convert value etc

(deftest emsane-handle-slot-with-overide ()
  "check that we can clone and set the :size slot"
  (let* ((modified-default (clone emsane-the-section-defaults)))
    (oset modified-default :size "a4")
    (should (equal "a4" (emsane-handle-slot modified-default :size)))))

(deftest emsane-handle-slot ()
  "check that definition chaining works. make a parent and child, check to see the child can overide the parent"
  (let* ((child (clone emsane-the-section-defaults)  )
         (parent (clone emsane-the-section-defaults)  ))
    (oset child :size "a4")
    (oset parent :size "a3")
    (oset parent :start-page 242)
    (slot-makeunbound child :start-page)
    (oset child :parent parent)
    (should (equal "a4" (emsane-handle-slot child :size))) ;;we get a4 from the child here and not 210x297, nor a3
    (should (equal 242 (emsane-handle-slot child :start-page))) ;;we should get the parent value since the child doesnt care
    ;;(assert-equal 1 (emsane-handle-slot p1 :start-page))

    ;;now test the getters, who wrap handle-slot and provides further processing
    (should (equal '(210 . 297) (emsane-get-size child)));;get-size parses a4 to (210x297)
    ))

(defvar emsane-test-jobdir "/tmp/emsane-test/")


(defvar emsane-test-jobdir "/tmp/emsane-test/")

(deftest emsane-scan ()
  "basic test of emsane-scan. check the buffer for nice output"
  (progn 
  (emsane-test-setup-jobdir)
    (let*
      ;;we need a clone since otherwise the orignal setting object will be modified
        ((settings (emsane-section-value "test-settings"
                                             :scanner "test"
                                             :source 'duplex
                                             :mode 'color
                                             :resolution 300
                                             :parent nil ;;dont mess up this test with too much deps
                                             :file-pattern "0100-%04d"
                                             :image-type 'jpg
                                             :size "a4"
                                             :start-page 1
                        ))
         (buffer (pop-to-buffer "*emsane test scan buffer*")))
      (emsane-scan settings buffer    (emsane-postop-queue "test_transaction_queue"
                        :default-directory emsane-test-jobdir
                        :process-buffer (get-buffer-create "*emsane postop test*")
                        :error-hooks    (list (lambda () (error "test postop q error hook called"))) )
                   (lambda (proc msg)     (with-current-buffer (process-buffer proc)(insert (format "sentinel:%s\n" msg)) ))
                   (lambda (proc string)  (with-current-buffer (process-buffer proc) (insert (format "filter:%s\n" string))))
                   )))
   )




(defun emsane-test-setup-jobdir ()
  (delete-directory emsane-test-jobdir t)
  (mkdir emsane-test-jobdir t)
  )

(deftest emsane-line-handler ()
  (emsane-test-setup-jobdir)
  (copy-file  "~/.elisp/emsane/0100-0001.scan" emsane-test-jobdir);;TODO remove hardcode
  ;;simulate the line handler receving a scaned file notification.
  ;;this will also test the default behaviour of the postop queue
  (emsane-line-handler
   (format "Scanned document %s0100-0001.scan" emsane-test-jobdir)
   (emsane-section "test-settings"
                   :operation-list nil                                             
                   :scanner "test"
                   :source 'duplex
                   :mode 'color
                   :resolution 300
                   ;;:parent nil ;;dont mess up this test with too much deps
                   :file-pattern "0100-%04d"
                   :image-type 'jpg
                   :size "a4"
                   :start-page 1
                   )
   (emsane-postop-queue "test_transaction_queue"
                        :default-directory emsane-test-jobdir
                        :process-buffer (get-buffer-create "*emsane postop test*")
                        :error-hooks    (list (lambda () (error "test postop q error hook called"))) )
   )
    )

(deftest emsane-process-running ()
   ;;just check no process running in this buffer
  ;;TODO check case with running process
   (should (null (emsane-process-running))))

(deftest emsane-mk-conversion-command ()  
  (should (equal "cjb2 -lossy mupp.scan mupp.djvu" (emsane-mk-conversion-command "mupp" ".scan" 'jpg 'djvu)))
  (should (equal "cp mupp.scan mupp.jpg"  (emsane-mk-conversion-command "mupp" ".scan" 'jpg 'jpg)))
  )

(deftest emsane-image-size ()
;;  (emsane-image-size "") ;;TODO get a test image to test with
  )

(deftest emsane-parse-paper-size ()
  "test paper size parsing"
  (should (equal '(210 . 297) (emsane-parse-paper-size '(210 . 297) emsane-paper-sizes)))
  (should (equal '(210 . 297) (emsane-parse-paper-size "a4" emsane-paper-sizes)))
  (should (equal '(210 . 297) (emsane-parse-paper-size "210 x 297" emsane-paper-sizes))))

(deftest emsane-declare-instance-get ()
  "just test declarations happened as expected, that is,
emsane-declare-instance-get worked as expected"
  (should (fboundp 'emsane-section-get))
  (should (boundp 'emsane-section-list))
  (should (boundp 'emsane-section-history)))


;;;;;;;;;;;;;;;;;;INTERACTIVE TESTS
;; they prompt the user and i havent found a way to simulate it yet.
;;to run t
;;xdootool might be used to simulate keypresses
;;(ert-run-tests-interactively "^emsanei-" " *emsane self-tests*")
(deftest emsanei-type-a4()
  "type a4"
  (interactive)
  (should (equal '(210 . 297) (emsane-handle-slot emsane-the-section-defaults :size))))

;;TODO some testing code. should be unit tests
;;(emsane-do-query (emsane-query-object "gimmescanner" :prompt "gimme scanner" :object-type 'scanner))
;;(emsane-do-query (emsane-query-object "gimmejob" :prompt "gimme job" :object-type 'job))
;;(emsane-do-query (emsane-query-integer "gimmeint" :prompt "gimme int" :default 100))
;;(emsane-do-query (emsane-query-named-values "gimmename" :prompt "gimme name" :values '(("a" 1)("b" 2)) :default "a"))
;;(emsane-do-query (emsane-query-string "gimmestring" :prompt "gimme string" :values '("a" "b") :default "a"))
;;(emsane-do-query (emsane-query-paper-size "gimmesize" :prompt "gimme paper size" :values '(("a4"   (210 . 297))    ("a5"   (148 . 210)))))

(deftest emsanei-getters ()
  (let*
      ;;we need a clone since otherwise the orignal setting object will be modified
      ((settings (clone            emsane-the-section-defaults "test-settings")))
    ;;type a4
    (should (equal '(210 . 297) (emsane-get-size           settings)))
    ;;type 242
    (should (equal 242 (emsane-get-start-page     settings)))
    ;;type test
    (should (equal (emsane-scanner-get "test") (emsane-get-scanner  settings)))
    ;;type test RET duplex
    (should (equal "Automatic Document Feeder" (emsane-get-source settings)))
    ;;type test RET color
    (should (equal "Color" (emsane-get-mode           settings)))
    ;;type 242
    (should (equal 242 (emsane-get-resolution     settings)))
    ;;type img
    (should (equal "img" (emsane-get-file-pattern   settings)))
    ;;type djvu
    (should (equal 'djvu (emsane-get-image-type     settings)))
))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run non-interactive tests and make sure they actually ran.
(let ((window-configuration (current-window-configuration)))
  (let ((ert-test-body-was-run nil))
    ;; The buffer name chosen here should not compete with the default
    ;; results buffer name for completion in `switch-to-buffer'.
    (let ((stats (ert-run-tests-interactively "^emsane-" " *emsane self-tests*")))
      ;;(assert ert-test-body-was-run) ;;??
      (when (zerop (+ (ert-stats-passed-unexpected stats)
                      (ert-stats-failed-unexpected stats)
                      (ert-stats-error-unexpected stats)))
        ;; Hide results window only when everything went well.
        (set-window-configuration window-configuration)))))
