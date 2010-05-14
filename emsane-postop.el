;; (C) FSF 2010, Joakim Verona, part of the emsane package

;;I want some form of process queue because:
;; - tramp gets swamped with lots of async calls
;; - some postops are slow, and hundreds of them cant be expected to run in parallell
;; emsane was used for a long time withouth a queue, but it put constraints on what could be done

;; it wouldve been nice if i didnt have to write it, but i didnt find anything.
;; i fully expect to be pointed to something glaringly obviously already existing once
;; i publish the code.

;; TODO I want scan-job postprocessing also, for book-level ocr for instance(with ocropus)


;;;tentative stuff

;; - operation  base class
;; - transaction class(a list of operations)
;; - queue class(a list of transactions, a result slot)

;;when a image is finished scanned, a new transaction is created, and the image pushed on the transaction result slot
;;the section definition pushes a bunch of ops on the tx
;;maybe someone else also pushes a op(the job? scanner?)
;;the tx is pushed on the queue
;;the queue is event driven, these are events:
;; - pushing a tx on the queue
;; - an emacs op finishes
;; - a shell op finishes
;;if any op ina tx fails, the entire tx fails, otoh other txes are unafected
;;the workdir is set on the post op buffer, so will work with tramp
;;there is a set workdir op, so different scan jobs wont trahs each other


;;its possible to define many queues,
;;a queue is connected to 1 postop buffer

;;transactions are independent, so they could in principle be executed in parallell
;;however, a queue will only do transactions in sequence
;;futureish is supporting several queues, then pushing transactions round robin on them

;;emsane-postop-next-op pops an op from the current tx, and executes it
;;emsane-postop-push-op pushes op on tx

;;an op ought to be able to provide environment modifiers, such as changing the flags for the scanner
;; the use-case for this is for instance a dust-detector that needs the scanner to scan a little bit more than
;; the actual document. the op will then split the img in 2 parts, one actual image, and one part used for dust detection.

(provide 'emsane-postop)

(defclass emsane-postop-operation ()
  ()
  "base class for post operations for image scans")

(defclass emsane-postop-lisp-operation (emsane-postop-operation)
  ((operation-lambda :initarg :operation-lambda
                     :documentation "a lambda of the form (lambda (transaction) (do-something-with-transaction))"))
  "A lisp image file operation. for instance for renaming files.")

(defclass emsane-postop-simple-shell-operation (emsane-postop-operation)
  ((operation-shell-command :initarg :operation-shell-command
                            :documentation "a simple string to be evaluated by a shell"))
  "a simple file operation done with a shell command")

(defclass emsane-postop-lifo ()
  ((lifo :initarg :lifo
         :initform '()
         :documentation "a lifo"))
  "base class for queue and transaction")

(defclass emsane-postop-queue (emsane-postop-lifo)
  ((process-buffer :initarg :process-buffer)
   (continue-go-loop :initarg  :continue-go-loop :initform t
                     :documentation "flag setable by subprocess, to indicate continuation")
   (default-directory :initarg :default-directory :initform default-directory
     :documentation "subproces default dir")
   (state :initarg :state :initform nil
          :documentation  "nil if ok, otherwise an object indicating some error")
   (error-queue :initarg :error-queue :initform nil  :documentation "transactions who failed gets pushed here")
   (current-tx :initarg :current-tx :initform nil)
   (current-op :initarg :current-op :initform nil)
   (error-hooks :initarg :error-hooks :initform nil
                :documentation "hooks to run in the event of a transaction error"))
  "a list of transactions to be performed")

(defmethod emsane-postop-exec ((this emsane-postop-lisp-operation) tx q)
  "execute lisp operation"
  (let*
      ((default-directory  (oref q default-directory)))
    (condition-case lispop-error
        (funcall (oref this :operation-lambda) tx q)
      (error (emsane-postop-signal-error q lispop-error)))
    ))

(defmethod emsane-postop-exec ((this emsane-postop-simple-shell-operation) tx q)
  "execute shell operation"
  (let*
      ((default-directory  (oref q default-directory))
       (cmd (oref this operation-shell-command))
       (proc-buf  (oref q :process-buffer))
       (process-environment (emsane-plist2env (oref tx :environment)))
       (post-process (start-file-process-shell-command
                      "postop"
                      proc-buf
                      cmd
                      )))
    (set-process-sentinel post-process 'emsane-postop-sentinel)
    (process-put post-process 'queue q)
    (with-current-buffer proc-buf
      (insert (format "postprocessing:%s env:%s\n" cmd (emsane-plist2env (oref tx environment))))
      )
    (oset q :continue-go-loop nil)))

(defun emsane-plist2env (plist)
  "convert a plist to the env var format used by process-environment"
  (let*
      ((env '()))
    (while plist
      (setq env (append env (list (format "%s=%s" (first plist) (second plist)))))
      (setq plist (cdr (cdr plist))))
    env))

(defun emsane-postop-sentinel (process result)
  "called when an image shell postop finishes"
  (let*
      ((queue (process-get process 'queue)))
    (unless (= 0 (process-exit-status process))
      (emsane-postop-signal-error queue result))
    (emsane-postop-finish-shell-operation queue)
    (emsane-postop-go queue);;now continue processing queue transations
    ))

(defmethod emsane-postop-signal-error ((this emsane-postop-queue) result)
  "error handler"
  ;;TODO better error handler
  ;;there are levels of errors:
  ;; - tx killers, move the tx to an error queue, other tx:es arent affected
  ;; - queue killers, inhibit further queue opperations, stop scanning!
  ;;TODO call hooks, client should know about error(shut down scanner processes in this case)
  ;;(oset this :state result) ;;TODO "state" is currently used as a queue-killer, which doesnt happen atm

  ;;the case below is "tx killer", push the broken tx on an error queue for later examination, queue chugs on as usual
  (emsane-postop-push (oref this :current-tx) (oref this :current-op))
  (emsane-postop-push (oref this :error-queue) (oref this :current-tx))
  (mapc #'funcall (oref this :error-hooks));;using run-hooks turned out not so good here
  (with-current-buffer (oref this :process-buffer)
    (insert (format "Non 0 return code from postop. This is bad.  result:%s" result)))
  (message (format "Non 0 return code from postop. This is bad.  result:%s" result))
  (emsane-postop-go this)
  )

(defmethod emsane-postop-push ((this emsane-postop-lifo) object)
  "push object on lifo"
  (oset this :lifo
        (cons object   (oref this :lifo))))

(defmethod emsane-postop-dequeue ((this emsane-postop-lifo))
  "pop object from end of queue"
  (unless (emsane-postop-hasnext this) (error "poping an empty queue is bad"))
  (let
      ((rv (car (last (oref this :lifo)))))
    (oset this :lifo
          (nreverse (cdr (nreverse (oref this :lifo))))) ;;TODO
    rv))

(defmethod emsane-postop-hasnext ((this emsane-postop-lifo))
  "empty?"
  (not (null (oref this :lifo))))

(defclass emsane-postop-transaction (emsane-postop-lifo)
  ((environment :initarg :environment
                :initform nil
                :documentation "transaction environment variables."))
  "a list of operations that must be performed together. contains environment operations can access")

(defmethod  emsane-postop-getenv ((this  emsane-postop-transaction) varname)
  (plist-get (oref this environment) varname))

(defmethod  emsane-postop-setenv ((this  emsane-postop-transaction) varname value)
  (oset this environment (plist-put (oref this environment) varname value)))

(defmethod emsane-postop-finish-shell-operation ((this emsane-postop-queue))
  "finishup an ongoing shell operatio"
  ;;TODO hmm is this all?
  (oset this :continue-go-loop t))

(defmethod emsane-postop-donext ((this emsane-postop-queue))
  "pops an operation from the current transaction in the queue and executes it.
continue with the 1st op of the next transaction if the current transaction is finished.
if the queue is empty return nil."
  ;;TODO the method should always be safe to call, regardless of the queue state, ensure this
  ;;TODO delete the transaction if the operation fails.
  ;;should almost work, because if crash, we dont push back th eop
  (if (oref this state) (error "the queue is unwell:%s" (oref this state)))
  (if (emsane-postop-hasnext this)
      (let*
          ((tx (emsane-postop-dequeue this))
           (op))
        (oset this :current-tx tx)
        (if (emsane-postop-hasnext tx)
            (progn
              (setq op  (emsane-postop-dequeue tx))
              (oset this :current-op op)
              (emsane-postop-exec op tx this)
              (emsane-postop-push this tx))
          (emsane-postop-donext this)))))

(defmethod emsane-postop-go ((this emsane-postop-queue))
  "start or continue executing transactions in queue."
  (if (oref this state) (error "the queue is unwell:%s" (oref this state)))
  (let
      ((continue-go-loop t))
    (while (and continue-go-loop (emsane-postop-hasnext this))
      (emsane-postop-donext this)
      (setq continue-go-loop (oref this :continue-go-loop)))))
