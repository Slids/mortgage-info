(defpackage #:mortgage-info
  (:use #:cl
        #:hunchentoot)
  (:local-nicknames
   (#:mf #:cl-protobufs.mortgage-forms))
  (:export #:start-server
           #:stop-server))

(in-package :mortgage-info)

(defun periodic-payment (loan-amount interest num-periods)
  (let* ((monthly-interest-rate (/ interest 12))
         (inverse-num-periods (- num-periods)))
    (/ (* loan-amount monthly-interest-rate)
       (- 1 (expt (+ 1 monthly-interest-rate)
                  inverse-num-periods)))))

(defun get-aproximate-monthly-dollars (monthly-amount)
  (/ (floor (* monthly-amount 100)) 100))

(defun get-monthly-interest (principal-remaining interest)
  (get-aproximate-monthly-dollars (* principal-remaining (/ interest 12))))

(defun populate-mortgage-info (loan-amount interest-percent num-periods)
  (let* ((interest (/ interest-percent 100))
         (%monthly-payment (periodic-payment loan-amount
                                             interest num-periods))
         (monthly-payment (get-aproximate-monthly-dollars %monthly-payment))
         amortization-lines
         (interest-paid 0)
         (principal-remaining loan-amount))
    (dotimes (i (1- num-periods))
      (let* ((period-interest (get-monthly-interest
                               principal-remaining interest))
             (ptp (- monthly-payment period-interest)))
        (setf interest-paid (+ interest-paid period-interest)
              principal-remaining (- principal-remaining ptp))
        (push (mf:make-amortization-line
               :period i
               :interest-charged period-interest
               :repayment-made monthly-payment
               :payment-toward-principal ptp
               :outstanding-principal principal-remaining)
              amortization-lines)))

    ;; Finally month treated differently due to rounding.
    (let* ((period-interest (get-monthly-interest
                             principal-remaining interest))
           (monthly-payment (+ period-interest
                               principal-remaining)))
      (setf interest-paid (+ interest-paid period-interest))
      (push (mf:make-amortization-line
             :period num-periods
             :interest-charged period-interest
             :repayment-made monthly-payment
             :payment-toward-principal principal-remaining
             :outstanding-principal 0)
            amortization-lines))

    (mf:make-mortgage-information
     :periodic-payment monthly-payment
     :total-interest interest-paid
     :total-paid (reduce #'+ amortization-lines :key #'mf:amortization-line.repayment-made)
     :repayment-information (nreverse amortization-lines))))

(setf *dispatch-table*
      (list #'dispatch-easy-handlers))

(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)

(setf *acceptor* nil)

(define-easy-handler (mortgage-info :uri "/mortgage-info") ()
  (let* ((request (raw-post-data))
         (request-type (cdr (assoc :content-type (headers-in *request*))))
         (request (cond ((string= request-type "application/json")
                         (let ((string-request
                                 (flexi-streams:octets-to-string request)))
                           (print string-request)
                           (cl-protobufs.json:parse-json
                            'mf:mortgage-information-request
                            :stream (make-string-input-stream string-request))))
                        (t
                         (print (raw-post-data))
                         (cl-protobufs:deserialize-from-stream
                          'mf:mortgage-information-request
                          :stream request))))
         (response (populate-mortgage-info
                    (mf:loan-amount request)
                    (mf:interest request)
                    (mf:num-periods request))))
    (cond ((string= request-type "application/json")
           (setf (hunchentoot:content-type*) "application/json")
           (let ((out-stream (make-string-output-stream)))
             (cl-protobufs.json:print-json response
                                           :stream out-stream)
             (get-output-stream-string out-stream)))
          (t
           (cl-protobufs:serialize-to-bytes response)))))

(defun stop-server ()
  (when *acceptor*
    (stop *acceptor*)))

(defun start-server ()
  (stop-server)
  (start (setf *acceptor*
               (make-instance 'easy-acceptor
                              :port 4242))))

;; (start-server)
