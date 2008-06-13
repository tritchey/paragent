;;;; Created on 2006-12-07 14:53:00

(in-package :adjutant)


#.(clsql:locally-enable-sql-reader-syntax)


(defvar *email-from-address* "support@paragent.com")
(defvar *email-server* "localhost")

(defun create-installers ()
  (with-db
      (start-sql-recording)
    (dolist (company (select 'company :refresh t :flatp t
			     :where [null [slot-value 'company 'msi]]))
      (handler-case
          (let* ((user (car (users company)))
                 (secret (secret company))
                 (name (name company))
                 (msi-maker (format nil "http://localhost:1234/create_beta_msi.php?company=~a&passcode=~a"
                                    (string-replace name " " "%20")
                                    (string-replace secret " " "%20")))
                 (target-dir (format nil "/lisp/repos/nexus/html/install/~a/" (remove #\/ (remove #\Space secret))))
                 (target-file (format nil "~aParagent.msi" target-dir))
                 (download-url (format nil "https://archon.paragent.com/install/~a/Paragent.msi"
                                       (remove #\/ (remove #\Space secret))))
                 (plain-body (format nil "
Welcome to Paragent.com!

A special installer has been created for your company, and can be downloaded at ~a . Simply install this on your computers and go to https://archon.paragent.com/login.ucw to begin managing your network.

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com
 
P.S.  In order to take full advantage of our free 30-day trial, please install Paragent.com agents today."
                              download-url))
		 (html-body (format nil "
<html>
<head>
<title></title>
<meta content=\"text/html;charset=iso-8859-1\" http-equiv=\"content-type\">
<style type=\"text/css\">
body {
color: #161715;
font-family: \"Myriad Pro\", \"Lucida Grande\", \"Helvetica Neue\", Calibri, Helvetica, Arial, sans-serif;
}
h2, h3 {
font-weight: normal;
}
a {
color: #5572BC;
font-weight: bold;
text-decoration: underline;
}
</style>
</head>
<body>
<table width=\"700\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" align=\"center\">
<tr>
<td colspan=\"5\"><img src=\"http://paragent.com/images/email_top.png\" alt=\"Paragent.com: Powerful + Easy To Use + Affordable\"></td>
</tr>
<tr valign=\"top\">
<td width=\"30\" bgcolor=\"#ffffff\">&nbsp;</td>
<td width=\"30\" bgcolor=\"#f4f4f4\">&nbsp;</td>
<td width=\"580\" bgcolor=\"#f4f4f4\" height=\"300\" style=\"line-height: 20px; font-size: 14px;\">
<h2>Welcome to Paragent.com!</h2>

<p>A special installer has been created for your company, and can be downloaded <a href=\"~a\">here</a>. Simply install this on your computers and go <a href=\"https://archon.paragent.com/login.ucw\">here</a> to begin managing your network.</p>
<p>Recommended Links:</p>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<p>Any Questions?</p>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<p>P.S. In order to take full advantage of our free 30-day trial, please install the agents and start using Paragent.com today.</p>
</td>
<td width=\"30\" bgcolor=\"#f4f4f4\">&nbsp;</td>
<td width=\"30\" bgcolor=\"#ffffff\">&nbsp;</td>
</tr>
<tr>
<td colspan=\"5\"><img src=\"http://paragent.com/images/email_bottom.png\" alt=\"paragent.com\"></td>
</tr>
</table>
</body>
</html>"
                              download-url)))
            (format t "Making directories~%")
            (ensure-directories-exist target-dir)
            (format t "Downloading msi from ~a to ~a~%" msi-maker target-file)
            (let* ((process (sb-ext:run-program "/usr/bin/curl" (list msi-maker "-o" target-file) 
                                                :wait t :output t))
                   (exit-code (sb-ext:process-exit-code process)))
              (format t "return code: ~a~%" exit-code)
              (when (equal exit-code 0)
                (let ((size (with-open-file (file target-file) (file-length file))))
                  (when (>= size 1000000)
                    (setf (msi company) download-url)
                    (update-records-from-instance company)
                    (format t "Sending email~%")
                    (when user
                      (cl-smtp:send-email *email-server*
                                          *email-from-address*
                                          (list (email user))
                                          "Welcome to Your Paragent.com 30-day Trial"
                                          plain-body
					  :port 25
                                          :authentication nil
					  :html-message html-body
                                          :buffer-size 1)))))))
          (simple-error ()
	    (format t "Error occurred trying to send email for ~a" (name company)))
          (sb-bsd-sockets:host-not-found-error ()
	    (format t "Error occurred trying to send email for ~a" (name company)))))))

(defun string-replace (str1 sub1 sub2)
  "Handy function to replace all instances of a given substring with a new string"
  (if (string= str1 "")
      ""
      (let ((str1 (string str1))
            (str2 "")
            (sub1 (string sub1))
            (sub2 (string sub2))
            (index1 0))
        (loop
          (if (string-equal str1 sub1
                            :start1 index1
                            :end1 (min (length str1)
                                       (+ index1 (length sub1))))
              (progn
                (setq str2 (concatenate 'string str2 sub2))
                (incf index1 (length sub1)))
              (progn
                (setq str2 (concatenate 'string
                                        str2
                                        (subseq str1 index1 (1+ index1))))
                (incf index1)))
          (unless (< index1 (length str1))
            (return str2))))))

#.(clsql:restore-sql-reader-syntax-state)
