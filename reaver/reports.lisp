#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#



(in-package :reaver)

#.(clsql:locally-enable-sql-reader-syntax)

(defclass trial-report ()
  ((company :initarg :company
	    :accessor company
	    :initform nil)))


(defgeneric report-title (report))

(defgeneric report-plain-body (report))

(defgeneric report-html-body (report))

(defclass trial-email-no-agents (trial-report)
  ((discount-code :accessor discount-code
		 :initarg :discount-code
		 :initform "")))

(defmethod report-title ((report trial-email-no-agents))
  "30-day Trial Account from Paragent.com")

(defmethod report-plain-body ((report trial-email-no-agents))
  (format nil
  "To experience the power of Paragent.com during the 30-day trial, Paragent recommends you install at least 10 agents.

Get 25% off your Paragent.com subscription if you sign up in the next 10 days. Use the discount code ~a.

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

As a reminder:

* A special installer was created for your organization, and can be downloaded here: <~a>.
* Simply install the agent on the systems you want to manage and then go here: <https://archon.paragent.com/login.ucw> to begin managing your network.

Forgot your Paragent.com login?

* Your email address is your login username.
* You can reset your password here: <https://archon.paragent.com/login.ucw>

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com"
  (discount-code report)
  (when (company report)
    (msi (company report)))))

(defmethod report-html-body ((report trial-email-no-agents))
  (format nil
  "<p>To experience the power of Paragent.com during the 30-day trial, Paragent
recommends you install at least 10 agents.</p>
<p>Get 25% off your Paragent.com subscription if you sign up in the next 10 days. Use the discount code ~a.</p>
<h3>As a reminder:</h3>
<ul>
<li>A special installer was created for your organization, and can be downloaded <a href=\"~a\">here</a>.</li>
<li>Simply install the agent on the systems you want to manage and then go <a href=\"https://archon.paragent.com/login.ucw\">here</a> to begin managing your network.</li>
</ul>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<h3>Forgot your Paragent.com login?</h3>
<ul>
<li>Your email address is your login username.</li>
<li>You can reset your password <a href=\"https://archon.paragent.com/login.ucw\">here</a>.</li>
</ul>
<h3>Recommended Links:</h3>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<h3>Any Questions?</h3>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>"
  (discount-code report)
  (when (company report)
    (msi (company report)))))

(defclass trial-email-with-agents (trial-report)
  ((discount-code :accessor discount-code
		 :initarg :discount-code
		 :initform "")))

(defmethod report-title ((report trial-email-with-agents))
  "Save on your 30-day Paragent.com trial account")

(defmethod report-plain-body ((report trial-email-with-agents))
  (format nil
  "We hope you are enjoying your 30-day Paragent.com trial. 

Get 25% off your Paragent.com subscription if you sign up in the next 10 days. Use the discount code ~a.

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

Forgot your Paragent.com login?

* Your email address is your login username.
* You can reset your password here: <https://archon.paragent.com/login.ucw>

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com"
  (discount-code report)))

(defmethod report-html-body ((report trial-email-with-agents))
  (format nil
  "<p>We hope you are enjoying your 30-day Paragent.com trial.</p>
<p>Get 25% off your Paragent.com subscription if you sign up in the next 10 days. Use the discount code ~a.</p>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<h3>Forgot your Paragent.com login?</h3>
<ul>
<li>Your email address is your login username.</li>
<li>You can reset your password <a href=\"https://archon.paragent.com/login.ucw\">here</a>.</li>
</ul>
<h3>Recommended Links:</h3>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<h3>Any Questions?</h3>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>"
  (discount-code report)))

(defclass trial-email-inventory (trial-report)
  ())

(defmethod report-title ((report trial-email-inventory))
  "20 Days left in your Paragent.com trial")

(defmethod report-plain-body ((report trial-email-inventory))
  "20 Days left in your Paragent.com trial.

Paragent hopes to find you enjoying your 30-day trial account with us. It has been our experience that customers overlook a few cool features during the trial period. Here is a short three-minute video that will bring you up to speed on some basic Paragent.com features.

<http://paragent.com/videos/inventory.html>

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

Forgot your Paragent.com login?

* Your email address is your login username.
* You can reset your password here: <https://archon.paragent.com/login.ucw>

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com")

(defmethod report-html-body ((report trial-email-inventory))
  "<p>20 Days left in your Paragent.com trial.</p>
<p>Paragent hopes to find you enjoying your 30-day trial account with us. It has been our experience that customers overlook a few cool features during the trial period. Here is a short three-minute video that will bring you up to speed on some basic Paragent.com features.</p>

<h2><a href=\"http://paragent.com/videos/inventory.html\">Click Here to Watch Inventory &amp; Alerting Video</a></h2>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<p>Forgot your Paragent.com login?</p>
<ul>
<li>Your email address is your login username.</li>
<li>You can reset your password <a href=\"https://archon.paragent.com/login.ucw\">here</a>.</li>
</ul>
<h3>Recommended Links:</h3>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<h3>Any Questions?</h3>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>")

(defclass trial-email-search (trial-report)
  ())

(defmethod report-title ((report trial-email-search))
  "15 Days left in your Paragent.com trial")

(defmethod report-plain-body ((report trial-email-search))
  "15 Days left in your Paragent.com trial.

Can you believe it? You are half way through your 30-day trial with Paragent.com. What have you been doing with all the additional information Paragent.com is providing? If you installed the agents, you now have a lot of information readily available. To better understand your options, here is a short three-minute video on Paragent.com's reporting and searching functions.

<http://paragent.com/videos/search.html>

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

Forgot your Paragent.com login?

* Your email address is your login username.
* You can reset your password here: <https://archon.paragent.com/login.ucw>

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com")

(defmethod report-html-body ((report trial-email-search))
  "<p>15 Days left in your Paragent.com trial.</p>
<p>Can you believe it? You are half way through your 30-day trial with Paragent.com. What have you been doing with all the additional information Paragent.com is providing? If you installed the agents, you now have a lot of information readily available. To better understand your options, here is a short three-minute video on Paragent.com&rsquo;s reporting and searching functions.</p>

<h2><a href=\"http://paragent.com/videos/search.html\">Click Here to Watch Reporting &amp; Search Video</a></h2>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<h3>Forgot your Paragent.com login?</h3>
<ul>
<li>Your email address is your login username.</li>
<li>You can reset your password <a href=\"https://archon.paragent.com/login.ucw\">here</a>.</li>
</ul>
<h3>Recommended Links:</h3>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<h3>Any Questions?</h3>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>")

(defclass trial-email-remote (trial-report)
  ())

(defmethod report-title ((report trial-email-remote))
  "10 Days left in your Paragent.com trial")

(defmethod report-plain-body ((report trial-email-remote))
  "10 Days left in your Paragent.com trial.

How is you Paragent.com trial going? Typically, trial customers have seen most of the basic features but now. If you have not tried out Remote Desktop or Helpdesk, here is a short three-minute video to help better understand how Paragent.com can improve your business productivity.

<http://paragent.com/videos/remote.html>

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

Forgot your Paragent.com login?

* Your email address is your login username.
* You can reset your password here: <https://archon.paragent.com/login.ucw>

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com")

(defmethod report-html-body ((report trial-email-remote))
  "<p>10 Days left in your Paragent.com trial.</p>
<p>How is you Paragent.com trial going? Typically, trial customers have seen most of the basic features but now. If you have not tried out <a href=\"http://paragent.com/details/remote.html\">Remote Desktop</a> or <a href=\"http://paragent.com/details/ticketing.html\">Helpdesk</a>, here is a short three-minute video to help better understand how Paragent.com can improve your business productivity.</p>
<h2><a href=\"http://paragent.com/videos/remote.html\">Click Here to Watch Remote Desktop &amp; Helpdesk Video</a></h2>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<h3>Forgot your Paragent.com login?</h3>
<ul>
<li>Your email address is your login username.</li>
<li>You can reset your password <a href=\"https://archon.paragent.com/login.ucw\">here</a>.</li>
</ul>
<h3>Recommended Links:</h3>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<h3>Any Questions?</h3>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>")

(defclass trial-email-final (trial-report)
  ())

(defmethod report-title ((report trial-email-final))
  "Only 2 Days left in your Paragent.com trial")

(defmethod report-plain-body ((report trial-email-final))
  "Only 2 Days left in your Paragent.com trial.

We hope you have enjoyed your 30-day trial account with Paragent. Here is short two-minute video explaining the process of how your organization can continue to use Paragent's software.

<http://paragent.com/videos/closing.html>

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

Forgot your Paragent.com login?

* Your email address is your login username.
* You can reset your password here: <https://archon.paragent.com/login.ucw>

Recommended Links:

* Quick Start Guide: <http://paragent.com/support/quickstart.html>
* Video review - Installing the Paragent.com Agent: <http://paragent.com/videos/install.html>
* Step-by-step instructions for pushing out the agent using Active Directory: <http://paragent.com/support/ad.html>

Any Questions?

* Frequently Asked Questions: <http://paragent.com/support/faq.html>
* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com")

(defmethod report-html-body ((report trial-email-final))
  "<p>Only 2 Days left in your Paragent.com trial.</p>
<p>We hope you have been enjoying your 30-day trial account with Paragent.com. Here is a short two-minute video explaining the process of how your organization can continue to use Paragent.com&rsquo;s software.</p>

<h2><a href=\"http://paragent.com/videos/closing.html\">Click Here to Watch Signup Video</a></h2>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>
<h3>Forgot your Paragent.com login?</h3>
<ul>
<li>Your email address is your login username.</li>
<li>You can reset your password <a href=\"https://archon.paragent.com/login.ucw\">here</a>.</li>
</ul>
<h3>Recommended Links:</h3>
<ul>
<li><a href=\"http://paragent.com/support/quickstart.html\">Quick start guide</a> to installing Paragent.com</li>
<li>Video review&mdash;<a href=\"http://paragent.com/videos/install.html\">Installing the Paragent.com Agent</a></li>
<li><a href=\"http://paragent.com/support/ad.html\">Step-by-step instructions for pushing out the agent using Active Directory</a></li>
</ul>
<h3>Any Questions?</h3>
<ul>
<li><a href=\"http://paragent.com/support/faq.html\">Frequently Asked Questions</a></li>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>")

(defclass trial-email-close (trial-report)
  ())

(defmethod report-title ((report trial-email-close))
  "Your Paragent.com 30-day trial has ended.")

(defmethod report-plain-body ((report trial-email-close))
  "Thank you for trying Paragent.com. Your 30-day trial has come to an end, and your account has been disabled. If you would like to continue using Paragent.com, please contact our sales team at sales@paragent.com. We appreciate your interest, and if you have any questions in the future, please let us know.

Sincerely,
The Paragent.com Team
<http://www.paragent.com>
1-800-839-9625

Any Questions?

* Support related questions - email us at support@paragent.com
* Sales related questions - email us at sales@paragent.com")

(defmethod report-html-body ((report trial-email-close))
  "<p>Thank you for trying Paragent.com. Your 30-day trial has come to an end, and your account has been disabled. If you would like to continue using Paragent.com, please contact our sales team at sales@paragent.com. We appreciate your interest, and if you have any questions in the future, please let us know.</p>
<p>Sincerely,<br />
The Paragent.com Team<br />
<a href=\"http://www.paragent.com\">www.paragent.com</a><br />
1-800-839-9625</p>

<h3>Any Questions?</h3>

<ul>
<li>Support related questions&mdash;email us at <a href=\"mailto:support@paragent.com\">support@paragent.com</a></li>
<li>Sales related questions&mdash;email us at <a href=\"mailto:sales@paragent.com\">sales@paragent.com</a></li>
</ul>")


(defmethod report-html-body :around ((report trial-report))
  (format nil 
  "<html>
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
<td colspan=\"5\"><img src=\"http://paragent.com/images/email_top.png\" alt=\"Paragent.com: Powerful + Easy to use + Affordable\"></td>
</tr>
<tr valign=\"top\">
<td width=\"30\" bgcolor=\"#ffffff\">&nbsp;</td>
<td width=\"30\" bgcolor=\"#f4f4f4\">&nbsp;</td>
<td width=\"580\" bgcolor=\"#f4f4f4\" height=\"300\" style=\"line-height: 20px; font-size: 14px;\">
~a
</td>
<td width=\"30\" bgcolor=\"#f4f4f4\">&nbsp;</td>
<td width=\"30\" bgcolor=\"#ffffff\">&nbsp;</td>
</tr>
<tr>
<td colspan=\"5\"><img src=\"http://paragent.com/images/email_bottom.png\" alt=\"paragent.com\"></td>
</tr>
</table>
</body>
</html>" (call-next-method)))


#.(clsql:restore-sql-reader-syntax-state)
