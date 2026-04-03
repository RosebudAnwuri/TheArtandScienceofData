"""
Email notification module - sends weekly job digest via SMTP (Gmail).
"""

import logging
import smtplib
import traceback

from config import EMAIL_CONFIG

logger = logging.getLogger(__name__)


def _to_ascii(text):
    """Force anything to pure ASCII string."""
    if not isinstance(text, str):
        text = str(text)
    return text.encode("ascii", errors="replace").decode("ascii")


def send_job_digest(scored_jobs, dry_run=False):
    if not scored_jobs:
        print("[EMAIL] No new jobs to send.")
        return False

    subject = _to_ascii(str(len(scored_jobs)) + " New Data Science Jobs This Week")
    html_body = _to_ascii(_build_html(scored_jobs))

    if dry_run:
        print("\n" + "=" * 70)
        print("SUBJECT: " + subject)
        print("=" * 70)
        print(html_body[:3000])
        print("... (truncated for dry-run)")
        return True

    return _send_smtp(subject, html_body)


def _send_smtp(subject, html_body):
    """Send email via Gmail SMTP."""
    cfg = EMAIL_CONFIG
    sender = _to_ascii(cfg["sender_email"])
    recipient = _to_ascii(cfg["recipient_email"])
    password = cfg["sender_password"]
    smtp_server = _to_ascii(cfg["smtp_server"])
    smtp_port = cfg["smtp_port"]

    # Build email as bytes from the start
    header = (
        "From: " + sender + "\r\n"
        "To: " + recipient + "\r\n"
        "Subject: " + subject + "\r\n"
        "MIME-Version: 1.0\r\n"
        "Content-Type: text/html; charset=us-ascii\r\n"
        "\r\n"
    )
    raw_email_str = header + html_body
    raw_email_bytes = raw_email_str.encode("ascii", "replace")

    try:
        print("[EMAIL] Step 1: Connecting to " + smtp_server + "...")
        server = smtplib.SMTP(smtp_server, smtp_port)

        print("[EMAIL] Step 2: EHLO...")
        server.ehlo()

        print("[EMAIL] Step 3: Starting TLS...")
        server.starttls()
        server.ehlo()

        print("[EMAIL] Step 4: Logging in as " + sender + "...")
        server.login(sender, password)

        print("[EMAIL] Step 5: Sending email to " + recipient + "...")
        server.sendmail(sender, [recipient], raw_email_bytes)

        print("[EMAIL] Step 6: Closing connection...")
        server.quit()

        print("[EMAIL] SUCCESS -- email sent!")
        return True

    except Exception as exc:
        print("\n[EMAIL] FAILED at the step above.")
        print("[EMAIL] Error type: " + str(type(exc).__name__))
        print("[EMAIL] Error message: " + _to_ascii(str(exc)))
        print("[EMAIL] Full traceback:")
        traceback.print_exc()
        return False


def _build_html(scored_jobs):
    """Build a clean HTML email body from scored jobs."""
    rows = ""
    for i, sj in enumerate(scored_jobs, 1):
        j = sj.listing

        work_modes = {
            "great": "[REMOTE]",
            "good": "[HYBRID]",
            "acceptable": "[ON-SITE]",
            "poor": "[5-DAY OFFICE]",
            "unknown": "[UNKNOWN]",
        }
        work_badge = work_modes.get(sj.work_mode_rating, "[UNKNOWN]")

        benefits_list = sj.benefits[:6] if sj.benefits else []
        benefits_str = ", ".join(_esc(b) for b in benefits_list) if benefits_list else "Not specified"

        reasons_parts = []
        for r in sj.match_reasons:
            reasons_parts.append("- " + _esc(r))
        reasons_str = "<br>".join(reasons_parts)

        penalties_parts = []
        for r in sj.penalty_reasons:
            penalties_parts.append("* " + _esc(r))
        penalties_str = "<br>".join(penalties_parts)

        stock_badge = "Yes" if sj.has_stock else "No"
        if sj.match_score >= 90:
            score_color = "#27ae60"
        else:
            score_color = "#f39c12"

        title = _esc(j.title)
        company = _esc(j.company)
        location = _esc(j.location)
        salary = _esc(sj.predicted_salary)
        total_comp = _esc(sj.predicted_total_comp)
        url = _esc(j.url)

        notes_html = ""
        if penalties_str:
            notes_html = "<br><br><strong>Notes:</strong><br>" + penalties_str

        rows += (
            '<tr style="border-bottom: 2px solid #eee;">'
            '<td style="padding: 16px; vertical-align: top;">'
            '<h2 style="margin: 0 0 4px 0; color: #1a1a2e;">#' + str(i) + " -- " + title + "</h2>"
            '<p style="margin: 0 0 8px 0; color: #555; font-size: 14px;">'
            "<strong>" + company + "</strong> | " + location + " | " + work_badge
            + "</p>"
            '<table style="width: 100%; font-size: 13px; margin-bottom: 10px;">'
            "<tr>"
            '<td style="width: 50%; padding: 4px 0;"><strong>Match Score:</strong> '
            '<span style="color: ' + score_color + '; font-weight: bold; font-size: 16px;">'
            + str(sj.match_score) + "%</span></td>"
            '<td style="width: 50%; padding: 4px 0;"><strong>Stock/Equity:</strong> '
            + stock_badge + "</td>"
            "</tr><tr>"
            '<td style="padding: 4px 0;"><strong>Base Salary:</strong> ' + salary + "</td>"
            '<td style="padding: 4px 0;"><strong>Total Comp (est.):</strong> ' + total_comp + "</td>"
            "</tr><tr>"
            '<td colspan="2" style="padding: 4px 0;"><strong>Benefits:</strong> '
            + benefits_str + "</td>"
            "</tr></table>"
            '<div style="background: #f8f9fa; padding: 10px; border-radius: 6px; font-size: 12px; margin-bottom: 8px;">'
            "<strong>Why you are a fit:</strong><br>" + reasons_str
            + notes_html
            + "</div>"
            '<a href="' + url + '" style="display: inline-block; padding: 8px 20px; background: #1a1a2e; color: #fff;'
            ' text-decoration: none; border-radius: 4px; font-size: 13px;">Apply Now</a>'
            "</td></tr>"
        )

    html = (
        "<!DOCTYPE html><html><head><meta charset='utf-8'></head>"
        '<body style="font-family: Arial, sans-serif; max-width: 700px; margin: 0 auto; background: #fff; color: #333;">'
        '<div style="background: #1a1a2e; padding: 24px; text-align: center;">'
        '<h1 style="color: #fff; margin: 0; font-size: 22px;">Weekly Data Science Job Digest</h1>'
        '<p style="color: #b8b8d0; margin: 6px 0 0 0; font-size: 13px;">'
        + str(len(scored_jobs)) + " new listings matching your profile -- London and Remote"
        + "</p></div>"
        '<table style="width: 100%; border-collapse: collapse;">'
        + rows
        + "</table>"
        '<div style="padding: 16px; text-align: center; color: #999; font-size: 11px;">'
        "Generated by Job Listings Scraper -- Deduplication enabled -- Only new roles shown"
        "</div></body></html>"
    )
    return html


def _esc(text):
    """HTML escaping with full ASCII cleanup."""
    if not isinstance(text, str):
        text = str(text)
    text = text.encode("ascii", errors="replace").decode("ascii")
    text = text.replace("&", "&amp;")
    text = text.replace("<", "&lt;")
    text = text.replace(">", "&gt;")
    text = text.replace('"', "&quot;")
    return text
