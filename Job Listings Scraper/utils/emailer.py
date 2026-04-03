"""
Email notification module — sends weekly job digest via SMTP (Gmail).
"""

import logging
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

from config import EMAIL_CONFIG

logger = logging.getLogger(__name__)


def send_job_digest(scored_jobs: list, dry_run: bool = False) -> bool:
    """
    Send a formatted HTML email with the ranked job listings.

    Args:
        scored_jobs: List of ScoredJob objects from the matcher.
        dry_run: If True, print the email to console instead of sending.

    Returns:
        True if email sent (or printed) successfully.
    """
    if not scored_jobs:
        logger.info("No new jobs to send — skipping email.")
        return False

    subject = f"🎯 {len(scored_jobs)} New Data Science Jobs This Week"
    html_body = _build_html(scored_jobs)

    if dry_run:
        print("\n" + "=" * 70)
        print(f"SUBJECT: {subject}")
        print("=" * 70)
        print(html_body[:3000])
        print("... (truncated for dry-run)")
        return True

    return _send_smtp(subject, html_body)


def _send_smtp(subject: str, html_body: str) -> bool:
    """Send email via Gmail SMTP."""
    cfg = EMAIL_CONFIG

    msg = MIMEMultipart("alternative")
    msg["Subject"] = subject
    msg["From"] = cfg["sender_email"]
    msg["To"] = cfg["recipient_email"]
    msg.attach(MIMEText(html_body, "html", "utf-8"))

    try:
        print(f"\n[EMAIL] Connecting to {cfg['smtp_server']}:{cfg['smtp_port']}...")
        with smtplib.SMTP(cfg["smtp_server"], cfg["smtp_port"]) as server:
            server.ehlo()
            print("[EMAIL] Starting TLS...")
            server.starttls()
            server.ehlo()
            print(f"[EMAIL] Logging in as {cfg['sender_email']}...")
            server.login(cfg["sender_email"], cfg["sender_password"])
            print(f"[EMAIL] Sending to {cfg['recipient_email']}...")
            server.send_message(msg)

        print("[EMAIL] SUCCESS — email sent!")
        logger.info("Email sent to %s.", cfg["recipient_email"])
        return True
    except Exception as exc:
        print(f"\n[EMAIL] FAILED: {type(exc).__name__}: {exc}")
        logger.error("Failed to send email: %s", exc)
        return False


def _build_html(scored_jobs: list) -> str:
    """Build a clean HTML email body from scored jobs."""
    rows = ""
    for i, sj in enumerate(scored_jobs, 1):
        j = sj.listing
        work_badge = {
            "great": "🟢 Remote",
            "good": "🟡 Hybrid",
            "acceptable": "🟠 On-site",
            "poor": "🔴 5-day office",
            "unknown": "⚪ Unknown",
        }.get(sj.work_mode_rating, "⚪ Unknown")

        benefits_str = ", ".join(sj.benefits[:6]) if sj.benefits else "Not specified"
        reasons_str = "<br>".join(f"✅ {r}" for r in sj.match_reasons)
        penalties_str = "<br>".join(f"⚠️ {r}" for r in sj.penalty_reasons) if sj.penalty_reasons else ""

        stock_badge = "📈 Yes" if sj.has_stock else "—"

        rows += f"""
        <tr style="border-bottom: 2px solid #eee;">
            <td style="padding: 16px; vertical-align: top;">
                <h2 style="margin: 0 0 4px 0; color: #1a1a2e;">#{i} — {_esc(j.title)}</h2>
                <p style="margin: 0 0 8px 0; color: #555; font-size: 14px;">
                    <strong>{_esc(j.company)}</strong> &nbsp;|&nbsp; {_esc(j.location)} &nbsp;|&nbsp; {work_badge}
                </p>

                <table style="width: 100%; font-size: 13px; margin-bottom: 10px;">
                    <tr>
                        <td style="width: 50%; padding: 4px 0;"><strong>Match Score:</strong>
                            <span style="color: {'#27ae60' if sj.match_score >= 90 else '#f39c12'}; font-weight: bold; font-size: 16px;">
                                {sj.match_score}%
                            </span>
                        </td>
                        <td style="width: 50%; padding: 4px 0;"><strong>Stock/Equity:</strong> {stock_badge}</td>
                    </tr>
                    <tr>
                        <td style="padding: 4px 0;"><strong>Base Salary:</strong> {_esc(sj.predicted_salary)}</td>
                        <td style="padding: 4px 0;"><strong>Total Comp (est.):</strong> {_esc(sj.predicted_total_comp)}</td>
                    </tr>
                    <tr>
                        <td colspan="2" style="padding: 4px 0;"><strong>Benefits:</strong> {_esc(benefits_str)}</td>
                    </tr>
                </table>

                <div style="background: #f8f9fa; padding: 10px; border-radius: 6px; font-size: 12px; margin-bottom: 8px;">
                    <strong>Why you're a fit:</strong><br>{reasons_str}
                    {f'<br><br><strong>Notes:</strong><br>{penalties_str}' if penalties_str else ''}
                </div>

                <a href="{_esc(j.url)}" style="display: inline-block; padding: 8px 20px; background: #1a1a2e; color: #fff;
                   text-decoration: none; border-radius: 4px; font-size: 13px;">Apply Now →</a>
            </td>
        </tr>
        """

    return f"""
    <!DOCTYPE html>
    <html>
    <head><meta charset="utf-8"></head>
    <body style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                 max-width: 700px; margin: 0 auto; background: #fff; color: #333;">
        <div style="background: #1a1a2e; padding: 24px; text-align: center;">
            <h1 style="color: #fff; margin: 0; font-size: 22px;">Weekly Data Science Job Digest</h1>
            <p style="color: #b8b8d0; margin: 6px 0 0 0; font-size: 13px;">
                {len(scored_jobs)} new listings matching your profile (≥85% fit) — London & Remote
            </p>
        </div>

        <table style="width: 100%; border-collapse: collapse;">
            {rows}
        </table>

        <div style="padding: 16px; text-align: center; color: #999; font-size: 11px;">
            Generated by Job Listings Scraper · Deduplication enabled · Only new roles shown
        </div>
    </body>
    </html>
    """


def _esc(text: str) -> str:
    """HTML escaping with special character cleanup."""
    # Replace non-breaking spaces and other problematic characters
    text = text.replace("\xa0", " ").replace("\u200b", "")
    return (
        text.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )
