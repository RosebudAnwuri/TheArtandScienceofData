#!/usr/bin/env python3
"""
Job Listings Scraper — Main Orchestrator

Scrapes data science roles from top tech companies in London,
scores them against your profile, deduplicates, and sends a
weekly email digest.

Usage:
    # One-off run (dry run — prints to console, no email)
    python main.py --dry-run

    # One-off run (sends email)
    python main.py --run-once

    # Start the weekly scheduler
    python main.py --schedule
"""

import argparse
import logging
import time

import schedule

from config import MIN_MATCH_SCORE, SCHEDULE_DAY, SCHEDULE_TIME, SENT_JOBS_DB
from matcher import rank_jobs
from scrapers.adzuna import AdzunaScraper
from scrapers.jsearch import JSearchScraper
from scrapers.base import JobListing
from utils.dedup import DeduplicationTracker
from utils.emailer import send_job_digest

# ── Logging Setup ───────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger("main")


def scrape_all_sources() -> list[JobListing]:
    """Run all configured scrapers and collect job listings."""
    all_jobs: list[JobListing] = []

    # ── Adzuna (UK-focused, best for London) ────────────────────────────
    logger.info("Running Adzuna scraper...")
    try:
        adzuna = AdzunaScraper()
        jobs = adzuna.scrape()
        all_jobs.extend(jobs)
        logger.info("  → %d jobs from Adzuna", len(jobs))
    except Exception as exc:
        logger.error("  ✗ Adzuna scraper failed: %s", exc)

    # ── JSearch / RapidAPI (LinkedIn, Indeed, Glassdoor aggregator) ─────
    logger.info("Running JSearch scraper...")
    try:
        jsearch = JSearchScraper()
        jobs = jsearch.scrape()
        all_jobs.extend(jobs)
        logger.info("  → %d jobs from JSearch", len(jobs))
    except Exception as exc:
        logger.error("  ✗ JSearch scraper failed: %s", exc)

    # ── Deduplicate across sources (same job may appear on both) ────────
    seen: dict[str, JobListing] = {}
    for job in all_jobs:
        key = f"{job.company.lower()}|{job.title.lower()}"
        if key not in seen:
            seen[key] = job

    deduped = list(seen.values())
    logger.info("Total unique listings: %d (from %d raw)", len(deduped), len(all_jobs))
    return deduped


def run_pipeline(dry_run: bool = False):
    """Full pipeline: scrape → match → dedup → email."""
    logger.info("=" * 60)
    logger.info("Starting job scraping pipeline...")
    logger.info("=" * 60)

    # 1. Scrape
    all_jobs = scrape_all_sources()
    if not all_jobs:
        logger.warning("No jobs found. Make sure at least one API key is configured in config.py.")
        return

    # 2. Match & rank (≥ 85% score)
    scored = rank_jobs(all_jobs, min_score=MIN_MATCH_SCORE)
    logger.info("Jobs scoring ≥ %d%%: %d", MIN_MATCH_SCORE, len(scored))

    if not scored:
        logger.info("No jobs met the %d%% match threshold this run.", MIN_MATCH_SCORE)
        return

    # 3. Deduplicate — remove previously sent
    dedup = DeduplicationTracker(db_path=SENT_JOBS_DB)
    dedup.cleanup_old(max_age_days=90)

    new_scored = [s for s in scored if dedup.is_new(s.listing.unique_key())]
    logger.info("After dedup: %d new jobs (filtered %d duplicates).",
                len(new_scored), len(scored) - len(new_scored))

    if not new_scored:
        logger.info("All matching jobs were already sent previously. Nothing to email.")
        return

    # 4. Print summary to console
    _print_summary(new_scored)

    # 5. Send email
    success = send_job_digest(new_scored, dry_run=dry_run)

    # 6. Mark as sent (only if email succeeded or dry-run)
    if success or dry_run:
        dedup.mark_sent([s.listing.unique_key() for s in new_scored])

    logger.info("Pipeline complete.")


def _print_summary(scored_jobs: list):
    """Print a readable console summary of matched jobs."""
    print("\n" + "=" * 70)
    print(f"  MATCHED JOBS: {len(scored_jobs)} listings (≥ {MIN_MATCH_SCORE}% fit)")
    print("=" * 70)
    for i, sj in enumerate(scored_jobs, 1):
        j = sj.listing
        print(f"\n  #{i}  {j.title}")
        print(f"      Company:    {j.company}")
        print(f"      Location:   {j.location}")
        print(f"      Match:      {sj.match_score}%")
        print(f"      Work Mode:  {sj.work_mode_rating}")
        print(f"      Salary:     {sj.predicted_salary}")
        print(f"      Total Comp: {sj.predicted_total_comp}")
        print(f"      Stock:      {'Yes' if sj.has_stock else 'No'}")
        print(f"      Benefits:   {', '.join(sj.benefits[:5])}")
        print(f"      URL:        {j.url}")
        if sj.match_reasons:
            print(f"      Fit:        {'; '.join(sj.match_reasons[:3])}")
        if sj.penalty_reasons:
            print(f"      Notes:      {'; '.join(sj.penalty_reasons)}")
    print("\n" + "=" * 70 + "\n")


def start_scheduler():
    """Start the weekly scheduled scraping job."""
    logger.info("Scheduler started — will run every %s at %s.", SCHEDULE_DAY, SCHEDULE_TIME)

    schedule_func = getattr(schedule.every(), SCHEDULE_DAY)
    schedule_func.at(SCHEDULE_TIME).do(run_pipeline, dry_run=False)

    # Also run immediately on first start
    logger.info("Running initial scrape now...")
    run_pipeline(dry_run=False)

    while True:
        schedule.run_pending()
        time.sleep(60)


def main():
    parser = argparse.ArgumentParser(
        description="Data Science Job Listings Scraper — London & Remote"
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--dry-run", action="store_true",
                       help="Run once, print results to console (no email)")
    group.add_argument("--run-once", action="store_true",
                       help="Run once and send email")
    group.add_argument("--schedule", action="store_true",
                       help="Start the weekly scheduler")

    args = parser.parse_args()

    if args.dry_run:
        run_pipeline(dry_run=True)
    elif args.run_once:
        run_pipeline(dry_run=False)
    elif args.schedule:
        start_scheduler()


if __name__ == "__main__":
    main()
