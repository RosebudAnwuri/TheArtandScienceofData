"""
Deduplication tracker — ensures the same role is never sent twice.
Uses a simple JSON file to persist sent job keys across runs.
"""

import json
import logging
import os
from datetime import datetime

logger = logging.getLogger(__name__)


class DeduplicationTracker:
    """Tracks which jobs have already been emailed to avoid duplicates."""

    def __init__(self, db_path: str = "sent_jobs.json"):
        self.db_path = db_path
        self._sent: dict[str, str] = {}  # key -> ISO date first sent
        self._load()

    def _load(self):
        if os.path.exists(self.db_path):
            try:
                with open(self.db_path, "r") as f:
                    self._sent = json.load(f)
                logger.info("Loaded %d previously sent jobs.", len(self._sent))
            except (json.JSONDecodeError, IOError) as exc:
                logger.warning("Could not load dedup DB: %s. Starting fresh.", exc)
                self._sent = {}

    def _save(self):
        with open(self.db_path, "w") as f:
            json.dump(self._sent, f, indent=2)

    def is_new(self, key: str) -> bool:
        """Return True if this job has NOT been sent before."""
        return key not in self._sent

    def mark_sent(self, keys: list[str]):
        """Mark a batch of job keys as sent."""
        now = datetime.utcnow().isoformat()
        for k in keys:
            self._sent[k] = now
        self._save()
        logger.info("Marked %d jobs as sent (total tracked: %d).", len(keys), len(self._sent))

    def filter_new(self, keys: list[str]) -> list[str]:
        """Return only keys that have not been sent before."""
        return [k for k in keys if self.is_new(k)]

    def cleanup_old(self, max_age_days: int = 90):
        """Remove entries older than max_age_days to keep the DB manageable."""
        cutoff = datetime.utcnow().timestamp() - (max_age_days * 86400)
        before = len(self._sent)
        self._sent = {
            k: v for k, v in self._sent.items()
            if datetime.fromisoformat(v).timestamp() > cutoff
        }
        removed = before - len(self._sent)
        if removed:
            logger.info("Cleaned up %d old entries from dedup DB.", removed)
            self._save()
