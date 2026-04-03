"""
Candidate profile extracted from resume.
Used by the matching engine to score job listings.
"""

CANDIDATE_PROFILE = {
    "name": "Rosebud Anwuri",
    "current_title": "Staff Data Scientist",
    "years_experience": 10,  # 2015 to present
    "seniority_level": "staff",  # junior, mid, senior, staff, principal

    # ── Core Skills ─────────────────────────────────────────────────────
    "technical_skills": [
        "machine learning",
        "ml modeling",
        "predictive modeling",
        "causal inference",
        "causal analysis",
        "a/b testing",
        "experimentation",
        "statistical modeling",
        "sql",
        "r",
        "python",
        "data analysis",
        "advanced analytics",
        "root cause analysis",
        "funnel analysis",
        "roi measurement",
        "metric design",
        "product analytics",
        "data quality",
        "etl",
        "dashboards",
        "data visualization",
        "deep learning",
        "nlp",
        "classification",
        "regression",
        "clustering",
        "time series",
    ],

    "domain_skills": [
        "partnerships analytics",
        "ads measurement",
        "trust and safety",
        "integrity",
        "abuse detection",
        "business messaging",
        "customer support analytics",
        "product strategy",
        "cross-functional collaboration",
        "stakeholder management",
        "executive communication",
        "mentoring",
        "team leadership",
        "hiring",
        "analytics roadmap",
        "goal setting",
        "inventory optimisation",
        "working capital optimisation",
        "organisational design",
        "consulting",
        "financial services",
        "telecommunications",
    ],

    # ── Work History ────────────────────────────────────────────────────
    "work_history": [
        {
            "company": "Meta (Facebook)",
            "title": "Staff Data Scientist",
            "team": "Messenger Integrity, Creative Analytics, Partnerships Analytics",
            "start": "2019-09",
            "end": "present",
            "highlights": [
                "Founded and scaled Partnerships Analytics across Facebook, Instagram, WhatsApp Business Messaging",
                "Built predictive prioritisation models driving $100K incremental daily revenue within one month",
                "Published ML research on bad actor identification reducing abuse reports by 40%",
                "Designed ROI and funnel measurement frameworks for Ads, Business Messaging, Customer Support",
                "Built automated root-cause analysis tooling for metric decline diagnosis",
                "Led experimentation strategy (A/B testing, causal analysis) across multiple products",
                "Mentored 4 Meta employees into Data Science roles",
            ],
        },
        {
            "company": "Ernst & Young",
            "title": "Data Scientist",
            "start": "2018-11",
            "end": "2019-08",
            "highlights": [
                "Delivered €400M working capital impact for FTSE 500 client",
                "Designed Organisational Design Optimisation model reducing timelines by 80%",
                "Led inventory optimisation across three continents, £100M+ stock reduction",
                "Built in-database late-payment risk model using R and SQL with ~90% accuracy",
            ],
        },
        {
            "company": "Accenture",
            "title": "Technology Consulting Analyst",
            "start": "2015-09",
            "end": "2018-06",
            "highlights": [
                "Executive-level analytics and dashboards for telecom client",
                "Reduced data migration issues by 90% via automated SQL-based data quality frameworks",
            ],
        },
    ],

    # ── Target Roles ────────────────────────────────────────────────────
    "target_titles": [
        "data scientist",
        "staff data scientist",
        "senior data scientist",
        "principal data scientist",
        "lead data scientist",
        "data science manager",
        "head of data science",
        "applied scientist",
        "research scientist",
        "machine learning scientist",
        "analytics lead",
        "analytics manager",
        "senior analytics manager",
        "decision scientist",
        "quantitative analyst",
        "ml engineer",
        "machine learning engineer",
    ],

    # ── Location Preference ─────────────────────────────────────────────
    "preferred_locations": ["london", "remote", "hybrid", "uk", "emea"],

    # ── Work Mode Preference ────────────────────────────────────────────
    # "remote" > "hybrid" > "office" ; reject 5-days-in-office
    "max_office_days": 4,
    "prefers_remote": True,
}
