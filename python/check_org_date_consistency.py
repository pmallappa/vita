#!/usr/bin/env python3
"""Validate date consistency between Org experience and projects sections.

Detailed role/project dates live in org/sections/projects.org.  This checker
verifies that:
1. every project company has a matching company window in experience.org, and
2. every project date falls within its enclosing company date window.
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass, field
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EXPERIENCE_ORG = ROOT / "org" / "sections" / "experience.org"
PROJECTS_ORG = ROOT / "org" / "sections" / "projects.org"

MONTHS = {
    "jan": 1,
    "feb": 2,
    "mar": 3,
    "apr": 4,
    "may": 5,
    "jun": 6,
    "jul": 7,
    "aug": 8,
    "sep": 9,
    "oct": 10,
    "nov": 11,
    "dec": 12,
}

COMPANY_STOPWORDS = {
    "communication",
    "communications",
    "india",
    "international",
    "limited",
    "ltd",
    "ltd.",
    "network",
    "networks",
    "private",
    "pvt",
    "pvt.",
    "remote",
    "london",
    "system",
    "systems",
    "technologies",
    "technology",
    "uk",
}


@dataclass(order=True, frozen=True)
class DatePoint:
    year: int
    month: int

    def display(self) -> str:
        return f"{self.year:04d}-{self.month:02d}"


PRESENT = DatePoint(9999, 12)


@dataclass(frozen=True)
class DateSpan:
    start: DatePoint
    end: DatePoint
    raw: str

    def contains(self, other: "DateSpan") -> bool:
        return self.start <= other.start and self.end >= other.end

    def display(self) -> str:
        end = "Present" if self.end == PRESENT else self.end.display()
        return f"{self.start.display()} -> {end}"


@dataclass
class Heading:
    level: int
    title: str
    props: dict[str, str] = field(default_factory=dict)
    children: list["Heading"] = field(default_factory=list)


@dataclass
class CompanyRecord:
    title: str
    key: str
    span: DateSpan


def canonical_company_key(name: str) -> str:
    cleaned = re.sub(r"[^a-z0-9]+", " ", name.lower()).strip()
    tokens = [token for token in cleaned.split() if token not in COMPANY_STOPWORDS]
    if not tokens:
        return cleaned
    return " ".join(tokens)


def parse_date_point(value: str, *, is_end: bool) -> DatePoint:
    text = value.strip()
    if not text:
        raise ValueError("empty date value")
    if text.lower() == "present":
        return PRESENT

    year_only = re.fullmatch(r"(\d{4})", text)
    if year_only:
        year = int(year_only.group(1))
        return DatePoint(year, 12 if is_end else 1)

    month_year = re.fullmatch(r"([A-Za-z]+)\.?\s+(\d{4})", text)
    if month_year:
        month_key = month_year.group(1).lower()[:3]
        if month_key not in MONTHS:
            raise ValueError(f"unknown month in date '{value}'")
        return DatePoint(int(month_year.group(2)), MONTHS[month_key])

    raise ValueError(f"unsupported date format '{value}'")


def parse_span(start_value: str, end_value: str | None = None) -> DateSpan:
    if end_value is None:
        parts = re.split(r"\s*--\s*", start_value.strip(), maxsplit=1)
        if len(parts) == 2:
            start_raw, end_raw = parts
        else:
            start_raw = parts[0]
            end_raw = parts[0]
        raw = start_value.strip()
    else:
        start_raw = start_value.strip()
        end_raw = end_value.strip()
        raw = f"{start_raw} -- {end_raw}"

    return DateSpan(
        start=parse_date_point(start_raw, is_end=False),
        end=parse_date_point(end_raw, is_end=True),
        raw=raw,
    )


def parse_org_headings(path: Path) -> list[Heading]:
    lines = path.read_text(encoding="utf-8").splitlines()
    roots: list[Heading] = []
    stack: list[Heading] = []
    idx = 0

    while idx < len(lines):
        match = re.match(r"^(\*+)\s+(.*)$", lines[idx])
        if not match:
            idx += 1
            continue

        heading = Heading(level=len(match.group(1)), title=match.group(2).strip())
        idx += 1

        if idx < len(lines) and lines[idx].strip() == ":PROPERTIES:":
            idx += 1
            while idx < len(lines) and lines[idx].strip() != ":END:":
                prop_match = re.match(r"^:([^:]+):\s*(.*)$", lines[idx])
                if prop_match:
                    heading.props[prop_match.group(1)] = prop_match.group(2).strip()
                idx += 1
            if idx < len(lines) and lines[idx].strip() == ":END:":
                idx += 1

        while stack and stack[-1].level >= heading.level:
            stack.pop()
        if stack:
            stack[-1].children.append(heading)
        else:
            roots.append(heading)
        stack.append(heading)

    return roots


def load_experience_companies(path: Path) -> dict[str, CompanyRecord]:
    records: dict[str, CompanyRecord] = {}
    for heading in parse_org_headings(path):
        cv_env = heading.props.get("CV_ENV")
        if cv_env == "cvemployer":
            span = parse_span(heading.props["FROM"], heading.props["TO"])
            key = canonical_company_key(heading.title)
            records[key] = CompanyRecord(heading.title, key, span)
        elif cv_env == "cvsubsection":
            for child in heading.children:
                if child.props.get("CV_ENV") != "cventryshort":
                    continue
                employer = child.props["EMPLOYER"]
                span = parse_span(child.props["FROM"], child.props["TO"])
                key = canonical_company_key(employer)
                records[key] = CompanyRecord(employer, key, span)
    return records


def validate_projects(path: Path, experience: dict[str, CompanyRecord]) -> list[str]:
    errors: list[str] = []
    for company in parse_org_headings(path):
        if company.props.get("CV_ENV") != "cvemployer":
            continue

        company_span = parse_span(company.props["FROM"], company.props["TO"])
        company_key = canonical_company_key(company.title)
        exp_record = experience.get(company_key)
        if exp_record is None:
            errors.append(
                f"projects company '{company.title}' has no matching entry in experience.org"
            )
            continue

        if not exp_record.span.contains(company_span):
            errors.append(
                "company window mismatch for "
                f"'{company.title}': projects={company_span.display()} is not within "
                f"experience={exp_record.span.display()} ({exp_record.title})"
            )

        for role in company.children:
            if role.props.get("CV_ENV") != "cvrole":
                continue
            for project in role.children:
                if project.props.get("CV_ENV") != "cvproject":
                    continue
                project_span = parse_span(project.props["DATE"])
                if not company_span.contains(project_span):
                    errors.append(
                        f"project '{project.title}' ({project_span.display()}) falls outside "
                        f"company '{company.title}' window ({company_span.display()})"
                    )
    return errors


def main() -> int:
    experience = load_experience_companies(EXPERIENCE_ORG)
    errors = validate_projects(PROJECTS_ORG, experience)

    if errors:
        print("Date consistency check failed:")
        for error in errors:
            print(f"  - {error}")
        return 1

    print("Date consistency check passed.")
    print(f"Detailed date source: {PROJECTS_ORG.relative_to(ROOT)}")
    print(f"Validated against:    {EXPERIENCE_ORG.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
