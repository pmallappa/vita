# Resume/CV User Guide

## Overview

This repository contains a modular resume/CV system that can generate multiple document formats:
- **Vita**: Full 10-page detailed curriculum vitae
- **CV**: 2-page condensed CV
- **Resume**: Traditional resume format

All documents are built from org-mode files, allowing for easy content management and version control.

## Quick Start

### Building Documents

```bash
# Build the full vita (10 pages)
make vita

# Build the 2-page CV
make cv

# Build both
make all

# Clean build artifacts
make clean
```

### Output Location

All PDFs are generated in the `output/` directory:
- `output/prem-mallappa-vita.pdf` - Full vita
- `output/prem-mallappa-cv.pdf` - 2-page CV

## Document Structure

### Source Files

```
org/
├── prem-mallappa-vita.org       # Main vita file
├── prem-mallappa-cv.org         # Main CV file
└── sections/
    ├── 00-header.org           # Personal info, summary
    ├── skills.org              # Technical skills
    ├── education.org           # Education and degrees
    ├── experience.org          # Work experience (bullet format)
    ├── projects.org            # Detailed projects with margin notes
    ├── open-source.org         # Open source contributions
    └── awards.org              # Awards and recognition
```

### Data Files (for Python-based resume)

```
python/data/
├── personal.yaml               # Contact information
├── skills.yaml                # Technical skills by category
├── education.yaml             # Degrees and institutions
├── experience.yaml            # Work history
├── projects.yaml              # Detailed project descriptions
├── opensource.yaml            # Open source contributions
└── awards.yaml                # Awards and honors
```

## Content Management

### Editing Experience

The `org/sections/experience.org` file contains work experience in bullet-point format:

```org
* Principal Engineer
:PROPERTIES:
:CV_ENV: cventry
:EMPLOYER: AMD Ltd.
:LOCATION: Bengaluru, India
:FROM: Feb. 2018
:TO: Present
:END:

- Achievement bullet point 1
- Achievement bullet point 2
```

**Tips:**
- Use action verbs (Architected, Developed, Engineered, Led)
- Include quantifiable results (30% improvement, 3x performance)
- Focus on impact and outcomes

### Editing Projects

The `org/sections/projects.org` file contains detailed project descriptions with margin notes:

```org
*** AMD Cryptography Library
:PROPERTIES:
:CV_ENV: cvproject
:PROJECT: AMD Cryptography Library
:DATE: Jan. 2022 -- Jun. 2025
:LANGUAGES: C++, Assembly
:TOOLS: Git, CMake, OpenSSL
:TECH: AES-CFB, SHA2, AVX2, AESNI, SHANI
:END:

*Summary:* Brief project overview and context.

#+latex: \par\vspace{2mm}

*Contributions:*
- Specific achievement 1
- Specific achievement 2
```

**Margin Notes:**
- `:LANGUAGES:` - Programming languages used
- `:TOOLS:` - Development tools, frameworks
- `:TECH:` - Technologies, architectures, protocols

### Editing Awards

The `org/sections/awards.org` file lists awards and recognition:

```org
* Spotlight
:PROPERTIES:
:CV_ENV: cvhonor
:ORGANIZATION: AMD
:LOCATION: AMD
:DATE: Q3 2021
:END:
Description of achievement that led to the award
```

The description will now align properly with the award title.

### Page Breaks

Control page breaks using the `:PAGEBREAK:` property:

```org
* Experience
:PROPERTIES:
:PAGEBREAK: t
:END:
```

This forces a new page before the section.

## Styling

### LaTeX Style Files

```
assets/style/
├── pawesome-cv.cls            # Main document class
├── cv-core.sty                # Core layout settings
├── cv-colors.sty              # Color definitions
├── cv-layout.sty              # Page layout
├── cv-styles.sty              # Text styles
├── cv-commands.sty            # Custom commands
├── cv-projects.sty            # Project-specific styles
└── cv-header-footer.sty       # Headers and footers
```

### Key Style Customizations

**Font Sizes:**
- Company name: 11pt
- Project title: 8pt
- Description text: 9pt

**Spacing:**
- Entry spacing: `\\[-2mm]` between role and details
- Project gap: `\\[1mm]` between title and summary
- Definition indent: 5em for "Summary:" and "Contributions:"

**Margin Notes:**
- Offset: `[-0.5em]` for vertical alignment
- Used in Projects section for Languages/Tools/Tech

## Build System

### Prerequisites

- **Emacs** with org-mode
- **XeLaTeX** for PDF generation
- **Python 3** (for Python-based resume)
- **Podman/Docker** (optional, for containerized builds)

### Containerized Build

```bash
# Using the provided container
cmd /c podman run --rm -v "%CD%:/workspace" -w /workspace resume-builder make vita
```

### Direct Build

```bash
# Build vita directly
make vita

# Build CV directly
make cv
```

## Troubleshooting

### Common Issues

**LaTeX compilation errors:**
- Check for special characters that need escaping (& becomes \&)
- Verify all org-mode sections have proper `:PROPERTIES:` blocks
- Ensure `:END:` tags close all properties

**Missing sections:**
- Verify section files exist in `org/sections/`
- Check main org file includes the section correctly

**Spacing issues:**
- Adjust `\vspace{}` values in style files
- Modify `\\[Xmm]` spacing in cv-commands.sty

**Font warnings:**
- These are cosmetic and don't affect the output
- Source Sans Pro fonts are correctly embedded

## Version Control

### Recommended Workflow

1. Edit org files in `org/sections/`
2. Build and review PDF: `make vita`
3. Commit changes to git
4. Tag major versions: `git tag v1.2.0`

### What to Track

**Track in Git:**
- All `.org` files
- Style files (`.sty`, `.cls`)
- Documentation
- Makefile, scripts

**Ignore:**
- `output/*.pdf`
- `output/*.tex`
- `output/*.aux`, `*.log`, `*.out`

## Advanced Customization

### Adding New Sections

1. Create `org/sections/newsection.org`
2. Add to main org file: `#+INCLUDE: "sections/newsection.org"`
3. Define appropriate `:CV_ENV:` property
4. Rebuild: `make vita`

### Changing Colors

Edit `assets/style/cv-colors.sty`:

```latex
\definecolor{awesome}{HTML}{0395DE}     % Primary accent
\definecolor{darktext}{HTML}{414141}    # Header text
\definecolor{text}{HTML}{333333}        % Body text
\definecolor{graytext}{HTML}{5D5D5D}    % Metadata
```

### Modifying Layout

Edit `assets/style/cv-layout.sty`:

```latex
\geometry{
  left=2.0cm,
  top=1.5cm,
  right=2.0cm,
  bottom=2.0cm,
  footskip=.5cm
}
```

## Tips for Professional Content

### Writing Achievements

**Good:**
- "Achieved 30% performance improvement through algorithmic optimization"
- "Led development of cryptographic library used by 5 Fortune 500 companies"
- "Architected scalable system handling 1M+ requests/second"

**Avoid:**
- "Worked on performance"
- "Helped with development"
- "Responsible for system design"

### Project Summaries

Include:
- Business context (why was this needed?)
- Technical challenge
- Your role and impact
- Quantifiable results

### Margin Notes

Be specific and accurate:
- Languages: Actual languages used, not just known
- Tools: Version control, build systems, IDEs actually used
- Tech: Specific technologies, not generic terms

## Getting Help

- Check `docs/FORMATTING_GUIDE.md` for LaTeX formatting details
- See `docs/ARCHITECTURE.md` for system architecture
- Review example sections for proper org-mode syntax
