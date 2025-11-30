# AI Assistant Context Document

## Project Overview

**Purpose**: Modular resume/CV generation system using org-mode source files compiled to LaTeX/PDF  
**Current State**: Production-ready, 10-page vita successfully building  
**Last Major Update**: 2025-11-29 - Professional content rewrite completed  
**Deployment Model**: Containerized build system using Podman - all builds run inside containers

### Containerization Architecture

**CRITICAL**: This is a **containerized project**. All build commands execute inside a Podman container, not natively on the host system.

- **Container Image**: `localhost/resume-builder` (Alpine Linux-based)
- **Build Method**: `podman run --rm -v .:/workspace:Z -w /workspace localhost/resume-builder make <target>`
- **Makefile Purpose**: Defines build targets for **container execution**, not native host execution
- **Dependencies**: All LaTeX, Emacs, Python tools installed in container, not required on host

**Why Containerized?**
- Consistent build environment across different host systems
- No need to install LaTeX distribution (multi-GB) on host
- Reproducible builds with locked dependency versions
- Isolation from host system configuration

## Directory Structure

### Overview

```
Resume/
├── .devcontainer/          # Container configuration
├── assets/                 # LaTeX styling and fonts
├── docs/                   # Documentation
├── elisp/                  # Emacs Lisp configuration
├── org/                    # Source content (org-mode files)
├── outputs/                # Generated PDFs and LaTeX files
├── python/                 # Legacy Python-based generator (reference only)
├── Makefile               # Build automation (runs in container)
└── .gitignore             # Git ignore patterns
```

### Detailed Directory Breakdown

#### `.devcontainer/` - Container Configuration
**Purpose**: Defines the containerized build environment

```
.devcontainer/
├── devcontainer.json      # VSCode devcontainer configuration
└── Containerfile          # Podman/Docker image definition (Alpine Linux)
```

**Key Files:**
- `Containerfile`: Defines Alpine Linux-based image with TeX Live, Emacs, Python
- `devcontainer.json`: VSCode integration for container development

**Container Contents:**
- TeX Live 2024 (XeLaTeX, LaTeX packages)
- Emacs (non-X version for org-mode export)
- Python 3 with YAML, Markdown support
- Build tools: make, bash, git
- Fonts: DejaVu, Source Sans Pro

#### `assets/` - LaTeX Styling and Resources
**Purpose**: All LaTeX class files, style packages, and fonts

```
assets/
├── fonts/                 # TrueType fonts (Nokia Sans, Roboto, FontAwesome)
│   ├── FontAwesome.ttf
│   ├── NokiaSans-*.ttf   # Multiple weights/styles
│   └── Roboto-*.ttf      # Multiple weights/styles
├── images/               # Certification badge images (currently unused)
└── style/                # LaTeX style system
    ├── pawesome-cv.cls           # Main document class
    ├── cv-core.sty              # Core packages and settings
    ├── cv-colors.sty            # Color definitions
    ├── cv-layout.sty            # Page layout and geometry
    ├── cv-styles.sty            # Text styling and fonts
    ├── cv-commands.sty          # Custom LaTeX commands
    ├── cv-header-footer.sty     # Header/footer definitions
    ├── cv-experience.sty        # Experience entry formatting
    ├── cv-projects.sty          # Project entry formatting
    ├── cv-opensource.sty        # Open source contribution formatting
    ├── fontawesome.sty          # FontAwesome icon support
    └── fonts.sty                # Font loading configuration
```

**Style System Hierarchy:**
1. `pawesome-cv.cls` - Document class (loads all other styles)
2. `cv-core.sty` - Foundation (packages, basic settings)
3. `cv-colors.sty` - Color palette
4. `cv-layout.sty` - Page geometry, margins
5. `cv-styles.sty` - Typography, font sizes
6. `cv-commands.sty` - Custom commands and environments
7. `cv-*.sty` - Section-specific formatting

**Modification Guidelines:**
- **Colors**: Edit `cv-colors.sty`
- **Spacing**: Edit `cv-layout.sty` or `cv-commands.sty`
- **Fonts**: Edit `cv-styles.sty` or `fonts.sty`
- **Entry formats**: Edit `cv-experience.sty`, `cv-projects.sty`, etc.

#### `docs/` - Documentation
**Purpose**: Project documentation for users and AI assistants

```
docs/
├── AI_CONTEXT.md          # This file - AI assistant guide
├── USER_GUIDE.md          # User-facing documentation
├── MODULARIZATION.md      # LaTeX modularization notes
├── FORMATTING_GUIDE.md    # Formatting conventions
├── MARKDOWN_SUPPORT.md    # Markdown integration notes
├── PROJECT_MARGIN_NOTES.md # Margin notes implementation
└── README.md              # General project overview
```

**Key Documents:**
- `AI_CONTEXT.md`: Primary reference for AI assistants (this file)
- `USER_GUIDE.md`: How to edit content and build resumes
- `MODULARIZATION.md`: LaTeX architecture decisions

#### `elisp/` - Emacs Lisp Configuration
**Purpose**: Emacs configuration for org-mode to LaTeX export

```
elisp/
├── org-cv-init.el         # Main initialization file (loads org-cv)
├── org-cv/                # Cloned from GitLab (org-cv package)
│   ├── ox-awesomecv.el   # AwesomeCV export backend
│   ├── ox-awesomecv2.el  # Alternative backend
│   ├── ox-altacv.el      # AltaCV backend
│   ├── ox-moderncv.el    # ModernCV backend
│   └── ...               # Other org-cv files
└── ox-ext/                # Custom extensions to org-cv
    ├── ox-awesomecv-extensions.el      # Main extension file
    ├── ox-awesomecv-ext-core.el        # Core functionality
    ├── ox-awesomecv-ext-entries.el     # Entry type handlers
    ├── ox-awesomecv-ext-headline.el    # Headline processing
    ├── ox-awesomecv-ext-project.el     # Project-specific logic
    └── ox-awesomecv-ext-utils.el       # Utility functions
```

**Export Flow:**
1. `org-cv-init.el` loads org-cv package and extensions
2. Defines `pawesome-cv` LaTeX class mapping
3. Configures export settings (XeLaTeX, package handling)
4. Extensions add custom property handling (margin notes, page breaks)

**When to Modify:**
- Adding new org-mode properties: Edit `ox-ext/` files
- Changing export behavior: Edit `org-cv-init.el`
- **Rarely needed** - most customization happens in LaTeX styles

#### `org/` - Source Content (Org-Mode Files)
**Purpose**: **PRIMARY SOURCE OF TRUTH** for all resume content

```
org/
├── prem-mallappa-vita.org     # Main vita file (includes all sections)
├── prem-mallappa-cv.org       # 2-page CV file (subset of sections)
├── sections/                  # Modular content sections
│   ├── 00-header.org         # Name, contact info, summary
│   ├── skills.org            # Technical skills
│   ├── education.org         # Degrees and certifications
│   ├── experience.org        # Work history
│   ├── projects.org          # Detailed project descriptions
│   ├── open-source.org       # Open source contributions
│   └── awards.org            # Awards and recognition
├── images/                    # Certification badge images
│   └── *.png                 # AWS, CISSP, SAFe badges
└── elisp/                     # Legacy/backup elisp files
    └── debug-cv.el           # Debugging utilities
```

**Content Organization:**
- **Main files**: `prem-mallappa-vita.org`, `prem-mallappa-cv.org`
- **Sections**: Modular `.org` files included via `#+INCLUDE:`
- **Images**: Referenced in org-mode with `[[file:...]]` syntax

**Editing Workflow:**
1. Edit content in `org/sections/*.org`
2. Main files automatically include updated sections
3. Build with `make vita` or `make cv`

**File Purposes:**
- `00-header.org`: Personal info, professional summary
- `skills.org`: Technical skills matrix
- `education.org`: Degrees, certifications
- `experience.org`: Work history (companies, roles, achievements)
- `projects.org`: Detailed project descriptions with margin notes
- `open-source.org`: Open source contributions
- `awards.org`: Professional awards and recognition

#### `outputs/` - Generated Files
**Purpose**: Build artifacts (PDFs and intermediate LaTeX files)

```
outputs/
├── prem-mallappa-vita.pdf     # Final 10-page vita PDF
├── prem-mallappa-vita.tex     # Generated LaTeX source
├── prem-mallappa-vita.aux     # LaTeX auxiliary file
├── prem-mallappa-vita.log     # LaTeX build log
├── prem-mallappa-vita.out     # Hyperref output
├── prem-mallappa-cv.pdf       # Final 2-page CV PDF
└── prem-mallappa-cv.tex       # Generated LaTeX source
```

**Build Artifacts:**
- `.pdf`: Final output documents
- `.tex`: Generated LaTeX (from org-mode export)
- `.aux`, `.log`, `.out`: LaTeX compilation artifacts

**Cleaning:**
- `make clean` removes all files in this directory
- `.gitignore` excludes this directory from version control

#### `python/` - Legacy Python Generator (Reference Only)
**Purpose**: Original Python-based resume generator (no longer in build pipeline)

```
python/
├── generate.py            # Main generator script (unused)
├── data/                  # YAML data files (reference only)
│   ├── personal.yaml     # Personal information
│   ├── experience.yaml   # Work history
│   ├── projects.yaml     # Project descriptions
│   ├── education.yaml    # Education details
│   ├── skills.yaml       # Technical skills
│   ├── awards.yaml       # Awards and recognition
│   ├── opensource.yaml   # Open source contributions
│   └── publications.yaml # Publications (if any)
└── resume_builder/        # Python package (unused)
    ├── __init__.py
    ├── resume_generator.py
    ├── section_builders.py
    ├── formatters.py
    └── loaders.py
```

**Status**: **DEPRECATED** - Not used in current build pipeline

**Historical Context:**
- Original approach: Python reads YAML → generates LaTeX → compiles PDF
- Current approach: Org-mode → Emacs export → LaTeX → PDF
- YAML files kept as reference but **NOT source of truth**

**Important**: When updating content, edit `org/sections/*.org`, not YAML files

#### Root Level Files

```
Resume/
├── Makefile              # Build automation (container targets)
├── .gitignore           # Git ignore patterns
└── git-commits.sh       # Git commit helper script
```

**Makefile Targets:**
- `make vita`: Build full vita (10 pages)
- `make cv`: Build 2-page CV
- `make clean`: Remove build artifacts
- `make all`: Build all documents

**CRITICAL**: Makefile commands run **inside container**, not on host

### Directory Relationships

```
Content Flow:
org/sections/*.org → (Emacs + elisp/) → outputs/*.tex → (XeLaTeX + assets/style/) → outputs/*.pdf

Style Application:
assets/style/pawesome-cv.cls → loads → cv-*.sty → applied during → XeLaTeX compilation

Container Execution:
.devcontainer/Containerfile → builds → resume-builder image → runs → Makefile targets
```

### File Count Summary

- **Source files**: ~15 org-mode files
- **Style files**: ~12 LaTeX style packages
- **Fonts**: ~30 font files (multiple weights/styles)
- **Documentation**: ~7 markdown files
- **Elisp files**: ~10 configuration files
- **Total project size**: ~50MB (mostly fonts)

### What NOT to Modify

**Never Edit:**
- `outputs/*.tex` - Auto-generated from org-mode
- `outputs/*.pdf` - Build artifacts
- `elisp/org-cv/*` - Third-party package (cloned from GitLab)
- `python/` - Legacy code, not in build pipeline

**Rarely Edit:**
- `.devcontainer/Containerfile` - Only for dependency changes
- `elisp/org-cv-init.el` - Only for export configuration changes
- `assets/style/pawesome-cv.cls` - Only for major structural changes

**Frequently Edit:**
- `org/sections/*.org` - Content updates
- `assets/style/cv-commands.sty` - Layout tweaks
- `assets/style/cv-styles.sty` - Styling adjustments

## System Architecture

### Build Pipeline

```
org-mode files → Emacs export → LaTeX (.tex) → XeLaTeX → PDF
     ↓
org/sections/*.org (source content)
     ↓
elisp/org-cv-init.el (export configuration)
     ↓
outputs/*.tex (generated LaTeX)
     ↓
assets/style/*.sty (styling)
     ↓
outputs/*.pdf (final documents)
```

### Critical Files and Their Roles

| File | Purpose | Edit Frequency |
|------|---------|----------------|
| `org/prem-mallappa-vita.org` | Main vita file, includes all sections | Rarely |
| `org/sections/experience.org` | Work history in bullet points | Often |
| `org/sections/projects.org` | Detailed project descriptions + margin notes | Often |
| `org/sections/awards.org` | Awards and recognition | Occasionally |
| `org/sections/education.org` | Degrees and certifications | Rarely |
| `org/sections/skills.org` | Technical skills | Occasionally |
| `Makefile` | Build automation | Rarely |
| `assets/style/cv-commands.sty` | LaTeX command definitions | When fixing layout |
| `assets/style/cv-styles.sty` | Text styling, fonts, colors | When adjusting appearance |

### Output Locations

**IMPORTANT**: As of 2025-11-29, all outputs go to `output/` directory:
- `output/prem-mallappa-vita.pdf` - Full 10-page vita
- `output/prem-mallappa-cv.pdf` - 2-page CV
- `output/*.tex` - Generated LaTeX (intermediate files)

## Content Structure

### Org-Mode Format

All content lives in `org/sections/*.org` files using org-mode syntax:

```org
* Section Title
:PROPERTIES:
:CV_ENV: environment_name
:EMPLOYER: Company Name
:FROM: Start Date
:TO: End Date
:END:

Content here (bullets, paragraphs, etc.)
```

### Key Environment Types

- `cventry` - Work experience entry (company, role, dates, bullets)
- `cvproject` - Detailed project (includes margin notes)
- `cvhonor` - Award/recognition
- `cvschool` - Education entry
- `cventryshort` - Short stint (company name, dates only)

### Margin Notes System

Projects use special properties for margin notes:

```org
:PROPERTIES:
:LANGUAGES: C++, Assembly
:TOOLS: Git, CMake, OpenSSL
:TECH: AES-CFB, SHA2, AVX2
:END:
```

These appear as small text in the right margin showing technologies used.

## Recent Changes History

### 2025-11-29: Professional Content Rewrite

**What Changed:**
- All experience descriptions rewritten with action-oriented language
- All 14 project summaries professionally rewritten
- Fixed AMD Pensando SmartNIC project (had wrong summary)
- Removed "Model0" placeholder project
- Updated margin notes for accuracy
- Changed education.org "Bacherlors" → "Bachelor of Engineering"
- Changed output directory from `org/` to `output/`
- Fixed awards description alignment

**Content Style Guidelines Applied:**
- Strong action verbs: Architected, Engineered, Developed, Led, Achieved
- Quantifiable results: "30% improvement", "3x performance"
- Technical depth: Specific technologies, not generic terms
- Impact focus: Business context + technical solution + measurable outcome

### Layout Fixes (Previous Sessions)

1. **Spacing Issues Resolved:**
   - Reduced spacing between role headers and details: `\\[-2mm]`
   - Added gap between project title and summary: `\\[1mm]`
   - Fixed vertical spacing in entry types

2. **Margin Note Alignment:**
   - Fixed with `\marginnote{...}[-0.5em]` offset
   - Moved margin note inside tabular environment

3. **Definition-Style Formatting:**
   - Applied hanging indent (5em) to "Summary:" and "Contributions:"
   - Set line height to 1em

4. **Page Breaks:**
   - Selective page breaks using `:PAGEBREAK: t` property
   - Currently: Experience (page 2), Projects (page 4), Awards (page 8)

## Known Issues and Gotchas

### Critical Bugs Fixed

✅ "Bacherlors" typo in education.org  
✅ AMD Pensando SmartNIC had wrong summary (was crypto library summary)  
✅ Model0 project was placeholder with duplicated content  
✅ Awards description not aligned with title  

### Active Warnings (Non-Critical)

- Font shape warnings for SourceSansPro - cosmetic only, fonts work correctly
- Overfull hbox for long skill lists - acceptable in current layout

### Common LaTeX Pitfalls

1. **Ampersand escaping**: Always use `\&` in org-mode for ampersands
2. **Property blocks**: Must have matching `:END:` tag
3. **Build order**: XeLaTeX must run twice for proper cross-references
4. **Sed processing**: Removes unwanted packages from generated .tex

## Build Commands

### Standard Build

```bash
make vita        # Build full vita → output/prem-mallappa-vita.pdf
make cv          # Build 2-page CV → output/prem-mallappa-cv.pdf
make clean       # Remove build artifacts
```

### Containerized Build

```bash
cmd /c podman run --rm -v "%CD%:/workspace" -w /workspace resume-builder make vita
```

### Build Steps (Internal)

1. Emacs exports org → tex using `elisp/org-cv-init.el`
2. Sed removes unwanted LaTeX packages
3. XeLaTeX compiles tex → PDF (run twice for references)
4. Output placed in `output/` directory

## Styling System

### Font Hierarchy

```
Company Name (cventry):        11pt bold
Project Title (cvproject):     8pt bold
Summary/Description:           9pt regular
Margin Notes:                  8pt
```

### Key Spacing Values

```latex
Entry spacing:              \\[-2mm]   (role to details)
Project title to summary:   \\[1mm]    (after title)
Honor description spacing:  [2pt]      (after title line)
Definition indent:          5em        (Summary:/Contributions:)
Margin note offset:         [-0.5em]   (vertical alignment)
```

### Color Scheme

```latex
awesome (primary):    #0395DE  (blue accent)
darktext:            #414141  (headers)
text:                #333333  (body)
graytext:            #5D5D5D  (metadata)
lighttext:           #999999  (subtle elements)
```

## Content Guidelines for AI Assistants

### When Rewriting Content

**DO:**
- Use strong action verbs (Architected, Engineered, Led, Achieved)
- Include quantifiable metrics (30%, 3x, 10μs, 60%)
- Provide technical context (specific technologies, architectures)
- Focus on impact and outcomes
- Maintain consistent professional tone

**DON'T:**
- Use passive voice ("was responsible for")
- Use vague terms ("helped with", "worked on")
- Include generic statements without specifics
- Mix informal and formal language
- Forget to update margin notes when changing project technologies

### Margin Notes Best Practices

**Languages**: Only languages actually used in the project  
**Tools**: Specific tools, not categories (Git, CMake, not "version control")  
**Tech**: Specific technologies/protocols (AVX2, AESNI, not "SIMD")

### Professional Writing Examples

**Good Experience Bullet:**
> Engineered parallel AES-CFB decryption algorithm delivering 3x performance improvement over standard OpenSSL implementation

**Good Project Summary:**
> Initiated as proof-of-concept to address ISV performance bottlenecks with AES-CFB cryptography on AMD EPYC Milan servers. Evolved into comprehensive cryptographic primitives library delivering production-grade implementations.

## File Modification Patterns

### Adding New Experience Entry

```org
* Job Title
:PROPERTIES:
:CV_ENV: cventry
:EMPLOYER: Company Name
:LOCATION: City, Country
:FROM: MMM. YYYY
:TO: MMM. YYYY  (or "Present")
:END:

- Achievement bullet 1
- Achievement bullet 2
```

### Adding New Project

```org
*** Project Name
:PROPERTIES:
:CV_ENV: cvproject
:PROJECT: Short Project Name
:DATE: MMM. YYYY -- MMM. YYYY
:LANGUAGES: Lang1, Lang2
:TOOLS: Tool1, Tool2
:TECH: Tech1, Tech2
:END:

*Summary:* Project overview and context.

#+latex: \par\vspace{2mm}

*Contributions:*
- Specific achievement 1
- Specific achievement 2
```

### Adding Page Break

```org
* Section Name
:PROPERTIES:
:PAGEBREAK: t
:END:
```

## Testing and Validation

### After Any Change

1. Build: `make vita`
2. Check exit code: Should be 0
3. Verify page count: Should be ~10 pages
4. Spot-check PDF for:
   - Proper spacing
   - No LaTeX errors visible
   - Margin notes aligned
   - No orphaned headers

### Common Build Errors

**Error**: "Forbidden control sequence"  
**Cause**: Unescaped special character (usually `&`)  
**Fix**: Use `\&` in org-mode source

**Error**: "Undefined control sequence"  
**Cause**: Missing style file or typo in command name  
**Fix**: Check `\usepackage` and custom command definitions

**Error**: "Runaway argument"  
**Cause**: Missing `:END:` tag or unmatched braces  
**Fix**: Check org-mode property blocks

## Python Data Files (Legacy)

Located in `python/data/*.yaml`, these are **source references only**.

**IMPORTANT**: The org-mode files in `org/sections/*.org` are the **actual source of truth** for content. The YAML files were used to initially populate org files but are no longer in the build pipeline.

If asked to update content, always edit the `.org` files, not the `.yaml` files.

## Integration Points

### Emacs Configuration

`elisp/org-cv-init.el` configures:
- org-mode export backend (awesomecv)
- LaTeX class mappings
- Export settings

**Don't modify** unless changing export behavior.

### LaTeX Class System

`assets/style/pawesome-cv.cls` is the document class.  
Custom packages in `assets/style/cv-*.sty` extend it.

**Modification hierarchy**:
1. Try spacing adjustments in existing commands first
2. Then modify cv-commands.sty for behavior changes
3. Last resort: modify document class

## Context Recovery Checklist

When resuming after context loss, an AI assistant should:

1. ✅ Read this file first (AI_CONTEXT.md)
2. ✅ Check current output directory: Should be `output/`
3. ✅ Verify latest org-mode content in `org/sections/*.org`
4. ✅ Review recent git commits for change history
5. ✅ Build current state: `make vita` and check for errors
6. ✅ Read USER_GUIDE.md for user-facing documentation

## Quick Reference Commands

```bash
# Build vita
make vita

# Build and view
make vita && start output/prem-mallappa-vita.pdf

# Clean build
make clean && make vita

# Check for LaTeX errors
grep -i "error\|warning" output/prem-mallappa-vita.log
```

## Version Information

**Document Version**: 1.0  
**Last Updated**: 2025-11-29  
**Resume System Version**: 2.0  
**Current Page Count**: 10 pages (vita)  
**Build Status**: ✅ Working

## Contact and Maintenance

**Primary User**: Prem Kumar Mallappa  
**Use Case**: Professional resume/CV for software engineering positions  
**Specialization**: Low-level systems, virtualization, cryptography, performance optimization  
**Target Audience**: Senior/Principal Engineering roles at AMD, ARM, Broadcom (similar companies)

---

**Note to AI Assistants**: This document is your anchor. Reference it when uncertain about build process, file locations, or content guidelines. Update it when making significant changes to the system.
