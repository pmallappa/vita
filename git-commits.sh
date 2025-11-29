#!/bin/sh
# Git commit script for resume changes
# This script should be run inside the podman container

# Check if we're in a git repository, if not initialize
if [ ! -d .git ]; then
    git init
fi

# Configure git
git config user.name "Prem Mallappa"
git config user.email "prem.mallappa@gmail.com"
git config --global --add safe.directory /workspace

echo "=== Commit 1: Directory restructuring and elisp modularization ==="
git add assets/ elisp/ .gitignore
git add -u src/
git status --short
git commit -m "Reorganize directory structure and modularize elisp code

- Renamed src/ to assets/style/ for clarity
- Created assets/fonts/ for font files
- Split elisp into elisp/org-cv and elisp/ox-ext modules
- Modularized ox-awesomecv extensions into 6 files:
  - ox-awesomecv-extensions.el (main loader)
  - ox-awesomecv-ext-core.el (dispatcher)
  - ox-awesomecv-ext-project.el (project/opensource formatters)
  - ox-awesomecv-ext-entries.el (entry formatters)
  - ox-awesomecv-ext-headline.el (headline handler)
  - ox-awesomecv-ext-utils.el (margin note utilities)
- Added .gitignore excluding output/, .cline_storage/, tmp-something/"

echo ""
echo "=== Commit 2: Split org files into modular sections ==="
git add org/
git status --short
git commit -m "Split monolithic org file into modular sections

- Created org/sections/ directory with:
  - 00-header.org: Personal info and LaTeX config
  - skills.org, education.org, experience.org
  - open-source.org, projects.org, awards.org
- Removed top-level headings from sections to reduce indentation
- Created skeleton main org files:
  - prem-mallappa-vita.org: Full CV with all sections
  - prem-mallappa-cv.org: Short 2-page resume
- Both use #+INCLUDE directives for modularity"

echo ""
echo "=== Commit 3: Enhance projects with Summary/Contributions ==="
git add org/sections/projects.org
git status --short
git commit -m "Add Summary/Contributions structure to all projects

- Added Summary paragraphs to 11 projects
- Added Contributions itemized lists
- Proper formatting with \par\vspace{2mm} spacing
- Improved readability and structure"

echo ""
echo "=== Commit 4: Minimize cv-header-footer.sty ==="
git add assets/style/cv-header-footer.sty org/sections/00-header.org
git status --short
git commit -m "Minimize cv-header-footer.sty and fix footer

- Removed unused social media links (leanpub, gitlab, stackoverflow, twitter, skype, reddit, xing)
- Kept only: mobile, email, homepage, github, linkedin
- Reduced from 145 to 70 lines
- Removed duplicate author name from footer (%a)"

echo ""
echo "=== Commit 5: Update build system ==="
git add Makefile .devcontainer/ python/
git add -u data/ scripts/ QUICKSTART.md README.md
git status --short
git commit -m "Update build system and documentation

- Updated Makefile paths to use assets/style/
- Added 'cv' target for 2-page resume
- Updated devcontainer configuration
- Reorganized Python scripts to python/ directory
- Moved data files to python/data/
- Build results: vita (7 pages), cv (3 pages)"

echo ""
echo "=== Commit 6: Add documentation ==="
git add docs/ git-commits.sh
git status --short
git commit -m "Add documentation and git workflow script

- Created docs/ directory with:
  - FORMATTING_GUIDE.md
  - MARKDOWN_SUPPORT.md
  - MODULARIZATION.md
  - PROJECT_MARGIN_NOTES.md
- Added git-commits.sh for containerized git workflow"

echo ""
echo "=== Commit 7: Improve project section formatting ==="
git add assets/style/cv-commands.sty assets/style/cv-styles.sty org/sections/projects.org
git status --short
git commit -m "Refine project section with definition-style formatting

- Increased spacing between projects (from -2mm to 1mm)
- Changed Summary -> Synopsis, Contributions -> Details
- Implemented definition-list style with LaTeX description environment
- Synopsis/Details labels at left margin with content aligned below
- Swapped font sizes: project titles 9pt (larger), labels 8pt (smaller)
- Added consistent font styling: labels bold, content light
- All 14 projects updated with new format
- Improved visual hierarchy and readability"

echo ""
echo "=== All Commits Complete ==="

