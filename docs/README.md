# Documentation Index

This directory contains comprehensive documentation for the resume/CV system.

## For Human Users

**[USER_GUIDE.md](USER_GUIDE.md)** - Complete user guide covering:
- Quick start and build commands
- Document structure and organization
- Content editing guidelines
- Styling customization
- Troubleshooting common issues
- Professional writing tips

Start here if you're editing content or customizing the layout.

## For AI Assistants

**[AI_CONTEXT.md](AI_CONTEXT.md)** - Context preservation document for AI systems:
- System architecture and build pipeline
- File locations and their purposes
- Recent changes history (with dates)
- Known issues and resolutions
- Content style guidelines
- Build commands and workflows
- Context recovery checklist

AI assistants should read this file first to quickly understand the project state and avoid common pitfalls.

## Technical Documentation

**[FORMATTING_GUIDE.md](FORMATTING_GUIDE.md)** - LaTeX formatting details:
- Custom command reference
- Spacing and layout values
- Font specifications
- Color definitions

**[MODULARIZATION.md](MODULARIZATION.md)** - System architecture:
- Module organization
- Build system design
- Data flow

**[PROJECT_MARGIN_NOTES.md](PROJECT_MARGIN_NOTES.md)** - Margin notes implementation:
- How margin notes work
- Customization options
- Troubleshooting alignment

**[MARKDOWN_SUPPORT.md](MARKDOWN_SUPPORT.md)** - Org-mode syntax guide:
- Supported org-mode features
- LaTeX export configuration
- Special formatting

## Quick Reference

### Build Commands
```bash
make vita        # Build full vita → output/prem-mallappa-vita.pdf
make cv          # Build 2-page CV → output/prem-mallappa-cv.pdf
make clean       # Remove build artifacts
```

### Key Files to Edit
- `org/sections/experience.org` - Work history
- `org/sections/projects.org` - Detailed projects
- `org/sections/awards.org` - Awards and recognition
- `org/sections/skills.org` - Technical skills
- `org/sections/education.org` - Education

### Output Location
All PDFs are generated in `output/` directory.

## Document History

- **2025-11-29**: Created comprehensive documentation structure
  - Added USER_GUIDE.md for human users
  - Added AI_CONTEXT.md for AI assistants
  - Updated Makefile to output to output/ directory
  - Fixed awards description alignment
  - Completed professional content rewrite

## Getting Started

1. **Humans**: Read [USER_GUIDE.md](USER_GUIDE.md)
2. **AI Assistants**: Read [AI_CONTEXT.md](AI_CONTEXT.md)
3. **Build**: Run `make vita`
4. **View**: Open `output/prem-mallappa-vita.pdf`

## Contributing

When making significant changes:
1. Update relevant documentation
2. Update AI_CONTEXT.md with date and description
3. Test build: `make clean && make vita`
4. Commit with descriptive message
