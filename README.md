# Modular LaTeX Resume Builder

A containerized, data-driven resume and cover letter generation system built with Alpine Linux, LaTeX, and Python. Generate professional resumes from structured YAML data.

## Features

- 🐳 **Containerized Environment**: Alpine Linux-based container with all dependencies
- 📝 **Modular LaTeX Classes**: Custom resume and cover letter classes with clean, professional styling
- 📊 **Data-Driven**: Separate content from presentation using YAML data files
- 🚀 **Easy to Use**: Simple Makefile commands for building documents
- 🎨 **Customizable**: Easy to modify styles and layouts

## Project Structure

```
.
├── .devcontainer/
│   ├── Containerfile          # Alpine Linux container definition
│   └── devcontainer.json      # VSCode devcontainer configuration
├── data/
│   ├── personal.yaml          # Personal information and contact details
│   ├── experience.yaml        # Work experience with keywords for matching
│   ├── education.yaml         # Education and certifications
│   └── skills.yaml            # Technical skills and projects
├── src/
│   ├── resume.cls             # Custom LaTeX resume class
│   └── coverletter.cls        # Custom LaTeX cover letter class
├── scripts/
│   └── generate.py            # Python script for data processing and LaTeX generation
├── output/                    # Generated PDFs and intermediate files
├── Makefile                   # Build automation
└── README.md                  # This file
```

## Quick Start

### Using DevContainer (Recommended)

1. Open this project in VSCode
2. Install the "Dev Containers" extension
3. Click "Reopen in Container" when prompted (or use Command Palette: "Dev Containers: Reopen in Container")
4. Wait for the container to build
5. Run `make resume` to generate your resume

### Manual Setup

If not using DevContainer, ensure you have:
- LaTeX distribution (texlive with xetex)
- Python 3 with PyYAML
- Make

## Usage

### Building a Standard Resume

```bash
make resume
```

This generates `output/resume.pdf` from your YAML data files.

### Building a Cover Letter

```bash
python3 scripts/generate.py \
  --type coverletter \
  --output output/coverletter.tex \
  --company "Tech Company Inc" \
  --position "Senior Software Engineer"

cd output && pdflatex coverletter.tex
```

Or use the Makefile target:

```bash
make coverletter
```

### Cleaning Build Artifacts

```bash
make clean
```

## Customizing Your Resume

### 1. Update Personal Information

Edit `data/personal.yaml`:

```yaml
name: "Your Name"
phone: "+1 (555) 123-4567"
email: "your.email@example.com"
website: "https://yourwebsite.com"
location: "Your City, State"
```

### 2. Add Work Experience

Edit `data/experience.yaml`:

```yaml
experiences:
  - company: "Company Name"
    position: "Your Position"
    location: "City, State"
    start_date: "2021-03"
    end_date: "Present"
    keywords: ["python", "kubernetes", "aws"]
    achievements:
      - "Achievement 1"
      - "Achievement 2"
```

**Important**: Add relevant `keywords` to each experience.

### 3. Update Education

Edit `data/education.yaml`:

```yaml
education:
  - institution: "University Name"
    degree: "Bachelor of Science in Computer Science"
    location: "City, State"
    graduation_date: "2020-05"
    gpa: "3.8/4.0"
    honors: "Magna Cum Laude"
```

### 4. Update Skills

Edit `data/skills.yaml`:

```yaml
skills:
  - category: "Programming Languages"
    items: ["Python", "JavaScript", "Go"]
  - category: "Cloud & DevOps"
    items: ["AWS", "Docker", "Kubernetes"]
```

## Customizing LaTeX Styles

### Resume Class (`src/resume.cls`)

The resume class provides these commands:

- `\resumeheader{name}{location}{phone}{email}{website}` - Header section
- `\experience{company}{location}{position}{dates}{achievements}` - Work experience
- `\education{institution}{location}{degree}{date}` - Education entry
- `\skillsection{category}{items}` - Skills section
- `\project{name}{date}{description}` - Project entry

### Cover Letter Class (`src/coverletter.cls`)

The cover letter class provides:

- `\sender{name}{address}{phone}{email}{website}` - Your information
- `\recipient{name}{title}{company}{address}` - Recipient information
- `\opening{salutation}` - Letter opening
- `\closing{closing}` - Letter closing

### Modifying Colors

Edit the color definitions in `src/resume.cls`:

```latex
\definecolor{primarycolor}{RGB}{0,102,204}  % Change to your preferred color
```

## Advanced Usage

### Direct Python Script Usage

```bash
# Generate resume
python3 scripts/generate.py \
  --type resume \
  --output output/resume.tex

# Generate cover letter
python3 scripts/generate.py \
  --type coverletter \
  --output output/coverletter.tex \
  --company "Company Name" \
  --position "Job Title"
```

### Compile LaTeX Manually

```bash
cd output
pdflatex resume.tex
pdflatex resume.tex  # Run twice for proper references
```

## Troubleshooting

### LaTeX Errors

If you encounter LaTeX compilation errors:

1. Check for special characters in your YAML files (they should be auto-escaped)
2. Ensure all required fields are present in YAML files
3. Run `pdflatex` twice to resolve references
4. Check the `.log` file in the output directory for detailed errors

### Container Issues

If the container fails to build:

1. Ensure Docker is running
2. Try rebuilding: "Dev Containers: Rebuild Container"
3. Check that you have sufficient disk space

### Missing Dependencies

If running outside the container:

```bash
# Alpine Linux
apk add texlive texlive-xetex python3 py3-yaml make

# Ubuntu/Debian
apt-get install texlive-xetex python3 python3-yaml make

# macOS
brew install texlive python3
pip3 install pyyaml
```

## Tips for Best Results

1. **Keep it concise**: Aim for 1-2 pages maximum
2. **Use action verbs**: Start achievements with strong action verbs
3. **Quantify achievements**: Include numbers and metrics where possible
4. **Update keywords**: Keep experience keywords current with industry terms
5. **Proofread**: Always review the generated PDF before sending

## Contributing

Feel free to customize this system for your needs. Some ideas:

- Add more LaTeX styling options
- Implement more sophisticated keyword matching (NLP)
- Add support for multiple resume templates
- Create a web interface for data entry
- Add PDF metadata for ATS compatibility

## License

This project is provided as-is for personal and professional use.

## Support

For issues or questions:
1. Check the troubleshooting section above
2. Review the example YAML files for proper formatting
3. Examine the generated `.log` files for LaTeX errors
