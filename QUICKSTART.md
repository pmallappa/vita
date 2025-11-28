# Quick Start Guide

Get your resume up and running in 5 minutes!

## Prerequisites

Choose one of these options:

### Option 1: Using DevContainer (Recommended)
- VSCode with "Dev Containers" extension
- Docker installed and running

### Option 2: Local Setup
- Python 3.x
- LaTeX distribution (texlive with xetex)
- Make
- PyYAML (`pip install pyyaml`)

## Step 1: Customize Your Data

Edit the YAML files in the `data/` directory with your information:

### 1. Personal Information (`data/personal.yaml`)
```yaml
name: "Your Name"
phone: "+1 (555) 123-4567"
email: "your.email@example.com"
website: "https://yourwebsite.com"
location: "Your City, State"
```

### 2. Work Experience (`data/experience.yaml`)
Add your work history. **Important**: Include keywords for each position!

```yaml
experiences:
  - company: "Your Company"
    position: "Your Position"
    location: "City, State"
    start_date: "2021-03"
    end_date: "Present"
    keywords: ["python", "aws", "kubernetes"]  # Add relevant keywords!
    achievements:
      - "Your achievement 1"
      - "Your achievement 2"
```

### 3. Education (`data/education.yaml`)
```yaml
education:
  - institution: "Your University"
    degree: "Your Degree"
    location: "City, State"
    graduation_date: "2020-05"
```

### 4. Skills (`data/skills.yaml`)
```yaml
skills:
  - category: "Programming Languages"
    items: ["Python", "JavaScript", "Go"]
```

## Step 2: Generate Your Resume

### Using DevContainer:
1. Open project in VSCode
2. Click "Reopen in Container" when prompted
3. Wait for container to build (first time only)
4. Run: `make resume`

### Using Local Setup:
```bash
make resume
```

Your resume will be generated at `output/resume.pdf`!

## Step 3: Generate a Cover Letter (Optional)

```bash
python3 scripts/generate.py \
  --type coverletter \
  --output output/coverletter.tex \
  --company "Company Name" \
  --position "Job Title"

cd output && pdflatex coverletter.tex
```

## Common Commands

```bash
# Generate standard resume
make resume

# Clean build artifacts
make clean

# Show all available commands
make help
```

## Troubleshooting

### "Command not found: make"
- **Windows**: Install Make via Chocolatey: `choco install make`
- **Mac**: Install via Homebrew: `brew install make`
- **Linux**: `sudo apt-get install make` or `sudo yum install make`

### "pdflatex: command not found"
You need to install LaTeX:
- **Windows**: Install MiKTeX or TeX Live
- **Mac**: `brew install texlive`
- **Linux**: `sudo apt-get install texlive-xetex texlive-latex-extra`

### "No module named 'yaml'"
Install PyYAML:
```bash
pip install pyyaml
# or
pip3 install pyyaml
```

### LaTeX compilation errors
1. Check your YAML files for special characters
2. Run `pdflatex` twice: `cd output && pdflatex resume.tex && pdflatex resume.tex`
3. Check `output/resume.log` for detailed error messages

## Next Steps

1. **Customize the style**: Edit `src/resume.cls` to change colors and formatting
2. **Add more data**: Include projects, certifications, awards in your YAML files
3. **Read the full README**: Check `README.md` for advanced features

## Tips for Success

✅ **DO:**
- Keep achievements concise and quantifiable
- Use action verbs (Led, Developed, Implemented, etc.)
- Add relevant keywords to each experience
- Update your data regularly

❌ **DON'T:**
- Use special characters without escaping (the script handles this)
- Make your resume longer than 2 pages
- Forget to proofread the generated PDF
- Skip the keywords in experience.yaml

## Getting Help

- Check `README.md` for detailed documentation
- Examine the example YAML files for proper formatting
- Look at `output/*.log` files for LaTeX errors

---

**Ready to go?** Start by editing `data/personal.yaml` with your information, then run `make resume`!
