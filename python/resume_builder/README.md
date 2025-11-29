# Resume Builder Package

A modular Python package for generating LaTeX resumes from YAML data with improved organization and naming conventions.

## Package Structure

```
resume_builder/
├── __init__.py              # Package initialization and exports
├── loaders.py               # YAML data loading (YAMLDataLoader)
├── formatters.py            # LaTeX formatting utilities (LaTeXFormatter)
├── section_builders.py      # Section generators (ResumeSectionBuilder)
├── resume_generator.py      # Main orchestrator (ResumeGenerator)
└── README.md               # This file
```

## Modules

### loaders.py
**Class:** `YAMLDataLoader`

Handles loading and validation of YAML data files.

**Methods:**
- `load_file(filename)`: Load a single YAML file
- `load_all_data()`: Load all resume data files

**Supported Data Files:**
- personal.yaml
- experience.yaml
- education.yaml
- skills.yaml
- projects.yaml
- publications.yaml
- awards.yaml

### formatters.py
**Class:** `LaTeXFormatter`

Provides LaTeX formatting utilities.

**Methods:**
- `escape_text(text)`: Escape special LaTeX characters
- `format_date(date_str)`: Format dates for display (e.g., "2024-01" → "Jan. 2024")
- `split_full_name(full_name)`: Split full name into first and last names

### section_builders.py
**Class:** `ResumeSectionBuilder`

Generates LaTeX code for different resume sections.

**Methods:**
- `build_header(personal_data)`: Personal information and header
- `build_experience_section(experience_data)`: Work experience entries
- `build_skills_section(skills_data)`: Skills section
- `build_education_section(education_data)`: Education entries
- `build_awards_section(awards_data)`: Awards and honors
- `build_publications_section(publications_data)`: Publications section
- `build_projects_section(projects_data)`: Project entries with technology notes

### resume_generator.py
**Class:** `ResumeGenerator`

Main orchestrator that coordinates all components.

**Methods:**
- `generate_resume_latex(job_desc=None)`: Generate complete resume
- `generate_coverletter_latex(company_name, job_title, job_desc=None)`: Generate cover letter

## Usage

```python
from resume_builder import ResumeGenerator

# Initialize generator with data directory
generator = ResumeGenerator(data_dir='data')

# Generate resume
resume_latex = generator.generate_resume_latex()

# Generate cover letter
coverletter_latex = generator.generate_coverletter_latex(
    company_name="Company Name",
    job_title="Position Title"
)
```

## Naming Conventions

The package follows Python naming best practices:

- **Modules**: lowercase with underscores (e.g., `section_builders.py`)
- **Classes**: PascalCase (e.g., `ResumeSectionBuilder`)
- **Methods**: lowercase with underscores (e.g., `build_header()`)
- **Variables**: lowercase with underscores (e.g., `data_loader`)

## Benefits

1. **Clear Naming**: Descriptive names that indicate purpose
2. **Separation of Concerns**: Each module has a single responsibility
3. **Easy Maintenance**: Changes to one aspect don't affect others
4. **Testability**: Individual modules can be tested independently
5. **Extensibility**: New sections or formatters can be added easily
6. **Pythonic**: Follows PEP 8 style guidelines
