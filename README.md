# Resume / Curriculum Vitae

Automated LaTeX-based resume generation using Org-mode and LuaLaTeX.

## 📄 Download Latest Resume

**[Download Latest PDF →](https://github.com/pmallappa/vita/releases/latest/download/prem-mallappa-vita.pdf)**

View all builds: [Releases](https://github.com/pmallappa/vita/releases)

## 🏗️ Build System

This project uses a containerized build system with Podman/Docker to ensure consistent output across environments.

### Prerequisites

- Podman or Docker
- Make (optional, for convenience)

### Building Locally

Using the pre-built image:
```bash
podman run --rm -v ${PWD}:/workspace -w /workspace localhost/resume-builder make vita
```

Or build the container first:
```bash
# Build the container image
podman build -t resume-builder -f .devcontainer/Containerfile .

# Generate the resume
podman run --rm -v ${PWD}:/workspace -w /workspace resume-builder make vita
```

Output will be in `outputs/prem-mallappa-vita.pdf`

### Build Targets

- `make vita` - Full CV/Vita (10+ pages)
- `make cv` - Condensed CV (2 pages)
- `make resume` - Python-generated resume (alternative format)

## 🤖 Automated Builds

GitHub Actions automatically builds and releases the resume:

- **On every push to `main`** with changes to content or build files
- **Weekly** every Monday at 9 AM UTC
- **Manually** via the Actions tab

The last 5 builds are available in [Releases](https://github.com/pmallappa/vita/releases).

## 📂 Project Structure

```
├── org/sections/          # Content in Org-mode format
│   ├── experience.yaml
│   ├── projects.yaml
│   ├── education.yaml
│   └── ...
├── texmf/                 # Custom LaTeX class (pawesome-cv)
├── assets/fonts/          # Custom fonts (Nokia Sans, etc.)
├── .devcontainer/         # Container build definition
├── Makefile              # Build automation
└── outputs/              # Generated PDFs
```

## 🎨 Technology Stack

- **Content**: Org-mode with YAML metadata
- **LaTeX Engine**: LuaLaTeX (LuaHBTeX)
- **Document Class**: pawesome-cv (custom modular system)
- **Fonts**: Nokia Sans, Source Sans Pro, FontAwesome5
- **Build Environment**: Alpine Linux + TeX Live 2024

## 📝 Making Changes

1. Edit content files in `org/sections/`
2. Build locally to verify changes
3. Commit and push to `main`
4. GitHub Actions automatically builds and releases the PDF

## 📜 License

This resume is personal content. The LaTeX template and build system are available for reference.
