# Resume / Curriculum Vitae

Automated LaTeX-based resume generation using Org-mode and LuaLaTeX.

## 📄 Download Latest Resume

The latest generated PDFs are published by GitHub Pages from the `gh-pages` branch:

- `prem-mallappa-cv.pdf` - condensed CV
- `prem-mallappa-vita.pdf` - full vita

After Pages is enabled for this repository, the public download page is available at:

```text
https://pmallappa.github.io/vita/
```

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

GitHub Actions automatically builds and publishes the resume PDFs:

- **On every push to `main`**
- **Monthly** on the 1st day of the month at 9 AM UTC
- **Manually** via the Actions tab

The workflow builds `make cv` and `make vita`, stages only the generated PDFs plus a small `index.html`, and publishes that static payload to the `gh-pages` branch. The latest PDFs are copied to the site root, each run is archived under `builds/<run>-<sha>/`, and older published builds are pruned after the most recent 7 entries. Matching `resume-pdfs-*` workflow artifacts are also pruned after the most recent 7 entries, and workflow run history is pruned after the most recent 10 runs. Keep `main` private and configure GitHub Pages to publish from `gh-pages` at the repository root.

### GitHub Repository Settings

GitHub privacy is repository-level, not branch-level. To keep the resume source private while publishing the generated PDFs:

1. Change the repository visibility to private.
2. Enable GitHub Pages.
3. Set the Pages source to **Deploy from a branch**.
4. Select the `gh-pages` branch and `/ (root)` folder.
5. Ensure Actions can write repository contents so the workflow can update `gh-pages`.

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
4. GitHub Actions automatically builds the PDFs and publishes them to `gh-pages`

## 📜 License

This resume is personal content. The LaTeX template and build system are available for reference.
