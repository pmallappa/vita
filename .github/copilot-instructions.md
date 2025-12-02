# Copilot Instructions

## Project intent
- Generate polished resumes/CVs from Org-mode sources and the custom pawesome-cv LaTeX class.
- Primary deliverable is `outputs/prem-mallappa-vita.pdf`; `outputs/prem-mallappa-cv.pdf` is a condensed variant.

## Build workflow
- All builds run in the `resume-builder` container (Alpine + TeX Live + Emacs). Never invoke LaTeX directly on the host.
- Rebuild the image with `podman build -t resume-builder -f .devcontainer/Containerfile .` when dependencies change.
- Produce the vita inside the container: `podman run --rm -v ${PWD}:/workspace -w /workspace resume-builder make vita` (similar for `cv`, `resume`, `clean`).
- Local `Makefile` already assumes LuaLaTeX; do not reintroduce XeLaTeX.

## Source of truth
- Content lives under `org/sections/*.org`; the top-level `prem-mallappa-*.org` files only aggregate sections via `#+INCLUDE`.
- Each project entry uses Org description lists with `Synopsis ::` and `Details ::` so the enumitem rules in `cv-commands.sty` can style them.
- Margin notes come from Org properties `:LANGUAGES:`, `:TOOLS:`, `:TECH:` etc.; keep these consistent so `ox-awesomecv-extensions.el` can render them.

## Emacs export layer
- `elisp/org-cv-init.el` adds `elisp/org-cv/` (cloned from gitlab.com/Titan-C/org-cv) and `elisp/ox-ext/` to the load-path, then requires `ox-awesomecv*` backends.
- If export fails with `Cannot open load file, ox-awesomecv`, ensure the container copied `/tmp/org-cv` into `elisp/org-cv/` during image build.
- Extend the export pipeline by editing files in `elisp/ox-ext/`; avoid modifying `elisp/org-cv/**` because it is third-party code.

## LaTeX styling
- `assets/style/pawesome-cv.cls` loads modular files (`cv-core.sty`, `cv-layout.sty`, `cv-commands.sty`, etc.).
- Spacing tweaks usually belong in `assets/style/cv-commands.sty`; typography changes go into `cv-styles.sty` or `fonts.sty`.
- Font assets live in `assets/fonts/`. Keep file names unchanged; the class relies on Nokia Sans and Source Sans Pro.

## Generated artifacts
- Everything under `outputs/` is auto-generated; never hand-edit `.tex` or `.pdf` files there.
- Run `make clean` (within the container) before committing if intermediate `.aux/.log` files appear.

## Automation & releases
- `.github/workflows/build-resume.yml` builds the PDF in CI, uploads it as an artifact, and publishes GitHub releases tagged `v${{ github.run_number }}` (workflow-level `permissions: contents: write` is required).
- Releases keep only the last 5 entries via `dev-drprasad/delete-older-releases@v0.3.2`; avoid manual deletions that would break the history.

## Common pitfalls
- Forgetting to run inside the container results in missing Emacs packages or TeX fonts.
- Editing legacy YAML under `python/data/` has no effect—the generator is deprecated in favor of Org-mode.
- Org description lists need a blank line before the block to render correctly; otherwise LaTeX spacing regresses.
