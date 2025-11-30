# Pawesome-CV Modularization Plan

## Goals
- Promote `texmf/tex/latex/pawesome-cv` as the canonical class/module tree.
- Replace the legacy `assets/style` search path with a proper TEXMF root so XeLaTeX loads `pawesome-cv.cls` automatically.
- Align the Org export backends so each `CV_ENV` emitted in `org/sections/*.org` maps to a dedicated LaTeX macro or environment from the modular packages (`cv-experience`, `cv-projects`, `cv-opensource`, etc.).
- Preserve backward compatibility enough that existing exports still build while we migrate content.

## Current State Recap
- `pawesome-cv.cls` already exists under `texmf/tex/latex/pawesome-cv/` and requires the decomposed `cv-*.sty` files, but the build scripts continue to expose only `assets/style` via `TEXINPUTS`.
- The Org exporters (`elisp/org-cv/ox-awesomecv.el`, `elisp/org-cv/ox-awesomecv2.el`, plus the `ox-ext` overrides) still emit the legacy AwesomeCV macros (`\cventry`, `\cvsubentry`, etc.). None of the new section helpers defined in `cv-experience.sty`, `cv-projects.sty`, or `cv-opensource.sty` are reachable.
- Org content uses enriched `CV_ENV` values (`cvemployer`, `cvrole`, `cvproject`, `opensourceentry`, …) which are currently collapsed back to legacy markup, yielding formatting drift (paragraph vs bullet inconsistencies, no margin data, etc.).
- Documentation (`docs/AI_CONTEXT.md`, `docs/MODULARIZATION.md`) still points contributors at `assets/style`.

## Proposed Architecture
1. **TEXMF Root Wiring**
   - Treat `$(CURDIR)/texmf` as `TEXMFHOME` for both XeLaTeX and BibLaTeX lookups.
   - Update `Makefile` Vita/CV targets to prefix commands with `TEXMFHOME=$(CURDIR)/texmf` (or export before running `xelatex`) so `\documentclass{pawesome-cv}` resolves without `TEXINPUTS` hacks.
   - Keep `assets/style` temporarily for regression comparison but mark it deprecated in docs.

2. **Class/File Ownership**
   - `pawesome-cv.cls`: solely responsible for loading modular packages; no duplicate copy under `assets/style` once migration completes.
   - `cv-core.sty`, `cv-colors.sty`, `cv-layout.sty`, `cv-styles.sty`: shared foundations (packages, spacing, palette, fonts) referenced by all downstream components.
   - Section-specific packages (`cv-commands`, `cv-experience`, `cv-projects`, `cv-opensource`, `cv-header-footer`) expose semantic macros that the exporter should call directly. Future sections (awards, education) can live in dedicated files to keep parity.

3. **Exporter Alignment**
   - Extend `ox-awesomecv-ext-headline.el` (and base backends) with dispatch tables:
     - `cvemployer` → `\experienceemployer{company}{from}{to}{location}`.
     - `cvrole` → `\experiencerole{title}`.
     - `cvproject` → `\projectentry{project}{start}{end}{description + margin note hooks}`; parse Org properties (`PROJECT`, `DATE`, `LANGUAGES`, `TOOLS`, `TECH`) to generate optional margin notes via `\marginnote{}` helpers (existing macros in `cv-styles.sty`).
     - `opensourceentry` → `\opensourceproject{project}{start}{end}{url}` plus subsequent bullet list mapped to repeated `\opensourceentry{…}` commands for individual contributions.
     - `cvsubsection`, `cventryshort`, `cvskills`, etc. continue to use existing macros but should be moved into dedicated packages if we introduce `.sty` specializations later.
   - Ensure exporters request `\documentclass{pawesome-cv}` (either by renaming the class entry or adding a new class and updating `org-cv-init.el`).

4. **Org Property Mapping**
   - Normalize date handling: keep `FROM`/`TO` pairs for employers/roles; allow `DATE` string for projects/open-source (fallback to property if both missing).
   - Introduce helper functions in exporter utilities to format property lists into LaTeX key/value strings (e.g., `LANGUAGES`, `TOOLS`, `TECH` displayed as margin note blocks or bold labels inside the description).

5. **Documentation & Tooling**
   - Update `docs/AI_CONTEXT.md` and `docs/MODULARIZATION.md` once implementation lands, emphasizing the new TEXMF workflow and exporter mappings.
   - Document developer workflow: “Place new style files under `texmf/tex/latex/pawesome-cv`, run `make vita` which automatically picks them up.”

## Implementation Sequence
1. **Bootstrap TEXMF wiring**
   - Modify `Makefile` to set `TEXMFHOME=$(CURDIR)/texmf` (and drop `assets/style` from `TEXINPUTS`).
   - Verify `xelatex` locates `pawesome-cv.cls`.

2. **Exporter updates**
   - Update `org-cv-init.el` to prefer the new class.
   - Teach `ox-awesomecv-ext-headline.el` (and base backend if necessary) to emit the new macros per `CV_ENV` type.
   - Add helper functions to format project/employer data, including margin notes.

3. **LaTeX refinements**
   - If any section requires extra macros (e.g., short stints), create dedicated `.sty` files or extend existing ones, keeping code commented per the repo guidelines.

4. **Validation**
   - Rebuild vita/CV via Podman (`make vita`, `make cv`).
   - Compare PDFs against previous outputs for regressions (fonts, alignment, pagination).

5. **Documentation**
   - Refresh `docs/AI_CONTEXT.md`, `docs/MODULARIZATION.md`, and any contributor guides with the new structure, notes on deprecated paths, and exporter behavior.

## Risks & Mitigations
- **Risk:** Org exporter changes could break existing sections if a `CV_ENV` tag is missing.
  - *Mitigation:* Provide safe fallbacks to legacy macros when required properties are absent.
- **Risk:** Setting `TEXMFHOME` might interfere with system-wide TeX installations.
  - *Mitigation:* Scope `TEXMFHOME` to the make invocation (`TEXMFHOME=$(CURDIR)/texmf TEXINPUTS=... xelatex`).
- **Risk:** Margin note rendering may overflow if we auto-generate content.
  - *Mitigation:* Keep initial implementation conservative—reuse existing manual `#+latex` descriptions until we can safely convert them.

This plan provides the blueprint for the next todo items (implementation, documentation updates, build validation).
