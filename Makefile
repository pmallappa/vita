.PHONY: all resume coverletter vita cv clean help

# Default target
all: resume

# Build resume
resume:
	@echo "Building resume..."
	@python3 scripts/generate.py --type resume --output output/resume.tex
	@cd output && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex resume.tex
	@cd output && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex resume.tex
	@echo "Resume built: output/resume.pdf"

# Build cover letter
coverletter:
	@echo "Building cover letter..."
	@python3 scripts/generate.py --type coverletter --output output/coverletter.tex
	@cd output && TEXINPUTS=../assets/style//:$$TEXINPUTS pdflatex coverletter.tex
	@cd output && TEXINPUTS=../assets/style//:$$TEXINPUTS pdflatex coverletter.tex
	@echo "Cover letter built: output/coverletter.pdf"

# Build vita from org-mode file
vita:
	@echo "Building vita from org-mode..."
	@emacs --batch \
			--load elisp/org-cv-init.el \
			--eval "(setq debug-on-error nil inhibit-debug-on-quit t)" \
		--visit=org/prem-mallappa-vita.org \
		--eval "(org-export-to-file 'awesomecv \"$(CURDIR)/org/prem-mallappa-vita.tex\")"
	@cd org && sed -i 's/\\n\[NO-DEFAULT-PACKAGES\]//g;/\\usepackage\[normalem]{ulem}/d;/\\usepackage{amssymb}/d' prem-mallappa-vita.tex
	@cd org && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-vita.tex
	@cd org && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-vita.tex
	@echo "Vita built: org/prem-mallappa-vita.pdf"

# Build 2-page CV from org-mode file
cv:
	@echo "Building CV from org-mode..."
	@emacs --batch \
			--load elisp/org-cv-init.el \
			--eval "(setq debug-on-error nil inhibit-debug-on-quit t)" \
		--visit=org/prem-mallappa-cv.org \
		--eval "(org-export-to-file 'awesomecv \"$(CURDIR)/org/prem-mallappa-cv.tex\")"
	@cd org && sed -i 's/\\n\[NO-DEFAULT-PACKAGES\]//g;/\\usepackage\[normalem]{ulem}/d;/\\usepackage{amssymb}/d' prem-mallappa-cv.tex
	@cd org && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-cv.tex
	@cd org && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-cv.tex
	@echo "CV built: org/prem-mallappa-cv.pdf"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f output/*.aux output/*.log output/*.out output/*.tex output/*.pdf
	@rm -f org/*.aux org/*.log org/*.out org/*.tex org/*.pdf
	@echo "Clean complete"

# Show help
help:
	@echo "Available targets:"
	@echo "  make resume       - Build standard resume (Python-based)"
	@echo "  make coverletter  - Build cover letter"
	@echo "  make vita         - Build full vita from org-mode file"
	@echo "  make cv           - Build 2-page CV from org-mode file"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make help         - Show this help message"
