.PHONY: all resume coverletter vita cv clean help

# Default target - build all documents
all: cv vita coverletter

# Build resume
resume:
	@echo "Building resume..."
	@python3 scripts/generate.py --type resume --output outputs/resume.tex
	@cd outputs && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex resume.tex
	@cd outputs && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex resume.tex
	@echo "Resume built: outputs/resume.pdf"

# Build cover letter (dummy target for now)
coverletter:
	@echo "Cover letter target - to be implemented later"

# Build vita from org-mode file
vita:
	@echo "Building vita from org-mode..."
	@emacs --batch \
			--load elisp/org-cv-init.el \
			--eval "(setq debug-on-error nil inhibit-debug-on-quit t)" \
		--visit=org/prem-mallappa-vita.org \
		--eval "(org-export-to-file 'awesomecv \"$(CURDIR)/outputs/prem-mallappa-vita.tex\")"
	@cd outputs && sed -i 's/\\n\[NO-DEFAULT-PACKAGES\]//g;/\\usepackage\[normalem]{ulem}/d;/\\usepackage{amssymb}/d' prem-mallappa-vita.tex
	@cd outputs && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-vita.tex
	@cd outputs && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-vita.tex
	@echo "Vita built: outputs/prem-mallappa-vita.pdf"

# Build 2-page CV from org-mode file
cv:
	@echo "Building CV from org-mode..."
	@emacs --batch \
			--load elisp/org-cv-init.el \
			--eval "(setq debug-on-error nil inhibit-debug-on-quit t)" \
		--visit=org/prem-mallappa-cv.org \
		--eval "(org-export-to-file 'awesomecv \"$(CURDIR)/outputs/prem-mallappa-cv.tex\")"
	@cd outputs && sed -i 's/\\n\[NO-DEFAULT-PACKAGES\]//g;/\\usepackage\[normalem]{ulem}/d;/\\usepackage{amssymb}/d' prem-mallappa-cv.tex
	@cd outputs && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-cv.tex
	@cd outputs && TEXINPUTS=../assets/style//:$$TEXINPUTS xelatex prem-mallappa-cv.tex
	@echo "CV built: outputs/prem-mallappa-cv.pdf"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f outputs/*.aux outputs/*.log outputs/*.out outputs/*.tex outputs/*.pdf
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
