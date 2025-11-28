.PHONY: all resume coverletter clean help

# Default target
all: resume

# Build resume
resume:
	@echo "Building resume..."
	@python3 scripts/generate.py --type resume --output output/resume.tex
	@cd output && TEXINPUTS=../src//:$$TEXINPUTS xelatex resume.tex
	@cd output && TEXINPUTS=../src//:$$TEXINPUTS xelatex resume.tex
	@echo "Resume built: output/resume.pdf"

# Build cover letter
coverletter:
	@echo "Building cover letter..."
	@python3 scripts/generate.py --type coverletter --output output/coverletter.tex
	@cd output && TEXINPUTS=../src//:$$TEXINPUTS pdflatex coverletter.tex
	@cd output && TEXINPUTS=../src//:$$TEXINPUTS pdflatex coverletter.tex
	@echo "Cover letter built: output/coverletter.pdf"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f output/*.aux output/*.log output/*.out output/*.tex output/*.pdf
	@echo "Clean complete"

# Show help
help:
	@echo "Available targets:"
	@echo "  make resume       - Build standard resume"
	@echo "  make coverletter  - Build cover letter"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make help         - Show this help message"
