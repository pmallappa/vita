# Markdown Support in Resume Builder

The resume builder now supports Markdown formatting in YAML data files, which is automatically converted to LaTeX during PDF generation.

## Supported Markdown Features

### 1. Text Formatting

#### Bold Text
Use `**text**` or `__text__` for bold formatting.

```yaml
description: "Developed **high-performance** algorithms"
```
Converts to: `\textbf{high-performance}`

#### Italic Text
Use `*text*` or `_text_` for italic formatting.

```yaml
description: "Implemented *novel* approach to optimization"
```
Converts to: `\textit{novel}`

#### Inline Code
Use backticks for inline code/monospace text.

```yaml
description: "Optimized `memcpy()` function for AMD processors"
```
Converts to: `\texttt{memcpy()}`

### 2. Links

#### Markdown Links
Use `[text](url)` syntax for hyperlinks.

```yaml
description: "Contributed to [QEMU](https://www.qemu.org/) mainline"
```
Converts to: `\href{https://www.qemu.org/}{QEMU}`

#### Plain URLs
Plain URLs are automatically converted to clickable links.

```yaml
website: "https://github.com/pmallappa"
```
Converts to: `\url{https://github.com/pmallappa}`

### 3. Nested Formatting

You can combine multiple formatting styles:

```yaml
description: "Achieved **[3x performance](https://example.com/benchmark)** improvement"
```
Converts to: `\textbf{\href{https://example.com/benchmark}{3x performance}}`

```yaml
description: "Implemented **_critical_** security patches"
```
Converts to: `\textbf{\textit{critical}}`

### 4. Lists

Lists in YAML are automatically converted to LaTeX itemize environments:

```yaml
achievements:
  - "**Optimized** performance by 30%"
  - "Developed [new algorithm](https://example.com)"
  - "Fixed `critical` bug in production"
```

Converts to:
```latex
\begin{itemize}
  \item \textbf{Optimized} performance by 30%
  \item Developed \href{https://example.com}{new algorithm}
  \item Fixed \texttt{critical} bug in production
\end{itemize}
```

## Usage Examples

### Experience Section

```yaml
companies:
  - name: "AMD Ltd."
    positions:
      - title: "Principal Engineer"
        achievements:
          - "**AMD Math Library**: Optimizing exponential/power/logarithmic functions"
          - "Improved performance of `exp()` and `log()` by **30%**"
          - "Developed *dynamic dispatcher* using `cpuid` instruction"
          - "Achieved **3x** performance improvement in _CFB-based_ parallel decrypting"
```

### Projects Section

```yaml
projects:
  - name: "SMMUv3 QEMU Emulation"
    description:
      - "Designed and developed **SMMUv3 model** for [QEMU](https://www.qemu.org/)"
      - "Implemented *command queue*, `STE/CD` parsing, and pagetable walk"
      - "Supported **Stage1**, **Stage2**, and _nested virtualization_"
```

### Skills Section

```yaml
skills:
  - category: "Programming Languages"
    items:
      - "**Proficient**: C, Assembly (x86, ARM, MIPS)"
      - "**Experienced**: Rust, Python, C++"
      - "**Familiar**: Go, Shell, Haskell"
```

### Awards Section

```yaml
awards:
  - name: "Spotlight Award"
    description: "Providing important patches to **GLIBC** to fix `memcpy` behaviour on AMD"
```

## Special Characters

The following special characters are automatically escaped for LaTeX:

- `&` → `\&`
- `%` → `\%`
- `$` → `\$`
- `#` → `\#`
- `_` → `\_`
- `{` → `\{`
- `}` → `\}`
- `~` → `\textasciitilde{}`
- `^` → `\^{}`
- `\` → `\textbackslash{}`

You don't need to escape these manually in your YAML files - the formatter handles it automatically.

## Best Practices

### 1. Use Markdown for Emphasis
Instead of writing plain text, use markdown to highlight important information:

**Before:**
```yaml
description: "Optimized performance by 30%"
```

**After:**
```yaml
description: "Optimized performance by **30%**"
```

### 2. Link to External Resources
Add links to projects, publications, or documentation:

```yaml
description: "Contributed to [Linux Kernel](https://kernel.org) IOMMU subsystem"
```

### 3. Use Code Formatting for Technical Terms
Highlight function names, commands, and technical terms:

```yaml
description: "Fixed critical bug in `kexec` implementation"
```

### 4. Combine Formatting for Impact
Use multiple formatting styles to create emphasis:

```yaml
description: "Achieved **_unprecedented_** performance gains using `SIMD` instructions"
```

## Implementation Details

The markdown conversion happens in `scripts/resume_builder/formatters.py`:

1. **MarkdownToLaTeX.convert()**: Converts inline markdown formatting
2. **MarkdownToLaTeX.convert_list()**: Converts lists with markdown items
3. **MarkdownToLaTeX.convert_paragraph()**: Converts paragraphs with line breaks

The conversion process:
1. Protects markdown patterns from LaTeX escaping
2. Escapes special LaTeX characters
3. Converts markdown patterns to LaTeX commands
4. Handles nested formatting recursively

## Testing

To test markdown formatting in your resume:

1. Add markdown formatting to your YAML files
2. Generate the resume:
   ```bash
   make resume
   ```
3. Check the output PDF for proper formatting

## Limitations

- Code blocks (triple backticks) are not supported - use inline code instead
- Tables are not supported
- Headers (#, ##, etc.) are not supported in content (use YAML structure instead)
- Images are not supported

## Future Enhancements

Potential additions:
- Support for strikethrough text (`~~text~~`)
- Support for subscript and superscript
- Support for definition lists
- Support for footnotes
- Custom color formatting

## Troubleshooting

### Bold/Italic Not Working

Make sure you're using the correct syntax:
- Bold: `**text**` (two asterisks)
- Italic: `*text*` (one asterisk)

### Links Not Rendering

Check that your link syntax is correct:
```yaml
[link text](https://url.com)
```

### Special Characters Appearing Literally

If you see `\&` or `\_` in your PDF, the text might not be going through the formatter. Check that you're using the correct YAML field names.

## Contributing

To improve markdown support:
1. Edit `scripts/resume_builder/formatters.py`
2. Add new patterns to `MarkdownToLaTeX.convert()`
3. Test with various markdown combinations
4. Update this documentation

## Examples in This Resume

Check the existing YAML files for examples:
- `data/experience.yaml` - Bold text in achievements
- `data/projects.yaml` - Code formatting for technical terms
- `data/skills.yaml` - Nested formatting in descriptions
