# Text Formatting Guide for YAML Files

This guide explains how to add text formatting (bold, italic, underline, monospace) in your YAML data files.

## Supported Formatting

The resume builder supports markdown-style formatting markers that are automatically converted to LaTeX commands:

| Marker | Effect | LaTeX Command | Example |
|--------|--------|---------------|---------|
| `**text**` | **Bold** | `\textbf{text}` | `**important**` |
| `*text*` | *Italic* | `\textit{text}` | `*emphasis*` |
| `__text__` | <u>Underline</u> | `\underline{text}` | `__highlight__` |
| `` `text` `` | `Monospace` | `\texttt{text}` | `` `code()` `` |

## Usage Examples

### In Awards Section

```yaml
awards:
  - name: "Technical Achievement Award"
    description: "Recognized for implementing `exp()` function optimization"
    date: "2023"
    location: ""
```

Result: "Recognized for implementing exp() function optimization" (with exp() in monospace)

### In Experience Section

```yaml
achievements:
  - "Developed **high-performance** algorithm using `Python`"
  - "Led team of *5 engineers* to deliver __critical__ features"
  - "Improved performance by **300%** using `Redis` caching"
```

### In Projects Section

```yaml
contributions:
  - "Implemented **real-time** data processing with `Apache Kafka`"
  - "Reduced latency by __50%__ through *optimization*"
```

### Combining Formats

You can combine different formatting styles:

```yaml
description: "Built **scalable** API using `FastAPI` with *async* processing"
```

Result: "Built **scalable** API using `FastAPI` with *async* processing"

## Important Notes

1. **Matching Pairs**: Always use matching pairs of markers
   - ✅ Correct: `**bold**`
   - ❌ Wrong: `**bold*` or `*bold**`

2. **Nesting**: Avoid nesting different formats
   - ✅ Correct: `**bold** and *italic*`
   - ⚠️ Avoid: `**bold *and italic***`

3. **Special Characters**: The formatter handles LaTeX special characters automatically
   - `&`, `%`, `$`, `#`, `_`, `{`, `}`, `~`, `^` are automatically escaped

4. **Code/Functions**: Use backticks for function names, code snippets, or technical terms
   - `` `exp()` ``, `` `Python` ``, `` `API` ``

## Common Use Cases

### Highlighting Technical Terms
```yaml
"Optimized `SQL` queries and implemented `Redis` caching"
```

### Emphasizing Achievements
```yaml
"Increased revenue by **$2M** through *strategic* initiatives"
```

### Function/Method Names
```yaml
"Refactored `calculate_metrics()` to improve performance"
```

### Important Keywords
```yaml
"Led **cross-functional** team in __Agile__ environment"
```

## Where Formatting Works

Text formatting is supported in:
- ✅ Experience achievements
- ✅ Project summaries and contributions
- ✅ Award names and descriptions
- ✅ Publication descriptions
- ✅ Education details
- ✅ Any text field processed by `escape_text()`

## Alternative: Org-mode

If you prefer Org-mode syntax, you could:
1. Write content in `.org` files
2. Use Emacs org-export to generate LaTeX
3. However, the current YAML + markdown approach is simpler and doesn't require Emacs

The markdown-style formatting provides a good balance between:
- **Simplicity**: Easy to write and read
- **Portability**: Works without external tools
- **Flexibility**: Supports common formatting needs
