"""
LaTeX Formatters Module
Handles LaTeX-specific formatting and escaping with Markdown support
"""

from datetime import datetime
import re


class MarkdownToLaTeX:
    """Convert Markdown formatting to LaTeX"""
    
    @staticmethod
    def convert(text):
        """Convert markdown text to LaTeX with comprehensive formatting support"""
        if not isinstance(text, str):
            return text
        
        # Store original text for debugging
        original = text
        
        # Step 1: Protect URLs and special patterns from escaping
        url_pattern = r'(https?://[^\s\)]+)'
        urls = re.findall(url_pattern, text)
        for i, url in enumerate(urls):
            text = text.replace(url, f'\x00URL{i}\x00')
        
        # Step 2: Protect markdown formatting markers
        # Links: [text](url) -> placeholder
        link_pattern = r'\[([^\]]+)\]\(([^\)]+)\)'
        links = re.findall(link_pattern, text)
        for i, (link_text, link_url) in enumerate(links):
            text = text.replace(f'[{link_text}]({link_url})', f'\x00LINK{i}\x00')
        
        # Inline code: `code` -> placeholder
        inline_code_pattern = r'`([^`]+)`'
        inline_codes = re.findall(inline_code_pattern, text)
        for i, code in enumerate(inline_codes):
            text = text.replace(f'`{code}`', f'\x00CODE{i}\x00')
        
        # Bold: **text** or __text__ -> placeholder
        bold_pattern = r'(\*\*|__)(.+?)\1'
        bolds = re.findall(bold_pattern, text)
        for i, (marker, bold_text) in enumerate(bolds):
            text = text.replace(f'{marker}{bold_text}{marker}', f'\x00BOLD{i}\x00')
        
        # Italic: *text* or _text_ -> placeholder (but not ** or __)
        italic_pattern = r'(?<!\*)\*(?!\*)([^\*]+)\*(?!\*)|(?<!_)_(?!_)([^_]+)_(?!_)'
        italics = []
        for match in re.finditer(italic_pattern, text):
            italic_text = match.group(1) or match.group(2)
            italics.append(italic_text)
            text = text.replace(match.group(0), f'\x00ITALIC{len(italics)-1}\x00', 1)
        
        # Step 3: Escape special LaTeX characters
        # Note: We need to escape backslash first to avoid double-escaping
        text = text.replace('\\', r'\textbackslash{}')
        
        replacements = {
            '&': r'\&',
            '%': r'\%',
            '$': r'\$',
            '#': r'\#',
            '_': r'\_',
            '{': r'\{',
            '}': r'\}',
            '~': r'\textasciitilde{}',
            '^': r'\^{}',
        }
        for old, new in replacements.items():
            text = text.replace(old, new)
        
        # Step 4: Restore and convert markdown to LaTeX
        # Restore URLs
        for i, url in enumerate(urls):
            # Escape special chars in URL for LaTeX
            safe_url = url.replace('#', r'\#').replace('%', r'\%').replace('_', r'\_')
            text = text.replace(f'\x00URL{i}\x00', f'\\url{{{safe_url}}}')
        
        # Restore links
        for i, (link_text, link_url) in enumerate(links):
            # Recursively process link text for nested formatting
            processed_text = MarkdownToLaTeX.convert(link_text)
            safe_url = link_url.replace('#', r'\#').replace('%', r'\%').replace('_', r'\_')
            text = text.replace(f'\x00LINK{i}\x00', f'\\href{{{safe_url}}}{{{processed_text}}}')
        
        # Restore inline code
        for i, code in enumerate(inline_codes):
            # Escape special chars in code
            safe_code = code.replace('\\', r'\textbackslash{}')
            text = text.replace(f'\x00CODE{i}\x00', f'\\texttt{{{safe_code}}}')
        
        # Restore bold
        for i, (marker, bold_text) in enumerate(bolds):
            # Recursively process bold text for nested formatting
            processed_text = MarkdownToLaTeX.convert(bold_text)
            text = text.replace(f'\x00BOLD{i}\x00', f'\\textbf{{{processed_text}}}')
        
        # Restore italic
        for i, italic_text in enumerate(italics):
            # Recursively process italic text for nested formatting
            processed_text = MarkdownToLaTeX.convert(italic_text)
            text = text.replace(f'\x00ITALIC{i}\x00', f'\\textit{{{processed_text}}}')
        
        return text
    
    @staticmethod
    def convert_list(items):
        """Convert a list of markdown items to LaTeX itemize"""
        if not items:
            return ''
        
        latex_items = []
        for item in items:
            # Convert markdown in each item
            converted = MarkdownToLaTeX.convert(item)
            latex_items.append(f'  \\item {converted}')
        
        return '\\begin{itemize}\n' + '\n'.join(latex_items) + '\n\\end{itemize}'
    
    @staticmethod
    def convert_paragraph(text):
        """Convert a paragraph with potential line breaks"""
        if not isinstance(text, str):
            return text
        
        # Split by double newlines for paragraphs
        paragraphs = text.split('\n\n')
        converted_paragraphs = []
        
        for para in paragraphs:
            # Convert single newlines to LaTeX line breaks
            para = para.replace('\n', ' \\\\\n')
            # Convert markdown formatting
            converted = MarkdownToLaTeX.convert(para)
            converted_paragraphs.append(converted)
        
        return '\n\n'.join(converted_paragraphs)


class LaTeXFormatter:
    """Formats data for LaTeX output with Markdown support"""
    
    @staticmethod
    def escape_text(text):
        """Escape special LaTeX characters and convert markdown formatting"""
        return MarkdownToLaTeX.convert(text)
    
    @staticmethod
    def escape_paragraph(text):
        """Escape and convert paragraph text with markdown"""
        return MarkdownToLaTeX.convert_paragraph(text)
    
    @staticmethod
    def escape_list(items):
        """Convert list items with markdown to LaTeX"""
        return MarkdownToLaTeX.convert_list(items)
    
    @staticmethod
    def format_date(date_str, year_only=False):
        """Format date string for display"""
        if not date_str or date_str.lower() == 'present':
            return 'Present'
        try:
            date = datetime.strptime(date_str, '%Y-%m')
            if year_only:
                return date.strftime('%Y')
            else:
                return date.strftime('%b. %Y')
        except:
            # If it's already just a year, return as-is
            return date_str
    
    @staticmethod
    def split_full_name(full_name):
        """Split full name into first and last name"""
        name_parts = full_name.split()
        if len(name_parts) > 1:
            firstname = ' '.join(name_parts[:-1])
            lastname = name_parts[-1]
        else:
            firstname = full_name
            lastname = ''
        return firstname, lastname
