"""
Resume Generator Module
Main generator class that orchestrates resume creation
"""

from .loaders import YAMLDataLoader
from .section_builders import ResumeSectionBuilder
from .formatters import LaTeXFormatter


class ResumeGenerator:
    """Main resume generator class - orchestrates all components"""
    
    def __init__(self, data_dir='data'):
        self.data_loader = YAMLDataLoader(data_dir)
        self.section_builder = ResumeSectionBuilder()
        self.formatter = LaTeXFormatter()
        
        # Load all data
        data = self.data_loader.load_all_data()
        self.personal = data['personal']
        self.experience = data['experience']
        self.education = data['education']
        self.skills = data['skills']
        self.projects = data['projects']
        self.publications = data['publications']
        self.awards = data['awards']
        self.opensource = data.get('opensource', {})
    
    def generate_resume_latex(self, job_desc=None):
        """Generate complete LaTeX resume document"""
        latex = []
        
        # Document preamble
        latex.extend(self._build_document_preamble())
        
        # Header and personal info
        latex.extend(self.section_builder.build_header(self.personal))
        
        # Main sections
        latex.extend(self.section_builder.build_experience_section(self.experience))
        latex.extend(self.section_builder.build_opensource_section(self.opensource))
        latex.extend(self.section_builder.build_skills_section(self.skills))
        latex.extend(self.section_builder.build_education_section(self.education))
        latex.extend(self.section_builder.build_awards_section(self.awards))
        latex.extend(self.section_builder.build_publications_section(self.publications))
        latex.extend(self.section_builder.build_projects_section(self.projects))
        
        # Document end
        latex.append(r'\end{document}')
        
        return '\n'.join(latex)
    
    def generate_coverletter_latex(self, company_name, job_title, job_desc=None):
        """Generate cover letter document"""
        latex = []
        
        # Document preamble
        latex.append(r'\documentclass[11pt,a4paper]{awesome-cv}')
        latex.append(r'\fontdir[assets/fonts/]')
        latex.append(r'\begin{document}')
        latex.append('')
        
        # Personal info
        p = self.personal
        firstname, lastname = self.formatter.split_full_name(p['name'])
        
        latex.append(r'\name{%s}{%s}' % (
            self.formatter.escape_text(firstname),
            self.formatter.escape_text(lastname)
        ))
        latex.append(r'\address{%s}' % self.formatter.escape_text(p['location']))
        latex.append(r'\mobile{%s}' % self.formatter.escape_text(p['phone']))
        latex.append(r'\email{%s}' % p['email'])
        latex.append('')
        
        # Letter details
        latex.append(r'\recipient{%s}{%s}' % (self.formatter.escape_text(company_name), ''))
        latex.append(r'\letterdate{\today}')
        latex.append(r'\letteropening{Dear Hiring Manager,}')
        latex.append(r'\letterclosing{Sincerely,}')
        latex.append('')
        
        # Letter content
        latex.append(r'\makecvheader')
        latex.append(r'\makelettertitle')
        latex.append('')
        latex.append(r'\begin{cvletter}')
        latex.append(
            f'I am writing to express my interest in the {self.formatter.escape_text(job_title)} '
            f'position at {self.formatter.escape_text(company_name)}.'
        )
        latex.append(r'\end{cvletter}')
        latex.append('')
        latex.append(r'\makeletterclosing')
        latex.append(r'\end{document}')
        
        return '\n'.join(latex)
    
    def _build_document_preamble(self):
        """Build LaTeX document preamble"""
        # Get author name for footer
        author_name = self.personal.get('name', '')
        
        return [
            r'\documentclass[11pt,a4paper]{awesome-cv}',
            '',
            r'\fontdir[assets/fonts/]',
            r'\colorlet{awesome}{awesome-skyblue}',
            '',
            r'\begin{document}',
            '',
            r'\makecvfooter{\today}{%s}{\thepage}' % self.formatter.escape_text(author_name),
            ''
        ]
