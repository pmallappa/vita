"""
Section Builders Module
Generates LaTeX code for different resume sections
"""

from .formatters import LaTeXFormatter


class ResumeSectionBuilder:
    """Builds LaTeX code for resume sections"""
    
    def __init__(self):
        self.fmt = LaTeXFormatter()
    
    def build_header(self, personal_data):
        """Build resume header with personal information"""
        latex = []
        p = personal_data
        
        firstname, lastname = self.fmt.split_full_name(p['name'])
        
        latex.append(r'\name{%s}{%s}' % (
            self.fmt.escape_text(firstname),
            self.fmt.escape_text(lastname)
        ))
        
        if 'tagline' in p:
            latex.append(r'\position{%s}' % self.fmt.escape_text(p['tagline']))
        
        latex.append(r'\address{%s}' % self.fmt.escape_text(p['location']))
        latex.append(r'\mobile{%s}' % self.fmt.escape_text(p['phone']))
        latex.append(r'\email{%s}' % p['email'])
        
        if 'github' in p:
            latex.append(r'\github{%s}' % p['github'].split('/')[-1])
        if 'linkedin' in p:
            latex.append(r'\linkedin{%s}' % p['linkedin'].split('/')[-1])
        
        latex.append('')
        latex.append(r'\makecvheader')
        latex.append('')
        
        # Profile/Summary
        if 'summary' in p:
            latex.append(r'\begin{cvparagraph}')
            latex.append(self.fmt.escape_text(p['summary'].strip()))
            latex.append(r'\end{cvparagraph}')
            latex.append('')
        
        return latex
    
    def build_experience_section(self, experience_data):
        """Build experience section"""
        latex = []
        latex.append(r'\cvsection{Experience}')
        latex.append(r'\begin{cventries}')
        
        for company in experience_data.get('companies', []):
            positions = company.get('positions', [])
            
            if len(positions) == 1:
                # Single position
                pos = positions[0]
                desc_items = [
                    r'  \item ' + self.fmt.escape_text(item)
                    for item in pos['achievements']
                ]
                description = r'\begin{cvitems}' + '\n' + '\n'.join(desc_items) + '\n' + r'\end{cvitems}'
                
                latex.append(r'\cventry')
                latex.append(r'  {%s}' % self.fmt.escape_text(company['name']))
                latex.append(r'  {%s}' % self.fmt.escape_text(company['location']))
                latex.append(r'  {%s}' % self.fmt.escape_text(pos['title']))
                latex.append(r'  {%s -- %s}' % (
                    self.fmt.format_date(pos['start_date']),
                    self.fmt.format_date(pos['end_date'])
                ))
                latex.append(r'  {')
                latex.append(description)
                latex.append(r'  }')
            else:
                # Multiple positions
                first_pos = positions[0]
                last_pos = positions[-1]
                
                latex.append(r'\cventry')
                latex.append(r'  {%s}' % self.fmt.escape_text(company['name']))
                latex.append(r'  {%s}' % self.fmt.escape_text(company['location']))
                latex.append(r'  {}')
                latex.append(r'  {%s -- %s}' % (
                    self.fmt.format_date(last_pos['start_date']),
                    self.fmt.format_date(first_pos['end_date'])
                ))
                latex.append(r'  {}')
                
                for pos in positions:
                    desc_items = [
                        r'  \item ' + self.fmt.escape_text(item)
                        for item in pos['achievements']
                    ]
                    description = r'\begin{cvitems}' + '\n' + '\n'.join(desc_items) + '\n' + r'\end{cvitems}'
                    
                    latex.append(r'\cvsubentry')
                    latex.append(r'  {%s}' % self.fmt.escape_text(pos['title']))
                    latex.append(r'  {%s -- %s}' % (
                        self.fmt.format_date(pos['start_date']),
                        self.fmt.format_date(pos['end_date'])
                    ))
                    latex.append(r'  {')
                    latex.append(description)
                    latex.append(r'  }')
        
        # Add short stints section
        if 'short_stints' in experience_data and experience_data['short_stints']:
            latex.append(r'\vspace{5mm}')
            latex.append(r'\cvsubsection{Short Stints}')
            
            for company in experience_data['short_stints']:
                positions = company.get('positions', [])
                if positions:
                    pos = positions[0]
                    latex.append(r'\cventry')
                    latex.append(r'  {%s}' % self.fmt.escape_text(company['name']))
                    latex.append(r'  {%s}' % self.fmt.escape_text(company['location']))
                    latex.append(r'  {%s}' % self.fmt.escape_text(pos['title']))
                    latex.append(r'  {%s -- %s}' % (
                        self.fmt.format_date(pos['start_date']),
                        self.fmt.format_date(pos['end_date'])
                    ))
                    latex.append(r'  {}')
        
        latex.append(r'\end{cventries}')
        latex.append('')
        return latex
    
    def build_skills_section(self, skills_data):
        """Build skills section"""
        latex = []
        latex.append(r'\cvsection{Skills}')
        latex.append(r'\begin{cvskills}')
        
        for skill in skills_data.get('skills', []):
            items = ', '.join([self.fmt.escape_text(item) for item in skill['items']])
            latex.append(r'\cvskill{%s}{%s}' % (
                self.fmt.escape_text(skill['category']),
                items
            ))
        
        latex.append(r'\end{cvskills}')
        latex.append('')
        return latex
    
    def build_education_section(self, education_data):
        """Build education section"""
        latex = []
        latex.append(r'\cvsection{Education}')
        latex.append(r'\begin{cventries}')
        
        for edu in education_data.get('education', []):
            latex.append(r'\cventry')
            latex.append(r'  {%s}' % self.fmt.escape_text(edu['degree']))
            latex.append(r'  {%s}' % self.fmt.escape_text(edu['institution']))
            latex.append(r'  {%s}' % self.fmt.escape_text(edu['location']))
            latex.append(r'  {%s}' % edu['graduation_date'])
            latex.append(r'  {}')
        
        latex.append(r'\end{cventries}')
        latex.append('')
        return latex
    
    def build_awards_section(self, awards_data):
        """Build awards section"""
        latex = []
        
        if 'awards' in awards_data and awards_data['awards']:
            latex.append(r'\cvsection{Awards}')
            latex.append(r'\begin{cvhonors}')
            
            for award in awards_data['awards']:
                location = award.get('location', '')
                latex.append(r'\cvhonor')
                latex.append(r'  {%s}' % self.fmt.escape_text(award['name']))
                latex.append(r'  {%s}' % self.fmt.escape_text(award['description']))
                latex.append(r'  {%s}' % self.fmt.escape_text(location))
                latex.append(r'  {%s}' % award['date'])
            
            latex.append(r'\end{cvhonors}')
            latex.append('')
        
        return latex
    
    def build_publications_section(self, publications_data):
        """Build publications section"""
        latex = []
        
        if 'publications' in publications_data and publications_data['publications']:
            latex.append(r'\cvsection{Publications}')
            latex.append(r'\begin{cventries}')
            
            for pub in publications_data['publications']:
                # Build description with authors and venue
                desc_parts = []
                if 'authors' in pub:
                    desc_parts.append(r'\textit{Authors:} ' + self.fmt.escape_text(pub['authors']))
                if 'venue' in pub:
                    desc_parts.append(r'\textit{Venue:} ' + self.fmt.escape_text(pub['venue']))
                if 'description' in pub:
                    desc_parts.append(self.fmt.escape_text(pub['description']))
                
                desc_items = [r'  \item ' + part for part in desc_parts]
                description = r'\begin{cvitems}' + '\n' + '\n'.join(desc_items) + '\n' + r'\end{cvitems}'
                
                latex.append(r'\cventry')
                latex.append(r'  {%s}' % self.fmt.escape_text(pub['title']))
                latex.append(r'  {}')  # Empty company field
                latex.append(r'  {}')  # Empty location
                latex.append(r'  {%s}' % pub['date'])
                latex.append(r'  {')
                latex.append(description)
                latex.append(r'  }')
            
            latex.append(r'\end{cventries}')
            latex.append('')
        
        return latex
    
    def build_projects_section(self, projects_data):
        """Build projects section with Company -> Role -> Projects hierarchy"""
        latex = []
        
        if 'companies' in projects_data and projects_data['companies']:
            latex.append(r'\newpage')
            latex.append(r'\cvsection{Projects}')
            latex.append(r'\begin{cventries}')
            
            for company in projects_data['companies']:
                roles = company.get('roles', [])
                
                # Company header - show once per company
                latex.append(r'\projectcompany')
                latex.append(r'  {%s}' % self.fmt.escape_text(company['name']))
                latex.append(r'  {%s}' % self.fmt.format_date(company['start_date'], year_only=True))
                latex.append(r'  {%s}' % self.fmt.format_date(company['end_date'], year_only=True))
                latex.append(r'  {%s}' % self.fmt.escape_text(company['location']))
                
                for role in roles:
                    projects = role.get('projects', [])
                    
                    if not projects:
                        continue
                    
                    # Role using new projectrole command
                    latex.append(r'\projectrole{%s}' % self.fmt.escape_text(role['title']))
                    
                    # Projects under this role
                    for proj in projects:
                        # Format technologies for margin note
                        tech = proj.get('technologies', {})
                        tech_lines = []
                        
                        for key, value in tech.items():
                            if value:
                                display_key = key.replace('_', ' ').title()
                                if isinstance(value, list):
                                    value_str = ', '.join([self.fmt.escape_text(str(v)) for v in value])
                                else:
                                    value_str = self.fmt.escape_text(str(value))
                                tech_lines.append(r'\textbf{%s:} %s' % (display_key, value_str))
                        
                        margin_note = ''
                        if tech_lines:
                            margin_content = r'{\scriptsize\color{graytext} ' + r' \\ '.join(tech_lines) + r'}'
                            margin_note = r'\marginnote{' + margin_content + r'}[0pt]'
                        
                        # Build description - plain text without itemization
                        description_parts = []
                        if margin_note:
                            description_parts.append(margin_note)
                        
                        # Add summary as plain text
                        if 'summary' in proj and proj['summary']:
                            summary_text = self.fmt.escape_text(proj['summary'].strip())
                            description_parts.append(r'\textbf{Summary:} ' + summary_text)
                            description_parts.append(r'')  # Add blank line
                        
                        # Add contributions as plain text
                        if 'contributions' in proj and proj['contributions']:
                            description_parts.append(r'\textbf{Contributions:}')
                            for item in proj['contributions']:
                                description_parts.append(r'\begin{itemize}[leftmargin=2ex, nosep, noitemsep]')
                                description_parts.append(r'  \item ' + self.fmt.escape_text(item))
                                description_parts.append(r'\end{itemize}')
                        
                        description = '\n'.join(description_parts)
                        
                        # Use cvsubentry command (3 parameters: position, date, description)
                        latex.append(r'\cvsubentry')
                        latex.append(r'  {%s}' % self.fmt.escape_text(proj['name']))
                        latex.append(r'  {%s -- %s}' % (
                            self.fmt.format_date(proj['start_date']),
                            self.fmt.format_date(proj['end_date'])
                        ))
                        latex.append(r'  {')
                        latex.append(description)
                        latex.append(r'  }')
            
            latex.append(r'\end{cventries}')
            latex.append('')
        
        return latex
    
    def build_opensource_section(self, opensource_data):
        """Build open source contributions section"""
        latex = []
        
        if 'opensource' in opensource_data and opensource_data['opensource']:
            latex.append(r'\cvsection{Open Source Contributions}')
            latex.append(r'\begin{cventries}')
            
            for contrib in opensource_data['opensource']:
                # Build description with project URL and contributions
                desc_items = []
                
                # Add project description
                if 'description' in contrib:
                    desc_items.append(r'  \item ' + self.fmt.escape_text(contrib['description']))
                
                # Add contributions
                if 'contributions' in contrib:
                    desc_items.extend([
                        r'  \item ' + self.fmt.escape_text(item)
                        for item in contrib['contributions']
                    ])
                
                description = r'\begin{cvitems}' + '\n' + '\n'.join(desc_items) + '\n' + r'\end{cvitems}'
                
                # Format project name with URL if available
                project_name = contrib['project']
                if 'url' in contrib and contrib['url']:
                    project_name = r'\href{%s}{%s}' % (contrib['url'], self.fmt.escape_text(contrib['project']))
                else:
                    project_name = self.fmt.escape_text(contrib['project'])
                
                latex.append(r'\cventry')
                latex.append(r'  {%s}' % project_name)
                latex.append(r'  {}')  # Empty company field
                latex.append(r'  {}')  # Empty location
                latex.append(r'  {%s}' % contrib.get('date', ''))
                latex.append(r'  {')
                latex.append(description)
                latex.append(r'  }')
            
            latex.append(r'\end{cventries}')
            latex.append('')
        
        return latex
        
        return latex
