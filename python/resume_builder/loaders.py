"""
Data Loaders Module
Handles loading and validation of YAML data files
"""

import yaml
import sys
from pathlib import Path


class YAMLDataLoader:
    """Loads YAML data files for resume generation"""
    
    def __init__(self, data_dir='data'):
        self.data_dir = Path(data_dir)
    
    def load_file(self, filename):
        """Load a single YAML data file"""
        filepath = self.data_dir / filename
        try:
            with open(filepath, 'r') as f:
                return yaml.safe_load(f)
        except FileNotFoundError:
            print(f"Error: {filepath} not found")
            sys.exit(1)
        except yaml.YAMLError as e:
            print(f"Error parsing {filepath}: {e}")
            sys.exit(1)
    
    def load_all_data(self):
        """Load all required data files"""
        data = {
            'personal': self.load_file('personal.yaml'),
            'experience': self.load_file('experience.yaml'),
            'education': self.load_file('education.yaml'),
            'skills': self.load_file('skills.yaml'),
            'projects': self.load_file('projects.yaml'),
            'publications': self.load_file('publications.yaml'),
            'awards': self.load_file('awards.yaml')
        }
        
        # Load optional opensource file
        opensource_file = self.data_dir / 'opensource.yaml'
        if opensource_file.exists():
            data['opensource'] = self.load_file('opensource.yaml')
        else:
            data['opensource'] = {}
        
        return data
