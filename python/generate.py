#!/usr/bin/env python3
"""
Resume Generator CLI
Command-line interface for generating resumes and cover letters
"""

import argparse
import sys
from pathlib import Path

# Add the scripts directory to the path to import resume_builder
sys.path.insert(0, str(Path(__file__).parent))

from resume_builder import ResumeGenerator


def main():
    """Main entry point for the resume generator"""
    parser = argparse.ArgumentParser(
        description='Generate resume or cover letter from YAML data'
    )
    parser.add_argument(
        '--type',
        choices=['resume', 'coverletter'],
        required=True,
        help='Type of document to generate'
    )
    parser.add_argument(
        '--output',
        required=True,
        help='Output file path'
    )
    parser.add_argument(
        '--job-desc',
        help='Job description for tailoring (optional)'
    )
    parser.add_argument(
        '--company',
        help='Company name (required for cover letter)'
    )
    parser.add_argument(
        '--position',
        help='Job position (required for cover letter)'
    )
    parser.add_argument(
        '--data-dir',
        default='data',
        help='Directory containing YAML data files (default: data)'
    )
    
    args = parser.parse_args()
    
    # Create output directory if needed
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Initialize generator
    try:
        generator = ResumeGenerator(args.data_dir)
    except Exception as e:
        print(f"Error initializing generator: {e}")
        sys.exit(1)
    
    # Generate content
    try:
        if args.type == 'resume':
            content = generator.generate_resume_latex(args.job_desc)
        else:
            if not args.company or not args.position:
                print("Error: --company and --position required for cover letter")
                sys.exit(1)
            content = generator.generate_coverletter_latex(
                args.company,
                args.position,
                args.job_desc
            )
    except Exception as e:
        print(f"Error generating {args.type}: {e}")
        sys.exit(1)
    
    # Write output
    try:
        with open(args.output, 'w') as f:
            f.write(content)
        print(f"Generated {args.type} at {args.output}")
    except Exception as e:
        print(f"Error writing output file: {e}")
        sys.exit(1)


if __name__ == '__main__':
    main()
