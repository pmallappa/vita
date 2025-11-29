# Project Margin Notes - Flexible Key-Value Format

The projects section now supports flexible key-value pairs in margin notes, allowing you to use any keys you want instead of being limited to "Languages", "Tools", and "Tech".

## How It Works

The `technologies` field in your project YAML can now contain **any key-value pairs** you want. The system will automatically:

1. Display the key name (capitalized and formatted)
2. Handle both list and string values
3. Show them in the margin notes

## Example Usage

### Basic Example

```yaml
projects:
  - name: "My Project"
    company: "Company Name"
    date: "2020-2023"
    summary: "Project description"
    contributions:
      - "Achievement 1"
      - "Achievement 2"
    technologies:
      languages: ["Python", "C++", "Rust"]
      frameworks: ["Django", "FastAPI"]
      databases: ["PostgreSQL", "Redis"]
      cloud: ["AWS", "Docker", "Kubernetes"]
```

**Margin Note Output:**
```
Languages: Python, C++, Rust
Frameworks: Django, FastAPI
Databases: PostgreSQL, Redis
Cloud: AWS, Docker, Kubernetes
```

### Custom Keys Example

You can use ANY keys you want:

```yaml
technologies:
  stack: ["React", "Node.js", "MongoDB"]
  deployment: "AWS ECS"
  ci_cd: "GitHub Actions"
  monitoring: ["Prometheus", "Grafana"]
  architecture: "Microservices"
```

**Margin Note Output:**
```
Stack: React, Node.js, MongoDB
Deployment: AWS ECS
Ci Cd: GitHub Actions
Monitoring: Prometheus, Grafana
Architecture: Microservices
```

### Mixed Value Types

You can mix lists and strings:

```yaml
technologies:
  primary_language: "Rust"
  libraries: ["tokio", "serde", "actix-web"]
  platform: "Linux"
  tools: ["Docker", "Kubernetes"]
  version_control: "Git"
```

**Margin Note Output:**
```
Primary Language: Rust
Libraries: tokio, serde, actix-web
Platform: Linux
Tools: Docker, Kubernetes
Version Control: Git
```

## Key Formatting Rules

1. **Underscores** in key names are converted to spaces
   - `primary_language` → "Primary Language"
   - `ci_cd` → "Ci Cd"

2. **Title Case** is applied automatically
   - `languages` → "Languages"
   - `cloud_platform` → "Cloud Platform"

3. **Empty values** are skipped
   - If a key has an empty list `[]` or empty string `""`, it won't appear

## Company Name Formatting

The company name in projects is now displayed in **bold non-italic** font for better emphasis:

```yaml
company: "AMD Ltd."
```

Renders as: **AMD Ltd.** (bold, not italic)

## Complete Example

```yaml
projects:
  - name: "High-Performance Computing Framework"
    company: "Tech Corp"
    date: "2021-Present"
    summary: "Developed a distributed computing framework for scientific simulations"
    contributions:
      - "Achieved 10x performance improvement over previous solution"
      - "Reduced memory footprint by 40%"
      - "Implemented fault-tolerant job scheduling"
    technologies:
      languages: ["C++", "Python", "CUDA"]
      frameworks: ["MPI", "OpenMP"]
      libraries: ["Boost", "Eigen", "HDF5"]
      platforms: ["Linux", "HPC Clusters"]
      tools: ["CMake", "GDB", "Valgrind"]
      architecture: "Distributed"
      performance: "SIMD optimized"
```

## Migration from Old Format

### Old Format (Still Works)
```yaml
technologies:
  languages: ["C", "Python"]
  tools: ["Git", "Docker"]
  tech: ["Linux", "AWS"]
```

### New Format (More Flexible)
```yaml
technologies:
  languages: ["C", "Python"]
  devops: ["Git", "Docker", "CI/CD"]
  infrastructure: ["Linux", "AWS", "Kubernetes"]
  databases: ["PostgreSQL", "Redis"]
  monitoring: ["Prometheus", "Grafana"]
```

Both formats work! The new format just gives you more flexibility in organizing your technology information.

## Tips

1. **Be Consistent**: Use similar key names across projects for a professional look
2. **Keep It Relevant**: Only include technologies that are important for that specific project
3. **Order Matters**: Keys appear in the order you define them in the YAML
4. **Use Descriptive Keys**: Instead of generic "tech", use specific keys like "cloud_platform", "databases", "frameworks"

## Example Projects YAML

```yaml
projects:
  - name: "Real-time Analytics Platform"
    company: "Data Solutions Inc"
    date: "2022-2023"
    summary: "Built a real-time data processing pipeline handling 1M events/sec"
    contributions:
      - "Designed scalable microservices architecture"
      - "Implemented real-time stream processing"
      - "Achieved 99.99% uptime SLA"
    technologies:
      backend: ["Go", "Python"]
      streaming: ["Apache Kafka", "Apache Flink"]
      storage: ["PostgreSQL", "ClickHouse", "Redis"]
      cloud: ["AWS", "EKS", "S3"]
      monitoring: ["Prometheus", "Grafana", "ELK Stack"]
      
  - name: "Mobile Banking Application"
    company: "FinTech Startup"
    date: "2020-2022"
    summary: "Developed secure mobile banking app with 500K+ users"
    contributions:
      - "Implemented end-to-end encryption"
      - "Achieved PCI-DSS compliance"
      - "Reduced transaction latency by 60%"
    technologies:
      mobile: ["React Native", "TypeScript"]
      backend: ["Node.js", "Express"]
      database: "MongoDB"
      security: ["OAuth 2.0", "JWT", "AES-256"]
      cloud: "Google Cloud Platform"
      ci_cd: "GitLab CI"
```

This flexibility allows you to better represent the diverse technology stacks used in different projects!
