## Contribution to Planning and Autonomy

This project provides a data-driven framework for understanding and quantifying constraints in industrial production systems. By linking network topology and statistical patterns in production data to underlying constraints, the solution enables several advances in planning and autonomy:

- **Constraint Identification:** The approach allows planners to distinguish between technology-driven and load-driven constraints, revealing which aspects of the production process are most limiting and how they manifest in operational data.
- **Scenario Simulation:** The integration of theoretical models and simulation (e.g., Flux Balance Analysis) enables virtual perturbation experiments, helping planners anticipate the effects of changes in constraints or process parameters before implementation.
- **Network Vulnerability and Robustness:** By analyzing the association networks, the solution highlights vulnerabilities in the production system’s architecture, supporting more resilient planning and risk mitigation.
- **Adaptive Planning:** The methods support dynamic adjustment of production plans in response to input fluctuations, technical limitations, or changing product portfolios, fostering greater autonomy in manufacturing operations.
- **Decision Support:** Quantitative metrics and visualizations provide actionable insights for optimizing resource allocation, sequencing, and technology selection, enhancing both manual and automated decision-making.

Overall, this solution bridges the gap between raw production data and high-level planning, offering tools and methodologies that can be integrated into autonomous manufacturing systems for improved efficiency, adaptability, and strategic foresight.

## Further Discussion

The results show that constraints in the production process have a systematic effect on the decisions made during the production process. The systematic shaping of the decisions shows up as non-random features of the association networks. Below are brief discussions on several key aspects:

1. **Matching of Simulation and Real Production Results:**
	The analysis results obtained from simulation events (using Flux Balance Analysis) and from steel production events show similar patterns in the association networks. Both approaches reveal how constraints systematically shape decision-making, with modularity and connectivity features reflecting the underlying constraint types. This match validates the simulation model and demonstrates its relevance to real-world production planning.

2. **Association Networks as Quantitative Measures:**
	Association networks provide a quantitative way to measure the impact of constraints on the decision patterns of human experts. Modularity is selected in the analysis pipeline because it captures the degree of community structure and clustering, which often reflects constraint-driven grouping in decisions. Alternatives to modularity include metrics like assortativity, clustering coefficient, or network entropy, which can also reveal structural effects of constraints.

3. **Simpler Models for Comparison:**
		Random graphs have already been used in this project as null models by shuffling edges in the association networks while preserving degree sequences. This allowed for comparison of real network modularity against randomized baselines and quantification of the significance of observed patterns. For further comparison, decision trees can serve as interpretable models that highlight how different constraints influence decision patterns, making it easier to trace the impact of specific features or rules on production outcomes.

4. **Temporal Trends in Steel Production Data:**
	The data analysis from steel production events did not reveal a significant trend over time in the properties of association networks. Possible reasons include stable production processes, consistent application of constraints, or the aggregation of data over long periods masking short-term variations. It may also indicate that constraint effects are persistent and not strongly time-dependent in this context.

5. **Product Diversity and Constraints:**
			Resource Utilisation (RU) and Product Portfolio Diversification (PPD) were already introduced and implemented in the simulation model. RU manipulates resource usage by deleting reactions or limiting fluxes, while PPD varies the objective function coefficients and richness, directly affecting the diversity and directionality of production plans. These interventions allow for controlled simulation experiments to probe causal relationships. Alternatives for interventions could include varying process parameters, introducing artificial bottlenecks, or simulating changes in scheduling policies. Such interventions help distinguish correlation from causation and provide practical guidance for optimizing both resource use and product portfolio in real production environments.

# Discrimination of the Two Types of Constraints in Statistical Properties of Production Data

## Overview
This project provides a comprehensive framework for analyzing how different types of constraints—technology-driven and load-driven—shape the statistical properties of industrial production data. Using large-scale datasets from steel manufacturing, the project combines empirical data analysis, network science, and theoretical modeling to uncover the mechanisms behind observed production patterns.

## Key Features
- **Network Analysis:** Construction and analysis of association networks to reveal how constraints affect connectivity, modularity, and diversity in production systems.
- **Time Window Studies:** Systematic investigation of constraint impacts across various time windows, including sliding and fixed intervals, to capture dynamic changes in production behavior.
- **Operations Research Modeling:** Integration of OR methods to simulate and compare the effects of different constraint types on production outcomes.
- **Diversity Metrics:** Quantitative assessment of product diversity and richness, with visualizations and statistical summaries.
- **Algorithmic Tools:** Modular code packages for data cleaning, network construction, and statistical analysis, enabling reproducible and extensible research.
- **Reporting and Visualization:** Automated generation of figures, tables, and summary reports to support scientific communication and decision-making.

## Impact
The project advances understanding of how constraints drive statistical regularities in industrial data, providing actionable insights for process optimization, technology selection, and production planning. The methods and tools developed here are applicable to a wide range of manufacturing domains beyond steel production.

## Getting Started
Explore the code folders for step-by-step analyses, reusable algorithms, and example datasets. For a high-level summary, see the abstract below.


## Report
All results, figures, and detailed methodology are documented in the `report` folder. The complete thesis report is available as a PDF file: [`report/paper/main.pdf`](report/paper/main.pdf). This document provides a comprehensive overview of the project, including background, methods, results, and conclusions.

## Abstract
Constraints lead to statistical patterns in data. This work quantifies the characteristics of two hypothetical types of constraints in industrial production: technology-driven and load-driven constraints. By analyzing the statistical properties of association networks over time in large datasets from steel manufacturing, and developing an abstract theoretical framework, the project clarifies the connection between each type of constraint and its statistical patterns.
