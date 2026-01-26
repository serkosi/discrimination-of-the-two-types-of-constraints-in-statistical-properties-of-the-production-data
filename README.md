# Discriminating Technology-driven and Load-driven Constraints in Production Data

## The Challenge

Steel manufacturing is a complex, multi-stage process where events flow sequentially through integrated production lines. Each event (a semi-finished steel product) carries attributes—width, thickness, weight, length—that change as it progresses through different machines. The critical question: **Why do certain product attributes cluster together in production sequences?**

The answer lies in **constraints**—invisible rules that govern which products can be processed together and in what order. But constraints are not monolithic. We identified two fundamentally different types operating in parallel:

- **Technology-driven constraints** arise from how machines process materials. For example, a hot rolling mill handles one slab at a time and must arrange events in specific order (by width) to avoid equipment wear and maintain quality.

- **Load-driven constraints** come from capacity limits. A pickling tank treats multiple slabs simultaneously but is capped at maximum batch volume. The order matters less than staying within capacity.

These constraints leave statistical fingerprints in production data. The challenge: can we formally discriminate between these two constraint types using only the patterns we observe in historical production sequences?

## Our Solution: A Three-Step Framework

### Step 1: Extract Hidden Patterns via Association Networks

Raw production data contains thousands of events arranged in sequences. Visually identifying which products cluster together is impossible at scale. We solve this through **association rule mining**:

1. **Calculate co-occurrence strength** for every pair of product attributes within the same sequences using the Lift metric:
   - Lift > 1 means two products occur together more frequently than independent probability would predict
   - Lift < 1 means their co-occurrence is less likely from a probability perspective
   
2. **Convert these patterns into networks** where nodes represent product types (binned by attributes) and edges represent strong co-occurrences

3. **Analyze network topology** to uncover whether constraints have created clusters, hierarchies, or random structures

The key insight: **Different constraints generate different network patterns.** We needed to expose these differences.

### Step 2: Operationalize Constraints via Dual Binning Schemes

We developed two complementary ways to group events:

**Fixed Step Size (FSS) Binning**: Divide the attribute range into equal intervals. For width, create bins of 99mm width (e.g., 800–899mm, 900–999mm). This mirrors technology-driven constraints, which operate uniformly across the product range.

**Fixed Bucket Size (FBS) Binning**: Create bins with equal event counts. The first 20% of events go in bin 1, next 20% in bin 2, etc. This mirrors load-driven constraints, which care about batch volume regardless of product specifications.

By constructing networks using both schemes and comparing results, we can see whether FSS or FBS better explains the observed clustering—revealing which constraint type dominates.

### Step 3: Quantify Constraint Effects via Statistical Validation

Network patterns alone aren't sufficient; we need statistical rigor. We employ:

**Modularity Measurement**: Quantify how tightly events cluster within groups. Higher modularity indicates strong constraints forcing specific combinations.

**Randomness Testing via Null Models**: Compare real networks against random networks that preserve degree structure. If the real network shows significantly higher modularity than random versions, we have evidence of true constraints—not accidental clustering.

**Z-Score Standardization**: Express deviation from randomness in standard units:
- |z| < 1: Network resembles randomness
- 1 ≤ |z| < 2: Borderline structure  
- |z| ≥ 2: Significant, non-random constraint-driven patterns

**Robustness Testing**: Perturb the data (remove and restore 10% ten times) to ensure findings survive variation. Stable results with low error bars confirm genuine constraints; large error bars signal artifacts.

## Implementation and Results

### Data Foundation

Analyzed **23 years of production data** across four integrated steel production lines:
- **CCM** (Continuous Casting Machine): 347,418 events
- **CSP** (Compact Strip Production): 205,496 events
- **PLTCM** (Pickling Line Tandem Cold Mill): 59,604 events
- **CGL** (Continuous Galvanizing Line): 27,147 events

### Data Preparation Pipeline

Before analysis, we cleaned raw data through:

1. **Standardization**: Converted string data to floats, normalized punctuation, filled nulls consistently

2. **Physical validation**: Calculated density for every event (valid range: 6.5–8.5 × 10⁻⁶ kg/mm³); discarded outliers

3. **Capacity ranges**: Applied machine input capacity limits:
   - Width: 800–2000 mm
   - Weight: 2669–26,690 kg

4. **Gap filling**: Imputed missing values using density-mass-volume equations and cross-event consistency checks

5. **Sequence filtering**: Removed sequences with < 50 events (likely test processes)

6. **Cross-line data integration**: Joined PLTCM and CGL datasets using Material ID and Piece ID as foreign keys to establish referential integrity across production stages; propagated PLTCM input-labeled attributes (input width, input thickness) to corresponding CGL events for downstream analysis

### Key Findings

**CCM and CSP (Technology-driven lines) — Width dimension:**
- FSS networks: consistently modular and hierarchically organized
- FBS networks: non-random, non-modular hierarchical structure (but unstable—FBS modularity breaks down after 10% data perturbation)
- **Interpretation**: Fixed-interval binning reveals genuine constraint structure; equal-population binning creates artificial patterns that don't persist

**CCM and CSP (Technology-driven lines) — Thickness dimension:**
- FSS networks: modular and simple (robust)
- FBS networks: non-modular and hierarchical (robust)
- **Interpretation**: Technology-driven machines show clear separation between features

**PLTCM and CGL (Load-driven lines) — Thickness dimension:**
- FSS networks: highly modular
- FBS networks: highly modular
- CGL networks: both approach robust (no complex hierarchical structure)
- PLTCM networks: both modular but hierarchical (less robust than CGL)
- **Interpretation**: Load constraints strongly organize thickness across both binning schemes; CGL shows more stable structure than PLTCM

**PLTCM and CGL (Load-driven lines) — Width dimension:**
- PLTCM: both FSS and FBS modular but hierarchical with non-robust structure
- CGL: FSS non-modular hierarchical; FBS modular hierarchical (non-robust)
- **Interpretation**: Width dimension shows sensitivity to binning method in load-driven lines; suggests multiple constraint mechanisms operate simultaneously

**Temporal stability**: Constraint patterns remain consistent across time windows, confirming they reflect persistent operational rules rather than transient variation

## Theoretical Validation via Simulation

To validate findings beyond empirical observation, we developed a **constraint-based simulation** inspired by metabolic network modeling:

1. **Build artificial production networks** with embedded constraints using Flux Balance Analysis (Homo sapiens metabolic model: 738 metabolites, 1008 reactions)

2. **Systematically vary constraints through two experimental designs:**
   - **Resource Utilisation (RU)**: RU-1 progressively deletes reactions (50–450 in 50-unit steps); RU-2 limits flux bounds (105–420 reactions)
   - **Product Portfolio Diversification (PPD)**: PPD-1 varies objective function coefficients using directional intervals (±[1,4], ±[4,2], ±[2,4]); PPD-2 reduces objective function richness (25%, 50%, 75%)

3. **Compare simulated patterns** to real production data using identical analysis methods

**Key simulation results:**

**Resource utilisation effects (RU-1)**: As deleted reactions increase, modularity decreases and NM-d z-scores show significant decline. NM-m z-scores remain near zero, indicating no hierarchical structure breakdown.

**Portfolio diversification effects (PPD-1 & PPD-2)**: 
- FSS networks: no modularity change across constraint variations
- FBS networks: modularity increases only for directional (asymmetric) objective functions
- Symmetric production plans (coefficients balanced around zero) show dampening effects regardless of richness reductions
- **Interpretation**: Production plan directionality matters for load-driven network topology; generic product capability does not significantly constrain network structure

## What This Means

This framework enables:

- **Constraint identification**: Use network patterns to detect which constraint types are active in the production system of interest
- **Process diagnostics**: Understand how constraints shape observed production behavior and identify which attributes are most strongly affected
- **Theoretical grounding**: Validate constraint discrimination through constraint-based simulation with controlled perturbations

## Tools and Data

- **SQL extraction** queries for pulling events from enterprise databases (supporting incremental updates across 23-year periods)
- **Binning algorithms** implementing FSS and FBS discretization at configurable resolutions
- **Network construction** via association rules and adjacency matrices
- **Statistical analysis** suite including modularity computation, null model randomization, z-score calculation, and error estimation
- **Simulation framework** integrating constraint-based optimization with production-compatible data generation

All methods support large-scale datasets (300,000+ events) with modular, reproducible implementation.

---

**Status**: Active development | **Focus**: Production data analysis, constraint discrimination, network topology characterization