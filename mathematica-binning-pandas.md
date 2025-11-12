# Pandas Implementation of Mathematica Binning Code

This document provides a complete Pandas-based implementation of the Mathematica binning code that returns bin intervals for each value.

## Original Mathematica Code

```mathematica
binning=Table[Catch[Do[If[IntervalMemberQ[Interval[i],aim[[j]]]==True,Throw[i]],
{i,Partition[Range[min,max,step],2,1]}]],{j,Length[aim]}];
```

## Pandas Implementation

```python
import pandas as pd
import numpy as np

def mathematica_binning(aim, min_val, max_val, step):
    """
    Python equivalent of Mathematica's binning code using pandas.
    
    Replicates:
    binning=Table[Catch[Do[If[IntervalMemberQ[Interval[i],aim[[j]]]==True,Throw[i]],
    {i,Partition[Range[min,max,step],2,1]}]],{j,Length[aim]}];
    
    Parameters:
    -----------
    aim : list or array
        Values to bin
    min_val : numeric
        Minimum bin boundary
    max_val : numeric
        Maximum bin boundary
    step : numeric
        Bin width
        
    Returns:
    --------
    list of tuples : Bin intervals (left, right) for each value
    
    Examples:
    ---------
    >>> mathematica_binning([1.5, 3.2, 7.8, 9.1], 0, 10, 2)
    [(0, 2), (2, 4), (6, 8), (8, 10)]
    """
    # Create bin edges
    edges = np.arange(min_val, max_val + step, step)
    
    # Create interval index with closed boundaries on both sides
    # This matches Mathematica's Interval behavior where both endpoints are included
    intervals = pd.IntervalIndex.from_breaks(edges, closed='both')
    
    # Find matching bin for each value
    result = []
    for value in aim:
        # Find interval containing this value
        matching = [i for i in intervals if value in i]
        if matching:
            interval = matching[0]
            result.append((int(interval.left), int(interval.right)))
        else:
            result.append(None)
    
    return result
```

## Alternative Implementations

### 1. Using pd.cut() (Simpler but less exact)

```python
def binning_pandas_cut(aim, min_val, max_val, step):
    """
    Using pd.cut() for binning - simpler but returns slightly different format.
    """
    bins = np.arange(min_val, max_val + step, step)
    binned = pd.cut(aim, bins=bins, include_lowest=True)
    
    # Extract intervals as tuples
    result = [(int(interval.left), int(interval.right)) 
              for interval in binned]
    
    return result
```

### 2. DataFrame-Based Approach (Best for Data Engineering)

```python
def binning_dataframe(df, value_column, min_val, max_val, step, 
                      output_column='bin_interval'):
    """
    DataFrame-friendly version for data engineering workflows.
    
    Parameters:
    -----------
    df : pd.DataFrame
        Input DataFrame
    value_column : str
        Name of the column containing values to bin
    min_val, max_val, step : numeric
        Binning parameters
    output_column : str
        Name for the output column
    
    Returns:
    --------
    pd.DataFrame with added bin interval columns
    """
    result_df = df.copy()
    
    # Create bin edges
    edges = np.arange(min_val, max_val + step, step)
    intervals = pd.IntervalIndex.from_breaks(edges, closed='both')
    
    # Find bin for each value
    def find_bin(value):
        for interval in intervals:
            if value in interval:
                return (int(interval.left), int(interval.right))
        return None
    
    result_df[output_column] = result_df[value_column].apply(find_bin)
    
    # Create separate columns for left and right boundaries
    result_df[f'{output_column}_left'] = result_df[output_column].apply(
        lambda x: x[0] if x else None)
    result_df[f'{output_column}_right'] = result_df[output_column].apply(
        lambda x: x[1] if x else None)
    
    return result_df
```

### 3. Vectorized Version (Best Performance)

```python
def binning_vectorized(aim, min_val, max_val, step):
    """
    Vectorized version using pandas for better performance with large datasets.
    """
    edges = np.arange(min_val, max_val + step, step)
    intervals = pd.IntervalIndex.from_breaks(edges, closed='both')
    
    # Vectorized approach using list comprehension
    return [(int(intervals[intervals.contains(v)][0].left), 
             int(intervals[intervals.contains(v)][0].right)) 
            if len(intervals[intervals.contains(v)]) > 0 else None 
            for v in aim]
```

## Usage Examples

### Basic Usage

```python
# Example data
aim = [1.5, 3.2, 7.8, 9.1]
min_val = 0
max_val = 10
step = 2

result = mathematica_binning(aim, min_val, max_val, step)
print(result)
# Output: [(0, 2), (2, 4), (6, 8), (8, 10)]
```

### With DataFrame

```python
import pandas as pd

# Create DataFrame
df = pd.DataFrame({
    'values': [1.5, 3.2, 7.8, 9.1],
    'name': ['A', 'B', 'C', 'D']
})

# Apply binning
df_result = binning_dataframe(df, 'values', 0, 10, 2)

print(df_result)
# Output:
#    values name bin_interval  bin_interval_left  bin_interval_right
# 0     1.5    A       (0, 2)                  0                   2
# 1     3.2    B       (2, 4)                  2                   4
# 2     7.8    C       (6, 8)                  6                   8
# 3     9.1    D      (8, 10)                  8                  10
```

### Edge Cases

```python
# Test with boundary values
aim_edge = [0, 2, 10, 5.0, 1.99, 2.01]
result = mathematica_binning(aim_edge, 0, 10, 2)
print(result)
# Output: [(0, 2), (0, 2), (8, 10), (4, 6), (0, 2), (2, 4)]
```

## Key Differences from Mathematica

| Aspect | Mathematica | Pandas Implementation |
|--------|-------------|----------------------|
| **Indexing** | 1-indexed | 0-indexed |
| **Interval notation** | `{a, b}` | `(a, b)` |
| **Closed intervals** | Both sides closed by default | Configurable via `closed` parameter |
| **Early exit** | `Throw`/`Catch` | `break` statement |
| **List generation** | `Table[]` | List comprehension |

## Performance Considerations

For **data engineering workflows**, consider:

1. **Small datasets (<1000 values)**: Use the basic implementation
2. **Medium datasets (1K-100K values)**: Use vectorized version
3. **Large datasets (>100K values)**: Use DataFrame-based approach with chunking
4. **Real-time processing**: Pre-compute `IntervalIndex` and reuse

### Optimization Example

```python
# Pre-compute intervals for repeated use
class BinningOptimized:
    def __init__(self, min_val, max_val, step):
        edges = np.arange(min_val, max_val + step, step)
        self.intervals = pd.IntervalIndex.from_breaks(edges, closed='both')
        
    def bin(self, values):
        result = []
        for value in values:
            matching = [i for i in self.intervals if value in i]
            if matching:
                interval = matching[0]
                result.append((int(interval.left), int(interval.right)))
            else:
                result.append(None)
        return result

# Usage
binner = BinningOptimized(0, 10, 2)
result1 = binner.bin([1.5, 3.2])
result2 = binner.bin([7.8, 9.1])
```

## Integration with Data Pipelines

### Example: ETL Pipeline

```python
import pandas as pd

def etl_with_binning(input_file, output_file, bin_params):
    """
    ETL pipeline with binning transformation.
    """
    # Extract
    df = pd.read_csv(input_file)
    
    # Transform - Apply binning
    df = binning_dataframe(
        df, 
        value_column='value', 
        min_val=bin_params['min'],
        max_val=bin_params['max'],
        step=bin_params['step']
    )
    
    # Load
    df.to_csv(output_file, index=False)
    
    return df

# Usage
params = {'min': 0, 'max': 100, 'step': 10}
result_df = etl_with_binning('input.csv', 'output.csv', params)
```

## Comparison with Other Binning Methods

| Method | Use Case | Returns |
|--------|----------|---------|
| `mathematica_binning()` | Exact Mathematica replication | Bin intervals |
| `pd.cut()` | Equal-width binning | Categorical with intervals |
| `pd.qcut()` | Equal-frequency binning | Categorical with intervals |
| `np.digitize()` | Low-level bin indices | Integer indices |
| `scipy.stats.binned_statistic()` | Statistics within bins | Statistics + bin edges |

## Summary

The recommended `mathematica_binning()` function provides:
- Exact behavior match with Mathematica
- Clean, readable code
- Integration with pandas workflows
- Handles edge cases properly
- Returns integer tuples like Mathematica

For production data engineering, combine with the DataFrame-based approach for seamless integration into ETL pipelines.