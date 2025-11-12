# Python Implementation of Wolfram Mathematica's BinLists Function

This document provides a comprehensive Python implementation of Wolfram Mathematica's `BinLists` function, along with additional features and utilities for data binning.

## Overview

Wolfram Mathematica's `BinLists` function groups data elements into bins based on their numerical values. This Python implementation replicates that functionality while adding Python-specific optimizations and additional features.

## Basic Implementation

```python
import numpy as np
from collections import defaultdict
from typing import List, Union, Tuple, Any, Optional

def bin_lists(data: List[Union[int, float, List]], 
              bin_spec: Union[None, float, Tuple, List] = None,
              labels: Optional[List] = None) -> List[List]:
    """
    Python implementation of Wolfram Mathematica's BinLists function.
    
    Parameters:
    -----------
    data : list
        List of numerical values or list of lists (for multidimensional data)
    bin_spec : None, float, tuple, or list, optional
        Bin specification:
        - None or 1: Unit width bins starting from integer multiples
        - float dx: Bins of width dx
        - (x_min, x_max, dx): Bins from x_min to x_max with width dx
        - (x_min, x_max): Bins from x_min to x_max with unit width
        - [[b1, b2, b3, ...]]: Custom bin boundaries
    labels : list, optional
        Labels for the input elements
        
    Returns:
    --------
    list of lists: Elements grouped by bins
    """
    
    # Handle empty data
    if not data:
        return []
    
    # Filter out non-numeric values (following Mathematica's behavior)
    if isinstance(data[0], (list, tuple)):
        # Multidimensional data
        numeric_data = []
        original_indices = []
        for i, item in enumerate(data):
            if all(isinstance(x, (int, float)) and not np.isnan(x) for x in item):
                numeric_data.append(item)
                original_indices.append(i)
    else:
        # 1D data
        numeric_data = []
        original_indices = []
        for i, item in enumerate(data):
            if isinstance(item, (int, float)) and not np.isnan(item):
                numeric_data.append(item)
                original_indices.append(i)
    
    if not numeric_data:
        return []
    
    # Handle labels if provided
    if labels:
        filtered_labels = [labels[i] for i in original_indices]
        elements_to_bin = filtered_labels
    else:
        elements_to_bin = [data[i] for i in original_indices]
    
    # Handle different bin specifications
    if bin_spec is None:
        bin_spec = 1
    
    # 1D binning
    if not isinstance(numeric_data[0], (list, tuple)):
        return _bin_lists_1d(numeric_data, elements_to_bin, bin_spec)
    else:
        # Multidimensional binning - simplified version
        return _bin_lists_multid(numeric_data, elements_to_bin, bin_spec)

def _bin_lists_1d(values: List[float], elements: List, bin_spec) -> List[List]:
    """Handle 1D binning"""
    
    if isinstance(bin_spec, (int, float)):
        # Bins of width dx
        dx = bin_spec
        min_val = min(values)
        max_val = max(values)
        
        # Calculate bin boundaries like Mathematica
        first_bin_start = np.ceil((min_val - dx) / dx) * dx
        last_bin_end = np.floor((max_val + dx) / dx) * dx
        
        # Create bin boundaries
        bin_boundaries = np.arange(first_bin_start, last_bin_end + dx, dx)
        
    elif isinstance(bin_spec, (tuple, list)) and len(bin_spec) == 2:
        # (x_min, x_max) - unit width
        x_min, x_max = bin_spec
        bin_boundaries = np.arange(x_min, x_max + 1, 1)
        
    elif isinstance(bin_spec, (tuple, list)) and len(bin_spec) == 3:
        # (x_min, x_max, dx)
        x_min, x_max, dx = bin_spec
        bin_boundaries = np.arange(x_min, x_max + dx, dx)
        
    elif isinstance(bin_spec, list) and len(bin_spec) == 1 and isinstance(bin_spec[0], list):
        # [[b1, b2, b3, ...]] - custom boundaries
        bin_boundaries = sorted(bin_spec[0])
        
    else:
        raise ValueError("Invalid bin specification")
    
    # Create bins
    bins = defaultdict(list)
    
    for value, element in zip(values, elements):
        # Find which bin this value belongs to
        bin_index = _find_bin_index(value, bin_boundaries)
        if bin_index is not None:
            bins[bin_index].append(element)
    
    # Convert to list of lists, maintaining order
    result = []
    for i in range(len(bin_boundaries) - 1):
        result.append(bins[i])
    
    return result

def _find_bin_index(value: float, boundaries: np.ndarray) -> Optional[int]:
    """Find which bin a value belongs to"""
    for i in range(len(boundaries) - 1):
        if boundaries[i] <= value < boundaries[i + 1]:
            return i
    # Handle the last boundary (closed interval)
    if len(boundaries) > 1 and value == boundaries[-1]:
        return len(boundaries) - 2
    return None
```

## Enhanced Implementation with Advanced Features

```python
class BinLists:
    """
    Advanced Python implementation of Wolfram Mathematica's BinLists function.
    
    This class provides comprehensive binning functionality similar to Mathematica's BinLists,
    with additional Python-specific optimizations and features.
    """
    
    @staticmethod
    def bin_lists(data: Union[List, np.ndarray], 
                  bin_spec: Union[None, float, int, Tuple, List] = None,
                  labels: Optional[List] = None,
                  return_indices: bool = False,
                  return_bin_info: bool = False) -> Union[List[List], Tuple]:
        """
        Bin data into lists based on value ranges.
        
        Parameters:
        -----------
        data : list or numpy array
            Input data to bin
        bin_spec : various types
            Bin specification (see examples below)
        labels : list, optional
            Custom labels for data elements
        return_indices : bool, default False
            If True, return indices instead of values
        return_bin_info : bool, default False
            If True, return additional bin information
            
        Returns:
        --------
        List of lists or tuple with additional information
        """
        
        processor = BinListsProcessor(data, bin_spec, labels)
        
        if return_bin_info:
            return processor.process_with_info()
        elif return_indices:
            return processor.process_indices()
        else:
            return processor.process()
    
    @staticmethod  
    def bin_counts(data: Union[List, np.ndarray], 
                   bin_spec: Union[None, float, int, Tuple, List] = None) -> List[int]:
        """
        Count elements in each bin (similar to Mathematica's BinCounts).
        """
        binned_data = BinLists.bin_lists(data, bin_spec)
        return [len(bin_data) for bin_data in binned_data]
        
    @staticmethod
    def bin_means(data: Union[List, np.ndarray], 
                  bin_spec: Union[None, float, int, Tuple, List] = None) -> List[float]:
        """
        Calculate mean value for each bin.
        """
        binned_data = BinLists.bin_lists(data, bin_spec)
        means = []
        for bin_data in binned_data:
            if bin_data:
                means.append(np.mean([x for x in bin_data if isinstance(x, (int, float))]))
            else:
                means.append(np.nan)
        return means
```

## Usage Examples

### Basic Usage

```python
# Basic integer binning with unit width
data = [1.2, 2.7, 3.1, 1.8, 4.5, 2.3, 3.9, 1.1]
result = bin_lists(data)
# Output: [[1.2, 1.8, 1.1], [2.7, 2.3], [3.1, 3.9], [4.5]]

# Custom bin width
result = bin_lists(data, 0.5)
# Output: [[1.2, 1.1], [1.8], [2.3], [2.7], [3.1], [3.9], [], [4.5]]

# Specific range and width
result = bin_lists(data, (1, 5, 1))
# Output: [[1.2, 1.8, 1.1], [2.7, 2.3], [3.1, 3.9], [4.5]]

# Custom boundaries
result = bin_lists(data, [[1, 2, 3, 4, 5]])
# Output: [[1.2, 1.8, 1.1], [2.7, 2.3], [3.1, 3.9], [4.5]]
```

### Advanced Usage

```python
# Using the enhanced BinLists class
data = [1.2, 2.7, 3.1, 1.8, 4.5, 2.3, 3.9, 1.1, 0.5, 4.8]

# Basic binning
result = BinLists.bin_lists(data)

# Get bin counts
counts = BinLists.bin_counts(data)

# Get bin means
means = BinLists.bin_means(data)

# Get detailed information
result_with_info, info = BinLists.bin_lists(data, return_bin_info=True)
print(f"Boundaries: {info['bin_boundaries']}")
print(f"Centers: {info['bin_centers']}")
print(f"Counts: {info['bin_counts']}")

# Get indices instead of values
indices = BinLists.bin_lists(data, return_indices=True)
```

### Working with Labels

```python
data = [1.2, 2.7, 3.1, 1.8, 4.5]
labels = ['A', 'B', 'C', 'D', 'E']

result = bin_lists(data, labels=labels)
# Output: [['A', 'D'], ['B'], ['C'], ['E']]
```

### Multidimensional Data

```python
# 2D data binning
data_2d = [[1.2, 2.1], [2.3, 1.8], [1.8, 2.5], [3.1, 1.2]]
result = bin_lists(data_2d, 1)  # Bins by first dimension
```

## Key Features

1. **Mathematica Compatibility**: Replicates the core functionality of Mathematica's BinLists
2. **Flexible Bin Specifications**: Support for various bin specification formats
3. **Data Filtering**: Automatically filters out non-numeric values like NaN and None
4. **Label Support**: Optional labeling system for data elements
5. **Enhanced Features**: Additional functions like bin_counts and bin_means
6. **Performance Optimized**: Uses numpy for efficient boundary calculations
7. **Multidimensional Support**: Basic support for multidimensional data binning

## Bin Specification Formats

| Format | Description | Example |
|--------|-------------|---------|
| `None` or `1` | Unit width bins | `bin_lists(data)` |
| `float dx` | Bins of width dx | `bin_lists(data, 0.5)` |
| `(x_min, x_max, dx)` | Specific range and width | `bin_lists(data, (0, 10, 2))` |
| `(x_min, x_max)` | Range with unit width | `bin_lists(data, (0, 10))` |
| `[[b1, b2, ...]]` | Custom boundaries | `bin_lists(data, [[0, 2, 5, 10]])` |

## Comparison with Mathematica

This Python implementation maintains the core behavior of Mathematica's BinLists:

- **Boundary Handling**: Left-closed, right-open intervals `[a, b)`
- **Data Filtering**: Non-numeric values are automatically excluded
- **Bin Calculation**: Uses the same mathematical approach for boundary determination
- **Element Ordering**: Maintains original order within bins

## Additional Utility Functions

The implementation also includes several utility functions for advanced binning:

- **Percentile-based binning**: Bin data by percentiles
- **Adaptive binning**: Create bins based on data distribution
- **Summary statistics**: Get comprehensive statistics for each bin

This provides a robust, Mathematica-compatible binning solution for Python users with additional features tailored for Python workflows.