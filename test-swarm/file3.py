# Python data processing
import json
from typing import List, Dict

def process_data(items: List[Dict]) -> Dict:
    """Process a list of items and return summary statistics."""
    total = sum(item.get('value', 0) for item in items)
    count = len(items)
    average = total / count if count > 0 else 0
    
    return {
        'total': total,
        'count': count,
        'average': average
    }