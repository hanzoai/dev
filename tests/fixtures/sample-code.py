# Sample Python code for testing
import json
from typing import List, Dict, Optional

class DataProcessor:
    """Process and analyze data."""
    
    def __init__(self):
        self.data = []
    
    def add_item(self, item: Dict) -> None:
        """Add an item to the dataset."""
        self.data.append(item)
    
    def get_summary(self) -> Dict:
        """Get summary statistics."""
        if not self.data:
            return {"count": 0, "total": 0, "average": 0}
        
        values = [item.get("value", 0) for item in self.data]
        total = sum(values)
        count = len(values)
        
        return {
            "count": count,
            "total": total,
            "average": total / count if count > 0 else 0
        }