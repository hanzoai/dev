"""This file imports a global singleton of the `EditTool` class as well as raw functions that expose
its __call__.
The implementation of the `EditTool` class can be found at: https://github.com/hanzoai/ide-aci/.
"""

from hanzo_aci.editor import file_editor

__all__ = ['file_editor']
