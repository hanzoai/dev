from dataclasses import dataclass, field
from typing import Any

from ide.core.schema import ObservationType
from ide.events.observation.observation import Observation


@dataclass
class MCPObservation(Observation):
    """This data class represents the result of a MCP Server operation."""

    observation: str = ObservationType.MCP
    name: str = ''  # The name of the MCP tool that was called
    arguments: dict[str, Any] = field(
        default_factory=dict
    )  # The arguments passed to the MCP tool

    @property
    def message(self) -> str:
        return self.content
