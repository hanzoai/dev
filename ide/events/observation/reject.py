from dataclasses import dataclass

from ide.core.schema import ObservationType
from ide.events.observation.observation import Observation


@dataclass
class UserRejectObservation(Observation):
    """This data class represents the result of a rejected action."""

    observation: str = ObservationType.USER_REJECTED

    @property
    def message(self) -> str:
        return self.content
